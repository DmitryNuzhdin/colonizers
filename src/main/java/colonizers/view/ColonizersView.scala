package colonizers.view

import colonizers.model.common.Cube6x6
import colonizers.model.turns.EndTurn
import colonizers.model.{GameState, Player, StateHolder}
import com.vaadin.flow.component.button.Button
import com.vaadin.flow.component.html.Label
import com.vaadin.flow.component.orderedlayout.{HorizontalLayout, VerticalLayout}
import com.vaadin.flow.component.page.Push
import com.vaadin.flow.component.{AttachEvent, ClickEvent}
import com.vaadin.flow.router.Route
import javax.annotation.PreDestroy
import org.springframework.beans.factory.annotation.Autowired

@Route
@Push
class ColonizersView(@Autowired val stateHolder: StateHolder) extends HorizontalLayout with StateView {

  val playerNamePanel = new VerticalLayout(){
    val nameLabel = new Label("Player name: ")
    val name = new Label("")
    val turnLabel = new Label("Current turn of: ")
    val turn = new Label("")
    val lastDiceLabel = new Label("Last dice")
    val lastDice = new Label("")
    add(new HorizontalLayout(nameLabel, name))
    add(new HorizontalLayout(turnLabel, turn))
    add(new HorizontalLayout(lastDiceLabel, lastDice))
  }

  val resourcesView = new ResourcesView
  val fieldView: FieldView = new FieldView(stateHolder.gameState.gameField){
    getStyle.set("width", "400px")
    getStyle.set("height", "400px")
  }

  val endTurnButton = new Button("End turn", (_: ClickEvent[Button]) => {stateHolder.makeTurn(EndTurn(Cube6x6.roll()))})

  def refresh(player: Player, gameState: GameState): Unit = {
    getUI.ifPresent((ui) => ui.access{() =>
      playerNamePanel.name.setText(player.name)
      playerNamePanel.turn.setText(gameState.currentPlayer.name)
      playerNamePanel.lastDice.setText(gameState.lastToss.map(_.dots.toString).getOrElse(""))
      resourcesView.refresh(gameState)
      fieldView.refresh(gameState)
    })
  }
  add(new VerticalLayout(){
    add(playerNamePanel)
    add(resourcesView)
    add(endTurnButton)
  })
  add(new VerticalLayout(){
    add(fieldView)
  })

  override def onAttach(attachEvent: AttachEvent): Unit = {
    stateHolder.registerView(this)
    refresh(stateHolder.players.head, stateHolder.gameState)
  }

  @PreDestroy
  def preDestroy():Unit = {
    stateHolder.unRegisterView(this)
  }

  override def refresh(gameState: GameState): Unit = ???
}
