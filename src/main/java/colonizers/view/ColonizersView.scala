package colonizers.view

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
class ColonizersView(@Autowired val stateHolder: StateHolder) extends VerticalLayout{

  val playerNamePanel = new VerticalLayout(){
    val nameLabel = new Label("Player name: ")
    val name = new Label("")
    val turnLabel = new Label("Current turn of: ")
    val turn = new Label("")
    add(new HorizontalLayout(nameLabel, name))
    add(new HorizontalLayout(turnLabel, turn))
  }

  val endTurnButton = new Button("End turn", (_: ClickEvent[Button]) => {stateHolder.makeTurn(EndTurn)})

  def refresh(player: Player, gameState: GameState) = {
    getUI.ifPresent((ui) => ui.access{() =>
      playerNamePanel.name.setText(player.name)
      playerNamePanel.turn.setText(gameState.currentPlayer.name)
    })
  }

  add(playerNamePanel)
  add(endTurnButton)

  override def onAttach(attachEvent: AttachEvent): Unit = {
    stateHolder.registerView(this)
    refresh(stateHolder.players.head, stateHolder.gameState)
  }

  @PreDestroy
  def preDestroy():Unit = {
    stateHolder.unRegisterView(this)
  }
}
