package colonizers.view

import colonizers.model.buildings.{Town, Village}
import colonizers.model.common.Cube6x6
import colonizers.model.field.{IntersectionCoordinate, SideCoordinate}
import colonizers.model.turns.{BuildRoad, BuildVillage, CheatResources, EndTurn, InitialRoadPlacement, InitialVillagePlacement, RollCubes, UpgradeVillageToTown}
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
  implicit def gameState: GameState = stateHolder.gameState

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
    getStyle.set("width", "1000px")
    getStyle.set("height", "800px")
  }

  val restartButton = new Button("RESTART",  (_: ClickEvent[Button]) => {stateHolder.restart()})
  val endTurnButton = new Button("End turn", (_: ClickEvent[Button]) => {stateHolder.makeTurn(EndTurn)})
  val rollCubesButton = new Button("Roll cubes", (_: ClickEvent[Button]) => {stateHolder.makeTurn(RollCubes(Cube6x6.roll()))})
  val buildRoadButton = new Button("Build Road", (_: ClickEvent[Button]) => {
    fieldView.ClickController.setNextAction{
      case side: SideCoordinate if BuildRoad(side).isAllowed => stateHolder.makeTurn(BuildRoad(side))
    }
  })
  val buildVillageButton = new Button("Build Village", (_: Any) => {
    fieldView.ClickController.setNextAction{
      case ic: IntersectionCoordinate if BuildVillage(ic).isAllowed => stateHolder.makeTurn(BuildVillage(ic))
    }
  })
  val initialVillagePlaceButton = new Button("Place Village", (_: Any) => {
    fieldView.ClickController.setNextAction{
      case ic: IntersectionCoordinate if InitialVillagePlacement(ic).isAllowed => stateHolder.makeTurn(InitialVillagePlacement(ic))
    }
  })
  val initialRoadPlaceButton = new Button("Place Road", (_: Any) => {
    fieldView.ClickController.setNextAction{
      case sc: SideCoordinate if InitialRoadPlacement(sc).isAllowed => stateHolder.makeTurn(InitialRoadPlacement(sc))
    }
  })
  val upgradeToTownButton = new Button("Upgrade to Town", (_: Any) => {
    val villages:Map[IntersectionCoordinate, Village] =
      gameState.buildings.collect{case t:Village => t.intersection -> t}.toMap
    fieldView.ClickController.setNextAction{
      case ic: IntersectionCoordinate if villages.contains(ic) && UpgradeVillageToTown(villages(ic)).isAllowed =>
        stateHolder.makeTurn(UpgradeVillageToTown(villages(ic)))
    }
  })
  val cheatResourcesButton = new Button("CHEAT resources", (_: Any) => {stateHolder.makeTurn(CheatResources)})


  def refresh(player: Player, gameState: GameState): Unit = {
    getUI.ifPresent((ui) => ui.access{() =>
      playerNamePanel.name.setText(player.name)
      playerNamePanel.turn.setText(gameState.currentPlayer.name)
      playerNamePanel.lastDice.setText(gameState.lastRoll.map(_.dots.toString).getOrElse(""))
      resourcesView.refresh(gameState)
      fieldView.refresh(gameState)
    })
  }
  add(new VerticalLayout(){
    add(playerNamePanel)
    add(resourcesView)
    add(endTurnButton)
    add(rollCubesButton)
    add(buildRoadButton)
    add(buildVillageButton)
    add(upgradeToTownButton)
    add(initialVillagePlaceButton)
    add(initialRoadPlaceButton)
    add(cheatResourcesButton)
    add(restartButton)
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
