package colonizers.model.turns

import colonizers.model.buildings.{IntersectionBuilding, Town, Village}
import colonizers.model.common.Cube6x6
import colonizers.model.{GameState, Player}
import colonizers.model.resources.PlayersResources
import colonizers.util.RichAnys._

sealed trait Turn

trait ChangeResources extends Turn {
  def changeResources(implicit gameState: GameState): PlayersResources
}

trait ChangeCurrentPlayer extends Turn {
  def changeCurrentPlayer(implicit gameState: GameState): Player
}

case class EndTurn(dice: Cube6x6) extends ChangeCurrentPlayer with ChangeResources {
  override def changeCurrentPlayer(implicit gameState: GameState): Player = {
    val doublePlayers = gameState.players ++ gameState.players
    val currentIndex = doublePlayers.indexOf(gameState.currentPlayer)
    doublePlayers(currentIndex + 1)
  }

  override def changeResources(implicit gameState: GameState): PlayersResources = {
    (for {
      building <- gameState.buildings.flatMap(_.cast[IntersectionBuilding])
      hexagonCoordinate <- building.intersection.hexagonCoordinates
      hexagon <- gameState.gameField(hexagonCoordinate)
      if hexagon.dice.dots == dice.dots && hexagon.resource.isDefined
    } yield (hexagon, building))
      .foldLeft(gameState.resources) {case (resources, (hexagon, building)) =>
        building match {
          case Town(player, _) =>
            resources + (player, hexagon.resource.get, 1)
          case Village(player, _) =>
            resources + (player, hexagon.resource.get, 2)
        }
      }
  }
}