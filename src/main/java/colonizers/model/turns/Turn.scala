package colonizers.model.turns

import colonizers.model.buildings._
import colonizers.model.common.Cube6x6
import colonizers.model._
import colonizers.model.field.{IntersectionCoordinate, SideCoordinate}
import colonizers.model.resources._
//import colonizers.util.RichAnys._
import shapeless.syntax.typeable._

sealed trait Turn {
  def isAllowed(implicit gameState: GameState): Boolean
}

trait ChangeResources extends Turn {
  def changeResources(implicit gameState: GameState): PlayersResources

  def isEnoughResources(implicit gameState: GameState): Boolean =
    !changeResources(gameState).containsNegativeAmounts
}

trait ChangeCurrentPlayer extends Turn {
  def changeCurrentPlayer(implicit gameState: GameState): Player
}

trait ChangeBuildings extends Turn {
  def changeBuildings(implicit gameState: GameState): Set[Building]
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
            resources + (player, hexagon.resource.get, 2)
          case Village(player, _) =>
            resources + (player, hexagon.resource.get, 1)
        }
      }
  }

  override def isAllowed(implicit gameState: GameState): Boolean = true
}

case class BuildVillage(intersectionCoordinate: IntersectionCoordinate) extends ChangeResources with ChangeBuildings {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Wood, Clay, Sheep, Grain)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings + Village(gameState.currentPlayer, intersectionCoordinate)
  }

  override def isAllowed(implicit gameState: GameState): Boolean = {
    def noBuildingsNear: Boolean = {
      val nearestIntersections = (intersectionCoordinate.adjacentIntersections + intersectionCoordinate).filter(_.isValid)
      gameState.buildings.collect{case ib:IntersectionBuilding => ib.intersection}.intersect(nearestIntersections).isEmpty
    }

    def connectedToRoad: Boolean =
      gameState.buildings.flatMap(_.cast[Road]).exists(road => intersectionCoordinate.sides.contains(road.sideCoordinate))

    intersectionCoordinate.isValid &&
      isEnoughResources &&
      noBuildingsNear &&
      connectedToRoad
  }
}

case class UpgradeVillageToTown(village: Village) extends ChangeResources with ChangeBuildings {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Grain, Grain, Rock, Rock, Rock)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings - village + village.upgradeToTown
  }

  override def isAllowed(implicit gameState: GameState): Boolean = {
    isEnoughResources &&
      village.player == gameState.currentPlayer
  }
}

case class BuildRoad(sideCoordinate: SideCoordinate) extends ChangeResources with ChangeBuildings {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Wood, Clay)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings + Road(gameState.currentPlayer, sideCoordinate)
  }

  override def isAllowed(implicit gameState: GameState): Boolean = {
    def sideIsEmpty =
      !gameState.buildings.collect { case r: Road => r.sideCoordinate }.contains(sideCoordinate)

    def sideConnected: Boolean = {
      def connectedToBuilding =
        gameState.buildings.flatMap {_.cast[IntersectionBuilding]}.exists { ib =>
          ib.player == gameState.currentPlayer && sideCoordinate.intersections.contains(ib.intersection)
        }
      def connectedToRoad = {
        val roadOpt: Set[Road] = gameState.buildings.flatMap {
          _.cast[Road]
        }

        roadOpt.exists { road =>
          road.player == gameState.currentPlayer && sideCoordinate.adjacent.contains(road.sideCoordinate)
        }
      }

      connectedToBuilding || connectedToRoad
    }

    isEnoughResources &&
      sideIsEmpty &&
      sideConnected
  }
}