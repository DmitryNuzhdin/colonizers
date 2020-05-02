package colonizers.model.turns

import colonizers.model.buildings._
import colonizers.model.common.Cube6x6
import colonizers.model.{GamePhase, _}
import colonizers.model.field.{IntersectionCoordinate, SideCoordinate}
import colonizers.model.resources._
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

trait ChangeWinPoints extends Turn {
  def changeWinPoints(implicit gameState: GameState): WinPoints
}

trait ChangeGamePhase extends Turn {
  def changeGamePhase(implicit gameState: GameState): GamePhase.Value
}

trait ChangeLastRoll extends Turn {
  def changeLastRoll(implicit gameState: GameState): Option[Cube6x6]
}

trait MainPhaseTurn extends Turn {
  override def isAllowed(implicit gameState: GameState): Boolean =
    gameState.mainPhase && isAllowedOnMainPhase
  def isAllowedOnMainPhase(implicit gameState: GameState): Boolean
}

object EndTurn extends MainPhaseTurn with ChangeCurrentPlayer with ChangeLastRoll {

  override def changeCurrentPlayer(implicit gameState: GameState): Player = {
    val doublePlayers = gameState.players ++ gameState.players
    val currentIndex = doublePlayers.indexOf(gameState.currentPlayer)
    doublePlayers(currentIndex + 1)
  }

  override def changeLastRoll(implicit gameState: GameState): Option[Cube6x6] = None

  override def isAllowedOnMainPhase(implicit gameState: GameState): Boolean =
    gameState.lastRoll.isDefined
}

case class RollCubes(roll: Cube6x6) extends MainPhaseTurn with ChangeResources with ChangeLastRoll {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    (for {
      building <- gameState.buildings.flatMap(_.cast[IntersectionBuilding])
      hexagonCoordinate <- building.intersection.hexagonCoordinates
      hexagon <- gameState.gameField(hexagonCoordinate)
      if hexagon.dice == roll.dots && hexagon.resource.isDefined
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

  override def changeLastRoll(implicit gameState: GameState): Option[Cube6x6] = Some(roll)

  override def isAllowedOnMainPhase(implicit gameState: GameState): Boolean = gameState.lastRoll.isEmpty
}

case class BuildVillage(intersectionCoordinate: IntersectionCoordinate) extends MainPhaseTurn
  with ChangeResources with ChangeBuildings with ChangeWinPoints {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Wood, Clay, Sheep, Grain)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings + Village(gameState.currentPlayer, intersectionCoordinate)
  }

  override def changeWinPoints(implicit gameState: GameState): WinPoints = gameState.winPoints + gameState.currentPlayer

  def connectedToRoad(implicit gameState: GameState): Boolean =
    gameState.buildings.collect{case r:Road if intersectionCoordinate.sides.contains(r.sideCoordinate) => r}.nonEmpty

  def noBuildingsNear(implicit gameState: GameState): Boolean = {
    val nearestIntersections = (intersectionCoordinate.adjacentIntersections + intersectionCoordinate).filter(_.isValid)
    gameState.buildings.collect{case ib:IntersectionBuilding => ib.intersection}.intersect(nearestIntersections).isEmpty
  }

  override def isAllowedOnMainPhase(implicit gameState: GameState): Boolean = {
    def noBuildingsNear: Boolean = {
      val nearestIntersections = (intersectionCoordinate.adjacentIntersections + intersectionCoordinate).filter(_.isValid)
      gameState.buildings.collect{case ib:IntersectionBuilding => ib.intersection}.intersect(nearestIntersections).isEmpty
    }

    intersectionCoordinate.isValid &&
      isEnoughResources &&
      noBuildingsNear &&
      connectedToRoad
  }
}

case class UpgradeVillageToTown(village: Village) extends MainPhaseTurn with ChangeResources with ChangeBuildings with ChangeWinPoints {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Grain, Grain, Rock, Rock, Rock)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings - village + village.upgradeToTown
  }

  override def changeWinPoints(implicit gameState: GameState): WinPoints = gameState.winPoints + gameState.currentPlayer

  override def isAllowedOnMainPhase(implicit gameState: GameState): Boolean = {
    isEnoughResources &&
      village.player == gameState.currentPlayer
  }
}

case class BuildRoad(sideCoordinate: SideCoordinate) extends MainPhaseTurn with ChangeResources with ChangeBuildings {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    val requiredResources = List(Wood, Clay)
    requiredResources.foldLeft(gameState.resources){(resources, requiredResource) =>
      resources + (gameState.currentPlayer, requiredResource, -1)}
  }

  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings + Road(gameState.currentPlayer, sideCoordinate)
  }

  def connectedToBuilding(implicit gameState: GameState): Boolean =
    gameState.buildings.flatMap {_.cast[IntersectionBuilding]}.exists { ib =>
      ib.player == gameState.currentPlayer && sideCoordinate.intersections.contains(ib.intersection)
    }

  def connectedToRoad(implicit gameState: GameState): Boolean = {
    val roadOpt: Set[Road] = gameState.buildings.flatMap {
      _.cast[Road]
    }

    roadOpt.exists { road =>
      road.player == gameState.currentPlayer && sideCoordinate.adjacent.contains(road.sideCoordinate)
    }
  }

  def sideIsEmpty(implicit gameState: GameState): Boolean =
    !gameState.buildings.collect { case r: Road => r.sideCoordinate }.contains(sideCoordinate)

  override def isAllowedOnMainPhase(implicit gameState: GameState): Boolean = {
    isEnoughResources &&
      sideIsEmpty &&
      (connectedToBuilding || connectedToRoad)
  }
}

case class InitialVillagePlacement(intersectionCoordinate: IntersectionCoordinate)
  extends ChangeBuildings  with ChangeWinPoints {
  override def changeBuildings(implicit gameState: GameState): Set[Building] = {
    gameState.buildings + Village(gameState.currentPlayer, intersectionCoordinate)
  }

  override def changeWinPoints(implicit gameState: GameState): WinPoints =
    gameState.winPoints + gameState.currentPlayer

  override def isAllowed(implicit gameState: GameState): Boolean = {
    val buildVillage = BuildVillage(intersectionCoordinate)
    gameState.gamePhase == GamePhase.Preparation &&
      gameState.previousTurn.collect{case ivp:InitialVillagePlacement => ivp}.isEmpty &&
      buildVillage.noBuildingsNear
  }
}

case class InitialRoadPlacement(sideCoordinate: SideCoordinate)
  extends ChangeBuildings with ChangeCurrentPlayer with ChangeGamePhase {
  override def changeBuildings(implicit gameState: GameState): Set[Building] =
    gameState.buildings + Road(gameState.currentPlayer, sideCoordinate)

  override def changeCurrentPlayer(implicit gameState: GameState): Player = {
    val playersDidTurns: Int = (gameState.previousTurns.size + 1) / 2
    ((gameState.players ::: gameState.players.reverse ) :+ gameState.players.head)(playersDidTurns)
  }

  override def changeGamePhase(implicit gameState: GameState): GamePhase.Value =
    if (gameState.previousTurns.size + 1 == gameState.players.size * 4) GamePhase.MainPhase else GamePhase.Preparation

  override def isAllowed(implicit gameState: GameState): Boolean = {
    val buildRoad = BuildRoad(sideCoordinate)

    gameState.gamePhase == GamePhase.Preparation &&
      gameState.previousTurn.collect{case ivp:InitialVillagePlacement => ivp}.isDefined &&
      buildRoad.sideIsEmpty &&
      (buildRoad.connectedToRoad || buildRoad.connectedToBuilding)
  }
}

case object CheatResources extends ChangeResources {
  override def changeResources(implicit gameState: GameState): PlayersResources = {
    (for {
      resource <- ResourceType.allKnownTypes
      player <- gameState.players
    } yield (player, resource)).foldLeft(gameState.resources){case (resources, (player, resource)) =>
      resources + (player, resource, 1)}
  }

  override def isAllowed(implicit gameState: GameState): Boolean = true
}