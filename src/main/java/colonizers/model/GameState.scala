package colonizers.model

import colonizers.model.buildings.{Building, Village}
import colonizers.model.common.Cube6x6
import colonizers.model.field._
import colonizers.model.resources._
import colonizers.model.turns._
import shapeless.syntax.typeable._

trait GameState {
  def players: List[Player]
  def gameField: GameField
  def currentPlayer: Player
  def buildings: Set[Building]
  def resources: PlayersResources
  def lastToss: Option[Cube6x6]

  def makeTurn(turn: Turn): GameState = AfterTurnState(this, turn)
}

case class InitialGameState(
                             players: List[Player],
                             gameField: GameField
                           ) extends GameState {
  override val currentPlayer: Player = players.head
  override val buildings: Set[Building] = Set(Village(players.head, gameField.hexagons.head.coordinate.intersections.head)) //TODO: remove
  override val resources: PlayersResources = PlayersResources.apply(players)
  override val lastToss: Option[Cube6x6] = None
}

case class AfterTurnState(
                           previousState: GameState,
                           lastTurn: Turn
                         ) extends GameState {
  implicit val state:GameState = previousState
  override val players: List[Player] = previousState.players
  override val gameField: GameField = previousState.gameField
  override val currentPlayer: Player =
    lastTurn match {
      case t:ChangeCurrentPlayer => t.changeCurrentPlayer
      case _ => previousState.currentPlayer
    }
  override val buildings: Set[Building] = {
    lastTurn match {
      case t:ChangeBuildings => t.changeBuildings
      case _ => previousState.buildings
    }
  }
  override val resources: PlayersResources = {
    lastTurn match {
      case t:ChangeResources => t.changeResources
      case _ => previousState.resources
    }
  }
  override val lastToss: Option[Cube6x6] =
    lastTurn.cast[EndTurn].map{et => Some(et.dice)}.getOrElse(previousState.lastToss)
}