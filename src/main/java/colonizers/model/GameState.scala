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
  def winPoints: WinPoints
  def winner: Option[Player] = winPoints.winner

  def makeTurn(turn: Turn): GameState = AfterTurnState(this, turn)
}

object WinPoints{
  def apply(players: Iterable[Player]): WinPoints = WinPoints(players.map(_ -> 0).toMap)
}

case class WinPoints(data: Map[Player, Int]) {
  def apply(player: Player): Int = data(player)
  def +(player: Player): WinPoints = {
    assert(data.contains(player))
    copy(data + (player -> (data(player) + 1)))
  }
  def winner: Option[Player] = {
    val winners = data.collect{case (player, points) if points >= 10 => player}
    assert(winners.size < 2)
    winners.headOption
  }
}

case class InitialGameState(
                             players: List[Player],
                             gameField: GameField
                           ) extends GameState {
  override val currentPlayer: Player = players.head
  override val buildings: Set[Building] = Set(Village(players.head, gameField.hexagons.head.coordinate.intersections.head)) //TODO: remove
  override val resources: PlayersResources = PlayersResources.apply(players)
  override val lastToss: Option[Cube6x6] = None
  override val winPoints: WinPoints = WinPoints(players)
}

case class AfterTurnState(
                           previousState: GameState,
                           lastTurn: Turn
                         ) extends GameState {
  implicit val state:GameState = previousState
  override val players: List[Player] = previousState.players
  override val gameField: GameField = previousState.gameField

  override val currentPlayer: Player =
    lastTurn.cast[ChangeCurrentPlayer].map(_.changeCurrentPlayer).getOrElse(previousState.currentPlayer)
  override val buildings: Set[Building] =
    lastTurn.cast[ChangeBuildings].map(_.changeBuildings).getOrElse(previousState.buildings)
  override val resources: PlayersResources =
    lastTurn.cast[ChangeResources].map(_.changeResources).getOrElse(previousState.resources)
  override val lastToss: Option[Cube6x6] =
    lastTurn.cast[EndTurn].map{et => Some(et.dice)}.getOrElse(previousState.lastToss)
  override val winPoints: WinPoints =
    lastTurn.cast[ChangeWinPoints].map(_.changeWinPoints).getOrElse(previousState.winPoints)
}