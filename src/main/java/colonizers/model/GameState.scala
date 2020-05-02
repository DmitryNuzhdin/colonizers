package colonizers.model

import colonizers.model.buildings.{Building, Village}
import colonizers.model.common.Cube6x6
import colonizers.model.field._
import colonizers.model.resources._
import colonizers.model.turns._
import shapeless.syntax.typeable._

object GamePhase extends Enumeration {
  val Preparation, MainPhase = Value
}

trait GameState {
  def players: List[Player]
  def gameField: GameField
  def currentPlayer: Player
  def buildings: Set[Building]
  def resources: PlayersResources
  def lastRoll: Option[Cube6x6]
  def winPoints: WinPoints
  def winner: Option[Player] = winPoints.winner
  def gamePhase: GamePhase.Value
  def mainPhase: Boolean = gamePhase == GamePhase.MainPhase
  def previousTurn: Option[Turn]
  def previousStateOpt: Option[GameState]
  def previousTurns: Iterable[Turn] = {
    new Iterator[Turn]{
      var currentState: GameState = GameState.this
      override def hasNext: Boolean = currentState.previousTurn.isDefined

      override def next(): Turn = {
        val ans = currentState.previousTurn
        currentState = currentState.previousStateOpt.get
        ans.get
      }
    }.to(Iterable)
  }

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
  override val buildings: Set[Building] = Set()
  override val resources: PlayersResources = PlayersResources.apply(players)
  override val lastRoll: Option[Cube6x6] = None
  override val winPoints: WinPoints = WinPoints(players)
  override def gamePhase: GamePhase.Value = GamePhase.Preparation
  override def previousTurn: Option[Turn] = None
  override def previousStateOpt: Option[GameState] = None
}

case class AfterTurnState(
                           previousState: GameState,
                           lastTurn: Turn
                         ) extends GameState {
  implicit val state:GameState = previousState
  override val players: List[Player] = previousState.players
  override val gameField: GameField = previousState.gameField
  override def previousStateOpt: Option[GameState] = Some(previousState)
  override def previousTurn: Option[Turn] = Some(lastTurn)

  override val currentPlayer: Player =
    lastTurn.cast[ChangeCurrentPlayer].map(_.changeCurrentPlayer).getOrElse(previousState.currentPlayer)
  override val buildings: Set[Building] =
    lastTurn.cast[ChangeBuildings].map(_.changeBuildings).getOrElse(previousState.buildings)
  override val resources: PlayersResources =
    lastTurn.cast[ChangeResources].map(_.changeResources).getOrElse(previousState.resources)
  override val lastRoll: Option[Cube6x6] =
    lastTurn.cast[ChangeLastRoll].map(_.changeLastRoll).getOrElse(previousState.lastRoll)
  override val winPoints: WinPoints =
    lastTurn.cast[ChangeWinPoints].map(_.changeWinPoints).getOrElse(previousState.winPoints)
  override val gamePhase: GamePhase.Value =
    lastTurn.cast[ChangeGamePhase].map(_.changeGamePhase).getOrElse(previousState.gamePhase)

}