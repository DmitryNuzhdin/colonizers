package colonizers.model

import colonizers.model.field._
import colonizers.model.resources._
import colonizers.model.turns._

trait GameState {
  def players: List[Player]
  def gameField: GameField
  def currentPlayer: Player
  def buildings: List[Building]
  def resources: PlayersResources

  def makeTurn(turn: Turn): GameState = AfterTurnState(this, turn)
}

case class InitialGameState(
                           players: List[Player],
                           gameField: GameField
                           ) extends GameState {
  override val currentPlayer: Player = players.head
  override val buildings: List[Building] = List()
  override val resources: PlayersResources = PlayersResources.apply(players)
}

case class AfterTurnState(
                           previousState: GameState,
                           lastTurn: Turn
                         ) extends GameState {
  implicit val state:GameState = this

  override def players: List[Player] = previousState.players

  override def gameField: GameField = previousState.gameField

  override def currentPlayer: Player = lastTurn match {
    case t:ChangeCurrentPlayer => t.changeCurrentPlayer
    case _ => previousState.currentPlayer
  }

  override def buildings: List[Building] = ???

  override def resources: PlayersResources = ???
}