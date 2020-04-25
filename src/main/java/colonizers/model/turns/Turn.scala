package colonizers.model.turns

import colonizers.model.{GameState, Player}
import colonizers.model.resources.PlayersResources

sealed trait Turn

trait ChangeResources extends Turn {
  def changeResources(implicit gameState: GameState): PlayersResources
}

trait ChangeCurrentPlayer extends Turn {
  def changeCurrentPlayer(implicit gameState: GameState): Player
}

case class ChangePlayerTurn(player: Player) extends ChangeCurrentPlayer {
  override def changeCurrentPlayer(implicit gameState: GameState): Player = player
}