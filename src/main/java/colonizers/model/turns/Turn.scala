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

case object EndTurn extends ChangeCurrentPlayer {
  override def changeCurrentPlayer(implicit gameState: GameState): Player = {
    val doublePlayers = gameState.players ++ gameState.players
    val currentIndex = doublePlayers.indexOf(gameState.currentPlayer)
    doublePlayers(currentIndex + 1)
  }
}