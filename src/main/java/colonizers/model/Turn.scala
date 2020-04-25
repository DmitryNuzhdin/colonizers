package colonizers.model

sealed trait Turn {
  def changeGameState(gameState: GameState): GameState
}