package colonizers.view

import colonizers.model.GameState

trait StateView {
  def refresh(gameState: GameState):Unit
}
