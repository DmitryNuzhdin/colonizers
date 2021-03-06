package colonizers.model

import colonizers.model.field.GameField
import colonizers.model.turns.Turn
import colonizers.view.ColonizersView
import org.springframework.stereotype.Component

@Component
class StateHolder {
  val players = List(Player("p1"), Player("p2"), Player("p3"), Player("p4"))
  var gameState: GameState = InitialGameState.apply(players, GameField.generateBalancedRandomField)
  var views:Set[ColonizersView] = Set()

  def registerView(colonizersView: ColonizersView): Unit = {
    synchronized{
      views = views + colonizersView
    }
  }

  def unRegisterView(colonizersView: ColonizersView): Unit = {
    synchronized{
      views = views - colonizersView
    }
  }

  def makeTurn(turn: Turn): Unit = {
    assert(turn.isAllowed(gameState))
    if (gameState.winner.isEmpty) gameState = gameState.makeTurn(turn)
    refreshViews()
  }

  def refreshViews(): Unit = {
    views.foreach(_.refresh(gameState.currentPlayer, gameState))
  }

  def restart(): Unit = {
    gameState = InitialGameState.apply(players, GameField.generateBalancedRandomField)
    refreshViews()
  }
}
