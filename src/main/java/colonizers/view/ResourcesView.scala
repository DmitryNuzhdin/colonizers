package colonizers.view

import colonizers.model.GameState
import colonizers.model.resources.ResourceType
import com.vaadin.flow.component.html.Label
import com.vaadin.flow.component.orderedlayout.{HorizontalLayout, VerticalLayout}

class ResourcesView extends VerticalLayout with StateView {
  val amounts: Map[ResourceType, Label] = ResourceType.allKnownTypes.map(t => (t, new Label(""))).toMap
  val winPoints = new Label("")

  override def refresh(gameState: GameState): Unit = {
    //TODO wrong player
    gameState.resources.ofPlayer(gameState.currentPlayer).foreach{ case (rt, amount) =>
      val youWin = if (gameState.winner.contains(gameState.currentPlayer)) " (YOU WIN!)" else ""
      amounts(rt).setText(amount.toString + youWin)
    }
    winPoints.setText(gameState.winPoints(gameState.currentPlayer).toString)
  }


  amounts.foreach{case (rt, label) =>
    add(new HorizontalLayout(new Label(rt.toString), label))
  }
  add(new HorizontalLayout(new Label("Win points"), winPoints))
}
