package colonizers

import colonizers.model.turns.ChangePlayerTurn
import colonizers.model.{InitialGameState, Player}

object Tst {
  def main(args: Array[String]): Unit = {
    val p1 = Player("p1")
    val p2 = Player("p2")

    val gs = InitialGameState(List(p1,p2), null)

    val newGs = gs.makeTurn(ChangePlayerTurn(p2))

    print(gs.currentPlayer)
    print(newGs.currentPlayer)
  }
}
