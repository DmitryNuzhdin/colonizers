package colonizers

import colonizers.model.{InitialGameState, Player}

object Tst {
  def main(args: Array[String]): Unit = {
//    val p1 = Player("p1")
//    val p2 = Player("p2")
//
//    val gs = InitialGameState(List(p1,p2), null)
//
//
//    print(gs.currentPlayer)

    val cubes = List(1, 2, 3, 4 , 5, 6)

    val q = for {
      c1 <- cubes
      c2 <- cubes
      c3 <- cubes
      c4 <- cubes
      equals = c1 + c2 == c3 + c4
    } yield equals



    println((q.count(a => a).toDouble / q.size.toDouble).toString)

  }
}
