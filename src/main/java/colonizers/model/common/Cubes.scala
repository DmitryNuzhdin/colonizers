package colonizers.model.common

import scala.util.Random

object Cube6 {
  def toss(): Cube6 = Cube6(Random.nextInt(6) + 1)
}

object Cube6x6 {
  def roll(): Cube6x6 = Cube6x6(Cube6.toss(), Cube6.toss())
}

case class Cube6(dots: Int){
  assert(dots > 0 && dots < 7)
}

case class Cube6x6(cube6: (Cube6, Cube6)){
  def dots: Int = cube6._1.dots + cube6._2.dots
}