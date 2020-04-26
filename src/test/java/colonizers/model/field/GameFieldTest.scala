package colonizers.model.field

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try

class GameFieldTest extends AnyFlatSpec {
  val coordinate1: HexagonCoordinate = HexagonCoordinate(0, 0)
  val coordinate2: HexagonCoordinate = HexagonCoordinate(1, 0)

  "HexagonCoordinate" should "have 6 valid adjacent IntersectionCoordinates" in {
    val adjacent = coordinate1.adjacent
    val combinations = for {
      c1 <- adjacent
      c2 <- adjacent
    } yield (c1, c2)
    val validIntersections = combinations.flatMap{case (c1, c2) =>
      Try(IntersectionCoordinate(Set(c1, c2, coordinate1))).toOption
    }
    assert(validIntersections.size == 6)
  }

  it should "have 6 sides" in {
    assert(coordinate1.sides.size == 6)
  }

  "SideCoordinate" should "construct" in {
    SideCoordinate(Set(coordinate1, coordinate2))
  }

  "GameField" should "generate random field" in {
    GameField.generateRandomField
  }

}
