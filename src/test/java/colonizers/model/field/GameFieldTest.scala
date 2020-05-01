package colonizers.model.field

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try

class GameFieldTest extends AnyFlatSpec {
  val coordinate00: HexagonCoordinate = HexagonCoordinate(0, 0)
  val coordinate10: HexagonCoordinate = HexagonCoordinate(1, 0)
  val coordinate11: HexagonCoordinate = HexagonCoordinate(1, 1)

  val coordinate0_1: HexagonCoordinate = HexagonCoordinate(0, -1)
  val coordinate21: HexagonCoordinate = HexagonCoordinate(2, 1)
  val coordinate01: HexagonCoordinate = HexagonCoordinate(0, 1)

  "HexagonCoordinate" should "have 6 valid adjacent IntersectionCoordinates" in {
    val adjacent = coordinate00.adjacent
    val combinations = for {
      c1 <- adjacent
      c2 <- adjacent
    } yield (c1, c2)
    val validIntersections = combinations.flatMap{case (c1, c2) =>
      Try(IntersectionCoordinate(Set(c1, c2, coordinate00))).toOption
    }
    assert(validIntersections.size == 6)
  }

  it should "have 6 sides" in {
    assert(coordinate00.sides.size == 6)
  }

  "SideCoordinate" should "construct" in {
    SideCoordinate(Set(coordinate00, coordinate10))
  }

  "GameField" should "generate random balanced field" in {
    GameField.generateBalancedRandomField
  }

  "IntersectionCoordinate" should "have 3 adjacent" in {
    assert(coordinate00.intersections.head.adjacentIntersections.size == 3)
  }

  it should "adjacents right" in {
    val ic = IntersectionCoordinate(Set(coordinate00, coordinate10, coordinate11))
    val expected = Set(
      IntersectionCoordinate(Set(coordinate0_1, coordinate00, coordinate10)),
      IntersectionCoordinate(Set(coordinate01, coordinate00, coordinate11)),
      IntersectionCoordinate(Set(coordinate21, coordinate11, coordinate10))
    )
    assert(ic.adjacentIntersections == expected)
  }

}
