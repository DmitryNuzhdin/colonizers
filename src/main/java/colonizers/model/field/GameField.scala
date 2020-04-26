package colonizers.model.field

import colonizers.model.common.Cube6x6
import colonizers.model.resources.{ResourceType, Wood}

trait Port

case class HexagonCoordinate(x: Int, y:Int){
  def isValid: Boolean = (y match {
    case 0 => x < 3
    case 1 => x < 4
    case 2 => x < 5
    case 3 => x > 0
    case 4 => x > 1
    case _ => false
  }) && x >= 0 && x < 5

  def adjacent:Set[HexagonCoordinate] = {
    val shifts = Set(
      (1,   0),
      (-1,  0),
      (0,   1),
      (0,  -1),
      (-1, -1),
      (1,   1)
    )
    shifts.map{case (xShift, yShift) => copy(x + xShift, y + yShift)}
  }

  def sides: Set[SideCoordinate] = {
    adjacent.map{h => SideCoordinate(Set(h, this))}
  }

  def intersections: Set[IntersectionCoordinate] = {
    sides.foldLeft(Set[IntersectionCoordinate]()){(set, side) => set ++ side.intersections}
  }
}

case class IntersectionCoordinate(hexagonCoordinates: Set[HexagonCoordinate]){
  assert(hexagonCoordinates.size == 3)
  assert(hexagonCoordinates.forall{coordinate =>
    coordinate.adjacent.intersect(hexagonCoordinates - coordinate).size == 2
  })
}

case class SideCoordinate(hexagons: Set[HexagonCoordinate]){
  assert(hexagons.size == 2)
  assert(hexagons.head.adjacent.contains(hexagons.tail.head))

  def intersections: Set[IntersectionCoordinate] = {
    hexagons
      .foldLeft(hexagons.head.adjacent){(set, hex) => set.intersect(hex.adjacent)}
      .map{c => IntersectionCoordinate(hexagons + c)}
  }
}

case class Hexagon(coordinate: HexagonCoordinate, resource: Option[ResourceType], dice: Cube6x6)

case class GameField(
                      hexagons: Set[Hexagon],
                      ports: Map[IntersectionCoordinate, Port]
                    ) {
  def allIntersections: Set[IntersectionCoordinate] =
    hexagons.foldLeft(Set[IntersectionCoordinate]()){(set, hex) => set ++ hex.coordinate.intersections}

  def apply(hexagonCoordinate: HexagonCoordinate): Option[Hexagon] = {
    hexagons.find(_.coordinate == hexagonCoordinate)
  }

  assert(hexagons.map(_.coordinate).size == 19)
  assert(hexagons.forall(_.coordinate.isValid))
  assert{ports.keys.forall(allIntersections.contains)}
}

object GameField {
  def generateRandomField: GameField = {
    val coordinates = for {
      x <- 0 to 5
      y <- 0 to 5
      coordinate = HexagonCoordinate(x, y) if coordinate.isValid
    } yield coordinate
    val hexagons = coordinates.map(Hexagon(_, Some(Wood), Cube6x6.roll())).toSet
    GameField(hexagons, Map())
  }
}
