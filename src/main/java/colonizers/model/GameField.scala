package colonizers.model

trait Port

case class Hexagon(id: Int, adjacentHexagons: Set[Hexagon], resource: Option[ResourceType], dice: Cube6x6)
case class Intersection(id: Int, adjacentIntersections: Set[Intersection], adjacentHexagons: Set[Hexagon], port: Option[Port])

object GameField {
  val hexagons: List[Hexagon] = ???
  val intersections: List[Intersection] = ???
}


case class GameFieldState(buildings: List[Building], thiefPlace: Hexagon)

trait Building{
  def player: Player
}

