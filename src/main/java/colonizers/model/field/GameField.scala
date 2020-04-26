package colonizers.model.field

import colonizers.model.buildings.Building
import colonizers.model.common.Cube6x6
import colonizers.model.resources.ResourceType

trait Port

case class Hexagon(id: Int, adjacentHexagons: Set[Hexagon], resource: Option[ResourceType], dice: Cube6x6)
case class Intersection(id: Int, adjacentIntersections: Set[Intersection], adjacentHexagons: Set[Hexagon], port: Option[Port])

trait GameField {
  val hexagons: List[Hexagon] = ???
  val intersections: List[Intersection] = ???
}


case class GameFieldState(buildings: List[Building], thiefPlace: Hexagon)

