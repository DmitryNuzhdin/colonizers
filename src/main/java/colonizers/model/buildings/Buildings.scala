package colonizers.model.buildings

import colonizers.model.Player
import colonizers.model.field.IntersectionCoordinate

trait Building{
  def player: Player
}

trait IntersectionBuilding extends Building{
  def intersection: IntersectionCoordinate
}

case class Road(player:Player, intersections: Set[IntersectionCoordinate]) extends Building{
  assert(intersections.size == 2)
}

case class Village(player: Player, intersection: IntersectionCoordinate) extends IntersectionBuilding {
  def upgradeToTown: Town = Town(player, intersection)
}

case class Town(player: Player, intersection: IntersectionCoordinate) extends IntersectionBuilding