package colonizers.model.buildings

import colonizers.model.Player
import colonizers.model.field.Intersection

trait Building{
  def player: Player
}

trait IntersectionBuilding extends Building{
  def intersection: Intersection
}

case class Road(player:Player, intersections: Set[Intersection]) extends Building{
  assert(intersections.size == 2)
}

case class Village(player: Player, intersection: Intersection) extends IntersectionBuilding {
  def upgradeToTown: Town = Town(player, intersection)
}

case class Town(player: Player, intersection: Intersection) extends IntersectionBuilding