package colonizers.model.buildings

import colonizers.model.Player
import colonizers.model.field.{IntersectionCoordinate, SideCoordinate}

trait Building{
  def player: Player
}

trait IntersectionBuilding extends Building{
  def intersection: IntersectionCoordinate
}

case class Village(player: Player, intersection: IntersectionCoordinate) extends IntersectionBuilding {
  def upgradeToTown: Town = Town(player, intersection)
}

case class Town(player: Player, intersection: IntersectionCoordinate) extends IntersectionBuilding

case class Road(player: Player, sideCoordinate: SideCoordinate) extends Building