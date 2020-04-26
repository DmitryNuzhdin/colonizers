package colonizers.model.resources

import colonizers.model.Player


object PlayersResources {
  def apply(players: List[Player]): PlayersResources = {
    val data: Map[(Player, ResourceType), Int] = (for {
      player <- players
      resourceType <- ResourceType.allKnownTypes
    } yield ((player, resourceType), 0)).toMap
    PlayersResources(data)
  }
}

case class PlayersResources(data: Map[(Player, ResourceType), Int]) {
  def apply(player: Player, resourceType: ResourceType): Int = {
    data.getOrElse((player, resourceType), throw new RuntimeException("Unknown player or resource type"))
  }

  def + (player: Player, resourceType: ResourceType, amount: Int):PlayersResources = {
    val newAmount = apply(player, resourceType) + amount
    copy(data = data + ((player ,resourceType) -> newAmount))
  }

  def ofPlayer(player: Player): Map[ResourceType, Int] = {
    data.collect{case ((p, resourceType), amount) if p == player =>
      (resourceType, amount)
    }
  }
}

object ResourceType {
  val allKnownTypes: List[ResourceType] =
    List(Wood, Sheep, Rock, Clay, Grain)
}

sealed trait ResourceType

case object Wood extends ResourceType
case object Sheep extends ResourceType
case object Rock extends ResourceType
case object Clay extends ResourceType
case object Grain extends ResourceType