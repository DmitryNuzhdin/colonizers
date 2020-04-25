package colonizers.model

trait Resource[T <: Resource[_]]{
  def value: Int
  def copy(int: Int): T
  def + (int :Int) : T = copy(value + int)
}

sealed trait ResourceType

case object Wood extends ResourceType
case object Sheep extends ResourceType
case object Rock extends ResourceType
case object Clay extends ResourceType
case object Grain extends ResourceType

//case class Wood(value: Int) extends Resource[Wood]{
//  override def copy(int: Int): Wood = Wood(int)
//}
//case class Clay(value: Int) extends Resource[Clay]{
//  override def copy(int: Int): Clay = Clay(int)
//}
//case class Sheep(value: Int) extends Resource[Sheep]{
//  override def copy(int: Int): Sheep = Sheep(int)
//}
//case class Rock(value: Int) extends Resource[Rock]{
//  override def copy(int: Int): Rock = Rock(int)
//}
//case class Grain(value: Int) extends Resource[Grain]{
//  override def copy(int: Int): Grain = Grain(int)
//}