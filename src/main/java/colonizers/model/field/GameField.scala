package colonizers.model.field

import colonizers.model.common.Cube6x6
import colonizers.model.resources._

import scala.annotation.tailrec
import scala.util.Random

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

  def adjacentIntersections: Set[IntersectionCoordinate] = {
    (for {
      subset: List[HexagonCoordinate] <- hexagonCoordinates.subsets(2).map(_.toList)
      c1 :: c2 :: Nil = subset
      intersection <- c1.intersections.intersect(c2.intersections)
    } yield intersection).toSet - this
  }

  def isValid: Boolean = hexagonCoordinates.exists(_.isValid)

  def sides: Set[SideCoordinate] =
    hexagonCoordinates.subsets(2).map(SideCoordinate).toSet
}

case class SideCoordinate(hexagons: Set[HexagonCoordinate]){
  assert(hexagons.size == 2)
  assert(hexagons.head.adjacent.contains(hexagons.tail.head))

  def intersections: Set[IntersectionCoordinate] = {
    hexagons
      .foldLeft(hexagons.head.adjacent){(set, hex) => set.intersect(hex.adjacent)}
      .map{c => IntersectionCoordinate(hexagons + c)}
  }

  def adjacent: Set[SideCoordinate] = {
    for {
      hexagonCoordinate <- hexagons
      shared <- hexagons.map(_.adjacent).reduce(_.intersect(_))
    } yield SideCoordinate(Set(hexagonCoordinate, shared))
  }
}

case class Hexagon(coordinate: HexagonCoordinate, resource: Option[ResourceType], dice: Int) {
  assert(coordinate.isValid)
}

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
  @tailrec
  def generateBalancedRandomField: GameField = {
    val field = generateRandomField
    val h6 = field.hexagons.find(_.dice == 6).get
    val h8 = field.hexagons.find(_.dice == 8).get
    val isBalanced = !h6.coordinate.adjacent.contains(h8.coordinate)
    if (isBalanced)
      field
    else
      generateBalancedRandomField
  }

  private def generateRandomField: GameField = {
    val resources = Random.shuffle(
      List((Wood, 4), (Sheep, 4), (Grain, 4), (Clay, 3), (Rock, 3))
        .flatMap{case (rt, number) => (1 to number).map(_ => rt) }
    )
    val shuffledResources = Random.shuffle(resources)
    val coordinates = for {
      x <- 0 to 5
      y <- 0 to 5
      coordinate = HexagonCoordinate(x, y) if coordinate.isValid
    } yield coordinate
    val diceNumbers = Random.shuffle(List(2,2,3,3,4,4,5,5,6,8,9,9,10,10,11,11,12,12))
    val resourceCoordinates = (coordinates.toSet - HexagonCoordinate(2, 2)).toList
    assert(resourceCoordinates.size == shuffledResources.size)
    assert(resourceCoordinates.size == diceNumbers.size)

    val randomHexagons : Set[Hexagon] = resourceCoordinates.lazyZip(shuffledResources).lazyZip(diceNumbers)
      .map{case (coordinate, resource, dice) => Hexagon(coordinate, Some(resource), dice)}.toSet

    val hexagons = randomHexagons + Hexagon(HexagonCoordinate(2, 2), None, 7)

    assert(hexagons.size == 19)

    GameField(hexagons, Map())
  }
}
