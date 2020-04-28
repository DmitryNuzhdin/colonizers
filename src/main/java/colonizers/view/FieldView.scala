package colonizers.view
import colonizers.model.{GameState, Player}
import colonizers.model.buildings.{IntersectionBuilding, Town, Village}
import colonizers.model.field.{GameField, HexagonCoordinate, IntersectionCoordinate}
import colonizers.model.resources._
import com.vaadin.flow.component.html.Div
import com.vaadin.flow.dom.Style


class FieldView(gameField: GameField) extends Div {
  class HexagonView(hexagonCoordinate: HexagonCoordinate) extends Div {
    import hexagonCoordinate._
    def style: Style = getStyle

    def color: String = gameField(hexagonCoordinate).flatMap(_.resource).collect{
      case Rock => "lightblue"
      case Clay => "orange"
      case Wood => "green"
      case Grain => "yellow"
      case Sheep => "cyan"
    }.getOrElse("white")

    val boxCorner: (Double, Double) = hexagonBoxXY(hexagonCoordinate)

    style.set("width", %(xSize))
    style.set("height", %(ySize))
    style.set("left", %(boxCorner._1))
    style.set("top", %(boxCorner._2))

    style.set("border", s"5px solid $color")

    style.set("position", "absolute")
    style.set("box-sizing", "border-box")
    setText(gameField(hexagonCoordinate).map(_.dice.dots.toString).getOrElse(""))
  }

  class IntersectionView(intersectionCoordinate: IntersectionCoordinate) extends Div {
    def style: Style = getStyle
    val (x, y) = intersectionXY(intersectionCoordinate)
    style.set("left", %(x))
    style.set("top", %(y))
    style.set("position", "absolute")
  }

  def %(d: Double): String =  d.toString + "%"

  def hexagonBoxXY(hexagonCoordinate: HexagonCoordinate): (Double, Double) = {
    import hexagonCoordinate._
    (xSize * (x - 0.5 *(y - zeroRow)), ySize * y * yOffset)
  }

  def intersectionXY(intersectionCoordinate: IntersectionCoordinate): (Double, Double) = {
    import intersectionCoordinate._
    val minX = hexagonCoordinates.map(_.x).min
    val minY = hexagonCoordinates.map(_.y).min
    val isBottomPoint = hexagonCoordinates.count(_.y == minY) == 1
    val (boxCornerX, boxCornerY) = hexagonBoxXY(HexagonCoordinate(minX, minY))
    val x = boxCornerX + xSize * (if (isBottomPoint) 0.5 else 1.0)
    val y = boxCornerY + ySize * (if (isBottomPoint) 1 else yOffset)
    (x, y)
  }

  val yOffset = 0.75

  val maxX: Int = gameField.hexagons.map(_.coordinate.x).max
  val maxY: Int = gameField.hexagons.map(_.coordinate.y).max
  val xSize: Double = 100.0 / (maxX + 1)
  val ySize: Double = 100.0 / (maxY * yOffset + 1)
  val zeroRow: Int = maxY / 2

  val hexagonViews: Map[HexagonCoordinate, HexagonView] = gameField.hexagons.map(_.coordinate)
    .map{coordinate => coordinate -> new HexagonView(coordinate)}.toMap

  val intersectionViews: Map[IntersectionCoordinate, IntersectionView] =
    gameField.hexagons.flatMap(_.coordinate.intersections).map{intersectionCoordinate =>
      intersectionCoordinate -> new IntersectionView(intersectionCoordinate)
    }.toMap

  hexagonViews.foreach{case (_, view) => add(view)}
  intersectionViews.foreach{case (_, view) => add(view)}

  def refresh(gameState: GameState): Unit = {
    gameState.buildings.collect{case ib: IntersectionBuilding => ib }.foreach{ building =>
      intersectionViews(building.intersection).setText{
        building match {
          case Village(Player(player), _) => s"[$player]V"
          case Town(Player(player), _) => s"[$player]T"
        }
      }
    }
  }

  getStyle.set("position", "relative")
}
