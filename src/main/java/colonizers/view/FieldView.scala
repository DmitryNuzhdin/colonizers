package colonizers.view
import colonizers.model.buildings.{IntersectionBuilding, Road, Town, Village}
import colonizers.model.field.{GameField, HexagonCoordinate, IntersectionCoordinate, SideCoordinate}
import colonizers.model.resources._
import colonizers.model.{GameState, Player}
import com.vaadin.flow.component.html.Div
import com.vaadin.flow.component.{ClickEvent, ComponentEventListener, HasStyle}
import com.vaadin.flow.dom.Style
import shapeless.syntax.typeable._


class FieldView(gameField: GameField) extends Div {
  trait Highlightable extends HasStyle{
    def highlight(value: Boolean): Unit =
      if (value) {
        getStyle.set("border", "3px solid yellow")
      } else {
        getStyle.set("border", "none")
      }
  }

  class HexagonView(val hexagonCoordinate: HexagonCoordinate) extends Div with Highlightable {
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

    override def highlight(value: Boolean): Unit = {}

    addClickListener(ClickController)
  }

  class IntersectionView(val intersectionCoordinate: IntersectionCoordinate) extends Div with Highlightable {
    def style: Style = getStyle
    val (x, y) = intersectionXY(intersectionCoordinate)
    style.set("left", %(x))
    style.set("top", %(y))
    style.set("position", "absolute")
    style.set("width", "10px")
    style.set("height", "10px")
    style.set("margin-left", "-5px")
    style.set("margin-top", "-5px")
    style.set("background", "grey")

    addClickListener(ClickController)
  }

  class SideView(val sideCoordinate: SideCoordinate) extends Div with Highlightable {
    def style: Style = getStyle
    val (x, y) = sideXY(sideCoordinate)
    style.set("left", %(x))
    style.set("top", %(y))
    style.set("position", "absolute")
    style.set("width", "10px")
    style.set("height", "10px")
    style.set("margin-left", "-5px")
    style.set("margin-top", "-5px")
    style.set("background", "grey")

    addClickListener(ClickController)
  }

  object ClickController extends ComponentEventListener[ClickEvent[Div]] {
    private var nextAction: PartialFunction[Any, Unit] = PartialFunction.empty

    def setNextAction(action: PartialFunction[Any, Unit]): Unit = synchronized {
      nextAction = action

      def highlight(pair: (Any, Highlightable)): Unit = {
        val (coordinate, view) = pair
        view.highlight(action.isDefinedAt(coordinate))
      }

      sideViews.foreach(highlight)
      intersectionViews.foreach(highlight)
      hexagonViews.foreach(highlight)
    }

    override def onComponentEvent(event: ClickEvent[Div]): Unit = {
      def performAction(value: Any): Unit = synchronized {
        if (nextAction.isDefinedAt(value)) {
          val action = nextAction
          setNextAction(PartialFunction.empty)
          action(value)
        }
      }

      event.getSource match {
        case sideView: SideView => performAction(sideView.sideCoordinate)
        case hexagonView: HexagonView => performAction(hexagonView.hexagonCoordinate)
        case intersectionView: IntersectionView => performAction(intersectionView.intersectionCoordinate)
        case _ =>
      }
    }
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

  def sideXY(sideCoordinate: SideCoordinate): (Double,Double) = {
    sideCoordinate.intersections.map(intersectionXY).foldLeft((0.0, 0.0)){case((sx, sy),(x,y)) => (sx + x/2, sy + y/2)}
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

  val sideViews: Map[SideCoordinate, SideView] =
    gameField.hexagons.flatMap(_.coordinate.sides).filter(_.intersections.forall(_.isValid)).map{sideCoordinate =>
      sideCoordinate -> new SideView(sideCoordinate)
    }.toMap

  hexagonViews.foreach{case (_, view) => add(view)}
  intersectionViews.foreach{case (_, view) => add(view)}
  sideViews.foreach{case (_, view) => add(view)}

  def refresh(gameState: GameState): Unit = {
    gameState.buildings.foreach {
      case building: IntersectionBuilding =>
        intersectionViews(building.intersection).setText {
          building match {
            case Village(player, _) => s"[${player.name}]V"
            case Town(player, _) => s"[${player.name}]T"
          }
        }
      case Road(player, coordinate) =>
        sideViews(coordinate).setText(s"[${player.name}]R")
    }

    ClickController.setNextAction(PartialFunction.empty)
  }

  getStyle.set("position", "relative")
}
