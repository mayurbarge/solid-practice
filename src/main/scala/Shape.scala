import zio.prelude.{NonEmptyList, Validation}

trait Shape
abstract class GenericShape(val edges: List[Line]) extends Shape {
  def area: Option[Double]
  val perimeter = edges.map(_.length).sum
}
case object InvalidShape extends Shape
object Shape {
  def isShapeClosed(shape: GenericShape) = {
    val isStartPointOfLineMatchingEndOfAnotherLine = (shape: GenericShape, line: Line) => shape.edges.filter(_ != line).map(_.b).contains(line.a)
    val isEndPointOfLineMatchingStartOfAnotherLine = (shape: GenericShape, line: Line) => shape.edges.filter(_ != line).map(_.a).contains(line.b)

    shape.edges.map(line => {
      isStartPointOfLineMatchingEndOfAnotherLine(shape, line) &&
        isEndPointOfLineMatchingStartOfAnotherLine(shape, line)
    } ).reduce(_ && _)
  }
  def apply() = InvalidShape
  def apply(edges: NonEmptyList[Line]) = {
    val validateQuadrilateral =
      (edges: NonEmptyList[Line]) => Quadrilateral.validateQuadrilateral(new Quadrilateral(edges)).isSuccess

    val validateSquare =
      (edges: NonEmptyList[Line]) => Square.validateSquare(new Square(edges)).isSuccess

    val validateTrapezoid =
      (edges: NonEmptyList[Line]) => Trapezoid.validateTrapezoid(new Trapezoid(edges)).isSuccess

    edges match {
      case _ if validateSquare(edges) => new Square(edges)
      case _ if validateTrapezoid(edges) => new Trapezoid(edges)
      case _ if validateQuadrilateral(edges) => new Quadrilateral(edges)
      case _=> InvalidShape
    }
  }
}
