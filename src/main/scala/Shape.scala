import zio.prelude.Validation

trait Shape
abstract class GenericShape extends Shape {
  val edges: List[Line]
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
  def apply(edges: List[Line]) = InvalidShape
}
