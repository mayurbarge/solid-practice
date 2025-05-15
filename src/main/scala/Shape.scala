import zio.prelude.{NonEmptyList, Validation}

trait Shape
abstract class GenericShape(val edges: List[Line]) extends Shape {
  val vertices: List[Point] = edges.flatMap(line => List(line.a, line.b )).distinct
  val perimeter = edges.map(_.length).sum
  val intersectingEdges: Option[List[Line]] = edges.combinations(2).find(
    pair => Line.isIntersecting(pair.head, pair.last)
  )

  val thetaOne = intersectingEdges.map(pairOfAdjacentEdges => {
    Line.angleBetween(pairOfAdjacentEdges.head, pairOfAdjacentEdges.last)
  })

  val thetaTwo = thetaOne.map(180 - _)

  val area: Option[Double] =
    for {
      theta1 <- thetaOne
      theta2 <- thetaTwo
    } yield {
      val semiPerimeter = perimeter/2.0
      val theta1InRadian = Math.toRadians(theta1)
      val theta2InRadian = Math.toRadians(theta2)
      Math.sqrt(
        edges.map(edge => semiPerimeter - edge.length).product
          - (edges.map(_.length).product * Math.pow(Math.cos((theta1InRadian+theta2InRadian)/2.0), 2))
      )
    }
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

    edges match {
      case _ if validateSquare(edges) => new Square(edges)
      case _ if validateQuadrilateral(edges) => new Quadrilateral(edges)
      case _=> InvalidShape
    }
  }
}
