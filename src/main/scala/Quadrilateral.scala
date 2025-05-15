import zio.prelude.Validation

class Quadrilateral(override val edges: List[Line]) extends GenericShape(edges) {
  val vertices: List[Point] = edges.flatMap(line => List(line.a, line.b )).distinct

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
  def parallelEdgesPair = edges.combinations(2).filter(pairOfEdges =>
    Line.isParallel(pairOfEdges.head, pairOfEdges.last)
  ).toList
}

object Quadrilateral {
  def validateQuadrilateral(quadrilateral: Quadrilateral): Validation[String, Quadrilateral] = {
    if(Shape.isShapeClosed(quadrilateral) && quadrilateral.edges.size == 4)
      Validation.succeed(quadrilateral)
    else
      Validation.fail("Invalid quadrilateral.")
  }
}
