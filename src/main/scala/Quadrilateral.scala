import zio.prelude.Validation

case class Quadrilateral(edges: List[Line]) {
  val vertices: List[Point] = edges.flatMap(line => List(line.a, line.b )).distinct
  val perimeter = edges.map(_.length).sum
}

object Quadrilateral {

  def qualifyCheck(quadrilateral: Quadrilateral) = {
    val isStartPointOfLineMatchingEndOfAnotherLine = (quadrilateral: Quadrilateral, line: Line) => quadrilateral.edges.filter(_ != line).map(_.b).contains(line.a)
    val isEndPointOfLineMatchingStartOfAnotherLine = (quadrilateral: Quadrilateral, line: Line) => quadrilateral.edges.filter(_ != line).map(_.a).contains(line.b)

    quadrilateral.edges.map(line => {
      isStartPointOfLineMatchingEndOfAnotherLine(quadrilateral, line) &&
        isEndPointOfLineMatchingStartOfAnotherLine(quadrilateral, line)
    } ).reduce(_ && _)
  }

  def validateQuadrilateral(quadrilateral: Quadrilateral): Validation[String, Quadrilateral] = {
    if(qualifyCheck(quadrilateral))
      Validation.succeed(quadrilateral)
    else
      Validation.fail("Invalid quadrilateral.")
  }
}
