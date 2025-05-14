import zio.prelude.Validation

case class Quadrilateral(edges: List[Line]) extends GenericShape

object Quadrilateral {
  def validateQuadrilateral(quadrilateral: Quadrilateral): Validation[String, Quadrilateral] = {
    if(Shape.isShapeClosed(quadrilateral) && quadrilateral.edges.size == 4)
      Validation.succeed(quadrilateral)
    else
      Validation.fail("Invalid quadrilateral.")
  }
}
