import zio.prelude.Validation

class Quadrilateral(override val edges: List[Line]) extends GenericShape(edges)

object Quadrilateral {
  def validateQuadrilateral(quadrilateral: Quadrilateral): Validation[String, Quadrilateral] = {
    if(Shape.isShapeClosed(quadrilateral) && quadrilateral.edges.size == 4)
      Validation.succeed(quadrilateral)
    else
      Validation.fail("Invalid quadrilateral.")
  }
}
