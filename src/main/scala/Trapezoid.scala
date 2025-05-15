import zio.prelude.Validation

case class Trapezoid(override val edges: List[Line]) extends Quadrilateral(edges)

object Trapezoid {
  def validateTrapezoid(trapezoid: Trapezoid) = {
    if(trapezoid.parallelEdgesPair.size == 1 && trapezoid.edges.size == 4)
      Validation.succeed(trapezoid)
    else Validation.fail("Invalid Trapezoid.")
  }
}