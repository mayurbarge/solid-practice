import zio.prelude.Validation

case class Square(override val edges: List[Line]) extends Quadrilateral(edges) {
  def parallelEdgesPair = edges.combinations(2).filter(pairOfEdges =>
    Line.isParallel(pairOfEdges.head, pairOfEdges.last)
  ).toList
}
object Square {
  def validateSquare(square: Square) = {
    if(square.parallelEdgesPair.size  == 2 && square.thetaOne.contains(90))
      Validation.succeed(square)
    else Validation.fail("Invalid square.")
  }
}
