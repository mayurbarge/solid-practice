case class Square(override val edges: List[Line]) extends Quadrilateral(edges) {
  def parallelEdges = edges.combinations(2).filter(pairOfEdges =>
    Line.isParallel(pairOfEdges.head, pairOfEdges.last)
  ).toList
}
