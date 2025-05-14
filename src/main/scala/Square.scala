case class Square(edges: List[Line]) extends GenericShape {
  def parallelEdgesPair = edges.combinations(2).filter(pairOfEdges =>
    Line.isParallel(pairOfEdges.head, pairOfEdges.last)
  ).toList
}
