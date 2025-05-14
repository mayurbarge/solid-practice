case class Square(edges: List[Line]) {
  val vertices = edges.flatMap(edge => List(edge.a, edge.b)).distinct
}
