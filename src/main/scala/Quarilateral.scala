case class Quarilateral(edges: List[Line]) {
  val vertices: List[Point] = edges.flatMap(line => List(line.a, line.b )).distinct
  val perimeter = edges.map(_.length).sum
}
