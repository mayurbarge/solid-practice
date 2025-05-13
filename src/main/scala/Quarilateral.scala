case class Quarilateral(a: Point, b: Point, c: Point, d: Point) {
  val edges = List(
    Line(a, b),
    Line(b, c),
    Line(c, d),
    Line(d, a)
  )

  val perimeter = edges.map(_.length).sum
}
