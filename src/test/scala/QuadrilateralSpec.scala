import zio.test.Assertion.hasSameElements
import zio.test.{ZIOSpecDefault, _}

object QuadrilateralSpec extends ZIOSpecDefault {
  def spec = suite("QuadrilateralSpec")(
    test("a quadrilateral should have four vertices") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quarilateral(List(lineAB, lineBC, lineCD, lineDA))
      assert(quadrilateral.vertices)(hasSameElements(List(Point(0,0), Point(1, 0), Point(1,1), Point(0, 1))))
    },

    test("a quadrilateral should have four edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quarilateral(List(lineAB, lineBC, lineCD, lineDA))
      val expectedEdges = List(lineAB, lineBC, lineCD, lineDA)

      assert(quadrilateral.edges)(hasSameElements(expectedEdges))
    },

    test("a quadrilateral having unit length edges should have a perimeter 4") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quarilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.perimeter == 4)
    },

  )
}
