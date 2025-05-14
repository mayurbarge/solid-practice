import zio.prelude.Validation
import zio.test.Assertion.{equalTo, hasSameElements}
import zio.test.{ZIOSpecDefault, _}

object QuadrilateralSpec extends ZIOSpecDefault {
  def spec = suite("QuadrilateralSpec")(
    test("a quadrilateral should have four vertices") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assert(quadrilateral.vertices)(hasSameElements(List(Point(0,0), Point(1, 0), Point(1,1), Point(0, 1))))
    },

    test("a quadrilateral should have four edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      val expectedEdges = List(lineAB, lineBC, lineCD, lineDA)

      assert(quadrilateral.edges)(hasSameElements(expectedEdges))
    },

    test("a quadrilateral having unit length edges should have a perimeter 4") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.perimeter == 4)
    },

    test("Quadrilateral is defined when for each edge if starting point of one edge is ending point of other and vice versa") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(Quadrilateral.qualifyCheck(quadrilateral))
    },

    test("Quadrilateral validation should pass when qualification criteria is not matching") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(Quadrilateral.qualifyCheck(quadrilateral))
      assert(Quadrilateral.validateQuadrilateral(quadrilateral))(equalTo(Validation.succeed(quadrilateral)))
    },

    test("Quadrilateral is not defined when for eny edge starting point or ending point does not coincide with another edge") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(2, 0))
      val lineCD = Line(Point(2, 0), Point(3, 0))
      val lineDA = Line(Point(3, 0), Point(4, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(!Quadrilateral.qualifyCheck(quadrilateral))
    },

    test("Quadrilateral validation should fail when qualification criteria is not matching") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(2, 0))
      val lineCD = Line(Point(2, 0), Point(3, 0))
      val lineDA = Line(Point(3, 0), Point(4, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(Quadrilateral.qualifyCheck(quadrilateral))
      assert(Quadrilateral.validateQuadrilateral(quadrilateral))(equalTo(Validation.fail("Invalid quadrilateral.")))
    },

    test("Quadrilateral should have pair of non-intersecting edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val expectedIntersectingLines = List(
        List(lineAB, lineCD), List(lineCD, lineAB),
        List(lineBC, lineDA), List(lineDA, lineBC),
      )

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(expectedIntersectingLines.contains(quadrilateral.nonIntersectingEdges))
    },
  )
}
