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

    test("Quadrilateral validation should pass when it is closed by connecting edges and edges are 4") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(Shape.isShapeClosed(quadrilateral))
      assert(Quadrilateral.validateQuadrilateral(quadrilateral))(equalTo(Validation.succeed(quadrilateral)))
    },

    test("Quadrilateral validation should fail when qualification criteria is not matching") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(2, 0))
      val lineCD = Line(Point(2, 0), Point(3, 0))
      val lineDA = Line(Point(3, 0), Point(4, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(Shape.isShapeClosed(quadrilateral))
      assert(Quadrilateral.validateQuadrilateral(quadrilateral))(equalTo(Validation.fail("Invalid quadrilateral.")))
    },

    test("Quadrilateral validation should fail for more than four edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(2, 0))
      val lineCD = Line(Point(2, 0), Point(3, 0))
      val lineDA = Line(Point(3, 0), Point(4, 0))
      val extraLine = Line(Point(3, 0), Point(4, 0))

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA, extraLine))
      assert(Quadrilateral.validateQuadrilateral(quadrilateral))(equalTo(Validation.fail("Invalid quadrilateral.")))
    },

    test("Quadrilateral should have pair of intersecting edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val expectedIntersectingLines = List(
        List(lineAB, lineBC), List(lineBC, lineAB),
        List(lineBC, lineCD), List(lineCD, lineBC),
        List(lineCD, lineDA), List(lineDA, lineCD),
        List(lineDA, lineAB), List(lineAB, lineDA),
      )

      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.intersectingEdges.exists(intersectingEdge => expectedIntersectingLines.contains(intersectingEdge)))
    },

    test("Quadrilateral angles on opposite sides should sum up to 180 degrees") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))
      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.thetaOne.contains(90))
      assertTrue(quadrilateral.thetaTwo.contains(90))
    },

    test("Quadrilateral area of unit edges should be 1") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))
      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.area.contains(1))
    },

    test("Quadrilateral area of non-unit edges should be 15") {
      val lineAB = Line(Point(1,2), Point(4, 5))
      val lineBC = Line(Point(4, 5), Point(7, 2))
      val lineCD = Line(Point(7, 2), Point(4, 0))
      val lineDA = Line(Point(4, 0), Point(1, 2))
      val quadrilateral = Quadrilateral(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(quadrilateral.area.map(_.toInt).contains(15))
    }
  )
}
