import zio.test._
import zio.test.Assertion._
import zio.prelude.Validation

object SquareSpec extends ZIOSpecDefault {
  def spec = suite("SquareSpec")(
    test("Square should have four edges and four vertices") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))
      assertTrue(square.edges.size == 4)
      assertTrue(square.vertices.size == 4)
    },

    test("Square is a Shape and a Quadrilateral") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))

      assertTrue(square.isInstanceOf[Shape])
      assertTrue(square.isInstanceOf[Quadrilateral])
    },

    test("Square should have two pair of parallel edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))

      assertTrue(square.parallelEdgesPair.size == 2)
      assert(square.parallelEdgesPair.head)(hasSameElements(List(lineAB, lineCD)))
      assert(square.parallelEdgesPair.last)(hasSameElements(List(lineBC, lineDA)))
    },

    test("Square should an 90 degree angle between intersecting edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))

      val angleBetweenEdges = square.thetaOne
      assertTrue(angleBetweenEdges.contains(90))
    },

    test("Validation should pass when Square has two parallel edges pair and theta 90 degree") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))
      val validatedSquare = Square.validateSquare(square)

      assert(validatedSquare)(equalTo(Validation.succeed(square)))
    },

    test("Validation should fail if edges are not parallel") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(2,2))
      val lineCD = Line(Point(2,2), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))
      val validatedSquare = Square.validateSquare(square)

      assert(validatedSquare)(equalTo(Validation.fail("Invalid square.")))
    },

    test("Validation should fail if theta is not 90 degree") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(2,1))
      val lineCD = Line(Point(2,1), Point(1,1))
      val lineDA = Line(Point(1,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineCD, lineDA))
      val validatedSquare = Square.validateSquare(square)

      assert(validatedSquare)(equalTo(Validation.fail("Invalid square.")))
    },

  )
}

