import zio.test._
import zio.test.Assertion._
import zio.prelude.Validation

object SquareSpec extends ZIOSpecDefault {
  def spec = suite("SquareSpec")(
    test("Square should have four edges and four vertices") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineAC = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val square = Square(List(lineAB, lineBC, lineAC, lineDA))
      assertTrue(square.edges.size == 4)
      assertTrue(square.vertices.size == 4)
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

  )
}

