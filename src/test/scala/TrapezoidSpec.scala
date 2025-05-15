import TriangleSpec.test
import zio.test._
import zio.test.Assertion._
import zio.prelude.{NonEmptyList, Validation}
object TrapezoidSpec extends ZIOSpecDefault {
  def spec = suite("TrapezoidSpec")(
    test("Trapezoid should have four edges and four vertices") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))

      val trapezoid = Trapezoid(NonEmptyList(lineAB, lineBC, lineCD, lineDA))
      assertTrue(trapezoid.edges.size == 4)
      assertTrue(trapezoid.vertices.size == 4)
    },

    test("Trapezoid is a quadrilateral") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))

      val trapezoid = Trapezoid(NonEmptyList(lineAB, lineBC, lineCD, lineDA))
      assertTrue(trapezoid.isInstanceOf[Quadrilateral])
    },

    test("Trapezoid should have one pair of parallel edges") {
      val lineAB = Line(Point(0,0), Point(4, 0))
      val lineBC = Line(Point(4,0), Point(2,1))
      val lineCD = Line(Point(2,1), Point(1,1))
      val lineDA = Line(Point(1,1), Point(0,0))

      val trapezoid = Trapezoid(NonEmptyList(lineAB, lineBC, lineCD, lineDA))
      assertTrue(trapezoid.parallelEdgesPair.size == 1)
    },

  )
}
