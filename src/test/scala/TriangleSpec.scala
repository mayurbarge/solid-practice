import zio.test._
import zio.test.Assertion._
import zio.prelude.Validation

object TriangleSpec extends ZIOSpecDefault {
  def spec = suite("LineSpec")(
    test("Triangle should have three edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineAC = Line(Point(1,1), Point(0,0))
      val triangle = Triangle(List(lineAB, lineBC, lineAC))
      assertTrue(triangle.edges.size == 3)
    },

  )
}
