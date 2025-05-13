import LineSpec.test
import PointSpec.{suite, test}
import zio.test.{ZIOSpecDefault, _}

object LineSpec extends ZIOSpecDefault {
  def spec = suite("LineSpec")(
    test("Line should get created with two points") {
      val line = Line(Point(1,1), Point(10,10))
      assertTrue(line == Line(Point(1,1), Point(10, 10)))
    },

    test("should return dy which is difference between y co-ordinates of point a and b") {
      val line = Line(Point(1,1), Point(10,10))
      assertTrue(line.dy == 9)
    },

    test("should return dx which is difference between x co-ordinates of point a and b") {
      val line = Line(Point(1,1), Point(10,10))
      assertTrue(line.dx == 9)
    },

    test("should return length of a line") {
      val line = Line(Point(0,0), Point(4,3))
      assertTrue(line.length == 5)
    },

    test("should return slope of a line") {
      val line = Line(Point(0,0), Point(5,10))
      assertTrue(line.slope == 2)
    },

    test("should return negative infinity when line is vertically down") {
      val line = Line(Point(0,0), Point(0, -10))

      assertTrue(line.dy < 0)
      assertTrue(line.dx >= 0)
      assertTrue(line.slope == Double.NegativeInfinity)
    },

  )
}
