import zio.test._
import zio.test.Assertion._
import zio.prelude.Validation

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

    test("should return negative infinity when line is vertically down") {
      val line = Line(Point(0,0), Point(0, -10))

      assertTrue(line.dy < 0)
      assertTrue(line.dx == 0)
      assertTrue(line.slope == Double.NegativeInfinity)
    },

    test("should return positive infinity when line is vertically up") {
      val line = Line(Point(0,0), Point(0, 2))

      assertTrue(line.dy >= 0)
      assertTrue(line.dx == 0)
      assertTrue(line.slope == Double.PositiveInfinity)
    },

    test("should return slope of a line") {
      val line = Line(Point(0,0), Point(5,10))
      assertTrue(line.slope == 2)
    },

    test("should return true when line has different points") {
      val line = Line(Point(0,0), Point(5,10))
      assert(Line.validateLine(line))(equalTo(Validation.succeed(line)))
    },

    test("should return false when line has same points") {
      val line = Line(Point(5,10), Point(5,10))
      assert(Line.validateLine(line))(equalTo(Validation.fail("Invalid line.")))
    },

    test("should check if lines are same") {
      val line = Line(Point(5,10), Point(5,10))
      assertTrue(Line.isSame(line, line))
    },

    test("should check if lines are not same") {
      val line = Line(Point(5,10), Point(5,10))
      assertTrue(Line.isNotSame(line, line.copy(a = Point(10, 10))))
    },

    test("should check if two lines are parallel") {
      val lineAB = Line(Point(0,10), Point(0,10))
      val lineCD = Line(Point(0,5), Point(0,5))
      assertTrue(Line.isParallel(lineAB, lineCD))
      assertTrue(Line.isParallel(lineCD, lineAB))
    },

    test("parallel check should fail if both lines are same") {
      val line = Line(Point(0,5), Point(0,5))
      assertTrue(!Line.isParallel(line, line))
    },

    test("should check if two lines are perpendicular") {
      val lineAB = Line(Point(0,5), Point(0,-10))
      val lineCD = Line(Point(5,0), Point(-5,0))
      assertTrue(Line.isPerpendicular(lineAB, lineCD))
      assertTrue(Line.isPerpendicular(lineCD, lineAB))
    },

    test("perpendicular check should fail if both lines are same") {
      val line = Line(Point(0,5), Point(0,5))
      assertTrue(!Line.isPerpendicular(line, line))
    },

    test("should check if two lines are intersecting") {
      val lineAB = Line(Point(0,5), Point(0,-10))
      val lineCD = Line(Point(4,0), Point(0,2))
      val x = Line.isSame(lineAB, lineCD)
      assertTrue(Line.isIntersecting(lineAB, lineCD))
      assertTrue(Line.isIntersecting(lineAB, lineCD))
    },

    test("intersecting check should fail if both lines are same") {
      val line = Line(Point(0,5), Point(0,5))
      assertTrue(!Line.isIntersecting(line, line))
    },

  )
}
