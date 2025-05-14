import TriangleSpec.{suite, test}
import zio.test._
import zio.test.Assertion._
import zio.prelude.Validation

object ShapeSpec extends ZIOSpecDefault {
  val lineAB = Line(Point(0,0), Point(1, 0))
  val lineBC = Line(Point(1, 0), Point(2, 0))
  val lineCD = Line(Point(2, 0), Point(3, 0))
  val lineDA = Line(Point(3, 0), Point(4, 0))

  val edges = List(lineAB, lineBC, lineCD, lineDA)
  def spec = suite("ShapeSpec")(
    test("InvalidShape does not have any properties") {
      val shape = Shape()
      assertTrue(shape == InvalidShape)
    },

    test("InvalidShape does not contain any edges") {
      val shape = Shape(List.empty[Line])
      assertTrue(shape == InvalidShape)
    },

  )
}
