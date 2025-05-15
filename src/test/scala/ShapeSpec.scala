import QuadrilateralSpec.test
import TriangleSpec.{suite, test}
import zio.test._
import zio.test.Assertion._
import zio.prelude.{NonEmptyList, NonEmptyMultiSet, Validation}

object ShapeSpec extends ZIOSpecDefault {
  val lineAB = Line(Point(0,0), Point(1, 0))
  val lineBC = Line(Point(1, 0), Point(2, 0))
  val lineCD = Line(Point(2, 0), Point(3, 0))
  val lineDA = Line(Point(3, 0), Point(4, 0))

  val edges = List(lineAB, lineBC, lineCD, lineDA)

  def spec = suite("CombinedSpec")(
    shapeSpec,
    genericShapeSpec
  )
  def shapeSpec = suite("ShapeSpec")(
    test("InvalidShape does not have any properties") {
      val shape = Shape()
      assertTrue(shape == InvalidShape)
    },

    test("Given four edges Quadrilateral Shape should be created") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val edges = NonEmptyList(lineAB, lineBC, lineCD, lineDA)

      val validatedShape = Shape(edges)
      assertTrue(validatedShape.isInstanceOf[Quadrilateral])
    },

    test("Given four edges Square Shape should be created") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1,0), Point(1,1))
      val lineCD = Line(Point(1,1), Point(0,1))
      val lineDA = Line(Point(0,1), Point(0,0))
      val edges = NonEmptyList(lineAB, lineBC, lineCD, lineDA)

      val validatedShape = Shape(edges)
      assertTrue(validatedShape.isInstanceOf[Square])
    },
  )
  def genericShapeSpec = suite("GenericShapeSpec")(

    test("Shape is closed when for each edge if starting point of one edge is ending point of other and vice versa") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(1, 1))
      val lineCD = Line(Point(1, 1), Point(0, 1))
      val lineDA = Line(Point(0, 1), Point(0, 0))

      val shapeWithEdges = new GenericShape(List(lineAB, lineBC, lineCD, lineDA)) {
        val area = None
      }
      assertTrue(Shape.isShapeClosed(shapeWithEdges))
    },

    test("Shape is not defined when it is not closed by edges") {
      val lineAB = Line(Point(0,0), Point(1, 0))
      val lineBC = Line(Point(1, 0), Point(2, 0))
      val lineCD = Line(Point(2, 0), Point(3, 0))
      val lineDA = Line(Point(3, 0), Point(4, 0))

      val shapeWithEdges = new GenericShape(List(lineAB, lineBC, lineCD, lineDA)) {
        val area = None
      }
      assertTrue(!Shape.isShapeClosed(shapeWithEdges))
    },

  )
}
