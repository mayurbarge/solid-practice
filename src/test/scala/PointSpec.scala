import zio._
import zio.test._
import zio.test.Assertion._

import java.io.IOException


object PointSpec extends ZIOSpecDefault {
  def spec = suite("PointSpec")(
    test("Point should get created with x and y coordinates") {
      val point = Point(xCoordinate = 10, yCoordinate = 20)
      assertTrue(point == Point(10, 20))
    }
  )
}