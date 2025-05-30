import zio.prelude.Validation

case class Line(a: Point, b: Point) {
  def dy = b.yCoordinate - a.yCoordinate
  def dx = b.xCoordinate - a.xCoordinate
  def length = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))

  val isPositive = (number: Double) => !isNegative(number)
  val isNegative = (number: Double) => number < 0

  val slope: Double = {
    (dy, dx) match {
      case (dy, dx) if isNegative(dy) && dx == 0 => Double.NegativeInfinity
      case (dy, dx) if isPositive(dy) && dx == 0 => Double.PositiveInfinity
      case _=> dy.toDouble / dx.toDouble
    }
  }

  val directionVector: List[Int] = List(b.xCoordinate-a.xCoordinate, b.yCoordinate-a.yCoordinate)
  val magnitude = Math.sqrt(directionVector.map(Math.pow(_, 2)).sum)
}

object Line {
  val isSame = (lineAB: Line, lineCD: Line) => lineAB == lineCD
  val isNotSame = (lineAB: Line, lineCD: Line) => !isSame(lineAB, lineCD)

  val isParallel = (lineAB: Line, lineCD: Line) => isNotSame(lineAB, lineCD) && Math.abs(lineAB.slope) == Math.abs(lineCD.slope)
  val isPerpendicular = (lineAB: Line, lineCD: Line) => isNotSame(lineAB, lineCD) && lineAB.slope == -(1/lineCD.slope)
  val isIntersecting = (lineAB: Line, lineCD: Line) => isNotSame(lineAB, lineCD) && !isParallel(lineAB, lineCD)

  val dotProduct = (lineAB: Line, lineCD: Line) =>
    (lineAB.directionVector zip lineCD.directionVector).map(pair => {
      val (diffX,diffY) = pair
      diffX*diffY
    }).sum

  val angleBetween = (lineAB: Line, lineCD: Line) => {
    Math.toDegrees(Math.acos(dotProduct(lineAB, lineCD).toDouble / (lineAB.magnitude * lineCD.magnitude).toDouble))
  }

  def validateLine(line: Line): Validation[String, Line] = {
    if(line.a == line.b)
      Validation.fail("Invalid line.")
    else
      Validation.succeed(line)
  }
}
