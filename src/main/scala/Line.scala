case class Line(a: Point, b: Point) {
  def dy = b.yCoordinate - a.yCoordinate
  def dx = b.xCoordinate - a.xCoordinate
  def length = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))

  val isPositive = (number: Double) =>  number >= 0
  val isNegative = (number: Double) => number < 0

  def slope = {
    (dy, dx) match {
      case (dy, dx) if isNegative(dy) && isPositive(dx) && dx == 0 => Double.NegativeInfinity
      //case (dy, dx) if isPositive(dy) && isNegative(dx) && dx == 0 => Double.NegativeInfinity
      case _=> dy / dx
    }
  }
}
