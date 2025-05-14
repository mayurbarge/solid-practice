import zio.prelude.Validation

trait Shape {
}
case object InvalidShape extends Shape
object Shape {
  def apply() = InvalidShape
  def apply(edges: List[Line]) = InvalidShape
}
