package colonizers.util

object RichAnys {
  implicit class RichAnysClass(any: Any) {
    def cast[T]: Option[T] = {
      any match {
        case t: T => Some(t)
        case _ => None
      }
    }
  }
}
