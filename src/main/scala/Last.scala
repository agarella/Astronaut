import shapeless._

trait Last[A <: HList] {
  type Out
  def apply(t: A): Out
}

object Last {

  implicit def base[H]: Last[H :: HNil] = new Last[H :: HNil]{
    type Out = H
    override def apply(t: H :: HNil): H = t.head
  }

  implicit def inductive[H, T <: HList](implicit last: Last[T]): Last[H :: T] = new Last[H :: T] {
    type Out = last.Out
    def apply(t: H :: T): Out = last(t.tail)
  }

  def apply[L <: HList](implicit l: Last[L]) = l
}
