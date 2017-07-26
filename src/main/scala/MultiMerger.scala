import shapeless._
import shapeless.ops.record._

trait MultiMerger[L <: HList] {
  type Out <: HList
  def apply(l: L): Out
}

/**
  * Typeclass to merge fields of case classes into one HList
  * val multiMerge = MultiMerger[Foo :: Bar :: HNil].build
  * multiMerge(Foo(1) :: Bar("bar") :: HNil)
  * Results in 1 :: "bar" :: HNil
  */
object MultiMerger {

  type Aux[A <: HList, B <: HList] = MultiMerger[A] { type Out = B }

  implicit lazy val base: MultiMerger.Aux[HNil, HNil] = new MultiMerger[HNil] {
    type Out = HNil
    def apply(h: HNil): Out = h
  }

  implicit def inductive[H, Repr <: HList, Out <: HList, T <: HList](
    implicit labelledGeneric: LabelledGeneric.Aux[H, Repr],
    multiMerge: MultiMerger.Aux[T, Out],
    merge: Merger[Repr, Out]
  ): MultiMerger.Aux[H :: T, merge.Out] = new MultiMerger[H :: T] {
    type Out = merge.Out
    def apply(ht: H :: T) = merge(labelledGeneric.to(ht.head), multiMerge(ht.tail))
  }

  class PartiallyApplied[L <: HList] {
    def build[Out <: HList](implicit multiMerge: MultiMerger.Aux[L, Out]) = multiMerge
  }

  def apply[L <: HList] = new PartiallyApplied[L]
}