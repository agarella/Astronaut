import scala.language.implicitConversions

import cats.Semigroup
import shapeless._

trait Show[A] {
  def show(a: A): String
}

object Show {

  def apply[A](implicit show: Show[A]): Show[A] = show

  implicit val showInt = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit val showString = new Show[String] {
    def show(a: String): String = a.toString
  }

  implicit val showHNil = new Show[HNil] {
    def show(a: HNil): String = ""
  }

  implicit def hlistShow[H, T <: HList](
    implicit
    h: Lazy[Show[H]],
    t: Show[T]
  ): Show[H :: T] = (a: H :: T) => h.value.show(a.head) + " " + t.show(a.tail)

  implicit val showCNil = new Show[CNil] {
    def show(a: CNil): String = throw new Exception("CNil")
  }

  implicit def coproductShow[H, T <: Coproduct](
    implicit
    hShow: Lazy[Show[H]],
    tShow: Show[T]
  ): Show[H :+: T] =
    new Show[H :+: T] {
      def show(a: H :+: T): String = a match {
        case Inl(h) => hShow.value.show(h)
        case Inr(r) => tShow.show(r)
      }
    }

  implicit def genericShow[A, Repr](
    implicit
    a: Generic.Aux[A, Repr],
    t: Lazy[Show[Repr]]): Show[A] =
    new Show[A] {
      def show(f: A): String = t.value.show(a.to(f))
    }
}

object AutoSemigroup {

  def apply[A](implicit a: Semigroup[A]) = a

  implicit val hBase = new Semigroup[HNil] {
    def combine(x: HNil, y: HNil): HNil = HNil
  }

  implicit def hStep[H, T <: HList](
    implicit
    h: Lazy[Semigroup[H]],
    t: Semigroup[T]
  ): Semigroup[H :: T] = new Semigroup[H :: T] {
    def combine(x: H :: T, y: H :: T): H :: T = h.value.combine(x.head, y.head) :: t.combine(x.tail, y.tail)
  }

  implicit val cBase = new Semigroup[CNil] {
    def combine(x: CNil, y: CNil): CNil = throw new Exception("Autosemigroup CNil")
  }

  implicit def cStep[H, T <: Coproduct](
    implicit
    hSemigroup: Lazy[Semigroup[H]],
    tSemigroup: Semigroup[T]
  ): Semigroup[H :+: T] = new Semigroup[H :+: T] {
    def combine(x: H :+: T, y: H :+: T): H :+: T =
      (x, y) match {
        case (Inl(h1), Inl(h2)) => Inl(hSemigroup.value.combine(h1, h2))
        case (Inr(t1), Inr(t2)) => Inr(tSemigroup.combine(t1, t2))
        case _ => throw new Exception("Autosemigroup CNil")
      }
  }

  implicit def deriveSemigroup[A, Repr](
    implicit
    gen: Generic.Aux[A, Repr],
    genericSemigroup: Lazy[Semigroup[Repr]]
  ): Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = gen.from(genericSemigroup.value.combine(gen.to(x), gen.to(y)))
  }

}

sealed trait Animal

final case class Cat(age: Int, color: String, other: Foo) extends Animal

final case class Dog(age: Int, color: String, race: String, other: Foo) extends Animal

sealed trait Foo

case class Bar(x: String) extends Foo

case class Baz(x: Int) extends Foo


object Astronaut extends App {
  val cat = Cat(1, "Red", Bar("Alex"))
  val dog = Dog(1, "Red", "Fool", Baz(1))

  println(s"Dog: ${Show[Animal].show(dog)}")
  println(s"Cat: ${Show[Animal].show(cat)}")

  import AutoSemigroup._
  import cats.syntax.semigroup._
  import cats.instances.int._
  import cats.instances.string._

  println(Show[Animal].show(cat))
  println(cat |+| cat)
}