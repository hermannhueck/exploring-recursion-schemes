package my
package cataimpl

import scala.util.chaining._
import recursion._
import my.functor.Functor

/*
  Our catamorphism is generic.
  Hence we cannot use it merely for a CalcF,
  but also for a ListF, we define in this example.
 */
object Cata14List extends util.App {

  import fixpoint._

  sealed trait ListF[+A] extends Product with Serializable {
    def fix: Fix[ListF] = ListF.fix(this)
  }

  object ListF {

    case class NilF[A]()                    extends ListF[A]
    case class ConsF[A](head: Int, tail: A) extends ListF[A]

    // converts a List[Int] into a ListF[A]
    def list2ListF[A]: List[Int] => ListF[A] = {
      case Nil          => NilF()
      case head :: tail => ConsF(head, tail.asInstanceOf[A])
    }

    final implicit class ListSyntax(li: List[Int]) {
      def toListF[A]: ListF[A] = list2ListF(li)
    }

    private def fixList[A](a: A): Fix[ListF] =
      fix apply a.asInstanceOf[List[Int]].toListF

    def fix[A]: ListF[A] => Fix[ListF] = {
      case NilF() =>
        Fix(NilF[Fix[ListF]]()) // needs type param for type inference
      case ConsF(head, tail) =>
        Fix(ConsF(head, fixList(tail)))
    }

    // pattern functor for ListF
    implicit val functorListF: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](fa: ListF[A])(f: A => B): ListF[B] =
        fa match {
          case NilF()            => NilF()
          case ConsF(head, tail) => ConsF(head, f(tail))
        }
    }

    val sumAlgebra: ListF[Int] => Int = {
      case NilF()            => 0
      case ConsF(head, tail) => head + tail
    }

    val productAlgebra: ListF[Int] => Int = {
      case NilF()            => 1
      case ConsF(head, tail) => head * tail
    }

    val lengthAlgebra: ListF[Int] => Int = {
      case NilF()         => 0
      case ConsF(_, tail) => 1 + tail
    }

    val concatSquaresAlgebra: ListF[String] => String = {
      case NilF()                            => ""
      case ConsF(head, tail) if tail.isEmpty => (head * head).toString
      case ConsF(head, tail)                 => s"${(head * head).toString} + $tail"
    }

    val sumOfSquaresAlgebra: ListF[Int] => Int = {
      case NilF()            => 0
      case ConsF(head, tail) => head * head + tail
    }
  }

  import ListF._

  val l1 =
    List(1, 2, 3, 4, 5)
      .tap(println)

  println
  l1.toListF.fix.cata(sumAlgebra) pipe (sum => println(s"sum: $sum"))
  l1.toListF.fix.cata(productAlgebra) pipe (product => println(s"product: $product"))
  l1.toListF.fix.cata(lengthAlgebra) pipe (length => println(s"length: $length"))
  l1.toListF.fix.cata(concatSquaresAlgebra) pipe (concatSquares => println(s"concatSquares: $concatSquares"))
  l1.toListF.fix.cata(sumOfSquaresAlgebra) pipe (sumOfSquares => println(s"sumOfSquares: $sumOfSquares"))
}
