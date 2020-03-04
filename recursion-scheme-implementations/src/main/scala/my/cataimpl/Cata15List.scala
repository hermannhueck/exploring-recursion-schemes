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
object Cata15List extends util.App {

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

    def algebra[A](onNilF: A)(onConsF: (Int, A) => A): ListF[A] => A = {
      case NilF()            => onNilF
      case ConsF(head, tail) => onConsF(head, tail)
    }

    val sumAlgebra: ListF[Int] => Int = algebra(0)(_ + _)

    val productAlgebra: ListF[Int] => Int = algebra(1)(_ * _)

    val lengthAlgebra: ListF[Int] => Int = algebra(0)((_, t) => 1 + t)

    val concatSquaresAlgebra: ListF[String] => String = algebra("") {
      case (head, tail) =>
        if (tail.isEmpty)
          (head * head).toString
        else
          s"${(head * head).toString} + $tail"
    }

    val sumOfSquaresAlgebra: ListF[Int] => Int = algebra(0) {
      case (head, tail) => head * head + tail
    }

    def zipAlgebras[A, B](algA: ListF[A] => A, algB: ListF[B] => B): ListF[(A, B)] => (A, B) = {
      case NilF() =>
        val a = algA(NilF())
        val b = algB(NilF())
        (a, b)
      case ConsF(head, tail) =>
        val a = algA(ConsF(head, tail._1))
        val b = algB(ConsF(head, tail._2))
        (a, b)
    }

    val squares = zipAlgebras(concatSquaresAlgebra, sumOfSquaresAlgebra)

    val sumLength = zipAlgebras(sumAlgebra, lengthAlgebra)
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
  l1.toListF.fix.cata(squares) pipe (squares => println(s"squares: $squares"))
  l1.toListF.fix.cata(squares) pipe (squares => println(s"squares: ${squares._1} = ${squares._2}"))
  l1.toListF.fix.cata(sumLength) pipe (sumLength => println(s"sumLength: $sumLength"))
  l1.toListF.fix.cata(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println
  Nil.toListF.fix.cata(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println

  def average(sum: Int, length: Int): String =
    if (length > 0)
      s"average: ${sum / length}"
    else
      "empty List -> no average !!!"
}
