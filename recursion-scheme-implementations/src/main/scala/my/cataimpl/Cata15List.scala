package my
package cataimpl

import scala.util.chaining._
import recursion._
import my.functor.Functor

/*
  Our self-defined catamorphism is generic. Hence we cannot use it merely for a CalcF,
  but also for a ListF, which we define in this example.

  Instead of Calc and CalcF we use the recursive structures List[Int] (from the standard library)
  and a self defined ListF[+A]:

    sealed trait ListF[+A]

    object ListF {
      case class NilF[A]()                    extends ListF[A]
      case class ConsF[A](head: Int, tail: A) extends ListF[A]
    }

  This definition again removes recursion from the type.
  tail just hast type A instead of ListF[A] to avoid type recursion.

  list2ListF converts a List[Int] into a ListF[A]:

    def list2ListF[A]: List[Int] => ListF[A] = ???

  Using the extension method toListF we can invoke the conversion directly on a List[Int].

  With the ListF.fix function  we can create a nested 'Fix' structure of any depth.

    def fix[A]: ListF[A] => Fix[ListF] = ???

  The method CalcF#fix allows us to invoke 'fix' directly on a ListF instance.

  To use 'cata' we have to provide a Pattern Functor for ListF as an implicit instance.
  Again, the Pattern Functor allows for recursion in a generic way.

    implicit val functorListF: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](fa: ListF[A])(f: A => B): ListF[B] =
        fa match {
          case NilF()            => NilF()
          case ConsF(head, tail) => ConsF(head, f(tail))
        }
    }

  Now we create different Algebras: ListF[A] => A.
  The simplest one is sumAlgebra: ListF[Int] => Int.
  But there are others, see below:

    val sumAlgebra: ListF[Int] => Int = {
      case NilF()            => 0
      case ConsF(head, tail) => head + tail
    }

  All ingredients are there to invoke cata passing the algebra:

    val sum: Int =
      List(1, 2, 3, 4, 5)
      .toListF.fix.cata(sumAlgebra)

  Summary of all steps:
  ---------------------

  - For a given recursive Structure (List[Int] in this case) create a higher-kinded
    structure (ListF[A]) such that the recursive type is no longer visible and replaced by an A.
    List[Int] is recursive in the tail of the :: (cons), which itsself is a List[Int].
    In ListF[A] the tail get type A to swallow the recursive type away.

  - Define two function to convert from the recursive type to the higher-kinded type and back
    (list2ListF and listF2List, the latter is not needed in our example)

  - Define a fix function which allow you to create a nested Fix structure from your
    higher-kinded structure:    def fix: ListF[A] => Fix[ListF]

  - Define a Pattern Functor on the higher-kinded structure. in the implementation of map
    the function f: A => B should be applied to the recursive part of the structure
    (tail in our case).

  - Define the Algebras of type F[A] => A, (in our case: ListF[A] => A).

  - Invocation:
    - Convert the instance of the recursive type (List[Int]) into the higer-kinded structure (ListF[A]).
    - Convert the higher-kinded structure (ListF[A]) into a nested Fix structure (Fix(ListF))
    - Invoke cata on the Fix passing an Algebra.

    val result: Int =
      List(1, 2, 3, 4, 5)
        .toListF
        .fix
        .cata(sumAlgebra)
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
