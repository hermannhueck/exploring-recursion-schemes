package freecofree

/*
  See blog post:
  https://free.cofree.io/2017/11/13/recursion/
 */
package object recursion {

  // import scalaz._, Scalaz._

  final implicit class FunctorSyntax[F[_]: Functor, A](private val fa: F[A]) {
    @inline def map[B](f: A => B): F[B] =
      Functor[F].map(fa)(f)
  }

  // ===== Anamorphism ===============

  // Anamorphism is a type of recursion scheme, which takes a function of type A => F[A]
  // (also known as a Coalgebra) where F is a functor, and returns a function of type A => Fix[F],
  // that takes an A and unfolds it into a recursive structure, Fix[F]. The implementation
  // is quite simple if guided by the type signature, in fact it is the only sensible implementation
  // that compiles:

  // Type A => F[A] is also known as Coalgebra.
  def ana[F[_]: Functor, A](f: A => F[A]): A => Fix[F] =
    a => Fix(f(a) map ana(f))

  // That is, we only need to supply a coalgebra, A => F[A] which does not need to be recursive,
  // in order to generate the recursive structure. This is what we mean by “factoring recursion out”:
  // your coalgebra only needs to contain the core business logic, and the recursion is taken care of by ana.

  // ===== Katamorphism ===============

  // Catamorphism is the dual of anamorphism, and can be implemented by reversing the arrows in ana:

  // Type F[A] => A is also known as Algebra.
  def cata[F[_]: Functor, A](f: F[A] => A): Fix[F] => A =
    fix => f(fix.unfix map cata(f))

  // As with ana, we only need to provide an algebra, F[A] => A, and the recursion is taken care of by cata.

  // ===== Hylomorphism ===============

  // Hylomorphism is the composition of anamorphism and catamorphism, and can be implemented simply as

  def hyloSimple[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    ana(g) andThen cata(f)

  // hyloSimple first uses ana to build a potentially large structure, before using cata
  // to tear it down. Alternatively, we can implement hylomorphism directly without using ana or cata,
  // which “fuses” the anamorphism and the catamorphism, meaning at no time do we have
  // a large Fix structure in memory.

  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    a => f(g(a) map hylo(f)(g))

  // ===== Paramorphism ===============

  // Paramorphism is also a generalization of fold. It is an extension of catamorphism,
  // and offers more power. Here’s the type signature and implementation:

  def para[F[_]: Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
    fix => f(fix.unfix.map(fix => fix -> para(f).apply(fix)))

  // We can implement cata in terms of para:

  def cataViaPara[F[_]: Functor, A](f: F[A] => A): Fix[F] => A =
    para(((_: F[(Fix[F], A)]).map(_._2)) andThen f)

  // Paramorphism is more powerful than catamorphism in the sense that in the algebra f,
  // we not only have an F[A] to work with, but we also have an F[Fix[F]], which means
  // we have access to the Fix structure that yields the A when being folded.

  // ===== Apomorphism ===============

  // Apomorphism is the dual of paramorphism, and is an extension of anamorphism.

  // Note that in addition to reversing the arrows in para, we also change F[(Fix[F], A)]
  // to F[Either[Fix[F], A]], since the dual of a pair (product type) is an Either (sum type).

  // Not surprisingly, ana can be implemented in terms of apo:

  def apo[F[_]: Functor, A](f: A => F[Either[Fix[F], A]]): A => Fix[F] =
    a =>
      Fix(f(a) map {
        case Left(fix) => fix
        case Right(aa) => apo(f).apply(aa)
      })

  import compat213.either._
  def anaViaApo[F[_]: Functor, A](f: A => F[A]): A => Fix[F] =
    // apo(f andThen (_.map(_.asRight[Fix[F]])))
    apo(f andThen (_.map(Right(_).withLeft[Fix[F]])))

  // ===== Histomorphism ===============

  // Histomorphism is yet another recursion scheme for fold, and is more powerful than paramorphism.
  // Histomorphism operates on an enhanced version of Fix, called Cofree, where each node
  // in the structure is annotated by some value.

  // final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  // The implementation of histomorphism requires a helper function toCofree to convert
  // a Fix[F] to a Cofree[F, A], where each node is annotated by the value generated
  // by folding the corresponding Fix[F] structure:

  def histo[F[_]: Functor, A](f: F[Cofree[F, A]] => A): Fix[F] => A = {

    def toCofree: Fix[F] => Cofree[F, A] =
      fix => Cofree(head = histo(f).apply(fix), tail = fix.unfix map toCofree)

    fix => f(fix.unfix map toCofree)
  }

  // Recall that in catamorphism, at each step of the fold, you only have the value
  // of the current fold, F[A]. In paramorphism, you additionally have access to the structure
  // that generated that value, F[Fix[F]]. And in histomorphism, additionally, you also have
  // the history of the values generated by the fold so far, or the history of the computation if you will.

  // ===== Dynamorphism ===============

  // Dynamorphism is the composition of anamorhpism and histomorphism.
  // Similar as hylomorphism, it can be implemented either via ana and histo:

  def dynaSimple[F[_]: Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B =
    ana(g) andThen histo(f)

  // or directly:

  def dyna[F[_]: Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B = {
    val cofree: F[Cofree[F, B]] => Cofree[F, B] =
      fc => Cofree(f(fc), fc)
    a => hylo(cofree)(g).apply(a).head
  }

  // The latter implementation avoids building the whole Fix structure.

  // ===== Futumorphism ===============

  // The last recursion scheme we are going to cover in this post is futumorphism.
  // It sounds like the dual of histomorphism, and it indeed is. The dual of Cofree is,
  // unsurprisingly, the following Free type:

  /*
  sealed trait Free[F[_], A]
  final case class Continue[F[_], A](a: A) extends Free[F, A]
  final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

  object Free {
    def continue[F[_], A](a: A): Free[F, A] = Continue(a)
    def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
  }
   */

  // Each Cofree has a recursive structure tagged with a value of type A, while each Free
  // has either a recursive structure, or a tag with a value of type A.

  // Given Free, here’s the definition and implementation of futumorphism:

  def futu[F[_]: Functor, A](f: A => F[Free[F, A]]): A => Fix[F] = {

    def toFix: Free[F, A] => Fix[F] = {
      case Continue(a) => futu(f).apply(a)
      case Combine(fa) => Fix(fa map toFix)
    }

    a => Fix(f(a) map toFix)
  }

  // Futumorphism is another unfold scheme, and is a more powerful one than apomorphism.
  // Recall that in apomorphism, given a value of A, you either choose to continue the recursion
  // by returning a Right, or choose to stop the recursion by returning a Left. In futumorphism,
  // you can also choose to continue the recursion by returning a Continue or stop by returning a Combine.
  // Additionally, you will be able to unfold multiple steps at a time, which you cannot do with apomorphism.

  // In anamorphism, to build the stack corresponding to the computation of the factorial of 5,
  // you’ll need 5 recursive steps. In apomorphism, as we’ve seen earlier, you can effectively combine
  // the last three steps together, by terminating the recursion at n = 3 and returning lastThreeSteps.
  // But once the recursion is terminated, it’s terminated. You cannot, for example, combine the first
  // three steps together, and then resume the recursion. On the other hand, you can do that with futumorphism.

  // The reason you can do that with futumorphism is because a Free is either a single A
  // wrapped in Continue or a recursive structure wrapped in Combine. To combine multiple steps together,
  // just have your coalgebra return a Combine wrapping a recursive structure corresponding to those steps;
  // to resume the normal recursion, just have it return a Continue wrapping a single A.
}
