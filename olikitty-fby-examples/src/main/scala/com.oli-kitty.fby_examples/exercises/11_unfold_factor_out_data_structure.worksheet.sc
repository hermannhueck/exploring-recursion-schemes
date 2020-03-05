import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

/**
  * Let's factor all steps out of the function
  */
type ListF[A, B] = Option[(A, B)]

implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

def embedList[E]: ListF[E, List[E]] => List[E] = {
  case None          => Nil
  case Some((e, le)) => e :: le
}

def range: Int => ListF[Int, Int] =
  v => if (v <= 0) None else Some((v, v - 1))

// reimplement unfold
def unfold[F[_]: Functor, S, B](f: B => F[B])(embed: F[S] => S): B => S = {
  new (B => S) { self =>
    def apply(b: B): S =
      embed(f(b).fmap(self))
  }
}

unfold1(range)(embedList).apply(10)

// reimplement unfold
def unfold1[F[_]: Functor, S, B](f: B => F[B])(embed: F[S] => S): B => S = {
  def func(b: B): S = embed(f(b).fmap(func))
  func
}

unfold1(range)(embedList).apply(10)
