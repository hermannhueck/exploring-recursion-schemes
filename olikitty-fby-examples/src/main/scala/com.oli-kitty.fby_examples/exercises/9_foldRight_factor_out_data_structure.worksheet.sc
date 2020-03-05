import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

/**
  * Let's factor all steps out of the function
  */
type ListF[A, B] = Option[(A, B)]
implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

def projectList[E]: List[E] => ListF[E, List[E]] = {
  case Nil          => None
  case head :: tail => Some((head, tail))
}

// reimplement foldRight
def foldRight[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B = {
  new (S => B) { self =>
    def apply(init: S): B = f(project(init).fmap(self))
  }
}

def prodFlist: ListF[Int, Int] => Int = {
  case None         => 1
  case Some((x, y)) => x * y
}

val list = 5 :: 4 :: 1 :: Nil

foldRight(prodFlist)(projectList).apply(list)
