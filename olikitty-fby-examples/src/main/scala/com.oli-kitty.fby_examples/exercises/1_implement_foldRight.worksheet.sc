/**
  * Implement foldRight function
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1
def product(x: Int, y: Int) = x * y

// using a recursion
def foldRight1[A, E](init: List[E])(z: A)(op: (E, A) => A): A = {
  init match {
    case Nil          => z
    case head :: tail => op(head, foldRight1(tail)(z)(op))
  }
}

foldRight1(list)(seed)(product)

// improve recursion using a nested function
def foldRight2[A, E](init: List[E])(z: A)(op: (E, A) => A): A = {
  def kernel(xs: List[E]): A = xs match {
    case Nil          => z
    case head :: tail => op(head, kernel(tail))
  }
  kernel(init)
}

foldRight2(list)(seed)(product)

// Let's return a function
def foldRight3[A, E](z: A)(op: (E, A) => A): List[E] => A = {
  def kernel(xs: List[E]): A = xs match {
    case Nil          => z
    case head :: tail => op(head, kernel(tail))
  }
  kernel
}

foldRight3(seed)(product)(list)

// use anonymous function
def foldRight4[A, E](z: A)(op: (E, A) => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A = xs match {
      case Nil          => z
      case head :: tail => op(head, self(tail))
    }
  }
}

foldRight4(seed)(product)(list)
