/**
  * Implement unfold function
  */
def range(x: Int): Option[(Int, Int)] =
  if (x > 0) Some((x, x - 1)) else None

// using recursion
def unfold1[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = {
  f(init) match {
    case None         => Nil
    case Some(e -> b) => e :: unfold1(b)(f)
  }
}

unfold1(10)(range)

// improve recursion using a nested function
def unfold2[E, B](init: B)(f: B => Option[(E, B)]): List[E] = {
  def kernel(value: B): List[E] = f(value) match {
    case None         => Nil
    case Some(e -> b) => e :: kernel(b)
  }
  kernel(init)
}

unfold2(10)(range)

def unfold3[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>
    def apply(value: B): List[E] = f(value) match {
      case None         => Nil
      case Some(e -> b) => e :: self(b)
    }
  }
}

unfold3(range)(10)
