/*
 * Implement a function product which  multiplies all elements of a list
 */

val a = 10 :: 5 :: 1 :: Nil

// using a foreach
def productList(xs: List[Int]): Int = {
  var product = 1
  xs.foreach(product *= _)
  product
}

productList(a)

// using recursion
def productList2(xs: List[Int]): Int = {
  xs match {
    case Nil    => 1
    case h :: t => h * productList2(t)
  }
}

productList2(a)

// using a standard scala library function
a.product
a.foldRight(1)(_ * _)

/*
 * Implement a function range which gives you a range of numbers starting with the given number to 1
 */
val b = 10

// using while loop
def range(b: Int): List[Int] = {
  var xs = List.empty[Int]
  var i = 1
  while (i <= b) {
    xs = i :: xs
    i = i + 1
  }
  xs
}

range(b)

// using recursion
def range2(b: Int): List[Int] = {
  if (b > 0)
    b :: range2(b - 1)
  else
    Nil
}

range2(b)

// using a standard scala library function
List.unfold(b) { x =>
  if (x <= 0)
    None
  else
    Some((x, x - 1))
}
