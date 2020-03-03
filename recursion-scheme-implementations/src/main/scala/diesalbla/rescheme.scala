package diesalbla

/**
  * Code Snippets from the Blog Post "A lightweight introduction to Recursion Schemes in Scala"
  * by Diego Alonso:
  * https://www.47deg.com/blog/basic-recursion-schemes-in-scala/
  *
  * Written here to accompany the article. This file should compile  directly if inserted into ammonite.
  */
object IListBase {
  sealed trait IList
  case object INil                         extends IList
  case class ICons(head: Int, tail: IList) extends IList
}

object ITreeBase {
  sealed trait ITree
  case object Leaf                                     extends ITree
  case class Node(left: ITree, top: Int, right: ITree) extends ITree
}

/* Steps 1-2: Initials definitions, directly recursive, of `sum` and `digits` */
object directRecursion {
  import IListBase._

  def sum(list: IList): Int =
    list match {
      case INil              => 0
      case ICons(head, tail) => head + sum(tail)
    }

  def digits(seed: Int): IList =
    if (seed == 0)
      INil
    else
      ICons(seed % 10, digits(seed / 10))
}

/* Steps 1-2: */
object monomorphicList {
  import IListBase._

  def fold(zero: Int, op: (Int, Int) => Int)(list: IList): Int =
    list match {
      case INil              => zero
      case ICons(head, tail) => op(head, fold(zero, op)(tail))
    }

  def unfold(isEnd: Int => Boolean, op: Int => (Int, Int))(seed: Int): IList =
    if (isEnd(seed))
      INil
    else {
      val (head, next) = op(seed)
      ICons(head, unfold(isEnd, op)(next))
    }

  def isZero(x: Int): Boolean   = x == 0
  def div10(x: Int): (Int, Int) = (x % 10, x / 10)
  def digits(seed: Int): IList  = unfold(isZero, div10)(seed)

  def add(a: Int, b: Int)   = a + b
  def sum(list: IList): Int = fold(0, add)(list)
}

/*  Step 3: Introduce an "OCons" type alias, and write fold/unfold of lists based on it. */
object v3 {
  import IListBase._

  type OCons = Option[(Int, Int)]

  def fold(out: OCons => Int)(list: IList): Int =
    list match {
      case INil              => out(None)
      case ICons(head, tail) => out(Some((head, fold(out)(tail))))
    }

  def unfold(into: Int => OCons)(seed: Int): IList =
    into(seed) match {
      case None               => INil
      case Some((head, next)) => ICons(head, unfold(into)(next))
    }

  def addO(ocons: OCons): Int = ocons match {
    case None         => 0
    case Some((x, y)) => x + y
  }

  def split(x: Int): OCons =
    if (x == 0)
      None
    else
      Some((x % 10, x / 10))

  def digits(seed: Int): IList = unfold(split)(seed)
  def sum(list: IList): Int    = fold(addO)(list)
}

/* Step 4: define fold, unfold for treees */
object treeDirect {
  import ITreeBase._

  def sum(tree: ITree): Int =
    tree match {
      case Leaf              => 0
      case Node(ll, top, rr) => sum(ll) + top + sum(rr)
    }

  def digits(seed: Int): ITree =
    if (seed == 0)
      Leaf
    else {
      val (pref, mid, suff) = splitNumber(seed)
      Node(digits(pref), mid, digits(suff))
    }

  // splitNumbers: split a number's digits in the middle,
  // for example,  splitNumber(56784197) = (567, 8, 4197)
  def splitNumber(seed: Int): (Int, Int, Int) = (0, seed, 0) // TODO

}

/* Step 5: define monomorphic fold/unfold for trees */
object treeUnFold {
  import ITreeBase._

  def fold(zero: Int, op: (Int, Int, Int) => Int)(tree: ITree): Int =
    tree match {
      case Leaf =>
        zero
      case Node(ll, top, rr) =>
        op(fold(zero, op)(ll), top, fold(zero, op)(rr))
    }

  def add3(a: Int, b: Int, c: Int) = a + b + c
  def sum(tree: ITree): Int        = fold(0, add3)(tree)

  def unfold(isEnd: Int => Boolean, op: Int => (Int, Int, Int))(seed: Int): ITree =
    if (isEnd(seed))
      Leaf
    else {
      val (ll, top, rr) = op(seed)
      Node(unfold(isEnd, op)(ll), top, unfold(isEnd, op)(rr))
    }

  def isZero(x: Int): Boolean                 = x == 0
  def splitNumber(seed: Int): (Int, Int, Int) = (0, seed, 0)
  def digits(seed: Int): ITree                = unfold(isZero, splitNumber)(seed)
}

object step6 {
  import ITreeBase._
  type ONode = Option[(Int, Int, Int)]

  def fold(out: ONode => Int)(tree: ITree): Int =
    tree match {
      case Leaf =>
        out(None)
      case Node(ll, top, rr) =>
        out(Some((fold(out)(ll), top, fold(out)(rr))))
    }

  def unfold(in: Int => ONode)(seed: Int): ITree =
    in(seed) match {
      case None =>
        Leaf
      case Some((ll, top, rr)) =>
        Node(unfold(in)(ll), top, unfold(in)(rr))
    }
}

/* Step 7: Using maps. We define here */
object OptTypes {
  type OCons[R] = Option[(Int, R)]
  type ONode[R] = Option[(R, Int, R)]

  def mapOC[A, B](fun: A => B, ocons: OCons[A]): OCons[B] =
    ocons match {
      case None               => None
      case Some((head, tail)) => Some((head, fun(tail)))
    }

  def mapON[A, B](fun: A => B, onode: ONode[A]): ONode[B] =
    onode match {
      case None                => None
      case Some((ll, top, rr)) => Some((fun(ll), top, fun(rr)))
    }

}

object v7 {
  import OptTypes._
  import IListBase._
  import ITreeBase._

  def open(tree: IList): OCons[IList] =
    tree match {
      case INil              => None
      case ICons(head, tail) => Some((head, tail))
    }

  def open(tree: ITree): ONode[ITree] =
    tree match {
      case Leaf              => None
      case Node(ll, top, rr) => Some((ll, top, rr))
    }

  def foldL(out: OCons[Int] => Int)(list: IList): Int =
    out(mapOC(foldL(out), open(list)))

  def foldT(out: ONode[Int] => Int)(tree: ITree): Int =
    out(mapON(foldT(out), open(tree)))

  // STEP 9 - Align Unfolds
  def close(ocons: OCons[IList]): IList =
    ocons match {
      case None               => INil
      case Some((head, tail)) => ICons(head, tail)
    }

  def close(onode: ONode[ITree]): ITree =
    onode match {
      case None                => Leaf
      case Some((ll, top, rr)) => Node(ll, top, rr)
    }

  def unfold(into: Int => OCons[Int])(seed: Int): IList =
    close(mapOC(unfold(into), into(seed)))

  def unfold(into: Int => ONode[Int])(seed: Int): ITree =
    close(mapON(unfold(into), into(seed)))

}

// STEP 10: Indirect recursion, but sepparate for each data -type
object indirectRecursion {
  import OptTypes._

  case class List_Ind(opt: OCons[List_Ind])
  case class Tree_Ind(opt: ONode[Tree_Ind])

  def fold(out: OCons[Int] => Int)(list: List_Ind): Int =
    out(mapOC(fold(out), list.opt))

  def fold(out: ONode[Int] => Int)(tree: Tree_Ind): Int =
    out(mapON(fold(out), tree.opt))

  def unfold(into: Int => OCons[Int])(seed: Int): List_Ind =
    List_Ind(mapOC(unfold(into), into(seed)))

  def unfold(into: Int => ONode[Int])(seed: Int): Tree_Ind =
    Tree_Ind(mapON(unfold(into), into(seed)))
}

object fixpointTypes {
  import OptTypes._

  // STEP 11: Indirect Recursion for _all_ data types

  // case class Fix[F[_]](unfix: F[Fix[F]])
  case class Ind[Rec[_]](opt: Rec[Ind[Rec]])

  type List_Ind = Ind[OCons]
  type Tree_Ind = Ind[ONode]

  // STEP 12: Unified fold and unfold: use functors.

  trait Functor[F[_]] {
    def map[A, B](fun: A => B, from: F[A]): F[B]
  }

  def fold[Rec[_]](ff: Functor[Rec], out: Rec[Int] => Int)(ind: Ind[Rec]): Int =
    out(ff.map(fold(ff, out), ind.opt))

  def unfold[Rec[_]](ff: Functor[Rec], into: Int => Rec[Int])(seed: Int): Ind[Rec] =
    Ind(ff.map(unfold(ff, into), into(seed)))
}
