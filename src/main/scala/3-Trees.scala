// Binary tree data structure
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  //////////////////////////////
  //  Exercise 25             //
  // number of nodes in tree  //
  //////////////////////////////
  // def size[A](t: Tree[A]): Int = {
  //   def go(t: Tree[A], n: Int): Int = t match {
  //     case Leaf(x) ⇒ n
  //     case Branch(l, r) ⇒ go(l, n); go(r, n)
  //   }
  //   go(t, 0)
  // }
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(l, r) ⇒ 1 + size(l) + size(r)
  }

  /////////////////////////
  // Exercise 26         //
  // Compute max of tree //
  /////////////////////////
  /*
   We're using the method `max` that exists on all `Int`
   values rather than an explicit `if` expression.
   Note how similar the implementation is to `size`.
   We'll abstract out the common pattern in a later exercise.
   */
  // def maxElement[A](t: Tree[Int]): Int = t match {
  //   case Leaf(x) ⇒ x
  //   case Branch(l, r) ⇒ maxElement(l) max maxElement(r)
  // }
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) ⇒ n
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  /////////////////
  // Exercise 27 //
  // Depth       //
  /////////////////
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 0
    case Branch(l, r) ⇒ 1 + (depth(l) max depth(r))
  }

  /////////////////
  // Exercise 28 //
  // Map         //
  /////////////////
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(a) ⇒ Leaf(f(a))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

}
val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(10), Leaf(12))))
val t = Branch(Leaf(1), Leaf(2))
Tree.size(t)
Tree.maximum(t2)
Tree.depth(t2)
Tree.map(t2)(_^2)

