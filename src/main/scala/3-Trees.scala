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

  /////////////////
  // Exercise 29 //
  // Fold        //
  /////////////////
  // def fold[A, B](t: Tree[A], z: A ⇒ B)(f: (B, B) ⇒ B): B = t match {
  //   case Leaf(a) ⇒ z(a)
  //   case Branch(l, r) ⇒ f(fold(l, z)(f),  fold(r, z)(f))
  // }
  /*
   Like `foldRight` for lists, `fold` receives a "handler" for each of
   the data constructors of the type, and recursively
   accumulates some value using these handlers. As with `foldRight`,
   `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
   this function to implement just about any recursive function
   that would otherwise be defined by pattern matching.
   */
  def fold[A,B](t: Tree[A])(f: A ⇒ B)(g: (B, B) ⇒ B): B = t match {
    case Leaf(a) ⇒ f(a)
    case Branch(l, r) ⇒ g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ ⇒ 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a ⇒ a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ ⇒ 0)(_ max _ + 1)

  /*
  Note the type annotation required on the expression `Leaf(f(a))`.
   Without this annotation, we get an error like this:

  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^

  This error is an unfortunate consequence of Scala using subtyping
  to encode algebraic data types. Without the annotation, the result
  type of the fold gets inferred as `Leaf[B]` and it is then expected
  that the second argument to `fold` will return `Leaf[B]`, which it
   doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with
  algebraic data types in Scala, it's somewhat common to define helper
  functions that simply call the corresponding data constructors but
  give the less specific result type:

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  // def mapViaFold[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] =
  //   fold(t)(a ⇒ Branch(Leaf(f(a)), Leaf(f(a))))((l, r) ⇒
  //     (l, r) match { case (Branch(l1, r1), Branch(l2, r2)) ⇒ Branch(l1, l2) })
  def mapViaFold[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] =
    fold(t)(a ⇒ Leaf(f(a)): Tree[B])(Branch(_,_))

}




val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(10), Leaf(12))))
val t = Branch(Leaf(1), Leaf(2))

Tree.size(t2)
Tree.sizeViaFold(t2)

Tree.maximum(t2)
Tree.maximumViaFold(t2)

Tree.depth(t2)
Tree.depthViaFold(t2)


Tree.map(t2)(_+1)
Tree.mapViaFold(t2)(_+1)

