// package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
 which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /////////////////////////////////
  // Excersize 1                 //
  // Which case will be returned //
  /////////////////////////////////
  //Answer: 3

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil ⇒ a2
      case Cons(h, t) ⇒ Cons(h, append(t, a2))
    }

  // Utility function
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B =
    as match {
      case Nil ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) ⇒ x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  ///////////////////
  // Exercise 2    //
  // Tail function //
  ///////////////////
  /*
   Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
   a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
   returning a value just means this bug will be discovered later, further from the place where it was introduced.

   It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
   right hand side of a pattern. This makes it clear the value isn't relevant.
   */

  def tail[A](l: List[A]): List[A] = l match {
      case Nil ⇒ sys.error("Tail of empty list")
      case Cons(_, t) ⇒ t
    }

  ///////////////////
  // Exercise 3    //
  // Drop function //
  ///////////////////
  /*
   Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
   contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
   is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
   length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
   If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil ⇒  Nil
      case Cons(_,t) ⇒ drop(t, n-1)
    }

  ////////////////
  // Exercise 4 //
  // Drop While //
  ////////////////
  // Not a true implementation, has bugs.
  /*
   Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
   satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
   use any of the variables introduced by the pattern.
   */
  // def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] =
  //   l match {
  //     case Cons(h, t) if f(h) ⇒ dropWhile(t, f)
  //     case _ ⇒ l
  //   }
  def dropWhile[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    l match {
      case Nil ⇒ l
      case Cons(h, t) if f(h) ⇒ dropWhile(t)(f)
    }

  ////////////////
  // Exercise 5 //
  // setHead    //
  ////////////////
  /*
   If a function body consists solely of a match expression, we'll often put the match on the same line as the
   function signature, rather than introducing another level of nesting.
   */
  // def setHead[A](l: List[A], h: A): List[A] =
  //   l match {
  //     case Nil ⇒ List(h)
  //     case Cons(_, t) ⇒ Cons(h, t)
  // }
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ sys.error("init on empty list")
    case Cons(_, Nil) ⇒ l
    case Cons(_, t) ⇒ init(t)
  }
}

val l = List(1, 2, 3, 4)
List.init(l)
List.setHead(List(1, 2, 3, 4, 5), 2)
