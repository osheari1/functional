import Stream._
// import collection.immutable.Stream

trait Stream[+A] {
  // def uncons: Option[(A, Stream[A])]
  // def isEmpty: Boolean = uncons.isEmpty

  ////////////////
  // Exercise 1 //
  ////////////////
  def toListRecursive: List[A] = this match {
    case Cons(h, t) ⇒ h() :: t().toListRecursive
    case _ ⇒ List()
  }
    /*
     The above solution will stack overflow for large streams, since it's
     not tail-recursive. Here is a tail-recursive implementation. At each
     step we cons onto the front of the `acc` list, which will result in the
     reverse of the stream. Then at the end we reverse the result to get the
     correct order again.
     [:ben] are the line breaks above okay? I'm unclear on whether these "hints" are supposed to go in the book or not
     */
  def toList: List[A] ={
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) ⇒ go(t(), h() :: acc)
      case _ ⇒ acc
    }
    go(this, List()).reverse
  }
    /*
     In order to avoid the `reverse` at the end, we could write it using a
     mutable list buffer and an explicit loop instead. Note that the mutable
     list buffer never escapes our `toList` method, so this function is
     still _pure_.
     */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) ⇒ buf += h(); go(t())
      case _ ⇒ buf.toList
    }
    go(this)
  }

  ////////////////
  // Exercise 2 //
  ////////////////
  /*
   Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
   calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
   we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
   at the stream at all.
   */
  // def take(n: Int): Stream[A] = {
  //   @annotation.tailrec
  //   def go(s: Stream[A], acc: Stream[A], n: Int): Stream[A] = s match {
  //     case Cons(h, t) if n == 0 ⇒ s
  //     case Cons(h, t) ⇒ go(t(), Cons(h , () ⇒ acc), n-1)
  //   }
  //   go(this, Stream(), n)
  // }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 ⇒ cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 ⇒ cons(h(), empty)
    case _ ⇒ empty
  }

  ////////////////
  // Exercise 3 //
  ////////////////
  // def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
  //   case Cons(h, t) if p(h()) ⇒ cons(h(), t().takeWhile(p))
  //   case Cons(h, t) ⇒ cons(h(), empty)
  //   case _ ⇒ empty
  // }
  // def takeWhile2(p: A ⇒ Boolean): Stream[A] = {
  //   @annotation.tailrec
  //   def go(s: Stream[A], acc: Stream[A])(p: A ⇒ Boolean): Stream[A] = s match {
  //     case Cons(h, t) if p(h()) ⇒ go(t(), cons(h(), acc))(p)
  //     case _ ⇒ acc
  //   }
  //   go(this, Stream())(p)
  // }
  def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) ⇒ cons(h(), t() takeWhile p)
    case _ ⇒ empty
  }

  ////////////////
  // Exercise 4 //
  ////////////////
  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B =
    this match {
      case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
      case _ ⇒ z
    }
  // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, b) ⇒ p(a) || b)

  /*
   Since `&&` is non-strict in its second argument, this terminates
   the traversal as soon as a nonmatching element is found.
   */
  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  ////////////////
  // Exercise 5 //
  ////////////////
  // def takeWhile2(p: A ⇒ Boolean): Stream[A] = {
  //   foldRight(empty)((h, t) ⇒ { if (p(h)) t else empty })
  // }
  def takeWhile2(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((h, t) ⇒ if (p(h)) cons(h, t) else empty)

  ////////////////
  // Exercise 6 //
  ////////////////
  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((h, t) ⇒ cons(f(h), t))

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((h, t) ⇒ if (p(h)) cons(h, t) else t)

  def append[B>:A](s: ⇒ Stream[B]): Stream[B] =
    foldRight(s)((h, t) ⇒ cons(h, t))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty[B])((h, t) ⇒ f(h) append t)

  /////////////////
  // Exercise 12 //
  /////////////////
  def mapViaUnfold[B](f: A ⇒ B): Stream[B] =
    unfold(this) {
        case Cons(h, t) ⇒ Some((f(h()), t()))
        case empty ⇒ None
    }

  // def takeViaUnfold(n: Int): Stream[A] =
  //   unfold((this, n)) {
  //     case (Cons(h, t), i) if i > 1 ⇒ Some(h(), (t(), i-1))
  //     case (Cons(h, t), i) if i == 1 ⇒ Some(h(), (empty, i-1))
  //     case (empty, i) ⇒ None
  //     }
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(x, xs), 1) ⇒ Some((x(), (empty, 0)))
      case (Cons(x, xs), n) if n > 1 ⇒ Some((x(), (xs(), n-1)))
      case _ ⇒ None
    }
  def takeWhileViaUnfold(p: A ⇒ Boolean): Stream[A] =
    unfold(this) {
      case Cons(x, xs) if p(x()) ⇒ Some((x(), xs()))
      case _ ⇒ None
    }

  // def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
  //   unfold((this, s)) {
  //     case (Cons(x, xs), empty) ⇒ None
  //     case (empty, Cons(y, ys)) ⇒ None
  //     case (Cons(x, xs), Cons(y, ys)) ⇒ Some((f(x(), y()), (xs(), ys())))
  //   }
  def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    unfold((this, s)) {
      case (Cons(x, xs), Cons(y, ys)) ⇒ Some((f(x(), y()), (xs(), ys())))
      case _ ⇒ None
    }
  def zipViaUnfold[B](s: Stream[B]): Stream[(A, B)] =
    zipWithViaUnfold(s)((_, _))

  // def zipViaUnfold[B](s: Stream[B]): Stream[(A, B)] =
  //   unfold((this, s)) {
  //     case (Cons(x, xs), empty) ⇒ None
  //     case (empty, Cons(y, ys)) ⇒ None
  //     case (Cons(x, xs), Cons(y, ys)) ⇒ Some(((x(), y()), (xs(), ys())))
  //   }
  // def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
  //   unfold((this, s)) {
  //     case (Cons(x, xs), Cons(y, ys)) ⇒ Some(((Some(x()), Some(y())), (xs(), ys())))
  //     case (empty, Cons(y, ys)) ⇒ Some(((None, Some(y())), (empty, ys())))
  //     case (Cons(x, xs), empty) ⇒ Some(((Some(x()), None), (xs(), empty)))
  //   }
  def zipWithAllViaUnfold[B, C](s: Stream[B])(f: (Option[A], Option[B]) ⇒ C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAllViaUnfold(s2)((_,_))








}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  ////////////////
  // Exercise 7 //
  ////////////////
  // Infinant streams
  val ones: Stream[Int] = cons(1, ones)

  // def constant[A](a: A): Stream[A] =
  //   cons(a, constant(a))
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() ⇒ a, () ⇒ tail)
    tail
  }

  /////////////////////////////
  // Exercise 8              //
  // Infinant stream of ints //
  /////////////////////////////
  // def from(n: Int): Stream[Int] = {
  //   lazy val next: Stream[Int] = Cons(() ⇒ n + 1, () ⇒ next)
  //   next
  // }
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  ////////////////////////
  // Exercise 9         //
  // Fibbonacci numbers //
  ////////////////////////
  // def fibNumbers[A]: Stream[Int] = {
  //   def go(a: Int, b: Int): Stream[Int] =
  //     cons(a, go(b, a+b))
  //   go(0, 1)
  // }
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  /////////////////
  // Exercise 10 //
  /////////////////
  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) ⇒ cons(h, unfold(s)(f))
      case None ⇒ empty
    }

  /////////////////
  // Exercise 11 //
  /////////////////
  /*
   Scala provides shorter syntax when the first action of a
   function literal is to match on an expression.  The function passed
   to `unfold` in `fibsViaUnfold` is equivalent to
   `p => p match { case (f0,f1) => ... }`, but we avoid having
   to choose a name for `p`, only to pattern match on it.
   */
  // val fibsViaUnfold: Stream[Int] =
  //   unfold((0, 1))((s) ⇒ Some((f0+f1, (f1, f0+f1))))
  // val fibsViaUnfold: Stream[Int] =
  //   unfold((0, 1))({
  //                    case (f0, f1) ⇒ Some((f0, (f1, f1+f0)))
  //                  })
  val fibsViaUnfold =
    unfold((0, 1))(p ⇒ p match {
                     case (f0, f1) ⇒ Some((f0, (f1, f1+f0)))
                   })

  // def fromViaUnfold(n: Int): Stream[Int] =
  //   unfold((n, n+1))({case (n0, n1) ⇒ Some((n1, (n1, n1+1)))})
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n ⇒ Some(n, n+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ ⇒ Some(a, a))

  // def onesViaUnfold: Stream[Int] =
  //   unfold(1)(_ ⇒ Some(1, 1))
  val onesViaUnfold = unfold(1)(_ ⇒ Some((1, 1)))




}




// REPL
///////////////////////////////////////////////

Stream.fibNumbers.take(5).toListRecursive


val s = Stream(1, 2, 3, 4, 5, 6)
Stream(1, 2, 3, 4, 5, 6).take(2).toList
s.toListFast

