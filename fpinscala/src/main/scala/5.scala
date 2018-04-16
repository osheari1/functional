import Stream._
// import collection.immutable.Stream

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

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
  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n == 0 ⇒ s
      case Cons(h, t) ⇒ go(t(), Cons(h , () ⇒ acc), n-1)
    }
    go(this, Stream(), n)
  }

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 ⇒ cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 ⇒ cons(h(), empty)
    case _ ⇒ empty
  }


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
}




