
//def sum(as: IndexedSeq[Int]): Par[Int] =
//  if (as.size <= 1)
//      Par.unit(as.headOption getOrElse 0)
//  else {
//    val (l, r) = as.splitAt(as.length / 2)
//    Par.map2(sum(l), sum(r))(_+_)
//    }

/*Exercise 1*/
//def map2[B, C](x: A, y: B)(f: (A, B) => C): Par[C] = ???

import scala.concurrent.duration.TimeUnit



//case class Par[A]() {
//  override def equals(that: Any) = ???
//
//  def run(s: ExecutorService)(a: Par[A]): Future[A] = a(s)
//
//}

type Par[A] = ExecutorService => Future[A]

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCanceled: Boolean
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???

}

class Callable[T] {

}

object Par {

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def async[A](a: => A): Par[A] = fork(unit(a))

  // The given Par should be run in a separate thread.
  // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable`
  // will block waiting for the "inner" task to complete. Since this
  // blocking occupies a thread in our thread pool, or whatever resource
  // backs the `ExecutorService`, this implies that we're losing out on
  // some potential parallelism. Essentially, we're using two threads
  // when one should suffice. This is a symptom of a more serious problem
  // with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call  = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // Exercise 3
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sortPart(l: Par[List[Int]]): Par[List[Int]] =
    map2(l, unit(()))((a, _) => a.sorted)

  def sortPar2(l: Par[List[Int]]): Par[List[Int]] =
    map_(l)(_.sorted)

  def map_[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def unit[A](a: A): Par[A] =
    (s: ExecutorService) => UnitFuture(a)

//  def map2[A, B, C](a: A, b: B)(f: (A, B) => C): Par[C] =
//    (es: ExecutorService) => UnitFuture(f(a, b))
// `map2` doesn't evaluate the call to `f` in a separate logical thread,
// in accord with our design choice of having `fork` be the sole function
// in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))`
// if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      // This implementation of `map2` does _not_ respect timeouts.
      // It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them,
      // and wraps them in a `UnitFuture`. In order to respect timeouts,
      // we'd need a new `Future` implementation that records the amount
      // of time spent evaluating `af`, then subtracts that time from the
      // available time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))

    }

  // Split map2 into its decomposed functions
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    (es: ExecutorService) => {
      val af = fa(es)
      val bf = fb(es)
      UnitFuture((af.get, bf.get))
    }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => {
      val af = fa(es)
      UnitFuture(f(af.get))
    }

  /*  Exercise 3*/
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    override def cancel(evenIfRunning: Boolean): Boolean = false
    override def isCanceled: Boolean = false
    override def equals(that: Any): Nothing = ???
  }

/*  Exercise 6*/
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    (es: ExecutorService) => {
      val l2:List[B] = l.foldRight(List[B]())((x, xs) => f(x) :: xs)
      UnitFuture(l2)
    }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap2[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

// Exercise 7
//    val l2: Par[List[A]] = l.foldRight(unit(List[A]()))((x, xs) => map2(x, unit(()))((x2, _) => ))
  def sequence2[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }



  }











}





