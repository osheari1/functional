
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

  def map2[A, B, C](a: A, b: B)(f: (A, B) => C): Par[A] = ???

  def async[A](a: => A): Par[A] = fork(unit(a))

  /*  Exercise 3*/
  def unit[A](a: A): Par[A] = ???

  // The given Par should be run in a separate thread.
  def fork[A](a: => Par[A]): Par[A] = ???

}







