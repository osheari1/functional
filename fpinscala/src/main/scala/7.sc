def sum(as: IndexedSeq[Int]): Par[Int] =
  if (as.size <= 1)
      Par.unit(as.headOption getOrElse 0)
  else {
    val (l, r) = as.splitAt(as.length / 2)
    Par.map2(sum(l), sum(r))(_+_)
    }

/*Exercise 1*/
//def map2[B, C](x: A, y: B)(f: (A, B) => C): Par[C] = ???

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCanceled: Boolean
}

import scala.concurrent.duration.TimeUnit

case class Par[A]() {
  override def equals(that: Any) = ???
}

class ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
}







