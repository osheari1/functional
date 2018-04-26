// Must implement these to get all 'for' functionality
abstract class CustomClass[A] {
  def map[B](f: A ⇒ B): B
  def flatMap[B](f: A ⇒ CustomClass[B]): CustomClass[B]
  def withFilter[A](p: A ⇒ Boolean): CustomClass[A]
  def foreach(b: A ⇒ Unit): Unit
}




