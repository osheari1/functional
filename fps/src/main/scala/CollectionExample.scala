case class Sequence[A](initialEl: A*) {
  private val elems = scala.collection.mutable.ArrayBuffer[A]()
  val elems ++= initialEl
}
