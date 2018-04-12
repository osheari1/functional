sealed trait Either[+E,+A] {
  ////////////////
  // Exercise 7 //
  ////////////////
  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Right(a) ⇒ Right(f(a))
    case Left(e) ⇒ Left(e)
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) ⇒ f(a)
      case Left(e) ⇒ Left(e)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) ⇒ Right(a)
      case Left(_) ⇒ b
  }

  // def map2[EE >: E, B, C](b: ⇒ Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] =
  //   this flatMap (a ⇒ b map (bb ⇒ f(a, bb))
  // Scala translates for comprehensions to flatMap and map
  def map2[EE >: E, B, C](b: ⇒ Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] =
    for {
      a ← this
      b1 ← b
    } yield f(a, b1)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
        Left("Mean of empty list!")
    else
        Right(xs.sum / xs.length)
  }

  // Can return stake trace by returning it in Left
  def safeDiv(x: Double, y: Double): Either[Exception, Double] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception ⇒ Left(e)
    }
  }

  ///////////////////////////
  // Exercise 8            //
  // Sequence and traverse //
  ///////////////////////////
  // def traverse[E, A, B](l: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
  //   l match {
  //     case h :: t ⇒ f(h).map2(traverse(t)(f))(_ :: _)
  //   }
  def traverse[E, A, B](l: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    l match {
      case Nil ⇒ Right(Nil)
      case h :: t ⇒ (f(h) map2 (traverse(t)(f)))(_ :: _)
    }

}


// val l = IndexedSeq(1.0, 2.0, 3.0, 4.0, 5.0)
// val l = IndexedSeq()
// val l = 1 to 10 toList

// Either.mean(l)
// Either.safeDiv(0, 0)

