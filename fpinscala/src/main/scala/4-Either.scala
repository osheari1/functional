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
  def sequence[E, A](l: List[A]): Either[E, List[A]]=
    traverse(l)(x ⇒ x)
}


case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age out of range")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))


/*
 There are a number of variations on `Option` and `Either`.
 If we want to accumulate multiple errors, a simple
 approach is a new data type that lets us keep a list of
 errors in the data constructor that represents failures:

 trait Partial[+A,+B]
 case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
 case class Success[+B](get: B) extends Partial[Nothing,B]

 There is a type very similar to this called `Validation` in the
 Scalaz library. You can implement `map`, `map2`,
 `sequence`, and so on for this type in such a way that errors are
 accumulated when possible (`flatMap` is unable to
 accumulate errors--can you see why?). This idea can even be generalized
 further--we don't need to accumulate failing
 values into a list; we can accumulate values using any user-supplied binary function.

 It's also possible to use `Either[List[E],_]` directly to accumulate errors,
 using different implementations of
 helper functions like `map2` and `sequence`.
 */

// val l = IndexedSeq(1.0, 2.0, 3.0, 4.0, 5.0)
// val l = IndexedSeq()
// val l = 1 to 10 toList

// Either.mean(l)
// Either.safeDiv(0, 0)

