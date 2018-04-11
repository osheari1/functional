// Throwing an exception breaks Referential Tansparency
/*
An expression is e referentially transparent if for all programs p , all
occurrences of in can be replaced by the result of evaluating e,
without affecting the observable behavior of p.
 A function f is  pure if the expression f(x) is referentially
 transparent for all x
 */

def failingFn(i: Int): Int = {
  val x: Int = throw new Exception("fail!")
  try {
    val y = 42 + 5
    x + y
  }
  catch { case e: Exception ⇒ 43 }
}

// def mean(xs: Seq[Double]): Double = {
//   if (xs.isEmpty)
//     throw new ArithmeticException("mean of empty list")
//   else xs.sum / xs.length
// }

/////////////////////////
// The Option datatype //
/////////////////////////
// sealed trait Option[+A]
// case class Some[+A](get: A) extends Option[A]
// case object None extends Option[Nothing]
// Mean is a TOTAL FUNCTION. Takes all allowed values of input and
// outputs one type.
def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}


//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}
sealed trait Option[+A] {
  ////////////////
  // Exercise 1 //
  ////////////////
  def map[B](f: A ⇒ B): Option[B] = this match {
    case None ⇒ None
    case Some(a) ⇒ Some(f(a))
  }
  // default: ⇒ B indicates the argument won't be evaluated
  // until it is needed by the function
  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case None ⇒ default
    case Some(a) ⇒ a
  }
  // def flatMap[B](f: A ⇒ Option[B]): Option[B] = this match {
  //   case None ⇒ None
  //   case Some(a) ⇒ f(a)
  // }
  def flatMap[B](f: A ⇒ Option[B]): Option[B] =
    map(f) getOrElse None

  // B >: A means B must be a supertype of A
  // def orElse[B>:A](ob: ⇒ Option[B]): Option[B] = this match {
  //   case None ⇒ ob
  //   case Some(a) ⇒ Some(a)
  // }
  def orElse[B>:A](ob: ⇒ Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def orElse2[B>:A](ob: ⇒ Option[B]): Option[B] = this match {
    case None ⇒ ob
    case _ ⇒ None
  }

  // def filter(f: A ⇒ Boolean): Option[A] = this match {
  //   case Some(a) ⇒ if (f(a)) Some(a) else None
  //   case _ ⇒ None
  // }

  def filter(f: A ⇒ Boolean): Option[A] =
    flatMap (a ⇒ if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  ////////////////
  // Exercise 2 //
  // Variance   //
  ////////////////
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m ⇒ mean(xs map (x ⇒ math.pow(x - m, 2))))

  /*
  A common pattern is to transform an option via map, flatMap, and/or filter, then use getOrElse
  to do error handling at the end.
  */

  /*
   Entire programs do not have to be modified to account for Some or None. They can be *Lifted*
  */
  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] =
    _ map f

  /*
   Example
   */
  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException ⇒ None
    }
  def mkMatcher(pat: String): Option[String ⇒ Boolean] =
    pattern(pat) map (p ⇒ (s: String) ⇒ p.matcher(s).matches)

  /*
   Can also be done via *list-comprehension*
   */
  def mkMatcher_1(pat: String): Option[String ⇒ Boolean] =
    for {
      p ← pattern(pat)
    } yield ((s: String) ⇒ p.matcher(s).matches)
  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p ← mkMatcher_1(pat)
    } yield p(s)

  def matchBoth(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f ← mkMatcher(pat)
      g ← mkMatcher(pat2)
    } yield f(s) && g(s)

  /*
   Internally scala will translate list comprehension to map and flatMap
   */
  def matchBoth_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f ⇒ mkMatcher(pat2) map (g ⇒ f(s) && g(s)))

  ////////////////
  // Exercise 3 //
  // Map 2      //
  ////////////////
  // Later in chapter will learn nicer syntax for this
  // def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
  //   a flatMap (x ⇒ b map (y ⇒ (f(x, y))))
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
    a flatMap (aa ⇒ b map (bb ⇒ f(aa, bb)))

  ////////////////
  // Exercise 4 //
  // Both match //
  ////////////////
  def matchBoth_2(pat: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat), mkMatcher(pat2))((f, g) ⇒ (f(s) && g(s)))

  //////////////////////////////////////////////////////////
  // Exercise 5                                           //
  // Combine a list of options into an option of a list   //
  //////////////////////////////////////////////////////////
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil ⇒ None
    case h :: t ⇒ h flatMap (hh ⇒ sequence(t) map (hh :: _))
  }
  /*
   It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
   Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
   unfortunate consequence of Scala using subtyping to encode algebraic data types.
   */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) ⇒ map2(x, y)(_ :: _))

  ////////////////
  // Exercise 6 //
  // Traverse   //
  ////////////////
  //WRONG
  // def traverse[A, B](l: List[A])(f: A ⇒ Option[B]): Option[List[B]] = l match {
  //   case Nil ⇒ Nil
  //   case h :: t ⇒ f(h) flatMap (_ ⇒ traverse(t)(f))
  // }
  def traverse[A, B](l: List[A])(f: A ⇒ Option[B]):Option[List[B]] = l match {
    case Nil ⇒ Some(Nil)
    case h :: t ⇒ map2(f(h), traverse(t)(f))(_ :: _)
  }
  def traverse_1[A, B](l: List[A])(f: A ⇒ Option[B]): Option[List[B]] =
    l.foldRight[Option[List[B]]](Some(Nil))((h, t) ⇒ map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(_)


}












val t = Seq(1, 2, 3, 4)
