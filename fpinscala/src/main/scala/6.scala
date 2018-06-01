trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = Simple(newSeed)
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }

//    override def equals(that: Any): Boolean = ???
    override def equals(that: Any): Boolean = ???
  }
  // def simple(seed: Long): RNG = new RNG { def nextInt = {
  //                                          val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
  //                                          ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
  //                                        }
  // }
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  ////////////////
  // Exercise 1 //
  ////////////////
  // def randomPositiveInteger(rng: RNG): Option[Int] = {
  //   val (i, rng2) = rng.nextInt
  //   if (i <= Int.MinValue)
  //     None
  //   else
  //     Some(i.abs)
  // }
  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  ////////////////
  // Exercise 2 //
  ////////////////
  // def double(rng: RNG): (Double, RNG) = {
  //   val (i, r) = nonNegativeInt(rng)
  //   (i.toDouble / Int.MaxValue.toDouble, r)
  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
////////////////////////////////////////////////////////////////////////////////
import State._

/////////////////
// Exercise 11 //
/////////////////
case class State[S, +A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] =
    flatMap(a ⇒ unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    flatMap (a ⇒ sb map (b ⇒ f(a, b)))

  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State {s ⇒
      val (a, s1): (A, S) = run(s)
      f(a) run s1
    }

  override def equals(that: Any): Boolean = ???

}


object State {
  // def unit[S,A](a: A): State[S, A]=
  //   State { (s: S) ⇒
  //     (a, s)
  //   }
  def unit[S, A](a: A): State[S, A] =
    State(s ⇒ (a, s))

  // def sequenceViaFoldRight[S, A](l: List[State[S, A]]): State[S, List[A]] =
  //   (l foldRight unit[S, List[A]](List[A]())) ((h, acc) ⇒ ((h map2 acc) (_ :: _)))

  def sequenceViaFoldRight[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List()))((f, acc) ⇒ f.map2(acc)(_ :: _))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil => (acc.reverse, s)
          case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc)}
        }
    State((s: S) => go(s, sas, List()))
    }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    a <- get
    _ <- set(f(a))
  } yield ()

/*  Exercise 12
  Getters and setters*/
//  def get[S, A](s: State[S, A]): A =
  def get[S]: State[S, S] =
    State {s => (s, s)}

//  def set[S](s: State[S, Unit], s2: S): State[S, S] =
  def set[S](s: S): State[S, Unit] = State {_ => ((), s)}



}



