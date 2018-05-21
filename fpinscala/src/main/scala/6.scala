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

  ////////////////
  // Exercise 3 //
  ////////////////
  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }
  def doubleThree(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  ////////////////
  // Exercise 4 //
  ////////////////
  // def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //   @annotation.tailrec
  //   def go(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = count match {
  //     case n if n >= 1 ⇒ {
  //       val (i, r) = rng.nextInt
  //       go(count-1, acc ++ List(i))(r)
  //     }
  //     case _ ⇒ (acc, rng)
  //   }
  //   go(count, List())(rng)
  // }
  // A simple recursive solution
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val(x, r1) = rng.nextInt
      val (xs, r2) = ints(count-1)(r1)
      (x :: xs, r2)
    }
  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count-1, r2, x::xs)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (i, rng2) = s(rng)
      (f(i), rng2)
    }

  ////////////////
  // Exercise 5 //
  ////////////////
  def positiveMax(n: Int): Rand[Int] =
    map(double)(x ⇒ (x * n).abs.toInt)

  ////////////////
  // Exercise 6 //
  ////////////////
  val double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  ////////////////
  // Exercise 7 //
  ////////////////
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] = {
    rng ⇒ {
      val(a, rng2) = ra(rng)
      val(b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  ////////////////
  // Exercise 8 //
  ////////////////
    // In `sequence`, the base case of the fold is a `unit` action that returns
    // the empty list. At each step in the fold, we accumulate in `acc`
    // and `f` is the current element in the list.
    // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
    // We map over that to prepend (cons) the element onto the accumulated list.
    //
    // We are using `foldRight`. If we used `foldLeft` then the values in the
    // resulting list would appear in reverse order. It would be arguably better
    // to use `foldLeft` followed by `reverse`. What do you think?
    // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //   def go(fs: List[Rand[A]], acc: List[A]): Rand[List[A]] =
    //     rng ⇒ {
    //       fs match {
    //         case Nil ⇒ (acc, rng)
    //         case h :: t ⇒ {
    //           val (i, rng2) = h(rng)
    //           (i :: acc, rng2)
    //         }
    //       }
    //     }
    //   go(fs, List())
    // }
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    (fs foldRight unit(List[A]()))((f, acc) ⇒ map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.
  // def ints2(count: Int): Rand[List[Int]] =
  //   sequence(List.fill(count)(_.nextInt))
  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))


  /////////////////
  // Excercise 9 //
  /////////////////
  def flatMap[A,B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] =
    rng ⇒ {
      val (i, rng2) = f(rng)
      g(i)(rng2)
    }

  // def positiveInt: Rand[Int] =
  //   flatMap(int)(i ⇒ rng ⇒ {
  //                  if (i != Int.MinValue)
  //                    (i.abs, rng)
  //                  else
  //                    positiveInt(rng)
  //                })
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i ⇒ {
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
      }
    }
  }

  /////////////////
  // Exercise 10 //
  /////////////////
  def mapViaFlatMap[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = {
    flatMap(s)(i ⇒ unit(f(i)))
  }

  // def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
  //   flatMap(ra) { a ⇒
  //     flatMap(rb)(b ⇒ unit(f(a, b)))
  //   }
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))


}

////////////////////////////////////////////////////////////////////////////////
// State
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
      val (a, s1) = run(s)
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



}



