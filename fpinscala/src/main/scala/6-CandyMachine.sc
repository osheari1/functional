import State._
sealed trait Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  override def equals(that: Any) = ???
}

case object Coin extends Input {
  override def equals(that: Any) = ???
}

case object Turn extends Input {
  override def equals(that: Any) = ???
}

object Candy {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin+1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}

