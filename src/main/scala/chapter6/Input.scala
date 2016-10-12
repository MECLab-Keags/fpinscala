package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int){}

object Candy {
  def update = (input: Input) => (state: Machine) => (input, state) match {
    case(_, Machine(_, 0, _)) => state
    case(Coin, Machine(false, _, _)) => state
    case(Turn, Machine(true, _, _)) => state
    case(Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case(Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}