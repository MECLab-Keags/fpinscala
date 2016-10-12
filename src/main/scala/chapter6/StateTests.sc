import chapter6._

val initial = SimpleRNG(42)
val (int1, rng1) = initial.nextInt
val (int2, rng2) = rng1.nextInt

val (pos1, prng1) = RNG.nonNegativeInt(initial)
val (pos2, prng2) = RNG.nonNegativeInt(prng1)
val (pos3, prng3) = RNG.nonNegativeInt(prng2)
val (pos4, prng4) = RNG.nonNegativeInt(prng3)

Int.MaxValue
pos1 / (Int.MaxValue.toDouble + 1)
pos2 / (Int.MaxValue.toDouble + 1)

val (db1, ex2rng1) = RNG.double(initial)
val (db2, ex2rng2) = RNG.double(ex2rng1)
val (db3, ex2rng3) = RNG.double(ex2rng2)

val (xs1, ex3rng1) = RNG.ints(0)(initial)
ex3rng1.nextInt

val (xs2, ex3rng2) = RNG.ints(15)(initial)


def originalRollDie: RNG.Rand[Int] = RNG.nonNegativeLessThan(6)
def rollDie: RNG.Rand[Int] = RNG.mapAlt(RNG.nonNegativeLessThan(6))(_ + 1)
val (roll1, dieRng1) = rollDie(SimpleRNG(5))
val (roll2, dieRng2) = rollDie(dieRng1)
val (roll3, dieRng3) = rollDie(dieRng2)
val (roll4, dieRng4) = rollDie(dieRng3)

/*
val ns: RNG.Rand[List[Int]] = for {
  x <- (0 until 5)
  y <- x * 2
} yield (x,y) */

val sim1 = Candy.simulateMachine(List(
  Turn, Coin, Coin, Turn
))

val sim2 = sim1.run(Machine(true, 15, 0))
sim2._1
sim2._2