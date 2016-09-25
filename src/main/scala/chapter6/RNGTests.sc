import chapter6._

val initial = SimpleRNG(42)
val (int1, rng1) = initial.nextInt
val (int2, rng2) = rng1.nextInt

val (pos1, prng1) = RNG.nonNegativeInt(initial)
val (pos2, prng2) = RNG.nonNegativeInt(prng1)
val (pos3, prng3) = RNG.nonNegativeInt(prng2)
val (pos4, prng4) = RNG.nonNegativeInt(prng3)



