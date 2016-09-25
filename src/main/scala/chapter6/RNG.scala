package chapter6

trait RNG {
  def nextInt(): (Int, RNG)
}
object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rngX) = rng.nextInt
    val result =
      if (x > 0) x
      else if(x == Int.MinValue) -(x + 1)
      else -x
    (result, rngX)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL+ 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}