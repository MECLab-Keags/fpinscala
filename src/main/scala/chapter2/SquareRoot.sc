import scala.annotation.tailrec
/** Example of using functions to calculate the square root using Newton's method */

def isGoodEnough(guess: Double, x: Double) : Boolean =
  Math.abs(guess * guess - x) < 0.001

def improve(guess: Double, x: Double) : Double =
  (guess + x / guess) / 2

@tailrec
def sqrtIter(guess: Double, x: Double) : Double =
  if(isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double) : Double =
  sqrtIter(1, x)

sqrt(2)
sqrt(4)
sqrt(1e0)