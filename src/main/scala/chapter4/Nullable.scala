//package fp101.errorhandling
/**
  * Chapter 4 - Handling errors without exceptions
  */
sealed trait Nullable[+A] {

    /** Exercise 4.1 - Implement helpful functions */
    def map[B](f : A => B) : Nullable[B] = this match {
        case Value(a) => Value(f(a))
        case _ => Empty
    }

    def flatMap[B](f : A => Nullable[B]) : Nullable[B] =
        map(f) getOrElse(Empty)

    def getOrElse[B >: A](default: => B) : B = this match {
        case Value(a) => a
        case Empty => default
    }

    def orElse[B >: A](ob: => Nullable[B]) : Nullable[B] =
        this map(Value(_)) getOrElse(ob)

    def filter(f : A => Boolean) : Nullable[A] = this match {
        case Value(a) if f(a) => this
        case Empty => Empty
     }
}
case class Value[+A](value: A) extends Nullable[A]
case object Empty extends Nullable[Nothing]

object Nullable {
    def lift[A,B](f: A => B) : A => Nullable[B] =  Value(_) map f

    /** Exercise 4.3 - Generic function that combines 2 Option values
      *     i.e. (Nullable[A], Nullable[B]) -> Nullable[C]  */
    def map2[A,B,C](a: Nullable[A], b: Nullable[B])(f: (A, B) => C): Nullable[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))

    def map2For[A,B,C](a: Nullable[A], b: Nullable[B])(f: (A,B) => C): Nullable[C] =
        for {
            aa <- a
            bb <- b
        } yield f(aa,bb)

    /** Exercise 4.4 - Combines a list of Option values into a Option value of a list.
      *     i.e. List[Option[A]] -> Option[List[A]] */
    def sequence[A](xs : List[Nullable[A]]) : Nullable[List[A]] = xs match {
        case Nil => Value(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

    def sequence2[A](xs : List[Nullable[A]]) : Nullable[List[A]] =
        traverse(xs)(x => x)

    /** Exercise 4.5 - Implement the traverse function that will only traverse the list once. */
    def traverse[A, B](xs: List[A])(f: A => Nullable[B]) : Nullable[List[B]] = xs match {
        case Nil => Value(Nil)
        case h :: t => f(h) flatMap(hh => traverse(t)(f) map(hh :: _))
    }

    /** Companion object examples from the book. */
    def Try[A](a: => A): Nullable[A] = {
        try Value(a)
        catch { case ex : Exception => Empty }
    }
}

object Chapter4 {
    def mean(xs : Seq[Double]) : Nullable[Double] = {
        if (xs.isEmpty) Empty
        else Value(xs.sum / xs.length)
    }
    /** Exercise 4.2 - Implement a variance function in terms of flatMap. */
    def variance(xs : Seq[Double]) : Nullable[Double] = {
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    /** Examples from the book */
    val absO : Double => Nullable[Double] = Nullable.lift(math.abs)

    /** This implementation is not very efficient since we have to traverse the list twice.
      *     Once for the map function that attempts to parse the string value into an Int
      *         (i.e. List[String] -> List[Option[Int]])
      *     then again within the sequence function
      *         (i.e. List[Option[Int]] -> Option[List[Int]]) */
    def parseInt(xs: List[String]): Nullable[List[Int]] =
        Nullable.sequence(xs map (x => Nullable.Try(x.toInt)))

}