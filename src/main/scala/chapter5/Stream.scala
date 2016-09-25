package chapter5
import scala.collection.mutable.ListBuffer
import Stream._
sealed trait Stream[+A] {

    def exists(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) || t().exists(p)
        case Empty => false
    }

    /** Exercise 5.1 - Write a toList function */
    def toList : List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    /** From the fpinscala answers GitHub repo */
    // Tail recursive solution of toList, however a reverse on the generated list
    // is required to maintain the original order.
    def toList2 : List[A] = {
        @annotation.tailrec
        def go(s: Stream[A], acc: List[A]) : List[A] = s match {
          case Cons(h, t) => go(t(), h() :: acc)
          case _ => acc
        }
        go(this, List()).reverse
    }

    // tail recursive solution that uses a mutable buffer.
    // As the stream is processed the order of the stream is maintained.
    def toListFast : List[A] = {
        var buf = new ListBuffer[A]
        def go(s: Stream[A]) : List[A] = s match {
          case Cons(h, t) =>
            buf += h()
            go(t())
          case _ => buf.toList
        }
        go(this)
    }

    /** Exercise 5.2 - Write a take(n) function  */
    def take(n: Int) : Stream[A] = {
        @annotation.tailrec
        def go(i: Int, xs: => Stream[A], agg: => Stream[A]) : Stream[A] = xs match {
          case Empty => agg
          case Cons(h, t) =>
            if (i == 0) agg
            else go(i-1, t(), cons(h(), agg))
        }
        go(n, this, empty)
    }

    @annotation.tailrec
    final def drop(n: Int) : Stream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n-1)
        case _ => this
    }

    /** Answer from fpinscala GitHub repo */
    def take2(n: Int) : Stream[A] = this match {
        case Cons(h, t) if n > 1 => cons(h(), t().take2(n-1))
        case Cons(h, t) if n == 1 => cons(h(), empty)
        case _ => empty
    }

    /** Exercise 5.3 - Write a function that continuously retrieves elements from
    * the stream until the the predicate returns false. */
    def takeWhile(predicate: (() => A) => Boolean) : Stream[A] = this match {
        case Cons(h, t) if predicate(h) => cons(h(), t().takeWhile(predicate))
        case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists2(p: A => Boolean) : Boolean =
        foldRight(false)((a,b) => p(a) || b)

    /** Exercise 5.4 - Write a forAll function that checks all elements in the
      * Stream match the given predicate. */
    def all(p: A => Boolean) : Boolean =
        foldRight(true)((a,b) => p(a) && b)

    /** Exercise 5.5 - Use foldRight to implement takeWhile */
    def takeWhile2(p: (() => A) => Boolean): Stream[A] = ???

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
  def empty[A] : Stream[A] = Empty

  def apply[A](as: A*) : Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail:_*))
}