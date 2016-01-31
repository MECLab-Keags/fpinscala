package Chapter3

/**
  * Created by SoHo on 10/01/16.
  */

sealed trait Tree[+A]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left : Tree[A], right : Tree[A]) extends Tree[A]

object Tree {

    def foldLeft[A,B](t : Tree[A], z : B)(f : A => B) : B = t match {
        case Leaf(v) => f(v)
        //case Branch(l,r) =>
        case Branch(l, r) => ((a:Tree[A]) => (b:B) => foldLeft(a, b)(f)) (l)(foldLeft(r, z)(f))
    }

    /** Exercise 3.25 - count all the branches and leaves */
    def size[A](t : Tree[A]) : Int = t match {
        case Leaf(v) => 1
        case Branch(l, r) => size(l) + size(r) + 1
    }

    /** Exercise 3.26 - find the largest leaf value */
    def maximum(t : Tree[Int]) : Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    /** Exercise 3.27 - find the deepest path to any leaf */
    def depth[A](t : Tree[A]) : Int = t match {
        case Leaf(_) => 0
        case Branch(l,r) => 1 + depth(l) + depth(r)
    }

    /** Exercise 3.28 - write a map function */
    def map[A,B](t : Tree[A])(f : A => B) : Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

    /** Exercise 3.29 - Write a fold function and update size, maximum and depth to use the fold. */
    def fold[A,B](t : Tree[A])(f : A => B)(g : (B,B) => B) : B = t match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g((fold(l)(f)(g)), (fold(r)(f)(g)))
    }

    def size1[A](t : Tree[A]) : Int =
        fold(t) (a => 1) (1 + _ + _)
    def maximum1(t : Tree[Int]) : Int =
        fold(t) (a => a) (_ max _)
    def depth1[A](t : Tree[A]) : Int =
        fold(t) (a => 0) ((l,r) => 1 + l max  r)
    def map1[A,B](t : Tree[A])(f : A => B) : Tree[B] =
        fold(t) (a => Leaf(f(a)) : Tree[B]) (Branch(_,_))
}
