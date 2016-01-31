/**
  * Chapter 3: Functional Data Structures
  */
package fp101.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sumOld(xs: List[Int]): Int = xs match {
        case Nil => 0
        case Cons(y, ys) => y + sumOld(ys)
    }

    def productOld(xs: List[Double]): Double = xs match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(y, ys) => y * productOld(ys)
    }

    // A variadic function (a function that takes zero, one or more
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def append[A](xs1:List[A], xs2:List[A]) : List[A] =
        xs1 match {
            case Nil => xs2
            case Cons(h,t) => Cons(h, append(t, xs2))
        }

    def foldRight[A,B](as : List[A], b : B)(f : (A,B) => B) : B = as match {
        case Nil => b
        case Cons(x,xs) => f(x, foldRight(xs, b)(f))
    }

    def sum(ns : List[Int]) : Int = {
        foldRight(ns, 0)((x,y) => x + y)
    }

    def product(ns : List[Double]) : Double = {
        foldRight(ns, 1.0)(_*_)
    }
}

object Chapter3 {
    /** Exercise 3.1 - Pattern matching*/
    def e31(xs : List[Int]) : Int = {
        xs match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + List.sum(t)
            case _ => 101
        }
    }

    /** Exercise 3.2 - Implement a tail function
      *
      * Notes: If the function consists of just a single statement then it is good practice
      * to put it on the same line as the method signature, rather than introducing another level of nesting.
      * We could also throw an exception on the Nil case, but this is not the functional way.
      * So Nil is returned instead. */
    def tail[A](xs : List[A]) : List[A] = xs match {
        case Nil => Nil
        case Cons(_, t) => t
    }

    /** Exercise 3.3 - Replace the head of the given list.
      *
      * Notes: The Nil case in this function, however, makes sense to return a new list with
      * just the head set and no tail rather than returning Nil */
    def replaceHead[A](h : A, xs : List[A]) : List[A] = xs match {
        case Nil => Cons(h, Nil)
        case Cons(_, t) => Cons(h, t)
    }

    /** Exercise 3.4 - Generalize exercise 3.2 to drop n number of elements and return the tail. */
    /** This was my original attempt.
      * The below version is a much better solution, the inner recursive function is not required. */
    def drop[A](n : Int, xs : List[A]) : List[A] = {
        @annotation.tailrec
        def go(x : Int, ys : List[A]) : List[A] = ys match {
            case Nil => Nil
            case Cons(_, t) =>
                if(x == n) {
                    println("x:", x, "n:", n)
                    t
                } else {
                    println("x:", x, "n:", n)
                    go(x + 1, t)
                }
        }
        go(0, xs)
    }
    def drop[A](xs : List[A], n : Int) : List[A] = {
        if(n == 0) xs
        else xs match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n-1)
        }
    }

    /** Exercise 3.5 - Implement a dropWhile function which drops the n number of  */
    def dropWhile1[A](xs : List[A], f : A => Boolean) : List[A] = xs match {
        case Cons(h, t) if(f(h)) => dropWhile1(t, f)
        case _ => xs
    }

    /** Curried version of dropWhile which improves type inference */
    def dropWhile[A](xs:List[A])(f: A => Boolean) : List[A] = xs match{
        case Cons(h,t) if(f(h)) => dropWhile(t)(f)
        case _ => xs
    }
    def t35() : Unit = {
        val initial = List(1,2,3,4,5)

        dropWhile1(initial, (x:Int) => x < 4)    // Original version: we need to explicitly define that x is an Int in the predicate anonymous function
        dropWhile(initial)(x => x < 4)           // Curried version: Type inference is able to work out the type of x.
    }

    /** Exercise 3.6 - Implement a function that returns all but the last element */
    /** Note: this is NOT a constant time (i.e. not very efficient) function since we need to copy
      * all the initial elements into new Lists. So the bigger the list the longer this function will take to process.*/
    def initial[A](xs:List[A]) : List[A] = xs match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h,t) => Cons(h, initial(t))
    }

    /** Exercise 3.7 - Can product, implemented using foldRight, immediately halt the recursion
      * and return 0.0 if it encounters a 0.0? Why or why not? */
    /** No since foldRight cannot be short-circuited and the parameters are evaluated before the function is executed.  */

    /** Exercise 3.8 - What happens when Nil is passed. */
    /** Expressed out:
      *     Cons(1, Cons(2, Cons(3, Nil))) (Cons(_,_))
      *     f(Cons(1, f(Cons(2, f(Cons(3, Cons(Nil, Nil)))))))
      * */
    def e38() : List[Int] = {
        List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    }

    /** Exercise 3.9 - Write a length function that uses the foldRight function */
    def length[A](ns : List[A]) : Int = {
        List.foldRight(ns, 0)((_,n) => n + 1)
    }

    /** Exercise 3.10 - Write a foldLeft tail recursive function */
    /** Expressed out:
      *     foldLeft(List(1,2,3), 0) ((x,y) => x + y)
      *     Cons(1, Cons(2, Cons(3, Nil)))
      *     f(1,0) -> f(2, 1) -> f(3, 3) -> 6
      * This is almost a while loop in imperative code with a mutable variable that is accumulated on each element.
      * Instead having the z parameter as just the default value that should be returned on a Nil list, the value of z
      * is the value of the accumulated total, therefore once the end of the list is reached the value of z (i.e the Nil case)
      * is returned and no other work is required.
      * Where as foldRight will still need to apply the function (i.e. fold) on each element.
      * */
    @annotation.tailrec
    def foldLeft[A,B](as : List[A], z : B)(f : (A,B) => B) : B = as match {
        case Nil => z
        case Cons(h,t) => foldLeft(t, f(h, z))(f)
    }


    /** Exercise 3.11 - Write sum, product and length using foldLeft */
    def sumLeft(xs : List[Int]) : Int =
        foldLeft(xs, 0) (_+_)
    def productLeft(xs : List[Double]) : Double =
        foldLeft(xs, 1.0) (_*_)
    def lengthLeft[A](xs : List[A]) : Int =
        foldLeft(xs, 0) ((_,n) => n + 1)

    /** Exercise 3.12 - Write a reverse function using a fold */
    /** Expressed out:
      *     foldLeft(List(1,2,3), Nil) ((x,y) => Cons(x,y))
      *     f(1, Nil) => Cons(1,Nil)
      *     f(2, Cons(1,Nil)) => Cons(2, Cons(1, Nil))
      *     f(3, Cons(2, Cons(1, Nil))) => Cons(3, Cons(2, Cons(1, Nil)))
      * */
    def reverse[A](as : List[A]) : List[A] = {
        Chapter3.foldLeft(as, Nil : List[A]) ((x,xs) => Cons(x,xs))
    }

    /** Exercise 3.13 - 1) Write a foldLeft in terms of foldRight
      *                 2) Write a foldRight in terms of foldLeft making it tail recursive */
    def foldLeftViaRight[A,B](as : List[A], z : B)(f : (A,B) => B) : B =
        List.foldRight(reverse(as), z)(f)
    def foldRightViaLeft[A,B](as : List[A], z : B)(f : (A,B) => B) : B =
        foldLeft(reverse(as), z)(f)
    /** Answer for foldRightViaLeft, this one looks like it doesn't have to process the list twice. */
    def foldRightViaLeft_Answer[A,B](as : List[A], z : B)(f : (A,B) => B) : B =
        foldLeft(as, (b:B) => b) ((a,g) => b => g(f(a, b)))(z)

    /** Exercise 3.14 - write append in terms of either foldLeft or foldRight */
    def append[A](xs:List[A], ys:List[A]) : List[A] =
        foldLeft(xs, ys) ((x, zs) => Cons(x, zs))

    /** Exercise 3.15 - write a function that flattens a list of lists */
    def concat[A](xs:List[List[A]]) : List[A] =
        foldLeft(xs, Nil:List[A]) ((as, bs) => append(as, bs))

    /**  Exercise 3.16 - write a function that transforms a list of Ints by adding 1 to each element */
    def add1(xs:List[Int]) : List[Int] =
        foldLeft(xs, Nil:List[Int]) ((h,t) => Cons(h+1, t))

    /** Exercise 3.17 - write a function that transforms a list of Doubles to string */
    def doubleToString(xs:List[Double]) : List[String] =
        foldLeft(xs, Nil:List[String]) ((h, t) => Cons(h.toString, t))

    /** Exercise 3.18 - write a map function that generalizes transforming each element */
    def map[A,B](as:List[A])(f : (A) => B) : List[B] =
        foldLeft(as, Nil:List[B])((a,bs) => Cons(f(a), bs))

    /** Exercise 3.19 - write a filter function that ignores elements that do not return true by the predicate */
    def filter[A](as:List[A])(f : (A) => Boolean) : List[A] =
        foldLeft(as, Nil:List[A]) ((x,xs) => if(f(x)) Cons(x,xs) else xs)

    /** Exercise 3.20 - write a flatMap function */
    def flatMap[A,B](as:List[A])(f : A => List[B]) : List[B] =
        concat(map(as)(f))

    /** Exercise 3.21 - use flatMap to filter */
    def filter_1[A](as:List[A])(f : A => Boolean) : List[A] =
        flatMap(as)(a => if (f(a)) List(a) else Nil)

    /** Exercise 3.21 - write a function that adds each head of the two lists together and creates a new list
      * of the aggregates.
      * We can create Using pattern matching to create a Tuple pair of lists
      */
    def addPairs(as:List[Int], bs:List[Int]) : List[Int] = (as,bs) match {
        case (_,Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(x,xs),Cons(y,ys)) => Cons(x + y, addPairs(xs, ys))
    }

    def zipWith[A,B](as:List[A], bs:List[A])(f : (A,A) => B) : List[B] = (as,bs) match {
        case (_,Nil) => Nil:List[B]
        case (Nil, _) => Nil:List[B]
        case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
    }
}
