/**
  * Chapter 2: Getting started with functional programming in Scala
  * */
object Chapter2{
    def abs(x : Int) : Int =
      if(x < 0) -x
      else x

    def formattedAbs(x : Int) : String = {
        val msg = "The absolute number of %d is %d"
        msg.format(x, abs(x))
    }

    // Tail Recursive function.
    def factorial(x : Int) : Int = {
        @annotation.tailrec
        def go(n : Int, accumulation : Int): Int =
            if(n <= 0) accumulation
            else go(n-1, n * accumulation)

        go(x, 1)
    }

    def formattedFactorial(x : Int) : String = {
        val msg = "The factorial of %d is %d"
        msg.format(x, factorial(x))
    }


    // Tail Recursive function.
    // 0,1,1,2,3,5
    def findFibonacci(x : Int) : Int = {
        @annotation.tailrec
        def go(counter : Int, target : Int, preceding : Int, current : Int) : Int = {
            val fib = preceding + current
            if(counter == target) fib
            else go(counter + 1, target, current, fib)
        }

        go(0, x, 0, 1)
    }

    // 2.4.2 - Higher Order Function
    // Function that takes a function as a parameter.
    // This can replace the formattedXX functions that are specific to the mathematical operation
    def formatResult(name: String, n: Int, f: Int => Int) : String = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    /** 2.5.1: Polymorphic Function */
    def monomorphic(xs: Array[String], key: String): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n > xs.length) -1
            else if(xs(n) == key) n
            else loop(n + 1)

        loop(0)
    }

    def polymorphic[A](xs: Array[A], f: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n > xs.length) -1
            else if(f(xs(n))) n
            else loop(n + 1)

        loop(0)
    }

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean =
            if(n > as.length) false
            else if(!ordered(as(n), as(n + 1))) false
            else loop(n + 1)
        loop(0)
    }

    /** 2.6 Following types to implementations */
    def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
        // We start with two parameters first an 'a' of type A and a function that takes (A,B) => C
        // So... we return a function that partially applies f with the given a so the consumer
        // will have to receive the resulting function and apply b to finally get C.
        // makes sense...!
        b => f(a, b)
    }

    // Exercise 2.3
    def curry[A,B,C](f: (A,B) => C): A => B => C = {
        a => b => f(a, b)
    }
    // Exercise 2.4
    def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
        (a,b) => f(a)(b)
    }
    // Exercise 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        /** Returns a function that takes a that returns a function that executes 'g' using a.
          * this returns a value of B, use this value to execute 'f' to get a value of C
          */

        a => f(g(a))
        /**
          * Could also be written:
          * f andThen g
          * */
    }



}

