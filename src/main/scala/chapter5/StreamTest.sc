import scala.collection.immutable.{Stream => _, _}

Stream(1,2,3,4,5).take2(3).toListFast
Stream(1,2,3,4,5).takeWhile(x => x() < 3).toListFast

Stream(1,2,3,4,5).take(1).toListFast


/** Exercise 5.4 Tests */
Stream(1,2,3,4).all(x => x > 0)
Stream(1,2,3,4).all(x => x > 1)