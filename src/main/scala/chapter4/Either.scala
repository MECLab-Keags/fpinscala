package fpinscala.chapter4


sealed trait Either[+E,+A]{
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]) : Either[EE,B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
}
case class Left[+E](e: E) extends Either[E, Nothing]
case class Right[+A](a: A) extends Either[Nothing, A]

object Either{
  def traverse[E,A,B](xs: List[A])(f: A => Either[E,B]) : Either[E, List[B]] = xs match {
    case Nil => Right(Nil)
    case h :: t => f(h) flatMap(hh => traverse(t)(f) map(hh :: _))
  }

  def sequence[E,A](xs: List[Either[E,A]]): Either[E, List[A]] =
    traverse(xs)(x => x)

  def Try[A](a: => A): Either[Exception,A] =
    try Right(a)
    catch { case ex : Exception => Left(ex) }
}
