import fpinscala.chapter4._

Right(List("logs")).map2(Left(List("logs")))((r,l) => r ++ l)
Either.Try(Right(List("logsR"))).map2(Either.Try(Left(List"logsL")))((r,l) => r.a ++ l.e)


