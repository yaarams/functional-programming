package functional.part2

object logging {

  case class Logging[+A](log: List[String], value: A) {
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): Logging[B] = Logging.map(this, f)
    def flatMap[B](f: A => Logging[B]): Logging[B] = Logging.flatMap(this, f)


    // like flat map but ignore the value returned from the first operation
    // => A is a shorthand for () => A, and allows the caller to not wrap the argument in a lambda
    def >>[B](f: => Logging[B]): Logging[B] = this.flatMap(_ => f)

    // like flat map but ignore the value returned from the second operation
    // => A is a shorthand for () => A, and allows the caller to not wrap the argument in a lambda
    def <<[B](f: => Logging[B]): Logging[A] = this.flatMap(r => f.map(_ => r))
  }

  
  object Logging {
    def pure[A](v: A): Logging[A] = Logging(List.empty, v)

    def log(msg: String): Logging[Unit] = Logging(List(msg), ())

    def flatMap[A, B](ma: Logging[A], f: A => Logging[B]): Logging[B] = {
      val result = f(ma.value)
      // ++ concatenation of lists
      Logging(ma.log ++ result.log, result.value)
    }

    // --------------- //
    // Derived Methods //
    // --------------- //

    def map[A, B](ma: Logging[A], f: A => B): Logging[B] = flatMap(ma, a => pure(f(a)))

    def ap[A, B](ma: Logging[A], ff: Logging[A => B]): Logging[B] = {
      for (
        f <- ff;
        a <- ma
      ) yield f(a) // implement ap using map and flatMap
    }

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: Logging[A], b: Logging[B], f: (A, B) => C): Logging[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: Logging[A], b: Logging[B], c: Logging[C], f: (A, B, C) => D): Logging[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: Logging[A], b: Logging[B]): Logging[(A, B)] = map2(a, b, (_, _))
  }


}
