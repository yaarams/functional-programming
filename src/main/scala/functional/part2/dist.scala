package functional.part2


object dist {

  case class Dist[+A](value: List[(A, Double)]) {
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): Dist[B] = Dist.map(this, f)
    def flatMap[B](f: A => Dist[B]): Dist[B] = Dist.flatMap(this, f)

    // This is true if we keep the the list sorted by probability
    def mostProbable: A = value.head._1

  }
  
  object Dist {

    // constructor
    def apply[A](values: (A, Double)*): Dist[A] = Dist(List(values.sortBy(d => -d._2):_*))
    
    def pure[A](v: A): Dist[A] = Dist(List((v, 1.0)))

    def flatMap[A, B](ma: Dist[A], f: A => Dist[B]): Dist[B] = Dist(
      (
        for (
          (v0, p0) <- ma.value;
          (v1, p1) <- f(v0).value
        ) yield (v1, p0 * p1)
      ).sortBy(d => -d._2)
    )

    // --------------- //
    // Derived Methods //
    // --------------- //

    def map[A, B](ma: Dist[A], f: A => B): Dist[B] = flatMap(ma, a => pure(f(a)))

    def ap[A, B](ma: Dist[A], ff: Dist[A => B]): Dist[B] = {
      for (
        f <- ff;
        a <- ma
      ) yield f(a) // implement ap using map and flatMap
    }

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: Dist[A], b: Dist[B], f: (A, B) => C): Dist[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: Dist[A], b: Dist[B], c: Dist[C], f: (A, B, C) => D): Dist[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: Dist[A], b: Dist[B]): Dist[(A, B)] = map2(a, b, (_, _))
  }
  
}
