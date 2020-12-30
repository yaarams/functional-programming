package functional.part2

object distribution {

  case class Prob[A](value: A, prob: Double) // just a named tuple of 2
  case class Dist[A](dist: List[Prob[A]]) {
    
    def mostProbable: A = this.dist.head.value
    
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): Dist[B] = Dist.map(this, f)
    def flatMap[B](f: A => Dist[B]): Dist[B] = Dist.flatMap(this, f)
  }
 
  object Dist {

    // pure value is just a probability distribution of a single value with a probability of 1
    def pure[A](v: A): Dist[A] = Dist(List(Prob(v, 1.0)))

    def map[A, B](ma: Dist[A], f: A => B): Dist[B] = Dist {
      ma.dist.map {
        case Prob(v, p) => Prob(f(v), p)
      }
    }

    def flatMap[A, B](ma: Dist[A], f: A => Dist[B]): Dist[B] = Dist {
      val combinedDist = for (
        d1 <- ma.dist;
        d2 <- f(d1.value).dist
      ) yield {
        Prob(d2.value, d1.prob * d2.prob)
      }

      // We don't have to but lets keep our distriution so that the most probably is first
      combinedDist.sortBy(_.prob).reverse
    }

    def ap[A, B](ma: Dist[A], ff: Dist[A => B]): Dist[B] =
      for (
        a <- ma;
        f <- ff
      ) yield f(a)

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: Dist[A], b: Dist[B], f: (A, B) => C): Dist[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: Dist[A], b: Dist[B], c: Dist[C], f: (A, B, C) => D): Dist[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: Dist[A], b: Dist[B]): Dist[(A, B)] = map2(a, b, (_, _))
  }
}
