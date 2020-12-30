package functional.part2

object random {

  type Seed = Long

  // Simple pseudo-random number generation
  // Implementation taken from the the Functional Programming in Scala Book
  def rnd(seed: Seed): (Int, Seed) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    ((nextSeed >>> 16).asInstanceOf[Int], nextSeed)
  }
  
  case class Rand[A](run: (Seed) => (A, Seed)) {
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): Rand[B] = Rand.map(this, f)
    def flatMap[B](f: A => Rand[B]): Rand[B] = Rand.flatMap(this, f)
  }
 
 
  object Rand {
    val randomInt: Rand[Int] = Rand(rnd)

    def pure[A](v: A): Rand[A] = Rand((seed) => (v, seed))

    // simple, just apply the function to all values of the list
    def map[A, B](ma: Rand[A], f: A => B): Rand[B] = Rand((seed) => {
      val (result, nextSeed) = ma.run(seed)
      (f(result), nextSeed)
    })

    // In a stateful computation we first pass the current state to the first computation
    // and then use the state returned from the first computation as the state we pass
    // to the second computation. The final resulting state is the state after running the
    // second computation
    def flatMap[A, B](ma: Rand[A], f: A => Rand[B]): Rand[B] = Rand((seed0) => {
      val (result1, seed1) = ma.run(seed0)
      val mb = f(result1)
      mb.run(seed1)
    })

    // all possible values cross product with all possible operations  
    def ap[A, B](ma: Rand[A], ff: Rand[A => B]): Rand[B] = {
      for (
        a <- ma;
        f <- ff
      ) yield f(a) // implement ap using map and flatMap
    }

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: Rand[A], b: Rand[B], f: (A, B) => C): Rand[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: Rand[A], b: Rand[B], c: Rand[C], f: (A, B, C) => D): Rand[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] = map2(a, b, (_, _))
  }
}
