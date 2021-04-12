package functional.part2

object sentOp {

  type TokenIndex = Int
  case class Edge(from: TokenIndex, to: TokenIndex, label: String)
  case class Sentence(tokens: List[String], edges: List[Edge])

  case class SentOp[A](run: (Sentence) => List[A]){
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): SentOp[B] = SentOp.map(this, f)
    def flatMap[B](f: A => SentOp[B]): SentOp[B] = SentOp.flatMap(this, f)
  }


  object SentOp {
    // a useful opreation (this will help us look at the sentence that was passed as a parameter)
    def sentenceInContext(): SentOp[Sentence] = SentOp(s => List(s))
    
    def noResults[A](v: A): SentOp[A] = SentOp(_ => List())
    
    //  The special functions
    def pure[A](v: A): SentOp[A] = SentOp(_ => List(v))
    
    def anyOf[A](v: List[A]): SentOp[A] = SentOp(_ => v)

    def map[A, B](ma: SentOp[A], f: A => B): SentOp[B] = SentOp(
      sent => ma.run(sent).map(f)
    )

    def flatMap[A, B](ma: SentOp[A], f: A => SentOp[B]): SentOp[B] = SentOp(
      sent => ma.run(sent).flatMap(a => f(a).run(sent))
    )

    def ap[A, B](ma: SentOp[A], ff: SentOp[A => B]): SentOp[B] = {
      for (
        f <- ff;
        a <- ma
      ) yield f(a) // implement ap using map and flatMap
    }

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: SentOp[A], b: SentOp[B], f: (A, B) => C): SentOp[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: SentOp[A], b: SentOp[B], c: SentOp[C], f: (A, B, C) => D): SentOp[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: SentOp[A], b: SentOp[B]): SentOp[(A, B)] = map2(a, b, (_, _))
  }    
}
