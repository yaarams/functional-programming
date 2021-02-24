package functional.part4

import functional.part3.monad.Monad

object readerMonad {
 
  // it's just a functions no need for some wrapper
  case class Reader[C, A](run: C => A)
  
  given[C] as Monad[[X] =>> Reader[C, X]] {

    override def pure[A](a: A): Reader[C, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[C, A], f: A => Reader[C, B]): Reader[C, B] = Reader(
      (c: C) => {
        val a = ma.run(c)
        f(a).run(c)
      }
    )
  }

  object Reader {
    
    def ask[C]: Reader[C, C] = Reader(identity) // identity is f(x) = x
    
  }
  
}
