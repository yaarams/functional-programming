package functional.part4

import functional.part3.monad.Monad

object stateMonad {
  
  case class State[S, A](run: S => (S, A))
    
  given[S] as Monad[[X] =>> State[S, X]] {

    override def pure[A](a: A): State[S, A] = State(s => (s, a))
    
    override def flatMap[A, B](ma: State[S, A], f: A => State[S, B]): State[S, B] = State(
      s0 => {
        val (s1, a) = ma.run(s0)
        f(a).run(s1)
      }
    ) 
    
  }
  
  object State {
    // very similar to Reader ask
    def get[S]: State[S, S] = State(s => (s, s))
    
    // ignore current state, override the state with the parameter
    def set[S](newState: S): State[S, Unit] = State(_ => (newState, ()))

  }
  
}
