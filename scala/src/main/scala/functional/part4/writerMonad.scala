package functional.part4

import functional.part3.monad.Monad
import functional.part3.monoid.Monoid

object writerMonad {

  case class Writer[W, +A](written: W, value: A)

  given [W: Monoid]: Monad[[A] =>> Writer[W, A]] with {

    override def pure[A](a: A): Writer[W, A] = Writer(Monoid[W].empty, a)

    override def flatMap[A, B](ma: Writer[W, A], f: A => Writer[W, B]): Writer[W, B] = {
      val mb = f(ma.value)
      Writer(Monoid[W].combine(ma.written, mb.written), mb.value)
    }
  }

  object Writer {

    // alias for pure that is more readable for this abstraction
    def value[W: Monoid, A](a: A): Writer[W, A] = Monad[[A] =>> Writer[W, A]].pure(a)

    // a writer specific constructur unlike pure which is a general monad constructor
    def tell[W, A](w: W): Writer[W, Unit] = Writer(w, ())

  }
  
}
