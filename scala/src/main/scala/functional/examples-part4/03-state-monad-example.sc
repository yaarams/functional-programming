import functional.part3.monad._
import functional.part3.applicative._
import functional.part4.stateMonad._

// Reverse Polish Notation helper functions

type WithIntStack[A] = State[List[Int], A]

def getStack(): WithIntStack[List[Int]] = State.get

def top(): WithIntStack[Int] = for {
  stack <- getStack()
} yield stack.head

def pop(): WithIntStack[Int] = for {
  stack <- getStack()
  _ <- State.set(stack.tail)
} yield stack.head

def push(v: Int): WithIntStack[Unit] = for {
  stack <- getStack()
  _ <- State.set(v :: stack)
} yield ()


// polish notation
def add : WithIntStack[Unit] = for {
  a <- pop()
  b <- pop()
  _ <- push(a + b)
} yield ()

def subtract : WithIntStack[Unit] = for {
  a <- pop()
  b <- pop()
  _ <- push(a - b)
} yield ()


// process a list of instructions

enum PolishOp {
  case Value(v: Int)
  case ADD
  case SUB
}

import PolishOp._

def reversePolishInstructions(instructions: List[PolishOp]): WithIntStack[Int] = {

  if (instructions.isEmpty) {
    pop()
  } else {
    val op = instructions.head match {
      case Value(v) => push(v)
      case ADD => add
      case SUB => subtract
    }

    if (instructions.length > 1) {
      op >> reversePolishInstructions(instructions.tail)
    } else {
      op >> pop()
    }
  }
}


val polishComputation = reversePolishInstructions(
  List(Value(1), Value(2), ADD, Value(9), SUB)
)

polishComputation.run(List.empty)._2 // run it with an initial empty stack, and get the result at the end

// different appraoach
// use the operations directly!

val program1 = push(1) >> push(2) >> add >> push(9) >> subtract >> pop()
program1.run(List.empty)._2 // get the returned value (_1 is the state)

// we have sequence why not use it?
val program2 = List(push(1), push(7), add, push(9), subtract).sequence >> pop()
program2.run(List.empty)._2 // get the returned value (_1 is the state)




// A more complicated example

type WithIndex[A] = State[Int, A]

def purelyFunctionalZipWithIndex[T](lst: List[T]): List[(Int, T)] = {
  // build a stateful computation using the state monad and fold

  val statefulOperation = lst.foldLeft(
    // here we're using a type hint for Nil, fold gets confused when used with lists because it tries to infer the type
    // from Nil which has a Nothing type (nothing to do with the state monad)
    Monad[WithIndex].pure(Nil: List[(Int, T)])
  ) {
    (acc, value) =>
      for {
        tail <- acc
        i <- State.get
        _ <- State.set(i + 1)
      } yield (i, value) :: tail
  }

  statefulOperation.run(0)._2.reverse
}

purelyFunctionalZipWithIndex(List("a", "b", "c", "d"))
