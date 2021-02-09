// scala builtin Either type
import functional.part4.listZippers._


val lst = List(1,2,3,4,5,6)

def replace[A](v:A): ListZipper[A, Unit] = for (
  _ <- delete;
  _ <- insert(v)
) yield ()


lst.runZipper(
  for (
    _ <- moveRight;
    _ <- moveRight;
    _ <- replace(2222)
  ) yield ()
)

def duplicate[A]: ListZipper[A, Unit] = for (
  x <- get;
  _ <- insert(x.get)
) yield ()


lst.runZipper(
  for (
    _ <- moveRight;
    _ <- duplicate
  ) yield ()
)

val add1: ListZipper[Int, Unit] = for (
  x <- get;
  _ <- replace(x.get + 1)
) yield ()



// There is some wierdness with how we defined the base operations
// so we need to move on place right first before starting and it
// ends up not updating the last element becuase we reach the end and
// don't do the op,
// we can design a more consistent sematics so that this will work better
lst.runZipper(
  moveRight.flatMap(_ => applyUntilRight(isEnd, add1))
)
