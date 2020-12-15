// scala builtin Either type
import functional.part2.listZippers._


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


