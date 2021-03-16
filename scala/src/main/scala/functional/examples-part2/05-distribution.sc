// scala builtin Either type
import functional.part2.dist._

case class Position(x: Int, y: Int)

def moveLeft(pos: Position): Dist[Position] = Dist(
  (Position(pos.x - 1, pos.y), 0.8),
  (Position(pos.x - 1, pos.y - 1), 0.1),
  (Position(pos.x - 1, pos.y + 1), 0.1)
)

def moveUp(pos: Position): Dist[Position] = Dist(
  (Position(pos.x, pos.y - 1), 0.8),
  (Position(pos.x + 1, pos.y - 1), 0.1),
  (Position(pos.x - 1, pos.y - 1), 0.1)
)

def moveRight(pos: Position): Dist[Position] = Dist(
  (Position(pos.x + 1, pos.y), 0.8),
  (Position(pos.x + 1, pos.y - 1), 0.1),
  (Position(pos.x + 1, pos.y + 1), 0.1)
)

def moveDown(pos: Position): Dist[Position] = Dist(
  (Position(pos.x, pos.y + 1), 0.8),
  (Position(pos.x - 1, pos.y + 1), 0.1),
  (Position(pos.x + 1, pos.y + 1), 0.1)
)


def moveUpLeft(p: Position) =
  for (
    p1 <- moveUp(p);
    p2 <- moveLeft(p1)
  ) yield p2


moveUpLeft(Position(0,0))
moveUpLeft(Position(0,0)).mostProbable
