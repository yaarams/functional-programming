// scala builtin Either type
import functional.part2.random.*


val seed0 = 25214903928L
val seed1 = 50429807845L
// will be different every time we run it
val seed2 = System.currentTimeMillis()


val random: Rand[Double] =
  Rand.randomInt.map(x => math.abs(x).toDouble/Int.MaxValue) // will give a value between 0 and 1


random.run(seed0)
random.run(seed1)
random.run(seed2)


def randomRange(n: Int): Rand[Int] = {
  random.map(x => math.floor(x * n).toInt)
}

randomRange(5).run(seed0)
randomRange(5).run(seed1)
randomRange(5).run(seed2)


def randomChoice[A](lst: List[A]): Rand[A] = {
  randomRange(lst.length).map(i => lst(i))
}

randomChoice(List("A", "B", "C")).run(seed0)
randomChoice(List("A", "B", "C")).run(seed1)
randomChoice(List("A", "B", "C")).run(seed2)


enum CardShape {
  case Heart;
  case Spade;
  case Tiles;
  case Pikes;
}

case class Card(shape: CardShape, value: Int)

val randomCard: Rand[Card] =
  for(
    shape <- randomChoice(
      List(
        CardShape.Heart,
        CardShape.Spade,
        CardShape.Tiles,
        CardShape.Pikes
      )
    );
    value <- randomRange(13)
  ) yield Card(shape, value)


randomCard.run(seed0)
randomCard.run(seed1)
randomCard.run(seed2)


val threeCards = Rand.map3(randomCard, randomCard, randomCard, List(_,_,_))

threeCards.run(seed0)
threeCards.run(seed1)
threeCards.run(seed2)


