import functional.part1.pList._

val lst1 = PList("A", "B", "C", "D")


lst1.map(v => v.toLowerCase)
lst1.map(_.charAt(0).toInt) 

