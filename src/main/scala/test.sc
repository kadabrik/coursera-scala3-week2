val starting = 10
val money = 3

starting * 2 / 3
money <= starting * 2 / 3

val coins = List(1, 2, 3, 4, 5, 6, 7)
val current = List(1,2, 3, 4)

current.length <=  7 * 2 / 3


case class Point(x: Int, y: Int)
// copy
// toString
// compare
// ...
def findX(p: Point): Int = p match {
  case Point(x, _) if x > 6 => x
  case Point(x, _) => x * 2
  case _ => 0
}

val x = Point(5, 7)

findX(x)
