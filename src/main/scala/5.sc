
case class Coord(x: Int, y: Int)

val sample =
  """0,9 -> 5,9
    |8,0 -> 0,8
    |9,4 -> 3,4
    |2,2 -> 2,1
    |7,0 -> 7,4
    |6,4 -> 2,0
    |0,9 -> 2,9
    |3,4 -> 1,4
    |0,0 -> 8,8
    |5,5 -> 8,2""".stripMargin.split("\n").map { l =>
    val regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

    val regex(x1, y1, x2, y2) = l
    (Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))
  }.filter {
    case (p1, p2) => p1.x == p2.x || p1.y == p2.y
  }

object o extends Ordering[Coord] {
  def compare(p1: Coord, p2: Coord) = if (p1.x.compare(p2.x) == 0) p1.y compare p2.y else p1.x compare p2.x
}

def printMap(m: scala.collection.mutable.TreeMap[Coord, Int]): String = {
  val xM = m.keys.map(_.x).max
  val yM = m.keys.map(_.y).max

  val sb = new StringBuilder("\n")

  0 to yM foreach { y =>
    0 to xM foreach { x =>
      sb ++= (if (m.isDefinedAt(Coord(x, y))) m(Coord(x, y)).toString else ".")
    }
    sb += '\n'
  }
  sb.toString

}


val m = sample.foldLeft(scala.collection.mutable.TreeMap.empty[Coord, Int](o)) { (m, pt) =>

  println(s"Processing point ${pt._1}, ${pt._2}")
  val xR = if (pt._1.x > pt._2.x)
    (pt._2.x to pt._1.x)
  else (
    pt._1.x to pt._2.x)
  val yR = if (pt._1.y > pt._2.y)
    (pt._2.y to pt._1.y)
  else
    (pt._1.y to pt._2.y)


  xR foreach { x =>
    yR foreach { y =>
      println(s"Adding point $x, $y")
      m.update(Coord(x, y), m.getOrElse(Coord(x, y), 0) + 1)

    }
  }
  println ("----------------------")
  m
}


printMap(m)