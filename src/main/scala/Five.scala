object Five {


  case class Coord(x: Int, y: Int)


def parseInput(in : Seq[String]) : Seq[(Coord, Coord)] = in.map { l =>
  val regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  val regex(x1, y1, x2, y2) = l

  (Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))
}

  val input = parseInput(scala.io.Source.fromFile("resources/5.txt").getLines.toSeq)

  val sample = parseInput("""0,9 -> 5,9
                 |8,0 -> 0,8
                 |9,4 -> 3,4
                 |2,2 -> 2,1
                 |7,0 -> 7,4
                 |6,4 -> 2,0
                 |0,9 -> 2,9
                 |3,4 -> 1,4
                 |0,0 -> 8,8
                 |5,5 -> 8,2""".stripMargin.split("\n"))

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


  def buildMap(coords : Seq[(Coord, Coord)], debug : Boolean = false) : scala.collection.mutable.TreeMap[Coord, Int] = {

    coords.foldLeft(scala.collection.mutable.TreeMap.empty[Coord, Int](o)) { (m, pt) =>

      val dX = pt._2.x compare pt._1.x

      val dY = pt._2.y compare pt._1.y

      if (debug) println(s"Processing point ${pt._1}, ${pt._2} with slope ($dX, $dY)")
      val xR = pt._1.x to pt._2.x by (if (dX == 0) 1 else dX)
      val yR = pt._1.y to pt._2.y  by (if (dY == 0) 1 else dY)

      val xOrigin = if (dX >= 0) xR.min else xR.max
      val yOrigin = if (dY >= 0 ) yR.min else yR.max
      xR foreach { x =>
        yR foreach { y =>

          if  (dX == 0 || dY== 0 || dX * (x - xOrigin) == dY * (y - yOrigin)) {
            if (debug) println(s"Adding point $x, $y")
            m.update(Coord(x, y), m.getOrElse(Coord(x, y), 0) + 1)
          }
          else if (debug) println(s"Skipping point $x, $y")
        }
      }
      if (debug) println ("----------------------")
      m
    }

  }


}
