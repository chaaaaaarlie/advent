import scala.annotation.tailrec
import scala.collection.StringOps._

object Six {

  val input =  Seq(1, 4, 1, 1, 1, 1, 5, 1, 1, 5, 1, 4, 2, 5, 1, 2, 3, 1, 1, 1, 1, 5, 4, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 5, 1, 4, 1, 1, 4, 1, 1, 1, 1, 4, 1, 1, 5, 5, 1, 1, 1, 4, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1, 1, 1, 2, 3, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 3, 1, 1, 1, 3, 3, 5, 1, 4, 1, 1, 5, 1, 1, 4, 1, 5, 3, 3, 5, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 4, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 4, 1, 1, 1, 1, 3, 1, 2, 3, 4, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, 1, 1, 4, 2, 5, 2, 1, 1, 4, 1, 1, 5, 1, 1, 5, 1, 5, 5, 1, 3, 5, 1, 1, 5, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 4, 3, 1, 1, 4, 1, 4, 1, 1, 1, 1, 4, 1, 4, 4, 4, 3, 1, 1, 3, 2, 1, 1, 1, 1, 1, 1, 1, 4, 1, 3, 1, 1, 1, 1, 1, 1, 1, 5, 2, 4, 2, 1, 4, 4, 1, 5, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 4, 2, 3, 2, 1, 1, 2, 1, 5, 2, 1, 1, 4, 1, 4, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 2, 1, 1, 2)
    .groupBy(x => x).map(x => x._1 -> x._2.size.toLong)
  @tailrec
  def spendTime(fish: Map[Int, Long], days: Int, spent: Int = 0): Map[Int, Long] = {

    val fishString = (0 to 8).map{x => s"$x days: " + fish.getOrElse(x, 0L).toString}.mkString("; ")

    println(s"After $spent days:  (${fish.values.sum} fish) $fishString")
    if (days == spent)
      fish
    else {
      val updated = (0 to 7).foldRight(Map.empty[Int, Long]) { (i, acc) =>

        if (i == 0) {
  acc.updated(6, acc.getOrElse(6, 0L) + fish.getOrElse(0, 0L)).updated(8, fish.getOrElse(0, 0L)).updated(0, fish.getOrElse(1, 0L))
        }
        else {
          acc.updated(i, fish.getOrElse(i + 1, 0L))

        }
      }

      spendTime(updated, days, spent + 1)
    }
  }

    val result = spendTime(input, 80).values.sum

}
