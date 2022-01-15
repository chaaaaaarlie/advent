import scala.annotation.tailrec

val commands = Seq(13,47,64,52,60,69,80,85,57,1,2,6,30,81,86,40,27,26,97,77,70,92,43,94,8,78,3,88,93,17,55,49,32,59,51,28,33,41,83,67,11,91,53,36,96,7,34,79,98,72,39,56,31,75,82,62,99,66,29,58,9,50,54,12,45,68,4,46,38,21,24,18,44,48,16,61,19,0,90,35,65,37,73,20,22,89,42,23,15,87,74,10,71,25,14,76,84,5,63,95)
val boards = scala.io.Source.fromFile("4.txt").getLines.grouped(6).map(_.take(5).map(_.trim.split("\\W+").map(_.toInt)).flatten.toList).toSeq

def markBoard(board : List[Int], value : Int) : List[Int] = board match {
  case Nil => Nil
  case a :: b if (a == value) => 0 :: markBoard(b, value)
  case a :: b => a :: markBoard(b, value)

}

def checkWin(board : List[Int]) : Boolean = {
  board.grouped(5).exists(_.sum == 0) || (0 to 4).exists { i =>
    board.zipWithIndex.filter(_._2 % 5 == i).map(_._1).sum == 0
  }
}

@tailrec
def playGame(boards : Seq[List[Int]], commands : Seq[Int]) : Option[(Seq[List[Int]], Int)] = {
  if (commands.isEmpty)
    None
  else {
    val next = boards.map(markBoard(_, commands.head))
    if (next.exists(checkWin))
      Some(next.filter(checkWin), commands.head)
    else
    playGame(next, commands.drop(1))
  }
}

@tailrec
def finalGame(boards : Seq[List[Int]], commands : Seq[Int]) : Option[(Seq[List[Int]], Int)] = {
  if (commands.isEmpty)
    None
  else {
    val next = boards.map(markBoard(_, commands.head))
    if (next.exists(checkWin) && next.size == 1)
      Some(next.filter(checkWin), commands.head)
    else
      finalGame(next.filter(! checkWin(_)), commands.drop(1))
  }
}





