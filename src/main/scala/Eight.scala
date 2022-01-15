import scala.io.Source

object Eight {

  val in = Source.fromFile("resources/8.txt").getLines.toSeq


    def parseInput(in : Seq[String]) = in.map{ln =>

    val regex = "(.*) \\| (.*)".r

    val regex(input, output) = ln

    (input.split(" "), output.split(" "))
  }





  def decodeLines(in : Seq[(Seq[String], Seq[String])]) : Seq[String] = in.map{
    case (input, output) =>
      val codeMap = Seq("a", "b", "c", "d", "e", "f", "g").foldLeft(Map.empty[String, String]){(map, v) =>
        v match {
          case "a" => map.updated("a", lengthMap(3).head.replaceAll(lengthMap(2).head))
        }

      }



  }

}
