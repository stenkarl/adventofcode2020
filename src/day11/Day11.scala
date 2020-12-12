package day11

import scala.io.Source

object Day11 {

  val input = Source.fromFile("src/day11/sample.txt").getLines


  def main(args: Array[String]): Unit = {
    println("Day 11")

    val list:List[List[Square]] = input.map(_.map {
      case '.' => Floor()
      case '#' => Occupied()
      case 'L' => Empty()
    }.toList).toList
  }

  def adjacent(list:List[List[Square]], row:Int, col:Int):Int = {
    var num = 0
    // top left

    // top center

    // top right

    // left

    // right

    // bottom left

    // bottom center

    // bottom right

    num
  }

}

sealed trait Square

case class Floor() extends Square {

  override def toString: String = "."
}

case class Empty() extends Square {

  override def toString: String = "L"
}

case class Occupied() extends Square {

  override def toString: String = "#"
}
