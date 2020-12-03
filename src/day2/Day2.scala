package day2

import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {
    println("Day 2")

    val input = Source.fromFile("src/day2/input.txt").getLines

    val lines:List[Line] = input.map(stringToLine(_)).toList

    println(s"Part one ${partOne(lines)}")
    println(s"Part two ${partTwo(lines)}")

  }

  def stringToLine(str:String): Line = {
    val split = str.split(" ")
    val range = split(0).split("-")
    val min = range(0).toInt
    val max = range(1).toInt

    Line(min, max, split(1).charAt(0), split(2))
  }

  def partOne(lines: List[Line]): Int = {
    lines.count(_.isValid)
  }

  def partTwo(lines: List[Line]): Int = {
    println(lines.filter(_.isPositionValid))
    lines.count(_.isPositionValid)
  }

  case class Line(min:Int, max:Int, ch:Char, pwd:String) {

    val isValid = {
      val filtered = pwd.filter(_ == ch)

      filtered.size >= min && filtered.size <= max
    }

    val isPositionValid = {
      val first = pwd.size > (min - 1) && pwd.charAt(min - 1) == ch
      val second = pwd.size > (max - 1) && pwd.charAt(max - 1) == ch

      if ( pwd == "ppppppppppplppppp") {
        println(s"min $min, max $max, pwd.size ${pwd.size}")
        println(first + " " + second)
      }

      (first || second) && !(first && second)
    }
  }

}
