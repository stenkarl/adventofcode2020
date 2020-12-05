package day5

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    println("Day 5")

    val input = Source.fromFile("src/day5/input.txt").getLines.toList

    val ids = input.map(findId)

    println(s"max id ${ids.max}")

    val sorted = ids.sorted
    println(sorted)
    println(findGap(sorted, 0))
  }

  def findGap(list:List[Int], index:Int):Int = {
    if (list(index) + 1 != list(index + 1)) return list(index) + 1
    findGap(list, index + 1)
  }

  def findId(assignment:String):Int = {
    val row = findHalf((0, 127), assignment,0)
    val seat = findHalf((0, 7), assignment, assignment.size - 3)
    val id = row._1 * 8 + seat._1
    println(s"Row ${row._1} Seat ${seat._1} Id:${id}")

    id
  }

  def findHalf(range:(Int, Int), direction:String, index:Int):(Int, Int) = {
    val mid = (range._2 + range._1) / 2
    if (range._1 == range._2) return range
    //println(s"findHalf $range $index $mid ${direction(index)}")
    direction(index) match {
      case 'F' => findHalf((range._1, mid), direction, index + 1)
      case 'B' => findHalf((mid + 1, range._2), direction, index + 1)
      case 'L' => findHalf((range._1, mid), direction, index + 1)
      case 'R' => findHalf((mid + 1, range._2), direction, index + 1)
    }
  }

}
