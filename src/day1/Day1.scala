package day1

import scala.io.Source

object Day1 {

  def main (args: Array[String] ): Unit = {
    println("Day 1")

    var numbers:List[Int] = Source.fromFile("src/day1/input.txt").getLines.map(_.toInt).toList

    println(s"Part one: ${partOne(numbers)}")

    println(s"Part two: ${partTwo(numbers)}")
  }

  def partOne(numbers:List[Int]): Int = {
    val pair = for (first <- numbers;
    second <- numbers if first != second && first + second == 2020 )
    yield (first, second)

    pair.map(p => p._1 * p._2).head
  }

  def partTwo(numbers:List[Int]): Int = {
    val pair = for (first <- numbers;
                    second <- numbers;
                    third <- numbers
                    if first != second && second != third && first + second + third == 2020 )
      yield (first, second, third)

    pair.map(p => p._1 * p._2 * p._3).head
  }
}

