package day10

import scala.io.Source

object Day10 {

  val input = Source.fromFile("src/day10/sample1.txt").getLines.map(_.toInt).toList

  val adapter = 3

  def main(args: Array[String]): Unit = {
    println("Day 10")

    val x = 2^1 * 4^1 * 7^4

    val sorted = 0 :: input.sorted

    println(sorted)

    val differences = sorted.sliding(2, 1).map(p => p(1) - p(0)).toList

    println(differences)

    val diff1:Int = differences.count (_ == 1)

    val diff3:Int = differences.count (_ == 3) + 1

    //println(s"diff3 $diff3")

    println(s"diff1 $diff1")

    val total = diff3 * diff1

    println(total)

    // combos of number of sets of 2 = 2
    // combos of number of sets of 3 = 4
    // combos of number of sets of 4 = 7

    println (differences.sliding(2, 2).toList)
  }

}
