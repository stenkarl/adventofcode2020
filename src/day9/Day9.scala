package day9

import scala.io.Source

object Day9 {

  val input = Source.fromFile("src/day9/input.txt").getLines.map(BigInt(_)).toList


  def main(args: Array[String]): Unit = {
    println("Day 9")

    println(input)
    println(part2(part1()))
  }

  def part1():BigInt = {
    val window = 25
    var index = window
    while (index < input.size &&
            hasPair(slice(index - window, window), input(index))) {

      index = index + 1
    }
    input(index)
  }

  def part2(target:BigInt):BigInt = {
    for (w <- 2 until input.size) {
      val list = checkWindow(w, target)
      if (list.nonEmpty) return findSmallestAndBiggest(list)
    }
    0
  }

  def findSmallestAndBiggest(list:List[BigInt]) = list.min + list.max

  def checkWindow(window:Int, target:BigInt):List[BigInt] = {
    println(s"checkWindow $window")
    var index = 0
    var list = slice(index, window)
    while (index + window < input.size && list.sum != target) {
      println(s"$index $list")
      list = slice(index, window)
      index = index + 1
    }
    if (index + window == input.size) Nil else { println(s"Found $index $list"); list }
  }

  def hasPair(from:List[BigInt], target:BigInt):Boolean = {
    val pair = for (first <- from;
         second <- from if (first != second) && first + second == target)
      yield (first, second)

    //println(s"$target $from $pair")

    pair.nonEmpty
  }

  def slice(from:Int, window:Int):List[BigInt] =
    input.slice(from, Math.min(from + window, input.size))

}
