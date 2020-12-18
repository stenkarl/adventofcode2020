package day15

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day15 {


  def main(args: Array[String]): Unit = {
    println("Day 15")

    val input = List(1,17,0,10,18,11,6)
    //val input = List(3,1,2)

    //println(part1(input))

    println(part2(input))
  }

  def part1(start:List[Int]):Int = {
    val turns:ListBuffer[Int] = ListBuffer()
    turns.appendAll(start)

    for (i <- start.size + 1 to 30000000) {
      turns += nextTurn(turns)
      if (i % 100000 == 0) println (s"Turn ${i}")
    }

    turns.last
  }

  def nextTurn(turns:ListBuffer[Int]):Int = {
    val reversed = turns.zipWithIndex.reverse.tail
    //println(reversed)
    val maybe = reversed.find(p => p._1 == turns.last)
    //println(maybe)
    val next = maybe match {
      case Some(p) => turns.size - 1 - p._2
      case None => 0
    }

    next
  }

  def nextTurn(turns:mutable.Map[Int, Int], current:(Int, Int)):Int = {
    //println(reversed)

    val maybe = turns.get(current._1)
    //println(maybe)
    val next = maybe match {
      case Some(value) => {
        //println(s"${current._1} found. next is ${current._2 - value}")
        current._2 - value
      }
      case None => /*println(s"${current._1} not found, next is 0");*/ 0
    }

    next
  }

  def part2(start:List[Int]):Int = {
    val turns:mutable.Map[Int, Int] = mutable.Map()
    start.zipWithIndex.foreach { it =>
      if (it._2 < start.size - 1)
        turns(it._1) = it._2 + 1
    }

    val beginIndex = start.size + 1
    var current = (start.last, start.size)
    var next = -1
    for (i <- beginIndex to 30000000) {
      //println(s"Begin turn $i current $current $turns")
      next = nextTurn(turns, current)
      turns(current._1) = current._2
      current = (next, i)
      //println(s"End turn $i current $current $turns")
      //if (i % 100000 == 0) println (s"Turn ${i}")
    }

    next
  }
}
