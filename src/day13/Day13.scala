package day13

import scala.io.Source

object Day13 {


  def main(args: Array[String]): Unit = {
    println("Day 13")

    val input = Source.fromFile("src/day13/input.txt").getLines.toList

    val time = input.head.toInt
    val schedule = input(1).split(",").filter(_ != "x").map(_.toInt).toList

    println(s"$time $schedule")

    //println(part1(time, schedule))

    println(part2(input(1).split(",").map(s => if (s == "x") 0 else s.toInt)))
  }

  def part1(time:Int, schedule:List[Int]):Int = {
    val mods = schedule.map(bus => bus - (time % bus))
    val smallest = mods.min

    val index = mods.zipWithIndex.min._2
    val smallestBus = schedule(index)

    println(s"mods $mods smallest $smallest index $index Id $smallestBus")

    smallestBus * smallest
  }

  def part2(schedule:Array[Int]):BigInt = {
    val withIndex = schedule.zipWithIndex
    var time = BigInt(0)
    var found = false
    while (!found) {
      time = time + 1
      if (time % BigInt(100000000) == 0) {
        println(time)
      }
      //println(s"time $time")
      //1202161486
      val size = withIndex.count(p => p._1 == 0 || (time + p._2) % p._1 == 0)
      found = size == schedule.length
    }

    time
  }
}
