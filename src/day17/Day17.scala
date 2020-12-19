package day17

import scala.collection.mutable
import scala.io.Source

object Day17 {

  val initialSpaceSize = 8
  val map = mutable.Map[Point, Boolean]()

  def main(args: Array[String]): Unit = {
    println("Day 17")

    val input = Source.fromFile("src/day17/input.txt").getLines.toList

    val values = for (y <- input.indices;
         x <- input(y).indices) yield (Point(x, y, 0), input(y)(x) == '#')

    println(input)

    println(values)

    values.foreach (p => map(p._1) = p._2)

    println(map)

    println(s"Part 1, num active ${part1()}")
  }

  def part1(): Int = {
    println(s"Cycle 0 active: ${sumActive()}")

    for (i <- 1 to 6) {

      cycle(i)
      val active = sumActive()

      println(s"Cycle  active: $active")

    }

    val active = sumActive()
    active
  }

  def sumActive():Int = {
    map.count(p => p._2)
  }

  def cycle(cycleNum:Int):Unit = {
    val extent = initialSpaceSize + cycleNum
    var changes = List[(Point, Boolean)]()
    //for (p <- map.keys) {
    val pointsToCheck = for (x <- -extent to extent;
         y <- -extent to extent;
         z <- -extent to extent) yield Point(x, y, z)

    println(s"Points to check ${pointsToCheck.size}")
    pointsToCheck.foreach { p =>
      val active = map.getOrElse(p, false)
      val neighbors = getNeighbors(p)
      val numActive = neighbors.count(n => map.getOrElse(n, false))
      //println(s"numActive $numActive for $p neighbors size ${neighbors.size} $neighbors")

      changes = if (active && (numActive == 2 || numActive == 3)) {
        (p, true) :: changes
      } else if (active) {
        (p, false) :: changes
      } else if (!active && numActive == 3) {
        (p, true) :: changes
      } else {
        (p, false) :: changes
      }
    }
    //println(changes)
    applyChanges(changes)
  }

  def applyChanges(changes:List[(Point, Boolean)]): Unit = {
    changes.foreach (p => map(p._1) = p._2)
  }

  def getNeighbors(point: Point):List[Point] = {
    val neighbors = (for (x <- point.x - 1 to point.x + 1;
         y <- point.y - 1 to point.y + 1;
         z <- point.z - 1 to point.z + 1)
      yield Point(x, y, z))

    (neighbors diff List(point)).toList
  }

}

case class Point(x:Int, y:Int, z:Int)
