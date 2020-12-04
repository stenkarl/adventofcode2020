package day3

import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {
    println("Day 3")

    val input = Source.fromFile("src/day3/input.txt").getLines.toList

    val ground = input.map(str => str.map {
      case '#' => Tree()
      case _ => Empty()
    }.toList)

    val terrain = Terrain(ground)

    val oneOne = checkSlope(terrain, 1, 1)
    val threeOne = checkSlope(terrain, 3, 1)
    val fiveOne = checkSlope(terrain, 5, 1)
    val sevenOne = checkSlope(terrain, 7, 1)
    val oneTwo = checkSlope(terrain, 1, 2)

    println(s"1:1 $oneOne")
    println(s"3:1 $threeOne")
    println(s"5:1 $fiveOne")
    println(s"7:1 $sevenOne")
    println(s"1:2 $oneTwo")

    val product:BigInt = BigInt(oneOne) * BigInt(threeOne) * BigInt(fiveOne) * BigInt(sevenOne) * BigInt(oneTwo)

    println(s"Product $product")


  }

  def checkSlope(terrain:Terrain, mx:Int, my:Int):Int = {
    var x, y = 0
    var trees:Int = 0
    while (!terrain.pastBottom(y)) {
      val curGround:Int = terrain.at(x, y) match {
        case Tree() => 1
        case Empty() => 0
      }
      trees = trees + curGround
      y += my
      x += mx
    }
    trees
  }

  sealed trait Ground
  case class Tree() extends Ground
  case class Empty() extends Ground

  case class Terrain(ground:List[List[Ground]]) {

    val height = ground.size

    def pastBottom(y:Int) = y >= height

    def at(x:Int, y:Int): Ground = {
      val line = ground(y)
      line(x % line.size)
    }
  }

}
