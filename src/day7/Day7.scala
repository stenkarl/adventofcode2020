package day7

import scala.io.Source

object Day7 {

  var bags:List[Bag] = Nil

  var input = Source.fromFile("src/day7/input.txt").getLines.toList

  def main(args: Array[String]): Unit = {
    println ("Day 7")

    createBags()

    //partOne()
    partTwo()
  }

  def partOne(): Unit = {
    val contains = canContainBag("shiny", "gold")
    println(contains)
    println(contains.size)
  }

  def partTwo():Unit = {
    val b = bags.find(it => it.adjective == "shiny" && it.color == "gold").get
    //val b = bags.find(it => it.adjective == "dark" && it.color == "blue").get

    println(b.countChildren())
  }

  def canContainBag(adj:String, color:String):List[Bag] = {
    bags.filter(_.canContainBag(adj, color))
  }

  def createBags(): Unit = {
    //println (input)
    var removal:List[String] = Nil
    //while (input.nonEmpty) {
    while(input.nonEmpty) {
      println(s"Begin pass ${input.size} input size")
      removal = bagCreatePass(input)
      input = input diff removal
      println (bags)
      println(s"End pass remaining ${input.size} $input")
    }
    println(s"${input.size} remaining input $input")
    println(s"${bags.size} bags $bags")
  }

  def bagCreatePass(input:List[String]):List[String] = {
    var removal:List[String] = Nil

    input.foreach { line =>
      val words = line.split(" ")
      val adjective = words(0)
      val color = words(1)
      if (line.endsWith("contain no other bags.")) {
        bags = Bag(adjective, color, Map()) :: bags
        removal = line :: removal
      } else {
        val contain = "contain "
        val suffix = line.substring(line.indexOf(contain) + contain.length)
        //println(s"suffix $suffix")
        val neededBags = suffix.split(", ")
        //println(s"neededBags ${neededBags.mkString("Array(", ", ", ")")}")
        var foundBags:Map[Bag, Int] = Map()
        neededBags.foreach { b =>
          val tokens = b.split(" ")
          val maybeBag = findBag(tokens(1), tokens(2))
          //println(maybeBag)
          foundBags = maybeBag match {
            case Some(bag) => foundBags + (bag -> tokens(0).toInt)
            case None => foundBags
          }
        }
        if (foundBags.size == neededBags.size) {
          println(s"Found all of the needed bags for $adjective $color")
          bags = Bag(adjective, color, foundBags) :: bags
          removal = line :: removal
        }
      }
      //println (words.mkString("Array(", ", ", ")"))
    }

    removal
  }

  def findBag(adj:String, color:String):Option[Bag] = {
    bags.find(b => b.adjective == adj && b.color == color)
  }

  case class Bag(adjective:String, color:String, bags:Map[Bag, Int]) {

    def canContainBag(adj:String, c:String):Boolean = {
      var found = false
      bags.foreach { entry =>
        if (entry._1.adjective == adj && entry._1.color == c) {
          return true
        } else {
          found = entry._1.canContainBag(adj, c)
          if (found) return true
        }
      }
      found
    }

    def countChildren():Int = {
      var c = 0
      bags.foreach { bag =>
        c = c + bag._2
        c = c + bag._2 * bag._1.countChildren()
      }
      c
    }

    override def toString: String = s"Bag($adjective $color)"
  }

}