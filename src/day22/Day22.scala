package day22

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day22 {

  def main(args: Array[String]): Unit = {
    println("Day 22")

    val input = Source.fromFile("src/day22/input.txt").getLines.toList
    val inputWithIndices = input.zipWithIndex
    val blankIndex = inputWithIndices.filter(p => p._1 == "").head._2.toInt
    val p1 = input.slice(1, blankIndex).map(_.toInt)
    val p2 = input.slice(blankIndex + 2, input.size).map(_.toInt)

    println(part2(p1.to[ListBuffer], p2.to[ListBuffer]))
  }

  def part1(p1:ListBuffer[Int], p2:ListBuffer[Int]):Int = {
    println(p1)
    println(p2)

    var round = 1
    while (p1.nonEmpty && p2.nonEmpty) {
      val card1 = p1.remove(0)
      val card2 = p2.remove(0)
      if (card1 > card2) {
        println(s"Player 1 wins $card1 and $card2")
        p1.append(card1)
        p1.append(card2)
      } else {
        println(s"Player 2 wins $card1 and $card2")
        p2.append(card2)
        p2.append(card1)
      }
      println(s"End of round $round p1:$p1 and p2:$p2")
      round = round + 1
    }
    val winner = if (p1.nonEmpty) p1 else p2

    val ret = winner.reverse.zipWithIndex.map(it => it._1 * (it._2 + 1))

    println(ret)

    ret.sum
  }

  def part2(p1:ListBuffer[Int], p2:ListBuffer[Int]):Int = {
    val p1Winner = subgame(p1, p2, 1)

    val winner = if (p1Winner) p1 else p2

    println(s"Winner's deck $winner")

    val ret = winner.reverse.zipWithIndex.map(it => it._1 * (it._2 + 1))

    println(ret)

    ret.sum
  }

  def subgame(p1:ListBuffer[Int], p2:ListBuffer[Int], num:Int):Boolean = {
    //println()
    //println(s"=== Game $num with $p1 $p2 ===")
    val set = mutable.Set[(String, String)]()

    var round = 1
    while (p1.nonEmpty && p2.nonEmpty) {
      val newConfig = (p1.mkString(","), p2.mkString(","))
      if (set.contains(newConfig)) {
        println(s"seen this config before: $newConfig")
        return true
      }
      set.add(newConfig)
      val card1 = p1.remove(0)
      val card2 = p2.remove(0)
      if (card1 <= p1.size && card2 <= p2.size) {
        //println(s"spawning new game $card1 ${p1.size}, $card2 ${p2.size}")
        val newp1 = p1.slice(0, card1)
        val newp2 = p2.slice(0, card2)
        val p1Winner = subgame(newp1, newp2, num + 1)
        //println(s"meanwhile back to game $num")
        if (p1Winner) {
          p1.append(card1)
          p1.append(card2)
        } else {
          p2.append(card2)
          p2.append(card1)
        }
      } else if (card1 > card2) {
        //println(s"Player 1 wins $card1 and $card2")
        p1.append(card1)
        p1.append(card2)
      } else {
        //println(s"Player 2 wins $card1 and $card2")
        p2.append(card2)
        p2.append(card1)
      }
      //println(s"End of round $round p1:$p1 and p2:$p2")
      round = round + 1
    }
    val winner = p1.nonEmpty
    //println(s"Player ${if (winner) 1 else 2} wins game $num")
    //println(s"=== Game $num ends ===")
    //println()
    winner
  }

}
