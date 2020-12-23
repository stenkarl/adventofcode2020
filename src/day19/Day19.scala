package day19

import scala.collection.mutable
import scala.io.Source

object Day19 {

  def main(args: Array[String]): Unit = {
    println("Day 19")

    val input = Source.fromFile("src/day19/sample2.txt").getLines.toList

    val splitIndex = input.zipWithIndex.filter(p => p._1 == "").head._2

    def rule0 = createRules(input.slice(0, splitIndex))
    def strings = input.slice(splitIndex + 1, input.length)

    println(s"strings $strings")

    println(part1(rule0, strings))
  }

  def createRules(lines:List[String]):Rule = {
    println(s"createRules $lines")
    val split = lines.map(_.split(" "))
    val map = mutable.Map[Int, Rule]()
    while (map.size != split.size) {
      split.foreach { it =>
        val ruleNum = it(0).substring(0, it(0).length - 1).toInt
        if (it(1).startsWith("\"")) {
          map(ruleNum) = Value(it(1).charAt(1))
        } else {
          val ruleNumsOnly:List[Int] = it.slice(1, it.length).filter(_ != "|").map(_.toInt).toList
          val allFound = ruleNumsOnly.count(map.contains) == ruleNumsOnly.length
          if (allFound) {
            if (it.contains("|")) {
              val pipeIndex = it.indexOf("|")
              val left = it.slice(1, pipeIndex).map(_.toInt).toList
              val right = it.slice(pipeIndex + 1, it.length).map(_.toInt).toList
              val leftRule = CompositeRule(left.map(map(_)))
              val rightRule = CompositeRule(right.map(map(_)))
              val compound = CompoundRule(leftRule, rightRule)
              map(ruleNum) = compound
            } else {
              val compositeRule = CompositeRule(ruleNumsOnly.map(map(_)))
              map(ruleNum) = compositeRule
            }
          } else {
            //println(s"not all required rules found $ruleNumsOnly")
          }
        }
      }
    }
    //println(map)
    map(0)
  }

  def part1(rule:Rule, strings:List[String]):Int = {
    strings.count(s => rule.verify(s, 0) == s.length)
  }

  def testRules():Unit = {
    val four = Value('a')
    val five = Value('b')
    val three = CompoundRule(CompositeRule(List(four, five)), CompositeRule(List(five, four)))
    val two = CompoundRule(CompositeRule(List(four, four)), CompositeRule(List(five, five)))
    val one = CompoundRule(CompositeRule(List(two, three)), CompositeRule(List(three, two)))
    val zero = CompositeRule(List(four, one, five))

    val str = "aaaabbb"
    val ret = zero.verify(str, 0)
    println(ret == str.length)
  }


}

sealed trait Rule {
  def verify(string:String, index:Int):Int
}

case class Value(ch:Char) extends Rule {
  override def verify(string:String, index:Int):Int = {
    if (index >= string.length) return -1
    //println(s"Checking ${string.charAt(index)} at index $index for $ch")
    if (string.charAt(index) == ch) index + 1 else -1
  }
}

case class CompositeRule(rules:List[Rule]) extends Rule {
  override def verify(string: String, index: Int): Int = {
    var nextIndex = index
    rules.foreach { it =>
      nextIndex = it.verify(string, nextIndex)
      if (nextIndex == -1) return nextIndex // no match
    }
    nextIndex
  }
}

case class CompoundRule(left:Rule, right:Rule) extends Rule {
  override def verify(string: String, index: Int): Int = {
    var nextIndex = left.verify(string, index)
    if (nextIndex == -1) {
      //println("Left side didn't work out, checking right")
      nextIndex = right.verify(string, index)
    }
    nextIndex
  }
}