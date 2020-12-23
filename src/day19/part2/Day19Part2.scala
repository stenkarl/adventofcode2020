package day19.part2

import scala.collection.mutable
import scala.io.Source

object Day19Part2 {

  def main(args: Array[String]): Unit = {
    println("Day 19")

    val input = Source.fromFile("src/day19/sample2a.txt").getLines.toList

    val splitIndex = input.zipWithIndex.filter(p => p._1 == "").head._2

    def rules = createRules(input.slice(0, splitIndex))
    def strings = input.slice(splitIndex + 1, input.length)

    println(s"rules ${rules.keys}.toList")
    println(s"strings $strings")

    val fortyTwo = rules(42)
    val eight = RecursiveRuleNoRight(fortyTwo, fortyTwo, 8)
    val thirtyOne = rules(31)

    val eleven = RecursiveRule(CompositeRule(List(fortyTwo, thirtyOne), 11), fortyTwo, thirtyOne, 11)

    val zero = CompositeRule(List(eight, eleven), 0)
    println(zero)

    println(part1(zero, strings))
  }

  def createRules(lines:List[String]):mutable.Map[Int, Rule] = {
    println(s"createRules $lines")
    val split = lines.map(_.split(" ")).filter { it =>
      val first = it(0)
      !first.startsWith("0:") && !first.startsWith("8:") && !first.startsWith("11:")
    }
    val map = mutable.Map[Int, Rule]()
    while (map.size != split.size) {
      split.foreach { it =>
        val ruleNum = it(0).substring(0, it(0).length - 1).toInt
        if (it(1).startsWith("\"")) {
          map(ruleNum) = Value(it(1).charAt(1), ruleNum)
        } else {
          val ruleNumsOnly:List[Int] = it.slice(1, it.length).filter(_ != "|").map(_.toInt).toList
          val allFound = ruleNumsOnly.count(map.contains) == ruleNumsOnly.length
          if (allFound) {
            if (it.contains("|")) {
              val pipeIndex = it.indexOf("|")
              val left = it.slice(1, pipeIndex).map(_.toInt).toList
              val right = it.slice(pipeIndex + 1, it.length).map(_.toInt).toList
              val leftRule = CompositeRule(left.map(map(_)), ruleNum)
              val rightRule = CompositeRule(right.map(map(_)), ruleNum)
              val compound = CompoundRule(leftRule, rightRule, ruleNum)
              map(ruleNum) = compound
            } else {
              val compositeRule = CompositeRule(ruleNumsOnly.map(map(_)), ruleNum)
              map(ruleNum) = compositeRule
            }
          } else {
            //println(s"not all required rules found $ruleNumsOnly")
          }
        }
      }
    }
    println(map)
    map
  }

  def part1(rule:Rule, strings:List[String]):Int = {

    val list = strings.filter(s => rule.verify(s, 0) == s.length)
    println(list)
    list.size
  }

}

sealed trait Rule {
  def verify(string:String, index:Int):Int
}

case class Value(ch:Char, num:Int) extends Rule {
  override def verify(string:String, index:Int):Int = {
    if (index >= string.length) {
      println(s"Rule $num Checking $index but at the end of string.")
      return -1
    }
    println(s"Rule $num Checking ${string.charAt(index)} at index $index for $ch")
    if (string.charAt(index) == ch) index + 1 else -1
  }
}

case class CompositeRule(rules:List[Rule], num:Int) extends Rule {
  override def verify(string: String, index: Int): Int = {
    var nextIndex = index
    println(s"CompositeRule $num checking $index")
    rules.foreach { it =>
      nextIndex = it.verify(string, nextIndex)
      if (nextIndex == -1) {
        println(s"Rule $num didn't match $index")
        return nextIndex
      } // no match
    }
    nextIndex
  }
}

case class CompoundRule(left:Rule, right:Rule, num:Int) extends Rule {
  override def verify(string: String, index: Int): Int = {
    println(s"CompoundRule $num checking $index")
    var nextIndex = left.verify(string, index)
    if (nextIndex == -1) {
      println(s"Left side of rule $num didn't match $index, checking right")
      nextIndex = right.verify(string, index)
      if (nextIndex == -1) {
        println(s"Right side of rule $num didn't match $index")
      }
    } else {
      println(s"CompoundRule $num left side matches $nextIndex")
    }
    if (nextIndex == -1) {
      println(s"CompoundRule $num didn't match left/right at $index")
    }
    nextIndex
  }
}

case class RecursiveRule(left:Rule, rightLeft:Rule, rightRight:Rule = NoOpRule(), rule:Int) extends Rule {

  val right = CompositeRule(List(rightLeft, this, rightRight), rule)

  override def verify(string: String, index: Int): Int = {
    println(s"RecursiveRule $rule - checking left side at $index")

    var nextIndex = left.verify(string, index)
    if (nextIndex == -1) {
      println(s"Recursive rule $rule Left side didn't work out, checking right at $index")
      nextIndex = right.verify(string, index)
      if (nextIndex == -1) {
        println(s"Recursive rule $rule Right side didn't match at $index")
      }
    } else {
      println(s"Recursive rule $rule left side matches $nextIndex")
    }
    if (nextIndex == -1) {
      println(s"Recursive rule $rule didn't match")
    }
    nextIndex
  }

}

case class RecursiveRuleNoRight(left:Rule, rightLeft:Rule, rule:Int) extends Rule {

  val right = CompositeRule(List(rightLeft, this), rule)

  override def verify(string: String, index: Int): Int = {
    println(s"RecursiveNoRight $rule - checking $index")

    var nextIndex = left.verify(string, index)
    if (nextIndex == -1) {
      println("RecursiveNoRight - Left side didn't work out, checking right")
      nextIndex = right.verify(string, index)
      if (nextIndex == -1) {
        println("Right side didn't match")
      }
    } else {
      println(s"RecursiveNoRight $rule left side matches $nextIndex")
    }
    if (nextIndex == -1) {
      println(s"RecursiveNoRight $rule didn't match")
    }

    nextIndex
  }

}

case class NoOpRule() extends Rule {
  override def verify(string: String, index: Int): Int = index
}