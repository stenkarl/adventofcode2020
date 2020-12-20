package Day18

import scala.collection.mutable
import scala.io.Source

object Day18Part1 {

  def main(args: Array[String]): Unit = {
    println("Day 18 Part 1")

    val input = Source.fromFile("src/day18/input.txt").getLines.toList


    println(part1(input))
    //println(eval(infixToRpn(input.head.split(" ").reverse.toList)))
  }

  def part1(input:List[String]):BigInt = {
    //input.foldLeft(0)(eval(infixToRpn(formatString(_).reverse))) + _)
    var sum = BigInt(0)

    input.foreach { it =>
      val formatted = formatString(it)
      println(s"formatted $formatted")
      val expr = infixToRpn(formatted)
      val s = eval(expr)

      if (s < 0) println(s"ERROR $s")

      sum = sum + s
    }
    sum
  }

  def formatString(str:String):List[String] = {
    val buffer = mutable.StringBuilder.newBuilder

    str.iterator.foreach {
      case '(' => buffer.append("("); buffer.append(" ")
      case ')' => buffer.append(" "); buffer.append(")")
      case it => buffer.append(it + "")
    }

    println(s"formatString $buffer")
    buffer.toString.split(" ").reverse.toList
  }

  def infixToRpn(tokens:List[String]):List[String] = {
    println(s"infixToRpn $tokens")
    val operators = mutable.Stack[String]()
    val output = mutable.ListBuffer[String]()

    tokens.foreach {
      case it@"+" => operators.push(it)
      case it@"*" => operators.push(it)
      case it@")" => operators.push(it)
      case "(" =>
        var curOp = operators.pop()
        while (curOp != ")") {
          output.append(curOp)
          curOp = operators.pop()
        }
      case it => output.append(it)
    }
    while (operators.nonEmpty) output.append(operators.pop())

    output.toList
  }

  def eval(expr:List[String]):BigInt = {
    println(s"eval $expr")
    val values = mutable.Stack[BigInt]()

    expr.foreach {
      case "+" => values.push(values.pop() + values.pop())
      case "*" => values.push(values.pop() * values.pop())
      case it => values.push(BigInt(it))
    }

    println(values)
    values.pop()
  }

}
