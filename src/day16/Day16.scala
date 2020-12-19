package day16

import scala.collection.mutable
import scala.io.Source

object Day16 {

  def main(args: Array[String]): Unit = {
    println("Day 16")

    val input = Source.fromFile("src/day16/input.txt").getLines.toList

    println(input)

    val fieldBreak = input.zipWithIndex.find(_._1 == "").get._2
    val fields = input.slice(0, fieldBreak).map { s =>
      val tokens = s.split(" ")
      val ranges = tokens.filter(_.contains("-")).map { str =>
        val r = str.split("-")
        r(0).toInt to r(1).toInt
      }.toList

      Field(tokens(0).substring(0, tokens(0).length - 1), ranges)
    }

    println(fields)

    val yourTicket = input(fieldBreak + 2).split(",").map(_.toInt).toList

    println(s"your ticket $yourTicket")

    val otherTickets = input.slice(fieldBreak + 5, input.size).map (
      _.split(",").map(_.toInt).toList
    )

    println(s"other tickets $otherTickets")

    //println(part1(fields, otherTickets))

    val validTickets = filterInvalid(fields, otherTickets)

    println(s"all size ${otherTickets.size} valid ${validTickets.size}")
    println(part2(fields, validTickets, yourTicket))
  }

  def part1(fields:List[Field], tickets:List[List[Int]]):Int = {
    var sum = 0
    var anyMatch = false
    val ranges = fields.flatMap { _.ranges }
    println(ranges)
    tickets.foreach { list =>
      list.foreach { num =>
        ranges.foreach { r:Range =>
          if (r.contains(num)) {
            anyMatch = true
          }

        }
        if (!anyMatch) {
          sum = sum + num
        }
        anyMatch = false
      }

    }
    sum
  }

  def filterInvalid(fields:List[Field], tickets:List[List[Int]]):List[List[Int]] = {
    var anyMatch = false
    val ranges = fields.flatMap { _.ranges }
    var invalid:List[List[Int]] = Nil
    tickets.foreach { list =>
      list.foreach { num =>
        ranges.foreach { r:Range =>
          if (r.contains(num)) {
            anyMatch = true
          }

        }
        if (!anyMatch) {
          // remove this one
          invalid = list :: invalid
        }
        anyMatch = false
      }

    }
    println(s"These tickets are invalid ${invalid.size}")

    tickets diff invalid
  }

  def part2(fields:List[Field], tickets:List[List[Int]], yourTicket:List[Int]):Int = {
    val possibleFieldList = for (idx <- tickets.head.indices) yield
      possibleFields(fieldValuesAt(idx, tickets), fields)

    val possible = possibleFieldList.zipWithIndex.toList
    //possible.foreach (it =>
    //          println(s"index ${it._2} has ${it._1.size} options ${it._1}")
    //)

    val list = reduce(possible)

    println(s"departure indices $list")

    val departureIndices = list.filter(p => p._1.name.startsWith("departure")).map(_._2)

    println(departureIndices)

    println(departureIndices.foldRight(BigInt(1))(yourTicket(_) * _))
    0
  }

  def reduce (fieldsAndIndices:List[(List[Field], Int)]):List[(Field, Int)] = {
    if (fieldsAndIndices.isEmpty) return Nil

    //fieldsAndIndices.foreach { it =>
    //  println(s"Index ${it._2} has ${it._1.size} options")
    //}

    val one = fieldsAndIndices.filter(p => p._1.size == 1).head

    val reduced = (for (cur <- fieldsAndIndices) yield (cur._1 diff one._1, cur._2)).filter(_._1.nonEmpty)

    //var idx = -1
    //if (one._1.head.name.startsWith("departure")) {
    //  println(s"index ${one._2} must be ${one._1}")
    //}
    //println(s"reduced size ${reduced.size} ${reduced}")

    (one._1.head, one._2) :: reduce(reduced)
  }


  def possibleFields(values:List[Int], fields:List[Field]):List[Field] = {
    //println(s"possibleFields $values")

    val possible = values.map { v =>
      val f = fields.filter(f => f.ranges.count(_.contains(v)) > 0)

      //println(s"$v is contained in ${f.size} fields $f")
      f
    }

    val reduced = possible.reduce(_.intersect(_))

    //println(s"possible size ${possible.size}")
    //println(s"reduced $reduced")

    reduced
  }

  def fieldValuesAt(index:Int, tickets:List[List[Int]]):List[Int] = {
    //println(s"fieldValuesAt $index ${tickets.size}")
    tickets.map(_(index))
  }

}

case class Field(name:String, ranges:List[Range])
