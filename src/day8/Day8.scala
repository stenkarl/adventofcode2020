package day8

import scala.io.Source

object Day8 {


  def main(args: Array[String]): Unit = {
    println("Day 8")
    val input = Source.fromFile("src/day8/input.txt").getLines.toList

    val ops = parse(input)

    println(part1(ops))

    part2(ops)
  }

  def parse(input:List[String]): List[Op] =
    input.map { line =>
      val split = line.split(" ")
      val opStr = split(0)
      val param = split(1).toInt

      opStr match {
        case "nop" => Nop(param)
        case "acc" => Acc(param)
        case "jmp" => Jmp(param)
      }
    }

  def part1(ops:List[Op]): (Int, Boolean) = {
    var acc = 0
    var pc = 0
    var executed:List[Int] = Nil
    while (!executed.contains(pc)) {
      executed = pc :: executed
      val next = ops(pc) match {
        case Nop(_) => 1
        case Acc(amt) => acc = acc + amt; 1
        case Jmp(to) => to
      }
      pc = pc + next
      if (pc >= ops.size) return (acc, true)
    }
    (acc, false)
  }

  def part2(ops:List[Op]) = {
    var index = 0
    while (index < ops.size) {
      val newOps = ops(index) match {
        case Nop(notused) => replace(ops, index, Jmp(notused))
        case Acc(amt) => ops
        case Jmp(to) => replace(ops, index, Nop(to))
      }
      val ret = part1(newOps)
      if (ret._2) {
        println(s"Found $ret")
      }
      index = index + 1
    }
  }

  def replace(ops:List[Op], index:Int, op:Op): List[Op] = {
    ops.updated(index, op)
  }

}

sealed trait Op

case class Nop(notused:Int) extends Op

case class Acc(amt:Int) extends Op

case class Jmp(to:Int) extends Op
