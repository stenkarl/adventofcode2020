package day6

import scala.io.Source

object Day6 {

  def main(args: Array[String]): Unit = {
    println("Day 6")

    val input = Source.fromFile("src/day6/input.txt").getLines.toList

    //println(createGroup(input, 0, Set()))
    val groups = createGroups(input, 0, List())
    println(groups)

    val sum = groups.foldRight(0)(_.size + _)

    println(s"Sum $sum")
  }

  def createGroups(input:List[String], index:Int, groups:List[Set[Char]]):List[Set[Char]] = {
    if (index >= input.size) return groups

    val ret:(Set[Char], Int) = createGroupPart2(input, index, Set())
    //println(s"createGroup $ret")
    createGroups(input, ret._2, ret._1 :: groups)
  }

  def createGroupPart1(input:List[String], index:Int, group:Set[Char]):(Set[Char], Int) = {
    if (index >= input.size || input(index) == "") return (group, index + 1)

    val ret = createGroupPart1(input, index + 1, input(index).toSet)
    (group ++ ret._1, ret._2)
  }

  def createGroupPart2(input:List[String], index:Int, group:Set[Char]):(Set[Char], Int) = {
    if (index >= input.size || input(index) == "") return (group, index + 1)

    val ret = createGroupPart2(input, index + 1, input(index).toSet)
    (if (group.isEmpty) ret._1 else group.intersect(ret._1), ret._2)
  }

}
