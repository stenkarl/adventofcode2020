package day21

import scala.io.Source

object Day21 {

  def main(args: Array[String]): Unit = {
    println("Day 21")

    val input = Source.fromFile("src/day21/input.txt").getLines.toList

    val foodAndAllergens = input.map{ it =>
      val split = it.split(" \\(contains ")
      val food = split(0).split(" ").toSet
      val allergens = split(1).substring(0, split(1).length -1).split(", ").toSet

      (food, allergens)
    }

    println(foodAndAllergens)

    val allAllergens = foodAndAllergens.map(_._2).reduce((p1, p2) => p1 union p2)

    println(allAllergens)

    val filtered = allAllergens.map { it =>
      val ret = foodAndAllergens.filter(p => p._2.contains(it)).map(_._1).reduce((f, s) => f intersect s)
      (it, ret)
    }

    val allFoodsWithAllergens = filtered.map(_._2).reduce(_ union _)

    val allFoods = foodAndAllergens.map(_._1).reduce(_ union _)
    val foodsNoAllergens = allFoods diff allFoodsWithAllergens
    println(allFoods)

    val sum = foodsNoAllergens.toList.map { it =>
      val s = foodAndAllergens.count(_._1.contains(it))
      println(s"$it appears $s")
      s
    }.sum


    println(sum)
  }

}
