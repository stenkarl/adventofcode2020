package day21

import scala.collection.mutable
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

    println(s"foodAndAllergens $foodAndAllergens")

    val allAllergens = foodAndAllergens.map(_._2).reduce((p1, p2) => p1 union p2)

    println(s"allAllergens $allAllergens")

    val filtered = allAllergens.map { it =>
      val ret = foodAndAllergens.filter(p => p._2.contains(it)).map(_._1).reduce((f, s) => f intersect s)
      (it, ret)
    }

    val allFoodsWithAllergens = filtered.map(_._2).reduce(_ union _)

    val allFoods = foodAndAllergens.map(_._1).reduce(_ union _)
    val foodsNoAllergens = allFoods diff allFoodsWithAllergens

    val onlyFoodsWithAllergen = foodAndAllergens.map { it =>
      (it._1 diff foodsNoAllergens, it._2)
    }

    val sum = foodsNoAllergens.toList.map { it =>
      val s = foodAndAllergens.count(_._1.contains(it))
      //println(s"$it appears $s")
      s
    }.sum

    println(part2(onlyFoodsWithAllergen, allAllergens))
    //println(sum)
  }

  def part2(foods:List[(Set[String], Set[String])], allergens:Set[String]):String = {
    println(s"onlyFoodsWithAllergen $foods")
    var f = allergens.map { it =>
      val filtered = foods.filter(p => p._2.contains(it))
      val foodOnly = filtered.map(_._1)
      println(s"$it filtered $filtered foodOnly $foodOnly")
      (it, foodOnly.reduce(_ intersect  _))
    }
    val map = mutable.Map[String, String]()
    var filteredSet = f
    while (filteredSet.nonEmpty) {
      val values = map.values.toSet
      filteredSet = filteredSet.map(it => (it._1, it._2 diff values))
      val justOne = filteredSet.filter(_._2.size == 1)
      println(s"filtered $filteredSet")
      println(s"justOne $justOne")
      justOne.foreach { it =>
        map(it._1) = it._2.head
      }
      filteredSet = filteredSet diff justOne
      println(s"after diff $filteredSet")
      println(filteredSet.size)
    }
    println(map)
    println(f)

    val ret = map.keys.toList.sorted.map(map(_)).mkString(",")

    ret
  }

}
