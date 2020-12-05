package day4

import scala.collection.mutable
import scala.io.Source

object Day4 {

  var numValid = 0

  def main(args: Array[String]): Unit = {
    println("Day 4")

    val input = Source.fromFile("src/day4/input.txt").getLines.toList

    println(input)

    part1(input)

  }

  def part1(input:List[String]): Unit = {
    var index = 0
    val map = mutable.Map[String, String]()
    while (index < input.size) {
      if (input(index) == "") {
        checkPassport(map)
        map.clear()
      } else {
        val tokens = input(index).split(" ")
        tokens.foreach( t => {
          val kv = t.split(":")
          map(kv(0)) = kv(1)
        })
      }
      index = index + 1
    }
    checkPassport(map)

    println(s"Valid: $numValid")
  }

  def checkPassport(map:mutable.Map[String, String]) = {
    val p = Passport(map.map(kv => (kv._1, kv._2)).toMap)
    //println(s"$p ${p.isValid}")
    if (p.isValid) numValid = numValid + 1
  }


  case class Passport(map:Map[String, String]) {

    def isValid:Boolean = {
      if (!checkRequired) return false
      if (!checkBirthYear) return false
      if (!checkIssueYear) return false
      if (!checkExpirationYear) return false
      if (!checkHeight) return false
      if (!checkHairColor) return false
      if (!checkEyeColor) return false
      if (!checkPassportId) return false

      true
    }

    def checkRequired:Boolean = map.contains("byr") && map.contains("iyr") &&
      map.contains("eyr") && map.contains("hgt") && map.contains("hcl") &&
      map.contains("ecl") && map.contains("pid")

    def checkBirthYear:Boolean = {
      val year = map("byr").toInt

      year >= 1920 && year <= 2002
    }

    def checkIssueYear:Boolean = {
      val year = map("iyr").toInt

      year >= 2010 && year <= 2020
    }

    def checkExpirationYear:Boolean = {
      val year = map("eyr").toInt

      year >= 2020 && year <= 2030
    }

    def checkHeight:Boolean = {
      val h = map("hgt")
      if (!h.endsWith("cm") && !h.endsWith("in")) return false
      val units = h.substring(h.size - 2)
      val valueStr = h.substring(0, h.size - 2)

      val value = valueStr.toInt

      if (units == "cm") return value >= 150 && value <= 193

      value >= 59 && value <= 76
    }

    def checkHairColor:Boolean = {
      val c = map("hcl")

      if (!c.startsWith("#") || c.size != 7) return false
      val sub = c.substring(1)

      sub.count(ch => Character.isDigit(ch) || ch == 'a' || ch == 'b' ||
            ch == 'c' || ch == 'd' || ch == 'e' || ch == 'f') == 6
    }

    def checkEyeColor:Boolean = {
      val c = map("ecl")

      c == "amb" || c == "blu" || c == "brn" || c == "gry" || c == "grn" ||
        c == "hzl" || c == "oth"
    }

    def checkPassportId:Boolean = {
      val id = map("pid")

      id.count(Character.isDigit) == 9
    }

    override def toString: String = map.toString
  }

}
