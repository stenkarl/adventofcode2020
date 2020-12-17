package day14

import scala.collection.mutable
import scala.io.Source

object Day14 {

  val memory:mutable.Map[BigInt, BigInt] = mutable.Map()

  def main(args: Array[String]): Unit = {
    println("Day 14")

    val input = Source.fromFile("src/day14/input.txt").getLines.toList

    println(input)

    //println(part1(input))

    println(part2(input))
  }

  def part2(input:List[String]):BigInt = {

    var mask = ""
    input.foreach {line =>
      val tokens = line.split(" = ")

      if (tokens(0).equals("mask")) {
        mask = tokens(1)
      } else if (tokens(0).startsWith("mem")) {
        val address = BigInt(tokens(0).substring("mem[".length, tokens(0).length - 1))
        val value = BigInt(tokens(1))
        setMemWithMask(address, value, mask)
      } else {
        println(s"Unknown token ${tokens(0)}")
      }
    }

    sumValues()
  }

  def maskedAddress(address:String, mask:String):String = {
    val builder = new StringBuilder(address)
    mask.zipWithIndex.foreach(p => builder.setCharAt(p._2, p._1))

    builder.toString()
  }

  def setMemWithMask(address: BigInt, value: BigInt, mask: String) = {
    //val maskIndices = List(3, 4, 5, 6, 8, 10)
    val maskIndices = mask.reverse.zipWithIndex.filter(p => p._1 == 'X').map(_._2)
    val oneZero = List(0, 1)

    val permutations = prod(oneZero, maskIndices.size).map(maskIndices.zip(_))

    //println(s"permutation size ${permutations.size}")

    permutations.foreach(it => println(s"perm $it"))
    val addresses = permutations.map(generateAddress(address, mask, _))

    addresses.foreach (memory(_) = value)
  }

  def generateAddress(address:BigInt, mask:String, pairs:IndexedSeq[(Int, Int)]):BigInt = {
    var maskedAddress = address
    mask.reverse.zipWithIndex.foreach { p =>
      if (p._1 == '1') {
         maskedAddress = setBit(maskedAddress, p._2)
      }
    }
    pairs.foreach { p =>
      if (p._2 == 0) {
        maskedAddress = clearBit(maskedAddress, p._1)
      } else if (p._2 == 1) {
        maskedAddress = setBit(maskedAddress, p._1)
      }
    }
    //println(s"generateAddress $address $mask ${maskedAddress}")
    //println(s"address = ${maskedAddress.toInt.toBinaryString}")
    maskedAddress
  }

  def prod[T](lst: List[T], n: Int) =
    List.fill(n)(lst).flatten.combinations(n).flatMap(_.permutations).toList

  def part1(input:List[String]):BigInt = {
    var mask = ""
    input.foreach {line =>
      val tokens = line.split(" = ")

      if (tokens(0).equals("mask")) {
        mask = tokens(1)
      } else if (tokens(0).startsWith("mem")) {
        val address = BigInt(tokens(0).substring("mem[".length, tokens(0).length - 1))
        val value = BigInt(tokens(1))
        setWithMask(address, value, mask)
      } else {
        println(s"Unknown token ${tokens(0)}")
      }
    }

    sumValues()
  }

  def setWithMask(address:BigInt, value:BigInt, mask:String):Unit = {
    val newValue = applyMask(mask, value)
    println(s"setWithMask(mem[$address] was $value, apply $mask is now $newValue)")

    memory(address) = newValue
  }

  def applyMask(mask:String, value:BigInt):BigInt = {
    var newValue = value
    mask.reverse.zipWithIndex.foreach ( p =>
      newValue = if (p._1 == '1') setBit(newValue, p._2)
      else if (p._1 == '0') clearBit(newValue, p._2)
      else newValue
    )
    newValue
  }

  def setBit(value:BigInt, position:Int):BigInt = {
    value | (BigInt("1") << position)
  }

  def clearBit(value:BigInt, position:Int):BigInt = {
    value & ~(BigInt(1) << position)
  }

  def set(address:BigInt, value:BigInt) = {
    memory(address) = value
  }

  def sumValues():BigInt = {
    memory.values.sum
  }

}
