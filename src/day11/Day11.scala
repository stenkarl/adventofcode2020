package day11

import scala.io.Source

object Day11 {

  var threshold = 0
  var adjacent:(IndexedSeq[IndexedSeq[Square]], Int, Int) => Int = adjacentPart1

  def main(args: Array[String]): Unit = {
    println("Day 11")

    val input = Source.fromFile("src/day11/input.txt").getLines

    val list:Vector[Vector[Square]] = input.map(_.map {
      case '.' => Floor()
      case '#' => Occupied()
      case 'L' => Empty()
    }.toVector).toVector

    println(list)

    partTwo(list)
  }

  def partOne(list:IndexedSeq[IndexedSeq[Square]]):Unit = {
    threshold = 4
    adjacent = adjacentPart1
    println(s"Occupied seats ${sumOccupied(list)}")
  }

  def partTwo(list:IndexedSeq[IndexedSeq[Square]]):Unit = {
    threshold = 5
    adjacent = adjacentPart2
    println(s"Occupied seats ${sumOccupied(list)}")
  }

  def sumOccupied(list:IndexedSeq[IndexedSeq[Square]]):Int = {
    var last = list
    while (last != nextState(last)) {
      last = nextState(last)
      println(last)
    }
    val sum = last.map(it => it.count(_ == Occupied())).sum

    sum
  }

  def adjacentPart1(list:IndexedSeq[IndexedSeq[Square]], row:Int, col:Int):Int = {
    val numCol = list.head.size
    val numRow = list.size
    var num = 0
    // top left
    num = num + (if (row > 0 && col > 0 && list(row - 1)(col - 1) == Occupied()) 1 else 0)
    // top center
    num = num + (if (row > 0 && list(row - 1)(col) == Occupied()) 1 else 0)
    // top right
    num = num + (if (row > 0 && col < numCol - 1 && list(row - 1)(col + 1) == Occupied()) 1 else 0)
    // left
    num = num + (if (col > 0 && list(row)(col - 1) == Occupied()) 1 else 0)
    // right
    num = num + (if (col < numCol - 1 && list(row)(col + 1) == Occupied()) 1 else 0)
    // bottom left
    num = num + (if (row < numRow - 1 && col > 0 && list(row + 1)(col - 1) == Occupied()) 1 else 0)
    // bottom center
    num = num + (if (row < numRow - 1 && list(row + 1)(col) == Occupied()) 1 else 0)
    // bottom right
    num = num + (if (row < numRow - 1 && col < numCol - 1 && list(row + 1)(col + 1) == Occupied()) 1 else 0)

    num
  }

  def adjacentPart2(list:IndexedSeq[IndexedSeq[Square]], row:Int, col:Int):Int = {
    //println(s"adjacent $row $col")
    val numCol = list.head.size
    val numRow = list.size
    var num = 0
    // top left
    num = num + nextSeat(list, row, col, -1, -1)
    // top center
    num = num + nextSeat(list, row, col, -1, 0)
    // top right
    num = num + nextSeat(list, row, col, -1, 1)
    // left
    num = num + nextSeat(list, row, col, 0, -1)
    // right
    num = num + nextSeat(list, row, col, 0, 1)
    // bottom left
    num = num + nextSeat(list, row, col, 1, -1)
    // bottom center
    num = num + nextSeat(list, row, col, 1, 0)
    // bottom right
    num = num + nextSeat(list, row, col, 1, 1)

    num
  }

  def nextSeat(list:IndexedSeq[IndexedSeq[Square]], row:Int, col:Int, rowDir:Int, colDir:Int):Int = {
    //println(s"nextSeat $row $col $rowDir $colDir")
    val numCol = list.head.size
    val numRow = list.size
    var curRow = row
    var curCol = col
    var ret = -1
    while (ret == -1 && curRow > -1 && curRow < numRow && curCol > -1 && curCol < numCol) {
      //println(s"before checking curRow $curRow, curCol $curCol $ret ${list(curRow)(curCol)} ${(curRow != row && curCol != col)}")
      if (!(curRow == row && curCol == col)) {
        //println(s"checking curRow $curRow, curCol $curCol $ret ${list(curRow)(curCol)}")

        ret = if (list(curRow)(curCol) == Occupied()) {
          1
        } else if (list(curRow)(curCol) == Empty()) {
          0
        } else -1
      }
      curRow = curRow + rowDir
      curCol = curCol + colDir
      //println(s"row $row col $col curRow $curRow, curCol $curCol $ret")
    }
    //println(s"done nextSeat $row $col $rowDir $colDir $ret")

    if (ret == -1) ret = 0
    ret
  }

  def nextState(list:IndexedSeq[IndexedSeq[Square]]): IndexedSeq[IndexedSeq[Square]] = {
    val adj = for (row <- list.indices)
      yield List(for (col <- list(row).indices)
        yield adjacent(list, row, col) match {
          case x if (x == 0 && list(row)(col) == Empty()) => Occupied()
          case x if (x >= threshold && list(row)(col) == Occupied()) => Empty()
          case _ => list(row)(col)
        }).head

    adj
  }

}

sealed trait Square

case class Floor() extends Square {

  override def toString: String = "."
}

case class Empty() extends Square {

  override def toString: String = "L"
}

case class Occupied() extends Square {

  override def toString: String = "#"
}
