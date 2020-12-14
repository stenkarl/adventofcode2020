package day12

import scala.io.Source

object Day12 {

  def main(args: Array[String]): Unit = {
    println("Day 12")

    val input = Source.fromFile("src/day12/input.txt").getLines.toList

    val instructions = input.map { str =>
      val amt = str.substring(1).toInt
      str.charAt(0) match {
        case 'F' => Forward(amt)
        case 'L' => Left(amt)
        case 'R' => Right(amt)
        case 'N' => North(amt)
        case 'S' => South(amt)
        case 'W' => West(amt)
        case 'E' => East(amt)
      }
    }

    println(part1(instructions))

  }

  def part1(instructions:List[Instruction]):Int = {
    val initialState = State(East(0), 0, 0)

    val state = instructions.foldLeft(initialState) { (cur, initial) =>
      val next = initial.move(cur)

      println(s"$cur apply $initial => $next")

      next
    }

    Math.abs(state.x) + Math.abs(state.y)
  }

}

case class State(facing:Instruction, x:Int, y:Int)

sealed trait Instruction {
  def move(state:State): State

}

case class Forward(amt:Int) extends Instruction {

  def move(state:State): State = {
    //println(s"Forward move ${state}")
    val dir = state.facing match {
      case North(_) => North(amt)
      case South(_) => South(amt)
      case East(_) => East(amt)
      case West(_) => West(amt)
    }
    dir.move(state)
  }
}

case class North(amt:Int) extends Instruction {

  def move(state:State): State = {
    State(state.facing, state.x, state.y + amt)
  }
}

case class South(amt:Int) extends Instruction {
  def move(state:State): State = {
    State(state.facing, state.x, state.y - amt)
  }
}

case class East(amt:Int) extends Instruction {
  def move(state:State): State = {
    //println(s"East, moving to ${state.x + amt}")
    State(state.facing, state.x + amt, state.y)
  }
}

case class West(amt:Int) extends Instruction {
  def move(state:State): State = {
    State(state.facing, state.x - amt, state.y)
  }
}

case class Left(deg:Int) extends Instruction {

  val dirs = Vector(North(0), West(0), South(0), East(0))

  def move(state:State): State = {
    val numTurns = deg / 90
    val newDir = state.facing match {
      case North(_) => nextDir(0, numTurns)
      case West(_) => nextDir(1, numTurns)
      case South(_) => nextDir(2, numTurns)
      case East(_) => nextDir(3, numTurns)
    }
    State(newDir, state.x, state.y)
  }

  def nextDir(from:Int, turns:Int):Instruction =
    if (from + turns >= dirs.size) {
      val newTurns = (from + turns) - dirs.size
      dirs(newTurns)
    } else {
      dirs(from + turns)
    }

}

case class Right(deg:Int) extends Instruction {

  val dirs = Vector(North(0), East(0), South(0), West(0))

  def move(state:State): State = {
    val numTurns = deg / 90
    //println(s"numTurns $numTurns $deg")
    val newDir = state.facing match {
      case North(_) => nextDir(0, numTurns)
      case East(_) => nextDir(1, numTurns)
      case South(_) => nextDir(2, numTurns)
      case West(_) => nextDir(3, numTurns)

    }
    //println(s"New dir $newDir")
    State(newDir, state.x, state.y)
  }

  def nextDir(from:Int, turns:Int):Instruction = {
    //println(s"nextDir $from $turns")
    if (from + turns >= dirs.size) {
      val newTurns = (from + turns) - dirs.size
      dirs(newTurns)
    } else {
      dirs(from + turns)
    }
  }
}
