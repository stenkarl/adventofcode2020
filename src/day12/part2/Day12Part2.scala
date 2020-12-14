package day12.part2

import scala.io.Source

object Day12Part2 {

  def main(args: Array[String]): Unit = {
    println("Day 12 Part 2")

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

    println(part2(instructions))
  }

  def part2(instructions:List[Instruction]):Int = {
    val initialState = State(Position(0, 0), Position(10, 1))

    val state = instructions.foldLeft(initialState) { (cur, initial) =>
      val next = initial.move(cur)

      println(s"$cur apply $initial => $next")

      next
    }

    Math.abs(state.ship.x) + Math.abs(state.ship.y)
  }

}

case class Position(x:Int, y:Int)

case class State(ship:Position, waypoint:Position)

sealed trait Instruction {
  def move(state:State): State
}

case class Forward(amt:Int) extends Instruction {

  def move(state:State): State = {
    val moveBy = Position(state.waypoint.x * amt, state.waypoint.y * amt)
    println(s"Forward move ${state} moveby $moveBy")
    State(Position(state.ship.x + moveBy.x,
      state.ship.y + moveBy.y), state.waypoint)
  }
}

case class North(amt:Int) extends Instruction {

  def move(state:State): State = {
    State(state.ship, Position(state.waypoint.x, state.waypoint.y + amt))
  }
}

case class South(amt:Int) extends Instruction {
  def move(state:State): State = {
    State(state.ship, Position(state.waypoint.x, state.waypoint.y - amt))
  }
}

case class East(amt:Int) extends Instruction {
  def move(state:State): State = {
    //println(s"East, moving to ${state.x + amt}")
    State(state.ship, Position(state.waypoint.x + amt, state.waypoint.y))
  }
}

case class West(amt:Int) extends Instruction {
  def move(state:State): State = {
    State(state.ship, Position(state.waypoint.x - amt, state.waypoint.y))
  }
}

case class Left(deg:Int) extends Instruction {

  def move(state:State): State = {
    val numTurns = deg / 90
    var newPos = state.waypoint
    for (x <- 1 to numTurns) {
      newPos = turnLeft(newPos)
    }
    State(state.ship, newPos)
  }

  def turnLeft(position:Position):Position = {
    Position(-position.y, position.x)
  }

}

case class Right(deg:Int) extends Instruction {

  def move(state:State): State = {
    val numTurns = deg / 90
    var newPos = state.waypoint
    for (x <- 1 to numTurns) {
      newPos = turnRight(newPos)
    }
    State(state.ship, newPos)
  }

  def turnRight(position:Position):Position = {
    Position(position.y, -position.x)
  }

}
