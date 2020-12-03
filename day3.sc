import scala.annotation.tailrec

def readFile(filename: String): List[String] = {
    val bufferedSource = scala.io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines.toList
}

trait Coord
case class Position(x: Int, y: Int, tree: Int) extends Coord
case class Movement(x: Int, y: Int) extends Coord

def move(a: Position, b: Movement): Position = {
  Position(a.x + b.x, a.y + b.y, a.tree)
}

def assess_new_position(line: String, pos: Position): Position = {
  val tree: Boolean = line((pos.x % line.length)) == '#'
  // print(pos)
  // println(line * 3)
  tree match {
    case true => Position(pos.x, pos.y, pos.tree + 1)
    case false => pos
  }
}

@tailrec
def assess_course(course: List[String], pos: Position, mov: Movement): Position = course match {
  case xs :: Nil => assess_new_position(xs, move(pos, mov))
  case x :: xs => assess_course(xs, assess_new_position(x, move(pos, mov)), mov)
}

val ex: List[String]  = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
""".split("\n").toList

val real_data = readFile("/home/leo/repos/aoc2020/data/day3.csv")

val m = Movement(3, 1)
val part1: Int  = assess_course(real_data, Position(-m.x, -m.y, 0), m).tree

println(s"Part 1: ${part1}")

// part 2

val movements = List(
  Movement(1, 1),
  Movement(3, 1),
  Movement(5, 1),
  Movement(7, 1),
  Movement(1, 2))

@tailrec
def assess_course2(course: List[String], pos: Position, mov: Movement): Position = course match {
  case xs :: Nil => assess_new_position(xs, move(pos, mov))
  case x :: xs => assess_course2(xs.tail, assess_new_position(x, move(pos, mov)), mov)
}

// lol
val part2: Long = movements.reverse.tail
  .map(m => assess_course(real_data, Position(-m.x, -m.y, 0), m))
  .map(_.tree.toLong)
  .reduce(_ * _) * assess_course2(real_data, Position(-movements.reverse.head.x, -movements.reverse.head.y, 0), movements.reverse.head).tree

println(s"Part 2: ${part2}")


