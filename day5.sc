import os._
import scala.annotation.tailrec

@tailrec
def findSeat(rowString: String, ranges: (Range, Range)): (Int, Int) = rowString match {
  case "" => (ranges._1(0), ranges._2(0))
  case s"F$xs" => findSeat(xs, (ranges._1.splitAt(ranges._1.length / 2)._1, ranges._2))
  case s"B$xs" => findSeat(xs, (ranges._1.splitAt(ranges._1.length / 2)._2, ranges._2))
  case s"L$xs" => findSeat(xs, (ranges._1, ranges._2.splitAt(ranges._2.length / 2)._1))
  case s"R$xs" => findSeat(xs, (ranges._1, ranges._2.splitAt(ranges._2.length / 2)._2))
}

def calculateSeatID(rowNum: Int, colNum: Int): Int = {
  rowNum * 8 + colNum
}

def getSeatID(seatString: String, ranges: (Range, Range), calcID: (Int, Int) => Int): Int = {
  val (row: Int, col: Int) = findSeat(seatString, ranges)
  calcID(row, col)
}


val ex: List[String] = """FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL""".split("\n").toList

val fullRowRange: Range.Inclusive = 0 to 127
val fullColRange : Range.Inclusive = 0 to 7
val fullRanges = (fullRowRange, fullColRange)

assert(ex.map(getSeatID(_, fullRanges, calculateSeatID)) == List(357, 567, 119, 820))

val realData: List[String] = os
  .read
  .lines(os.Path("/home/leo/repos/aoc2020/data/day5.csv"))
  .toList

val sortedSeatIDs = realData
  .map(getSeatID(_, fullRanges, calculateSeatID))
  .sorted

val ans1: Int = sortedSeatIDs.max

println(s"Ans 1 = ${ans1}")

val nearbySeat: (Int, Int) = (sortedSeatIDs.drop(1), sortedSeatIDs)
  .zipped
  .map(_-_)
  .zipWithIndex
  .filter(_._1 != 1)(0)

println(s"Seat No: ${nearbySeat._2} has a gap of ${nearbySeat._1} to the next seat:")
println(sortedSeatIDs(540), sortedSeatIDs(541), sortedSeatIDs(542))

val ans2: Int = sortedSeatIDs(nearbySeat._2) + 1
println(s"Ans 2 = ${ans2}")

