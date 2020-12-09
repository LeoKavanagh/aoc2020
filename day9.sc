
val ex = """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"""
  .split('\n')
  .map(_.toLong)
  .toList

def hasValidSum(preamble: Seq[Long], target: Long): Boolean = {
  val newX = preamble.sorted.filter(_ < target).toSet
  val candidates = for {
    xs <- newX
    ys <- newX
    if xs != ys && xs + ys == target } yield true
  candidates.size > 0
}

def findWeakness(data: List[Long], preambleLength: Int): Long = {
  val indexedData = data.zipWithIndex
  val isValid = for {
    x <- indexedData if x._2 >= preambleLength
    val comps = indexedData.splitAt(x._2)
  } yield (x._1, hasValidSum(comps._1.map(_._1).takeRight(preambleLength), x._1))
  isValid.filter(_._2 == false)(0)._1
}

assert(findWeakness(ex, 5).toInt == 127)

val realData = os
  .read
  .lines(os.Path("/home/leo/repos/aoc2020/data/day9.csv"))
  .map(_.toLong)
  .toList

val ans1: Long = findWeakness(realData, 25)

println(s"Ans 1: ${ans1}")

// dedicated to Martin Odersky
def groupPrefix[T](xs: List[T])(p: T => Boolean): List[List[T]] = xs match {
  case List() => List()
  case x :: xs1 =>
    val (ys, zs) = xs1 span (!p(_))
    ys :: groupPrefix(zs)(p)
}

def contig(data: List[Long], target: Long): IndexedSeq[Long] = {
  for {
    a <- (2 until data.length)
    b <- data.sliding(a) if b.sum == target
  } yield b.min + b.max
}

assert(groupPrefix(ex)(_ == 127).flatMap(a => contig(a, 127L))(0).toInt == 62)

val ans2: Long = groupPrefix(realData)(_ == ans1).flatMap(a => contig(a, ans1))(0)

println(s"Ans 2 = ${ans2}")

