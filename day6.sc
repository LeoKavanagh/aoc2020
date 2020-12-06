// import os._

println(s"Ans 1: ${os.read(os.Path("/home/leo/repos/aoc2020/data/day6.csv")).split("\n\n").map(_.filterNot(_ == '\n').toSet.size).sum}")

println(s"Ans 2: ${os.read(os.Path("/home/leo/repos/aoc2020/data/day6.csv")).split("\n\n").map(_.split('\n')).map(a => (a.length, a.flatMap(_.toList).groupBy(identity).mapValues(_.size).values)).map(b => (b._1, b._2.filter(_ >= b._1))).map(a => a._2.sum / a._1).sum}")

