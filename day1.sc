// Get product of the elements whose sum is 2020

import os._

val ex: List[Int] = List(1721, 979, 366, 299, 673, 1456)

def find_2020_product(x: Seq[Int]): Int = {
  val candidates: Seq[Int] = for {
    xs <- x
    ys <- x
    if xs + ys == 2020 } yield (xs * ys)
  // to deduplicate. I know there's only one answer so I won't use .reduce or toSet etc
  // Lazy evaluation...
  candidates(0)
}

assert(find_2020_product(ex) == 514579)
println(find_2020_product(ex))

// too hard to deal with logins through sttp or requests or whatever.
// Just save to file
val real_data = os
  .read
  .lines(os.Path("/home/leo/repos/aoc2020/data/day1.csv"))
  .map(_.toInt)
  .toList

println(find_2020_product(real_data))


def find_2020_product2(x: Seq[Int]): Int = {
  val candidates: Seq[Int] = for {
    xs <- x
    ys <- x
    zs <- x
    if xs + ys + zs == 2020 } yield (xs * ys * zs)
  // to deduplicate
  candidates(0)
}

println(find_2020_product2(real_data))

