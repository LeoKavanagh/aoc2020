// Get product of the elements whose sum is 2020

val ex: List[Int] = List(1721, 979, 366, 299, 673, 1456)

def find_2020_product(x: List[Int]): Int = {
  val candidates: List[Int] = for {
    xs <- ex
    ys <- ex
    if xs + ys == 2020 } yield (xs * ys)
  // to deduplicate
  candidates(0)
}

find_2020_product(ex)

assert(find_2020_product(ex) == 514579)

