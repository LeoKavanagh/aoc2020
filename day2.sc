import os._

case class Policy(letter: String, lower: Int, upper: Int)

val ex: List[String] = List(
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc")

def extract_policy(raw_policy: String): Policy = {
  val Array(raw_limits, letter) = raw_policy.split(" ")
  val limits = raw_limits.split("-").map(_.toInt)
  val policy = Policy(letter, limits(0), limits(1))
  policy
}

//   val counts = pw.map(a => (a.toString, 1)).groupBy(_._1).mapValues(_.size)

def apply_policy(pw: String, policy: Policy): Boolean = {
  val letter_count = pw.filter(_ == policy.letter.head).length
  letter_count >= policy.lower && letter_count <= policy.upper
}

def parse(entry: String): Boolean = {
  val Array(raw_policy, pw) = entry.split(": ")
  val policy = extract_policy(raw_policy)
  apply_policy(pw, policy)
}

assert(ex.filter(parse).length == 2)

val real_data = os
  .read
  .lines(os.Path("/home/leo/repos/aoc2020/data/day2.csv"))
  .toList

val num_valid = real_data.filter(parse).length

def apply_policy2(pw: String, policy: Policy): Boolean = {
  List(policy.lower-1, policy.upper-1)
    .collect(pw)
    .filter(_ == policy.letter.head)
    .length == 1
}

def parse2(entry: String): Boolean = {
  val Array(raw_policy, pw) = entry.split(": ")
  val policy = extract_policy(raw_policy)
  apply_policy2(pw, policy)
}

val num_valid2 = real_data.filter(parse2).length

println(s"Num valid: ${num_valid}")
println(s"Num valid 2: ${num_valid2}")

