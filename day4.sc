import os._
import scala.util.matching.{Regex}

val ex = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

val new_ex: String = """eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"""

val real_data: String = os
  .read(os.Path("/home/leo/repos/aoc2020/data/day4.csv"))


case class Passport(
  ecl: Option[String],
  pid: Option[String],
  eyr: Option[String],
  hcl: Option[String],
  byr: Option[String],
  iyr: Option[String],
  cid: Option[String],
  hgt: Option[String])

def map_to_passport(m: Map[String, String]) = {
  Passport(
    m.get("ecl"),
    m.get("pid"),
    m.get("eyr"),
    m.get("hcl"),
    m.get("byr"),
    m.get("iyr"),
    m.get("cid"),
    m.get("hgt")
    )
}

def make_passport(line: String): Passport = {

  val observed_fields: Map[String, String] = line
    .replace("\n", " ")
    .split("\\s+")
    .map(a => a.split(":"))
    .map(arr => Map[String, String](arr(0) -> arr(1)))
    .reduce(_ ++ _)

  val passport = map_to_passport(observed_fields)
  passport
}

val ex_passports: List[Passport] = ex.split("\n\n").toList.map(make_passport)

// check cid only
def is_valid1(passport: Passport): Boolean = passport match {
  case Passport(Some(_), Some(_), Some(_), Some(_), Some(_), Some(_), None, Some(_)) => true
  case Passport(Some(_), Some(_), Some(_), Some(_), Some(_), Some(_), Some(_), Some(_)) => true
  case _ => false
}

val ex_ans: Int = ex_passports.filter(is_valid1).length
println(s"Example answer 1: ${ex_ans}")

val ans1: Int = real_data
  .split("\n\n")
  .toList
  .map(make_passport)
  .filter(is_valid1)
  .length

println(s"Answer 1: ${ans1}")

def valid_digits(field: Option[String]): Boolean = field match {
  case None => false
  case Some(field) => field.forall(_.isDigit)
}

def within_range(x: Int, min: Int, max: Int) = x >= min && x <= max

def valid_year(year: Option[String], min: Int, max: Int): Boolean = year match {
  case None => false
  case Some(field) => {
    if( field.forall(_.isDigit) ) within_range(field.toInt, min, max)
    else false
  }
}

val height_regex_check = raw"(\d{2,3})(cm|in)".r
val pid_regex_check = raw"(\d{9})".r
val hair_regex_check = raw"(#)([0-9a-z]{6})".r
val eye_check = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

case class HeightLimits(cm_min: Int, cm_max: Int, in_min: Int, in_max: Int)
implicit val hl: HeightLimits = HeightLimits(150, 193, 59, 76)

def valid_height(field: Option[String],height_regex_check: Regex)(implicit hl: HeightLimits) =
  field match {
  case None => false
  case Some(field) => { field match {
    case height_regex_check(field, "cm") => within_range(field.toInt, hl.cm_min, hl.cm_max)
    case height_regex_check(field, "in") => within_range(field.toInt, hl.in_min, hl.in_max)
    case _ => false
  }
  }
}

def valid_hair(field: Option[String], hair_regex_check: Regex) = field match {
  case None => false
  case Some(field) => field match {
    case hair_regex_check(hash, digits) => true
    case _ => false
  }
}

// Didn't use x.contains(y) just in case "exactly one of" was a gotcha
def valid_eyes(field: Option[String], eye_check: List[String]) = field match {
  case None => false
  case Some(field) => if (eye_check.filter(_ == field).length == 1) true else false
}

def valid_pid(field: Option[String], pid_regex_check: Regex) = field match {
  case None => false
  case Some(field) => field match {
    case pid_regex_check(digits) => true
    case _ => false
  }
}

val ex_ans2 = new_ex
  .split("\n\n")
  .toList
  .map(make_passport)
  .filter(a => valid_year(a.byr, 1920, 2002))
  .filter(a => valid_year(a.iyr, 2010, 2020))
  .filter(a => valid_year(a.eyr, 2020, 2030))
  .filter(a => valid_hair(a.hcl, hair_regex_check))
  .filter(a => valid_eyes(a.ecl, eye_check))
  .filter(a => valid_pid(a.pid, pid_regex_check))
  .filter(a => valid_height(a.hgt, height_regex_check))
  .length


println(s"Example answer 2: ${ex_ans2}")

val ans2 = real_data
  .split("\n\n")
  .toList
  .map(make_passport)
  .filter(a => valid_year(a.byr, 1920, 2002))
  .filter(a => valid_year(a.iyr, 2010, 2020))
  .filter(a => valid_year(a.eyr, 2020, 2030))
  .filter(a => valid_hair(a.hcl, hair_regex_check))
  .filter(a => valid_eyes(a.ecl, eye_check))
  .filter(a => valid_pid(a.pid, pid_regex_check))
  .filter(a => valid_height(a.hgt, height_regex_check))
  .length

println(s"Answer 2: ${ans2}")
