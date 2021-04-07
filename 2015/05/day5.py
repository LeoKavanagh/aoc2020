"""
Lol
"""
import sys
from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql import DataFrame
from pyspark.sql.types import StructField, StructType, StringType


# This exists in Scala Spark but not PySpark
def transform(self, f):
    return f(self)
DataFrame.transform = transform


def with_regex_flag(regex, flag_name):
    """
    Check each row 'string' column to see if it matches the
    provided regex. If it matches, return 1, else return 0.
    Store these 1s and 0s in a new column called `flag_name`.

    This could obviously be a filter instead of a withColumn and F.when,
    but being able to populate a new column based on the value of
    some other column value is handy.
    """
    def _inner(df):
        condition = F.col('string').rlike(regex)
        return df \
            .withColumn(flag_name,
                        F.when(condition, F.lit(1)).otherwise(F.lit(0)))
    return _inner


def main(input_path):
    spark = SparkSession.builder.getOrCreate()

    input_schema = StructType([StructField('string', StringType(), False)])

    df = spark \
        .read \
        .csv(input_path,
             schema=input_schema,
             header=False)

    # Part 1 regex

    # this finds exactly 3 instances, which is good enough for at least 3
    three_vowels = r'[aeiou].*[aeiou].*[aeiou]'

    # [a-z] => any lower case letter
    # \1 => the previous thing (any lower case letter
    # {1,} => occurring between one and Inf times
    repeated_letter = r'([a-z])\1{1,}'

    # negative lookahead is a bit silly here,
    # but it lets us use the with_regex_flag with no alterations.
    # ^ => start of string
    # ?! => negative lookahead: next group is NOT allowed to match this expression
    # .* => any character, any number of times
    # (ab|cd|pq|xy) => 'ab' OR 'cd' OR 'pq' OR 'xy'
    no_bad_string = r'^(?!.*(ab|cd|pq|xy)).*'

    # Part 2 regex

    # (..) => any two characters
    # .* => any character, any number of times
    # \1 the first matching group, again
    non_overlapping_pairs = r'(..).*\1'

    # (.) => any character
    # . => any character
    # \1 => match the result of group one (the first (.) in this case)
    repeating_with_gap = r'(.).\1'


    part1 = df \
        .transform(with_regex_flag(three_vowels, 'three_vowels')) \
        .transform(with_regex_flag(repeated_letter, 'repeated_letter')) \
        .transform(with_regex_flag(no_bad_string, 'no_bad_string')) \
        .filter('three_vowels = 1 and repeated_letter = 1 and no_bad_string = 1') \
        .count()


    part2 = df \
        .transform(with_regex_flag(non_overlapping_pairs, 'non_overlapping_pairs')) \
        .transform(with_regex_flag(repeating_with_gap, 'repeating_with_gap')) \
        .filter('non_overlapping_pairs = 1 and repeating_with_gap = 1') \
        .count()

    print("Answer to Part 1: {}".format(part1))
    print("Answer to Part 2: {}".format(part2))

if __name__ == '__main__':
    main(sys.argv[1])

