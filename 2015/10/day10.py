import sys


def mapper(input_string):
    """
    Read the input string character-by-character.

    At each step in the for-loop, if the new character is the same
    as the current one, increment the counter.

    If it is different, yield the counter and the current char as one string,
    and then reset the counter to 1 and continue with the for-loop.

    So for the first part of "2225" we get
    -> three twos -> 32, where ct == 3 and curr == 2 and x != curr.

    The end of the string is just like seeing a new character, so we yield
    the count and character that we have last seen.

    In order to keep the logic simple, we initialise `curr` with something
    that isn't going to appear as the first character of the input string.
    """

    curr = '-1'
    ct = 0
    for x in input_string:
        if x == curr:
            ct += 1
        else:
            yield(str(ct) + curr)
            curr = x
            ct = 1
    yield(str(ct) + curr)


def reducer(count_strings):
    # Discard the "0-1" initialisation by calling next() on the input.
    # Consider this a shuffle-and-sort before the actual reduce step.
    _ = next(count_strings)
    return ''.join(a for a in count_strings)


def look_and_say(input_string):
    return reducer(mapper(input_string))


def repeated_look_and_say(input_string, times):
    acc = 0
    while acc < times:
        input_string = look_and_say(input_string)
        acc += 1
    return input_string, len(input_string)


def main(input_path):
    with open(input_path, 'r') as f:
        input_string = f.read().strip()

    part1 = repeated_look_and_say(input_string, 40)
    print(part1[1])
    part2 = repeated_look_and_say(part1[0], 10)
    print(part2[1])


if __name__ == '__main__':
    main(sys.argv[1])

