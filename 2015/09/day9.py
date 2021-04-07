import sys
from collections import defaultdict
from itertools import permutations
from functools import reduce


def parse(acc, line):
    """
    Take a line from the input data and add it to
    a dictionary of distances from one city to another.
    Accumulator object acc is a tuple (defaultdict, set)
    where:
    defaultdict is observed distances from source city to dest cities
    set is distinct list of city names
    """
    start, _, end, _, dist = line.split()
    dd = acc[0]
    cities = acc[1]
    dd[start][end] = int(dist)
    cities.add(start)
    cities.add(end)
    return dd, cities


def get_dist(a, b, adj):
    """
    Look up the dict of distances from city `a` to city `b` in dict `adj`.
    The way the dict is structured, either `a` is a key and `b` is
    a value, or vice versa. No need for any more complicated logic.
    """
    try:
        dist = adj[a][b]
    except KeyError:
        dist = adj[b][a]
    return dist


def get_full_path_length(cities_list, adj, minimise=True):
    """
    Get total length of every possible path.
    Then take minimum for Part 1 and maximum for Part 2.
    No Travelling Salesman algorithms here, it's just brute force.
    """
    fun = min if minimise else max
    return fun(sum(get_dist(idx[0], idx[1], adj)
                   for idx in zip(path, path[1:]))
               for path in permutations(cities_list))


def main(input_data):

    adj, cities = reduce(lambda x, y: parse(x, y),
                         (a.strip() for a in open(input_data, 'r')),
                         (defaultdict(dict), set()))
    cities_list = list(cities)

    part1 = get_full_path_length(cities_list, adj, minimise=True)
    part2 = get_full_path_length(cities_list, adj, minimise=False)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")


if __name__ == '__main__':
    main(sys.argv[1])

