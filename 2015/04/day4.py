from hashlib import md5
import itertools
import functools

secret_key = 'yzbqklnj'

def get_md5_hex(secret_key, value):
    if type(value) != int:
        raise TypeError(' {}: {} should be int'.format(value, type_value))
    elif value < 1:
        raise ValueError('value {} must be > 0'.format(value))
    proposed = (secret_key + str(value)).encode('utf-8')
    hexval = md5(proposed).hexdigest()
    return hexval


def find_value(secret_key, n_zeros):
    check = '0' * n_zeros
    i = 1
    while True:
        hexval = get_md5_hex(secret_key, i)
        if hexval.startswith(check):
            yield i
        i += 1

# examples
assert next(find_value('abcdef', 5)) == 609043
assert next(find_value('pqrstuv', 5)) == 1048970

%%time
part1 = next(find_value(secret_key, 5))

%%time
part2 = next(find_value(secret_key, 6))

