import sys
import re
from ctypes import c_uint16
from collections import defaultdict
from functools import reduce


INPUT_SIGNAL = r'(^\d+$)'
ANOTHER_WIRE = r'(^\w+$)'
AND_GATE = r'(\w+) AND (\w+)'
OR_GATE = r'(\w+) OR (\w+)'
LSHIFT_GATE = r'(\w+) LSHIFT (\w+)'
RSHIFT_GATE = r'(\w+) RSHIFT (\w+)'
NOT_GATE = r'NOT (\w+)'

def bitwise_and(wire1, wire2):
    return c_uint16(wire1 & wire2).value
def bitwise_or(wire1, wire2):
    return c_uint16(wire1 | wire2).value
def bitwise_not(input_value):
    return c_uint16(~input_value).value
def lshift(wire1, shiftval):
    return c_uint16(wire1 << shiftval).value
def rshift(wire1, shiftval):
    return c_uint16(wire1 >> shiftval).value


def add_instruction(dd, raw_instruction):
    body, target = raw_instruction.strip().split(' -> ')
    dd[target] = body
    return dd


def evaluate(circuit, wire, initial_signals):
    """
    Pattern matching isn't coming until Python 3.10,
    so I'll hack a version of it here with re.match in the recursive
    inner function.
    """

    signal_cache = initial_signals

    def _eval(circuit, wire):
        """
        Recursively walk through the circuit
        until we find an actual int input value.

        Keep track of these values by appending them to the
        signal_cache dictionary as they are evaluated.

        We check this signal_cache dictionary each time,
        and only recurse if we need to evaluate something new.

        Eventually, we have the fully defined value of the wire that we want.
        """

        # If we get an int input, that's simply the answer for this wire
        if wire.isnumeric():
            signal = int(wire)

        # Else if we get a wire name that we already know the value of,
        # (because it's in our signal_cache), return its known value
        elif signal_cache[wire]:
            signal = signal_cache[wire]

        # else keep searching, evaluate the wire input definition recursively,
        # adding to the known values (signal_cache) as we go
        else:
            wire_def = circuit[wire]
            if re.match(INPUT_SIGNAL, wire_def):
                signal = int(wire_def)

            elif re.match(ANOTHER_WIRE, wire_def):
                signal  = _eval(circuit, wire_def)

            elif re.match(AND_GATE, wire_def):
                input1, input2 = wire_def.split(' AND ')
                signal = bitwise_and(_eval(circuit, input1),
                                     _eval(circuit, input2))

            elif re.match(OR_GATE, wire_def):
                input1, input2 = wire_def.split(' OR ')
                signal = bitwise_or(_eval(circuit, input1),
                                    _eval(circuit, input2))

            elif re.match(LSHIFT_GATE, wire_def):
                input1, input2 = wire_def.split(' LSHIFT ')
                signal = lshift(_eval(circuit, input1),
                                _eval(circuit, input2))

            elif re.match(RSHIFT_GATE, wire_def):
                input1, input2 = wire_def.split(' RSHIFT ')
                signal = rshift(_eval(circuit, input1),
                                _eval(circuit, input2))

            elif re.match(NOT_GATE, wire_def):
                input1 = wire_def.replace('NOT ', '')
                signal = bitwise_not(_eval(circuit, input1))

            else:
                raise ValueError('Check your regex. '
                                 'Got to {}: {}'.format(wire, wire_def))

        signal_cache[wire] = signal
        return signal

    return _eval(circuit, wire)


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        instruction_map = reduce(add_instruction, f, defaultdict(str))

    part1 = evaluate(instruction_map, 'a', defaultdict(int))
    part2 = evaluate(instruction_map, 'a', defaultdict(int, {'b': part1}))
    print('Part 1: {}'.format(part1))
    print('Part 2: {}'.format(part2))

