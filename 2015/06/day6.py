import sys
import re
import numpy as np
from functools import reduce


def parse_instruction(raw_instr):
    instr = raw_instr.strip()
    # skip blank lines. Doing f.strip('\n') here basically
    if not instr:
        return -1, -1, -1, -1, None
    else:
        # at least one number,
        # followed by a comma,
        # followed by at least one number,
        start, end = re.findall(r'[0-9]+,[0-9]+', instr)
        x1, y1 = map(int, start.split(','))
        x2, y2 = map(int, end.split(','))
        # 2nd word in string is either 'on', 'off' or comma-separated numbers
        # (in which case it's a toggle)
        cmd = instr.split()[1]
        return x1, y1, x2, y2, cmd


def apply_instruction1(M, raw_instr):
    x1, y1, x2, y2, cmd = parse_instruction(raw_instr)
    if not cmd:
        return M
    else:
        subMatrix = M[x1:x2+1, y1:y2+1]
        if cmd == 'on':
            subMatrix = np.ones(subMatrix.shape)
        elif cmd == 'off':
            subMatrix = np.zeros(subMatrix.shape)
        else:
           np.logical_not(subMatrix, out=subMatrix).astype(int)
        M[x1:x2+1, y1:y2+1] = subMatrix
        return M


def apply_instruction2(raw_instr, M):
    x1, y1, x2, y2, cmd = parse_instruction(raw_instr)
    if not cmd:
        return M
    else:
        subMatrix = M[x1:x2+1, y1:y2+1]
        if cmd == 'on':
            subMatrix += 1
        elif cmd == 'off':
            subMatrix -= 1
            np.clip(subMatrix, 0, np.inf, out=subMatrix)
        else:
            subMatrix += 2
        M[x1:x2+1, y1:y2+1] = subMatrix
        return M



def main(args):

    n = 1000

    fgen = (line for line in open(sys.argv[1]))
    # part 1: 400410
    part1 = reduce(lambda M, line: apply_instruction1(M=M, raw_instr=line),
                fgen,
                np.zeros(shape=(n, n), dtype=int)).sum()
    print(part1)


    fgen = (line for line in open(sys.argv[1]))
    # part2: 15343601
    part2 = reduce(lambda M, line: apply_instruction2(M=M, raw_instr=line),
                fgen,
                np.zeros(shape=(n, n), dtype=int)).sum()
    print(part2)


if __name__ == '__main__':
    main(sys.argv)

