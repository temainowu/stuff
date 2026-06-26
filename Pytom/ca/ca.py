from random import randint


class ring:
    def __init__(self, array):
        self.out = array

    def sym(self):
        a, b, c, d, e, f, g, h = self.out
        return [[a, b, c, d, e, f, g, h],
                [c, e, h, b, g, a, d, f],
                [h, g, f, e, d, c, b, a],
                [f, d, a, g, b, h, e, c],
                [c, b, a, e, d, h, g, f],
                [f, g, h, d, e, a, b, c],
                [a, d, f, b, g, c, e, h],
                [h, e, c, g, b, f, d, a]]


def perms():
    xs = []
    for i in range(256):
        bitvec = ring([int(i & (1 << shift) != 0) for shift in range(8)])
        xs.append(bitvec)
    nubbed = []
    for x in xs:
        if any(n.out in x.sym() for n in nubbed):
            continue
        nubbed.append(x)
    return nubbed


def show(array):
    out = ''
    for r in array:
        for x in r:
            if x:
                x = '*'
            else:
                x = '-'
            out += x
        out += '\n'
    print(out)


def search(xs, target):
    for i in range(len(xs)):
        if target == xs[i].out:
            return i
    return None


def ZtB(n):
    x = str(bin(n)[2:])
    return list('0'*(51-len(x)) + x)


vibes = perms()


def main(board):
    show(board)
    for i in range(30):
        new = [[0]*len(board[0])]*len(board)
        for i in range(len(board)-2):
            for j in range(len(board[i])-2):
                pos = search(vibes, max(
                    ring([board[i+0][j], board[i+0][j+1], board[i+0][j+2],
                          board[i+1][j],                  board[i+1][j+2],
                          board[i+2][j], board[i+2][j+1], board[i+2][j+2]]).sym()))
                assert pos is not None
                new[i+1][j+1] = rule[pos]
        board = new
        show(board)


#rule = list(map(int, ZtB(int(input()))))
rule = list(map(int, ZtB(randint(0, 2251799813685247))))

board = [[0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 0, 0, 0],
         [0, 0, 0, 1, 0, 1, 0, 0],
         [0, 0, 0, 0, 1, 0, 0, 0],
         [0, 0, 1, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 1, 0],
         [0, 0, 0, 0, 0, 0, 0, 0]]

print(len(vibes))
for x in vibes:
    print(x.out)
print(rule)

main(board)
