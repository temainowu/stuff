import random
from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int

    def __hash__(self) -> int:
        return hash(self.x) ^ hash(self.y)


DIM = Point(x=9, y=9)


def init():
    global gb
    gb = [['.' for _ in range(DIM.x)] for _ in range(DIM.y)]
    player_loc = Point(x=DIM.x // 2, y=DIM.y // 2)

    roblocs = set()
    roblocs.add(player_loc)
    while len(roblocs) != 6 + 1:
        roblocs.add(rand_point())
    # roblocs.remove(player_loc)

    for p in roblocs:
        gb[p.y][p.x] = 'T'
    gb[player_loc.y][player_loc.x] = '⫯'


def rand_point():
    return Point(x=rand(False), y=rand(True))


def rand(along_y: bool):
    if along_y:
        return random.randint(0, DIM.y-1)
    else:
        return random.randint(0, DIM.x-1)


def search(target):
    found = None
    for row in range(DIM.y):
        for col in range(DIM.x):
            if gb[row][col] == target:
                found = Point(x=col, y=row)
    return found


def show():
    for row in gb:
        print(f"{' '.join(row)}")


def u():
    loc = search('⫯')
    assert loc is not None
    if loc.y == 0:
        return 0
    else:
        gb[loc.y][loc.x] = '.'
        gb[loc.y-1][loc.x] = '⫯'
        return 1


def d():
    loc = search('⫯')
    assert loc is not None
    if loc.y == DIM.y - 1:
        return 0
    else:
        gb[loc.y][loc.x] = '.'
        gb[loc.y+1][loc.x] = '⫯'
        return 1


def r():
    loc = search('⫯')
    assert loc is not None
    if loc.x == DIM.x - 1:
        return 0
    else:
        gb[loc.y][loc.x] = '.'
        gb[loc.y][loc.x+1] = '⫯'
        return 1


def l():
    loc = search('⫯')
    assert loc is not None
    if loc.x == 0:
        return 0
    else:
        gb[loc.y][loc.x] = '.'
        gb[loc.y][loc.x-1] = '⫯'
        return 1


def robot(roblocs):
    ploc = search('⫯')


init()
while True:
    show()
    turn = 0
    move = int(input())
    if move in [1, 2, 3]:
        if d():
            turn = 1
    if move in [3, 6, 9]:
        if r():
            turn = 1
    if move in [7, 8, 9]:
        if u():
            turn = 1
    if move in [1, 4, 7]:
        if l():
            turn = 1
    if turn:
        print('robot move')
