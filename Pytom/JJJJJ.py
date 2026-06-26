import random
from typing import Optional


class User:
    def __init__(self, name, score):
        self.name = name
        self.score = score
    # end def

    def show(self):
        print(f'{self.name} has won {self.score} games')
    # end def
# end class


loclist = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
           [0, 0, 0, 0, 0, 0, 0, 0, 0]]


def sort(array, key):
    for i in range(1, len(array)):
        temp = array[i]  # swap
        pos = i

        while pos > 0 and key(array[pos-1]) < key(temp):
            array[pos] = array[pos-1]  # swap
            pos -= 1
        # end while

        array[pos] = temp  # swap
    # end for


def readScore():
    scores = []
    file = open('Scores.txt', 'r')
    data = file.readlines()
    file.close()

    for line in data:
        user = line.split(",")
        scores.append(User(user[0], int(user[1])))
    # end for

    sort(scores, key=lambda u: u.score)
    for i in scores:
        i.show()
    # end for
# end def


def playMove():  # currently random (probably fine)
    play = random.randint(0, 8)
    while loclist[0][play] or loclist[1][play]:
        play = random.randint(0, 8)
    # end while
    return play
# end def


def binSearch(array, target) -> "Optional[int]":
    found = False
    lo = 0
    up = len(array) - 1

    while lo <= up and not found:
        mid = (lo + up) // 2

        if array[mid].name == target:
            found = True
            break

        elif array[mid].name > target:
            up = mid - 1

        else:
            lo = mid + 1
        # end if
    # end while
    if found == False:
        return None
    else:
        return mid
    # end if
# end def


def addScore(name):
    scores = []
    file = open('Scores.txt', 'r')
    data = file.readlines()
    file.close()

    for line in data:
        username, score_str = line.split(",")
        scores.append(User(username, int(score_str)))
    # end for

    sort(scores, key=lambda u: u.name)

    location = binSearch(scores, name)

    if location is not None:
        scores[location].score += 1
    else:
        scores.append(User(name, 1))
    # end if

    file = open('Scores.txt', 'w')
    for i in scores:
        file.write(f'{i.name},{i.score},\n')
    # end for
    file.close()
# end def


def gameOver(moveCount, name):
    winCombos = [[1, 1, 1, 0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 1, 1, 1, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0, 1, 1, 1],
                 [1, 0, 0, 1, 0, 0, 1, 0, 0],
                 [0, 1, 0, 0, 1, 0, 0, 1, 0],
                 [0, 0, 1, 0, 0, 1, 0, 0, 1],
                 [1, 0, 0, 0, 1, 0, 0, 0, 1],
                 [0, 0, 1, 0, 1, 0, 1, 0, 0]]
    for j in range(len(winCombos)):
        if [x & y for x, y in zip(winCombos[j], loclist[0])] in winCombos:
            print('You won in', (moveCount+1)//2, 'moves')
            addScore(name)
            return True
        elif [x & y for x, y in zip(winCombos[j], loclist[1])] in winCombos:
            print('AI won in', (moveCount)//2, 'moves')
            return True
        # end if
    # end for

    if moveCount > 8:
        print('Draw')
        return True
    else:
        return False
    # end if
# end def


def show_board(board):
    print(f'{board[0]} | {board[1]} | {board[2]}')
    print('---------')
    print(f'{board[3]} | {board[4]} | {board[5]}')
    print('---------')
    print(f'{board[6]} | {board[7]} | {board[8]}')
# end def


def play():
    board = [' ', ' ', ' ',
             ' ', ' ', ' ',
             ' ', ' ', ' ']
    symbol = ['X', 'O']
    print('Noughts and Crosses')
    show_board(board)

    name = input('Name: ')

    moveCount = 0
    while not gameOver(moveCount, name):
        move = moveCount & 1  # %2
        if move:
            loc = playMove()
        else:
            loc = int(input('Enter move: '))-1
            while loclist[0][loc] or loclist[1][loc]:
                print('invalid move')
                loc = int(input('Enter move: '))-1
            # end while
        # end if
        board[loc] = symbol[move]
        loclist[move][loc] = 1
        show_board(board)
        moveCount += 1
    # end while
# end if


def main():
    if input('Play Game? (Y/N): ') == 'Y':
        play()
    elif input('View scores? (Y/N): ') == 'Y':
        readScore()
    # end if
# end main


main()
