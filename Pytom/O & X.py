board = list('123456789')
winCombos = [[1, 1, 1, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 1, 1, 1, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 1, 1, 1],
             [1, 0, 0, 1, 0, 0, 1, 0, 0],
             [0, 1, 0, 0, 1, 0, 0, 1, 0],
             [0, 0, 1, 0, 0, 1, 0, 0, 1],
             [1, 0, 0, 0, 1, 0, 0, 0, 1],
             [0, 0, 1, 0, 1, 0, 1, 0, 0]]

loclist = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
           [0, 0, 0, 0, 0, 0, 0, 0, 0]]
player = ['', '']
symbol = ['X', 'O']


def win_found(win_pattern, halfboard):
    out = [x & y for x, y in zip(win_pattern, halfboard)]
    return out == win_pattern


def gameOver():
    for j in range(len(winCombos)):
        if win_found(winCombos[j], loclist[0]):
            print(f'Player 1 won in {moveCount} moves')
            return True
        elif win_found(winCombos[j], loclist[1]):
            print(f'Player 2 won in {moveCount} moves')
            return True
        # end if
    # end for

    if moveCount > 8:
        return True
    else:
        return False
    # end if
# end def


def show_board():
    print(board[0], '|', board[1], '|', board[2])
    print('---------')
    print(board[3], '|', board[4], '|', board[5])
    print('---------')
    print(board[6], '|', board[7], '|', board[8])
# end def


print('Noughts and Crosses')
show_board()

player[0] = input('Name of player 1: ')
player[1] = input('Name of player 2: ')

moveCount = 0
while not gameOver():
    move = moveCount & 1
    print('hello', player[move])
    loc = int(input('Enter move: '))-1
    while loclist[0][loc] or loclist[1][loc]:
        print('invalid move')
        loc = int(input('Enter move: '))-1
    board[loc] = symbol[move]
    loclist[move][loc] = 1
    show_board()
    moveCount += 1
# end while
