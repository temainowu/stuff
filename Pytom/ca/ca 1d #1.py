def symmetries(array):
    a, b = array
    return [[a, b], [b, a]]

def search(xs, target):
    for i in range(len(xs)):
        if target == xs[i]:
            return i
    return None


def show(array):
    out = ''
    for x in array:
        if x:
            x = '#'
        else:
            x = ' '
        out += x
    print(out)


board = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

vibes = [[0, 0], [1, 0], [1, 1]]
rule = [0, 1, 0]

show(board)
for i in range(120):
    new = [0]*len(board)
    for i in range(len(board)-2):
        pos = search(vibes, max(symmetries([board[i], board[i+2]])))
        assert pos is not None
        new[i+1] = rule[pos]
    board = new
    show(board)
