def show(array):
    out = ''
    for x in array:
        out += str(x)
    print(out)

'''
def f(a, b):
    return a+b

board = [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
'''

def f(a, b):
    return a*b

board = [1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

show(board)
for i in range(35):
    new = [0]*len(board)
    for i in range(len(board)-2):
        new[i+1] = f(board[i], board[i+1])
    board = new
    show(board)
