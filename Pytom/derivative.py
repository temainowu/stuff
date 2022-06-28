from math import sqrt


def replace(num, x, string='x'):
    if x == string:
        return num
    else:
        return x


def evalinrange(start, end, acc, func):
    dx = (end - start)/acc

    array = [0 for _ in range(acc)]
    for i, j in enumerate([dx*i for i in range(acc)]):
        array[i] = eval("".join([replace(str(j), e) for e in func]))

    return array


def zipw(f, array):
    out = [0]*(len(array)-1)
    for i in range(len(array)-1):
        out[i] = f(array[i+1], array[i])
    return out


def r(f, array):
    tot = f()
    for e in array:
        tot = f(tot, e)
    return tot


'''
def r_rec(f, array):
    if array == []: return f()
    e, *rest = array
    return f(e, r_rec(f, rest))
'''


def mult(a=None, b=None):
    if [a, b] == [None, None]:
        return 1
    else:
        return a*b


def add(a=None, b=None):
    if [a, b] == [None, None]:
        return 0
    else:
        return a+b


def cat(a=None, b=None):
    if [a, b] == [None, None]:
        return ''
    else:
        return a+b


def sub(a, b):
    return a - b


def derive(func):
    points = evalinrange(0, 2, 50, func)
    derived = zipw(sub, points)
    return lagrange_interp(derived)


def lagrange_interp(array):
    a = ['']*len(array)
    b = ['']*len(array)
    for i in range(len(array)):
        stuff = [i-j for j in range(len(array)) if j != i]
        a[i] = f'+{array[i] / int(r(mult, stuff))}'
    for i in range(len(array)):
        stuff = [f'(x-{str(j)})' for j in range(len(array)) if j != i]
        b[i] = str(r(cat, stuff))
    return "".join([replace('*10^', x, 'e') for x in r(cat, [cat(l, r) for l, r in zip(a, b)])])


print(derive(input()))
