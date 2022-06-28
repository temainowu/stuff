from functools import cache

MAX = 9999

array = [
    int(v) for v in map(
        lambda x: (6 * x + 1) ** 0.5,
        range(2, int((MAX ** 2 - 1)/6)))
    if int(v) == v]

array = [2, 3] + array


@cache
def flunc(x):
    global MAX
    for i in range(2, MAX):
        if x % i == 0 and x != i:
            return False
    return True


for g in range(2, MAX):
    array = list(filter(flunc, array))

print(array)
