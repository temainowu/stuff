from functools import cache

MAX = 999


array = [2, 3]
for i in range(int((MAX**2 - 1) / 24)):
    if i / 10 == int(i / 10) or (i - 2) / 10 == int((i - 2) / 10) or (i - 5) / 10 == int((i - 5) / 10) or (i - 7) / 10 == int((i - 7) / 10):
        array.append(int((24*i + 1) ** 0.5))


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
