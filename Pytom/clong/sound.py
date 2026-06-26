import random


def c(x):
    return random.choice(x)


O = ['p', 't', 'k', 'b', 'd', 'g', '?', 'f', 's', 'x', 'h', 'm', 'n']
N = ['a', 'i', 'u', 'e', 'o']
C = ['m', 'n', 'g']


def gen():
    r = [True, False]
    x = ''
    if c(r):
        x += c(O)
    x += c(N)
    if c(r):
        x += c(C)
    if c(r):
        x += c(O)
        x += c(N)
        if c(r):
            x += c(C)
    if c(r):
        x += c(O)
        x += c(N)
        if c(r):
            x += c(C)
    return x


file = open('words.txt', 'r')
words = file.readlines()
file.close()

W = [['', '', ''] for i in words]

for i, word in enumerate(words):
    p = 0
    word += '@'
    for j in range(len(word)):
        if word[j] in N and word[j+1] not in C:
            W[i][p] = word[j]
            p += 1
        if (word[j] in N and word[j+1] in C) or (word[j] in O and word[j+1] in N):
            W[i][p] = word[j:j+1]
            j += 1
            p += 1
        if word[j] in O and word[j+2] in C:
            W[i][p] = word[j:j+2]
            j += 2
            p += 1

print(W)
