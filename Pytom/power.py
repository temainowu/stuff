a = int(input('a: '))
b = int(input('b: '))


def pow(a=a, b=b):
    fa = 1
    fb = 1

    for i in range(b):
        fa *= a

    for i in range(a):
        fb *= b

    return (fa/2)+(fb/2)


def invpow(c=pow()):
    return c**(1/2)


# print(pow())
print(invpow())
