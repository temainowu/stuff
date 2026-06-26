print('ax²+bx+c')
a = float(input('a: '))
b = float(input('b: '))
c = float(input('c: '))

d = b/(2*a)
C = c-(a*(d**2))

if int(a) == a:
    a = int(a)
if int(d) == d:
    d = int(d)
if int(C) == C:
    C = int(C)

if a == 1:
    a = ''

if C >= 0:
    C = '+' + str(C)
if d >= 0:
    d = '+' + str(d)

print(f'{a}(x{d})²{C}')
