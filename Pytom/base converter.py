num = float(input())
base = int(input())
i = 0
out = ''

while int(num) > 0:
    num /= base
    i += 1

for _ in range(i):
    num *= base
    if int(num) > 9:
        out += chr(55+int(num))
    else:
        out += str(int(num))
    num -= int(num)

if num != 0:
    out += '.'
    while num != 0:
        num *= base
        if int(num) > 9:
            out += chr(55+int(num))
        else:
            out += str(int(num))
        num -= int(num)

print(out)
print(len(out))
