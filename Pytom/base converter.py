
num = float(input()) # input number in base ten
base = int(input()) # input base to convert to (in base ten)

i = 0 # initialize counter and output string
out = ''

while int(num) > 0:
    num /= base 
    i += 1

# assuming positive input for num
# i is now the least positive integer such that num / (base ^ i) < 1
# num is now num / (base ^ i)

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
