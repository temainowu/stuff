for i in range(int((int(input()) ** 2 - 1)/6)):
    k = (6 * i + 1) ** 0.5
    if int(k) == k:
        print(k)
