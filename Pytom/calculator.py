efre = 'the answer is '
hmn = int(input('how many numbers'))
if hmn == 1:
    a = float(input('what is the your number '))
    cat = input('(ord) (sqr) (rt) (rnd)')
    if cat == 'rt':
        answer = a ** 0.5
        print(efre + str(answer))
    elif cat == 'sqr':
        answer = a ** 2
        answey = b ** 2
        print(efre + str(answer) + ', ' + str(answey))
    elif cat == 'rnd':
        answer = int(a + 0.5)
        answey = int(b + 0.5)
        print(efre + str(answer) + ', ' + str(answey))
    elif cat == 'ord':
        b = float(input('to the power of '))
        answer = a ** b
        print (answer)
    else:
        print('invalid input')
elif hmn == 2:
    a = float(input('what is the first number '))
    cat = input('(+) (-) (*) (/)')
    b = float(input('what is the second number '))
    if cat == '+':
        answer = a + b
        print(efre + str(answer))
    elif cat == '-':
        answer = a - b
        print(efre + str(answer))
    elif cat == '*':
        answer = a * b
        print(efre + str(answer))
    elif cat == '/':
        answer = a / b
        print(efre + str(answer))
    else:
        print('invalid input')
elif hmn == 3:
    a = float(input('what is the first number '))
    cat = input('(+) (-) (*) (/)')
    b = float(input('what is the second number '))
    cat2 = input('(+) (-) (*) (/)')
    c = float(input('what is the third number '))
    if cat == '+':
        if cat2 == '+':
            print (efre + str(a+b+c))
        elif cat2 == '-':
            print (efre + str(a+b-c))
        elif cat2 == '*':
            print (efre + str(a+b*c))
        elif cat2 == '/':
            print (efre + str(a+b/c))
        else:
            print('invalid input')
    elif cat == '-':
        if cat2 == '+':
            print (efre + str(a-b+c))
        elif cat2 == '-':
            print (efre + str(a-b-c))
        elif cat2 == '*':
            print (efre + str(a-b*c))
        elif cat2 == '/':
            print (efre + str(a-b/c))
        else:
            print('invalid input')
    elif cat == '*':
        if cat2 == '+':
            print (efre + str(a*b+c))
        elif cat2 == '-':
            print (efre + str(a*b-c))
        elif cat2 == '*':
            print (efre + str(a*b*c))
        elif cat2 == '/':
            print (efre + str(a*b/c))
        else:
            print('invalid input')
    elif cat == '/':
        if cat2 == '+':
            print (efre + str(a/b+c))
        elif cat2 == '-':
            print (efre + str(a/b-c))
        elif cat2 == '*':
            print (efre + str(a/b*c))
        elif cat2 == '/':
            print (efre + str(a/b/c))
        else:
            print('invalid input')
    else:
        print('invalid input')
elif hmn == 4:
    a = float(input('what is the first number '))
    cat = input('(+) (-) (*) (/)')
    b = float(input('what is the second number '))
    cat2 = input('(+) (-) (*) (/)')
    c = float(input('what is the third number '))
    cat3 = input('(+) (-) (*) (/)')
    d = float(input('what is the third number '))
    if cat == '+':
        if cat2 == '+':
            if cat3 == '+':
                print (efre + str(a+b+c+d))
            elif cat3 == '-':
                print (efre + str(a+b+c-d))
            elif cat3 == '*':
                print (efre + str(a+b+c*d))
            elif cat3 == '/':
                print (efre + str(a+b+c/d))
        elif cat2 == '-':
            if cat3 == '+':
                print (efre + str(a+b-c+d))
            elif cat3 == '-':
                print (efre + str(a+b-c-d))
            elif cat3 == '*':
                print (efre + str(a+b-c*d))
            elif cat3 == '/':
                print (efre + str(a+b-c/d))
        elif cat == '*':
            if cat3 == '+':
                print (efre + str(a+b*c+d))
            elif cat3 == '-':
                print (efre + str(a+b*c-d))
            elif cat3 == '*':
                print (efre + str(a+b*c*d))
            elif cat3 == '/':
                print (efre + str(a+b*c/d))
        elif cat2 == '/':
            if cat3 == '+':
                print (efre + str(a+b/c+d))
            elif cat3 == '-':
                print (efre + str(a+b/c-d))
            elif cat3 == '*':
                print (efre + str(a+b/c*d))
            elif cat3 == '/':
                print (efre + str(a+b/c/d))
        else:
            print('invalid input')
    elif cat == '-':
        if cat3 == '+':
            if cat3 == '+':
                print (efre + str(a-b+c+d))
            elif cat3 == '-':
                print (efre + str(a-b+c-d))
            elif cat3 == '*':
                print (efre + str(a-b+c*d))
            elif cat3 == '/':
                print (efre + str(a-b+c/d))
        elif cat3 == '-':
            if cat3 == '+':
                print (efre + str(a-b-c+d))
            elif cat3 == '-':
                print (efre + str(a-b-c-d))
            elif cat3 == '*':
                print (efre + str(a-b-c*d))
            elif cat3 == '/':
                print (efre + str(a-b-c/d))
        elif cat3 == '*':
            if cat3 == '+':
                print (efre + str(a-b*c+d))
            elif cat3 == '-':
                print (efre + str(a-b*c-d))
            elif cat3 == '*':
                print (efre + str(a-b*c*d))
            elif cat3 == '/':
                print (efre + str(a-b*c/d))
        elif cat3 == '/':
            if cat3 == '+':
                print (efre + str(a-b/c+d))
            elif cat3 == '-':
                print (efre + str(a-b/c-d))
            elif cat3 == '*':
                print (efre + str(a-b/c*d))
            elif cat3 == '/':
                print (efre + str(a-b/c/d))
        else:
            print('invalid input')
    elif cat == '*':
        if cat2 == '+':
            if cat3 == '+':
                print (efre + str(a*b+c+d))
            elif cat3 == '-':
                print (efre + str(a*b+c-d))
            elif cat3 == '*':
                print (efre + str(a*b+c*d))
            elif cat3 == '/':
                print (efre + str(a*b+c/d))
        elif cat2 == '-':
            if cat3 == '+':
                print (efre + str(a*b-c+d))
            elif cat3 == '-':
                print (efre + str(a*b-c-d))
            elif cat3 == '*':
                print (efre + str(a*b-c*d))
            elif cat3 == '/':
                print (efre + str(a*b-c/d))
        elif cat == '*':
            if cat3 == '+':
                print (efre + str(a*b*c+d))
            elif cat3 == '-':
                print (efre + str(a*b*c-d))
            elif cat3 == '*':
                print (efre + str(a*b*c*d))
            elif cat3 == '/':
                print (efre + str(a*b*c/d))
        elif cat2 == '/':
            if cat3 == '+':
                print (efre + str(a*b/c+d))
            elif cat3 == '-':
                print (efre + str(a*b/c-d))
            elif cat3 == '*':
                print (efre + str(a*b/c*d))
            elif cat3 == '/':
                print (efre + str(a*b/c/d))
        else:
            print('invalid input')
    elif cat == '/':
        if cat2 == '+':
            if cat3 == '+':
                print (efre + str(a/b+c+d))
            elif cat3 == '-':
                print (efre + str(a/b+c-d))
            elif cat3 == '*':
                print (efre + str(a/b+c*d))
            elif cat3 == '/':
                print (efre + str(a/b+c/d))
        elif cat2 == '-':
            if cat3 == '+':
                print (efre + str(a/b-c+d))
            elif cat3 == '-':
                print (efre + str(a/b-c-d))
            elif cat3 == '*':
                print (efre + str(a/b-c*d))
            elif cat3 == '/':
                print (efre + str(a/b-c/d))
        elif cat2 == '*':
            if cat3 == '+':
                print (efre + str(a/b*c+d))
            elif cat3 == '-':
                print (efre + str(a/b*c-d))
            elif cat3 == '*':
                print (efre + str(a/b*c*d))
            elif cat3 == '/':
                print (efre + str(a/b*c/d))
        elif cat2 == '/':
            if cat3 == '+':
                print (efre + str(a/b/c+d))
            elif cat3 == '-':
                print (efre + str(a/b/c-d))
            elif cat3 == '*':
                print (efre + str(a/b/c*d))
            elif cat3 == '/':
                print (efre + str(a/b/c/d))
        else:
            print('invalid input')
    else:
        print('invalid input')
else:
    print ("I'm so sorry, this isn't the best calculator it only works with numbers from 1 to 4" + '\n' + 'PS: if you enter a real number it will round down like 3.2 goes to 3 and 1.99 goes to 1')
