for i in range(9999):
    if i%5 == 0 and i% 3 == 0:
        print ('fizzbuzz')
    elif i%5 == 0 and i%3 != 0:
        print ('buzz')
    elif i%5 != 0 and i%3 == 0:
        print ('fizz')
    else:
        print (i)
    
