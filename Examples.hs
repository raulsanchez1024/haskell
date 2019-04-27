myadd (x, y) = x + y
factorial n = if n == 0 then 1 else n * factorial (n - 1)

fact 0 = 1
fact n = n * fact (n - 1)

doubleMe x = x + x

multiplyByFour x = x * 4


doubleSmallNumber x = if x > 100 then x else x * 2
