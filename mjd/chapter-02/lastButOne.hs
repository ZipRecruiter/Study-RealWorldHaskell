
lastButOne [_] = error "YOUR LIST IS TOO SHORT"
lastButOne [] = error "YOUR LIST IS EMPTY"
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs
