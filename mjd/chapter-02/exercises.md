
## Useful composite data types: lists and tuples - Exercises

1. What are the types of the following expressions?
  1. `False`

  `Bool`
  
  2. `(["foo", "bar"], 'a')`

  ([[Char]], Char)
  
  3. `[(True, []), (False, [['a']])]`

  [(Bool, [[Char]])]

## Chapter 2 - Exercises

1. Haskell provides a standard function, `last :: [a] -> a`, that returns the last element of a list.
   From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have?
   What are a few things that this function clearly cannot do?

It could:
* Return some element of the list

Can't:
* Return the length of the list
* Return a value of some other type
* Return some value derived from the list elements
* ... some other value of the same type


2. Write a function lastButOne, that returns the element before the last.

   lastButOne [x,y] = x
   lastButOne (x:xs) = lastButOne xs

3. Load your lastButOne function into ghci, and try it out on lists of different lengths.
   What happens when you pass it a list that's too short?
