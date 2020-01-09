### lists & tuples
1. What are the types of the following expressions?
  1. `False` -> `Bool`
  2. `(["foo", "bar"], 'a')` -> `([[Char]], Char)`
  3. `[(True, []), (False, [['a']])]` -> `[(Bool, [[Char]])]`

### chapter 2

1. Haskell provides a standard function, last :: [a] -> a, that returns the last element of a list. From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have? What are a few things that this function clearly cannot do?

Returns the type constrained by its argument list of _type_.  It can't coerce.

2. Write a function lastButOne, that returns the element before the last.

```haskell
lB1 :: [a] -> a
lB1 []  = error "Not enough elements"
lB1 [a] = error "Not enough elements"
lB1 xs  = if lB1R_ xs 10
          then error "Infinite-ish list, won't continue"
          else xs !! ((length xs) - 2)

lB1R_ :: [a] -> Int -> Bool
lB1R_ [] _ = False
lB1R_ xs x = if x > 0
             then lB1R_ (drop 65000 xs) (x - 1)
             else True
```

3. Load your lastButOne function into ghci, and try it out on lists of different lengths. What happens when you pass it a list that's too short?

```
λ| lB1 [1..]
*** Exception: Infinite-ish list
CallStack (from HasCallStack):
  error, called at x.hs:5:16 in main:Main
λ| lB1 []
*** Exception: Not enough elements
CallStack (from HasCallStack):
  error, called at x.hs:2:11 in main:Main
λ| lB1 [1]
*** Exception: Not enough elements
CallStack (from HasCallStack):
  error, called at x.hs:3:11 in main:Main
λ| lB1 [1..5]
4
λ| lB1 [1..50000]
49999
```
