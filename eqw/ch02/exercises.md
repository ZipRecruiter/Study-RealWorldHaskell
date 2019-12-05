Chapter 2 Exercises
===================

## Set One

1. What are the types of the following expressions?

```hs
False
-- Bool

(["foo", "bar"], 'a')
-- ([[Char]], Char)

[(True, []), (False, [['a']])]
-- [(Bool, [[Char]])]
```

## Set Two

1. Haskell provides a standard function, `last :: [a] -> a`, that returns the last element of a list. From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have? What are a few things that this function clearly cannot do?

Possible valid behaviors include returning the final element of a list of length > 1, returning the sole element of a list of a single element, and (despite its name) returning the first (or any other) element from the list (since the type only guarantees that, given a list of `a`, something of type `a` comes back out). It might even return a random value of type `a` that isn't in the list at all! This function clearly cannot return a value of type other than `a`, which I imagine means it cannot be applied to an empty list (edit: confirmed).

2. Write a function `lastButOne`, that returns the element before the last.

```hs
lastButOne :: [a] -> a
lastButOne xs = xs!!(length xs - 2)
```

3. Load your `lastButOne` function into `ghci`, and try it out on lists of different lengths. What happens when you pass it a list that's too short?

I get `*** Exception: Prelude.!!: negative index`, since the length of the list minus two is a negative number.
