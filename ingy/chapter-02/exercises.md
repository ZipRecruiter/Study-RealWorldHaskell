
## Useful composite data types: lists and tuples - Exercises

1. What are the types of the following expressions?
  1. `False`
     * `False :: Bool`
  2. `(["foo", "bar"], 'a')`
     * `(["foo", "bar"], 'a') :: ([[Char]], Char)`
  3. `[(True, []), (False, [['a']])]`
     * `[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]`

## Chapter 2 - Exercises

1. Haskell provides a standard function, `last :: [a] -> a`, that returns the last element of a list.
   From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have?
   * Return value must be an actual element in the list
   What are a few things that this function clearly cannot do?
   * Take an empty list as an argument
   * Return something not in the list
   * Return a list
   * Not return anything
2. Write a function lastButOne, that returns the element before the last.
   ```
   lastButOne xs
     | len == 0 =
       error "List is empty"
     | len == 1 =
       error "List only has 1 item"
     | otherwise =
       head $ drop (len - 2) xs
     where
       len = length xs
   ```
3. Load your lastButOne function into ghci, and try it out on lists of different lengths.
   What happens when you pass it a list that's too short?
   * Handles lists that are too small with appropriate error messages
