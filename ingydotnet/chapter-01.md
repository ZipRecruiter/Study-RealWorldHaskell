1. Enter the following expressions into ghci.
   What are their types?

* `5 + 8`
  ```
  Prelude> :set prompt "> "
  > 5 + 8
  13
  > :type it
  it :: Num a => a
  ```

* `3 * 5 + 8`

* `2 + 4`

* `(+) 2 4`

* `sqrt 16`

* `succ 6`

* `succ 7`

* `pred 9`

* `pred 8`

* `sin (pi / 2)`

* `truncate pi`

* `round 3.5`

* `round 3.4`

* `floor 3.7`

* `ceiling 3.3`

2. From **ghci**, type `:?` to print some help.
   Define a variable, such as `let x = 1`, then type `:show bindings`. What do you see?

3. The words function counts the number of words in a string.
   Modify the `WC.hs` example to count the number of words in a file.

4. Modify the WC.hs example again, to print the number of characters in a file.
