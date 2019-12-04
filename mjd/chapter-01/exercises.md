1. Enter the following expressions into ghci.
   What are their types?

* `5 + 8`

Num a => a


* `3 * 5 + 8` 

Num a => a

* `2 + 4`

Num a => a

* `(+) 2 4`

Num a => a

* `sqrt 16`

Real a => a

   Actually Floating

* `succ 6`

Enum a => a

   Actually (Num a, Enum a) => a

* `succ 7`

Enum a => a

   Actually (Num a, Enum a) => a

* `pred 9`

Enum a => a

   Actually (Num a, Enum a) => a

* `pred 8`

Enum a => a

   Actually (Num a, Enum a) => a

* `sin (pi / 2)`

Real a => a

   Actually Floating

* `truncate pi`

Integral a => a

* `round 3.5`

Integral a => a

* `round 3.4`

Integral a => a

* `floor 3.7`

Integral a => a

* `ceiling 3.3`

Integral a => a

2. From **ghci**, type `:?` to print some help.
   Define a variable, such as `let x = 1`, then type `:show bindings`. What do you see?

3. The words function counts the number of words in a string.
   Modify the `WC.hs` example to count the number of words in a file.

4. Modify the WC.hs example again, to print the number of characters in a file.
