1. Enter the following expressions into ghci.
   What are their types?

* `5 + 8`
  ```
  Prelude> :set prompt "> "
  > 5 + 8
  13
  > :type 5 + 8
  5 + 8 :: Num a => a
  ```

* `3 * 5 + 8`
  ```
  > 3 * 5 + 8
  23
  > :t 3 * 5 + 8
  3 * 5 + 8 :: Num a => a
  ```

* `2 + 4`
  ```
  > 2 + 4
  6
  > :t 2 + 4
  2 + 4 :: Num a => a
  ```

* `(+) 2 4`
  ```
  > (+) 2 4
  6
  > :t (+) 2 4
  (+) 2 4 :: Num a => a
  ```

* `sqrt 16`
  ```
  > sqrt 16
  4.0
  > :t sqrt 16
  sqrt 16 :: Floating a => a
  ```

* `succ 6`
  ```
  > succ 6
  7
  > :t succ 6
  succ 6 :: (Num a, Enum a) => a
  ```

* `succ 7`
  ```
  > succ 7
  8
  > :t succ 7
  succ 7 :: (Num a, Enum a) => a
  ```

* `pred 9`
  ```
  > pred 9
  8
  > :t pred 9
  pred 9 :: (Num a, Enum a) => a
  ```

* `pred 8`
  ```
  > pred 8
  7
  > :t pred 8
  pred 8 :: (Num a, Enum a) => a
  ```

* `sin (pi / 2)`
  ```
  > sin (pi / 2)
  1.0
  > :t sin (pi / 2)
  sin (pi / 2) :: Floating a => a
  ```

* `truncate pi`
  ```
  > truncate pi
  3
  > :t truncate pi
  truncate pi :: Integral b => b
  ```

* `round 3.5`
  ```
  > round 3.5
  4
  > :t round 3.5
  round 3.5 :: Integral b => b
  ```

  ```
  > :t round
  round :: (Integral b, RealFrac a) => a -> b
  ```

* `round 3.4`
  ```
  > round 3.4
  3
  > :t round 3.4
  round 3.4 :: Integral b => b
  ```

* `floor 3.7`
  ```
  > floor 3.7
  3
  > :t floor 3.7
  floor 3.7 :: Integral b => b
  ```

* `ceiling 3.3`
  ```
  > ceiling 3.3
  4
  > :t ceiling 3.3
  ceiling 3.3 :: Integral b => b
  ```

2. From **ghci**, type `:?` to print some help.
   Define a variable, such as `let x = 1`, then type `:show bindings`. What do you see?
   ```
   > let x = 1
   > :show bindings
   it :: Integral b => b = _
   x :: Num t => t = _
   ```

3. The words function counts the number of words in a string.
   Modify the `WC.hs` example to count the number of words in a file.
   ```
   main = interact wordCount
       where wordCount input = show (length (words input)) ++ "\n"
   ```

4. Modify the WC.hs example again, to print the number of characters in a file.
   ```
   main = interact wordCount
      where wordCount input = show (length input) ++ "\n"
   ```
