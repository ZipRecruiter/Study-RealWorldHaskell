# Chapter 5 exercises

Our current pretty printer is spartan, so that it will fit within our space constraints, but there are a number of useful improvements we can make.

1. Write a function, `fill`, with the following type signature.

   ```
   -- file: ch05/Prettify.hs
   fill :: Int -> Doc -> Doc
   ```

   It should add spaces to a document until it is the given number of columns wide.
   If it is already wider than this value, it should add no spaces.

2. Our pretty printer does not take nesting into account.
   Whenever we open parentheses, braces, or brackets, any lines that follow should be indented so that they are aligned with the opening character until a matching closing character is encountered.

   Add support for nesting, with a controllable amount of indentation.

   ```
   -- file: ch05/Prettify.hs
   nest :: Int -> Doc -> Doc
   ```
