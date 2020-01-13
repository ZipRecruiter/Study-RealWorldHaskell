
Suppose I have

   f :: Int -> Int -> Int

and I want to turn it into

   lift f :: (Either Int String) -> (Either Int String) -> (Either Int String)

             
