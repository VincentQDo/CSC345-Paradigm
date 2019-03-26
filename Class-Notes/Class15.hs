-- how to take input from user and modify it
-- where is used to introduce variable at a function level
-- let is smaller than that
-- before we import random we need ot import unsafe
--let statement if you are just in ghci you can make a varialbe by using
-- >let {assignment} in {expresion}
-- >let x = 5 in x + 3
-- >8
-- do     statements
--        ...
--        let var = expr
--        let x = 5
--        statements
-- the let will only have scope for any statements after itself in the do sequence
--show :: Show a => a -> String --function type of show is defined in Show the =>
--will symbolize that Show is a class and a is a member of Show class

--the function read is defined in the class Read, the type of read is take
--in a string and return it as type a member of Read
-- "Read a" means a is a member of Read

-- read :: Read a => String -> a
--  > read "test" :: Int

--we will implement getInt on our own
getInt :: IO Int
getInt = do line <- getLine
            return (read line::Int)
strlen :: IO () --perform IO action but do Nothing
strlen = do putStr "Enter a string "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

copy :: IO () --will perform IO action but not return anything, that's what OP () mean
copy = do line <- getLine
          putStr line
          putStrLn ""
          copy --special property called "tail recursion", called tail recursive because the last thing a function does is a recursive call

copyN :: Integer -> IO ()
copyN n
    | n == 0     = return () --this line will return nothing
    | otherwise = do line <- getLine
                     putStrLn line
                     copyN (n-1)
