-- Lesson 8: Writing Recursive Functions

-- 1 Remove n elemnts from list
-- 2 Stop reomving elements from list
myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop n list = if n == 1
                then xs
                else myDrop (n-1) xs
    where (_:xs) = list
          
-- 8.2 Recursion on lists
-- 8.2.1 Implementing lenght
-- Quick check 8.1 -> use pattern mathcing without calling tail
myLength2 []     = 0
myLength2 (_:xs) = 1 + myLength2 xs

-- 8.2.2 Implementing take
-- This is taken from my work on L7
myTake2 :: Int -> [a] -> [a]
myTake2 n list = if n == length list
                 then list
                 else myTake2 n (init list)
-- This is taken from the book example
hisTake _ []     = []
hisTake 0 _      = []
hisTake n (x:xs) = x:rest
    where rest = hisTake (n-1) xs

-- 8.2.3 Implementing cycle
