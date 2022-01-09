-- Lesson 6
-- 6.3.1 The !! Operator
listy = [[1, 2, 3, 4],
         [5, 6, 7, 8]]

myfunc :: [[a]] -> Int -> Int -> a
myfunc list fstIndex sndIndex = (list !! fstIndex) !! sndIndex