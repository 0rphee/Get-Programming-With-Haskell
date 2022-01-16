-- Lesson 9: Higher order functions
import Data.Char (toLower)

operateOnList :: (t -> a) -> [t] -> [a]
operateOnList _  []     = []
operateOnList op (x:xs) = op x:operateOnList op xs

-- QC 9.1 implement remove
myRemove _ []     = []
myRemove f (x:xs) = if f x
                    then myRemove f xs
                    else x:myRemove f xs

-- QC 9.2 implement product of list (factorial function)
myProduct :: Num a => [a] -> a
myProduct []  = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

-- Q 9.1 myElem with filter and len
myElem elem list = length (filter (elem==) list) > 0

-- Q 9.2 palindrome with filter and map, test: "A man a plan a canal Panama"
myPalindrome str = fstring == reverse fstring
    where fstring = map toLower (filter (/=' ') str)

-- Q 9.3 harmonic series 1/1 + 1/2 ...
harmonic n = sum series
    where series =  map (1 /) [1..n]
