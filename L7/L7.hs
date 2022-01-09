-- Lesson 7: Rules for recursion and Pattern Matching
-- My implementation(s) of the built-in |take| with recursion
myTakeCrude :: Int -> [a] -> [a] -> [a]
myTakeCrude n list workingList = final
    where addition    = [list !! n]
          tempWrkList = workingList ++ addition
          final       = if n == 0
                        then reverse tempWrkList
                        else myTakeCrude (n-1) list tempWrkList

myTakeClean :: Int -> [a] -> [a]
myTakeClean a b = myTakeCrude (a-1) b []

-- Way simpler, way better
myTake2 :: Int -> [a] -> [a]
myTake2 n list = final
    where final = if n == length list
                  then list
                  else myTake2 n (init list)

-- 7.3 Your first recursive function: greatest common divisor
mygcd a b = if remnd == 0
            then b
            else mygcd b remnd
    where remnd = mod a b

-- Another version of the greatest common divisor with guards
myGCD :: Int -> Int -> Int 
myGCD a b
    | rmnd == 0 = b
    | otherwise = myGCD b rmnd
    where rmnd  = mod a b

-- Quick check 7.3 
myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail _      = []