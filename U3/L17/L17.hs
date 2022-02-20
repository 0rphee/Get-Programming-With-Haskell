-- Lesson 17: Design by Composition, Semigroups and Monoids
-- 17.1 Implement myAny by using function composition. my Any test that a property es True for at least one value in the list
myAny :: (a -> Bool) -> [a] -> Bool
myAny testfunc = (foldr (||) False) . (map testfunc)

funcy  = (==) 'a'

-- 17.2.1 The Color Semigroup
data Color = Red |
             Yellow |
             Blue |
             Green |
             Purple |
             Orange |
             Brown |
             White deriving (Show, Eq)
instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a White = a
    (<>) White a = a
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Purple
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange 
             | otherwise = Brown

instance Monoid Color where
    mempty = White
    mappend = (<>)

-- 17.3.3 Practical Monoids--Building probability tables
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs
instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|",show prob,"\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
    where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]
