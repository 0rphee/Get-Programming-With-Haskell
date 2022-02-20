import qualified Data.Map as Map

data Triple a = Triple a a a deriving Show
type Point3d = Triple Double 

point1 :: Point3d
point1 = Triple 0.1 53.2 12.3

--instance Num a => Num (Triple a) where
--    (+) (Triple x1 y1 z1) (Triple x2 y2 z2) = Triple (x1+x2) (y1+y2) (z1+z2) 
--    (-) (Triple x1 y1 z1) (Triple x2 y2 z2) = Triple (x1-x2) (y1-y2) (z1-z2) 
--    abs (Triple x y z) = (Triple (abs x) (abs y) (abs z))

data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq)
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]
ids =[2,7,13,14,21,24]

organPairs = zip ids organs
organCatalog = Map.fromList organPairs
