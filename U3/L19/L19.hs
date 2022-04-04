import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Listing 19.2: List of possibleDrawers in your organCatalog
possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int 
countOrgan organ available = length (filter (\x -> x == Just organ) available)

isSomething :: Maybe Organ -> Bool 
isSomething Nothing = False 
isSomething (Just _) = True 

justOrgans :: [Maybe Organ]
justOrgans = filter isSomething availableOrgans

getOrgan :: Maybe Organ -> Organ
getOrgan (Just organ) = organ

finOrgans = map getOrgan justOrgans

-- 19.4 Back to the lab! Morecomplex computation with Maybe
data Container = Vat Organ | Cooler Organ | Bag Organ
instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> [Char]
report (location, container) = show container ++ " in the " ++ show location

processAndReport :: Maybe Organ -> [Char]
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog