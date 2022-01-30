-- Lesson  12: Creating your own types
-- 12.1 Using type syonyms
type FirstName = String 
type LastName  = String 
type Age       = Int 
type Height    = Int 

-- 12.10 Suppoert different names
type MiddleName = String 
data Name = Name FirstName LastName
          | NameWMiddle FirstName MiddleName LastName

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = concat [name, " ", ageHeight]
    where name = concat [lname, ", ", fname]
          ageHeight = concat ["(", show age, "yrs. ", show height, "in.)"]

-- 12.2 Creating new types
data Sex = Female | Male

sexInitial :: Sex -> Char 
sexInitial Male   = 'M'
sexInitial Female = 'F'

-- 12.6 Defining the type RhType
data RhType = Pos | Neg deriving (Show, Eq)
-- 12.7 Defining the type ABOType
data ABOType = A | B | AB | O deriving (Show, Eq)
-- nstance Eq ABOType where
--   (==) A A    = True 
--   (==) A AB   = True 
--   (==) B B    = True 
--   (==) B AB   = True
--   (==) AB AB  = True 
--   (==) O _    = True
--   (==) _ _    = False 

data BloodType = BloodType ABOType RhType deriving (Show, Eq)

canDonateTo :: BloodType -> BloodType -> Bool 
canDonateTo (BloodType O _) _               = True 
canDonateTo _ (BloodType AB _)              = True 
canDonateTo (BloodType A _) (BloodType A _) = True 
canDonateTo (BloodType B _) (BloodType B _) = True 
canDonateTo _ _ = False 