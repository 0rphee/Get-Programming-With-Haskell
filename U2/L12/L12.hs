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
            deriving Show

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = concat [name, " ", ageHeight]
    where name = concat [lname, ", ", fname]
          ageHeight = concat ["(", show age, "yrs. ", show height, "in.)"]

-- 12.2 Creating new types
data Sex = Female | Male deriving Show

sexInitial :: Sex -> Char 
sexInitial Male   = 'M'
sexInitial Female = 'F'

-- 12.6 Defining the type RhType
data RhType = Pos | Neg deriving (Eq)
instance Show RhType where
    show Pos = "+"
    show Neg = "-"

-- 12.7 Defining the type ABOType
data ABOType = A | B | AB | O deriving (Show, Eq)
-- instance Eq ABOType where
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

-- Listing 12.12 Patient v.1
data Patient = Patient Name Sex Int Int Int BloodType deriving Show

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWMiddle "Jane" "Elizabeth" "Smith") Female 28 62 140 (BloodType O Neg)

getName :: Patient -> Name
getName (Patient n  _ _ _ _ _) = n

-- getAge, getBloodType, etc

-- Listing 12.14 Patient v.2 with record syntax

data Patient2 = Patient2 {name :: Name,
                          sex :: Sex,
                          age :: Int,
                          height :: Int,
                          weight :: Int,
                          bloodtype :: BloodType} 
                          deriving Show


peter = Patient2 (Name "Peter" "Robertson")  Male 21 12 32 (BloodType B Neg)
steven = Patient2 (Name "Peter" "Robertson")  Male 21 12 32 (BloodType B Pos)
esther = Patient2 (Name "Peter" "Robertson")  Male 21 12 32 (BloodType O Pos)

canDonate :: Patient2 -> Patient2 -> Bool
canDonate (Patient2 _ _ _ _ _ (BloodType O _)) _  = True
canDonate _ (Patient2 _ _ _ _ _ (BloodType AB _)) = True 
canDonate (Patient2 _ _ _ _ _ (BloodType A _)) (Patient2 _ _ _ _ _ (BloodType A _)) = True
canDonate (Patient2 _ _ _ _ _ (BloodType B _)) (Patient2 _ _ _ _ _ (BloodType B _)) = True 
canDonate _ _ = False
