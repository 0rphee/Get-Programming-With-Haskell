-- L15 Capstone: Secret Messages!
--  15.1.1 Implementing your own ROT cipher
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size letter = toEnum rotation
    where halfAlphabet = size `div` 2
          offset = fromEnum letter + halfAlphabet
          rotation = offset `mod` size

rotChar :: Char -> Char
rotChar = rotN size
    where size = 1 + fromEnum (maxBound :: Char)

rotEncoder :: [Char] -> [Char]
rotEncoder = map rotChar

-- 15.1.4 The problem with decoded odd-sized alphabets
data ThreeLetterAplhabet = Alpha
                         | Beta
                         | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage = [Alpha, Beta, Kappa]

threeLetterEncoder :: [ThreeLetterAplhabet] -> [ThreeLetterAplhabet]
threeLetterEncoder = map rot3l
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAplhabet)
          rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAplhabet] -> [ThreeLetterAplhabet]
threeLetterDecoder = map rot3ldecoder
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAplhabet)
          rot3ldecoder = rotNdecoder alphaSize

-- 15.10 Rotating strings with rotEncoder and rotDecoder
rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
    where alphasize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphasize

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && not (v1 && v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)


data Boolys = Falsy | Truthy deriving (Eq,Ord,Enum,Bounded,Read)
instance Show Boolys where
    show Truthy = "1"
    show Falsy  = "0"

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = mod n 2
          nextVal   = div n 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - length reversedBits
          leadingFalses = take missingBits (repeat False)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map(\x -> 2^snd x) trueLocations)
    where size = length bits
          indices = [size-1, size-2 .. 0]
          trueLocations = filter fst (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits

charToBits :: Char -> Bits
charToBits char = intToBits $ fromEnum char

stringToBits :: [Char] -> [Bits]
stringToBits = map charToBits

bitsToString :: [Bits] -> [Char]
bitsToString = map bitsToChar


myPad :: [Char]
myPad    = "poeiru1'39839ccmnñl{k mz-{Q-1GKDF{A1-pfh2i8|'}]"

original :: [Char]
original = "Haskell is the best language ever to exist in t"

xorBitString :: [Bits] -> [Bits] -> [Bits]
xorBitString = zipWith xor

applyOTP :: [Char] -> [Char] -> [Char]
applyOTP pad original = bitsToString bitXORresult
    where bitPad = stringToBits pad
          bitOriginal = stringToBits original
          bitXORresult = xorBitString bitPad bitOriginal

-- L15.5 A Cipher class
class Cipher a where
    encode :: a -> String -> String 
    decode :: a -> String -> String 

-- L15.27 making Rot an instance of Cipher
data Rot = Rot
instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

-- L15.29 Making OneTimePad an instance of Cipher
data OneTimePad = OTP String
instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

--  L15.30 Using lazy evaluation to creaate a limitless pad
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])