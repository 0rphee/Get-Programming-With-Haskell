-- L15 Capstone: Secret Messages!
--  15.1.1 Implementing your own ROT cipher
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded => a, Enum => ee)
rotN size letter = 