import Data.Text.Lazy (splitOn, find, Text, pack, breakOn, unpack, strip)
import Data.Char (isNumber)
sampleData :: [Char]
sampleData = "5*5\n6+4\n15/3\n7-2"


determineOp :: Text -> Char
determineOp str
    | find (== '+') str == Just '+' = '+'
    | find (== '*') str == Just '*' = '*'
    | find (== '/') str == Just '/' = '/'
    | find (== '-') str == Just '-' = '-'
    | otherwise = '0'

-- EVAL FUNC, DOESNT ACCOUNT FOR INPUT DIFFERENT THAN NUMBERS
-- evalOp :: (Fractional a, Read a) => [Char] -> a
-- evalOp str
--     | op == '+' = n1 + n2
--     | op == '*' = n1 * n2
--     | op == '-' = n1 - n2
--     | op == '/' = n1 / n2
--     | otherwise = 0
--     where text = pack str
--           op = determineOp text
--           (first,rest) = breakOn (pack [op]) text
--           n1 = read $ unpack first
--           n2 = read $ tail (unpack rest)

main :: IO ()
main = do
    userInput <- getContents
    let userLines = lines userInput
    mapM_ (print . evalOp) userLines

evalOp :: (Fractional a, Read a) => [Char] -> a
evalOp str
    | all isNumber n1 && all isNumber n2 =
        case operation of
            '+' -> x + y
            '*' -> x * y
            '-' -> x - y
            '/' -> x / y
            _   -> 0
    | otherwise = 0
    where text = pack str :: Text
          operation = determineOp text :: Char
          (first,rest) = breakOn (pack [operation]) text :: (Text,Text)
          n1 = unpack (strip first) :: String
          n2 = tail (unpack rest) :: String
          x = read n1 -- Final numbers for operation
          y = read n2 -- Final numbers for operation