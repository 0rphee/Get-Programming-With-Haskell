{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

tRead :: Read a => T.Text -> a
tRead = read . T.unpack

toInts :: T.Text  -> [Int]
toInts = map tRead . T.lines

main :: IO ()
main = do
    userInput <- TIO.getContents 
    let numbers = toInts userInput
    print (sum numbers)