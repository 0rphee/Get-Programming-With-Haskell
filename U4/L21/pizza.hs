pizzaCost :: Fractional a => a -> a -> a
pizzaCost size cost = size / cost

assembleTxt :: (Fractional a, Show a) => a -> a -> String
assembleTxt size costInch = "The "++sizeStr++" pizza is cheaper at "++costInchStr++" per square inch"
    where sizeStr = show size
          costInchStr = show costInch

getFractional :: IO a 
getFractional = do
    num <- getLine 
    return (read num)

askPizzaValues :: IO (a, a)
askPizzaValues = do
    putStrLn "What is the size of the pizza"
    size <- getFractional
    putStrLn "What is the cost of the pizza"
    cost <- getFractional
    return (size, cost)


main :: IO ()
main = do
    pair <-  askPizzaValues
    let size = fst pair
    let cost = snd pair 
    let costInch = pizzaCost size cost
    let finalTxt = assembleTxt size costInch
    putStrLn finalTxt
