type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
    where   costP1 = costPerInch p1
            costP2 = costPerInch p2

assembleTxt :: Pizza -> String
assembleTxt (size,cost) = "The "++sizeStr++"-inch pizza is cheaper at "
                                 ++costInchStr++" per square inch"
    where sizeStr = show size
          costInchStr = show $ costPerInch (size,cost)

getFractional :: IO Double
getFractional = do
    num <- getLine
    return (read num)

askPizzaValues :: (Num a,Show a) => a -> IO Pizza
askPizzaValues pizzaNum = do
    putStrLn ("What is the size of the pizza " ++ show pizzaNum)
    size <- getFractional
    putStrLn ("What is the cost of the pizza " ++ show pizzaNum)
    cost <- getFractional
    return (size, cost)

main :: IO ()
main = do
    p1 <- askPizzaValues 1 
    p2 <- askPizzaValues 2
    let finalPizza = comparePizzas p1 p2
    putStrLn $ assembleTxt finalPizza