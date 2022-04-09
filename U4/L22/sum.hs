import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args
