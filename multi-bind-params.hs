objective :: String
objective = "show a variable of monadic bindings withing a do block, use for attached a known number of arguments to the frame"

data ArgTypes = A | B | C deriving (Show)

stringArgs :: ArgTypes -> String
stringArgs x = show x

data Args = Args { numArgs :: Int, 
  argList :: [ArgTypes] } deriving (Show)

createArgs :: [ArgTypes] -> Args
createArgs args = Args (length args) args

showPosArg :: Args -> Int -> IO ()
showPosArg args n = putStrLn $ show $ (argList args) !! n

doGuessing args = do
  putStrLn "Enter a response for each letter: "
  case (numArgs args) of
    1 -> do showPosArg args 0
            arg1 <- getLine 
            putStrLn $ "you entered " ++ arg1 

    2 -> do showPosArg args 0
            arg1 <- getLine
            showPosArg args 1
            arg2 <- getLine
            putStrLn $ "you entered " ++ arg1 ++ " plus " ++arg2

    3 -> do showPosArg args 0
            arg1 <- getLine
            showPosArg args 1
            arg2 <- getLine
            showPosArg args 2
            arg3 <- getLine
            putStrLn $ "you entered " ++ arg1 ++ " plus " ++ arg2 ++ " plus " ++ arg3

arg1 :: Args
arg1 = createArgs [A]

arg2 :: Args
arg2 = createArgs [B,C]

arg3 :: Args
arg3 = createArgs [A,B,C]

main = doGuessing arg3


