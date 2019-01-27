module Main where
import Secret
import Decipher
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    
parseArgs :: [String] -> IO()
parseArgs []        = usage
parseArgs y@(x:_)   = do
    case x of
         "--help"       -> usage
         "--version"    -> version
         _              -> parseArgs' y
         
parseArgs' :: [String] -> IO()
parseArgs' [y] = do
    let n = read $ take 1 y
    case n of
         1 -> do putStrLn helloString
                 (a, s, b, r)  <- secret1
                 decipher1 a s b r
         2 -> do putStrLn helloString
                 (a, s1, b, s2, c, r)  <- secret2
                 decipher2 a s1 b s2 c r
         3 -> do putStrLn helloString
                 (a, s1, b, s2, c, s3, d, r)  <- secret3
                 decipher3 a s1 b s2 c s3 d r
         _ -> putStrLn "Неверное число, необходимое число от 1 до 3"
parseArgs' _ = usage

helloString :: String
helloString =   "Игра \"Волшебные числа\"\n"                                                                    ++
                "Загадано выражение. Нужно угадать какой знак (+, - или *) стоит вместо знака(ов) вопроса.\n"   ++
                "Знаки нужно указать в той последовательности, в которой они стоят в выражении, без пробелов."

usage :: IO()
usage = putStrLn usageStr

usageStr :: String
usageStr =  "Игра \"Волшебные числа\"\n"                                        ++
            "Использование:\n"                                                  ++
            "   MagicNumbers N \n"                                              ++
            "Здесь\n"                                                           ++
            "   N - число знаков, которые нужно будет угадать (от 1 до 3).\n"   ++
            "Загадано выражение. Нужно угадать какой знак (+, - или *) стоит вместо знака(ов) вопроса.\n"   ++
            "Знаки нужно указать в той последовательности, в которой они стоят в выражении, без пробелов."  


version :: IO()
version = putStrLn versionStr

versionStr :: String
versionStr =    "version 1.3  Щурупов Иван (с) 2018"
