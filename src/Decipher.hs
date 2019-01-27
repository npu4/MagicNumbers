module Decipher(
  decipher1,
  decipher2,
  decipher3
)where

decipher1 :: Int -> String -> Int -> Int ->IO()
decipher1 a s b r = do
  putStrLn $ show a ++ "?" ++ show b ++ "=" ++ show r
  try <- getLine
  if try == s then
    putStrLn $ "Верно! Это был пример:\n" ++ show a ++ s ++ show b ++ "=" ++ show r
  else do
    putStrLn $ "Неверно, попробуй ещё раз.\n"
    decipher1 a s b r
    
    
decipher2 :: Int -> String -> Int -> String -> Int -> Int ->IO()
decipher2 a s1 b s2 с r = do
  putStrLn $ show a ++ "?" ++ show b ++ "?" ++ show с ++ "=" ++ show r
  try <- getLine
  let t1 = take 1 try
      t2 = take 1 . drop 1 $ try
  if (t1 == s1)&&(t2==s2) then
    putStrLn $ "Верно! Это был пример:\n" ++ show a ++ s1 ++ show b ++ s2 ++ show с ++ "=" ++ show r
  else do
    putStrLn $ "Неверно, попробуй ещё раз.\n"
    decipher2 a s1 b s2 с r
    
    
decipher3 :: Int -> String -> Int -> String -> Int -> String -> Int -> Int ->IO()
decipher3 a s1 b s2 с s3 d r = do
  putStrLn $ show a ++ "?" ++ show b ++ "?" ++ show с ++ "?" ++ show d ++ "=" ++ show r
  try <- getLine
  let t1 = take 1 try
      t2 = take 1 . drop 1 $ try
      t3 = take 1 . drop 2 $ try
  if (t1 == s1)&&(t2==s2)&&(t3==s3) then
    putStrLn $ "Верно! Это был пример:\n" ++ show a ++ s1 ++ show b ++ s2 ++ show с ++ s3 ++ show d ++ "=" ++ show r
  else do
    putStrLn $ "Неверно, попробуй ещё раз.\n"
    decipher3 a s1 b s2 с s3 d r
