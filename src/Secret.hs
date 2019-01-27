module Secret(
    secret1,
    secret2,
    secret3
)where
import System.Random

secret1 :: IO(Int,String,Int,Int)
secret1 = do
    a <- randomElement [0..100]
    b <- randomElement [0..100]
    numOfSign <- randomElement [0,2]
    case numOfSign of
         0 -> return (a, "+", b, a+b)
         1 -> return (a, "-", b, a-b)
         2 -> return (a, "*", b, a*b)
         
         
secret2 :: IO(Int,String,Int,String,Int,Int)
secret2 = do
    a <- randomElement [0..100]
    b <- randomElement [0..100]
    c <- randomElement [0..100]
    numOfSign1 <- randomElement [0,2]
    numOfSign2 <- randomElement [0,2]
    case numOfSign1 of
         0 -> case numOfSign2 of
                    0 -> return (a, "+", b, "+", c, a+b+c)
                    1 -> return (a, "+", b, "-", c, a+b-c)
                    2 -> return (a, "+", b, "*", c, a+(b*c))
         1 -> case numOfSign2 of
                    0 -> return (a, "-", b, "+", c, a-b+c)
                    1 -> return (a, "-", b, "-", c, a-b-c)
                    2 -> return (a, "-", b, "*", c, a-(b*c))
         2 -> case numOfSign2 of
                    0 -> return (a, "*", b, "+", c, (a*b)+c)
                    1 -> return (a, "*", b, "-", c, (a*b)-c)
                    2 -> return (a, "*", b, "*", c, (a*b)*c)
                    
                    
secret3 :: IO(Int,String,Int,String,Int,String,Int,Int)
secret3 = do
    a <- randomElement [0..100]
    b <- randomElement [0..100]
    c <- randomElement [0..100]
    d <- randomElement [0..100]
    numOfSign1 <- randomElement [0,2]
    numOfSign2 <- randomElement [0,2]
    numOfSign3 <- randomElement [0,2]
    case numOfSign1 of
         0 -> case numOfSign2 of
                    0 -> case numOfSign3 of
                                0 -> return (a, "+", b, "+", c, "+", d, a+b+c+d)
                                1 -> return (a, "+", b, "+", c, "-", d, a+b+c-d)
                                2 -> return (a, "+", b, "+", c, "*", d, a+b+(c*d))
                    1 -> case numOfSign3 of
                                0 -> return (a, "+", b, "-", c, "+", d, a+b-c+d)
                                1 -> return (a, "+", b, "-", c, "-", d, a+b-c-d)
                                2 -> return (a, "+", b, "-", c, "*", d, a+b-(c*d))
                    2 -> case numOfSign3 of
                                0 -> return (a, "+", b, "*", c, "+", d, a+(b*c)+d)
                                1 -> return (a, "+", b, "*", c, "-", d, a+(b*c)-d)
                                2 -> return (a, "+", b, "*", c, "*", d, a+(b*(c*d)))
         1 -> case numOfSign2 of
                    0 -> case numOfSign3 of
                                0 -> return (a, "-", b, "+", c, "+", d, a-b+c+d)
                                1 -> return (a, "-", b, "+", c, "-", d, a-b+c-d)
                                2 -> return (a, "-", b, "+", c, "*", d, a-b+(c*d))
                    1 -> case numOfSign3 of
                                0 -> return (a, "-", b, "-", c, "+", d, a-b-c+d)
                                1 -> return (a, "-", b, "-", c, "-", d, a-b-c-d)
                                2 -> return (a, "-", b, "-", c, "*", d, a-b-(c*d))
                    2 -> case numOfSign3 of
                                0 -> return (a, "-", b, "*", c, "+", d, a-(b*c)+d)
                                1 -> return (a, "-", b, "*", c, "-", d, a-(b*c)-d)
                                2 -> return (a, "-", b, "*", c, "*", d, a-(b*(c*d)))
         2 -> case numOfSign2 of
                    0 -> case numOfSign3 of
                                0 -> return (a, "*", b, "+", c, "+", d, a*b+c+d)
                                1 -> return (a, "*", b, "+", c, "-", d, a*b+c-d)
                                2 -> return (a, "*", b, "+", c, "*", d, (a*b)+(c*d))
                    1 -> case numOfSign3 of
                                0 -> return (a, "*", b, "-", c, "+", d, a*b-c+d)
                                1 -> return (a, "*", b, "-", c, "-", d, a*b-c-d)
                                2 -> return (a, "*", b, "-", c, "*", d, a*b-(c*d))
                    2 -> case numOfSign3 of
                                0 -> return (a, "*", b, "*", c, "+", d, a*b*c+d)
                                1 -> return (a, "*", b, "*", c, "-", d, a*b*c-d)
                                2 -> return (a, "*", b, "*", c, "*", d, a*b*c*d)
         
         
         
randomElement :: [a] -> IO a
randomElement xs = do
    i<- randomRIO (0, length xs - 1)
    return $ xs !! i
