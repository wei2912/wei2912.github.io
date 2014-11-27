pows :: Integer -> [Integer]
pows pow = iterate (pow *) 1
 
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
 
main :: IO ()
main = do
    print $ take 5 (pows 3) -- prints out the first 5 powers of 3
    print $ takeWhile (< 100) (pows 2) -- prints out the powers of 2 < 100
    print $ take 10 fibs -- prints out the first 10 Fibonacci numbers
