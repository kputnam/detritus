import System.Random

-- | http://www.cs.utexas.edu/~moore/best-ideas/mjrty/
mjrty :: Eq a => [a] -> Maybe a
mjrty []     = Nothing
mjrty (x:xs) = loop 1 x xs
  where
    loop n m []
      | n > 0       = Just m
      | otherwise   = Nothing
    loop 0 _ (x:xs) = loop 1 x xs
    loop n m (x:xs)
      | m == x      = loop (n+1) m xs
      | otherwise   = loop (n-1) m xs

-- | http://link.springer.com/content/pdf/10.1007%2F978-3-642-40273-9_7.pdf
median :: (Ord a, Num a) => [a] -> a
median = loop 0
  where
    loop e []     = e
    loop e (x:xs)
      | e < x     = loop (e+1) xs
      | e > x     = loop (e-1) xs
      | otherwise = loop e xs

-- | http://link.springer.com/content/pdf/10.1007%2F978-3-642-40273-9_7.pdf
quartiles :: (Ord a, Num a) => [a] -> IO StdGen -> IO a
quartiles xs r = loop 0 xs `fmap` (randoms `fmap` r :: IO [Double])
  where
    loop e [] _ = e
    loop e (x:xs) (r:rs)
      | e < x && r > 0.25 = loop (e+1) xs rs
      | e > x && r > 0.75 = loop (e-1) xs rs
      | otherwise         = loop e xs rs
