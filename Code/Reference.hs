module Reference where



import Data.List (inits)



scProd :: Num a => [a] -> [a] -> a
scProd a b = sum $ zipWith (*) a b

fir :: Num a => [a] -> [a] -> [a]
fir bs inp = [scProd bs is | is <- map reverse $ tail $ inits inp]

iir :: Fractional a => [a] -> [a] -> [a] -> [a]
iir (a0:as) bs inp = outp
  where
    inps  = map reverse $ tail $ inits inp
    outps = map reverse $ tail $ inits (0:outp)
    outp  = [(scProd bs is - scProd as os) / a0 | (is,os) <- zip inps outps]

aCoeffs ord = [0.5/(-3)^i | i <- [0..ord]]
bCoeffs ord = [1.1/(-2)^i | i <- [0..ord]]

inp n = map (fromInteger . (+(-5)) . (`mod` 10)) [0..n-1]

testFir = sum $ fir (bCoeffs 7) (inp 1000)

