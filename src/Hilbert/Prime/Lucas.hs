module Hilbert.Prime.Lucas (lucas) where

import Data.Maybe (fromJust)
import Data.List (find)

import Hilbert.Legendre (jacobi)
import Hilbert.Square (isSquare)
import Hilbert.Lucas (lucasu, path, collapseMap')


-- Lucas probabilistic prime test
-- precondition :: n >= 3
lucas :: (Integral a) => a -> Bool
lucas n = n == 2 || precondition && condition
   where d = findd n
         p = 1
         q = (1 - d) `div` 4
         (o, e) = factorOut del
         del = n - (jacobi d n)
         precondition = n `rem` 2 == 1
                     && not (isSquare n)
                     && gcd n d == 1
                     && gcd n q == 1
         condition = condition1 n
                  || condition2 n

findd :: (Integral a) => a -> a
findd n = fromJust $ find (\a -> jacobi a n == -1) potentiald

factorOut :: (Integral a) => a -> (a, a)
factorOut n = if odd n
              then (n, 0)
              else (olda, oldb + 1)
                  where (olda, oldb) = factorOut (n `div` 2)

potentiald :: (Integral a) => [a]
potentiald = alternate [5, 9..] [-7, -11..]

alternate :: [a] -> [a] -> [a]
alternate (x:xs) (y:ys) = x:y:(alternate xs ys)

condition1 n =
  let d = findd n
      p = 1
      q = (1 - d) `div` 4
      (o, e) = factorOut del
      del = n - (jacobi d n)
   in lucasu p q o n == 0

condition2 n = any (== 0) $ take (fromIntegral s)
            $ tail
            $ reverse
            $ map snd
            $ collapseMap' p q (1, p) (path del) (n)
              where p = 1
                    q = (1 - d) `div` 4
                    d = findd n
                    (o, s) = factorOut del
                    del = n - (jacobi d n)
