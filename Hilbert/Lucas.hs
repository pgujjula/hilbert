module Hilbert.Lucas where

import Hilbert.Modular (modPow) 

u_seq :: (Integral a) => a -> a -> [a]
u_seq p q = list
  where list = [0, 1]
               ++ (zipWith (-)
                           (map (p*) (drop 1 list))
                           (map (q*) list)
                  )

v_seq :: (Integral a) => a -> a -> [a]
v_seq p q = list
  where list = [2, p]
               ++ (zipWith (-)
                           (map (p*) (drop 1 list))
                           (map (q*) list)
                  )

u :: (Integral a) => a -> a -> a -> a
u _ _ 0 = 0
u _ _ 1 = 1
u p q n | even n
  = (u p q k)*(v p q k) 
    where k = n `div` 2
u p q n | odd n
  = (u p q (k + 1))^2 - q*(u p q k)^2
    where k = (n - 1) `div` 2

v :: (Integral a) => a -> a -> a -> a
v _ _ 0 = 2
v p _ 1 = p
v p q n | even n
  = (v p q k)^2 - 2*q^k
    where k = n `div` 2
v p q n | odd n
   = ((v p q (k + 1))*(v p q k)) - ((q^k)*p)
     where k = (n - 1) `div` 2

path :: (Integral a) => a -> [a]
path = reverse . path'

path' :: (Integral a) => a -> [a]
path' 1 = [1]
path' n | even n = n:(path' $ n `div` 2)
path' n | odd n = n:(path' $ n - 1)

lucasu :: (Integral a) => a -> a -> a -> a -> a
lucasu p q n m = fst $ collapse' p q (1, p) (path n) m

collapse' :: (Integral a) => a -> a -> (a, a) -> [a] -> a -> (a, a)
collapse' _ _ t [x] _= t
collapse' p q (u, v) (x1:x2:xs) m
  = if x2 == (x1 + 1)
    then let u' = (numerator `div` 2) `rem` m
                where numerator = if odd expression
                                  then expression + m
                                  else expression
                      expression = p*u + v
             v' = (numerator `div` 2) `rem` m
                where numerator = if odd expression
                                  then expression + m
                                  else expression
                      expression = (p^2 - 4*q)*u + p*v
          in collapse' p q (u', v') (x2:xs) m
    else let u' = (u*v) `rem` m
             v' = (v^2 - 2*(modPow q x1 m)) `rem` m
          in collapse' p q (u', v') (x2:xs) m

collapseMap' :: (Integral a) => a -> a -> (a, a) -> [a] -> a -> [(a, a)]
collapseMap' _ _ t [x] _ = [t]
collapseMap' p q (u, v) (x1:x2:xs) m
  = if x2 == (x1 + 1)
    then let u' = (numerator `div` 2) `rem` m
                where numerator = if odd expression
                                  then expression + m
                                  else expression
                      expression = p*u + v
             v' = (numerator `div` 2) `rem` m
                where numerator = if odd expression
                                  then expression + m
                                  else expression
                      expression = (p^2 - 4*q)*u + p*v
         in (u, v):(collapseMap' p q (u', v') (x2:xs) m)
    else let u' = (u*v) `rem` m
             v' = (v^2 - 2*(modPow q x1 m)) `rem` m
          in (u, v):(collapseMap' p q (u', v') (x2:xs) m)
