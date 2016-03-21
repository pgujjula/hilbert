import Hilbert.Modular (modPow)
import Test.QuickCheck (Gen, choose, sample, sample', generate)
import Criterion.Measurement (measure)
import Criterion (whnf)
import Criterion.Types (measureAccessors, rescale)
import Data.Int (Int64)
import Control.Monad (liftM)
import Data.Map ((!))
import Data.Maybe (fromJust)
import Control.Monad (msum, join)
import Control.Applicative (liftA)
import Debug.Trace (trace)

-- genLog n generates an integer in the range [2^n, 2^(n + 1)]
genLog :: Integer -> Gen Integer
genLog n = choose (2^n, 2^(n + 1) - 1)

iterations = 1 :: Int64

measureModPow a b c = (liftM fst) $ measure (whnf (modPow a b) c) iterations

f a b c = do
  m' <- measureModPow a b c
  let m = rescale m'
  let accessor = fst $ measureAccessors ! "time"
  let time = accessor m
  return (fromJust time)

run :: Integer -> IO Double
run n = join $ generate genCase
  where genCase = do a <- genLog n
                     b <- genLog n
                     c <- genLog n
                     return (f a b c)

multiplier = 100
size = 50
sizes = [multiplier, 2*multiplier..(size*multiplier)]
mtimes = sequence $ map run sizes

display (x, y) = (show x) ++ (", ") ++ (show y)
main = do
  times <- mtimes
  mapM_ (putStrLn . display) (zip sizes times)
  
