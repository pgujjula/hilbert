import System.Random
import Hilbert.Modular
import System.Exit (exitFailure)

makeNums :: RandomGen t => t -> [Integer]
makeNums gen = num:(makeNums newgen)
  where (num, newgen) = randomR (1, 100) gen

test :: (Integer, Integer, Integer) -> Bool
test (base, ex, modulus) =
  (modPow base ex modulus) == ((base^ex) `rem` modulus)

main :: IO ()
main = do
  g <- getStdGen
  let bases = take 100 $ makeNums g
  let exps = take 100 $ drop 100 $ makeNums g
  let mods = take 100 $ drop 200 $ makeNums g
  if all test $ zip3 bases exps mods
  then return ()
  else exitFailure
