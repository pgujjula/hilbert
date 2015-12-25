import System.Exit (exitFailure, exitSuccess)
import Hilbert.Test
import Hilbert.PriorityQueue.MapQueue
import Hilbert.PriorityQueue.ADT as PQ

-- Test that size of empty is 1
emptyTest = Single "size empty" "0" (show $ size (empty::(MapQueue a a)))

-- Test that null of an empty queue is true
nullTest = Single "null empty" "True" (show $ PQ.null (empty::(MapQueue a a)))

e :: MapQueue String Int
e = empty

-- Insert into empty
mq1 = insert "zero1" 0 e
mqTests = Chain [Single "size mq1" "1" (show $ size mq1)
                ,Single "null mq1" "False" (show $ PQ.null mq1)]

allTests = Chain [emptyTest, nullTest]

main = mruntest allTests
