import System.Exit (exitFailure, exitSuccess)
import Hilbert.Test
import Hilbert.PriorityQueue.MapQueue
import Hilbert.PriorityQueue.ADT as PQ

e :: MapQueue String Int
e = empty

-- Test that size of empty is 1
emptyTest = Single
              "size empty"
              "0"
              (show $ size    (empty::(MapQueue a a)))

-- Test that null of an empty queue is true
nullTest  = Single
              "null empty"
              "True"
              (show $ PQ.null (empty::(MapQueue a a)))

-- Unit tests with small priority queues
mq1 = insert "two" 2 e
mq1Tests = Chain [Single "size mq1" "1"     (show $ size    mq1),
                  Single "null mq1" "False" (show $ PQ.null mq1)]

mq2 = insert "three" 3 mq1
mq2Tests = Chain [Single "size mq2"    "2"         (show $ size mq2),
                  Single "peekMin mq2" "\"two\"" (show $ peekMin mq2),
                  Single "peekMinWithPriority mq2"
                         "(\"two\",2)"
                         (show $ peekMinWithPriority mq2)]

allTests = Chain [emptyTest, nullTest, mq1Tests, mq2Tests]

main = mruntest allTests
