module Main where

import           System.CPUInstructionCounter

main :: IO ()
main = do
  putStrLn $
    "This test needs to run as root, or with CAP_SYS_ADMIN,"
    ++ " or with /proc/sys/kernel/perf_event_paranoid <= 2,"
    ++ " otherwise performance counters may not be available."

  ((), instrs) <- withInstructionsCounted $ do
    return ()
  putStrLn $ "`return ()` took " ++ show instrs ++ " instructions"


  -- Example regression test

  (_, sumInstrs) <- withInstructionsCounted $ do
    return $! sum [1..10000::Int]

  let expected = 90651 -- measured on my Core i5-4590, with lts-10.3

  if sumInstrs > (2 * expected)
    then error $ "sum [1..10000] performance regressed; took " ++ show sumInstrs ++ " instructions"
    else putStrLn $ "sum [1..10000] performance OK; took " ++ show sumInstrs ++ " instructions"
