{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include "perfcounters.h"

-- | Counting CPU instructions.
--
-- Compiles and works only on Linux.
--
-- Especially useful for writing performance regression tests.
--
-- __Example:__
--
-- > main :: IO ()
-- > main = do
-- >   putStrLn $
-- >     "This test needs to run as root, or with CAP_SYS_ADMIN,"
-- >     ++ " or with /proc/sys/kernel/perf_event_paranoid <= 2,"
-- >     ++ " otherwise performance counters may not be available."
-- >
-- >   ((), instrs) <- withInstructionsCounted $ do
-- >     return () -- your code here
-- >
-- >   putStrLn $ "`return ()` took " ++ show instrs ++ " instructions
module System.CPUInstructionCounter
  (
  -- * Counting instructions
    withInstructionsCounted

  -- * Low-level functions
  , perfEventOpenHwInstructions
  , startInstructionCounter
  , finishInstructionCounter
  , perfEventClose

  -- * Low-level C function wrappers
  , c_perf_event_open_hw_instructions
  , c_start_instruction_counter
  , c_finish_instruction_counter
  ) where

import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO, liftIO)
import           Data.Typeable (Typeable)
import           Foreign (Int64)
import           Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import           Foreign.C.Types (CInt(..), CLLong(..))


-- | Exception to be thrown when `finishInstructionCounter` does an unexpected
-- short read. This should never happen.
data PerfEventShortReadException = PerfEventShortReadException
  deriving (Eq, Ord, Show, Typeable)

instance Exception PerfEventShortReadException


-- | See @perfcounters.h@ for docs.
foreign import ccall unsafe "perfcounters.h perf_event_open_hw_instructions"
  c_perf_event_open_hw_instructions :: IO CInt


-- | See @perfcounters.h@ for docs.
foreign import ccall unsafe "perfcounters.h start_instruction_counter"
  c_start_instruction_counter :: CInt -> IO CInt


-- | See @perfcounters.h@ for docs.
foreign import ccall unsafe "perfcounters.h finish_instruction_counter"
  c_finish_instruction_counter :: CInt -> IO CLLong


-- | See @man 2 close@ for docs.
foreign import ccall unsafe "unistd.h close"
  c_close :: CInt -> IO CInt


-- | Calls @perf_event_open()@ with `PERF_COUNT_HW_INSTRUCTIONS`.
-- You should `perfEventClose` the returned FD when you're done with it.
perfEventOpenHwInstructions :: IO CInt
perfEventOpenHwInstructions =
  throwErrnoIfMinus1Retry "perf_event_open" c_perf_event_open_hw_instructions


-- | Starts instruction counting on the given `perfEventOpenHwInstructions` FD.
startInstructionCounter :: CInt -> IO ()
startInstructionCounter fd =
  throwErrnoIfMinus1Retry_ "start_instruction_counter" (c_start_instruction_counter fd)


-- | Stops instruction counting on the given `perfEventOpenHwInstructions` FD.
-- Returns the number of instructions.
finishInstructionCounter :: CInt -> IO CLLong
finishInstructionCounter fd = do
  val <- throwErrnoIfMinus1Retry "finish_instruction_counter" (c_finish_instruction_counter fd)
  if val == -2
    then throwIO PerfEventShortReadException
    else return val


-- | Closes the given `perfEventOpenHwInstructions` FD.
perfEventClose :: CInt -> IO ()
perfEventClose fd =
  -- Don't retry on EINTR on `close()`, see https://lwn.net/Articles/576478/.
  throwErrnoIfMinus1_ "close" (c_close fd)


-- | Performs an action, conting the CPU instructions taken to perform it.
--
-- This function is async-exception-safe.
withInstructionsCounted :: (MonadUnliftIO m) => m a -> m (a, Int64)
withInstructionsCounted f = withRunInIO $ \runInIO -> do
  bracket
    (liftIO perfEventOpenHwInstructions)
    (\fd -> liftIO $ perfEventClose fd)
    $ \fd -> do
      liftIO $ startInstructionCounter fd
      x <- runInIO f
      numInstrs <- liftIO $ finishInstructionCounter fd
      return (x, fromIntegral numInstrs)
