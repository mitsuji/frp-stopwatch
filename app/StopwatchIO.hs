module Main where

import System.IO
import Control.Monad (when)
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Data.Time.Clock

import Data.IORef



main :: IO ()
main = do
  displayHelpMessage
  timer <- newTimer
  stateRef <- newIORef (False, TimeCount 0)
  waitCommand timer stateRef
  

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "Commands are:":
    "   s   - start/stop ":
    "   l   - record lap time":
    "   q   - quit the program":
    "":
    []


type StateRef = IORef (Bool, TimeCount)


waitCommand :: TimerIO -> StateRef -> IO ()
waitCommand timer stateRef = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  s <- getChar
  case s of
    's' -> do
      isRunning <- readIsRunning stateRef
      if isRunning
        then stop timer stateRef
        else start timer stateRef
    'l' -> lap' timer stateRef
    'q' -> return ()
    _   -> do
      putStrLn $ [s] ++ " - unknown command"
      waitCommand timer stateRef


start :: TimerIO -> StateRef -> IO ()
start timer stateRef = do
  timeCurr <- getCurrentTime
  timeCount <- readTimeCount stateRef
  success <- repeatedStart timer (tick' timeCurr timeCount) $ msDelay 10
  -- [TODO] check success
  writeIsRunning stateRef True
  waitCommand timer stateRef
  where
    tick' timeStart timeCount = do
      timeCurr <- getCurrentTime
      let newTimeCount = TimeCount $ (diffUTCTime timeCurr timeStart) + (getDiffTime timeCount)
      tick $ newTimeCount
      writeTimeCount stateRef $ newTimeCount


stop :: TimerIO -> StateRef -> IO ()
stop timer stateRef = do
  stopTimer timer
  writeIsRunning stateRef False
  waitCommand timer stateRef



lap' :: TimerIO -> StateRef -> IO ()
lap' timer stateRef = do
  timeCount <- readTimeCount stateRef
  lap timeCount
  waitCommand timer stateRef



readIsRunning :: StateRef -> IO Bool
readIsRunning stateRef = do
  (isRunning, _) <- readIORef stateRef
  return isRunning

readTimeCount :: StateRef -> IO TimeCount
readTimeCount stateRef = do
  (_, timeCount) <- readIORef stateRef
  return timeCount

writeIsRunning :: StateRef -> Bool -> IO ()
writeIsRunning stateRef isRunning = do
  (_, timeCount) <- readIORef stateRef
  writeIORef stateRef (isRunning, timeCount)

writeTimeCount :: StateRef -> TimeCount -> IO ()
writeTimeCount stateRef timeCount = do
  (isRunning, _) <- readIORef stateRef
  writeIORef stateRef (isRunning, timeCount)


  

tick :: TimeCount -> IO ()
lap  :: TimeCount -> IO ()
tick timeCount = (putChar '\r') >> ( putStr $ show $ timeCount )
lap  timeCount = (putChar '\n') >> ( putStr $ show $ timeCount )


--
-- TimeCount for stop-watch counter
--
--
--

newtype TimeCount = TimeCount { getDiffTime::NominalDiffTime }

instance Show TimeCount where
  show tc = hour ++ ":" ++ min ++ ":" ++ sec ++ "." ++ ms
    where
      dt   = getDiffTime tc
      hour = show $ truncate (dt / 3600)        
      min  = fixed 2 $ mod ( truncate (dt / 60  ) ) 60
      sec  = fixed 2 $ mod ( truncate (dt       ) ) 60
      ms   = fixed 3 $ mod ( truncate (dt * 1000) ) 1000

fixed :: Int -> Int -> String
fixed digits num = replicate ( digits - len ) '0' ++ num'
  where
    num' = show num
    len  = length $ num'

