{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import Control.Monad (when)
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Data.Time.Clock

import Reactive.Banana
import Reactive.Banana.Frameworks



main :: IO ()
main = do
  displayHelpMessage
  sources <- (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
  network <- compile $ setupNetwork sources
  actuate network
  eventLoop sources


displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "Commands are:":
    "   s   - start/stop ":
    "   l   - record lap time":
    "   q   - quit the program":
    "":
    []



type EventSource a = ( AddHandler a, Handler a )

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> Handler a
fire = snd



eventLoop :: (EventSource (), EventSource (), EventSource TimeCount) -> IO ()
eventLoop (esMain, esLap, esTick) = loop
    where
    loop = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        s <- getChar
        case s of
            's' -> fire esMain ()
            'l' -> fire esLap  ()
            'q' -> return ()
            _   -> putStrLn $ [s] ++ " - unknown command"
        when (s /= 'q') loop


setupNetwork :: forall t. Frameworks t => (EventSource (), EventSource (), EventSource TimeCount) -> Moment t ()
setupNetwork (esMain, esLap, esTick) =do

    timer <- liftIO $ newTimer
  
    eMain <- fromAddHandler $ addHandler esMain
    eLap  <- fromAddHandler $ addHandler esLap
    eTick <- fromAddHandler $ addHandler esTick


    let

      --
      -- Event switch status -> running/not-running
      -- 
      eRun :: Event t Bool
      eRun = accumE False ( not <$ eMain )

      --
      -- Event start/stop
      -- 
      eStart :: Event t ()
      eStop  :: Event t ()
      eStart = () <$ filterE id  eRun 
      eStop  = () <$ filterE not eRun

      --
      -- Behavior current count
      --
      bCount :: Behavior t TimeCount
      bCount  = stepper (TimeCount 0) eTick

      --
      -- Event start/lap with current count
      --
      eStart' :: Event t TimeCount
      eLap'   :: Event t TimeCount
      eStart' = bCount <@ eStart
      eLap'   = bCount <@ eLap


    reactimate $ start timer esTick <$> eStart' -- start counter
    reactimate $ stop  timer        <$  eStop   -- stop  counter

    reactimate $ tick <$> eTick -- update counter
    reactimate $ lap  <$> eLap' -- update lap



start :: TimerIO -> EventSource TimeCount -> TimeCount -> IO ()
start timer esTick timeCount = do
  timeCurr <- getCurrentTime
  success <- repeatedStart timer (fireTick timeCurr) $ msDelay 10
  -- [TODO] check success
  return()
  where
    fireTick timeStart = do
      timeCurr <- getCurrentTime
      fire esTick $ TimeCount $ (diffUTCTime timeCurr timeStart) + (getDiffTime timeCount)


stop :: TimerIO -> IO ()
stop = stopTimer


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
