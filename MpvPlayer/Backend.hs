module MpvPlayer.Backend where

import Data.Word
import Data.IORef
import System.Process
import System.IO
import System.Posix.IO
import Text.Printf
import Control.Concurrent (threadDelay)

data FileInfo = FileInfo {
}

data State = Playing | Paused

data Player = Player {
      playerFileInfo :: FileInfo,
      playerVolume :: Float,
      playerPositionRatio :: Double,
      playerState :: State,

      playerProcess :: ProcessHandle,
      playerIn :: Handle
}

fifoPath = "/tmp/mpvguihs.pipe"

buildArgs :: Word32 -> FilePath -> FilePath -> [String]
buildArgs wid fifo filename = ["--wid=" ++ (printf "0x%x" wid), 
                               "--input-file=" ++ fifo, 
--                               "--slave-broken",
--                               "--paused",
                               filename]

mpvPlay :: Word32 -> FilePath -> c -> IO (IORef Player)
mpvPlay wid filename ignored = do
  let args = buildArgs wid fifoPath filename
  putStrLn $ "Launching mpv with args: " ++ show args
  
  system $ "mkfifo " ++ fifoPath
  
  process <- runProcess "mpv" args 
             Nothing Nothing Nothing Nothing Nothing 

  inpfd <- openFd fifoPath WriteOnly Nothing defaultFileFlags
  inp <- fdToHandle inpfd
  
  newIORef $ Player FileInfo 0.0 0.0 Playing process inp

mpvPause :: IORef Player -> IO ()
mpvPause playerRef = do
  player <- readIORef playerRef
  hPutStrLn (playerIn player) "cycle pause"
  hFlush (playerIn player)

mpvUnpause :: IORef Player -> IO ()
mpvUnpause = mpvPause

mpvKeyPress :: IORef Player -> Word32 -> IO ()
mpvKeyPress player key = undefined

mpvVolumeChange :: IORef Player -> Float -> IO ()
mpvVolumeChange player volume = undefined

mpvSeek :: IORef Player -> Double -> IO ()
mpvSeek player ratio = undefined

mpvStop :: IORef Player -> IO ()
mpvStop playerRef = do
  p <- readIORef playerRef
  hPutStrLn (playerIn p) "stop"
  hFlush (playerIn p)

mpvGetLength :: IORef Player -> IO Integer
mpvGetLength playerRef = do
{-  p <- readIORef playerRef
  let h = playerIn p
  hPutStrLn h "print_text ${length}"
  hFlush h -}
  return 0

mpvGetPosAsRatio :: IORef Player -> IO Double
mpvGetPosAsRatio playerRef = return 0.0

mpvTerminate :: IORef Player -> IO ()
mpvTerminate playerRef = do
  p <- readIORef playerRef
  let h = playerIn p
  hPutStrLn h "quit 0"
  hClose h 
  threadDelay 250000
  terminateProcess (playerProcess p)
