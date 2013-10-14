module MpvPlayer.Backend where

import Data.List
import Data.Word
import Data.IORef
import Data.Maybe
import System.Process
import System.IO
import System.Posix.IO
import Text.Printf
import Text.Read
import Control.Concurrent 
import Control.Monad
import Prelude hiding (catch)
import Control.Exception

data PlayStatus = PlayStatus {
      playStatusLength :: Double,
      playStatusPos :: Double
} deriving (Show)

data Player = Player {
      playerProcess :: ProcessHandle,
      playerCmdIn :: Handle,

      playerOutWrite :: Handle,
      playerOutRead :: Handle,

      playerThreadIn :: MVar String,
      playerThreadOut :: MVar PlayStatus
}

fifoPath = "/tmp/mpvguihs.pipe"
cmdPrefix = "mpvguihs command response: "

buildArgs :: Word32 -> FilePath -> FilePath -> [String]
buildArgs wid fifo filename = ["--wid=" ++ (printf "0x%x" wid), 
                               "--input-file=" ++ fifo, 
                               "--status-msg=",
--                               "--slave-broken",
                               filename]

parseLines readHandle outmv = do
  ready <- hWaitForInput readHandle 1000
  when ready $ do
    line <- hGetLine readHandle 
    let status = stripPrefix cmdPrefix line
    case status of
      Nothing -> putStrLn line
      Just i  -> do
        let [len, r] = words i
        let len' = fromMaybe 0.0 $ readMaybe len
        let r' = fromMaybe 0.0 $ readMaybe r
        tryTakeMVar outmv
        putMVar outmv $ PlayStatus len' r'

    parseLines readHandle outmv

statusThread writeHandle readHandle inmv outmv = forever $ do
  hPutStrLn writeHandle $ concat ["print_text \"", 
                                  cmdPrefix, 
                                  "${=length} ${=time-pos}\""]
  hFlush writeHandle
  
  catch (parseLines readHandle outmv) printException

  threadDelay 250000

  where printException :: SomeException -> IO ()
        printException e = print e

mpvPlay :: Word32 -> FilePath -> c -> IO (IORef Player)
mpvPlay wid filename ignored = do
  let args = buildArgs wid fifoPath filename
  putStrLn $ "Launching mpv with args: " ++ show args
  
  system $ "mkfifo " ++ fifoPath

  (readFd, writeFd) <- createPipe
  outWrite <- fdToHandle writeFd
  outRead <- fdToHandle readFd
  
  process <- runProcess "mpv" args 
             Nothing Nothing Nothing (Just outWrite) Nothing 

  inpfd <- openFd fifoPath WriteOnly Nothing defaultFileFlags
  inp <- fdToHandle inpfd

  inmv <- newEmptyMVar
  outmv <- newEmptyMVar

  forkIO $ statusThread inp outRead inmv outmv

  newIORef $ Player process inp outWrite outRead inmv outmv

mpvPause :: IORef Player -> IO ()
mpvPause playerRef = do
  player <- readIORef playerRef
  hPutStrLn (playerCmdIn player) "cycle pause"
  hFlush (playerCmdIn player)

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
  hPutStrLn (playerCmdIn p) "stop"
  hFlush (playerCmdIn p)

mpvGetPlayStatus :: IORef Player -> IO (Maybe PlayStatus)
mpvGetPlayStatus playerRef = do
  p <- readIORef playerRef
  tryTakeMVar $ playerThreadOut p

mpvTerminate :: IORef Player -> IO ()
mpvTerminate playerRef = do
  p <- readIORef playerRef
  let h = playerCmdIn p
  hPutStrLn h "quit 0"
  hClose h 
  threadDelay 250000
  terminateProcess (playerProcess p)
