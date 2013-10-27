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
import Control.Exception

data PlayStatus = PlayStatus {
      playStatusPaused :: Bool,
      playStatusLength :: Double,
      playStatusPos :: Double,
      playStatusMuted :: Bool,
      playStatusVol :: Int
} deriving (Show)

data Player = Player {
      playerProcess :: ProcessHandle,
      playerCmdIn :: Handle,

      playerOutWrite :: Handle,
      playerOutRead :: Handle
}

fifoPath :: String
fifoPath = "/tmp/mpvguihs.pipe"

cmdPrefix :: String
cmdPrefix = "mpvguihs command response: "

buildArgs :: Word32 -> FilePath -> FilePath -> String -> [String]
buildArgs wid fifo filename extraArgs = 
  ["--wid=" ++ (printf "0x%x" wid), 
   "--input-file=" ++ fifo, 
   "--status-msg=",
   extraArgs,
   filename]

parseLines :: Maybe PlayStatus -> Handle -> IO (Maybe PlayStatus)
parseLines ps readHandle = do
  ready <- hReady readHandle
  if ready
    then do
      line <- hGetLine readHandle 
      let status = stripPrefix cmdPrefix line
      case status of
        Nothing -> putStrLn line >> parseLines ps readHandle
        Just i  -> 
          let [len, r, p, m, vol] = words i
              len' = fromMaybe 0.0 $ readMaybe len
              r' = fromMaybe 0.0 $ readMaybe r
              paused = p == "yes"
              muted = m == "yes"
              vol' = round $ (fromMaybe 0.0 $ readMaybe vol :: Double)
          in parseLines (Just $ PlayStatus paused len' r' muted vol') readHandle

    else return ps

mpvGetPlayStatus :: IORef Player -> IO (Maybe PlayStatus)
mpvGetPlayStatus playerRef = do
  p <- readIORef playerRef

  hPutStrLn (playerCmdIn p) $ concat ["print_text \"", 
                                      cmdPrefix, 
                                      "${=length} ${=time-pos} ${pause} " ++
                                      "${mute} ${volume}\""]
    
  catch (parseLines Nothing $ playerOutRead p) $ printException

  where printException :: SomeException -> IO (Maybe PlayStatus)
        printException e = print e >> return Nothing

mpvPlay :: Word32 -> FilePath -> String -> IO (IORef Player)
mpvPlay wid filename extraArgs = do
  let args = buildArgs wid fifoPath filename extraArgs
  putStrLn $ "Launching mpv with args: " ++ show args
  
  system $ "mkfifo " ++ fifoPath

  (readFd, writeFd) <- createPipe
  outWrite <- fdToHandle writeFd
  outRead <- fdToHandle readFd
  
  errWrite <- openFile "/dev/null" WriteMode

  process <- runProcess "mpv" args 
             Nothing Nothing Nothing (Just outWrite) (Just errWrite)

  inpfd <- openFd fifoPath WriteOnly Nothing defaultFileFlags
  
  inp <- fdToHandle inpfd
  hSetBuffering inp NoBuffering

  newIORef $ Player process inp outWrite outRead

mpvPause :: IORef Player -> IO ()
mpvPause playerRef = do
  player <- readIORef playerRef
  status <- mpvGetPlayStatus playerRef
  case status of
    Nothing -> putStrLn "BUG!"
    Just s ->  when (not $ playStatusPaused s) $ 
                 hPutStrLn (playerCmdIn player) "cycle pause"
  
mpvUnpause :: IORef Player -> IO ()
mpvUnpause playerRef = do
  player <- readIORef playerRef
  status <- mpvGetPlayStatus playerRef
  case status of 
    Nothing -> putStrLn "BUG!"
    Just s -> when (playStatusPaused s) $ 
                hPutStrLn (playerCmdIn player) "cycle pause"

mpvKeyPress :: IORef Player -> Word32 -> IO ()
mpvKeyPress player key = undefined

mpvSeek :: IORef Player -> Double -> IO ()
mpvSeek player ratio = do
  p <- readIORef player
  let percent = ratio * 100
  hPutStrLn (playerCmdIn p) $ printf "seek %f absolute-percent" percent

mpvStop :: IORef Player -> IO ()
mpvStop playerRef = do
  p <- readIORef playerRef
  hPutStrLn (playerCmdIn p) "stop"

mpvSetMuted :: IORef Player -> Bool -> IO ()
mpvSetMuted playerRef val = do
  p <- readIORef playerRef
  let val' = if val then "yes" else "no"
  hPutStrLn (playerCmdIn p) $ "set mute " ++ val'

mpvSetVolume :: IORef Player -> Int -> IO ()
mpvSetVolume playerRef vol = do
  p <- readIORef playerRef
  hPutStrLn (playerCmdIn p) $ "set volume " ++ show vol
  
mpvTerminate :: IORef Player -> IO ()
mpvTerminate playerRef = do
  p <- readIORef playerRef
  let h = playerCmdIn p
  hPutStrLn h "quit 0"

  hClose h 
  hClose $ playerOutRead p
  hClose $ playerOutWrite p

  threadDelay 250000
  terminateProcess (playerProcess p)

