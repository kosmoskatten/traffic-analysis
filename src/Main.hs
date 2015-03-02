module Main (main) where

import CommandLineParser (Command (..), parseCommandLine)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.DeepSeq (($!!))
import Control.Monad (forever, void, when)
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object ( ObjectVector
                              , decodeObjectsPar
                              , filterObjects
                              , printable
                              , lastObjectTime
                              , totalPlaytime )
import Repl (Repl, get, liftIO, put, runRepl)
import System.Console.Readline (readline, addHistory)
import System.IO (hPutChar, hPutStrLn, hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = void $ runRepl repl Nothing

repl :: Repl (Maybe ObjectVector) ()
repl = do
  line <- liftIO $ readline "> "
  case line of
    Nothing    -> return ()
    Just line' -> do continue <- handleCommandLine line'
                     when continue repl

handleCommandLine :: String -> Repl (Maybe ObjectVector) Bool
handleCommandLine line =
  case parseCommandLine line of
    Left err      -> do liftIO $ putStrLn (show err)
                        return True
    Right command -> do liftIO $ addHistory line
                        handleCommand command

handleCommand :: Command -> Repl (Maybe ObjectVector) Bool
handleCommand EmptyLine = return True

handleCommand (Enumerate target filterFunc) = do
  state <- get
  case state of
    Nothing      -> liftIO $ putStrLn "No file is loaded"
    Just objects -> do
      p <- timedAction $ do
        let objects' = filterObjects filterFunc objects
        return $!! printable target objects'
  
      liftIO $ putStrLn p
  return True

handleCommand (File filePath) =
    timedAction $ do
      put Nothing
      file <- liftIO $ LBS.readFile filePath
      case decodeObjectsPar file of
        Right objects -> put $ Just objects
        Left err      -> liftIO $ putStrLn err
      return True

handleCommand Playtime = do
  state <- get
  case state of
    Nothing      -> liftIO $ putStrLn "No file is loaded"
    Just objects -> do t <- timedAction $ return $!! totalPlaytime objects
                       liftIO $ printf "Total playtime is %s\n" (show t)
  return True

handleCommand LastObjectTime = do
  state <- get
  case state of
    Nothing      -> liftIO $ putStrLn "No file is loaded"
    Just objects -> liftIO $ printf "Last object's time is: %s\n"
                                    (show $ lastObjectTime objects)
  return True

handleCommand Help = return True
handleCommand Quit = return False

timedAction :: Repl s a -> Repl s a
timedAction action = do
    t <- liftIO $ async ticker
    result <- action
    liftIO $ do cancel t
                hPutStrLn stdout ""
                hFlush stdout
    return result

ticker :: IO ()
ticker =
  forever $ do
    threadDelay 1000000
    hPutChar stdout '.'
    hFlush stdout
