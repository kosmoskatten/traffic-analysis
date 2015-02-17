module Main (main) where

import CommandLineParser (Command (..), parseCommandLine)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever, void, when)
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object
import Repl (Repl, get, liftIO, put, runRepl)
import System.Console.Readline (readline, addHistory)
import System.IO (hPutChar, hFlush, stdout)

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

handleCommand (Enumerate target) = do
  state <- get
  case state of
    Nothing      -> do liftIO $ putStrLn "No file is loaded"
                       return True
    Just objects -> do liftIO $ putStrLn $ printable target objects
                       return True

handleCommand (File filePath) = do
  put Nothing
  t    <- liftIO $ async ticker
  file <- liftIO $ LBS.readFile filePath
  case decodeObjectsPar file of
    Right objects -> do liftIO $ cancel t
                        put $ Just objects
    Left err      -> do liftIO $ cancel t
                        liftIO $ putStrLn err
  return True
      
handleCommand Help = return True
handleCommand Quit = return False

ticker :: IO ()
ticker =
  forever $ do
    threadDelay 1000000
    hPutChar stdout '.'
    hFlush stdout
