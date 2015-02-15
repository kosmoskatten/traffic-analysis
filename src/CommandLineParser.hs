module CommandLineParser
       ( Command (..)
       , parseCommandLine
       ) where

import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.String

data Command = File !FilePath | Help | Quit
    deriving Show

parseCommandLine :: String -> Either ParseError Command
parseCommandLine = parse commandLine ""

commandLine :: Parser Command
commandLine = file
              <|> quit
              <|> help

file :: Parser Command
file = do
  string "file"
  spaces
  File <$> filePath

help :: Parser Command
help = string "help" >> return Help

quit :: Parser Command
quit = string "quit" >> return Quit

filePath :: Parser FilePath
filePath = many1 (letter
                 <|> digit
                 <|> choice [char '.', char '-', char '_', char '/']
                 <?> "file name")



