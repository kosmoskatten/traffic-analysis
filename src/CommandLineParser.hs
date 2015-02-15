module CommandLineParser
       ( Command (..)
       , parseCommandLine
       ) where

import Control.Applicative ((<$>), (*>))
import Text.Parsec
import Text.Parsec.String

data Command = EmptyLine | File !FilePath | Help | Quit
    deriving Show

parseCommandLine :: String -> Either ParseError Command
parseCommandLine = parse commandLine ""

commandLine :: Parser Command
commandLine = spaces *> (emptyLine <|> file <|> quit <|> help)

emptyLine :: Parser Command
emptyLine = eof >> return EmptyLine

file :: Parser Command
file = string "file" *> spaces *> (File <$> filePath)

help :: Parser Command
help = string "help" *> return Help

quit :: Parser Command
quit = string "quit" *> return Quit

filePath :: Parser FilePath
filePath = many1 (letter
                 <|> digit
                 <|> choice [char '.', char '-', char '_', char '/']
                 <?> "file name")



