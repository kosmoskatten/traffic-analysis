module CommandLineParser
       ( Command (..)
       , parseCommandLine
       ) where

import Control.Applicative ((<$>), (*>))
import Network.Traffic.Object (EnumerationTarget (..))
import Text.Parsec
import Text.Parsec.String

data Command = EmptyLine 
             | Enumerate !EnumerationTarget
             | File !FilePath 
             | Help 
             | Quit
    deriving Show

parseCommandLine :: String -> Either ParseError Command
parseCommandLine = parse commandLine ""

commandLine :: Parser Command
commandLine = spaces *> ( emptyLine 
                          <|> enumerate
                          <|> file 
                          <|> quit
                          <|> help )

emptyLine :: Parser Command
emptyLine = eof *> return EmptyLine

enumerate :: Parser Command
enumerate = string "enumerate" *> spaces *> (Enumerate <$> enumerationTarget)

file :: Parser Command
file = string "file" *> spaces *> (File <$> filePath)

help :: Parser Command
help = string "help" *> return Help

quit :: Parser Command
quit = string "quit" *> return Quit

enumerationTarget :: Parser EnumerationTarget
enumerationTarget = try (string "transport") *> return TTransport
                    <|> try (string "application") *> return TApplication
                    <|> try (string "functionality") *> return TFunctionality
                    <|> try (string "serviceprovider") *> return TServiceProvider
                    <|> try (string "clientapp") *> return TClientApp
                    <|> try (string "terminaltype") *> return TTerminalType

filePath :: Parser FilePath
filePath = many1 ( letter
                   <|> digit
                   <|> choice [char '.', char '-', char '_', char '/']
                   <?> "file name" )
