module CommandLineParser
       ( Command (..)
       , parseCommandLine
       ) where

import Control.Applicative ((<$>), (*>), (<*))
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
                          <|> enumerate <* eof'
                          <|> file <* eof'
                          <|> quit <* eof'
                          <|> help <* eof' )

eof' :: Parser ()
eof' = spaces >> eof

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
enumerationTarget = try (string "transport") *> return Transport
                    <|> try (string "application") *> return Application
                    <|> try (string "functionality") *> return Functionality
                    <|> try (string "serviceprovider") *> return ServiceProvider
                    <|> try (string "clientapp") *> return ClientApp
                    <|> try (string "terminaltype") *> return TerminalType

filePath :: Parser FilePath
filePath = many1 ( letter
                   <|> digit
                   <|> choice [char '.', char '-', char '_', char '/']
                   <?> "file name" )
