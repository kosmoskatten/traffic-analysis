module CommandLineParser
       ( Command (..)
       , parseCommandLine
       ) where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Monad (void)
import Network.Traffic.Object ( EnumerationTarget (..)
                              , FilterFunc (..)
                              , filterFunc
                              , maybeRead )
import Text.Parsec
import Text.Parsec.String

data Command = EmptyLine 
             | Enumerate !EnumerationTarget !(Maybe FilterFunc)
             | File !FilePath
             | Playtime
             | LastObjectTime
             | Help 
             | Quit
    deriving Show

parseCommandLine :: String -> Either ParseError Command
parseCommandLine = parse commandLine ""

commandLine :: Parser Command
commandLine = spaces *> ( emptyLine 
                          <|> enumerate <* eof'
                          <|> file <* eof'
                          <|> playtime <* eof'
                          <|> lastObjectTime <* eof'
                          <|> quit <* eof'
                          <|> help <* eof' )

eof' :: Parser ()
eof' = spaces >> eof

emptyLine :: Parser Command
emptyLine = eof *> return EmptyLine

enumerate :: Parser Command
enumerate = string "enumerate" *> many1 space *> 
            (Enumerate <$> enumerationTarget <*> optionMaybe filterSpec)

file :: Parser Command
file = string "file" *> spaces *> (File <$> filePath)

playtime :: Parser Command
playtime = string "playtime" *> return Playtime

lastObjectTime :: Parser Command
lastObjectTime = string "lastobjecttime" *> return LastObjectTime

help :: Parser Command
help = string "help" *> return Help

quit :: Parser Command
quit = string "quit" *> return Quit

enumerationTarget :: Parser EnumerationTarget
enumerationTarget = do
  s <- many1 letter
  case maybeRead s of
    Just target -> return target
    Nothing     -> let expect = "Expected oneof: "
                                ++ show [ minBound :: EnumerationTarget .. ]
                   in parserFail expect

filterSpec :: Parser FilterFunc
filterSpec = many1 space *> string "where" *> filterSpec'
  where
    filterSpec' :: Parser FilterFunc
    filterSpec' = do
      void $ many1 space
      target <- many1 letter
      void $ many1 space
      void $ string "is"
      void $ many1 space
      value <- many1 letter
      case filterFunc target value of
        Left err -> parserFail err
        Right f  -> return f

filePath :: Parser FilePath
filePath = many1 ( letter
                   <|> digit
                   <|> choice [char '.', char '-', char '_', char '/']
                   <?> "file name" )
