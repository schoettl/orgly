{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text as T

data Source = Source
  { source :: Text
  , language :: Text
  } deriving Show

parseSource :: Parser Source
parseSource = do
  manyTill anyChar
    (endOfLine *> string "#+BEGIN_SOURCE")
  lang <- parseSourceLanguage
  src <- T.pack <$> manyTill anyChar
    (endOfLine *> string "#+END_SOURCE")
  many' anyChar
  endOfInput
  return $ Source src lang

parseSourceLanguage :: Parser Text
parseSourceLanguage = do
  lang <- option "" (many1 (char ' ') *> many1 letter)
  many' $ char ' '
  endOfLine
  return $ T.pack lang


main :: IO ()
main = do
  text <- fmap T.pack getContents
  print $ parseOnly parseSource text
