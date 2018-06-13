module OrgLy.OrgmodeParse
  ( Source (..)
  , parseSectionParagraph
  , unrollHeadlines
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section, subHeadlines), Section (..), Properties (..))
import Data.Attoparsec.Text
import Data.Maybe

data Source = Source
  { source :: Text
  , language :: Text
  } deriving Show

parseSectionParagraph :: Parser Source
parseSectionParagraph = parseSource

parseSource :: Parser Source
parseSource = do
  manyTill anyChar
    (endOfLine *> string "#+BEGIN_SRC")
  lang <- parseSourceLanguage
  src <- T.pack <$> manyTill anyChar
    (endOfLine *> string "#+END_SRC")
  many' anyChar
  endOfInput
  return $ Source src lang

parseSourceLanguage :: Parser Text
parseSourceLanguage = do
  lang <- option "" (many1 (char ' ') *> many1 letter)
  many' $ char ' '
  endOfLine
  return $ T.pack lang

unrollHeadlines :: [Headline] -> [Headline]
unrollHeadlines [] = []
unrollHeadlines (h:hs) = h : unrollHeadlines (subHeadlines h) ++ unrollHeadlines hs
