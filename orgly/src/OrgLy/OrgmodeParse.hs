module OrgLy.OrgmodeParse
  ( SectionContents (..)
  , SectionContent (..)
  , parseSectionParagraph
  , unrollHeadlines
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section, subHeadlines), Section (..), Properties (..))
import qualified Data.OrgMode.Types as O
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.HashMap.Strict as M

data SectionContents = SectionContents O.Properties [SectionContent]

data SectionContent =
    SectionText Text
  | Comment Text
  | Source
      { language :: Maybe Text
      , source :: Text
      }
  deriving Show


parseSectionParagraph :: Parser SectionContents
parseSectionParagraph = do
  s <- parseSource
  return $ SectionContents (O.Properties M.empty) [s]

parseSource :: Parser SectionContent
parseSource = do
  manyTill anyChar
    (endOfLine *> string "#+BEGIN_SRC")
  lang <- parseSourceLanguage
  src <- T.pack <$> manyTill anyChar
    (endOfLine *> string "#+END_SRC")
  many' anyChar
  endOfInput
  return $ Source (Just lang) src

parseSourceLanguage :: Parser Text
parseSourceLanguage = do
  lang <- option "" (many1 (char ' ') *> many1 letter)
  many' $ char ' '
  endOfLine
  return $ T.pack lang

unrollHeadlines :: [Headline] -> [Headline]
unrollHeadlines [] = []
unrollHeadlines (h:hs) = h : unrollHeadlines (subHeadlines h) ++ unrollHeadlines hs
