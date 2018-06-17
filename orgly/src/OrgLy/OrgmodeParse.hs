module OrgLy.OrgmodeParse
  ( SectionContents (..)
  , SectionContent (..)
  , parseSectionParagraph
  , unrollHeadlines
  ) where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section, subHeadlines), Section (..), Properties (..))
import qualified Data.OrgMode.Types as O
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Control.Applicative ((<|>))

data SectionContents = SectionContents O.Properties [SectionContent]

data SectionContent =
    SectionText Text
  | Comment Text
  | Source
      { language :: Maybe Text
      , source :: Text
      }
  deriving Show


-- | Parse the text of 'Data.OrgMode.Types.Section' sectionParagraph.
parseSectionParagraph :: Parser SectionContents
parseSectionParagraph = parseSectionContents

parseSectionContents :: Parser SectionContents
parseSectionContents = do
  cs <- many' (parseSource <|> parseComment)
  return $ SectionContents (O.Properties M.empty) cs

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

-- https://orgmode.org/manual/Comment-lines.html
parseComment :: Parser SectionContent
parseComment = parseLineComments <|> parseMultiLineComment

parseLineComments :: Parser SectionContent
parseLineComments = do
  skipWhile isHorizontalSpace
  string "# "
  -- char '#'
  -- satisfy isHorizontalSpace
  skipWhile isHorizontalSpace
  commentText <- takeWhile isEndOfLine
  endOfLine
  return $ Comment commentText


parseMultiLineComment :: Parser SectionContent
parseMultiLineComment = do
  startCommentLine
  commentText <- T.pack <$> manyTill anyChar endCommentLine
  return $ Comment commentText
  where
    startCommentLine = do
      skipWhile isHorizontalSpace
      string "#+BEGIN_COMMENT"
      skipWhile isHorizontalSpace
      endOfLine
    endCommentLine = do
      endOfLine
      skipWhile isHorizontalSpace
      string "#+END_COMMENT"
      skipWhile isHorizontalSpace
      endOfLine



unrollHeadlines :: [Headline] -> [Headline]
unrollHeadlines [] = []
unrollHeadlines (h:hs) = h : unrollHeadlines (subHeadlines h) ++ unrollHeadlines hs
