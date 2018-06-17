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
import Data.Char (isSpace)

data SectionContents = SectionContents O.Properties [SectionContent]
  deriving Show

data SectionContent =
    SectionText Text
  | Comment Text
  | Source
      { language :: Maybe Text
      , source :: Text
      }
  deriving Show


-- | Unroll headlines to a flat list. 'Headline' data structures will not be changed!
unrollHeadlines :: [Headline] -> [Headline]
unrollHeadlines [] = []
unrollHeadlines (h:hs) = h : unrollHeadlines (subHeadlines h) ++ unrollHeadlines hs



-- | Parse the text of 'Data.OrgMode.Types.Section' sectionParagraph.
parseSectionParagraph :: Parser SectionContents
parseSectionParagraph = parseSectionContents

parseSectionContents :: Parser SectionContents
parseSectionContents = do
  cs <- many' $
            (parseComment <?> "comment")
        <|> (parseSource  <?> "source code")
        <|> (parseText    <?> "normal text")
  let cs' = foldr mergeMultiLineText [] cs
  return $ SectionContents (O.Properties M.empty) cs'

mergeMultiLineText :: SectionContent -> [SectionContent] -> [SectionContent]
mergeMultiLineText c [] = [c]
mergeMultiLineText (SectionText t) (SectionText t' : xs) = (SectionText $ T.concat [t, "\n", t']) : xs
mergeMultiLineText c xs = c : xs

parseText :: Parser SectionContent
parseText = do
  -- text <- T.pack <$> manyTill anyChar endOfLine
  text <- takeTill isEndOfLine <* endOfLine
  return $ SectionText text

-- https://orgmode.org/manual/Literal-examples.html
parseSource :: Parser SectionContent
parseSource = do
  lang <- parseBeginSrcLine
  src <- T.pack <$> manyTill anyChar (endOfLine >> parseEndSrcLine)
  return $ Source lang src

parseBeginSrcLine :: Parser (Maybe Text)
parseBeginSrcLine = do
  skipHorizontalSpace
  string "#+BEGIN_SRC"
  lang <- takeWhile1 isHorizontalSpace *> takeTill isSpace
  -- also possible with option but not really nice
  skipToEOL -- skip optional flags
  return $ if lang == "" then Nothing else Just lang

parseEndSrcLine :: Parser ()
parseEndSrcLine = do
  skipHorizontalSpace
  string "#+END_SRC"
  skipSpaceToEOL

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skipToEOL :: Parser ()
skipToEOL = skipWhile (not . isEndOfLine) >> endOfLine

skipSpaceToEOL :: Parser ()
skipSpaceToEOL = skipHorizontalSpace >> endOfLine

-- https://orgmode.org/manual/Comment-lines.html
parseComment :: Parser SectionContent
parseComment = parseLineComments <|> parseMultiLineComment

parseLineComments :: Parser SectionContent
parseLineComments = do
  skipHorizontalSpace
  string "# "
  -- char '#'
  -- satisfy isHorizontalSpace
  -- skipHorizontalSpace
  commentText <- takeTill isEndOfLine
  endOfLine
  return $ Comment commentText


parseMultiLineComment :: Parser SectionContent
parseMultiLineComment = do
  startCommentLine
  commentText <- T.pack <$> manyTill anyChar endCommentLine
  return $ Comment commentText
  where
    startCommentLine = do
      skipHorizontalSpace
      string "#+BEGIN_COMMENT"
      skipHorizontalSpace
      endOfLine
    endCommentLine = do
      endOfLine
      skipHorizontalSpace
      string "#+END_COMMENT"
      skipHorizontalSpace
      endOfLine

parseLine :: Parser Text
parseLine = do
  -- text <- T.pack <$> manyTill anyChar endOfLine
  text <- takeTill isEndOfLine <* endOfLine
  return text
