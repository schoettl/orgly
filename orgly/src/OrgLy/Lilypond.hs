module OrgLy.Lilypond
  ( insertChordSettings
  , compileLilypondTemplate
  , LilypondSource (..)
  , OutputFormat (..)
  , Transpose (..)
  , getSectionText
  , getPieceAttributes
  , createOutput
  , createLilypond
  , createPdf
  ) where

import Prelude hiding (FilePath)
import OrgLy.OrgmodeParse
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))
import qualified Text.Blaze as B
import Shelly
import Data.Attoparsec.Text (parseOnly)

newtype LilypondStringLiteral = LilypondStringLiteral Text
  deriving Show

newtype LilypondSource = LilypondSource Text
  deriving Show

instance ToMarkup LilypondStringLiteral where
  toMarkup (LilypondStringLiteral s) = B.preEscapedText $ T.concat ["\"", escaped, "\""]
    where escaped = T.replace "\"" "\\\"" s

instance ToMarkup LilypondSource where
  toMarkup (LilypondSource s) = B.preEscapedText s

data OutputFormat = LilyPond | PDF deriving Show
data Transpose = Transpose (Maybe Char) deriving Show

data PieceAttributes = PieceAttributes
  { paTitle       :: LilypondStringLiteral
  , paSubtitle    :: LilypondStringLiteral
  , paSubsubtitle :: LilypondStringLiteral
  , paInstrument  :: LilypondStringLiteral
  , paComposer    :: LilypondStringLiteral
  , paPoet        :: LilypondStringLiteral
  , paArranger    :: LilypondStringLiteral
  , paDedication  :: LilypondStringLiteral
  , paMeter       :: LilypondStringLiteral
  } deriving Show

compileLilypondTemplate :: PieceAttributes -> LilypondSource -> Maybe Char -> Markup
compileLilypondTemplate attrs source transpose = [compileText|

\version "2.18.2"
\language "deutsch"

\header {
  title       = #{paTitle       attrs}
  subtitle    = #{paSubtitle    attrs}
  subsubtitle = #{paSubsubtitle attrs}
  instrument  = #{paInstrument  attrs}
  composer    = #{paComposer    attrs}
  poet        = #{paPoet        attrs}
  arranger    = #{paArranger    attrs}
  dedication  = #{paDedication  attrs}
  meter       = #{paMeter       attrs}
  tagline = ##f
}

% This must be inserted after the \chords in source below!
% Done in advance to the template rendering.
%\set chordNameLowercaseMinor = ##t
%\germanChords

%{ if isJust transpose }
\transpose c #{fromJust transpose} {
%{ endif }

#{source}

%{ if isJust transpose }
}
%{ endif }
|]

insertChordSettings :: Text -> Text
insertChordSettings = T.replace "\\chords {" "\\chords {\n\\set chordNameLowercaseMinor = ##t\n\\germanChords"

getPieceAttributes :: Headline -> PieceAttributes
getPieceAttributes headline = PieceAttributes
  { paTitle       = LilypondStringLiteral $ title headline
  , paSubtitle    = getProperty "subtitle" m
  , paSubsubtitle = getProperty "subsubtitle" m
  , paInstrument  = getProperty "instrument" m
  , paComposer    = getProperty "composer" m
  , paPoet        = getProperty "poet" m
  , paArranger    = getProperty "arranger" m
  , paDedication  = getProperty "dedication" m
  , paMeter       = getProperty "meter" m
  }
  where
    m = unProperties $ sectionProperties $ section headline

getProperty :: Text -> M.HashMap Text Text -> LilypondStringLiteral
getProperty property = LilypondStringLiteral . fromMaybe "" . M.lookup property

getSectionText :: Headline -> Text
getSectionText = sectionParagraph . section

createOutput :: OutputFormat -> Transpose -> Maybe FilePath -> Headline -> IO ()
createOutput PDF t f h = createPdf t f h
createOutput LilyPond t Nothing h = createLilypond t h >>= TIO.putStrLn
createOutput LilyPond t (Just f) h = createLilypond t h >>= shelly . writefile f

createLilypond :: Transpose -> Headline -> IO Text
createLilypond (Transpose transpose) headline = do
  --mapM_ (putStrLn . T.unpack . getSectionText) headlines
  -- mapM_ (print . getPieceAttributes) $ L.take 1 headlines
  let pieceAttributes = getPieceAttributes headline
  -- print pieceAttributes
  let text = getSectionText headline
  -- print $ parseOnly parseSource text
  let  Right (Source src lang) = parseOnly parseSectionParagraph text
  -- putStrLn $ T.unpack src
  let src' = insertChordSettings src
  return $ L.toStrict $ renderMarkup $ compileLilypondTemplate pieceAttributes (LilypondSource src') transpose

createPdf :: Transpose -> Maybe FilePath -> Headline -> IO ()
createPdf transpose outputFile headline = do
  text <- createLilypond transpose headline
  let name = fromMaybe (T.concat [title headline, ".ly"])
                       (fmap toTextIgnore outputFile)
  shelly $ do
    writefile (fromText name) text
    setStdin text
    run_ "lilypond" [name]

