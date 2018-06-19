module OrgLy.Lilypond
  ( insertChordSettings
  , OutputFormat (..)
  , Transpose (..)
  , createOutput
  , createBookOutput
  ) where

import Prelude hiding (FilePath)
import OrgLy.OrgmodeParse
import OrgLy.LilypondParse (parseLilypondSource)
import qualified Data.Music.Lilypond as L
import qualified Data.Text as T
import qualified Data.Text.Lazy
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
import System.IO (stderr)

-- for simple regex string substitution
import Text.Regex (Regex, subRegex)
import Text.Regex.Quote

newtype LilypondStringLiteral = LilypondStringLiteral Text
  deriving Show

newtype LilypondSource = LilypondSource Text
  deriving Show

instance ToMarkup LilypondStringLiteral where
  toMarkup (LilypondStringLiteral s) = B.preEscapedText $ T.concat ["\"", escaped, "\""]
    where escaped = T.replace "\"" "\\\"" s

instance ToMarkup LilypondSource where
  toMarkup (LilypondSource s) = B.preEscapedText s

data OutputFormat = LilyPond | PDF deriving (Show, Eq)
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

compileLilypondBookTemplate :: [(PieceAttributes, LilypondSource)] -> Maybe Char -> Markup
compileLilypondBookTemplate pieces transpose = [compileText|

\version "2.18.2"
\language "deutsch"

\header {
  tagline = ##f
}

\book {

  %{ forall (attrs, src) <- pieces }
  \score {
    \header {
      piece = #{ paTitle attrs }
    }

    %{ if isJust transpose }
    \transpose c #{fromJust transpose} {
    %{ endif }

    #{src}

    %{ if isJust transpose }
    }
    %{ endif }

  }
  %{ endforall }

}
|]

insertChordSettings :: Text -> Text
-- insertChordSettings = T.replace "\\chords {" "\\chords {\n\\set chordNameLowercaseMinor = ##t\n\\germanChords\n"
insertChordSettings input = T.pack $ subRegex
                              [r|\\chords[[:space:]]*\{|]
                              (T.unpack input)
                              "\\chords {\n\\set chordNameLowercaseMinor = ##t\n\\germanChords\n"

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
createOutput PDF t f h = createLilypond t h >>= createPdf (getFilename f h)
createOutput LilyPond t Nothing h = createLilypond t h >>= TIO.putStrLn
createOutput LilyPond t (Just f) h = createLilypond t h >>= shelly . writefile f

getFilename :: Maybe FilePath -> Headline -> FilePath
getFilename f h = fromMaybe (fromText $ T.concat [title h, ".ly"]) f

createBookOutput :: OutputFormat -> Transpose -> Maybe FilePath -> [Headline] -> IO ()
createBookOutput PDF t (Just f) hs = createBookLilypond t hs >>= createPdf f
createBookOutput LilyPond t Nothing hs = createBookLilypond t hs >>= TIO.putStrLn
createBookOutput LilyPond t (Just f) hs = createBookLilypond t hs >>= shelly . writefile f
createBookOutput _ _ _ _ = fail "you found a bug - errornous call to createBookOutput"

createLilypond :: Transpose -> Headline -> IO Text
createLilypond (Transpose transpose) headline = do
  let pieceAttributes = getPieceAttributes headline
  let text = getSectionText headline
  source <- insertChordSettings <$> fromMaybe "" <$> extractLilypondSource headline
  -- TODO parse source to get key for transpose
  let (Right music) = parseOnly parseLilypondSource source
  let (L.Key (L.Pitch (pitchName, 0, 0)) _) = head $ filter isKey music
  -- TODO pass Nothing for transpose if (show pitchName) == transpose
  returnRenderedMarkup $ compileLilypondTemplate pieceAttributes (LilypondSource source) transpose

isKey :: L.Music -> Bool
isKey (L.Key _ _) = True
isKey _ = False

isLilypondSource :: SectionContent -> Bool
isLilypondSource (Source (Just "lilypond") _) = True
isLilypondSource _ = False

createBookLilypond :: Transpose -> [Headline] -> IO Text
createBookLilypond (Transpose transpose) headlines = do
  sources <- mapM (fmap (fromMaybe "") . extractLilypondSource) headlines
  -- TODO parse sources to get key for transpose
  let pieces = zip (map getPieceAttributes headlines) (map (LilypondSource . insertChordSettings) sources)
  returnRenderedMarkup $ compileLilypondBookTemplate pieces transpose

extractLilypondSource :: Headline -> IO (Maybe Text)
extractLilypondSource headline = do
  let text = getSectionText headline
  case parseOnly parseSectionParagraph text of
    Right (SectionContents _ contents) -> do
      let source = listToMaybe $ filter isLilypondSource contents
      case source of
        Nothing -> do
          putStrLnStderr "warning: ignoring title without lilypond source code"
          return Nothing
        Just (Source _ src) -> return $ Just src
    Left x -> do
      fail $ "failed to parse '" ++ T.unpack (title headline) ++ "': " ++ x

createPdf :: FilePath -> Text -> IO ()
createPdf outputFile lilypond = do
  shelly $ do
    writefile outputFile lilypond
    run_ "lilypond" [toTextIgnore outputFile]

putStrLnStderr :: Text -> IO ()
putStrLnStderr = TIO.hPutStrLn stderr

returnRenderedMarkup = return . Data.Text.Lazy.toStrict . renderMarkup
