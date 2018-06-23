module OrgLy.Lilypond
  ( insertChordSettings
  , OutputFormat (..)
  , Transpose (..)
  , PieceAttributes (..)
  , LilypondStringLiteral (..)
  , LilypondRequisits
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
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))
import qualified Text.Blaze as B
import Shelly
import Data.Attoparsec.Text (parseOnly)
import System.IO (stderr)
import Data.List (intersperse)

-- for simple regex string substitution
import Text.Regex (Regex, subRegex)
import Text.Regex.Quote

newtype LilypondStringLiteral = LilypondStringLiteral
  { unLilypondStringLiteral :: Text
  } deriving Show

newtype LilypondSource = LilypondSource Text
  deriving Show

instance ToMarkup LilypondStringLiteral where
  toMarkup (LilypondStringLiteral s) = B.preEscapedText $ T.concat ["\"", escaped, "\""]
    where escaped = T.replace "\"" "\\\"" . T.replace "\\" "\\\\" $ s

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

type LilypondRequisits = (PieceAttributes, Text)

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
      piece = #{combinedTitle attrs}
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

createOutput :: OutputFormat -> Transpose -> Maybe FilePath -> LilypondRequisits -> IO ()
createOutput PDF t f a = createLilypond t a >>= createPdf (getFilename f a)
createOutput LilyPond t Nothing a = createLilypond t a >>= TIO.putStrLn
createOutput LilyPond t (Just f) a = createLilypond t a >>= shelly . writefile f

getFilename :: Maybe FilePath -> LilypondRequisits -> FilePath
getFilename f (a, _) = fromMaybe (fromText $ T.concat [(unLilypondStringLiteral.paTitle) a, ".ly"]) f

createBookOutput :: OutputFormat -> Transpose -> Maybe FilePath -> [LilypondRequisits] -> IO ()
createBookOutput PDF t (Just f) hs = createBookLilypond t hs >>= createPdf f
createBookOutput LilyPond t Nothing hs = createBookLilypond t hs >>= TIO.putStrLn
createBookOutput LilyPond t (Just f) hs = createBookLilypond t hs >>= shelly . writefile f
createBookOutput _ _ _ _ = fail "you found a bug - errornous call to createBookOutput"

createLilypond :: Transpose -> LilypondRequisits -> IO Text
createLilypond (Transpose transpose) (attrs, source) = do
  let src = LilypondSource $ insertChordSettings source
  -- TODO parse source to get key for transpose
  let (Right music) = parseOnly parseLilypondSource source
  let (L.Key pitch@(L.Pitch (pitchName, _, _)) _) = head $ filter isKey music
  print pitch
  print pitchName
  -- TODO prettyprint (prettify -> Text.Pretty
  -- TODO pass Nothing for transpose if (show pitchName) == transpose
  returnRenderedMarkup $ compileLilypondTemplate attrs (LilypondSource source) transpose

isKey :: L.Music -> Bool
isKey (L.Key _ _) = True
isKey _ = False

createBookLilypond :: Transpose -> [LilypondRequisits] -> IO Text
createBookLilypond (Transpose transpose) headlines = do
  -- TODO parse sources to get key for transpose
  let pieces = map (fmap $ LilypondSource . insertChordSettings) headlines
  returnRenderedMarkup $ compileLilypondBookTemplate pieces transpose

createPdf :: FilePath -> Text -> IO ()
createPdf outputFile lilypond = do
  shelly $ do
    writefile outputFile lilypond
    run_ "lilypond" [toTextIgnore outputFile]

putStrLnStderr :: Text -> IO ()
putStrLnStderr = TIO.hPutStrLn stderr

returnRenderedMarkup = return . Data.Text.Lazy.toStrict . renderMarkup

combinedTitle :: PieceAttributes -> LilypondStringLiteral
combinedTitle attrs =
  let list = map ((\(LilypondStringLiteral x) -> x) . ($attrs))
              [ paComposer
              , getArranger
              ]
      filteredList = filter (/="") list
      extra = if null filteredList then [] else [" ("] ++ intersperse ", " filteredList ++ [")"]
   in LilypondStringLiteral $ T.concat $ title : extra
  where
    LilypondStringLiteral title = paTitle attrs
    LilypondStringLiteral arranger = paArranger attrs
    LilypondStringLiteral composer = paComposer attrs

getArranger :: PieceAttributes -> LilypondStringLiteral
getArranger =
  fmapLilypondStringLiteral
      (\x -> if x == "" then "" else T.concat ["Arr. ", x])
  . paArranger

fmapLilypondStringLiteral :: (Text -> Text) -> LilypondStringLiteral -> LilypondStringLiteral
fmapLilypondStringLiteral f (LilypondStringLiteral x) = LilypondStringLiteral $ f x
