module OrgLy.Lilypond
  ( insertChordSettings
  , OutputFormat (..)
  , TransposeTo
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

type TransposeTo = Maybe Char
type Transpose = (Maybe (Char, Char))

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

compileLilypondTemplate :: PieceAttributes -> LilypondSource -> Transpose -> Markup
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
\transpose #{fst $ fromJust transpose} #{snd $ fromJust transpose} {
%{ endif }

#{source}

%{ if isJust transpose }
}
%{ endif }
|]

compileLilypondBookTemplate :: [(PieceAttributes, Transpose, LilypondSource)] -> Markup
compileLilypondBookTemplate pieces = [compileText|

\version "2.18.2"
\language "deutsch"

\header {
  tagline = ##f
}

\book {

  %{ forall (attrs, trans, src) <- pieces }
  \score {
    \header {
      piece = #{combinedTitle attrs}
    }

    %{ if isJust trans }
    \transpose #{fst $ fromJust trans} #{snd $ fromJust trans} {
    %{ endif }

    #{src}

    %{ if isJust trans }
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

createOutput :: OutputFormat -> TransposeTo -> Maybe FilePath -> LilypondRequisits -> IO ()
createOutput PDF t f r = createLilypond t r >>= createPdf (getFilename f r)
createOutput LilyPond t Nothing r = createLilypond t r >>= TIO.putStrLn
createOutput LilyPond t (Just f) r = createLilypond t r >>= shelly . writefile f

getFilename :: Maybe FilePath -> LilypondRequisits -> FilePath
getFilename f (a, _) = fromMaybe (fromText $ T.concat [(unLilypondStringLiteral.paTitle) a, ".ly"]) f

createBookOutput :: OutputFormat -> TransposeTo -> Maybe FilePath -> [LilypondRequisits] -> IO ()
createBookOutput PDF t (Just f) req = createBookLilypond t req >>= createPdf f
createBookOutput LilyPond t Nothing req = createBookLilypond t req >>= TIO.putStrLn
createBookOutput LilyPond t (Just f) req = createBookLilypond t req >>= shelly . writefile f
createBookOutput _ _ _ _ = fail "you found a bug - errornous call to createBookOutput"

createLilypond :: TransposeTo -> LilypondRequisits -> IO Text
createLilypond transpose (attrs, source) = do
  let src = LilypondSource $ insertChordSettings source
  -- TODO parse source to get key for transpose
  let (Right music) = parseOnly parseLilypondSource source
  let (L.Key pitch@(L.Pitch (pitchName, _, _)) _) = head $ filter isKey music
  print pitch
  print pitchName
  let tr = Just ('c', 'a')
  -- TODO prettyprint (prettify -> Text.Pretty
  -- TODO pass Nothing for transpose if (show pitchName) == transpose
  returnRenderedMarkup $ compileLilypondTemplate attrs (LilypondSource source) tr

isKey :: L.Music -> Bool
isKey (L.Key _ _) = True
isKey _ = False

createBookLilypond :: TransposeTo -> [LilypondRequisits] -> IO Text
createBookLilypond transpose requisits = do
  -- TODO parse sources to get key for transpose
  -- let (Right music) = parseOnly parseLilypondSource source
  -- let (L.Key pitch@(L.Pitch (pitchName, _, _)) _) = head $ filter isKey music
  -- print pitch
  -- print pitchName
  let trs = repeat $ Just ('c', 'a')
  let pieces = map (fmap $ LilypondSource . insertChordSettings) requisits
      pieces' = map (\((x, y), z) -> (x, z, y)) $ zip pieces trs
  returnRenderedMarkup $ compileLilypondBookTemplate pieces'

createPdf :: FilePath -> Text -> IO ()
createPdf outputFile lilypond = do
  shelly $ do
    writefile outputFile lilypond
    run_ "lilypond" [toTextIgnore outputFile]

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
