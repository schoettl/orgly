module OrgLy.Lilypond
  ( insertChordSettings
  , OutputFormat (..)
  , CollectionType (..)
  , TransposeTo
  , PieceAttributes (..)
  , LilypondStringLiteral (..)
  , LilypondRequisits
  , createOutput
  , createCollectionOutput
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
import Data.Char (toLower)
import System.IO (stderr)
import Filesystem.Path (directory)

-- for simple regex string substitution
import Text.Regex (Regex, subRegex)
import Text.Regex.Quote

lilypondVersion = LilypondStringLiteral "2.18.2"

data CollectionType = Collection | Book
  deriving Show

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

\version #{lilypondVersion}
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

compileLilypondCollectionTemplate :: [(PieceAttributes, Transpose, LilypondSource)] -> Markup
compileLilypondCollectionTemplate pieces = [compileText|

\version #{lilypondVersion}
\language "deutsch"

\header {
  tagline = ##f
}

%{ forall (attrs, trans, src) <- pieces }
  \markup { #{combinedTitle attrs} }

  %{ if isJust trans }
  \transpose #{fst $ fromJust trans} #{snd $ fromJust trans} {
  %{ endif }

  #{src}

  %{ if isJust trans }
  }
  %{ endif }

%{ endforall }

|]

compileLilypondBookTemplate :: [(PieceAttributes, Transpose, LilypondSource)] -> Markup
compileLilypondBookTemplate pieces = [compileText|

\version #{lilypondVersion}
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

insertTranspose :: Transpose -> Text -> Text
insertTranspose t input = maybe input transpose t
                where
                  transpose :: (Char, Char) -> Text
                  transpose (x, y) = T.pack
                                   . sub [r|^<<\s*(%.*)?$|] ("\\transpose " ++ [x] ++ " " ++ [y] ++ " {\n<<")
                                   . sub [r|^>>\s*(%.*)?$|] ">>\n}"
                                   . T.unpack
                                   $ input
                  sub r i s = subRegex r s i

createOutput :: OutputFormat -> TransposeTo -> Maybe FilePath -> LilypondRequisits -> IO ()
createOutput PDF t f r = createLilypond t r >>= createPdf (getFilename f r)
createOutput LilyPond t Nothing r = createLilypond t r >>= TIO.putStrLn
createOutput LilyPond t (Just f) r = createLilypond t r >>= shelly . writefile f

getFilename :: Maybe FilePath -> LilypondRequisits -> FilePath
getFilename f (a, _) = fromMaybe (fromText $ T.concat [(unLilypondStringLiteral.paTitle) a, ".ly"]) f

createCollectionOutput :: CollectionType -> OutputFormat -> TransposeTo -> Maybe FilePath -> [LilypondRequisits] -> IO ()
createCollectionOutput ct PDF t (Just f) req = createCollectionLilypond ct t req >>= createPdf f
createCollectionOutput ct LilyPond t Nothing req = createCollectionLilypond ct t req >>= TIO.putStrLn
createCollectionOutput ct LilyPond t (Just f) req = createCollectionLilypond ct t req >>= shelly . writefile f
createCollectionOutput _ _ _ _ _ = fail "you found a bug - errornous call to createCollectionOutput"

createLilypond :: TransposeTo -> LilypondRequisits -> IO Text
createLilypond transpose (attrs, source) = do
  tr <- tryParseLilypondAndMakeTranspose transpose source
  let src = prepareSource tr source
  returnRenderedMarkup $ compileLilypondTemplate attrs src tr

tryParseLilypondAndMakeTranspose :: TransposeTo -> Text -> IO Transpose
tryParseLilypondAndMakeTranspose Nothing _ = return Nothing
tryParseLilypondAndMakeTranspose (Just b) src = do
  case parseOnly parseLilypondSource src of
    Right music -> do
      let a = maybe b getPitchName $ listToMaybe $ filter isKey music
      return $ if b == a
        then Nothing
        else Just (a, b)
    Left msg -> do
      putStrLnStderr $ T.pack msg
      putStrLnStderr "warning: failed to parse LilyPond. assuming \\key c \\major."
      return Nothing
  where
    getPitchName :: L.Music -> Char
    getPitchName = (\(L.Key (L.Pitch (pitchName, _, _)) _)
                        -> toLower $ head $ show pitchName)

isKey :: L.Music -> Bool
isKey (L.Key _ _) = True
isKey _ = False

createCollectionLilypond :: CollectionType -> TransposeTo -> [LilypondRequisits] -> IO Text
createCollectionLilypond collectionType transpose requisits = do
  trs <- mapM (tryParseLilypondAndMakeTranspose transpose) $ map snd requisits
  let pieces = map (\((x, y), z) -> (x, z, prepareSource z y)) $ zip requisits trs
  returnRenderedMarkup $ case collectionType of
    Collection -> compileLilypondCollectionTemplate pieces
    Book       -> compileLilypondBookTemplate pieces

prepareSource :: Transpose -> Text -> LilypondSource
prepareSource transpose = LilypondSource . insertChordSettings . insertTranspose transpose

createPdf :: FilePath -> Text -> IO ()
createPdf outputFile lilypond = do
  shelly $ do
    writefile outputFile lilypond
    cd $ directory outputFile
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

putStrLnStderr :: Text -> IO ()
putStrLnStderr = TIO.hPutStrLn stderr

