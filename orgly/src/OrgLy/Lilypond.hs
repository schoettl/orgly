module OrgLy.Lilypond
  ( insertChordSettings
  , compileLilypondTemplate
  , LilypondSource (..)
  , getSectionText
  , getPieceAttributes
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import Text.Heterocephalus
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))
import qualified Text.Blaze as B

newtype LilypondStringLiteral = LilypondStringLiteral Text
  deriving Show

newtype LilypondSource = LilypondSource Text
  deriving Show

instance ToMarkup LilypondStringLiteral where
  toMarkup (LilypondStringLiteral s) = B.preEscapedText $ T.concat ["\"", escaped, "\""]
    where escaped = T.replace "\"" "\\\"" s

instance ToMarkup LilypondSource where
  toMarkup (LilypondSource s) = B.preEscapedText s

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

data Source = Source
  { source :: Text
  , language :: Text
  } deriving Show

compileLilypondTemplate :: PieceAttributes -> LilypondSource -> Maybe Char -> Markup
compileLilypondTemplate attrs source transpose = [compileText|

\version "2.18."
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

%{ case transpose }
%{ of Just x }
\transpose c #{x} {
%{ endcase }

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
