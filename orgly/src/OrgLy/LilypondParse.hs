module OrgLy.LilypondParse
  ( parseLilypondSource
  ) where

import Data.Attoparsec.Text
import Data.Music.Lilypond (Music (Key, Time, Clef))

parseLilypondSource :: Parser [Music]
parseLilypondSource = many' $ choice
  [ parseKey
  , parseTime
  , parseClef
  , parseOther
  ]

parseKey :: Parser Music
parseKey = undefined

parseTime :: Parser Music
parseTime = undefined

parseClef :: Parser Music
parseClef = undefined

parseOther :: Parser Music
parseOther = undefined
