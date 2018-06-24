module OrgLy.LilypondParse
  ( parseLilypondSource
  ) where

import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import qualified Data.Music.Lilypond as L
import Data.Char (isAlpha, isSpace)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (nub)

data LyObject =
    PlainCommand Text
  | MusicCommand L.Music
  | Comment
  deriving Show

parseLilypondSource :: Parser [L.Music]
parseLilypondSource = do
  cmds <- many' $ (skipWhile $ notInClass "\\%") >> (parseCommand <|> parseComment)
  return $ foldr f [] cmds
  where
    f (MusicCommand m) ms = m:ms
    f _ ms = ms

parseComment :: Parser LyObject
parseComment = takeTill isEndOfLine >> endOfLine >> return Comment

parseCommand :: Parser LyObject
parseCommand = do
  char '\\'
  parseMusicCommand <|> parsePlainCommand

parsePlainCommand :: Parser LyObject
parsePlainCommand = do
  cmd <- takeWhile isAlpha
  return $ PlainCommand cmd

parseMusicCommand :: Parser LyObject
parseMusicCommand = MusicCommand <$> parseKey

parseKey :: Parser L.Music
parseKey = do
  string "key"
  space
  skipWhile isSpace
  pitch <- parsePitchDe
  space
  skipWhile isSpace
  mode <- parseMode
  return $ L.Key pitch mode

-- | Parse pitch in simplified "deutsch" mode
parsePitchDe :: Parser L.Pitch
parsePitchDe = do
  s <- takeTill (\c -> isSpace c || inClass ",'" c)
  let pitch = case s of
        "c"   -> (L.C, 0)
        "ces" -> (L.C, -1)
        "cis" -> (L.C, 1)
        "d"   -> (L.D, 0)
        "des" -> (L.D, -1)
        "dis" -> (L.D, 1)
        "e"   -> (L.E, 0)
        "es"  -> (L.E, -1)
        "eis" -> (L.E, 1)
        "f"   -> (L.F, 0)
        "fes" -> (L.F, -1)
        "fis" -> (L.F, 1)
        "g"   -> (L.G, 0)
        "ges" -> (L.G, -1)
        "gis" -> (L.G, 1)
        "a"   -> (L.A, 0)
        "as"  -> (L.A, -1)
        "ais" -> (L.A, 1)
        "h"   -> (L.B, 0)
        "b"   -> (L.B, -1)
        "his" -> (L.B, 1)
  octaves <- many' (char ',' <|> char '\'')
  when ((length octaves > 5) || ((length.nub) octaves > 1)) $
    fail "invalid octave prefix to pitch"
  let octave = toOctave octaves
  let (pitchName, accidental) = pitch
  return $ L.Pitch (pitchName, accidental, octave)

parsePitch :: Parser L.Pitch
parsePitch = do
  c <- letter
  let pitchName = case c of
        'c' -> L.C
        'd' -> L.D
        'e' -> L.E
        'f' -> L.F
        'g' -> L.G
        'a' -> L.A
        'b' -> L.B
  accidentals <- many' (string "es" <|> string "is")
  when ((length accidentals > 2) || ((length.nub) accidentals > 1)) $
    fail "invalid accidental prefix to pitch name"
  let accidental = toAccidental accidentals
  octaves <- many' (char ',' <|> char '\'')
  when ((length octaves > 5) || ((length.nub) octaves > 1)) $
    fail "invalid octave prefix to pitch"
  let octave = toOctave octaves
  return $ L.Pitch (pitchName, accidental, octave)

toAccidental :: [Text] -> L.Accidental
toAccidental as
  | null as = 0
  | head as == "es" = - length as
  | head as == "is" =   length as

toOctave :: [Char] -> L.Octaves
toOctave os
  | null os = 0
  | head os == ','  = - length os
  | head os == '\'' =   length os

parseMode :: Parser L.Mode
parseMode = (string "\\major" >> return L.Major)
        <|> (string "\\minor" >> return L.Minor)
