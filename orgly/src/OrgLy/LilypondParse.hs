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

data Command =
    PlainCommand Text
  | MusicCommand L.Music
  deriving Show

parseLilypondSource :: Parser [L.Music]
parseLilypondSource = do
  cmds <- many' (skipWhile (/='\\') >> parseCommand)
  return $ foldr f [] cmds
  where
    f (MusicCommand m) ms = m:ms
    f _ ms = ms

parseCommand :: Parser Command
parseCommand = do
  char '\\'
  parseMusicCommand <|> parsePlainCommand

parsePlainCommand :: Parser Command
parsePlainCommand = do
  cmd <- takeWhile isAlpha
  return $ PlainCommand cmd

parseMusicCommand :: Parser Command
parseMusicCommand = MusicCommand <$> parseKey

parseKey :: Parser L.Music
parseKey = do
  string "key"
  space
  skipWhile isSpace
  pitch <- parsePitch
  space
  skipWhile isSpace
  mode <- parseMode
  return $ L.Key pitch mode

parsePitch :: Parser L.Pitch
parsePitch = do
  -- TODO auch schwierig: deutscher/englischer modus
  -- aktuell: engischer modus, aber h-dur funktioniert auch
  c <- letter
  let pitchName = case c of
        'c' -> L.C
        'd' -> L.D
        'e' -> L.E
        'f' -> L.F
        'g' -> L.G
        'a' -> L.A
        'b' -> L.B
        'h' -> L.B
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
