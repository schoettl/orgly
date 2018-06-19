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

data Command =
    PlainCommand Text
  | MusicCommand L.Music
  | Stuff

parseLilypondSource :: Parser [L.Music]
parseLilypondSource = do
  cmds <- many' $ choice
    [ parseStuff
    , parseCommand
    ]
  return $ foldr f [] cmds
  where
    f (MusicCommand m) ms = m:ms
    f _ ms = ms

parseStuff :: Parser Command
parseStuff = skipWhile (/='\\') >> return Stuff

parseCommand :: Parser Command
parseCommand = do
  char '\\'
  parsePlainCommand <|> parseMusicCommand

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
  -- TODO nur für einfache tonarten - fis dur geht noch nicht :(
  -- auch schwierig: deutscher/englischer modus
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
  return $ L.Pitch (pitchName, 0, 0)

parseMode :: Parser L.Mode
parseMode = (string "\\major" >> return L.Major)
        <|> (string "\\minor" >> return L.Minor)
