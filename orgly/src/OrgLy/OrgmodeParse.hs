module OrgLy.OrgmodeParse
  (
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Attoparsec.Text
import Data.Maybe

data Source = Source
  { source :: Text
  , language :: Text
  } deriving Show

main :: IO ()
main = do
  text <- fmap T.pack getContents
  let Right (Document _ headlines) = parseOnly (parseDocument []) text
  -- TODO bei -l
  --mapM_ (putStrLn  . T.unpack . title) headlines
  let t = "Filli-Walzer" -- TODO from command line -t title
  let headline = head $ filter ((==t) . title) headlines
  -- mapM_ (print . getPieceAttributes) $ L.take 1 headlines
  -- print pieceAttributes
  -- print $ parseOnly parseSource text
  let  Right (Source src lang) = parseOnly parseSource text
  -- putStrLn $ T.unpack src
  return ()

parseSource :: Parser Source
parseSource = do
  manyTill anyChar
    (endOfLine *> string "#+BEGIN_SRC")
  lang <- parseSourceLanguage
  src <- T.pack <$> manyTill anyChar
    (endOfLine *> string "#+END_SRC")
  many' anyChar
  endOfInput
  return $ Source src lang

parseSourceLanguage :: Parser Text
parseSourceLanguage = do
  lang <- option "" (many1 (char ' ') *> many1 letter)
  many' $ char ' '
  endOfLine
  return $ T.pack lang
