module Main where

import OrgLy.Lilypond
import OrgLy.OrgmodeParse

import qualified Data.Text as T
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Maybe
import Data.Attoparsec.Text
import Text.Heterocephalus
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))

import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docopt|
orgly version 0.1.0

usage:
  orgly cat <file>
  orgly echo [--caps] <string>

options:
  -c, --caps    Caps-lock the echoed argument
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  text <- fmap T.pack getContents
  let Right (Document _ headlines) = parseOnly (parseDocument []) text
  -- TODO bei -l
  --mapM_ (putStrLn  . T.unpack . title) headlines
  let t = "Filli-Walzer" -- TODO from command line -t title
  let headline = head $ filter ((==t) . title) headlines
  --mapM_ (putStrLn . T.unpack . getSectionText) headlines
  -- mapM_ (print . getPieceAttributes) $ L.take 1 headlines
  let pieceAttributes = getPieceAttributes headline
  -- print pieceAttributes
  let text = getSectionText headline
  -- print $ parseOnly parseSource text
  let  Right (Source src lang) = parseOnly parseSectionParagraph text
  -- putStrLn $ T.unpack src
  let src' = insertChordSettings src
  putStrLn $ renderMarkup $ compileLilypondTemplate pieceAttributes (LilypondSource src') (Just 'a')
