module Main where

import OrgLy.Lilypond
import OrgLy.OrgmodeParse

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Maybe
import Data.Attoparsec.Text (Parser, parseOnly, parse, maybeResult, asciiCI, endOfInput)
import Text.Heterocephalus
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (toUpper, toLower)
import System.Environment (getArgs)
import System.Console.Docopt

usageText :: Docopt
usageText = [docopt|
orgly version 0.1.0

usage:
  orgly [-io] --list
  orgly [-iof] --title=TITLE...
  orgly --help

options:
  -l, --list
    list all titles from orgmode input
  -t, --title=TITLE
    create PDF from title
  -f, --format=FORMAT
    output format, one of: lilypond, pdf [default: pdf]
  -o, --output-file=FILE
  -i, --input-file=FILE
  -h, --help
    print this help message
|]

data Command = Command (Maybe FilePath) CommandAction deriving Show
data CommandAction = ListTitles | CreateTitles OutputFormat [String] deriving Show
data OutputFormat = LilyPond | PDF deriving Show

getArgOrExit = getArgOrExitWith usageText

main :: IO ()
main = do
  command <- parseCommandLine
  input <- case command of
    Command Nothing _ -> TIO.getContents
    Command (Just f) _ -> TIO.readFile f
  document <- parseOrgmode input
  let Document _ headlines = document
  case command of
    Command _ ListTitles -> do
      mapM_ (putStrLn  . T.unpack . title) headlines
    Command _ (CreateTitles format titles) -> do
      let ts = map T.pack titles
      mapM_ (createOutput format) $ filter (\x -> title x `elem` ts) headlines
    _ -> putStrLn "command not yet implemented"

parseOrgmode :: Text -> IO Document
parseOrgmode text = do
  let eitherResult  = parseOnly (parseDocument []) text
  case eitherResult of
    Right doc -> return doc
    Left msg -> fail msg

createOutput :: OutputFormat -> Headline -> IO ()
createOutput PDF h = createPdf h
createOutput LilyPond headline = do
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

createPdf :: Headline -> IO ()
createPdf headline = do
  text <- createOutput LilyPond headline
  -- use shelly to execute lilypond on the generated output
  return ()

parseCommandLine :: IO Command
parseCommandLine = do
  args <- parseArgsOrExit usageText =<< getArgs
  let maybeInputFile = getArg args (longOption "input-file")
  commandAction <- if isPresent args (longOption "list")
    then return ListTitles
    else do
      let titles = getAllArgs args (longOption "title")
      let Just formatStr = getArg args (longOption "format")
      let Right format = parseOnly parseOutputType $ T.pack formatStr
      return $ CreateTitles format titles
  return $ Command maybeInputFile commandAction

parseOutputType :: Parser OutputFormat
parseOutputType = (parsePDFOutputType <|> parseLilyPondOutputType) <* endOfInput

parsePDFOutputType :: Parser OutputFormat
parsePDFOutputType = asciiCI "pdf" >> return PDF

parseLilyPondOutputType :: Parser OutputFormat
parseLilyPondOutputType = do
  asciiCI "lilypond" <|> asciiCI "ly"
  return LilyPond
