module Main where

import OrgLy.Lilypond
import OrgLy.OrgmodeParse

import Prelude hiding (FilePath)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Maybe
import Data.Attoparsec.Text (Parser, parseOnly, parse, maybeResult, asciiCI, endOfInput)
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (toUpper, toLower)
import System.Environment (getArgs)
import System.Console.Docopt
import Shelly

usageText :: Docopt
usageText = [docopt|
orgly version 0.1.0

usage:
  orgly [-io] --list
  orgly [-iofT] --title=TITLE...
  orgly --help

options:
  -l, --list
    List all titles from orgmode input
  -t, --title=TITLE
    Create PDF from title
  -f, --format=FORMAT
    Output format, one of: lilypond, pdf [default: pdf]
  -T, --transpose=TARGET_KEY
    Transpose music to TARGET_KEY assuming it is originally in c major or a
    minor respectively. TARGET_KEY must be a lower case letter and a valid
    pitch.
  -o, --output-file=FILE
  -i, --input-file=FILE
  -h, --help
    print this help message
|]

data Command = Command (Maybe FilePath) CommandAction deriving Show
data CommandAction = ListTitles | CreateTitles OutputFormat Transpose [String] deriving Show
data OutputFormat = LilyPond | PDF deriving Show
data Transpose = Transpose (Maybe Char) deriving Show

getArgOrExit = getArgOrExitWith usageText

main :: IO ()
main = do
  command <- parseCommandLine
  input <- case command of
    Command Nothing _ -> TIO.getContents
    Command (Just f) _ -> shelly $ readfile f
  document <- parseOrgmode input
  let Document _ headlines = document
  case command of
    Command _ ListTitles -> do
      mapM_ (putStrLn  . T.unpack . title) headlines
    Command _ (CreateTitles format transpose titles) -> do
      let ts = map T.pack titles
      mapM_ (createOutput format transpose) $ filter (\x -> title x `elem` ts) headlines
    _ -> putStrLn "command not yet implemented"

parseOrgmode :: Text -> IO Document
parseOrgmode text = do
  let eitherResult  = parseOnly (parseDocument []) text
  case eitherResult of
    Right doc -> return doc
    Left msg -> fail msg

createOutput :: OutputFormat -> Transpose -> Headline -> IO ()
createOutput PDF t h = createPdf t h
createOutput LilyPond t h = createLilypond t h >>= TIO.putStrLn

createLilypond :: Transpose -> Headline -> IO Text
createLilypond (Transpose transpose) headline = do
  --mapM_ (putStrLn . T.unpack . getSectionText) headlines
  -- mapM_ (print . getPieceAttributes) $ L.take 1 headlines
  let pieceAttributes = getPieceAttributes headline
  -- print pieceAttributes
  let text = getSectionText headline
  -- print $ parseOnly parseSource text
  let  Right (Source src lang) = parseOnly parseSectionParagraph text
  -- putStrLn $ T.unpack src
  let src' = insertChordSettings src
  return $ L.toStrict $ renderMarkup $ compileLilypondTemplate pieceAttributes (LilypondSource src') transpose

createPdf :: Transpose -> Headline -> IO ()
createPdf transpose headline = do
  text <- createLilypond transpose headline
  let name = T.concat [title headline, ".ly"]
  shelly $ do
    writefile (fromText name) text
    setStdin text
    run_ "lilypond" [name]

parseCommandLine :: IO Command
parseCommandLine = do
  args <- parseArgsOrExit usageText =<< getArgs
  let maybeInputFile = fmap (fromText . T.pack) $ getArg args (longOption "input-file")
  commandAction <- if isPresent args (longOption "list")
    then return ListTitles
    else do
      let titles = getAllArgs args (longOption "title")
      let transpose = Transpose $ fmap head $ getArg args (longOption "transpose")
      let Just formatStr = getArg args (longOption "format")
      let Right format = parseOnly parseOutputType $ T.pack formatStr
      return $ CreateTitles format transpose titles
  return $ Command maybeInputFile commandAction

parseOutputType :: Parser OutputFormat
parseOutputType = (parsePDFOutputType <|> parseLilyPondOutputType) <* endOfInput

parsePDFOutputType :: Parser OutputFormat
parsePDFOutputType = asciiCI "pdf" >> return PDF

parseLilyPondOutputType :: Parser OutputFormat
parseLilyPondOutputType = do
  asciiCI "lilypond" <|> asciiCI "ly"
  return LilyPond
