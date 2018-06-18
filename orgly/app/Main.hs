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
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))

import System.IO (stderr)
import System.Exit (exitFailure)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (toUpper, toLower)
import System.Environment (getArgs)
import System.Console.Docopt
import Shelly

usageText :: Docopt
usageText = [docopt|
usage:
  orgly --list [-io]
  orgly [-iofbT] --title=TITLE...
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
    Write output to this lilypond file instead of creating a file for every
    title. This only applies when multiple titles are given.
  -b, --book
    Create one document for all selected titles. This can save a lot of paper.
  -i, --input-file=FILE
    Read input file instead of stdin.
  -h, --help
    print this help message
|]

data Command = Help | Command (Maybe FilePath) CommandAction deriving Show
data CommandAction =
    ListTitles
  | CreateTitles OutputFormat Transpose (Maybe FilePath) [Text]
  | CreateTitlesBook OutputFormat Transpose (Maybe FilePath) [Text]
  deriving Show

getArgOrExit = getArgOrExitWith usageText

main :: IO ()
main = do
  command <- parseCommandLine
  input <- case command of
    Command Nothing _ -> TIO.getContents
    Command (Just f) _ -> shelly $ readfile f
    Help -> exitWithUsage usageText
  document <- parseOrgmode input
  let Document _ headlines = document
  let unrolledHeadlines = unrollHeadlines headlines
  case command of
    Command _ ListTitles -> do
      mapM_ (TIO.putStrLn . title) unrolledHeadlines
    Command _ (CreateTitles format transpose outputFile titles) -> do
      let selectedTitles = filter (\x -> title x `elem` titles) unrolledHeadlines
      mapM_ (createOutput format transpose outputFile) selectedTitles
    Command _ (CreateTitlesBook format transpose outputFile titles) -> do
      let selectedTitles = filter (\x -> title x `elem` titles) unrolledHeadlines
      createBookOutput format transpose outputFile selectedTitles

parseOrgmode :: Text -> IO Document
parseOrgmode text = do
  let eitherResult  = parseOnly (parseDocument []) text
  case eitherResult of
    Right doc -> return doc
    Left msg -> fail msg

parseCommandLine :: IO Command
parseCommandLine = do
  commandLine <- getArgs
  let parsed = parseArgs usageText commandLine
  case parsed of
    Left p -> exitWithUsageMessage usageText (show p)
    Right args -> do
      if isPresent args (longOption "help")
        then return Help
        else do
          let maybeInputFile = fmap (fromText . T.pack) $ getArg args (longOption "input-file")
          commandAction <- if isPresent args (longOption "list")
            then return ListTitles
            else do
              let titles = map T.pack $ getAllArgs args (longOption "title")
              let transpose = Transpose $ fmap head $ getArg args (longOption "transpose")
              let outputFile = fmap (fromText . T.pack) $ getArg args (longOption "output-file")
              let book = isPresent args (longOption "book")
              let Just formatStr = getArg args (longOption "format")
              let Right format = parseOnly parseOutputType $ T.pack formatStr

              if book
                then do
                  if isNothing outputFile && format == PDF
                    then do
                      putStrLnStderr "error: --book and --formt=pdf requires --output-file"
                      exitFailure
                    else return $ CreateTitlesBook format transpose outputFile titles

                else do
                  if length titles > 1 && isJust outputFile
                    then do
                      putStrLnStderr "warning: ignoring --output-file because of multiple --title"
                      return $ CreateTitles format transpose Nothing titles
                    else do
                      return $ CreateTitles format transpose outputFile titles

          return $ Command maybeInputFile commandAction

parseOutputType :: Parser OutputFormat
parseOutputType = (parsePDFOutputType <|> parseLilyPondOutputType) <* endOfInput

parsePDFOutputType :: Parser OutputFormat
parsePDFOutputType = asciiCI "pdf" >> return PDF

parseLilyPondOutputType :: Parser OutputFormat
parseLilyPondOutputType = do
  asciiCI "lilypond" <|> asciiCI "ly"
  return LilyPond

putStrLnStderr :: Text -> IO ()
putStrLnStderr = TIO.hPutStrLn stderr
