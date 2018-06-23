module Main where

import OrgLy.Lilypond
import OrgLy.OrgmodeParse

import Prelude hiding (FilePath)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section, subHeadlines), Section (..), Properties (..))
import Data.Maybe (isNothing, isJust, listToMaybe, fromJust, fromMaybe)
import Data.Attoparsec.Text (Parser, parseOnly, parse, maybeResult, asciiCI, endOfInput)
import Text.Heterocephalus
import Text.Blaze.Internal (Markup)
import Text.Blaze (ToMarkup (toMarkup))
import Data.List ((\\))
import qualified Data.HashMap.Lazy as M

import Data.String.Utils (endswith)
import System.IO (stderr)
import System.Exit (die)
import Control.Applicative ((<|>))
import Control.Monad (when, guard)
import Data.Char (toUpper, toLower)
import System.Environment (getArgs)
import System.Console.Docopt
import Shelly

usageText :: Docopt
usageText = [docopt|
usage:
  orgly [-ios] --list
  orgly [-iofbsT] --title=TITLE...
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
  -s, --sublist=TITLE
    Only read the sublist with this title from the orgmode input.
  -h, --help
    print this help message
|]

data Command = Help | Command (Maybe Text) (Maybe FilePath) CommandAction deriving Show
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
    Command _ Nothing _ -> TIO.getContents
    Command _ (Just f) _ -> shelly $ readfile f
    Help -> exitWithUsage usageText
  let Command sublistTitle _ _ = command
  document <- parseOrgmode input
  let Document _ headlines = document
  let sublist = maybe headlines (extractSublist headlines) $ sublistTitle
  let unrolledHeadlines = unrollHeadlines sublist
  case command of
    Command _ _ ListTitles -> do
      mapM_ (TIO.putStrLn . title) unrolledHeadlines
    Command _ _ (CreateTitles format transpose outputFile titles) -> do
      callWithLilypondRequisits titles unrolledHeadlines
        (mapM_ (createOutput format transpose outputFile))
    Command _ _ (CreateTitlesBook format transpose outputFile titles) -> do
      callWithLilypondRequisits titles unrolledHeadlines
        (createBookOutput format transpose outputFile)

callWithLilypondRequisits :: [Text] -> [Headline] -> ([LilypondRequisits] -> IO ()) -> IO ()
callWithLilypondRequisits titles unrolledHeadlines f = do
  selectedTitles <- filterTitles titles unrolledHeadlines
  when (null selectedTitles) $ die "error: no titles to create music from: titles not found."
  titlesWithSource <- getLilypondRequisits selectedTitles
  when (null titlesWithSource) $ die "error: no titles to create music from: missing LilyPond source."
  f titlesWithSource

filterTitles :: [Text] -> [Headline] -> IO [Headline]
filterTitles titles unrolledHeadlines = do
  let selected = filter (\x -> title x `elem` titles) unrolledHeadlines
      notFound = titles \\ map title selected
  mapM_ (putStrLnStderr.warningTextFor) notFound
  return selected
  where
    warningTextFor t = T.concat ["warning: title not found \"", t, "\"."]

getLilypondRequisits :: [Headline] -> IO [LilypondRequisits]
getLilypondRequisits selectedTitles = do
  sources <- mapM extractLilypondSource selectedTitles
  let attributes = map getPieceAttributes selectedTitles
  let titlesWithSource = map (fmap fromJust) $
                           filter (isJust.snd) $
                             zip attributes sources
  return titlesWithSource

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
              sublistTitle = T.pack <$> getArg args (longOption "sublist")
          commandAction <- if isPresent args (longOption "list")
            then return ListTitles
            else do
              let titles = map T.pack $ getAllArgs args (longOption "title")
              let transpose = Transpose $ fmap head $ getArg args (longOption "transpose")
              let outputFile = fmap (fromText . T.pack) $ getArg args (longOption "output-file")
              let book = isPresent args (longOption "book")
              let Just formatStr = getArg args (longOption "format")
              let Right format = parseOnly parseOutputType $ T.pack formatStr

              when ((not . endswith ".ly" . maybe ".ly" (T.unpack . toTextIgnore)) outputFile) $ do
                die "error: filename for --output-file must end with \".ly\"."

              if book
                then do
                  if isNothing outputFile && format == PDF
                    then do
                      die "error: --book and --formt=pdf requires --output-file"
                    else return $ CreateTitlesBook format transpose outputFile titles

                else do
                  if length titles > 1 && isJust outputFile
                    then do
                      putStrLnStderr "warning: ignoring --output-file because of multiple --title"
                      return $ CreateTitles format transpose Nothing titles
                    else do
                      return $ CreateTitles format transpose outputFile titles

          return $ Command sublistTitle maybeInputFile commandAction

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

extractSublist :: [Headline] -> Text -> [Headline]
extractSublist hs t =
  let h = listToMaybe $ filter (\x -> title x == t) hs
   in case h of
     Nothing -> extractSublist (concatMap subHeadlines hs) t
     Just x -> subHeadlines x

extractLilypondSource :: Headline -> IO (Maybe Text)
extractLilypondSource headline = do
  let text = getSectionText headline
  case parseOnly parseSectionParagraph text of
    Right (SectionContents _ contents) -> do
      let source = listToMaybe $ filter isLilypondSource contents
      case source of
        Nothing -> do
          putStrLnStderr $ T.concat ["warning: ignoring title without lilypond source code \"", title headline, "\"."]
          return Nothing
        Just (Source _ src) -> return $ Just src
    Left x -> do
      fail $ "failed to parse '" ++ T.unpack (title headline) ++ "': " ++ x

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

isLilypondSource :: SectionContent -> Bool
isLilypondSource (Source (Just "lilypond") _) = True
isLilypondSource _ = False

