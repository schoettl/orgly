
-- required packages: split orgmode-parse

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.OrgMode.Parse
import Data.OrgMode.Types (Document (..), Headline (title, section), Section (..), Properties (..))
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import Data.List.Split

data PieceAttributes = PieceAttributes
    { paTitle       :: String
    , paSubtitle    :: String
    , paSubsubtitle :: String
    , paInstrument  :: String
    , paComposer    :: String
    , paPoet        :: String
    , paArranger    :: String
    , paDedication  :: String
    , paMeter       :: String
    } deriving Show


main :: IO ()
main = do
    text <- fmap T.pack getContents
    let Right (Document _ headlines) = parseOnly (parseDocument []) text
    mapM_ (putStrLn . T.unpack . getSectionText) headlines
    mapM_ (putStrLn . show . getPieceAttributes) headlines

getSectionText :: Headline -> T.Text
getSectionText = sectionParagraph . section

getPieceAttributes :: Headline -> PieceAttributes
getPieceAttributes headline = PieceAttributes
    { paTitle       = T.unpack $ title headline
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

getProperty :: T.Text -> M.HashMap T.Text T.Text -> String
getProperty property = maybe "" T.unpack . M.lookup property
