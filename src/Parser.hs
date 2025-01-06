{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Control.Monad (void)

import AbstractSyntaxTree
import MarkersParagraphParsers
import MarkersMainParsers
import MarkersConverters


parseTitle :: Parser String
parseTitle = do
    _ <- string "(title)"
    titleText <- manyTill anySingle (string "(/title)")
    return titleText

parseMarkers :: Parser Markers
parseMarkers = do
    title <- parseTitle
    content <- manyTill parseMainContent eof
    return (MarkersMain title content)

parseFile :: IO ()
parseFile = do
    file <- readFile "src/documentacao.mks"
    case parse parseMarkers "src/documentacao.mks" file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile "src/saida.html" (toHtml res)