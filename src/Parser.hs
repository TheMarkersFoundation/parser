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

parseFileToHtml :: IO ()
parseFileToHtml = do
    file <- readFile "src/teste.mks"
    case parse parseMarkers "src/teste.mks" file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile "src/saida.html" (toHtml res)

parseFileToJSON :: IO ()
parseFileToJSON = do
    file <- readFile "src/teste.mks"
    case parse parseMarkers "src/teste.mks" file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile "src/saida.json" (toJson res)

parseFileToMarkdown :: IO ()
parseFileToMarkdown = do
    file <- readFile "src/teste.mks"
    case parse parseMarkers "src/teste.mks" file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile "src/saida.md" (toMarkdown res)
