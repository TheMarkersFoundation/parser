module Parser (
    parseFileToHtml,
    parseFileToMarkdown,
    parseFileToJSON,
    parseFileToAbnt
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Control.Monad (void)

import AbstractSyntaxTree
import MarkersParagraphParsers
import MarkersMainParsers
import MarkersConverters

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeBaseName, replaceExtension)

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

parseFileToHtml :: FilePath -> IO ()
parseFileToHtml filePath = do
    currentDir <- getCurrentDirectory
    let baseName = takeBaseName filePath
        outputFile = currentDir </> (baseName ++ ".html")

    file <- readFile filePath
    case parse parseMarkers filePath file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile outputFile (toHtml res)

parseFileToJSON :: FilePath -> IO ()
parseFileToJSON filePath = do
    currentDir <- getCurrentDirectory
    let baseName = takeBaseName filePath
        outputFile = currentDir </> (baseName ++ ".json")

    file <- readFile filePath
    case parse parseMarkers filePath file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile outputFile (toJson res)

parseFileToMarkdown :: FilePath -> IO ()
parseFileToMarkdown filePath = do
    currentDir <- getCurrentDirectory
    let baseName = takeBaseName filePath
        outputFile = currentDir </> (baseName ++ ".md")

    file <- readFile filePath
    case parse parseMarkers filePath file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile outputFile (toMarkdown res)

parseFileToAbnt :: FilePath -> IO ()
parseFileToAbnt filePath = do
    currentDir <- getCurrentDirectory
    let baseName = takeBaseName filePath
        outputFile = currentDir </> (baseName ++ ".html")

    file <- readFile filePath
    case parse parseMarkers filePath file of
        Left err -> putStr (errorBundlePretty err)
        Right res -> writeFile outputFile (toAbnt res)