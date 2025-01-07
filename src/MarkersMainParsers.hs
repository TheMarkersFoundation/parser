module MarkersMainParsers where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Control.Monad (void)

import AbstractSyntaxTree
import MarkersParagraphParsers

parseMainContent :: Parser MainSection
parseMainContent = parseChap <|> parseRef <|> parseList <|> parseLink <|> parseImage <|> parseCode <|> parseContent

parseJustParagraph :: String -> Parser [MainSection]
parseJustParagraph st = manyTill parseContent (lookAhead (string st))

parseStrictDefault :: String -> Parser [MainSection]
parseStrictDefault st = manyTill parseDefault (lookAhead (string st))


parseRef :: Parser MainSection
parseRef = do
    _ <- string "(ref |"
    url    <- manyTill anySingle (string " | ")
    author <- manyTill anySingle (string " | ")
    title  <- manyTill anySingle (string " | ")
    year   <- manyTill anySingle (string " | ")
    access <- manyTill anySingle (string ")")
    content <- parseParagraphTill "(/ref)"
    _ <- string "(/ref)"
    return (Ref url author title year access content)

parseList :: Parser MainSection
parseList = do
    _     <- string "(>> |"
    title <- manyTill anySingle (string ")")
    content <- parseListBody "(/>>)"
    _     <- string "(/>>)"
    return (List title content)

    where
    parseListBody :: String -> Parser [MainSection]
    parseListBody stopMark =
        manyTill parseMainContent (lookAhead (string stopMark))

parseChap :: Parser MainSection
parseChap = do
    _     <- string "(chap |"
    title <- manyTill anySingle (string ")")
    content <- parseListBody "(/chap)"
    _     <- string "(/chap)"
    return (Chap title content)

    where
    parseListBody :: String -> Parser [MainSection]
    parseListBody stopMark =
        manyTill parseMainContent (lookAhead (string stopMark))

parseLink :: Parser MainSection
parseLink = do
    _ <- string "(link | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/link)"
    _ <- string "(/link)"
    return (Link url content)

parseImage :: Parser MainSection
parseImage = do
    _ <- string "(img | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/img)"
    _ <- string "(/img)"
    return (Image url content)
    
parseCode :: Parser MainSection
parseCode = do
    _ <- string "(code)"
    content <- parseStrictDefault "(/code)"
    _ <- string "(/code)"
    return (Code content)
    