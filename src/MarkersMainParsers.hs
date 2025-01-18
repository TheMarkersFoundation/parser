module MarkersMainParsers where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Control.Monad (void)

import AbstractSyntaxTree
import MarkersParagraphParsers

parseMainContent :: Parser MainSection
parseMainContent = parseChap <|> parseRef <|> parseList <|> parseLink <|> parseImage <|> parseCode <|> parseAbnt <|> parseContent

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

parseAbntContent :: Parser AbntSection
parseAbntContent = parseAuthor <|> parseInstitution <|> parseSubtitle <|> parseLocation <|> parseYear

parseAbnt :: Parser MainSection
parseAbnt = do
    _ <- string "(abnt)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseAbntContent (string "(/abnt)")
    _ <- many (char ' ' <|> char '\n')
    return (Abnt content)

parseAuthor :: Parser AbntSection
parseAuthor = do
    _ <- string "(author)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/author)")
    _ <- many (char ' ' <|> char '\n')
    return (Author content)

parseInstitution :: Parser AbntSection
parseInstitution = do
    _ <- string "(institution)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/institution)")
    _ <- many (char ' ' <|> char '\n')
    return (Institution content)

parseSubtitle :: Parser AbntSection
parseSubtitle = do
    _ <- string "(subtitle)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/subtitle)")
    _ <- many (char ' ' <|> char '\n')
    return (Subtitle content)

parseLocation :: Parser AbntSection
parseLocation = do
    _ <- string "(location)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/location)")
    _ <- many (char ' ' <|> char '\n')
    return (Location content)

parseYear :: Parser AbntSection
parseYear = do
    _ <- string "(year)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/year)")
    _ <- many (char ' ' <|> char '\n')
    return (Year content)