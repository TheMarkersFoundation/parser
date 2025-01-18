module MarkersParagraphParsers where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Control.Monad (void)

import AbstractSyntaxTree

type Parser = Parsec Void String

-- -------------------------------------------------------------------------------------------------

{- 
    [ parseDefault ]
    Parser responsável por reconhecer um parágrafo padrão.
    Um parágrafo padrão é um parágrafo que não possui nenhuma formatação.
    Exemplo: "Olá, mundo!"

    Ele verifica se existe alguma tag de formatação no parágrafo.
    Caso exista, ele para de reconhecer o parágrafo e retorna o conteúdo que foi reconhecido até o momento.

    Exemplo:
    parseDefault "Olá, mundo! (b)Isso é um texto em negrito!(/b)" == Default "Olá, mundo!"
-}

parseDefault :: Parser MainSection
parseDefault = do
  content <- someTill anySingle $
               void (lookAhead $
                 string "(c)"       <|>
                 string "(u)"       <|>
                 string "(b)"       <|>
                 string "(i)"       <|>
                 string "(k)"       <|>
                 string "(/ref)"    <|>
                 string "(ref "     <|>
                 string "(>> |"     <|>
                 string "(/>>)"     <|>
                 string "(chap |"   <|>
                 string "(/chap)"   <|>
                 string "(link |"   <|>
                 string "(/link)"   <|>
                 string "(img |"    <|>
                 string "(/img)"    <|> 
                 string "(code)"    <|>
                 string "(/code)"   <|>
                 string "(hr)"      <|>
                 string "\n")
               <|> eof
  return (Paragraph (Default content))

-- -------------------------------------------------------------------------------------------------

{- 

    [ parseCrossed ]
    Parser responsável por reconhecer um parágrafo com texto riscado.
    Exemplo: "(c)Isso é um texto riscado!(/c)" --> Crossed "Isso é um texto riscado!"

-}

parseCrossed :: Parser MainSection
parseCrossed = do
    _ <- string "(c)"
    content <- manyTill anySingle (string "(/c)")
    return (Paragraph (Crossed content))


-- -------------------------------------------------------------------------------------------------

{- 

    [ parseUnderlined ]
    Parser responsável por reconhecer um parágrafo com texto sublinhado.
    Exemplo: "(u)Isso é um texto sublinhado!(/u)" --> Underlined "Isso é um texto sublinhado!"

-}

parseUnderlined :: Parser MainSection
parseUnderlined = do
    _ <- string "(u)"
    content <- manyTill anySingle (string "(/u)")
    return (Paragraph (Underlined content))
    

-- -------------------------------------------------------------------------------------------------

{-
    
    [ parseBold ]
    Parser responsável por reconhecer um parágrafo com texto em negrito.
    Exemplo: "(b)Isso é um texto em negrito!(/b)" --> Bold "Isso é um texto em negrito!"

-}

parseBold :: Parser MainSection
parseBold = do
    _ <- string "(b)"
    content <- manyTill anySingle (string "(/b)")
    return (Paragraph (Bold content))


-- -------------------------------------------------------------------------------------------------

{-

    [ parseItalic ]
    Parser responsável por reconhecer um parágrafo com texto em itálico.
    Exemplo: "(i)Isso é um texto em itálico!(/i)" --> Italic "Isso é um texto em itálico!"

-}

parseItalic :: Parser MainSection
parseItalic = do
    _ <- string "(i)"
    content <- manyTill anySingle (string "(/i)")
    return (Paragraph (Italic content))

-- -------------------------------------------------------------------------------------------------

{-

    [ parseInlineCode ]
    Parser responsável por reconhecer um parágrafo com código inline.
    Exemplo: "(k)Isso é um código inline!(/k)" --> CodeInline "Isso é um código inline!"

-}

parseInlineCode :: Parser MainSection
parseInlineCode = do
    _ <- string "(k)"
    content <- manyTill anySingle (string "(/k)")
    return (Paragraph (CodeInline content))

parseForceDefault :: Parser MainSection
parseForceDefault = do
    _ <- string "(p)"
    content <- manyTill anySingle (string "(/p)")
    return (Paragraph (Default content))

parseLineBreak :: Parser MainSection
parseLineBreak = do
    _ <- newline
    return LineBreak

parseSeparator :: Parser MainSection
parseSeparator = do
    _ <- string "(hr)"
    return Separator

-- -------------------------------------------------------------------------------------------------

{-

    [ parseParagraph ]
    Parser responsável por reconhecer um parágrafo inteiro.
    Um parágrafo pode conter várias formatações, por este motivo ele retorna um [Paragraph].
    Exemplo: "(b)Isso é um texto em negrito!(/b) (i)Isso é um texto em itálico!(/i)" 
       --> [Bold "Isso é um texto em negrito!", Italic "Isso é um texto em itálico!"]

    Ele faz isso chamando o parseContent várias vezes até que não exista mais formatações no parágrafo.
    A função parseContent é responsável por reconhecer uma formatação específica. 
    Ela é chamada várias vezes pelo parseParagraph e tenta encontrar uma das formatações definidas no parágrafo.

    Exemplo:
        parseContent "(b)Isso é um texto em negrito!(/b) (i)Isso é um texto em itálico!(/i)" 
        -> Bold "Isso é um texto em negrito!"

    Quando chamamos com o many, fazemos isso várias vezes e vamos acumulando as formatações reconhecidas.
    É isso o que a função parseParagraph faz.

-}

parseParagraph :: Parser [MainSection]
parseParagraph = many parseContent

parseContent :: Parser MainSection
parseContent =  parseSeparator <|> parseLineBreak <|> parseBold <|> parseItalic <|> parseCrossed <|> parseUnderlined <|> parseInlineCode <|> parseForceDefault <|> parseDefault

-- -------------------------------------------------------------------------------------------------

{-

    [parseParagraphTill]
    Uma função auxiliar que reconhece um parágrafo até encontrar uma string específica.
    Ela é usada para reconhecer um parágrafo até encontrar uma tag de fechamento. (Ex: "(/ref)")

-}

parseParagraphTill :: String -> Parser [MainSection]
parseParagraphTill st = manyTill parseContent (lookAhead (string st))

