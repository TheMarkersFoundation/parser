module MarkersConverters where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void
import Data.List
import Control.Monad (void)

import AbstractSyntaxTree

toAbnt :: Markers -> String
toAbnt (MarkersMain someString sections) = "<!DOCTYPE html>\
    \<html lang=\"pt-BR\">\
    \<head>\
    \<meta charset=\"UTF-8\">\
    \<title>" <> someString <> "</title>\
    \<style>\
    \  @page {\
    \    size: A4;\
    \    margin: 3cm 2cm 2cm 3cm;\
    \  }\
    \  body {\
    \    font-family: 'Times New Roman', Times, serif;\
    \    font-size: 12pt;\
    \    line-height: 1.5;\
    \    text-align: justify;\
    \    color: #000;\
    \    margin: 0;\
    \    padding: 0;\
    \  }\
    \  .container {\
    \    background-color: white;\
    \  }\
    \  .abnt {\
    \    text-align: center;\
    \    font-size: 12pt;\
    \    line-height: 2;\
    \    margin-top: 0%;\
    \  }\
    \  .abnt .author,\
    \  .abnt .institution,\
    \  .abnt .subtitle,\
    \  .abnt .location,\
    \  .abnt .year {\
    \    margin: 0;\
    \  }\
    \  .abnt .title {\
    \    font-size: 14pt;\
    \    font-weight: bold;\
    \    margin-bottom: 1em;\
    \  }\
    \</style>\
    \</head>\
    \<body>\
    \<div class=\"container\">"
    <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    <> "</div></body></html>"
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content))       = "<p>" <> content <> "</p>"
        helper (Paragraph (Bold content))          = "<p><strong>" <> content <> "</strong></p>"
        helper (Paragraph (Italic content))        = "<p><em>" <> content <> "</em></p>"
        helper (Paragraph (Underlined content))    = "<p><span style=\"text-decoration:underline\">" <> content <> "</span></p>"
        helper (Chap title content) =
            "<h2 style=\"font-weight: bold;\">" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Abnt content) = 
            "<div class=\"abnt\">\n" 
            <> Prelude.foldr (\x acc -> helperTopAbnt x <> acc) "" content 
            <> "<h1 class=\"title\">" <> someString <> "</h1>"
            <> Prelude.foldr (\x acc -> helperBottomAbnt x <> acc) "" content 
            <> "\n</div>"
        helper _ = ""

        helperTopAbnt :: AbntSection -> String
        helperTopAbnt (Institution content) = "<p class=\"institution\" style=\"margin-bottom: 30%\"><b>" <> content <> "</b></p>"
        helperTopAbnt (Author content) = "<p class=\"author\" style=\"margin-bottom: 30%\">" <> content <> "</p>"
        helperTopAbnt (Subtitle content) = ""
        helperTopAbnt (Location content) = ""
        helperTopAbnt (Year content) = ""

        helperBottomAbnt :: AbntSection -> String
        helperBottomAbnt (Author content) = ""
        helperBottomAbnt (Institution content) = ""
        helperBottomAbnt (Subtitle content) = "<p class=\"subtitle\" style=\"margin-top: -40px; margin-bottom: 60%\">" <> content <> "</p>"
        helperBottomAbnt (Location content) = "<p class=\"location\">" <> content <> "</p>"
        helperBottomAbnt (Year content) = "<p class=\"year\">" <> content <> "</p>"



toMarkdown :: Markers -> String
toMarkdown (MarkersMain titulo sections) = "# " <> titulo <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content)) = content
        helper (Paragraph (Bold content)) = "**" <> content <> "**"
        helper (Paragraph (Italic content)) = "*" <> content <> "*"
        helper (Separator)                          = "---"
        helper (Paragraph (BoldItalic content))     = "***" <> content <> "***"
        helper (Paragraph (Underlined content))     = "**" <> content <> "**" -- Não existe no Markdown. Fallback p/ Italico.
        helper (Paragraph (Crossed content))        = "~~" <> content <> "~~"
        helper (Paragraph (CodeInline content))     = "`" <> content <> "`"
        helper (Ref url author title year access content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (List title content) = "passed."
        helper (Chap title content) = "### " <> title <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Link url content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Image url content) = "![" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Code content)
            = "```"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "```"
        helper (LineBreak)
            = "\n"


toHtml :: Markers -> String
toHtml (MarkersMain someString sections) =
    "<!DOCTYPE html>\
    \<html lang=\"en\">\
    \<head>\
    \  <meta charset=\"UTF-8\">\
    \  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \  <title>" <> someString <> "</title>\
    \  <style>\
    \    body {\
    \      margin: 0;\ 
    \      padding: 0;\ 
    \      font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif;\
    \      background-color: #fafafa;\
    \      color: #333;\
    \    }\
    \    \
    \    .container {\
    \      max-width: 800px;\
    \      margin: 0 auto;\
    \      padding: 2em;\
    \      background-color: #fff;\
    \      box-shadow: 0 2px 5px rgba(0,0,0,0.1);\
    \    }\
    \    \
    \    .container h1, .container h2, .container h3 {\
    \      text-align: left;\
    \      margin-top: 1.2em;\
    \      margin-bottom: 0.8em;\
    \    }\
    \    \
    \    .container p {\ 
    \      text-align: justify;\
    \      line-height: 1.6;\
    \      margin: 1em 0;\
    \    } \
    \        details {\
    \            margin-left: 0px; \
    \            margin-top: 10px; \
    \            margin-bottom: 0px; \
    \        } \
    \        summary { \
    \            padding-left: -20px; \
    \            cursor: pointer; \
    \            font-weight: bold; \
    \            padding-top: 5px; \
    \            outline: none; \
    \        } \
    \        details > div, details > details, details > summary { \
    \            margin-left: 10px; \
    \            padding-top: -25px; \
    \        } \
    \    img {\
    \      max-width: 100%;\
    \      height: auto;\
    \      display: block;\
    \      margin: 1em auto;\
    \    }\
    \    \
    \    pre {\
    \      background: #272822;\
    \      color: #f8f8f2;\
    \      padding: 1em;\
    \      overflow-x: auto;\
    \      border-radius: 4px;\
    \      margin: 1em 0;\
    \    }\
    \    code {\
    \      background: #f4f4f4;\
    \      padding: 0.2em 0.4em;\
    \      border-radius: 4px;\
    \    }\
    \  </style>\
    \</head>\
    \<body>\
    \<div class=\"container\">\
    \<h1>" <> someString <> "</h1>"
    <> Prelude.foldr (\x acc -> helper x <> acc) "" sections <>
    "</div>\
    \<script>\n\
    \  document.addEventListener('DOMContentLoaded', () => {\n\
    \    const allDetails = document.querySelectorAll('details');\n\
    \    allDetails.forEach(det => {\n\
    \      det.addEventListener('toggle', () => {\n\
    \      });\n\
    \    });\n\
    \  });\n\
    \document.querySelectorAll('.chapter h2').forEach(h2 => {\n\
        \const level = h2.closest('.chapter').parentElement?.closest('.chapter') ? 2 : 1;\n\
        \if (level === 2) {\n\
            \h2.style.fontSize = '1.2em';\n\
        \} else {\n\
            \h2.style.fontSize = '1.5em';\n\
        \}\n\
    \});\n\
    \</script>\
    \</body>\
    \</html>"
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content))        = content
        helper (Paragraph (Bold content))           = "<strong>" <> content <> "</strong>"
        helper (Paragraph (Italic content))         = "<em>" <> content <> "</em>"
        helper (Paragraph (Underlined content))     = "<span style=\"text-decoration:underline\">" <> content <> "</span>"
        helper (Paragraph (Crossed content))        = "<s>" <> content <> "</s>"
        helper (Paragraph (BoldItalic content))     = "<b><i>" <> content <> "</i></b>"
        helper (Paragraph (CodeInline content))     = "<code>" <> content <> "</code>"
        helper (Paragraph (Color color content))    = "<b><span style=\"color:" <> color <> "\">" <> content <> "</span></b>"
        helper (Separator)                          = "\n\n<br><hr><br>\n\n"

        helper (Ref url author title year access content)
            = "<a href=\"" <> url <> "\">" <> title <> "</a>"

        helper (List title content)
            = "\n<details><summary>\n" <> title <> "\n</summary>\n"
            <> "<div>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>"
            <> "</details>\n"

        helper (Chap title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Image url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (Video url content)
            = "<center><video src=\"" <> url <> "\" style=\"width: 60%\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</video></center>\n"

        helper (Iframe url content)
            = "<center><iframe src=\"" <> url <> "\">\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</iframe></center>\n"

        helper (Audio url content)
            = "<center><audio src=\"" <> url <> "\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</audio></center>\n"

        helper (Code content)
            = "<pre>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</pre>\n"

        helper (LineBreak)
            = "\n<br>\n"

toJson :: Markers -> String
toJson (MarkersMain someString sections) = "{\n\t\"title\": \"" <> escapeJson someString <> "\",\n\t\"main\": [" <> processSections sections <> "\n\t]\n}"    where
    processSections :: [MainSection] -> String
    processSections [] = ""
    processSections sections =
        "\n\t\t" <> Data.List.intercalate ",\n\t\t" (Prelude.map helper sections)

    helper :: MainSection -> String
    helper Empty = "{}"
    helper (Paragraph (Default content)) =
        "{\"defaultText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Bold content)) =
        "{\"boldText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Italic content)) =
        "{\"italicText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Underlined content)) =
        "{\"underlinedText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Crossed content)) =
        "{\"crossedText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (CodeInline content)) =
        "{\"inlineCode\": \"" <> escapeJson content <> "\"}"
    helper (Ref url author title year access content) =
        "{\"reference\": {\"url\": \"" <> escapeJson url 
        <> "\", \"author\": \"" <> escapeJson author 
        <> "\", \"title\": \"" <> escapeJson title 
        <> "\", \"year\": \"" <> escapeJson year 
        <> "\", \"access\": \"" <> escapeJson access 
        <> "\", \"content\": [" <> processSections content <> "]}}"
    helper (List title content) =
        "{\"listTitle\": \"" <> escapeJson title <> "\", \"items\": [" <> processSections content <> "]}"
    helper (Chap title content) =
        "{\"chapterTitle\": \"" <> escapeJson title <> "\", \"chapterContent\": [" <> processSections content <> "]}"
    helper (Link url content) =
        "{\"link\": {\"url\": \"" <> escapeJson url <> "\", \"text\": [" <> processSections content <> "]}}"
    helper (Image url content) =
        "{\"image\": {\"url\": \"" <> escapeJson url <> "\", \"alt\": [" <> processSections content <> "]}}"
    helper (Code content) =
        "{\"code\": [" <> processSections content <> "]}"
    helper LineBreak =
        "{\"lineBreak\": true}"

    escapeJson :: String -> String
    escapeJson = Prelude.concatMap escapeChar
        where
        escapeChar :: Char -> String
        escapeChar c = case c of
            '"'  -> "\\\""
            '\\' -> "\\\\"
            '\n' -> "\\n"
            '\r' -> "\\r"
            '\t' -> "\\t"
            '\b' -> "\\b"
            '\f' -> "\\f"
            x    -> [x]

    removeTrailingComma :: String -> String
    removeTrailingComma s = if not (Prelude.null s) && Prelude.last s == ',' then Prelude.init s else s

