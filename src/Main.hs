module Main where
    
import Parser (parseFileToHtml, parseFileToMarkdown, parseFileToJSON)
import System.Environment (getArgs)

printHelp :: IO ()
printHelp = putStrLn "Usage: mks [-html <file-name> | -markdown <file-name> | -md <file-name> | -json <file-name> | -help | -h]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-html", filePath] -> parseFileToHtml filePath
        ["-markdown", filePath] -> parseFileToMarkdown filePath
        ["-md", filePath] -> parseFileToMarkdown filePath
        ["-json", filePath] -> parseFileToJSON filePath
        ["-help"] -> printHelp
        ["-h"] -> printHelp
        _ -> printHelp
        