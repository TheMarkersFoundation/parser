module Main where
    
import Markers
import System.Environment (getArgs)
import System.FilePath (replaceExtension)

printHelp :: IO ()
printHelp = putStrLn "Usage: mks [-html <file-name> | -markdown <file-name> | -md <file-name> | -abnt <file-name> | -help | -h]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-raw", filePath] -> do
            content <- readFile filePath
            writeFile (replaceExtension filePath ".html") (convertToRaw content)
        ["-html", filePath] -> do
            content <- readFile filePath
            writeFile (replaceExtension filePath ".html") (convertToHtml content)
        ["-markdown", filePath] -> do
            content <- readFile filePath
            writeFile (replaceExtension filePath ".md") (convertToMarkdown content)
        ["-md", filePath] -> do
            content <- readFile filePath
            writeFile (replaceExtension filePath ".md") (convertToMarkdown content)
        ["-abnt", filePath] -> do
            content <- readFile filePath
            writeFile (replaceExtension filePath ".abnt") (convertToAbnt content)
        ["-help"] -> printHelp
        ["-h"] -> printHelp
        _ -> printHelp
