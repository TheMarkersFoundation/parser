module Main where

import Markers
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as L
import System.IO (withFile, IOMode(..), hSetEncoding, utf8, hGetContents, hPutStr)

printHelp :: IO ()
printHelp = putStrLn "Usage: mks [-html <file-name> | -markdown <file-name> | -md <file-name> | -abnt <file-name> | -help | -h]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-raw", filePath] -> processFile filePath (replaceExtension filePath ".html") convertToRaw
        ["-html", filePath] -> processFile filePath (replaceExtension filePath ".html") convertToHtml
        ["-markdown", filePath] -> processFile filePath (replaceExtension filePath ".md") convertToMarkdown
        ["-md", filePath] -> processFile filePath (replaceExtension filePath ".md") convertToMarkdown
        ["-abnt", filePath] -> processFile filePath (replaceExtension filePath ".html") convertToAbnt
        ["-help"] -> printHelp
        ["-h"] -> printHelp
        _ -> printHelp

processFile :: FilePath -> FilePath -> (String -> String) -> IO ()
processFile input output converter = 
    withFile input ReadMode $ \inh -> do
        hSetEncoding inh utf8
        content <- hGetContents inh
        withFile output WriteMode $ \outh -> do
            hSetEncoding outh utf8
            hPutStr outh (converter content)