module Main where

import CodeGen.LispToJs
import CodeGen.JsToStr
import Parser
import TypeChecker
import Data.List
import System.Directory
import System.Environment
import Interpret
import Ast (Program)
import Data.Foldable

doc :: [String]
doc = [ "Usage: alisp <filename> [OPTIONS]"
      , ""
      , "  <filename> should end in .lisp"
      , ""
      , "OPTIONS:"
      , "  -i: Interpret <filename> (default is not options given)"
      , "  -c: Compile source to JavaScript and print result"
      , ""
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] ->
      filename ??> runFile (runIO . interpret) filename
    [filename, "-i"] ->
      filename ??> runFile (runIO . interpret) filename
    [filename, "-c"] ->
      filename ??> runFile (putStrLn . jsToString . genCode) filename
    _ -> showDoc

(??>) :: FilePath -> IO () -> IO ()
filename ??> a = if ".lisp" `isSuffixOf` filename
                   then do
                     fileExists <- doesFileExist filename
                     if fileExists
                       then a
                       else putStrLn $ filename ++ " was not found"
                   else showDoc

runIO :: IO (Maybe String) -> IO ()
runIO x = do
    y <- x
    forM_ y putStrLn

showDoc :: IO ()
showDoc = mapM_ putStrLn doc

runFile :: (Program -> IO ()) -> String -> IO ()
runFile f filename = do
    source <- readFile filename
    case parse source of
      Right ast -> if typed ast
                     then f ast
                     else putStrLn "Type error"
      Left e -> print e
