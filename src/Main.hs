module Main where

import CodeGen.LispToJs
import CodeGen.JsToStr
import Parser
import TypeChecker
import Data.List
import System.Directory
import System.Environment

doc :: [String]
doc = [ "Usage: alisp <filename>"
      , ""
      , "  <filename> should end in .lisp"
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] ->
      if ".lisp" `isSuffixOf` filename
      then runFile filename
      else showDoc
    _ -> showDoc

showDoc :: IO ()
showDoc = mapM_ putStrLn doc

runFile :: String -> IO ()
runFile filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then do
      source <- readFile filename
      case parse source of
        Right ast -> if typed ast
                       then putStrLn $ jsToString $ genCode ast
                       else putStrLn "Type error"
        Left e -> print e
    else putStrLn $ show filename ++ " was not found"
