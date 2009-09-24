module Main where

import System
import System.Cmd

import LLL.AST
import LLL.Parser

usage :: IO ()
usage = putStrLn "Usage: lllc input.l3"

printer :: LLLUnit -> IO ()
printer unit = putStrLn $ "AST Dump: " ++ show unit

doCompile filename = runLex program filename filename printer

main = do args <- getArgs
          if length args > 0 then doCompile (args !! 0)
                             else usage
