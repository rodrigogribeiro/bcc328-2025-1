import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr

import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] ->
    recursiveParser file
  [LALR file] ->
    lalrParser file
  [VM file] ->
    v1Compiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L1 programs

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = error "Not implemtented!"


-- Implement the function to do syntax analysis using a recursive parser

recursiveParser :: FilePath -> IO ()
recursiveParser file = error "Not implemented!"

-- Implement the LALR parser

lalrParser :: FilePath -> IO ()
lalrParser file = error "Not implemented!"

-- Implement the V1 code generator

v1Compiler :: FilePath -> IO ()
v1Compiler file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L1 language"
                       , "Usage: l1 [--lexer-only FILE | --recursive FILE  | --lalr FILE | -- v1 FILE | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a LALR parser."
                       , "--v1: Translate L1 code into V1 instructions."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath
  | VM FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [LALR arg]
    ("--v1" : arg : _) -> [VM arg]
    _ -> [Help]
