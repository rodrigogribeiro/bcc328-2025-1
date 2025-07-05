

import L.L3.Frontend.Syntax
import Utils.Pretty

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
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Typecheck file] ->
    typecheckOnly file
  [Interpret file] ->
    interpret file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L3 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file = error "Not implemented!"


-- Implement the function to do syntax analysis for L3 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = error "Not implemented!"

-- Implement the function to do the typechecking for L3 programs and outputs the elaborated syntax tree

typecheckOnly :: FilePath -> IO ()
typecheckOnly file = error "Not implemented!"

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L3 language"
                       , "Usage: l3 [--lexer-only | --parse-only | --interpret | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--typecheck: does the typechecking of the input program."
                       , "--interpret: does the syntax analysis, typechecks and interpret the input program."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Typecheck FilePath
  | Interpret FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--typecheck" : arg : _) -> [Typecheck arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    _ -> [Help]
