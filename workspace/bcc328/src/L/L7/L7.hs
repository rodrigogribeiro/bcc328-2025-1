import L.L7.Frontend.Syntax
import Utils.Pretty

import System.Environment
import System.FilePath
import System.Process

import qualified L.L7.Frontend.L7Lexer as L
import L.L7.Frontend.L7Parser
import L.L7.Frontend.Syntax
import L.L7.Frontend.TypeCheck

import L.L7.Interpreter.Interpreter

import Utils.Pretty

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
  [VM file] ->
    v4Compiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L3 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file
  = do
      content <- readFile file
      case L.lexer content of
        Left err -> putStrLn err
        Right tks -> mapM_ print tks


-- Implement the function to do syntax analysis for L3 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file
  = do
      content <- readFile file
      r <- l7Parser content
      case r of
        Left err -> putStrLn err
        Right ast -> putStrLn (pretty ast)

-- Implement the function to do the typechecking for L7 programs and outputs the elaborated syntax tree

typecheckOnly :: FilePath -> IO ()
typecheckOnly file
  = do
      content <- readFile file
      r <- l7Parser content
      case r of
        Left err -> putStrLn err
        Right ast ->
          case typeCheck ast of
            Left err -> putStrLn err
            Right env -> do
              mapM_ (\ (v,t) -> putStrLn $ unwords [pretty v, "::", pretty t]) env


-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file
  = do
      content <- readFile file
      r <- l7Parser content
      case r of
        Left err -> putStrLn err
        Right ast ->
          case typeCheck ast of
            Left err -> putStrLn err
            Right _ -> do
              _ <- l7Interpreter ast
              pure ()


-- Implement the compiler to V3 instructions.

v4Compiler :: FilePath -> IO ()
v4Compiler file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L7 language"
                       , "Usage: l7 [--lexer-only | --parse-only | --interpret | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--typecheck: does the typechecking of the input program."
                       , "--interpret: does the syntax analysis, typechecks and interpret the input program."
                       , "--v4: does the syntax analysis, typechecks and generates code for V4 virtual machine"
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Typecheck FilePath
  | Interpret FilePath
  | VM FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--typecheck" : arg : _) -> [Typecheck arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v4" : arg : _) -> [VM arg]
    _ -> [Help]
