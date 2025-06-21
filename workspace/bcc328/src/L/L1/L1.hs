import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer 
import L.L1.Frontend.Parser
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
  _ -> helpMessage


-- Implement the function to do lexical analysis for L1 programs

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = error "Not implemtented!"


-- Implement the function to do syntax analysis using a recursive parser

recursiveParser :: FilePath -> IO ()
recursiveParser file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--lexer-only | --recursive | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments 

data Option 
  = Help 
  | Lexer FilePath
  | Recursive FilePath 
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    _ -> [Help]
