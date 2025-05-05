import L.L1.Backend.CCodegen
import L.L1.Backend.V0Codegen
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
  [Interpreter] -> 
    repl "L0 repl>" interp showRes
  [VM file] -> do 
    content <- readFile file
    runVMCodegen file content 
  [C file] -> do 
    content <- readFile file
    runCCodegen file content 
  [Exec file] -> do 
    content <- readFile file
    compileWithGCC file content 
  _ -> helpMessage

helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--interp | --vm FILE | --codegen FILE | --help]"
                       , "--interp: starts the REPL for interpreting programs"
                       , "--vm: generates a V0 program from a L1 program"
                       , "--exec: generates executable from a L1 program using GCC" 
                       , "--c: generates C code from a L1 program"
                       , "--help: prints this help message"
                       ]

runVMCodegen :: FilePath -> String -> IO ()
runVMCodegen path content 
  = case lexer content of 
      Left err -> putStrLn err 
      Right tokens -> 
        case l0Parser tokens of 
          Left err -> putStrLn err
          Right ast -> 
            writeVMCode path (v0Codegen ast)

runCCodegen :: FilePath -> String -> IO ()
runCCodegen path content 
  = case lexer content of 
      Left err -> putStrLn err 
      Right tokens -> 
        case l0Parser tokens of 
          Left err -> putStrLn err
          Right ast -> do
            writeCCode path ast
            pure ()

writeCCode :: FilePath -> L1 -> IO FilePath 
writeCCode path ast 
  = do 
      let cfile = path -<.> "c"
          ccode = cL1Codegen ast 
      putStrLn (pretty ast)
      print ast
      writeFile cfile ccode
      pure cfile 

writeVMCode :: FilePath -> Code -> IO ()
writeVMCode path code 
  = do 
      let vmfile = path -<.> "v0"
          vmcode = pretty code
      writeFile vmfile vmcode 

compileWithGCC :: FilePath -> String -> IO ()
compileWithGCC path content 
  = case lexer content of
      Left err -> putStrLn err
      Right tokens -> 
        case l0Parser tokens of
          Left err -> putStrLn err
          Right ast -> do
            cfile <- writeCCode path ast 
            let exname = dropExtension cfile 
                args = [cfile, "-o", exname]
            (exitCode, stdout, stderr) <- readProcessWithExitCode "gcc" args ""
            pure ()

showRes :: Either String Value -> String 
showRes (Right (VInt n)) = show n 
showRes (Left err) = err 

-- parse command line arguments 

data Option 
  = Help 
  | Interpreter 
  | VM FilePath 
  | C FilePath
  | Exec FilePath 
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--interp" : _) -> [Interpreter]
    ("--vm" : arg : _) -> [VM arg]
    ("--c" : arg : _) -> [C arg]
    ("--exec" : arg : _) -> [Exec arg]
    _ -> [Help]
