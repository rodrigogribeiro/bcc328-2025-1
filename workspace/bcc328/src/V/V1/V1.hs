import System.Environment

import Utils.Parser

import V.V1.V1Parser
import V.V1.Interp

-- main function

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  runWithOptions opts

-- running the vm interpreter

runWithOptions :: Option -> IO ()
runWithOptions Help = helpMessage
runWithOptions (FileInput path)
  = do
      content <- readFile path
      result <- codeParser content
      case result of
        Left err -> putStrLn err
        Right ast -> do
               r <- interp ast
               case r of
                  Left err'' -> putStrLn err''
                  Right _    -> return ()

-- command line argument parser

data Option
  = Help
  | FileInput FilePath
  deriving (Eq, Ord, Show)

parseOptions :: [String] -> IO Option
parseOptions args
  = case args of
      ("--f" : path : _) -> pure (FileInput path)
      _                  -> pure Help

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "v1 language"
                       , "Usage: v1 [--f FILE | --help]"
                       , "--f FILE: executes the vm code in FILE."
                       , "--help: prints this help message"
                       ]

