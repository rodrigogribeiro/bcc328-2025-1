module Utils.Repl where 

import System.IO 

read_ :: String -> IO String 
read_ s 
  = do 
      putStr s 
      hFlush stdout 
      getLine

eval_ :: (String -> a) -> String -> IO a
eval_ f s = pure (f s)

print_ :: (a -> String) -> a -> IO ()
print_ f x = putStrLn (f x)

repl :: String -> 
        (String -> a) ->
        (a -> String) -> 
        IO () 
repl prompt evf pf 
  = do 
      input <- read_ prompt 
      if input == ":quit" 
      then return ()
      else do  
        r <- eval_ evf input 
        print_ pf r 
        repl prompt evf pf 
