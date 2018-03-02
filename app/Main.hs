module Main where
import Levy.AST
import Levy.Parse

main :: IO ()
main = 
  do contents <- getContents
     let ast = parseLevy contents
     putStrLn (show ast)
