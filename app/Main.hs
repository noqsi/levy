module Main where
import Levy.AST
import Levy.Parse
import Levy.CodeGen
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = 
  do contents <- getContents
     case (parseLevy contents) of
          (Left fail) -> hPutStrLn stderr (show fail) >> exitFailure 
          (Right ast) -> do putStrLn (show ast) 
                            putStrLn (codeGen ast)
