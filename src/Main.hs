module Main(main) where

import System.Environment
import qualified BionetSimulator

main = do
    (inFileName:outFileName:_) <- getArgs
    putStrLn $ "Reading from: " ++ inFileName
    putStrLn $ "Writing to: " ++ outFileName
    input <- readFile inFileName
    writeFile outFileName (BionetSimulator.process input) 
