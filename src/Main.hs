module Main(main) where

import System.Environment
import qualified Data.Map as Map
import qualified StringyTools as Bio

main = do
    args <- getArgs
    let argMap = argsToMap args
        sifFileName = argMap Map.! "-sif"
        outFileName = argMap Map.! "-out"
    putStrLn $ "Reading structure from sif: " ++ sifFileName
    putStrLn $ "Writing to: " ++ outFileName
    input <- readFile sifFileName
    let net = Bio.mkNet $ Bio.mkLOE input
    writeFile outFileName (show net)

argsToMap :: [String] -> Map.Map String String
argsToMap list = Map.fromList $ process list where
    process :: [String] -> [(String,String)]
    process [] = []
    process [x] = [(x,[])]
    process (k:v:rest) = ((k,v):process rest)
