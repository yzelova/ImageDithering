module Main where

import Atkinson (execute)
import Burkes (execute)
import Control.Exception as Exc
import Control.Monad (unless)
import FalseFloydSteinberg (execute)
import FloydSteinsberg (execute)
import JarvisJudiceNinke (execute)
import OrderedDithering (executeBayer4, executeBayer8)
import Sierra16 (execute)
import Sierra32 (execute)
import Sierra4 (execute)
import Stucki (execute)

algorithmNames :: String
algorithmNames = "floydSteinberg\nfalseFloydSteinberg\njarvisJudiceNinke\nstucki\natkinson\nburkes\nsierra32\nsierra16\nsierra4\nbayer4\nbayer8\n"

helper :: String -> IO ()
helper str =
  let wordsStr = words str
      inputFileName = head wordsStr
      outputFileName = wordsStr !! 1
      algorithm = wordsStr !! 2
      exec
        | algorithm == "floydSteinberg" = FloydSteinsberg.execute inputFileName outputFileName
        | algorithm == "falseFloydSteinberg" = FalseFloydSteinberg.execute inputFileName outputFileName
        | algorithm == "jarvisJusiceNinke" = JarvisJudiceNinke.execute inputFileName outputFileName
        | algorithm == "stucki" = Stucki.execute inputFileName outputFileName
        | algorithm == "atkinson" = Atkinson.execute inputFileName outputFileName
        | algorithm == "burkes" = Burkes.execute inputFileName outputFileName
        | algorithm == "sierra32" = Sierra32.execute inputFileName outputFileName
        | algorithm == "sierra16" = Sierra16.execute inputFileName outputFileName
        | algorithm == "sierra4" = Sierra4.execute inputFileName outputFileName
        | algorithm == "bayer4" = OrderedDithering.executeBayer4 inputFileName outputFileName
        | algorithm == "bayer8" = OrderedDithering.executeBayer8 inputFileName outputFileName
        | otherwise = error "Invalid command"
   in exec

parseCommand :: String -> IO ()
parseCommand ":names" = putStr algorithmNames
parseCommand str
  | length (words str) /= 3 = putStrLn "Invalid command\n"
  | otherwise = do
    let process = helper str
    return process ()
    putStrLn "Done!"

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Exc.catch

handler :: SomeException -> IO ()
handler _ = putStrLn "An Error Occured" 

processInputUser :: IO ()
processInputUser = do
  line <- getLine
  unless (line == ":quit") $ do
    catchAny (parseCommand line) handler
    processInputUser

main :: IO ()
main = do
  putStrLn "Enter input file name, output file name and dithering algorithm name to be applied:"
  putStrLn "Enter :names to view supported dithering algorithm names"
  putStrLn "Enter :quit to exit"
  processInputUser
