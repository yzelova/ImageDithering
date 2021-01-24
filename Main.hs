module Main where

import Algorithms.Atkinson (execute)
import Algorithms.Burkes (execute)
import Algorithms.FalseFloydSteinberg (execute)
import Algorithms.FloydSteinsberg (execute)
import Algorithms.JarvisJudiceNinke (execute)
import Algorithms.OrderedDithering (executeBayer4, executeBayer8)
import Algorithms.Sierra16 (execute)
import Algorithms.Sierra32 (execute)
import Algorithms.Sierra4 (execute)
import Algorithms.Stucki (execute)
import Control.Exception as Exc ( SomeException, catch )
import Control.Monad (unless)

algorithmNames :: String
algorithmNames = "floydSteinberg\nfalseFloydSteinberg\njarvisJudiceNinke\nstucki\natkinson\nburkes\nsierra32\nsierra16\nsierra4\nbayer4\nbayer8\n"

helper :: String -> IO ()
helper str =
  let wordsStr = words str
      inputFileName = head wordsStr
      outputFileName = wordsStr !! 1
      algorithm = wordsStr !! 2
      exec
        | algorithm == "floydSteinberg" = Algorithms.FloydSteinsberg.execute inputFileName outputFileName
        | algorithm == "falseFloydSteinberg" = Algorithms.FalseFloydSteinberg.execute inputFileName outputFileName
        | algorithm == "jarvisJusiceNinke" = Algorithms.JarvisJudiceNinke.execute inputFileName outputFileName
        | algorithm == "stucki" = Algorithms.Stucki.execute inputFileName outputFileName
        | algorithm == "atkinson" = Algorithms.Atkinson.execute inputFileName outputFileName
        | algorithm == "burkes" = Algorithms.Burkes.execute inputFileName outputFileName
        | algorithm == "sierra32" = Algorithms.Sierra32.execute inputFileName outputFileName
        | algorithm == "sierra16" = Algorithms.Sierra16.execute inputFileName outputFileName
        | algorithm == "sierra4" = Algorithms.Sierra4.execute inputFileName outputFileName
        | algorithm == "bayer4" = Algorithms.OrderedDithering.executeBayer4 inputFileName outputFileName
        | algorithm == "bayer8" = Algorithms.OrderedDithering.executeBayer8 inputFileName outputFileName
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
