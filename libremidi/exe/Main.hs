module Main
  ( main
  )
where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as TIO
import Libremidi.Easy qualified as E

main :: IO ()
main = do
  putStrLn "In ports:"
  inPorts <- E.listInPorts
  traverse_ (TIO.putStrLn . fst) inPorts
  putStrLn "Out ports:"
  outPorts <- E.listInPorts
  traverse_ (TIO.putStrLn . fst) outPorts
