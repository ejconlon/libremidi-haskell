module Main
  ( main
  )
where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as TIO
import Libremidi.Simple qualified as LMS

main :: IO ()
main = do
  putStrLn "In ports:"
  inPorts <- LMS.listInPorts
  traverse_ (TIO.putStrLn . fst) inPorts
  putStrLn "Out ports:"
  outPorts <- LMS.listInPorts
  traverse_ (TIO.putStrLn . fst) outPorts
