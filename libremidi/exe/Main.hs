module Main
  ( main
  )
where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as TIO
import Libremidi.Simple qualified as LMS
import Data.Sequence qualified as Seq

main :: IO ()
main = do
  putStrLn "In ports:"
  inPorts <- LMS.listInPorts
  traverse_ (TIO.putStrLn . fst) inPorts
  putStrLn "Out ports:"
  outPorts <- LMS.listOutPorts
  traverse_ (TIO.putStrLn . fst) outPorts
  let op = snd (Seq.index outPorts 0)
  print op
  oh <- LMS.openOutPort op
  putStrLn "Connected"
