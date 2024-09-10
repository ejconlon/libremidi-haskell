module Main
  ( main
  )
where

import Data.Foldable (traverse_)
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as TIO
import Libremidi.Api qualified as LMA
import Libremidi.Simple qualified as LMS

main :: IO ()
main = do
  let lf = LMS.stderrLogFun
  putStrLn "* In ports:"
  inPorts <- LMS.listInPorts lf
  traverse_ (TIO.putStrLn . fst) inPorts
  putStrLn "* Out ports:"
  outPorts <- LMS.listOutPorts lf
  traverse_ (TIO.putStrLn . fst) outPorts
  putStrLn "* Opening first out port"
  let op = snd (Seq.index outPorts 0)
  oh <- LMS.openOutPort lf op
  putStrLn "* Connected"
  LMA.freeOutHandle oh
  putStrLn "* Freed"
