module Main
  ( main
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as TIO
import Libremidi.Api qualified as LMA
import Libremidi.Simple qualified as LMS

main :: IO ()
main = flip LMS.runMidiM LMS.stderrLogFun $ do
  liftIO (putStrLn "* In ports:")
  inPorts <- LMS.listInPorts
  traverse_ (liftIO . TIO.putStrLn . fst) inPorts
  liftIO (putStrLn "* Out ports:")
  outPorts <- LMS.listOutPorts
  traverse_ (liftIO . TIO.putStrLn . fst) outPorts
  liftIO (putStrLn "* Opening first out port")
  let op = snd (Seq.index outPorts 0)
  oh <- LMS.openOutPort op
  liftIO (putStrLn "* Connected")
  liftIO (LMA.freeOutHandle oh)
  liftIO (putStrLn "* Freed")
