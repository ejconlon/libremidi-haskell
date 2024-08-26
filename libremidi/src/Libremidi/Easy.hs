{-# LANGUAGE OverloadedStrings #-}

module Libremidi.Easy where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Libremidi qualified as L
import Libremidi.Common qualified as C
import Libremidi.Foreign qualified as F
import System.IO qualified as SIO

runM :: C.ForeignM a -> IO a
runM = C.runErrM . C.runForeignM >=> either throwIO pure

printLogFun :: L.LogFun
printLogFun lvl msg =
  let prefix = case lvl of
        L.LogLvlWarn -> "[WARN] "
        L.LogLvlErr -> "[ERR ] "
  in  TIO.hPutStrLn SIO.stderr (prefix <> msg)

gatherEnumFun
  :: (Ptr p -> C.ForeignM Text)
  -> (Ptr p -> C.ForeignM (ForeignPtr p))
  -> IORef (Seq (Text, ForeignPtr p))
  -> L.EnumFun p
gatherEnumFun name clone r p = do
  n <- runM (name p)
  fp <- runM (clone p)
  modifyIORef' r (:|> (n, fp))

newObsHandle :: C.ForeignM (ForeignPtr F.ObsHandle)
newObsHandle = do
  oc <- liftIO (L.ocbSetLogCb printLogFun def) >>= L.buildObsConfig
  L.newObsHandle L.ApiUnspecified oc

listPorts
  :: (Ptr p -> C.ForeignM Text)
  -> (Ptr p -> C.ForeignM (ForeignPtr p))
  -> (ForeignPtr F.ObsHandle -> L.EnumFun p -> C.ForeignM ())
  -> IO (Seq (Text, ForeignPtr p))
listPorts name clone list = do
  r <- newIORef Empty
  let f = gatherEnumFun name clone r
  runM $ do
    foh <- newObsHandle
    list foh f
  readIORef r

listInPorts :: IO (Seq (Text, ForeignPtr F.InPort))
listInPorts = listPorts F.ipName F.ipClone L.listInPorts

listOutPorts :: IO (Seq (Text, ForeignPtr F.OutPort))
listOutPorts = listPorts F.opName F.opClone L.listOutPorts
