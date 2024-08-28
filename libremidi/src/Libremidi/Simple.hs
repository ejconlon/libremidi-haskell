{-# LANGUAGE OverloadedStrings #-}

-- | High-level operations.
module Libremidi.Simple where

import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Libremidi.Api
  ( Api (..)
  , EnumFun
  , InPort
  , LogFun
  , LogLvl (..)
  , ObsHandle
  , OutPort
  , cloneInPort
  , cloneOutPort
  , enumInPorts
  , enumOutPorts
  , inPortName
  , newObsHandle
  , outPortName
  , setObsLogCb
  )
import Libremidi.Common (ErrM, rethrowErrM)
import System.IO qualified as SIO

printLogFun :: LogFun
printLogFun lvl msg =
  let prefix = case lvl of
        LogLvlWarn -> "[WARN] "
        LogLvlErr -> "[ERR ] "
  in  TIO.hPutStrLn SIO.stderr (prefix <> msg)

gatherEnumFun
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> IORef (Seq (Text, ForeignPtr p))
  -> EnumFun p
gatherEnumFun name clone r p = do
  n <- rethrowErrM (name p)
  fp <- rethrowErrM (clone p)
  modifyIORef' r (:|> (n, fp))

newPrintingObsHandle :: IO ObsHandle
newPrintingObsHandle = do
  oc <- setObsLogCb printLogFun def
  rethrowErrM (newObsHandle ApiUnspecified oc)

listPorts
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (ObsHandle -> EnumFun p -> ErrM ())
  -> IO (Seq (Text, ForeignPtr p))
listPorts name clone list = do
  r <- newIORef Empty
  let f = gatherEnumFun name clone r
  foh <- newPrintingObsHandle
  rethrowErrM (list foh f)
  readIORef r

listInPorts :: IO (Seq (Text, InPort))
listInPorts = listPorts inPortName cloneInPort enumInPorts

listOutPorts :: IO (Seq (Text, OutPort))
listOutPorts = listPorts outPortName cloneOutPort enumOutPorts
