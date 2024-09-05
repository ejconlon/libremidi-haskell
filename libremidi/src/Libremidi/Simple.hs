{-# LANGUAGE OverloadedStrings #-}

-- | High-level operations.
module Libremidi.Simple where

import Control.Monad (when)
import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
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
  , cloneInPort'
  , cloneOutPort'
  , enumInPorts
  , enumOutPorts
  , inPortName'
  , newObsHandle
  , outPortName'
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

newPrintingObsHandle :: IO ObsHandle
newPrintingObsHandle = do
  oc <- setObsLogCb printLogFun def
  rethrowErrM (newObsHandle ApiUnspecified oc)

listEnumFun
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> IORef (Seq (Text, ForeignPtr p))
  -> EnumFun p
listEnumFun name clone r p = do
  n <- rethrowErrM (name p)
  fp <- rethrowErrM (clone p)
  modifyIORef' r (:|> (n, fp))

listPorts
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (ObsHandle -> EnumFun p -> ErrM ())
  -> IO (Seq (Text, ForeignPtr p))
listPorts name clone list = do
  r <- newIORef Empty
  let f = listEnumFun name clone r
  foh <- newPrintingObsHandle
  rethrowErrM (list foh f)
  readIORef r

listInPorts :: IO (Seq (Text, InPort))
listInPorts = listPorts inPortName' cloneInPort' enumInPorts

listOutPorts :: IO (Seq (Text, OutPort))
listOutPorts = listPorts outPortName' cloneOutPort' enumOutPorts

findEnumFun
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (Text -> Bool)
  -> IORef (Maybe (Text, ForeignPtr p))
  -> EnumFun p
findEnumFun name clone sel r p = do
  missing <- fmap isNothing (readIORef r)
  when missing $ do
    n <- rethrowErrM (name p)
    when (sel n) $ do
      fp <- rethrowErrM (clone p)
      writeIORef r (Just (n, fp))

findPort
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (ObsHandle -> EnumFun p -> ErrM ())
  -> (Text -> Bool)
  -> IO (Maybe (Text, ForeignPtr p))
findPort name clone list sel = do
  r <- newIORef Nothing
  let f = findEnumFun name clone sel r
  foh <- newPrintingObsHandle
  rethrowErrM (list foh f)
  readIORef r

findInPort :: (Text -> Bool) -> IO (Maybe (Text, InPort))
findInPort = findPort inPortName' cloneInPort' enumInPorts

findOutPort :: (Text -> Bool) -> IO (Maybe (Text, OutPort))
findOutPort = findPort outPortName' cloneOutPort' enumOutPorts
