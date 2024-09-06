{-# LANGUAGE OverloadedStrings #-}

-- | High-level operations.
module Libremidi.Simple where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Libremidi.Api qualified as LMA
import Libremidi.Common (ErrM, rethrowErrM, newUniquePtr)
import System.IO qualified as SIO

printLogFun :: LMA.LogFun
printLogFun lvl msg =
  let prefix = case lvl of
        LMA.LogLvlWarn -> "[WARN] "
        LMA.LogLvlErr -> "[ERR ] "
  in  TIO.hPutStrLn SIO.stderr (prefix <> msg)

newPrintingObsHandle :: IO LMA.ObsHandle
newPrintingObsHandle = do
  oc <- LMA.setObsLogCb printLogFun def
  rethrowErrM (LMA.newObsHandle LMA.ApiUnspecified oc)

listEnumFun
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> IORef (Seq (Text, ForeignPtr p))
  -> LMA.EnumFun p
listEnumFun name clone r p = do
  n <- rethrowErrM (name p)
  fp <- rethrowErrM (clone p)
  modifyIORef' r (:|> (n, fp))

listPorts
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (LMA.ObsHandle -> LMA.EnumFun p -> ErrM ())
  -> IO (Seq (Text, ForeignPtr p))
listPorts name clone list = do
  r <- newIORef Empty
  let f = listEnumFun name clone r
  foh <- newPrintingObsHandle
  rethrowErrM (list foh f)
  readIORef r

listInPorts :: IO (Seq (Text, LMA.InPort))
listInPorts = listPorts LMA.inPortName' LMA.cloneInPort' LMA.enumInPorts

listOutPorts :: IO (Seq (Text, LMA.OutPort))
listOutPorts = listPorts LMA.outPortName' LMA.cloneOutPort' LMA.enumOutPorts

findEnumFun
  :: (Ptr p -> ErrM Text)
  -> (Ptr p -> ErrM (ForeignPtr p))
  -> (Text -> Bool)
  -> IORef (Maybe (Text, ForeignPtr p))
  -> LMA.EnumFun p
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
  -> (LMA.ObsHandle -> LMA.EnumFun p -> ErrM ())
  -> (Text -> Bool)
  -> IO (Maybe (Text, ForeignPtr p))
findPort name clone list sel = do
  r <- newIORef Nothing
  let f = findEnumFun name clone sel r
  foh <- newPrintingObsHandle
  rethrowErrM (list foh f)
  readIORef r

findInPort :: (Text -> Bool) -> IO (Maybe (Text, LMA.InPort))
findInPort = findPort LMA.inPortName' LMA.cloneInPort' LMA.enumInPorts

findOutPort :: (Text -> Bool) -> IO (Maybe (Text, LMA.OutPort))
findOutPort = findPort LMA.outPortName' LMA.cloneOutPort' LMA.enumOutPorts

openOutPort :: LMA.OutPort -> IO LMA.OutHandle
openOutPort op = rethrowErrM $ do
  op' <- LMA.cloneOutPort op >>= liftIO . newUniquePtr
  let mc = def { LMA.mcPort = Just (LMA.MidiPortOut op') }
  LMA.newOutHandle LMA.ApiUnspecified mc

