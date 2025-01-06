{-# LANGUAGE OverloadedStrings #-}

-- | High-level operations.
module Libremidi.Simple where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask)
import Data.Default (def)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Libremidi.Api qualified as LMA
import Libremidi.Common (ErrM, newUniquePtr, rethrowErrM)
import System.IO qualified as SIO

type MidiM = ReaderT LMA.LogFun IO

runMidiM :: MidiM a -> LMA.LogFun -> IO a
runMidiM = runReaderT

nullLogFun :: LMA.LogFun
nullLogFun _ _ = pure ()

stderrLogFun :: LMA.LogFun
stderrLogFun lvl msg =
  let prefix = case lvl of
        LMA.LogLvlWarn -> "[WARN] "
        LMA.LogLvlErr -> "[ERR ] "
  in  TIO.hPutStrLn SIO.stderr (prefix <> msg)

newObsHandle :: MidiM LMA.ObsHandle
newObsHandle = do
  lf <- ask
  liftIO $ do
    oc <- LMA.setObsLogCb lf def
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
  -> MidiM (Seq (Text, ForeignPtr p))
listPorts name clone list = do
  r <- liftIO (newIORef Empty)
  let f = listEnumFun name clone r
  foh <- newObsHandle
  liftIO $ do
    rethrowErrM (list foh f)
    readIORef r

listInPorts :: MidiM (Seq (Text, LMA.InPort))
listInPorts = listPorts LMA.inPortName' LMA.cloneInPort' LMA.enumInPorts

listOutPorts :: MidiM (Seq (Text, LMA.OutPort))
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
  -> MidiM (Maybe (Text, ForeignPtr p))
findPort name clone list sel = do
  r <- liftIO (newIORef Nothing)
  let f = findEnumFun name clone sel r
  foh <- newObsHandle
  liftIO $ do
    rethrowErrM (list foh f)
    readIORef r

findInPort :: (Text -> Bool) -> MidiM (Maybe (Text, LMA.InPort))
findInPort = findPort LMA.inPortName' LMA.cloneInPort' LMA.enumInPorts

findOutPort :: (Text -> Bool) -> MidiM (Maybe (Text, LMA.OutPort))
findOutPort = findPort LMA.outPortName' LMA.cloneOutPort' LMA.enumOutPorts

openOutPort :: LMA.OutPort -> MidiM LMA.OutHandle
openOutPort op = do
  lf <- ask
  liftIO $ rethrowErrM $ do
    op' <- LMA.cloneOutPort op >>= liftIO . newUniquePtr
    let mc = def {LMA.mcPort = Just (LMA.MidiPortOut op')}
    mc' <- liftIO (LMA.setMidiLogCb lf mc)
    LMA.newOutHandle LMA.ApiUnspecified mc'

listAvailFun :: IORef (Seq LMA.Api) -> LMA.AvailFun
listAvailFun r = maybe (pure ()) (modifyIORef' r . flip (:|>))

listAvailApis1 :: IO (Seq LMA.Api)
listAvailApis1 = do
  r <- newIORef Empty
  LMA.availApis1 (listAvailFun r)
  readIORef r

listAvailApis2 :: IO (Seq LMA.Api)
listAvailApis2 = do
  r <- newIORef Empty
  LMA.availApis2 (listAvailFun r)
  readIORef r
