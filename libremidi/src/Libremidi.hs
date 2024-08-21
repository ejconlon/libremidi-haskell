module Libremidi where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (castPtr)
import Libremidi.Common (BitEnum)
import Libremidi.Foreign qualified as F

data TimestampMode
  = TimestampModeNone
  | TimestampModeRelative
  | TimestampModeAbsolute
  | TimestampModeSystemMono
  | TimestampModeAudioFrame
  | TimestampModeCustom
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CTimestampMode TimestampMode

data Api
  = ApiUnspecified
  | ApiCoremidi
  | ApiAlsaSeq
  | ApiAlsaRaw
  | ApiJackMidi
  | ApiWindowsMm
  | ApiWindowsUwp
  | ApiWebmidi
  | ApiPipewire
  | ApiAlsaRawUmp
  | ApiAlsaSeqUmp
  | ApiCoremidiUmp
  | ApiWindowsMidiServices
  | ApiDummy
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CApi Api

data ConfigType
  = ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CConfigType ConfigType

data Version
  = VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CVersion Version

newtype Err = Err F.CErr
  deriving stock (Eq, Ord, Show)

instance Exception Err

data F a where
  FErr :: Err -> F a
  FCont :: (forall r. (a -> IO r) -> IO r) -> F a

instance Functor F where
  fmap f = \case
    FErr e -> FErr e
    FCont k -> FCont (\g -> k (g . f))

newtype M a = M {unM :: Free F a}
  deriving (Functor, Applicative, Monad)

instance MonadIO M where
  liftIO = undefined

instance MonadError Err M where
  throwError = undefined

runM :: M a -> IO (Either Err a)
runM m = undefined

useM :: (forall r. (a -> IO r) -> IO r) -> M a
useM x = undefined

guardM :: IO F.CErr -> M ()
guardM eact = do
  e <- liftIO eact
  when (e /= 0) (throwError (Err e))

ipClone :: F.InPort -> M F.InPort
ipClone ip = do
  fp <- liftIO (mallocForeignPtrBytes 0)
  ptr <- useM (withForeignPtr fp)
  cip <- useM (F.ipWithPtr ip)
  guardM (F.libremidi_midi_in_port_clone cip (castPtr ptr))
  pure (F.InPort fp)

ipName :: F.InPort -> M Text
ipName = error "TODO"

opClone :: F.OutPort -> M F.OutPort
opClone op = do
  fp <- liftIO (mallocForeignPtrBytes 0)
  ptr <- useM (withForeignPtr fp)
  cop <- useM (F.opWithPtr op)
  guardM (F.libremidi_midi_out_port_clone cop (castPtr ptr))
  pure (F.OutPort fp)

opName :: F.OutPort -> M Text
opName = error "TODO"

obsNew :: F.ObsConfig -> F.ApiConfig -> M F.ObsHandle
obsNew oc ac = error "TODO"

-- TODO expose these
-- libremidi_midi_in_port_clone :: CInPort -> Ptr CInPort -> IO CErr
-- libremidi_midi_in_port_name :: CInPort -> Ptr CString -> Ptr CSize -> IO CErr
-- libremidi_midi_out_port_clone :: COutPort -> Ptr COutPort -> IO CErr
-- libremidi_midi_out_port_name :: COutPort -> Ptr CString -> Ptr CSize -> IO CErr
-- libremidi_midi_observer_new :: CObsConfig -> CApiConfig -> Ptr CObsHandle -> IO CErr
-- libremidi_midi_observer_enumerate_input_ports :: CObsHandle -> Ptr Void -> CInCb -> IO CErr
-- libremidi_midi_enumerate_output_ports :: CObsHandle -> Ptr Void -> COutCb -> IO CErr
-- libremidi_midi_in_new :: CMidiConfig -> CApiConfig -> Ptr CInHandle -> IO CErr
-- libremidi_midi_in_is_connected :: CInHandle -> IO CErr
-- libremidi_midi_in_absolute_timestamp :: CInHandle -> IO CTimestamp
-- libremidi_midi_in_free :: CInHandle -> IO CErr
-- libremidi_midi_out_new :: CMidiConfig -> CApiConfig -> Ptr COutHandle -> IO CErr
-- libremidi_midi_out_is_connected :: COutHandle -> IO CErr
-- libremidi_midi_out_send_message :: COutHandle -> CMsg1 -> CSize -> IO CErr
-- libremidi_midi_out_send_ump ::  COutHandle -> CMsg2 -> CSize -> IO CErr
-- libremidi_midi_out_schedule_message :: COutHandle -> CTimestamp -> CMsg1 -> CSize  -> IO CErr
-- libremidi_midi_out_schedule_ump :: COutHandle -> CTimestamp -> CMsg2 -> CSize -> IO CErr
--
