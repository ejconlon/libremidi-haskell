module Libremidi where

import Data.Coerce (coerce)
import Data.Text (Text)
import Libremidi.Common (BitEnum (..), M, assocM, takeM, textM)
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

ipClone :: F.InPort -> M F.InPort
ipClone ip = do
  ptr <- assocM ip
  fp <- takeM (F.libremidi_midi_in_port_clone ptr . coerce)
  pure (F.InPort fp)

ipName :: F.InPort -> M Text
ipName ip = do
  ptr <- assocM ip
  textM (F.libremidi_midi_in_port_name (coerce ptr))

opClone :: F.OutPort -> M F.OutPort
opClone op = do
  ptr <- assocM op
  fp <- takeM (F.libremidi_midi_out_port_clone ptr . coerce)
  pure (F.OutPort fp)

opName :: F.OutPort -> M Text
opName op = do
  ptr <- assocM op
  textM (F.libremidi_midi_out_port_name (coerce ptr))

obsNew :: F.ObsConfig -> F.ApiConfig -> M F.ObsHandle
obsNew oc ac = do
  ocptr <- assocM oc
  acptr <- assocM ac
  fp <- takeM (F.libremidi_midi_observer_new ocptr acptr . coerce)
  pure (F.ObsHandle fp)

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
