module Libremidi where

import Libremidi.Common (BitEnum)
import Libremidi.Foreign qualified as F
import Control.Exception (Exception)

data TimestampMode =
    TimestampModeNone
  | TimestampModeRelative
  | TimestampModeAbsolute
  | TimestampModeSystemMono
  | TimestampModeAudioFrame
  | TimestampModeCustom
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CTimestampMode TimestampMode

data Api =
   ApiUnspecified
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

data ConfigType =
    ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CConfigType ConfigType

data Version =
    VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CVersion Version

newtype Err = Err F.CErr
  deriving stock (Eq, Ord, Show)

instance Exception Err

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
