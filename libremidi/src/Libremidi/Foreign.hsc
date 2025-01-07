{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Low-level bindings to libremidi.
module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, FunPtr)
import Libremidi.Common (Err (..), Field, Cb, mallocForeignPtrBytes0, newCb, ptrSize, MallocPtr (..), newField)

#include "libremidi/api-c.h"
#include "libremidi/libremidi-c.h"

#{enum TimestampMode
 , CInt
 , tmNoTimestamp = NoTimestamp
 , tmRelative = Relative
 , tmAbsolute = Absolute
 , tmSystemMonotonic = SystemMonotonic
 , tmAudioFrame = AudioFrame
 , tmCustom = Custom
}

type Api = CInt

#{enum Api
 , CInt
 , apiUnspecified = UNSPECIFIED
 , apiCoremidi = COREMIDI
 , apiAlsaSeq = ALSA_SEQ
 , apiAlsaRaw = ALSA_RAW
 , apiJackMidi = JACK_MIDI
 , apiWindowsMm = WINDOWS_MM
 , apiWindowsUwp = WINDOWS_UWP
 , apiWebmidi = WEBMIDI
 , apiPipewire = PIPEWIRE
 , apiKeyboard = KEYBOARD
 , apiNetwork = NETWORK
 , apiAlsaRawUmp = ALSA_RAW_UMP
 , apiAlsaSeqUmp = ALSA_SEQ_UMP
 , apiCoremidiUmp = COREMIDI_UMP
 , apiWindowsMidiServices = WINDOWS_MIDI_SERVICES
 , apiKeyboardUmp = KEYBOARD_UMP
 , apiNetworkUmp = NETWORK_UMP
 , apiDummy = DUMMY
}

type ConfigType = CInt

#{enum ConfigType
 , CInt
 , ctObserver = Observer
 , ctInput = Input
 , ctOutput = Output
}

type Version = CInt

#{enum Version
 , CInt
 , verMidi1 = MIDI1
 , verMidi2 = MIDI2
}

type Sym1 = CUChar
type Sym2 = CUInt

type Timestamp = CLong

data InPort
data OutPort

data InHandle
data OutHandle
data ObsHandle

type TimestampMode = CInt

data ApiConfig

-- TODO Use libremidi_midi_api_configuration_init
instance MallocPtr ApiConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_api_configuration}

acApi :: Field ApiConfig Api
acApi = newField #{offset libremidi_api_configuration, api}

acConfigType :: Field ApiConfig ConfigType
acConfigType = newField #{offset libremidi_api_configuration, configuration_type}

acData :: Field ApiConfig (Ptr Void)
acData = newField #{offset libremidi_api_configuration, data}

type LogFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()

foreign import ccall "wrapper"
  logWrap :: LogFun -> IO (FunPtr LogFun)

newLogCb :: LogFun -> IO (Cb LogFun)
newLogCb = newCb logWrap

type ObsFun p = Ptr Void -> Ptr p -> IO ()

foreign import ccall "wrapper"
  obsWrap :: ObsFun p -> IO (FunPtr (ObsFun p))

newObsCb :: ObsFun p -> IO (Cb (ObsFun p))
newObsCb = newCb obsWrap

type AvailFun = Ptr Void -> Api -> IO ()

foreign import ccall "wrapper"
  availWrap :: AvailFun -> IO (FunPtr AvailFun)

newAvailCb :: AvailFun -> IO (Cb AvailFun)
newAvailCb = newCb availWrap

data ObsConfig

-- TODO Use libremidi_midi_observer_configuration_init
instance MallocPtr ObsConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_observer_configuration}

ocOnErr :: Field ObsConfig (FunPtr LogFun)
ocOnErr = newField (ptrSize + #{offset libremidi_observer_configuration, on_error})

ocOnWarn :: Field ObsConfig (FunPtr LogFun)
ocOnWarn = newField (ptrSize + #{offset libremidi_observer_configuration, on_warning})

ocInAdd :: Field ObsConfig (FunPtr (ObsFun InPort))
ocInAdd = newField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocInRem :: Field ObsConfig (FunPtr (ObsFun InPort))
ocInRem = newField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocOutAdd :: Field ObsConfig (FunPtr (ObsFun OutPort))
ocOutAdd = newField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocOutRem :: Field ObsConfig (FunPtr (ObsFun OutPort))
ocOutRem = newField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocTrackHardware :: Field ObsConfig CBool
ocTrackHardware = newField #{offset libremidi_observer_configuration, track_hardware}

ocTrackVirtual :: Field ObsConfig CBool
ocTrackVirtual = newField #{offset libremidi_observer_configuration, track_virtual}

ocTrackAny :: Field ObsConfig CBool
ocTrackAny = newField #{offset libremidi_observer_configuration, track_any}

ocNotifInCon :: Field ObsConfig CBool
ocNotifInCon = newField #{offset libremidi_observer_configuration, notify_in_constructor}

type MsgFun d = Ptr Void -> Ptr d -> CSize -> IO ()

foreign import ccall "wrapper"
  msgWrap :: MsgFun d -> IO (FunPtr (MsgFun d))

newMsgCb :: MsgFun d -> IO (Cb (MsgFun d))
newMsgCb = newCb msgWrap

type TimeFun = Ptr Void -> Timestamp -> IO ()

foreign import ccall "wrapper"
  timeWrap :: TimeFun -> IO (FunPtr TimeFun)

newTimeCb :: TimeFun -> IO (Cb TimeFun)
newTimeCb = newCb timeWrap

data MidiConfig

-- TODO Use libremidi_midi_configuration_init
instance MallocPtr MidiConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_midi_configuration}

mcVersion :: Field MidiConfig Version
mcVersion = newField #{offset libremidi_midi_configuration, version}

mcInPort :: Field MidiConfig (Ptr InPort)
mcInPort = newField #{offset libremidi_midi_configuration, in_port}

mcOutPort :: Field MidiConfig (Ptr OutPort)
mcOutPort = newField #{offset libremidi_midi_configuration, out_port}

mcOnMsg1 :: Field MidiConfig (FunPtr (MsgFun Sym1))
mcOnMsg1 = newField #{offset libremidi_midi_configuration, on_midi1_message}

mcOnMsg2 :: Field MidiConfig (FunPtr (MsgFun Sym2))
mcOnMsg2 = newField #{offset libremidi_midi_configuration, on_midi2_message}

mcGetTime :: Field MidiConfig (FunPtr TimeFun)
mcGetTime = newField #{offset libremidi_midi_configuration, get_timestamp}

mcOnErr :: Field MidiConfig (FunPtr LogFun)
mcOnErr = newField #{offset libremidi_midi_configuration, on_error}

mcOnWarn :: Field MidiConfig (FunPtr LogFun)
mcOnWarn = newField #{offset libremidi_midi_configuration, on_warning}

mcPortName :: Field MidiConfig CString
mcPortName = newField #{offset libremidi_midi_configuration, port_name}

mcVirtualPort :: Field MidiConfig CBool
mcVirtualPort = newField #{offset libremidi_midi_configuration, virtual_port}

mcIgnoreSysex :: Field MidiConfig CBool
mcIgnoreSysex = newField #{offset libremidi_midi_configuration, ignore_sysex}

mcIgnoreTiming :: Field MidiConfig CBool
mcIgnoreTiming = newField #{offset libremidi_midi_configuration, ignore_timing}

mcIgnoreSensing :: Field MidiConfig CBool
mcIgnoreSensing = newField #{offset libremidi_midi_configuration, ignore_sensing}

mcTimestamps :: Field MidiConfig TimestampMode
mcTimestamps = newField #{offset libremidi_midi_configuration, timestamps}

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_get_version :: IO CString

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi1_available_apis :: Ptr Void -> FunPtr AvailFun -> IO ()

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi2_available_apis :: Ptr Void -> FunPtr AvailFun -> IO ()

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_api_identifier :: Api -> IO CString

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_api_display_name :: Api -> IO CString

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_get_compiled_api_by_identifier :: CString -> IO Api

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_clone :: Ptr InPort -> Ptr (Ptr InPort) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_name :: Ptr InPort -> Ptr CString -> Ptr CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_clone :: Ptr OutPort -> Ptr (Ptr OutPort) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_name :: Ptr OutPort -> Ptr CString -> Ptr CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_new :: Ptr ObsConfig -> Ptr ApiConfig -> Ptr (Ptr ObsHandle) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_input_ports :: Ptr ObsHandle -> Ptr Void -> FunPtr (ObsFun InPort) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_output_ports :: Ptr ObsHandle -> Ptr Void -> FunPtr (ObsFun OutPort) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_new :: Ptr MidiConfig -> Ptr ApiConfig -> Ptr (Ptr InHandle) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_is_connected :: Ptr InHandle -> IO CBool

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_absolute_timestamp :: Ptr InHandle -> IO Timestamp

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_free :: Ptr InHandle -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_new :: Ptr MidiConfig -> Ptr ApiConfig -> Ptr (Ptr OutHandle) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_is_connected :: Ptr OutHandle -> IO CBool

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_message :: Ptr OutHandle -> Ptr Sym1 -> CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_ump ::  Ptr OutHandle -> Ptr Sym2 -> CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_message :: Ptr OutHandle -> Timestamp -> Ptr Sym1 -> CSize  -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_ump :: Ptr OutHandle -> Timestamp -> Ptr Sym2 -> CSize -> IO Err
