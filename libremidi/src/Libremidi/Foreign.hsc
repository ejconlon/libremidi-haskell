{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Libremidi.Common (Err (..), Field, Cb, mallocForeignPtrBytes0, cbMalloc, ptrSize, MallocPtr (..), mkField,  ForeignM, takeM, textM)
import Data.Text (Text)

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
 , apiAlsaRawUmp = ALSA_RAW_UMP
 , apiAlsaSeqUmp = ALSA_SEQ_UMP
 , apiCoremidiUmp = COREMIDI_UMP
 , apiWindowsMidiServices = WINDOWS_MIDI_SERVICES
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

instance MallocPtr ApiConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_api_configuration}

acApi :: Field ApiConfig (Ptr Api)
acApi = mkField #{offset libremidi_api_configuration, api}

acConfigType :: Field ApiConfig (Ptr ConfigType)
acConfigType = mkField #{offset libremidi_api_configuration, configuration_type}

acData :: Field ApiConfig (Ptr (Ptr Void))
acData = mkField #{offset libremidi_api_configuration, data}

type LogFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()

foreign import ccall "wrapper"
  logWrap :: LogFun -> IO (FunPtr LogFun)

mkLogCb :: LogFun -> IO (Cb LogFun)
mkLogCb = cbMalloc logWrap

type ObsFun p = Ptr Void -> Ptr p -> IO ()

foreign import ccall "wrapper"
  obsWrap :: ObsFun p -> IO (FunPtr (ObsFun p))

mkObsCb :: ObsFun p -> IO (Cb (ObsFun p))
mkObsCb = cbMalloc obsWrap

data ObsConfig

instance MallocPtr ObsConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_observer_configuration}

ocOnErr :: Field ObsConfig (FunPtr LogFun)
ocOnErr = mkField (ptrSize + #{offset libremidi_observer_configuration, on_error})

ocOnWarn :: Field ObsConfig (FunPtr LogFun)
ocOnWarn = mkField (ptrSize + #{offset libremidi_observer_configuration, on_warning})

ocInAdd :: Field ObsConfig (FunPtr (ObsFun InPort))
ocInAdd = mkField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocInRem :: Field ObsConfig (FunPtr (ObsFun InPort))
ocInRem = mkField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocOutAdd :: Field ObsConfig (FunPtr (ObsFun OutPort))
ocOutAdd = mkField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocOutRem :: Field ObsConfig (FunPtr (ObsFun OutPort))
ocOutRem = mkField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocTrackHardware :: Field ObsConfig CBool
ocTrackHardware = mkField #{offset libremidi_observer_configuration, track_hardware}

ocTrackVirtual :: Field ObsConfig CBool
ocTrackVirtual = mkField #{offset libremidi_observer_configuration, track_virtual}

ocTrackAny :: Field ObsConfig CBool
ocTrackAny = mkField #{offset libremidi_observer_configuration, track_any}

ocNotifInCon :: Field ObsConfig CBool
ocNotifInCon = mkField #{offset libremidi_observer_configuration, notify_in_constructor}

type MsgFun d = Ptr Void -> Ptr d -> CSize -> IO ()

foreign import ccall "wrapper"
  msgWrap :: MsgFun d -> IO (FunPtr (MsgFun d))

mkMsgCb :: MsgFun d -> IO (Cb (MsgFun d))
mkMsgCb = cbMalloc msgWrap

type TimeFun = Ptr Void -> Timestamp -> IO ()

foreign import ccall "wrapper"
  timeWrap :: TimeFun -> IO (FunPtr TimeFun)

mkTimeCb :: TimeFun -> IO (Cb TimeFun)
mkTimeCb = cbMalloc timeWrap

data MidiConfig

instance MallocPtr MidiConfig where
  mallocPtr _ = mallocForeignPtrBytes0 #{size libremidi_midi_configuration}

mcVersion :: Field MidiConfig Version
mcVersion = mkField #{offset libremidi_midi_configuration, version}

mcInPort :: Field MidiConfig InPort
mcInPort = mkField #{offset libremidi_midi_configuration, in_port}

mcOutPort :: Field MidiConfig OutPort
mcOutPort = mkField #{offset libremidi_midi_configuration, out_port}

mcOnMsg1 :: Field MidiConfig (FunPtr (MsgFun Sym1))
mcOnMsg1 = mkField #{offset libremidi_midi_configuration, on_midi1_message}

mcOnMsg2 :: Field MidiConfig (FunPtr (MsgFun Sym2))
mcOnMsg2 = mkField #{offset libremidi_midi_configuration, on_midi2_message}

mcGetTime :: Field MidiConfig (FunPtr TimeFun)
mcGetTime = mkField #{offset libremidi_midi_configuration, get_timestamp}

mcOnErr :: Field MidiConfig (FunPtr LogFun)
mcOnErr = mkField #{offset libremidi_midi_configuration, on_error}

mcOnWarn :: Field MidiConfig (FunPtr LogFun)
mcOnWarn = mkField #{offset libremidi_midi_configuration, on_warning}

mcPortName :: Field MidiConfig CString
mcPortName = mkField #{offset libremidi_midi_configuration, port_name}

mcVirtualPort :: Field MidiConfig CBool
mcVirtualPort = mkField #{offset libremidi_midi_configuration, virtual_port}

mcIgnoreSysex :: Field MidiConfig CBool
mcIgnoreSysex = mkField #{offset libremidi_midi_configuration, ignore_sysex}

mcIgnoreTiming :: Field MidiConfig CBool
mcIgnoreTiming = mkField #{offset libremidi_midi_configuration, ignore_timing}

mcIgnoreSensing :: Field MidiConfig CBool
mcIgnoreSensing = mkField #{offset libremidi_midi_configuration, ignore_sensing}

mcTimestamps :: Field MidiConfig TimestampMode
mcTimestamps = mkField #{offset libremidi_midi_configuration, timestamps}

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

ipClone :: Ptr InPort -> ForeignM (ForeignPtr InPort)
ipClone = takeM . libremidi_midi_in_port_clone

ipName :: Ptr InPort -> ForeignM Text
ipName = textM . libremidi_midi_in_port_name

opClone :: Ptr OutPort -> ForeignM (ForeignPtr OutPort)
opClone = takeM . libremidi_midi_out_port_clone

opName :: Ptr OutPort -> ForeignM Text
opName = textM . libremidi_midi_out_port_name

obsNew :: Ptr ObsConfig -> Ptr ApiConfig -> ForeignM (ForeignPtr ObsHandle)
obsNew oc ac = takeM (libremidi_midi_observer_new oc ac)

