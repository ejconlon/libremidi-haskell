{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Coerce (coerce)
import Libremidi.Common (Err (..), AssocPtr (..), Field, Cb, mallocForeignPtrBytes0, cbMalloc, ptrSize, MallocPtr (..), CallbackPtr (..), mkField,  ForeignM, takeM, textM)
import Data.Text (Text)

#include "libremidi/libremidi-c.h"

type Sym1 = CUChar
type Msg1 = Ptr Sym1

type Sym2 = CUInt
type Msg2 = Ptr Sym2

type Timestamp = CLong

data InPortX
newtype InPort = InPort { unInPort :: ForeignPtr InPortX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

data OutPortX
newtype OutPort = OutPort { unOutPort :: ForeignPtr OutPortX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

data InHandleX
newtype InHandle = InHandle { unInHandle :: ForeignPtr InHandleX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

data OutHandleX
newtype OutHandle = OutHandle { unOutHandle :: ForeignPtr OutHandleX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

data ObsHandleX
newtype ObsHandle = ObsHandle { unObsHandle :: ForeignPtr ObsHandleX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

type TimestampMode = CInt

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

data ApiConfigX
newtype ApiConfig = ApiConfig { unApiConfig :: ForeignPtr ApiConfigX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance MallocPtr ApiConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_api_configuration})

acApi :: Field ApiConfig (Ptr Api)
acApi = mkField #{offset libremidi_api_configuration, api}

acConfigType :: Field ApiConfig (Ptr ConfigType)
acConfigType = mkField #{offset libremidi_api_configuration, configuration_type}

acData :: Field ApiConfig (Ptr (Ptr Void))
acData = mkField #{offset libremidi_api_configuration, data}

type LogFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()

foreign import ccall "wrapper"
  logWrap :: LogFun -> IO (FunPtr LogFun)

newtype LogCb = LogCb { unLogCb :: Cb LogFun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr LogCb where
  type PtrCallback LogCb = LogFun
  callbackPtr = coerce . cbMalloc (coerce logWrap)

type InFun = Ptr Void -> Ptr InPortX -> IO ()

foreign import ccall "wrapper"
  inWrap :: InFun -> IO (FunPtr InFun)

newtype InCb = InCb { unInCb :: Cb InFun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr InCb where
  type PtrCallback InCb = InFun
  callbackPtr = coerce . cbMalloc (coerce inWrap)

type OutFun = Ptr Void -> Ptr OutPortX -> IO ()

foreign import ccall "wrapper"
  outWrap :: OutFun -> IO (FunPtr OutFun)

newtype OutCb = OutCb { unOutCb :: Cb OutFun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr OutCb where
  type PtrCallback OutCb = OutFun
  callbackPtr = coerce . cbMalloc (coerce outWrap)

data ObsConfigX
newtype ObsConfig = ObsConfig { unObsConfig :: ForeignPtr ObsConfigX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance MallocPtr ObsConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_observer_configuration})

ocOnErr :: Field ObsConfigX (FunPtr LogFun)
ocOnErr = mkField (ptrSize + #{offset libremidi_observer_configuration, on_error})

ocOnWarn :: Field ObsConfigX (FunPtr LogFun)
ocOnWarn = mkField (ptrSize + #{offset libremidi_observer_configuration, on_warning})

ocInAdd :: Field ObsConfigX (FunPtr InFun)
ocInAdd = mkField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocInRem :: Field ObsConfigX (FunPtr InFun)
ocInRem = mkField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocOutAdd :: Field ObsConfigX (FunPtr OutFun)
ocOutAdd = mkField (ptrSize + #{offset libremidi_observer_configuration, input_added})

ocOutRem :: Field ObsConfigX (FunPtr OutFun)
ocOutRem = mkField (ptrSize + #{offset libremidi_observer_configuration, input_removed})

ocTrackHardware :: Field ObsConfigX CBool
ocTrackHardware = mkField #{offset libremidi_observer_configuration, track_hardware}

ocTrackVirtual :: Field ObsConfigX CBool
ocTrackVirtual = mkField #{offset libremidi_observer_configuration, track_virtual}

ocTrackAny :: Field ObsConfigX CBool
ocTrackAny = mkField #{offset libremidi_observer_configuration, track_any}

ocNotifInCon :: Field ObsConfigX CBool
ocNotifInCon = mkField #{offset libremidi_observer_configuration, notify_in_constructor}

type Msg1Fun = Ptr Void -> Msg1 -> CSize -> IO ()

foreign import ccall "wrapper"
  msg1Wrap :: Msg1Fun -> IO (FunPtr Msg1Fun)

newtype Msg1Cb = Msg1Cb { unMsg1Cb :: Cb Msg1Fun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr Msg1Cb where
  type PtrCallback Msg1Cb = Msg1Fun
  callbackPtr = coerce . cbMalloc (coerce msg1Wrap)

type Msg2Fun = Ptr Void -> Msg2 -> CSize -> IO ()

foreign import ccall "wrapper"
  msg2Wrap :: Msg2Fun -> IO (FunPtr Msg2Fun)

newtype Msg2Cb = Msg2Cb { unMsg2Cb :: Cb Msg2Fun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr Msg2Cb where
  type PtrCallback Msg2Cb = Msg2Fun
  callbackPtr = coerce . cbMalloc (coerce msg2Wrap)

type TimeFun = Ptr Void -> Timestamp -> IO ()

foreign import ccall "wrapper"
  timeWrap :: TimeFun -> IO (FunPtr TimeFun)

newtype TimeCb = TimeCb { unTimeCb :: Cb TimeFun }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance CallbackPtr TimeCb where
  type PtrCallback TimeCb = TimeFun
  callbackPtr = coerce . cbMalloc (coerce timeWrap)

data MidiConfigX
newtype MidiConfig = MidiConfig { unMidiConfig :: ForeignPtr MidiConfigX }
  deriving stock (Eq, Show)
  deriving newtype (AssocPtr)

instance MallocPtr MidiConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_midi_configuration})

mcVersion :: Field MidiConfigX Version
mcVersion = mkField #{offset libremidi_midi_configuration, version}

mcInPort :: Field MidiConfigX InPortX
mcInPort = mkField #{offset libremidi_midi_configuration, in_port}

mcOutPort :: Field MidiConfigX utPortX
mcOutPort = mkField #{offset libremidi_midi_configuration, out_port}

mcOnMsg1 :: Field MidiConfigX (FunPtr Msg1Fun)
mcOnMsg1 = mkField #{offset libremidi_midi_configuration, on_midi1_message}

mcOnMsg2 :: Field MidiConfigX (FunPtr Msg2Fun)
mcOnMsg2 = mkField #{offset libremidi_midi_configuration, on_midi2_message}

mcGetTime :: Field MidiConfigX (FunPtr TimeFun)
mcGetTime = mkField #{offset libremidi_midi_configuration, get_timestamp}

mcOnErr :: Field MidiConfigX (FunPtr LogFun)
mcOnErr = mkField #{offset libremidi_midi_configuration, on_error}

mcOnWarn :: Field MidiConfigX (FunPtr LogFun)
mcOnWarn = mkField #{offset libremidi_midi_configuration, on_warning}

mcPortName :: Field MidiConfigX CString
mcPortName = mkField #{offset libremidi_midi_configuration, port_name}

mcVirtualPort :: Field MidiConfigX CBool
mcVirtualPort = mkField #{offset libremidi_midi_configuration, virtual_port}

mcIgnoreSysex :: Field MidiConfigX CBool
mcIgnoreSysex = mkField #{offset libremidi_midi_configuration, ignore_sysex}

mcIgnoreTiming :: Field MidiConfigX CBool
mcIgnoreTiming = mkField #{offset libremidi_midi_configuration, ignore_timing}

mcIgnoreSensing :: Field MidiConfigX CBool
mcIgnoreSensing = mkField #{offset libremidi_midi_configuration, ignore_sensing}

mcTimestamps :: Field MidiConfigX TimestampMode
mcTimestamps = mkField #{offset libremidi_midi_configuration, timestamps}

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_clone :: Ptr InPortX -> Ptr (Ptr InPortX) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_name :: Ptr InPortX -> Ptr CString -> Ptr CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_clone :: Ptr OutPortX -> Ptr (Ptr OutPortX) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_name :: Ptr OutPortX -> Ptr CString -> Ptr CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_new :: Ptr ObsConfigX -> Ptr ApiConfigX -> Ptr (Ptr ObsHandleX) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_input_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr InFun -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_output_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr OutFun -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr InHandleX) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_is_connected :: Ptr InHandleX -> IO CBool

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_absolute_timestamp :: Ptr InHandleX -> IO Timestamp

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_free :: Ptr InHandleX -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr OutHandleX) -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_is_connected :: Ptr OutHandleX -> IO CBool

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_message :: Ptr OutHandleX -> Msg1 -> CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_ump ::  Ptr OutHandleX -> Msg2 -> CSize -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_message :: Ptr OutHandleX -> Timestamp -> Msg1 -> CSize  -> IO Err

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_ump :: Ptr OutHandleX -> Timestamp -> Msg2 -> CSize -> IO Err

ipClone :: Ptr InPortX -> ForeignM InPort
ipClone ip = do
  fp <- takeM (libremidi_midi_in_port_clone ip)
  pure (InPort fp)

ipName :: Ptr InPortX -> ForeignM Text
ipName ip = do
  textM (libremidi_midi_in_port_name ip)

opClone :: Ptr OutPortX -> ForeignM OutPort
opClone op = do
  fp <- takeM (libremidi_midi_out_port_clone op)
  pure (OutPort fp)

opName :: Ptr OutPortX -> ForeignM Text
opName op = do
  textM (libremidi_midi_out_port_name op)

obsNew :: Ptr ObsConfigX -> Ptr ApiConfigX -> ForeignM ObsHandle
obsNew oc ac = do
  fp <- takeM (libremidi_midi_observer_new oc ac)
  pure (ObsHandle fp)

