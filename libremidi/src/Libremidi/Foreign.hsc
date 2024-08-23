{-# LANGUAGE ForeignFunctionInterface #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, plusPtr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Data.Coerce (coerce)
import Libremidi.Common (CErr, AssocPtr (..), Field, Cb, mallocForeignPtrBytes0, cbMalloc, cbWithPtr, ptrSize, cbTouch, MallocPtr (..), CallbackPtr (..))

#include "libremidi/libremidi-c.h"

type CSym1 = CUChar
type CMsg1 = Ptr CSym1

type CSym2 = CUInt
type CMsg2 = Ptr CSym2

type CTimestamp = CLong

newtype CInPort = CInPort { unCInPort :: Ptr Void }
  deriving stock (Eq, Show)

newtype InPort = InPort { unInPort :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr InPort where
  type PtrAssoc InPort = CInPort
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

newtype COutPort = COutPort { unCOutPort :: Ptr Void }
  deriving stock (Eq, Show)

newtype OutPort = OutPort { unOutPort :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr OutPort where
  type PtrAssoc OutPort = COutPort
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

newtype CInHandle = CInHandle { unCInHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype InHandle = InHandle { unInHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr InHandle where
  type PtrAssoc InHandle = CInHandle
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

newtype COutHandle = COutHandle { unCOutHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype OutHandle = OutHandle { unOutHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr OutHandle where
  type PtrAssoc OutHandle = COutHandle
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

newtype CObsHandle = CObsHandle { unCObsHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype ObsHandle = ObsHandle { unObsHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr ObsHandle where
  type PtrAssoc ObsHandle = CObsHandle
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

type CTimestampMode = CInt

#{enum CTimestampMode
 , CInt
 , ctmNoTimestamp = NoTimestamp
 , ctmRelative = Relative
 , ctmAbsolute = Absolute
 , ctmSystemMonotonic = SystemMonotonic
 , ctmAudioFrame = AudioFrame
 , ctmCustom = Custom
}

type CApi = CInt

#{enum CApi
 , CInt
 , caUnspecified = UNSPECIFIED
 , caCoremidi = COREMIDI
 , caAlsaSeq = ALSA_SEQ
 , caAlsaRaw = ALSA_RAW
 , caJackMidi = JACK_MIDI
 , caWindowsMm = WINDOWS_MM
 , caWindowsUwp = WINDOWS_UWP
 , caWebmidi = WEBMIDI
 , caPipewire = PIPEWIRE
 , caAlsaRawUmp = ALSA_RAW_UMP
 , caAlsaSeqUmp = ALSA_SEQ_UMP
 , caCoremidiUmp = COREMIDI_UMP
 , caWindowsMidiServices = WINDOWS_MIDI_SERVICES
 , caDummy = DUMMY
}

type CConfigType = CInt

#{enum CConfigType
 , CInt
 , cctObserver = Observer
 , cctInput = Input
 , cctOutput = Output
}

type CVersion = CInt

#{enum CVersion
 , CInt
 , cvMidi1 = MIDI1
 , cvMidi2 = MIDI2
}

newtype CApiConfig = CApiConfig { unCApiConfig :: Ptr Void }
  deriving stock (Eq, Show)

cacApi :: Field CApiConfig CApi
cacApi (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, api}

cacConfigType :: Field CApiConfig CConfigType
cacConfigType (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, configuration_type}

cacData :: Field CApiConfig (Ptr Void)
cacData (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, data}

newtype ApiConfig = ApiConfig { unApiConfig :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr ApiConfig where
  type PtrAssoc ApiConfig = CApiConfig
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

instance MallocPtr ApiConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_api_configuration})

type CLogCbFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()

newtype CLogCb = CLogCb { unCLogCb :: FunPtr CLogCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  clcbWrap :: CLogCbFun -> IO CLogCb

newtype LogCb = LogCb { unLogCb :: Cb CLogCbFun }

instance AssocPtr LogCb where
  type PtrAssoc LogCb = CLogCb
  withAssocPtr (LogCb c) f = cbWithPtr c (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr LogCb where
  type PtrCallback LogCb = CLogCbFun
  callbackPtr = coerce . cbMalloc (coerce clcbWrap)

type CInCbFun = Ptr Void -> CInPort -> IO ()

newtype CInCb = CMidiInCb { unCMidiInCb :: FunPtr CInCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cicbWrap :: CInCbFun -> IO CInCb

newtype InCb = InCb { unInCb :: Cb CInCbFun }

instance AssocPtr InCb where
  type PtrAssoc InCb = CInCb
  withAssocPtr cb f = cbWithPtr (coerce cb) (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr InCb where
  type PtrCallback InCb = CInCbFun
  callbackPtr = coerce . cbMalloc (coerce cicbWrap)

type COutCbFun = Ptr Void -> COutPort -> IO ()

newtype COutCb = CMidiOutCb { unCMidiOutCb :: FunPtr COutCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cocbWrap :: COutCbFun -> IO COutCb

newtype OutCb = OutCb { unOutCb :: Cb COutCbFun }

instance AssocPtr OutCb where
  type PtrAssoc OutCb = COutCb
  withAssocPtr cb f = cbWithPtr (coerce cb) (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr OutCb where
  type PtrCallback OutCb = COutCbFun
  callbackPtr = coerce . cbMalloc (coerce cocbWrap)

newtype CObsConfig = CObsConfig { unCObsConfig :: Ptr Void }
  deriving stock (Eq, Show)

cocOnErrCb :: Field CObsConfig CLogCb
cocOnErrCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, on_error})

cocOnWarnCb :: Field CObsConfig CLogCb
cocOnWarnCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, on_warning})

cocInAddCb :: Field CObsConfig CInCb
cocInAddCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_added})

cocInRemCb :: Field CObsConfig CInCb
cocInRemCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_removed})

cocOutAddCb :: Field CObsConfig COutCb
cocOutAddCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_added})

cocOutRemCb :: Field CObsConfig COutCb
cocOutRemCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_removed})

cocTrackHardware :: Field CObsConfig CBool
cocTrackHardware (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_hardware}

cocTrackVirtual :: Field CObsConfig CBool
cocTrackVirtual (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_virtual}

cocTrackAny :: Field CObsConfig CBool
cocTrackAny (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_any}

cocNotifInCon :: Field CObsConfig CBool
cocNotifInCon (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, notify_in_constructor}

newtype ObsConfig = ObsConfig { unObsConfig :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr ObsConfig where
  type PtrAssoc ObsConfig = CObsConfig
  withAssocPtr (ObsConfig fp) f = withForeignPtr fp (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

instance MallocPtr ObsConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_observer_configuration})

type CMsg1CbFun = Ptr Void -> CMsg1 -> CSize -> IO ()

newtype CMsg1Cb = CMidi1Cb (FunPtr CMsg1CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm1cbWrap :: CMsg1CbFun -> IO CMsg1Cb

newtype Msg1Cb = Msg1Cb { unMsg1Cb :: Cb CMsg1CbFun }

instance AssocPtr Msg1Cb where
  type PtrAssoc Msg1Cb = CMsg1Cb
  withAssocPtr cb f = cbWithPtr (coerce cb) (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr Msg1Cb where
  type PtrCallback Msg1Cb = CMsg1CbFun
  callbackPtr = coerce . cbMalloc (coerce cm1cbWrap)

type CMsg2CbFun = Ptr Void -> CMsg2 -> CSize -> IO ()

newtype CMsg2Cb = CMsg2Cb (FunPtr CMsg2CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm2cbWrap :: CMsg2CbFun -> IO CMsg2Cb

newtype Msg2Cb = Msg2Cb { unMsg2Cb :: Cb CMsg2CbFun }

instance AssocPtr Msg2Cb where
  type PtrAssoc Msg2Cb = CMsg2Cb
  withAssocPtr cb f = cbWithPtr (coerce cb) (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr Msg2Cb where
  type PtrCallback Msg2Cb = CMsg2CbFun
  callbackPtr = coerce . cbMalloc (coerce cm2cbWrap)

type CTimeCbFun = Ptr Void -> CTimestamp -> IO ()

newtype CTimeCb = CTimeCb (FunPtr CTimeCbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  ctcbWrap :: CTimeCbFun -> IO CTimeCb

newtype TimeCb = TimeCb { unTimeCb :: Cb CTimeCbFun }

instance AssocPtr TimeCb where
  type PtrAssoc TimeCb = CTimeCb
  withAssocPtr cb f = cbWithPtr (coerce cb) (f . coerce)
  touchAssocPtr = cbTouch . coerce

instance CallbackPtr TimeCb where
  type PtrCallback TimeCb = CTimeCbFun
  callbackPtr = coerce . cbMalloc (coerce ctcbWrap)

newtype CMidiConfig = CMidiConfig (Ptr Void)
  deriving stock (Eq, Show)

cmcVersion :: Field CMidiConfig CVersion
cmcVersion (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, version}

cmcInPort :: Field CMidiConfig CInPort
cmcInPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, in_port}

cmcOutPort :: Field CMidiConfig COutPort
cmcOutPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, out_port}

cmcMsg1Cb :: Field CMidiConfig CMsg1Cb
cmcMsg1Cb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_midi1_message}

cmcMsg2Cb :: Field CMidiConfig CMsg2Cb
cmcMsg2Cb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_midi2_message}

cmcTimeCb :: Field CMidiConfig CTimeCb
cmcTimeCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, get_timestamp}

cmcOnErrCb :: Field CMidiConfig CLogCb
cmcOnErrCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_error}

cmcOnWarnCb :: Field CMidiConfig CLogCb
cmcOnWarnCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_warning}

cmcPortName :: Field CMidiConfig CString
cmcPortName (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, port_name}

cmcVirtualPort :: Field CMidiConfig CBool
cmcVirtualPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, virtual_port}

cmcIgnoreSysex :: Field CMidiConfig CBool
cmcIgnoreSysex (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_sysex}

cmcIgnoreTiming :: Field CMidiConfig CBool
cmcIgnoreTiming (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_timing}

cmcIgnoreSensing :: Field CMidiConfig CBool
cmcIgnoreSensing (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_sensing}

cmcTimestamps :: Field CMidiConfig CTimestampMode
cmcTimestamps (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, timestamps}

newtype MidiConfig = MidiConfig { unMidiConfig :: ForeignPtr Void }
  deriving stock (Eq, Show)

instance AssocPtr MidiConfig where
  type PtrAssoc MidiConfig = CMidiConfig
  withAssocPtr fp f = withForeignPtr (coerce fp) (f . coerce)
  touchAssocPtr = touchForeignPtr . coerce

instance MallocPtr MidiConfig where
  mallocPtr _ = coerce (mallocForeignPtrBytes0 #{size libremidi_midi_configuration})

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_clone :: CInPort -> Ptr CInPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_name :: CInPort -> Ptr CString -> Ptr CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_clone :: COutPort -> Ptr COutPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_name :: COutPort -> Ptr CString -> Ptr CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_new :: CObsConfig -> CApiConfig -> Ptr CObsHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_input_ports :: CObsHandle -> Ptr Void -> CInCb -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_enumerate_output_ports :: CObsHandle -> Ptr Void -> COutCb -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_new :: CMidiConfig -> CApiConfig -> Ptr CInHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_is_connected :: CInHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_absolute_timestamp :: CInHandle -> IO CTimestamp

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_free :: CInHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_new :: CMidiConfig -> CApiConfig -> Ptr COutHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_is_connected :: COutHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_message :: COutHandle -> CMsg1 -> CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_send_ump ::  COutHandle -> CMsg2 -> CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_message :: COutHandle -> CTimestamp -> CMsg1 -> CSize  -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_schedule_ump :: COutHandle -> CTimestamp -> CMsg2 -> CSize -> IO CErr

