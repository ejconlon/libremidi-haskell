{-# LANGUAGE ForeignFunctionInterface #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, plusPtr, FunPtr, freeHaskellFunPtr, castFunPtrToPtr, castPtrToFunPtr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Concurrent qualified as FC
import Data.Coerce (coerce)
import Foreign.Marshal.Utils (fillBytes)

#include "libremidi/libremidi-c.h"

type Field a b = a -> Ptr b

ptrSize :: Int
ptrSize = #{size midi1_message}

newtype Cb x = Cb { unCb :: ForeignPtr x }

cbWithPtr :: Cb x -> (FunPtr x -> IO a) -> IO a
cbWithPtr (Cb fp) f = withForeignPtr fp (f . castPtrToFunPtr)

cbMalloc :: (x -> IO (FunPtr x)) -> x -> IO (Cb x)
cbMalloc g x = do
  y <- g x
  fp <- FC.newForeignPtr (castFunPtrToPtr y) (freeHaskellFunPtr y)
  pure (Cb fp)

mallocForeignPtrBytes0 :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes0 len = do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp (\p -> fillBytes p 0 len)
  pure fp

type CErr = CInt

type CSym1 = CUChar
type CMsg1 = Ptr CSym1

type CSym2 = CUInt
type CMsg2 = Ptr CSym2

type CTimestamp = CLong

newtype CInPort = CInPort { unCInPort :: Ptr Void }
  deriving stock (Eq, Show)

newtype InPort = InPort { unInPort :: ForeignPtr Void }
  deriving stock (Eq, Show)

ipWithPtr :: InPort -> (CInPort -> IO a) -> IO a
ipWithPtr (InPort fp) f = withForeignPtr fp (f . coerce)

newtype COutPort = COutPort { unCOutPort :: Ptr Void }
  deriving stock (Eq, Show)

newtype OutPort = OutPort { unOutPort :: ForeignPtr Void }
  deriving stock (Eq, Show)

opWithPtr :: OutPort -> (COutPort -> IO a) -> IO a
opWithPtr (OutPort fp) f = withForeignPtr fp (f . coerce)

newtype CInHandle = CInHandle { unCInHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype InHandle = InHandle { unInHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

ihWithPtr :: InHandle -> (CInHandle -> IO a) -> IO a
ihWithPtr (InHandle fp) f = withForeignPtr fp (f . coerce)

newtype COutHandle = COutHandle { unCOutHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype OutHandle = OutHandle { unOutHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

ohWithPtr :: OutHandle -> (COutHandle -> IO a) -> IO a
ohWithPtr (OutHandle fp) f = withForeignPtr fp (f . coerce)

newtype CObsHandle = CObsHandle { unCObsHandle :: Ptr Void }
  deriving stock (Eq, Show)

newtype ObsHandle = ObsHandle { unObsHandle :: ForeignPtr Void }
  deriving stock (Eq, Show)

bhWithPtr :: ObsHandle -> (CObsHandle -> IO a) -> IO a
bhWithPtr (ObsHandle fp) f = withForeignPtr fp (f . coerce)

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

acWithPtr :: ApiConfig -> (CApiConfig -> IO a) -> IO a
acWithPtr (ApiConfig fp) f = withForeignPtr fp (f . coerce)

acMalloc :: IO ApiConfig
acMalloc = coerce (mallocForeignPtrBytes0 #{size libremidi_api_configuration})

type CLogCbFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()

newtype CLogCb = CLogCb { unCLogCb :: FunPtr CLogCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  clcbWrap :: CLogCbFun -> IO CLogCb

newtype LogCb = LogCb { unLogCb :: Cb CLogCbFun }

lcbWithPtr :: LogCb -> (CLogCb -> IO a) -> IO a
lcbWithPtr (LogCb c) f = cbWithPtr c (f . coerce)

lcbMalloc :: CLogCbFun -> IO LogCb
lcbMalloc = coerce . cbMalloc (coerce clcbWrap)

type CInCbFun = Ptr Void -> CInPort -> IO ()

newtype CInCb = CMidiInCb { unCMidiInCb :: FunPtr CInCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cicbWrap :: CInCbFun -> IO CInCb

newtype InCb = InCb { unInCb :: Cb CInCbFun }

icbWithPtr :: InCb -> (CInCb -> IO a) -> IO a
icbWithPtr (InCb c) f = cbWithPtr c (f . coerce)

icbMalloc :: CInCbFun -> IO InCb
icbMalloc = coerce . cbMalloc (coerce cicbWrap)

type COutCbFun = Ptr Void -> COutPort -> IO ()

newtype COutCb = CMidiOutCb { unCMidiOutCb :: FunPtr COutCbFun }
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cocbWrap :: COutCbFun -> IO COutCb

newtype OutCb = OutCb { unOutCb :: Cb COutCbFun }

ocbWithPtr :: OutCb -> (COutCb -> IO a) -> IO a
ocbWithPtr (OutCb c) f = cbWithPtr c (f . coerce)

ocbMalloc :: COutCbFun -> IO OutCb
ocbMalloc = coerce . cbMalloc (coerce cocbWrap)

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

ocWithPtr :: ObsConfig -> (CObsConfig -> IO a) -> IO a
ocWithPtr (ObsConfig fp) f = withForeignPtr fp (f . coerce)

ocMalloc :: IO ObsConfig
ocMalloc = coerce (mallocForeignPtrBytes0 #{size libremidi_observer_configuration})

type CMsg1CbFun = Ptr Void -> CMsg1 -> CSize -> IO ()

newtype CMsg1Cb = CMidi1Cb (FunPtr CMsg1CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm1cbWrap :: CMsg1CbFun -> IO CMsg1Cb

newtype Msg1Cb = Msg1Cb { unMsg1Cb :: Cb CMsg1CbFun }

m1cbWithPtr :: Msg1Cb -> (CMsg1Cb -> IO a) -> IO a
m1cbWithPtr (Msg1Cb c) f = cbWithPtr c (f . coerce)

m1cbMalloc :: CMsg1CbFun -> IO Msg1Cb
m1cbMalloc = coerce . cbMalloc (coerce cm1cbWrap)

type CMsg2CbFun = Ptr Void -> CMsg2 -> CSize -> IO ()

newtype CMsg2Cb = CMsg2Cb (FunPtr CMsg2CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm2cbWrap :: CMsg2CbFun -> IO CMsg2Cb

newtype Msg2Cb = Msg2Cb { unMsg2Cb :: Cb CMsg2CbFun }

m2cbWithPtr :: Msg2Cb -> (CMsg2Cb -> IO a) -> IO a
m2cbWithPtr (Msg2Cb c) f = cbWithPtr c (f . coerce)

m2cbMalloc :: CMsg2CbFun -> IO Msg2Cb
m2cbMalloc = coerce . cbMalloc (coerce cm2cbWrap)

type CTimeCbFun = Ptr Void -> CTimestamp -> IO ()

newtype CTimeCb = CTimeCb (FunPtr CTimeCbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  ctcbWrap :: CTimeCbFun -> IO CTimeCb

newtype TimeCb = TimeCb { unTimeCb :: Cb CTimeCbFun }

tcbWithPtr :: TimeCb -> (CTimeCb -> IO a) -> IO a
tcbWithPtr (TimeCb c) f = cbWithPtr c (f . coerce)

tcbMalloc :: CTimeCbFun -> IO TimeCb
tcbMalloc = coerce . cbMalloc (coerce ctcbWrap)

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

mcWithPtr :: MidiConfig -> (CMidiConfig -> IO a) -> IO a
mcWithPtr (MidiConfig fp) f = withForeignPtr fp (f . coerce)

mcMalloc :: IO MidiConfig
mcMalloc = coerce (mallocForeignPtrBytes0 #{size libremidi_midi_configuration})

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

