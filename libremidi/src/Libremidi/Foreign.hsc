{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..), CSize (..), CBool (..), CUInt (..), CLong (..), CUChar (..))
import Foreign.C.String (CString)
import Data.Void (Void)
import Foreign.Ptr (Ptr, plusPtr, FunPtr)
import Foreign.Marshal.Alloc (allocaBytes)

#include "libremidi/libremidi-c.h"

type Loc a b = a -> Ptr b

ptrSize :: Int
ptrSize = #{size midi1_message}

type CErr = CInt

guardErr :: IO CErr -> IO a -> IO (Either CErr a)
guardErr eact act = do
  err <- eact
  if err == 0
    then fmap Right act
    else pure (Left err)

type CSym1 = CUChar
type CMsg1 = Ptr CSym1

type CSym2 = CUInt
type CMsg2 = Ptr CSym2

type CTimestamp = CLong

newtype CInPort = CInPort (Ptr Void)
  deriving stock (Eq, Show)

newtype COutPort = COutPort (Ptr Void)
  deriving stock (Eq, Show)

newtype CInHandle = CInHandle (Ptr Void)
  deriving stock (Eq, Show)

newtype COutHandle = COutHandle (Ptr Void)
  deriving stock (Eq, Show)

newtype CObsHandle = CObsHandle (Ptr Void)
  deriving stock (Eq, Show)

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

newtype CApiConfig = CApiConfig (Ptr Void)
  deriving stock (Eq, Show)

cacApi :: Loc CApiConfig CApi
cacApi (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, api}

cacConfigType :: Loc CApiConfig CConfigType
cacConfigType (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, configuration_type}

cacData :: Loc CApiConfig (Ptr Void)
cacData (CApiConfig base) = plusPtr base #{offset libremidi_api_configuration, data}

cacAlloca :: (CApiConfig -> IO a) -> IO (Either CErr a)
cacAlloca f = allocaBytes #{size libremidi_api_configuration} $ \ptr ->
  let ptr' = CApiConfig ptr
  in guardErr (libremidi_midi_api_configuration_init ptr') (f ptr')

type CLogCbFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()
newtype CLogCb = CLogCb (FunPtr CLogCbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  clcWrap :: CLogCbFun -> IO CLogCb

type CInCbFun = Ptr Void -> CInPort -> IO ()
newtype CInCb = CMidiInCb (FunPtr CInCbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cmicWrap :: CInCbFun -> IO CInCb

type COutCbFun = Ptr Void -> COutPort -> IO ()
newtype COutCb = CMidiOutCb (FunPtr COutCbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cmocWrap :: COutCbFun -> IO COutCb

newtype CObsConfig = CObsConfig (Ptr Void)
  deriving stock (Eq, Show)

cocOnErrCb :: Loc CObsConfig CLogCb
cocOnErrCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, on_error})

cocOnWarnCb :: Loc CObsConfig CLogCb
cocOnWarnCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, on_warning})

cocInAddCb :: Loc CObsConfig CInCb
cocInAddCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_added})

cocInRemCb :: Loc CObsConfig CInCb
cocInRemCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_removed})

cocOutAddCb :: Loc CObsConfig COutCb
cocOutAddCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_added})

cocOutRemCb :: Loc CObsConfig COutCb
cocOutRemCb (CObsConfig base) = plusPtr base (ptrSize + #{offset libremidi_observer_configuration, input_removed})

cocTrackHardware :: Loc CObsConfig CBool
cocTrackHardware (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_hardware}

cocTrackVirtual :: Loc CObsConfig CBool
cocTrackVirtual (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_virtual}

cocTrackAny :: Loc CObsConfig CBool
cocTrackAny (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, track_any}

cocNotifInCon :: Loc CObsConfig CBool
cocNotifInCon (CObsConfig base) = plusPtr base #{offset libremidi_observer_configuration, notify_in_constructor}

cocAlloca :: (CObsConfig -> IO a) -> IO (Either CErr a)
cocAlloca f = allocaBytes #{size libremidi_observer_configuration} $ \ptr ->
  let ptr' = CObsConfig ptr
  in guardErr (libremidi_midi_observer_configuration_init ptr') (f ptr')

type CMsg1CbFun = Ptr Void -> CMsg1 -> CSize -> IO ()
newtype CMsg1Cb = CMidi1Cb (FunPtr CMsg1CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm1cWrap :: CMsg1CbFun -> IO CMsg1Cb

type CMsg2CbFun = Ptr Void -> CMsg2 -> CSize -> IO ()
newtype CMsg2Cb = CMsg2Cb (FunPtr CMsg2CbFun)
  deriving stock (Eq, Show)

foreign import ccall "wrapper"
  cm2cWrap :: CMsg2CbFun -> IO CMsg2Cb

type CTimeCbFun = Ptr Void -> CTimestamp -> IO ()
newtype CTimeCb = CTimeCb (FunPtr CTimeCbFun)
  deriving stock (Eq, Show)

newtype CMidiConfig = CMidiConfig (Ptr Void)
  deriving stock (Eq, Show)

cmcVersion :: Loc CMidiConfig CVersion
cmcVersion (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, version}

cmcInPort :: Loc CMidiConfig (Ptr CInPort)
cmcInPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, in_port}

cmcOutPort :: Loc CMidiConfig (Ptr COutPort)
cmcOutPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, out_port}

cmcMsg1Cb :: Loc CMidiConfig CMsg1Cb
cmcMsg1Cb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_midi1_message}

cmcMsg2Cb :: Loc CMidiConfig CMsg2Cb
cmcMsg2Cb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_midi2_message}

cmcTimeCb :: Loc CMidiConfig CTimeCb
cmcTimeCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, get_timestamp}

cmcOnErrCb :: Loc CMidiConfig CLogCb
cmcOnErrCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_error}

cmcOnWarnCb :: Loc CMidiConfig CLogCb
cmcOnWarnCb (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, on_warning}

cmcPortName :: Loc CMidiConfig CString
cmcPortName (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, port_name}

cmcVirtualPort :: Loc CMidiConfig CBool
cmcVirtualPort (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, virtual_port}

cmcIgnoreSysex :: Loc CMidiConfig CBool
cmcIgnoreSysex (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_sysex}

cmcIgnoreTiming :: Loc CMidiConfig CBool
cmcIgnoreTiming (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_timing}

cmcIgnoreSensing :: Loc CMidiConfig CBool
cmcIgnoreSensing (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, ignore_sensing}

cmcTimestamps :: Loc CMidiConfig CTimestampMode
cmcTimestamps (CMidiConfig base) = plusPtr base #{offset libremidi_midi_configuration, timestamps}

cmcAlloca :: (CMidiConfig -> IO a) -> IO (Either CErr a)
cmcAlloca f = allocaBytes #{size libremidi_midi_configuration} $ \ptr ->
  let ptr' = CMidiConfig ptr
  in guardErr (libremidi_midi_midi_configuration_init ptr') (f ptr')

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_api_configuration_init :: CApiConfig -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_configuration_init :: CObsConfig -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_midi_configuration_init :: CMidiConfig -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_clone :: CInPort -> Ptr CInPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_free :: CInPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_in_port_name :: CInPort -> Ptr CString -> Ptr CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_clone :: COutPort -> Ptr COutPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_free :: COutPort -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_port_name :: COutPort -> Ptr CString -> Ptr CSize -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_new :: CObsConfig -> CApiConfig -> Ptr CObsHandle -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_enumerate_input_ports :: CObsHandle -> Ptr Void -> CInCb -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_enumerate_output_ports :: CObsHandle -> Ptr Void -> COutCb -> IO CErr

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_observer_free :: CObsHandle -> IO CErr

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

foreign import ccall "libremidi/libremidi-c.h"
  libremidi_midi_out_free :: COutHandle -> IO CErr
