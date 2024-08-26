module Libremidi where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Libremidi.Common
  ( BitEnum (..)
  , Cb
  , ForeignM
  , MallocPtr (..)
  , assocM
  , guardM
  , pokeField
  , toCBool
  )
import Libremidi.Foreign qualified as F

data TimestampMode
  = TimestampModeNone
  | TimestampModeRelative
  | TimestampModeAbsolute
  | TimestampModeSystemMono
  | TimestampModeAudioFrame
  | TimestampModeCustom
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.TimestampMode TimestampMode

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

instance BitEnum F.Api Api

data ConfigType
  = ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.ConfigType ConfigType

data Version
  = VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.Version Version

data LogLvl = LogLvlWarn | LogLvlErr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type LogFun = LogLvl -> Text -> IO ()

mkLogCb :: LogFun -> LogLvl -> IO (Cb F.LogFun)
mkLogCb f lvl = F.mkLogCb $ \_ s l _ -> do
  msg <- TF.fromPtr (coerce s) (fromIntegral l)
  f lvl msg

data ObsAct = ObsActAdd | ObsActRem
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ObsFun p = ObsAct -> Ptr p -> IO ()

mkObsCb :: ObsFun p -> ObsAct -> IO (Cb (F.ObsFun p))
mkObsCb f = F.mkObsCb . const . f

type EnumFun p = Ptr p -> IO ()

mkEnumCb :: EnumFun p -> IO (Cb (F.ObsFun p))
mkEnumCb = F.mkObsCb . const

data ObsConfig = ObsConfig
  { ocOnErr :: !(Maybe (Cb F.LogFun))
  , ocOnWarn :: !(Maybe (Cb F.LogFun))
  , ocInAdd :: !(Maybe (Cb (F.ObsFun F.InPort)))
  , ocInRem :: !(Maybe (Cb (F.ObsFun F.InPort)))
  , ocOutAdd :: !(Maybe (Cb (F.ObsFun F.OutPort)))
  , ocOutRem :: !(Maybe (Cb (F.ObsFun F.OutPort)))
  , ocTrackHardware :: !Bool
  , ocTrackVirtual :: !Bool
  , ocTrackAny :: !Bool
  , ocNotifInCon :: !Bool
  }
  deriving stock (Eq, Show)

defObsConfig :: ObsConfig
defObsConfig =
  ObsConfig
    { ocOnErr = Nothing
    , ocOnWarn = Nothing
    , ocInAdd = Nothing
    , ocInRem = Nothing
    , ocOutAdd = Nothing
    , ocOutRem = Nothing
    , ocTrackHardware = True
    , ocTrackVirtual = True
    , ocTrackAny = True
    , ocNotifInCon = True
    }

setLogCb :: LogFun -> ObsConfig -> IO ObsConfig
setLogCb f oc = do
  onErr <- mkLogCb f LogLvlErr
  onWarn <- mkLogCb f LogLvlWarn
  pure $
    oc
      { ocOnErr = Just onErr
      , ocOnWarn = Just onWarn
      }

setInPortCb :: ObsFun F.InPort -> ObsConfig -> IO ObsConfig
setInPortCb f oc = do
  onAdd <- mkObsCb f ObsActAdd
  onRem <- mkObsCb f ObsActRem
  pure $
    oc
      { ocInAdd = Just onAdd
      , ocInRem = Just onRem
      }

setOutPortCb :: ObsFun F.OutPort -> ObsConfig -> IO ObsConfig
setOutPortCb f oc = do
  onAdd <- mkObsCb f ObsActAdd
  onRem <- mkObsCb f ObsActRem
  pure $
    oc
      { ocOutAdd = Just onAdd
      , ocOutRem = Just onRem
      }

mkObsConfig :: ObsConfig -> ForeignM (ForeignPtr F.ObsConfig)
mkObsConfig oc = do
  foc <- liftIO (mallocPtr (Proxy @F.ObsConfig))
  poc <- assocM foc
  traverse_ (assocM >=> liftIO . pokeField F.ocOnErr poc) (ocOnErr oc)
  traverse_ (assocM >=> liftIO . pokeField F.ocOnWarn poc) (ocOnWarn oc)
  traverse_ (assocM >=> liftIO . pokeField F.ocInAdd poc) (ocInAdd oc)
  traverse_ (assocM >=> liftIO . pokeField F.ocInRem poc) (ocInRem oc)
  traverse_ (assocM >=> liftIO . pokeField F.ocOutAdd poc) (ocOutAdd oc)
  traverse_ (assocM >=> liftIO . pokeField F.ocOutRem poc) (ocOutRem oc)
  liftIO $ do
    pokeField F.ocTrackHardware poc (toCBool (ocTrackHardware oc))
    pokeField F.ocTrackVirtual poc (toCBool (ocTrackVirtual oc))
    pokeField F.ocTrackAny poc (toCBool (ocTrackAny oc))
    pokeField F.ocNotifInCon poc (toCBool (ocNotifInCon oc))
  pure foc

listInPorts :: ForeignPtr F.ObsHandle -> EnumFun F.InPort -> ForeignM ()
listInPorts foh f = do
  enumIn <- liftIO (mkEnumCb f)
  poh <- assocM foh
  fun <- assocM enumIn
  guardM (F.libremidi_midi_observer_enumerate_input_ports poh nullPtr fun)

listOutPorts :: ForeignPtr F.ObsHandle -> EnumFun F.OutPort -> ForeignM ()
listOutPorts foh f = do
  enumOut <- liftIO (mkEnumCb f)
  poh <- assocM foh
  fun <- assocM enumOut
  guardM (F.libremidi_midi_observer_enumerate_output_ports poh nullPtr fun)

data MidiPort
  = MidiPortIn !(ForeignPtr F.InPort)
  | MidiPortOut !(ForeignPtr F.OutPort)
  deriving stock (Eq, Show)

data MsgFun
  = MsgFun1 (Cb (F.MsgFun F.Sym1))
  | MsgFun2 (Cb (F.MsgFun F.Sym2))
  deriving stock (Eq, Show)

data MidiConfig = MidiConfig
  { mcVersion :: !Version
  , mcPort :: !(Maybe MidiPort)
  , mcOnMsg :: !(Maybe MsgFun)
  , mcGetTime :: !(Maybe (Cb F.TimeFun))
  , mcOnErr :: !(Maybe (Cb F.LogFun))
  , mcOnWarn :: !(Maybe (Cb F.LogFun))
  , mcPortName :: !Text
  , mcVirtualPort :: !Bool
  , mcIgnoreSysex :: !Bool
  , mcIgnoreTiming :: !Bool
  , mcIgnoreSensing :: !Bool
  , mcTimestamps :: !TimestampMode
  }
  deriving stock (Eq, Show)

-- defMidiConfig :: MidiConfig
-- defMidiConfig = MidiConfig
--   { mcVersion = undefined
--   , mcPort  = undefined
--   , mcOutPort  = undefined
--   , mcOnMsg1 = undefined
--   , mcOnMsg2 = undefined
--   , mcGetTime = undefined
--   , mcOnErr = undefined
--   , mcOnWarn = undefined
--   , mcPortName = undefined
--   , mcVirtualPort = undefined
--   , mcIgnoreSysex = undefined
--   , mcIgnoreTiming = undefined
--   , mcIgnoreSensing = undefined
--   , mcTimestamps = undefined
--   }

data ApiConfig = ApiConfig
  { acApi :: !Api
  , acConfigType :: !ConfigType
  }
  deriving stock (Eq, Show)

defApiConfig :: ApiConfig
defApiConfig =
  ApiConfig
    { acApi = ApiUnspecified
    , acConfigType = ConfigTypeObserver
    }

mkInHandle :: MidiConfig -> ApiConfig -> ForeignM F.InHandle
mkInHandle mc ac = undefined

-- DONE
-- libremidi_midi_in_port_clone :: Ptr InPortX -> Ptr (Ptr InPortX) -> IO Err
-- libremidi_midi_in_port_name :: Ptr InPortX -> Ptr CString -> Ptr CSize -> IO Err
-- libremidi_midi_out_port_clone :: Ptr OutPortX -> Ptr (Ptr OutPortX) -> IO Err
-- libremidi_midi_out_port_name :: Ptr OutPortX -> Ptr CString -> Ptr CSize -> IO Err
-- libremidi_midi_observer_new :: Ptr ObsConfigX -> Ptr ApiConfigX -> Ptr (Ptr ObsHandleX) -> IO Err
-- libremidi_midi_observer_enumerate_input_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr InFun -> IO Err
-- libremidi_midi_enumerate_output_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr OutFun -> IO Err
-- TODO
-- libremidi_midi_in_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr InHandleX) -> IO Err
-- libremidi_midi_in_is_connected :: Ptr InHandleX -> IO CBool
-- libremidi_midi_in_absolute_timestamp :: Ptr InHandleX -> IO Timestamp
-- libremidi_midi_in_free :: Ptr InHandleX -> IO Err
-- libremidi_midi_out_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr OutHandleX) -> IO Err
-- libremidi_midi_out_is_connected :: Ptr OutHandleX -> IO CBool
-- libremidi_midi_out_send_message :: Ptr OutHandleX -> Msg1 -> CSize -> IO Err
-- libremidi_midi_out_send_ump ::  Ptr OutHandleX -> Msg2 -> CSize -> IO Err
-- libremidi_midi_out_schedule_message :: Ptr OutHandleX -> Timestamp -> Msg1 -> CSize  -> IO Err
-- libremidi_midi_out_schedule_ump :: Ptr OutHandleX -> Timestamp -> Msg2 -> CSize -> IO Err
