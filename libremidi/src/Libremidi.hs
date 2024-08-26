module Libremidi where

import Data.Default (Default (..))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Data.Maybe (fromMaybe)
import Libremidi.Common
  ( BitEnum (..)
  , Cb
  , ForeignM
  , MallocPtr (..)
  , assocM
  , guardM
  , pokeField
  , toCBool
  , scopeM
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

data ObsConfigBuilder = ObsConfigBuilder
  { ocbOnErr :: !(Maybe (Cb F.LogFun))
  , ocbOnWarn :: !(Maybe (Cb F.LogFun))
  , ocbInAdd :: !(Maybe (Cb (F.ObsFun F.InPort)))
  , ocbInRem :: !(Maybe (Cb (F.ObsFun F.InPort)))
  , ocbOutAdd :: !(Maybe (Cb (F.ObsFun F.OutPort)))
  , ocbOutRem :: !(Maybe (Cb (F.ObsFun F.OutPort)))
  , ocbTrackHardware :: !Bool
  , ocbTrackVirtual :: !Bool
  , ocbTrackAny :: !Bool
  , ocbNotifInCon :: !Bool
  }
  deriving stock (Eq, Show)

instance Default ObsConfigBuilder where
  def = ObsConfigBuilder
    { ocbOnErr = Nothing
    , ocbOnWarn = Nothing
    , ocbInAdd = Nothing
    , ocbInRem = Nothing
    , ocbOutAdd = Nothing
    , ocbOutRem = Nothing
    , ocbTrackHardware = True
    , ocbTrackVirtual = True
    , ocbTrackAny = True
    , ocbNotifInCon = True
    }

ocbSetLogCb :: LogFun -> ObsConfigBuilder -> IO ObsConfigBuilder
ocbSetLogCb f ocb = do
  onErr <- mkLogCb f LogLvlErr
  onWarn <- mkLogCb f LogLvlWarn
  pure $
    ocb
      { ocbOnErr = Just onErr
      , ocbOnWarn = Just onWarn
      }

ocbSetInCb :: ObsFun F.InPort -> ObsConfigBuilder -> IO ObsConfigBuilder
ocbSetInCb f oc = do
  onAdd <- mkObsCb f ObsActAdd
  onRem <- mkObsCb f ObsActRem
  pure $
    oc
      { ocbInAdd = Just onAdd
      , ocbInRem = Just onRem
      }

ocbSetOutCb :: ObsFun F.OutPort -> ObsConfigBuilder -> IO ObsConfigBuilder
ocbSetOutCb f oc = do
  onAdd <- mkObsCb f ObsActAdd
  onRem <- mkObsCb f ObsActRem
  pure $
    oc
      { ocbOutAdd = Just onAdd
      , ocbOutRem = Just onRem
      }

buildObsConfig :: ObsConfigBuilder -> ForeignM (ForeignPtr F.ObsConfig)
buildObsConfig ocb = do
  foc <- liftIO (mallocPtr (Proxy @F.ObsConfig))
  poc <- assocM foc
  traverse_ (assocM >=> liftIO . pokeField F.ocOnErr poc) (ocbOnErr ocb)
  traverse_ (assocM >=> liftIO . pokeField F.ocOnWarn poc) (ocbOnWarn ocb)
  traverse_ (assocM >=> liftIO . pokeField F.ocInAdd poc) (ocbInAdd ocb)
  traverse_ (assocM >=> liftIO . pokeField F.ocInRem poc) (ocbInRem ocb)
  traverse_ (assocM >=> liftIO . pokeField F.ocOutAdd poc) (ocbOutAdd ocb)
  traverse_ (assocM >=> liftIO . pokeField F.ocOutRem poc) (ocbOutRem ocb)
  liftIO $ do
    pokeField F.ocTrackHardware poc (toCBool (ocbTrackHardware ocb))
    pokeField F.ocTrackVirtual poc (toCBool (ocbTrackVirtual ocb))
    pokeField F.ocTrackAny poc (toCBool (ocbTrackAny ocb))
    pokeField F.ocNotifInCon poc (toCBool (ocbNotifInCon ocb))
  pure foc

data MidiPort
  = MidiPortIn !(ForeignPtr F.InPort)
  | MidiPortOut !(ForeignPtr F.OutPort)
  deriving stock (Eq, Show)

data MsgFun
  = MsgFun1 (Cb (F.MsgFun F.Sym1))
  | MsgFun2 (Cb (F.MsgFun F.Sym2))
  deriving stock (Eq, Show)

msgFunVersion :: MsgFun -> Version
msgFunVersion = \case
  MsgFun1 _ -> VersionMidi1
  MsgFun2 _ -> VersionMidi2

data MidiConfigBuilder = MidiConfigBuilder
  { mcbVersion :: !(Maybe Version)
  , mcbPort :: !(Maybe MidiPort)
  , mcbOnMsg :: !(Maybe MsgFun)
  , mcbGetTime :: !(Maybe (Cb F.TimeFun))
  , mcbOnErr :: !(Maybe (Cb F.LogFun))
  , mcbOnWarn :: !(Maybe (Cb F.LogFun))
  , mcbPortName :: !(Maybe Text)
  , mcbVirtualPort :: !Bool
  , mcbIgnoreSysex :: !Bool
  , mcbIgnoreTiming :: !Bool
  , mcbIgnoreSensing :: !Bool
  , mcbTimestamps :: !TimestampMode
  }
  deriving stock (Eq, Show)

instance Default MidiConfigBuilder where
  def = MidiConfigBuilder
    { mcbVersion = Nothing
    , mcbPort  = Nothing
    , mcbOnMsg = Nothing
    , mcbGetTime = Nothing
    , mcbOnErr = Nothing
    , mcbOnWarn = Nothing
    , mcbPortName = Nothing
    , mcbVirtualPort = True
    , mcbIgnoreSysex = False
    , mcbIgnoreTiming = False
    , mcbIgnoreSensing = True
    , mcbTimestamps = TimestampModeAbsolute
    }

buildMidiConfig :: MidiConfigBuilder -> ForeignM (ForeignPtr F.MidiConfig)
buildMidiConfig mcb = do
  let ver = fromMaybe (maybe VersionMidi1 msgFunVersion (mcbOnMsg mcb)) (mcbVersion mcb)
  fmc <- liftIO (mallocPtr (Proxy @F.MidiConfig))
  pmc <- assocM fmc
  case mcbPort mcb of
    Just (MidiPortIn fip) -> assocM fip >>= liftIO . pokeField F.mcInPort pmc
    Just (MidiPortOut fop) -> assocM fop >>= liftIO . pokeField F.mcOutPort pmc
    Nothing -> pure ()
  case mcbOnMsg mcb of
    Just (MsgFun1 cb1) -> assocM cb1 >>= liftIO . pokeField F.mcOnMsg1 pmc
    Just (MsgFun2 cb2) -> assocM cb2 >>= liftIO . pokeField F.mcOnMsg2 pmc
    Nothing -> pure ()
  traverse_ (assocM >=> liftIO . pokeField F.mcGetTime pmc) (mcbGetTime mcb)
  traverse_ (assocM >=> liftIO . pokeField F.mcOnErr pmc) (mcbOnErr mcb)
  traverse_ (assocM >=> liftIO . pokeField F.mcOnWarn pmc) (mcbOnWarn mcb)
  liftIO $ do
    pokeField F.mcVersion pmc (toBitEnum ver)
    pokeField F.mcVirtualPort pmc (toCBool (mcbVirtualPort mcb))
    pokeField F.mcIgnoreSysex pmc (toCBool (mcbIgnoreSysex mcb))
    pokeField F.mcIgnoreTiming pmc (toCBool (mcbIgnoreTiming mcb))
    pokeField F.mcIgnoreSensing pmc (toCBool (mcbIgnoreSensing mcb))
    pokeField F.mcTimestamps pmc (toBitEnum (mcbTimestamps mcb))
  pure fmc

data ApiConfigBuilder = ApiConfigBuilder
  { acbApi :: !Api
  , acbConfigType :: !ConfigType
  }
  deriving stock (Eq, Show)

instance Default ApiConfigBuilder where
  def = ApiConfigBuilder
    { acbApi = ApiUnspecified
    , acbConfigType = ConfigTypeObserver
    }

buildApiConfig :: ApiConfigBuilder -> ForeignM (ForeignPtr F.ApiConfig)
buildApiConfig acb = do
  fac <- liftIO (mallocPtr (Proxy @F.ApiConfig))
  pac <- assocM fac
  liftIO $ do
    pokeField F.acApi pac (toBitEnum (acbApi acb))
    pokeField F.acConfigType pac (toBitEnum (acbConfigType acb))
  pure fac

listInPorts :: ForeignPtr F.ObsHandle -> EnumFun F.InPort -> ForeignM ()
listInPorts foh f = scopeM $ do
  enumIn <- liftIO (mkEnumCb f)
  poh <- assocM foh
  fun <- assocM enumIn
  guardM (F.libremidi_midi_observer_enumerate_input_ports poh nullPtr fun)

listOutPorts :: ForeignPtr F.ObsHandle -> EnumFun F.OutPort -> ForeignM ()
listOutPorts foh f = scopeM $ do
  enumOut <- liftIO (mkEnumCb f)
  poh <- assocM foh
  fun <- assocM enumOut
  guardM (F.libremidi_midi_observer_enumerate_output_ports poh nullPtr fun)

-- DONE
-- libremidi_midi_in_port_clone :: Ptr InPortX -> Ptr (Ptr InPortX) -> IO Err
-- libremidi_midi_in_port_name :: Ptr InPortX -> Ptr CString -> Ptr CSize -> IO Err
-- libremidi_midi_out_port_clone :: Ptr OutPortX -> Ptr (Ptr OutPortX) -> IO Err
-- libremidi_midi_out_port_name :: Ptr OutPortX -> Ptr CString -> Ptr CSize -> IO Err
-- libremidi_midi_observer_new :: Ptr ObsConfigX -> Ptr ApiConfigX -> Ptr (Ptr ObsHandleX) -> IO Err
-- libremidi_midi_observer_enumerate_input_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr InFun -> IO Err
-- libremidi_midi_enumerate_output_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr OutFun -> IO Err
-- libremidi_midi_in_free :: Ptr InHandleX -> IO Err
-- TODO
-- libremidi_midi_in_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr InHandleX) -> IO Err
-- libremidi_midi_in_is_connected :: Ptr InHandleX -> IO CBool
-- libremidi_midi_in_absolute_timestamp :: Ptr InHandleX -> IO Timestamp
-- libremidi_midi_out_new :: Ptr MidiConfigX -> Ptr ApiConfigX -> Ptr (Ptr OutHandleX) -> IO Err
-- libremidi_midi_out_is_connected :: Ptr OutHandleX -> IO CBool
-- libremidi_midi_out_send_message :: Ptr OutHandleX -> Msg1 -> CSize -> IO Err
-- libremidi_midi_out_send_ump ::  Ptr OutHandleX -> Msg2 -> CSize -> IO Err
-- libremidi_midi_out_schedule_message :: Ptr OutHandleX -> Timestamp -> Msg1 -> CSize  -> IO Err
-- libremidi_midi_out_schedule_ump :: Ptr OutHandleX -> Timestamp -> Msg2 -> CSize -> IO Err
