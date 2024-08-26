module Libremidi where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Libremidi.Common
  ( BitEnum (..)
  , Cb
  , ErrM
  , ForeignM
  , MallocPtr (..)
  , assocM
  , fromCBool
  , fromCLong
  , guardM
  , pokeField
  , runForeignM
  , scopeM
  , toCBool
  , toCLong
  , toCSize
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

type Timestamp = Int64

data LogLvl = LogLvlWarn | LogLvlErr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type LogFun = LogLvl -> Text -> IO ()

mkLogCb :: LogFun -> LogLvl -> IO (Cb F.LogFun)
mkLogCb f lvl = F.mkLogCb $ \_ s l _ -> do
  msg <- liftIO (TF.fromPtr (coerce s) (fromIntegral l))
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
  def =
    ObsConfigBuilder
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
  def =
    MidiConfigBuilder
      { mcbVersion = Nothing
      , mcbPort = Nothing
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
  def =
    ApiConfigBuilder
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

newObsHandle :: Api -> ForeignPtr F.ObsConfig -> ForeignM (ForeignPtr F.ObsHandle)
newObsHandle api foc = do
  fac <- buildApiConfig (ApiConfigBuilder {acbApi = api, acbConfigType = ConfigTypeObserver})
  pac <- assocM fac
  poc <- assocM foc
  F.obsNew poc pac

-- Will be done automatically on GC but you can force it early
freeObsHandle :: ForeignPtr F.ObsHandle -> IO ()
freeObsHandle = finalizeForeignPtr

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

newInHandle :: Api -> ForeignPtr F.MidiConfig -> ForeignM (ForeignPtr F.InHandle)
newInHandle api fmc = do
  fac <- buildApiConfig (ApiConfigBuilder {acbApi = api, acbConfigType = ConfigTypeInput})
  pac <- assocM fac
  pmc <- assocM fmc
  F.inNew pmc pac

-- Will be done automatically on GC but you can force it early
freeInHandle :: ForeignPtr F.InHandle -> IO ()
freeInHandle = finalizeForeignPtr

inIsConnected :: ForeignPtr F.InHandle -> IO Bool
inIsConnected fih = withForeignPtr fih (fmap fromCBool . F.libremidi_midi_in_is_connected)

inAbsTimestamp :: ForeignPtr F.InHandle -> IO Timestamp
inAbsTimestamp fih = withForeignPtr fih (fmap fromCLong . F.libremidi_midi_in_absolute_timestamp)

newOutHandle :: Api -> ForeignPtr F.MidiConfig -> ForeignM (ForeignPtr F.OutHandle)
newOutHandle api fmc = do
  fac <- buildApiConfig (ApiConfigBuilder {acbApi = api, acbConfigType = ConfigTypeOutput})
  pac <- assocM fac
  pmc <- assocM fmc
  F.outNew pmc pac

-- Will be done automatically on GC but you can do it early
freeOutHandle :: ForeignPtr F.OutHandle -> IO ()
freeOutHandle = finalizeForeignPtr

outIsConnected :: ForeignPtr F.OutHandle -> IO Bool
outIsConnected foh = withForeignPtr foh (fmap fromCBool . F.libremidi_midi_out_is_connected)

outSendMsg1 :: ForeignPtr F.OutHandle -> Ptr F.Sym1 -> Int -> ErrM ()
outSendMsg1 foh ptr len = runForeignM $ do
  poh <- assocM foh
  guardM (F.libremidi_midi_out_send_message poh ptr (toCSize (fromIntegral len)))

outSchedMsg1 :: ForeignPtr F.OutHandle -> Timestamp -> Ptr F.Sym1 -> Int -> ErrM ()
outSchedMsg1 foh ts ptr len = runForeignM $ do
  poh <- assocM foh
  guardM (F.libremidi_midi_out_schedule_message poh (toCLong ts) ptr (toCSize (fromIntegral len)))

outSendMsg2 :: ForeignPtr F.OutHandle -> Ptr F.Sym2 -> Int -> ErrM ()
outSendMsg2 foh ptr len = runForeignM $ do
  poh <- assocM foh
  guardM (F.libremidi_midi_out_send_ump poh ptr (toCSize (fromIntegral len)))

outSchedMsg2 :: ForeignPtr F.OutHandle -> Timestamp -> Ptr F.Sym2 -> Int -> ErrM ()
outSchedMsg2 foh ts ptr len = runForeignM $ do
  poh <- assocM foh
  guardM (F.libremidi_midi_out_schedule_ump poh (toCLong ts) ptr (toCSize (fromIntegral len)))
