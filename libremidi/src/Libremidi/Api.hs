-- | Mid-level interface to Libremidi
-- Operations in IO do not fail
-- Operations in ErrM can possibly fail
module Libremidi.Api where

import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.Concurrent qualified as FC
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, nullPtr)
import Libremidi.Common
  ( BitEnum (..)
  , Cb
  , ErrM
  , Field
  , MallocPtr (..)
  , UniquePtr
  , checkM
  , consumeCb
  , consumeUniquePtr
  , fromCBool
  , fromCLong
  , pokeField
  , takeM
  , textM
  , toCBool
  , toCLong
  , toCSize
  , unRunErrM
  , withCb
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

instance BitEnum F.TimestampMode TimestampMode where
  fromBitEnum = \case
    a | a == F.tmNoTimestamp -> Just TimestampModeNone
    a | a == F.tmRelative -> Just TimestampModeRelative
    a | a == F.tmAbsolute -> Just TimestampModeAbsolute
    a | a == F.tmSystemMonotonic -> Just TimestampModeSystemMono
    a | a == F.tmAudioFrame -> Just TimestampModeAudioFrame
    a | a == F.tmCustom -> Just TimestampModeCustom
    _ -> Nothing
  toBitEnum = \case
    TimestampModeNone -> F.tmNoTimestamp
    TimestampModeRelative -> F.tmRelative
    TimestampModeAbsolute -> F.tmAbsolute
    TimestampModeSystemMono -> F.tmSystemMonotonic
    TimestampModeAudioFrame -> F.tmAudioFrame
    TimestampModeCustom -> F.tmCustom

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

instance BitEnum F.Api Api where
  fromBitEnum = \case
    a | a == F.apiUnspecified -> Just ApiUnspecified
    a | a == F.apiCoremidi -> Just ApiCoremidi
    a | a == F.apiAlsaSeq -> Just ApiAlsaSeq
    a | a == F.apiAlsaRaw -> Just ApiAlsaRaw
    a | a == F.apiJackMidi -> Just ApiJackMidi
    a | a == F.apiWindowsMm -> Just ApiWindowsMm
    a | a == F.apiWindowsUwp -> Just ApiWindowsUwp
    a | a == F.apiWebmidi -> Just ApiWebmidi
    a | a == F.apiPipewire -> Just ApiPipewire
    a | a == F.apiAlsaRawUmp -> Just ApiAlsaRawUmp
    a | a == F.apiAlsaSeqUmp -> Just ApiAlsaSeqUmp
    a | a == F.apiCoremidiUmp -> Just ApiCoremidiUmp
    a | a == F.apiWindowsMidiServices -> Just ApiWindowsMidiServices
    a | a == F.apiDummy -> Just ApiDummy
    _ -> Nothing
  toBitEnum = \case
    ApiUnspecified -> F.apiUnspecified
    ApiCoremidi -> F.apiCoremidi
    ApiAlsaSeq -> F.apiAlsaSeq
    ApiAlsaRaw -> F.apiAlsaRaw
    ApiJackMidi -> F.apiJackMidi
    ApiWindowsMm -> F.apiWindowsMm
    ApiWindowsUwp -> F.apiWindowsUwp
    ApiWebmidi -> F.apiWebmidi
    ApiPipewire -> F.apiPipewire
    ApiAlsaRawUmp -> F.apiAlsaRawUmp
    ApiAlsaSeqUmp -> F.apiAlsaSeqUmp
    ApiCoremidiUmp -> F.apiCoremidiUmp
    ApiWindowsMidiServices -> F.apiWindowsMidiServices
    ApiDummy -> F.apiDummy

data ConfigType
  = ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.ConfigType ConfigType where
  fromBitEnum = \case
    a | a == F.ctObserver -> Just ConfigTypeObserver
    a | a == F.ctInput -> Just ConfigTypeInput
    a | a == F.ctOutput -> Just ConfigTypeOutput
    _ -> Nothing
  toBitEnum = \case
    ConfigTypeObserver -> F.ctObserver
    ConfigTypeInput -> F.ctInput
    ConfigTypeOutput -> F.ctOutput

data Version
  = VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.Version Version where
  fromBitEnum = \case
    a | a == F.verMidi1 -> Just VersionMidi1
    a | a == F.verMidi2 -> Just VersionMidi2
    _ -> Nothing
  toBitEnum = \case
    VersionMidi1 -> F.verMidi1
    VersionMidi2 -> F.verMidi2

type Timestamp = Int64

type InPort = ForeignPtr F.InPort

cloneInPort :: Ptr F.InPort -> ErrM InPort
cloneInPort = takeM . F.libremidi_midi_in_port_clone

inPortName :: Ptr F.InPort -> ErrM Text
inPortName = textM . F.libremidi_midi_in_port_name

type OutPort = ForeignPtr F.OutPort

cloneOutPort :: Ptr F.OutPort -> ErrM OutPort
cloneOutPort = takeM . F.libremidi_midi_out_port_clone

outPortName :: Ptr F.OutPort -> ErrM Text
outPortName = textM . F.libremidi_midi_out_port_name

data LogLvl = LogLvlWarn | LogLvlErr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type LogFun = LogLvl -> Text -> IO ()

newLogCb :: LogFun -> LogLvl -> IO (Cb F.LogFun)
newLogCb f lvl = F.newLogCb $ \_ s l _ -> do
  msg <- liftIO (TF.fromPtr (coerce s) (fromIntegral l))
  f lvl msg

data ObsAct = ObsActAdd | ObsActRem
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ObsFun p = ObsAct -> Ptr p -> IO ()

type ObsInFun = ObsFun F.InPort

type ObsOutFun = ObsFun F.OutPort

newObsCb :: ObsFun p -> ObsAct -> IO (Cb (F.ObsFun p))
newObsCb f = F.newObsCb . const . f

type EnumFun p = Ptr p -> IO ()

type EnumInFun = EnumFun F.InPort

type EnumOutFun = EnumFun F.OutPort

newEnumCb :: EnumFun p -> IO (Cb (F.ObsFun p))
newEnumCb = F.newObsCb . const

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
  deriving stock (Eq)

instance Default ObsConfig where
  def =
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

setObsLogCb :: LogFun -> ObsConfig -> IO ObsConfig
setObsLogCb f oc = do
  onErr <- newLogCb f LogLvlErr
  onWarn <- newLogCb f LogLvlWarn
  pure $
    oc
      { ocOnErr = Just onErr
      , ocOnWarn = Just onWarn
      }

setObsInCb :: ObsInFun -> ObsConfig -> IO ObsConfig
setObsInCb f oc = do
  onAdd <- newObsCb f ObsActAdd
  onRem <- newObsCb f ObsActRem
  pure $
    oc
      { ocInAdd = Just onAdd
      , ocInRem = Just onRem
      }

setObsOutCb :: ObsOutFun -> ObsConfig -> IO ObsConfig
setObsOutCb f oc = do
  onAdd <- newObsCb f ObsActAdd
  onRem <- newObsCb f ObsActRem
  pure $
    oc
      { ocOutAdd = Just onAdd
      , ocOutRem = Just onRem
      }

mkObsConfig :: ObsConfig -> IO (ForeignPtr F.ObsConfig)
mkObsConfig oc = do
  f <- mallocPtr (Proxy @F.ObsConfig)
  withForeignPtr f $ \p -> do
    let
      setFun :: forall x. Field F.ObsConfig (FunPtr x) -> Cb x -> IO ()
      setFun field cb = do
        q <- consumeCb cb
        let w = castPtrToFunPtr (unsafeForeignPtrToPtr q)
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
    traverse_ (setFun F.ocOnErr) (ocOnErr oc)
    traverse_ (setFun F.ocOnWarn) (ocOnWarn oc)
    traverse_ (setFun F.ocInAdd) (ocInAdd oc)
    traverse_ (setFun F.ocInRem) (ocInRem oc)
    traverse_ (setFun F.ocOutAdd) (ocOutAdd oc)
    traverse_ (setFun F.ocOutRem) (ocOutRem oc)
    pokeField F.ocTrackHardware p (toCBool (ocTrackHardware oc))
    pokeField F.ocTrackVirtual p (toCBool (ocTrackVirtual oc))
    pokeField F.ocTrackAny p (toCBool (ocTrackAny oc))
    pokeField F.ocNotifInCon p (toCBool (ocNotifInCon oc))
  pure f

data MidiPort
  = MidiPortIn !(UniquePtr F.InPort)
  | MidiPortOut !(UniquePtr F.OutPort)
  deriving stock (Eq)

data MsgFun
  = MsgFun1 (Cb (F.MsgFun F.Sym1))
  | MsgFun2 (Cb (F.MsgFun F.Sym2))
  deriving stock (Eq)

msgFunVersion :: MsgFun -> Version
msgFunVersion = \case
  MsgFun1 _ -> VersionMidi1
  MsgFun2 _ -> VersionMidi2

data MidiConfig = MidiConfig
  { mcVersion :: !(Maybe Version)
  , mcPort :: !(Maybe MidiPort)
  , mcOnMsg :: !(Maybe MsgFun)
  , mcGetTime :: !(Maybe (Cb F.TimeFun))
  , mcOnErr :: !(Maybe (Cb F.LogFun))
  , mcOnWarn :: !(Maybe (Cb F.LogFun))
  , mcPortName :: !(Maybe Text)
  , mcVirtualPort :: !Bool
  , mcIgnoreSysex :: !Bool
  , mcIgnoreTiming :: !Bool
  , mcIgnoreSensing :: !Bool
  , mcTimestamps :: !TimestampMode
  }
  deriving stock (Eq)

instance Default MidiConfig where
  def =
    MidiConfig
      { mcVersion = Nothing
      , mcPort = Nothing
      , mcOnMsg = Nothing
      , mcGetTime = Nothing
      , mcOnErr = Nothing
      , mcOnWarn = Nothing
      , mcPortName = Nothing
      , mcVirtualPort = True
      , mcIgnoreSysex = False
      , mcIgnoreTiming = False
      , mcIgnoreSensing = True
      , mcTimestamps = TimestampModeAbsolute
      }

mkMidiConfig :: MidiConfig -> IO (ForeignPtr F.MidiConfig)
mkMidiConfig mc = do
  let ver = fromMaybe (maybe VersionMidi1 msgFunVersion (mcOnMsg mc)) (mcVersion mc)
  f <- mallocPtr (Proxy @F.MidiConfig)
  withForeignPtr f $ \p -> do
    let
      setFun :: forall x. Field F.MidiConfig (FunPtr x) -> Cb x -> IO ()
      setFun field cb = do
        q <- consumeCb cb
        let w = castPtrToFunPtr (unsafeForeignPtrToPtr q)
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
      setPtr :: forall x. Field F.MidiConfig (Ptr x) -> UniquePtr x -> IO ()
      setPtr field u = do
        q <- consumeUniquePtr u
        let w = unsafeForeignPtrToPtr q
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
    case mcPort mc of
      Just (MidiPortIn ip) -> setPtr F.mcInPort ip
      Just (MidiPortOut op) -> setPtr F.mcOutPort op
      Nothing -> pure ()
    case mcOnMsg mc of
      Just (MsgFun1 cb1) -> setFun F.mcOnMsg1 cb1
      Just (MsgFun2 cb2) -> setFun F.mcOnMsg2 cb2
      Nothing -> pure ()
    traverse_ (setFun F.mcGetTime) (mcGetTime mc)
    traverse_ (setFun F.mcOnErr) (mcOnErr mc)
    traverse_ (setFun F.mcOnWarn) (mcOnWarn mc)
    pokeField F.mcVersion p (toBitEnum ver)
    pokeField F.mcVirtualPort p (toCBool (mcVirtualPort mc))
    pokeField F.mcIgnoreSysex p (toCBool (mcIgnoreSysex mc))
    pokeField F.mcIgnoreTiming p (toCBool (mcIgnoreTiming mc))
    pokeField F.mcIgnoreSensing p (toCBool (mcIgnoreSensing mc))
    pokeField F.mcTimestamps p (toBitEnum (mcTimestamps mc))
  pure f

data ApiConfig = ApiConfig
  { acApi :: !Api
  , acConfigType :: !ConfigType
  }
  deriving stock (Eq)

instance Default ApiConfig where
  def =
    ApiConfig
      { acApi = ApiUnspecified
      , acConfigType = ConfigTypeObserver
      }

mkApiConfig :: ApiConfig -> IO (ForeignPtr F.ApiConfig)
mkApiConfig ac = do
  f <- mallocPtr (Proxy @F.ApiConfig)
  withForeignPtr f $ \p -> do
    pokeField F.acApi p (toBitEnum (acApi ac))
    pokeField F.acConfigType p (toBitEnum (acConfigType ac))
  pure f

type ObsHandle = ForeignPtr F.ObsHandle

newObsHandle :: Api -> ObsConfig -> ErrM ObsHandle
newObsHandle api oc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeObserver}))
  foc <- liftIO (mkObsConfig oc)
  let pac = unsafeForeignPtrToPtr fac
      poc = unsafeForeignPtrToPtr foc
  takeM (F.libremidi_midi_observer_new poc pac)

enumInPorts :: ObsHandle -> EnumInFun -> ErrM ()
enumInPorts h f = unRunErrM $ do
  cb <- newEnumCb f
  withForeignPtr h $ \p ->
    withCb cb $ \fun ->
      checkM (F.libremidi_midi_observer_enumerate_input_ports p nullPtr fun)

enumOutPorts :: ObsHandle -> EnumOutFun -> ErrM ()
enumOutPorts h f = unRunErrM $ do
  cb <- newEnumCb f
  withForeignPtr h $ \p ->
    withCb cb $ \fun ->
      checkM (F.libremidi_midi_observer_enumerate_output_ports p nullPtr fun)

type InHandle = ForeignPtr F.InHandle

newInHandle :: Api -> MidiConfig -> ErrM InHandle
newInHandle api mc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeInput}))
  fmc <- liftIO (mkMidiConfig mc)
  let pac = unsafeForeignPtrToPtr fac
      pmc = unsafeForeignPtrToPtr fmc
  takeM (F.libremidi_midi_in_new pmc pac)

inIsConnected :: InHandle -> IO Bool
inIsConnected fih = withForeignPtr fih (fmap fromCBool . F.libremidi_midi_in_is_connected)

inAbsTimestamp :: InHandle -> IO Timestamp
inAbsTimestamp fih = withForeignPtr fih (fmap fromCLong . F.libremidi_midi_in_absolute_timestamp)

type OutHandle = ForeignPtr F.OutHandle

newOutHandle :: Api -> MidiConfig -> ErrM OutHandle
newOutHandle api mc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeOutput}))
  fmc <- liftIO (mkMidiConfig mc)
  let pac = unsafeForeignPtrToPtr fac
      pmc = unsafeForeignPtrToPtr fmc
  takeM (F.libremidi_midi_out_new pmc pac)

outIsConnected :: OutHandle -> IO Bool
outIsConnected foh = withForeignPtr foh (fmap fromCBool . F.libremidi_midi_out_is_connected)

outSendMsg1 :: OutHandle -> Ptr F.Sym1 -> Int -> ErrM ()
outSendMsg1 h dat len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (F.libremidi_midi_out_send_message p dat (toCSize (fromIntegral len)))

outSchedMsg1 :: OutHandle -> Timestamp -> Ptr F.Sym1 -> Int -> ErrM ()
outSchedMsg1 h ts dat len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (F.libremidi_midi_out_schedule_message p (toCLong ts) dat (toCSize (fromIntegral len)))

outSendMsg2 :: OutHandle -> Ptr F.Sym2 -> Int -> ErrM ()
outSendMsg2 h dat len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (F.libremidi_midi_out_send_ump p dat (toCSize (fromIntegral len)))

outSchedMsg2 :: OutHandle -> Timestamp -> Ptr F.Sym2 -> Int -> ErrM ()
outSchedMsg2 h ts dat len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (F.libremidi_midi_out_schedule_ump p (toCLong ts) dat (toCSize (fromIntegral len)))
