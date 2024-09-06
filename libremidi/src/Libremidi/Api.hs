-- | Mid-level interface to Libremidi.
-- Operations in IO do not fail.
-- Operations in ErrM can possibly fail.
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
  , assocM
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
import Libremidi.Foreign qualified as LMF

data TimestampMode
  = TimestampModeNone
  | TimestampModeRelative
  | TimestampModeAbsolute
  | TimestampModeSystemMono
  | TimestampModeAudioFrame
  | TimestampModeCustom
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum LMF.TimestampMode TimestampMode where
  fromBitEnum = \case
    a | a == LMF.tmNoTimestamp -> Just TimestampModeNone
    a | a == LMF.tmRelative -> Just TimestampModeRelative
    a | a == LMF.tmAbsolute -> Just TimestampModeAbsolute
    a | a == LMF.tmSystemMonotonic -> Just TimestampModeSystemMono
    a | a == LMF.tmAudioFrame -> Just TimestampModeAudioFrame
    a | a == LMF.tmCustom -> Just TimestampModeCustom
    _ -> Nothing
  toBitEnum = \case
    TimestampModeNone -> LMF.tmNoTimestamp
    TimestampModeRelative -> LMF.tmRelative
    TimestampModeAbsolute -> LMF.tmAbsolute
    TimestampModeSystemMono -> LMF.tmSystemMonotonic
    TimestampModeAudioFrame -> LMF.tmAudioFrame
    TimestampModeCustom -> LMF.tmCustom

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

instance BitEnum LMF.Api Api where
  fromBitEnum = \case
    a | a == LMF.apiUnspecified -> Just ApiUnspecified
    a | a == LMF.apiCoremidi -> Just ApiCoremidi
    a | a == LMF.apiAlsaSeq -> Just ApiAlsaSeq
    a | a == LMF.apiAlsaRaw -> Just ApiAlsaRaw
    a | a == LMF.apiJackMidi -> Just ApiJackMidi
    a | a == LMF.apiWindowsMm -> Just ApiWindowsMm
    a | a == LMF.apiWindowsUwp -> Just ApiWindowsUwp
    a | a == LMF.apiWebmidi -> Just ApiWebmidi
    a | a == LMF.apiPipewire -> Just ApiPipewire
    a | a == LMF.apiAlsaRawUmp -> Just ApiAlsaRawUmp
    a | a == LMF.apiAlsaSeqUmp -> Just ApiAlsaSeqUmp
    a | a == LMF.apiCoremidiUmp -> Just ApiCoremidiUmp
    a | a == LMF.apiWindowsMidiServices -> Just ApiWindowsMidiServices
    a | a == LMF.apiDummy -> Just ApiDummy
    _ -> Nothing
  toBitEnum = \case
    ApiUnspecified -> LMF.apiUnspecified
    ApiCoremidi -> LMF.apiCoremidi
    ApiAlsaSeq -> LMF.apiAlsaSeq
    ApiAlsaRaw -> LMF.apiAlsaRaw
    ApiJackMidi -> LMF.apiJackMidi
    ApiWindowsMm -> LMF.apiWindowsMm
    ApiWindowsUwp -> LMF.apiWindowsUwp
    ApiWebmidi -> LMF.apiWebmidi
    ApiPipewire -> LMF.apiPipewire
    ApiAlsaRawUmp -> LMF.apiAlsaRawUmp
    ApiAlsaSeqUmp -> LMF.apiAlsaSeqUmp
    ApiCoremidiUmp -> LMF.apiCoremidiUmp
    ApiWindowsMidiServices -> LMF.apiWindowsMidiServices
    ApiDummy -> LMF.apiDummy

data ConfigType
  = ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum LMF.ConfigType ConfigType where
  fromBitEnum = \case
    a | a == LMF.ctObserver -> Just ConfigTypeObserver
    a | a == LMF.ctInput -> Just ConfigTypeInput
    a | a == LMF.ctOutput -> Just ConfigTypeOutput
    _ -> Nothing
  toBitEnum = \case
    ConfigTypeObserver -> LMF.ctObserver
    ConfigTypeInput -> LMF.ctInput
    ConfigTypeOutput -> LMF.ctOutput

data Version
  = VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum LMF.Version Version where
  fromBitEnum = \case
    a | a == LMF.verMidi1 -> Just VersionMidi1
    a | a == LMF.verMidi2 -> Just VersionMidi2
    _ -> Nothing
  toBitEnum = \case
    VersionMidi1 -> LMF.verMidi1
    VersionMidi2 -> LMF.verMidi2

type Timestamp = Int64

type InPort = ForeignPtr LMF.InPort

freeInPort :: InPort -> IO ()
freeInPort = finalizeForeignPtr

cloneInPort' :: Ptr LMF.InPort -> ErrM InPort
cloneInPort' = takeM . LMF.libremidi_midi_in_port_clone

cloneInPort :: InPort -> ErrM InPort
cloneInPort = assocM cloneInPort'

inPortName' :: Ptr LMF.InPort -> ErrM Text
inPortName' = textM . LMF.libremidi_midi_in_port_name

inPortName :: InPort -> ErrM Text
inPortName = assocM inPortName'

type OutPort = ForeignPtr LMF.OutPort

freeOutPort :: OutPort -> IO ()
freeOutPort = finalizeForeignPtr

cloneOutPort' :: Ptr LMF.OutPort -> ErrM OutPort
cloneOutPort' = takeM . LMF.libremidi_midi_out_port_clone

cloneOutPort :: OutPort -> ErrM OutPort
cloneOutPort = assocM cloneOutPort'

outPortName' :: Ptr LMF.OutPort -> ErrM Text
outPortName' = textM . LMF.libremidi_midi_out_port_name

outPortName :: OutPort -> ErrM Text
outPortName = assocM outPortName'

data LogLvl = LogLvlWarn | LogLvlErr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type LogFun = LogLvl -> Text -> IO ()

newLogCb :: LogFun -> LogLvl -> IO (Cb LMF.LogFun)
newLogCb f lvl = LMF.newLogCb $ \_ s l _ -> do
  msg <- liftIO (TF.fromPtr (coerce s) (fromIntegral l))
  f lvl msg

data ObsAct = ObsActAdd | ObsActRem
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ObsFun p = ObsAct -> Ptr p -> IO ()

type ObsInFun = ObsFun LMF.InPort

type ObsOutFun = ObsFun LMF.OutPort

newObsCb :: ObsFun p -> ObsAct -> IO (Cb (LMF.ObsFun p))
newObsCb f = LMF.newObsCb . const . f

type EnumFun p = Ptr p -> IO ()

type EnumInFun = EnumFun LMF.InPort

type EnumOutFun = EnumFun LMF.OutPort

newEnumCb :: EnumFun p -> IO (Cb (LMF.ObsFun p))
newEnumCb = LMF.newObsCb . const

data ObsConfig = ObsConfig
  { ocOnErr :: !(Maybe (Cb LMF.LogFun))
  , ocOnWarn :: !(Maybe (Cb LMF.LogFun))
  , ocInAdd :: !(Maybe (Cb (LMF.ObsFun LMF.InPort)))
  , ocInRem :: !(Maybe (Cb (LMF.ObsFun LMF.InPort)))
  , ocOutAdd :: !(Maybe (Cb (LMF.ObsFun LMF.OutPort)))
  , ocOutRem :: !(Maybe (Cb (LMF.ObsFun LMF.OutPort)))
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

mkObsConfig :: ObsConfig -> IO (ForeignPtr LMF.ObsConfig)
mkObsConfig oc = do
  f <- mallocPtr (Proxy @LMF.ObsConfig)
  withForeignPtr f $ \p -> do
    let
      setFun :: forall x. Field LMF.ObsConfig (FunPtr x) -> Cb x -> IO ()
      setFun field cb = do
        q <- consumeCb cb
        let w = castPtrToFunPtr (unsafeForeignPtrToPtr q)
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
    traverse_ (setFun LMF.ocOnErr) (ocOnErr oc)
    traverse_ (setFun LMF.ocOnWarn) (ocOnWarn oc)
    traverse_ (setFun LMF.ocInAdd) (ocInAdd oc)
    traverse_ (setFun LMF.ocInRem) (ocInRem oc)
    traverse_ (setFun LMF.ocOutAdd) (ocOutAdd oc)
    traverse_ (setFun LMF.ocOutRem) (ocOutRem oc)
    pokeField LMF.ocTrackHardware p (toCBool (ocTrackHardware oc))
    pokeField LMF.ocTrackVirtual p (toCBool (ocTrackVirtual oc))
    pokeField LMF.ocTrackAny p (toCBool (ocTrackAny oc))
    pokeField LMF.ocNotifInCon p (toCBool (ocNotifInCon oc))
  pure f

data MidiPort
  = MidiPortIn !(UniquePtr LMF.InPort)
  | MidiPortOut !(UniquePtr LMF.OutPort)
  deriving stock (Eq)

data MsgFun
  = MsgFun1 (Cb (LMF.MsgFun LMF.Sym1))
  | MsgFun2 (Cb (LMF.MsgFun LMF.Sym2))
  deriving stock (Eq)

msgFunVersion :: MsgFun -> Version
msgFunVersion = \case
  MsgFun1 _ -> VersionMidi1
  MsgFun2 _ -> VersionMidi2

data MidiConfig = MidiConfig
  { mcVersion :: !(Maybe Version)
  , mcPort :: !(Maybe MidiPort)
  , mcOnMsg :: !(Maybe MsgFun)
  , mcGetTime :: !(Maybe (Cb LMF.TimeFun))
  , mcOnErr :: !(Maybe (Cb LMF.LogFun))
  , mcOnWarn :: !(Maybe (Cb LMF.LogFun))
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
      , mcVirtualPort = True
      , mcIgnoreSysex = False
      , mcIgnoreTiming = False
      , mcIgnoreSensing = True
      , mcTimestamps = TimestampModeAbsolute
      }

mkMidiConfig :: MidiConfig -> IO (ForeignPtr LMF.MidiConfig)
mkMidiConfig mc = do
  let ver = fromMaybe (maybe VersionMidi1 msgFunVersion (mcOnMsg mc)) (mcVersion mc)
  f <- mallocPtr (Proxy @LMF.MidiConfig)
  withForeignPtr f $ \p -> do
    let
      setFun :: forall x. Field LMF.MidiConfig (FunPtr x) -> Cb x -> IO ()
      setFun field cb = do
        q <- consumeCb cb
        let w = castPtrToFunPtr (unsafeForeignPtrToPtr q)
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
      setPtr :: forall x. Field LMF.MidiConfig (Ptr x) -> UniquePtr x -> IO ()
      setPtr field u = do
        q <- consumeUniquePtr u
        let w = unsafeForeignPtrToPtr q
        pokeField field p w
        FC.addForeignPtrFinalizer f (finalizeForeignPtr q)
    case mcPort mc of
      Just (MidiPortIn ip) -> setPtr LMF.mcInPort ip
      Just (MidiPortOut op) -> setPtr LMF.mcOutPort op
      Nothing -> pure ()
    case mcOnMsg mc of
      Just (MsgFun1 cb1) -> setFun LMF.mcOnMsg1 cb1
      Just (MsgFun2 cb2) -> setFun LMF.mcOnMsg2 cb2
      Nothing -> pure ()
    traverse_ (setFun LMF.mcGetTime) (mcGetTime mc)
    traverse_ (setFun LMF.mcOnErr) (mcOnErr mc)
    traverse_ (setFun LMF.mcOnWarn) (mcOnWarn mc)
    pokeField LMF.mcVersion p (toBitEnum ver)
    pokeField LMF.mcVirtualPort p (toCBool (mcVirtualPort mc))
    pokeField LMF.mcIgnoreSysex p (toCBool (mcIgnoreSysex mc))
    pokeField LMF.mcIgnoreTiming p (toCBool (mcIgnoreTiming mc))
    pokeField LMF.mcIgnoreSensing p (toCBool (mcIgnoreSensing mc))
    pokeField LMF.mcTimestamps p (toBitEnum (mcTimestamps mc))
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

mkApiConfig :: ApiConfig -> IO (ForeignPtr LMF.ApiConfig)
mkApiConfig ac = do
  f <- mallocPtr (Proxy @LMF.ApiConfig)
  withForeignPtr f $ \p -> do
    pokeField LMF.acApi p (toBitEnum (acApi ac))
    pokeField LMF.acConfigType p (toBitEnum (acConfigType ac))
  pure f

type ObsHandle = ForeignPtr LMF.ObsHandle

newObsHandle :: Api -> ObsConfig -> ErrM ObsHandle
newObsHandle api oc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeObserver}))
  foc <- liftIO (mkObsConfig oc)
  let pac = unsafeForeignPtrToPtr fac
      poc = unsafeForeignPtrToPtr foc
  takeM (LMF.libremidi_midi_observer_new poc pac)

freeObsHandle :: ObsHandle -> IO ()
freeObsHandle = finalizeForeignPtr

enumInPorts :: ObsHandle -> EnumInFun -> ErrM ()
enumInPorts h f = unRunErrM $ do
  cb <- newEnumCb f
  withForeignPtr h $ \p ->
    withCb cb $ \fun ->
      checkM (LMF.libremidi_midi_observer_enumerate_input_ports p nullPtr fun)

enumOutPorts :: ObsHandle -> EnumOutFun -> ErrM ()
enumOutPorts h f = unRunErrM $ do
  cb <- newEnumCb f
  withForeignPtr h $ \p ->
    withCb cb $ \fun ->
      checkM (LMF.libremidi_midi_observer_enumerate_output_ports p nullPtr fun)

type InHandle = ForeignPtr LMF.InHandle

newInHandle :: Api -> MidiConfig -> ErrM InHandle
newInHandle api mc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeInput}))
  fmc <- liftIO (mkMidiConfig mc)
  let pac = unsafeForeignPtrToPtr fac
      pmc = unsafeForeignPtrToPtr fmc
  takeM (LMF.libremidi_midi_in_new pmc pac)

freeInHandle :: InHandle -> IO ()
freeInHandle = finalizeForeignPtr

inIsConnected :: InHandle -> IO Bool
inIsConnected fih = withForeignPtr fih (fmap fromCBool . LMF.libremidi_midi_in_is_connected)

inAbsTimestamp :: InHandle -> IO Timestamp
inAbsTimestamp fih = withForeignPtr fih (fmap fromCLong . LMF.libremidi_midi_in_absolute_timestamp)

type OutHandle = ForeignPtr LMF.OutHandle

newOutHandle :: Api -> MidiConfig -> ErrM OutHandle
newOutHandle api mc = do
  fac <- liftIO (mkApiConfig (ApiConfig {acApi = api, acConfigType = ConfigTypeOutput}))
  fmc <- liftIO (mkMidiConfig mc)
  let pac = unsafeForeignPtrToPtr fac
      pmc = unsafeForeignPtrToPtr fmc
  takeM (LMF.libremidi_midi_out_new pmc pac)

freeOutHandle :: OutHandle -> IO ()
freeOutHandle = finalizeForeignPtr

outIsConnected :: OutHandle -> IO Bool
outIsConnected foh = withForeignPtr foh (fmap fromCBool . LMF.libremidi_midi_out_is_connected)

type Msg1 = Ptr LMF.Sym1

outSendMsg1 :: OutHandle -> Msg1 -> Int -> ErrM ()
outSendMsg1 h msg len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (LMF.libremidi_midi_out_send_message p msg (toCSize (fromIntegral len)))

outSchedMsg1 :: OutHandle -> Timestamp -> Msg1 -> Int -> ErrM ()
outSchedMsg1 h ts msg len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (LMF.libremidi_midi_out_schedule_message p (toCLong ts) msg (toCSize (fromIntegral len)))

type Msg2 = Ptr LMF.Sym2

outSendMsg2 :: OutHandle -> Msg2 -> Int -> ErrM ()
outSendMsg2 h msg len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (LMF.libremidi_midi_out_send_ump p msg (toCSize (fromIntegral len)))

outSchedMsg2 :: OutHandle -> Timestamp -> Msg2 -> Int -> ErrM ()
outSchedMsg2 h ts msg len = unRunErrM $
  withForeignPtr h $ \p ->
    checkM (LMF.libremidi_midi_out_schedule_ump p (toCLong ts) msg (toCSize (fromIntegral len)))
