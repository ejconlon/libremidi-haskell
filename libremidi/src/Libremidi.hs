module Libremidi where

import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Libremidi.Common
  ( AssocPtr (..)
  , BitEnum (..)
  , CallbackPtr (..)
  , Err
  , M
  , MallocPtr (..)
  , assocM
  , pokeField
  , runM
  , takeM
  , textM
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

instance BitEnum F.CTimestampMode TimestampMode

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

instance BitEnum F.CApi Api

data ConfigType
  = ConfigTypeObserver
  | ConfigTypeInput
  | ConfigTypeOutput
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CConfigType ConfigType

data Version
  = VersionMidi1
  | VersionMidi2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance BitEnum F.CVersion Version

ipClone :: F.CInPort -> M F.InPort
ipClone ip = do
  fp <- takeM (F.libremidi_midi_in_port_clone ip . coerce)
  pure (F.InPort fp)

ipName :: F.CInPort -> M Text
ipName ip = do
  textM (F.libremidi_midi_in_port_name (coerce ip))

opClone :: F.COutPort -> M F.OutPort
opClone op = do
  fp <- takeM (F.libremidi_midi_out_port_clone op . coerce)
  pure (F.OutPort fp)

opName :: F.COutPort -> M Text
opName op = do
  textM (F.libremidi_midi_out_port_name (coerce op))

obsNew :: F.CObsConfig -> F.CApiConfig -> M F.ObsHandle
obsNew oc ac = do
  fp <- takeM (F.libremidi_midi_observer_new oc ac . coerce)
  pure (F.ObsHandle fp)

data LogLvl = LogLvlWarn | LogLvlErr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data LogEv = LogEv
  { leLvl :: !LogLvl
  , leMsg :: !Text
  }
  deriving stock (Eq, Ord, Show)

data IfaceAct = IfaceActAdd | IfaceActRem
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data IfacePort = IfacePortIn !F.CInPort | IfacePortOut !F.COutPort
  deriving stock (Eq, Show)

data IfaceEv = IfaceEv
  { ieAct :: !IfaceAct
  , iePort :: !IfacePort
  }
  deriving stock (Eq, Show)

data ObsEv
  = ObsEvLog !LogEv
  | ObsEvIface !IfaceEv
  deriving stock (Eq, Show)

type ObsCb = ObsEv -> IO ()

-- type CLogCbFun = Ptr Void -> CString -> CSize -> Ptr Void -> IO ()
mkLogCb :: ObsCb -> LogLvl -> IO F.LogCb
mkLogCb f lvl = callbackPtr $ \_ s l _ -> do
  msg <- TF.fromPtr (coerce s) (fromIntegral l)
  f (ObsEvLog (LogEv lvl msg))

mkInCb :: ObsCb -> IfaceAct -> IO F.InCb
mkInCb f act = callbackPtr (\_ p -> f (ObsEvIface (IfaceEv act (IfacePortIn p))))

mkOutCb :: ObsCb -> IfaceAct -> IO F.OutCb
mkOutCb f act = callbackPtr (\_ p -> f (ObsEvIface (IfaceEv act (IfacePortOut p))))

data ObsConfig = ObsConfig
  { ocOnErr :: !(Maybe F.LogCb)
  , ocOnWarn :: !(Maybe F.LogCb)
  , ocInAdd :: !(Maybe F.InCb)
  , ocInRem :: !(Maybe F.InCb)
  , ocOutAdd :: !(Maybe F.OutCb)
  , ocOutRem :: !(Maybe F.OutCb)
  , ocTrackHardware :: !Bool
  , ocTrackVirtual :: !Bool
  , ocTrackAny :: !Bool
  , ocNotifInCon :: !Bool
  }

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

setObsCb :: ObsCb -> ObsConfig -> IO ObsConfig
setObsCb f oc = do
  onErr <- mkLogCb f LogLvlErr
  onWarn <- mkLogCb f LogLvlWarn
  inAdd <- mkInCb f IfaceActAdd
  inRem <- mkInCb f IfaceActRem
  outAdd <- mkOutCb f IfaceActAdd
  outRem <- mkOutCb f IfaceActRem
  pure $
    oc
      { ocOnErr = Just onErr
      , ocOnWarn = Just onWarn
      , ocInAdd = Just inAdd
      , ocInRem = Just inRem
      , ocOutAdd = Just outAdd
      , ocOutRem = Just outRem
      }

touchObsConfig :: ObsConfig -> IO ()
touchObsConfig oc = do
  traverse_ touchAssocPtr (ocOnErr oc)
  traverse_ touchAssocPtr (ocOnWarn oc)
  traverse_ touchAssocPtr (ocInAdd oc)
  traverse_ touchAssocPtr (ocInRem oc)
  traverse_ touchAssocPtr (ocOutAdd oc)
  traverse_ touchAssocPtr (ocOutRem oc)

withObsConfig :: ObsConfig -> (F.CObsConfig -> M a) -> IO (Either Err a)
withObsConfig oc f = do
  a <- runM $ do
    foc <- liftIO (mallocPtr (Proxy @F.ObsConfig))
    coc <- assocM foc
    -- traverse_ (assocM >=> liftIO . pokeField F.cocOnErrCb coc . coerce) (ocOnErr oc)
    liftIO $ do
      pokeField F.cocTrackHardware coc (toCBool (ocTrackHardware oc))
      pokeField F.cocTrackVirtual coc (toCBool (ocTrackVirtual oc))
      pokeField F.cocTrackAny coc (toCBool (ocTrackAny oc))
      pokeField F.cocNotifInCon coc (toCBool (ocNotifInCon oc))
    -- { ocOnErr = Nothing
    -- , ocOnWarn = Nothing
    -- , ocInAdd = Nothing
    -- , ocInRem = Nothing
    -- , ocOutAdd = Nothing
    -- , ocOutRem = Nothing
    -- , ocTrackHardware = True
    -- , ocTrackVirtual = True
    -- , ocTrackAny = True
    -- , ocNotifyInConstructor = True
    f coc
  liftIO (touchObsConfig oc)
  pure a

-- TODO expose these
-- libremidi_midi_observer_enumerate_input_ports :: CObsHandle -> Ptr Void -> CInCb -> IO CErr
-- libremidi_midi_enumerate_output_ports :: CObsHandle -> Ptr Void -> COutCb -> IO CErr
-- libremidi_midi_in_new :: CMidiConfig -> CApiConfig -> Ptr CInHandle -> IO CErr
-- libremidi_midi_in_is_connected :: CInHandle -> IO CErr
-- libremidi_midi_in_absolute_timestamp :: CInHandle -> IO CTimestamp
-- libremidi_midi_in_free :: CInHandle -> IO CErr
-- libremidi_midi_out_new :: CMidiConfig -> CApiConfig -> Ptr COutHandle -> IO CErr
-- libremidi_midi_out_is_connected :: COutHandle -> IO CErr
-- libremidi_midi_out_send_message :: COutHandle -> CMsg1 -> CSize -> IO CErr
-- libremidi_midi_out_send_ump ::  COutHandle -> CMsg2 -> CSize -> IO CErr
-- libremidi_midi_out_schedule_message :: COutHandle -> CTimestamp -> CMsg1 -> CSize  -> IO CErr
-- libremidi_midi_out_schedule_ump :: COutHandle -> CTimestamp -> CMsg2 -> CSize -> IO CErr
--
-- DONE
-- libremidi_midi_in_port_clone :: CInPort -> Ptr CInPort -> IO CErr
-- libremidi_midi_in_port_name :: CInPort -> Ptr CString -> Ptr CSize -> IO CErr
-- libremidi_midi_out_port_clone :: COutPort -> Ptr COutPort -> IO CErr
-- libremidi_midi_out_port_name :: COutPort -> Ptr CString -> Ptr CSize -> IO CErr
-- libremidi_midi_observer_new :: CObsConfig -> CApiConfig -> Ptr CObsHandle -> IO CErr
