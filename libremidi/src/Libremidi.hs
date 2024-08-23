module Libremidi where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.Ptr (Ptr, nullPtr)
import Libremidi.Common
  ( BitEnum (..)
  , CallbackPtr (..)
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

data LogEv = LogEv
  { leLvl :: !LogLvl
  , leMsg :: !Text
  }
  deriving stock (Eq, Ord, Show)

type LogCb = LogEv -> IO ()

mkLogCb :: LogCb -> LogLvl -> IO F.LogCb
mkLogCb f lvl = callbackPtr $ \_ s l _ -> do
  msg <- TF.fromPtr (coerce s) (fromIntegral l)
  f (LogEv lvl msg)

data IfaceAct = IfaceActAdd | IfaceActRem
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data IfacePort = IfacePortIn !(Ptr F.InPortX) | IfacePortOut !(Ptr F.OutPortX)
  deriving stock (Eq, Show)

data IfaceEv = IfaceEv
  { ieAct :: !IfaceAct
  , iePort :: !IfacePort
  }
  deriving stock (Eq, Show)

type IfaceCb = IfaceEv -> IO ()

mkIfaceInCb :: IfaceCb -> IfaceAct -> IO F.InCb
mkIfaceInCb f act = callbackPtr (\_ p -> f (IfaceEv act (IfacePortIn p)))

mkIfaceOutCb :: IfaceCb -> IfaceAct -> IO F.OutCb
mkIfaceOutCb f act = callbackPtr (\_ p -> f (IfaceEv act (IfacePortOut p)))

type EnumInCb = Ptr F.InPortX -> IO ()

mkEnumInCb :: EnumInCb -> IO F.InCb
mkEnumInCb f = callbackPtr (\_ p -> f p)

type EnumOutCb = Ptr F.OutPortX -> IO ()

mkEnumOutCb :: EnumOutCb -> IO F.OutCb
mkEnumOutCb f = callbackPtr (\_ p -> f p)

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

setLogCb :: LogCb -> ObsConfig -> IO ObsConfig
setLogCb f oc = do
  onErr <- mkLogCb f LogLvlErr
  onWarn <- mkLogCb f LogLvlWarn
  pure $
    oc
      { ocOnErr = Just onErr
      , ocOnWarn = Just onWarn
      }

setIfaceCb :: IfaceCb -> ObsConfig -> IO ObsConfig
setIfaceCb f oc = do
  inAdd <- mkIfaceInCb f IfaceActAdd
  inRem <- mkIfaceInCb f IfaceActRem
  outAdd <- mkIfaceOutCb f IfaceActAdd
  outRem <- mkIfaceOutCb f IfaceActRem
  pure $
    oc
      { ocInAdd = Just inAdd
      , ocInRem = Just inRem
      , ocOutAdd = Just outAdd
      , ocOutRem = Just outRem
      }

-- touchObsConfig :: ObsConfig -> IO ()
-- touchObsConfig oc = do
--   traverse_ touchAssocPtr (ocOnErr oc)
--   traverse_ touchAssocPtr (ocOnWarn oc)
--   traverse_ touchAssocPtr (ocInAdd oc)
--   traverse_ touchAssocPtr (ocInRem oc)
--   traverse_ touchAssocPtr (ocOutAdd oc)
--   traverse_ touchAssocPtr (ocOutRem oc)

mkObsConfig :: ObsConfig -> ForeignM F.ObsConfig
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

listInPorts :: F.ObsHandle -> EnumInCb -> ForeignM ()
listInPorts (F.ObsHandle foh) f = do
  enumIn <- liftIO (mkEnumInCb f)
  poh <- assocM foh
  fun <- assocM enumIn
  guardM (F.libremidi_midi_observer_enumerate_input_ports poh nullPtr fun)

listOutPorts :: F.ObsHandle -> EnumOutCb -> ForeignM ()
listOutPorts (F.ObsHandle foh) f = do
  enumOut <- liftIO (mkEnumOutCb f)
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
-- TODO
-- libremidi_midi_enumerate_output_ports :: Ptr ObsHandleX -> Ptr Void -> FunPtr OutFun -> IO Err
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
