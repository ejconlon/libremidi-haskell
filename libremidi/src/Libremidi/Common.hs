module Libremidi.Common where

import Control.Concurrent.STM (STM, atomically, retry, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (Exception, finally, mask, mask_, throwIO)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..), CLong (..), CSize (..))
import Foreign.Concurrent qualified as FC
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, mallocForeignPtrBytes, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

mallocForeignPtrBytes0 :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes0 len = do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp (\p -> fillBytes p 0 len)
  pure fp

allocaPtr :: (Ptr (Ptr x) -> IO a) -> IO a
allocaPtr f = alloca (\p -> poke p nullPtr >> f p)

ptrSize :: Int
ptrSize = sizeOf nullPtr

-- | A typed "accessor"
newtype Field a b = Field (Ptr a -> Ptr b)

newField :: Int -> Field a b
newField i = Field (`plusPtr` i)

pokeField :: (Storable b) => Field a b -> Ptr a -> b -> IO ()
pokeField (Field f) = poke . f

-- | Maps Haskell enums to C enums
class (Integral a, Enum b, Bounded b) => BitEnum a b | b -> a where
  fromBitEnum :: a -> Maybe b
  toBitEnum :: b -> a

-- | Generalizes ForeignPtr
class AssocPtr (z :: Type) where
  type PtrAssoc z :: Type
  withAssocPtr :: z -> (PtrAssoc z -> IO a) -> IO a

instance AssocPtr (ForeignPtr x) where
  type PtrAssoc (ForeignPtr x) = Ptr x
  withAssocPtr = withForeignPtr

data Ref p
  = RefUnlock !(ForeignPtr p)
  | RefLock
  | RefFree
  deriving stock (Eq, Ord, Show)

newtype UniquePtr p = UniquePtr
  { unUniquePtr :: TVar (Ref p)
  }
  deriving stock (Eq)

instance AssocPtr (UniquePtr x) where
  type PtrAssoc (UniquePtr x) = Ptr x
  withAssocPtr = withUniquePtr

data FreeErr = FreeErr
  deriving stock (Eq, Ord, Show)

instance Exception FreeErr

newUniquePtr :: ForeignPtr p -> IO (UniquePtr p)
newUniquePtr = fmap UniquePtr . newTVarIO . RefUnlock

aliveUniquePtr :: UniquePtr p -> STM Bool
aliveUniquePtr (UniquePtr v) = fmap isAlive (readTVar v)
 where
  isAlive = \case
    RefFree -> False
    _ -> True

freeUniquePtr :: UniquePtr p -> IO ()
freeUniquePtr (UniquePtr v) = mask_ $ do
  mfp <- atomically $ do
    r <- readTVar v
    case r of
      RefUnlock fp -> do
        writeTVar v RefFree
        pure (Just fp)
      RefLock -> retry
      RefFree -> pure Nothing
  traverse_ finalizeForeignPtr mfp

withUniquePtr' :: UniquePtr p -> (ForeignPtr p -> IO a) -> IO a
withUniquePtr' (UniquePtr v) f = mask $ \restore -> do
  fp <- atomically $ do
    r <- readTVar v
    case r of
      RefUnlock fp -> do
        writeTVar v RefLock
        pure fp
      RefLock -> retry
      RefFree -> throwSTM FreeErr
  finally
    (restore (f fp))
    (atomically (writeTVar v (RefUnlock fp)))

withUniquePtr :: UniquePtr p -> (Ptr p -> IO a) -> IO a
withUniquePtr u f = withUniquePtr' u (`withForeignPtr` f)

consumeUniquePtr :: UniquePtr p -> IO (ForeignPtr p)
consumeUniquePtr (UniquePtr v) = atomically $ do
  r <- readTVar v
  case r of
    RefUnlock fp -> do
      writeTVar v RefFree
      pure fp
    RefLock -> retry
    RefFree -> throwSTM FreeErr

-- | Like a ForeignFunPtr
-- We need to free the function pointer on finalization to not leak it
newtype Cb x = Cb {unCb :: UniquePtr x}
  deriving stock (Eq)

instance AssocPtr (Cb x) where
  type PtrAssoc (Cb x) = FunPtr x
  withAssocPtr = withCb

-- | Given an FFI wrapper function, allocate a callback
newCb :: (x -> IO (FunPtr x)) -> x -> IO (Cb x)
newCb w x = do
  y <- w x
  fp <- FC.newForeignPtr (castFunPtrToPtr y) (freeHaskellFunPtr y)
  up <- newUniquePtr fp
  pure (Cb up)

freeCb :: Cb x -> IO ()
freeCb = freeUniquePtr . unCb

withCb :: Cb x -> (FunPtr x -> IO a) -> IO a
withCb (Cb up) f = withUniquePtr up (f . castPtrToFunPtr)

consumeCb :: Cb x -> IO (ForeignPtr x)
consumeCb = consumeUniquePtr . unCb

-- | Foreign types that have a meaningful initial state
class MallocPtr (p :: Type) where
  mallocPtr :: Proxy p -> IO (ForeignPtr p)

-- | Error code returned by API calls
newtype Err = Err CInt
  deriving stock (Eq, Ord, Show)

instance Exception Err

-- | Computations that short-circuit on error
newtype ErrM a = ErrM {unErrM :: ExceptT Err IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError Err)

runErrM :: ErrM a -> IO (Either Err a)
runErrM = runExceptT . unErrM

unRunErrM :: IO (Either Err a) -> ErrM a
unRunErrM = ErrM . ExceptT

rethrowErrM :: ErrM a -> IO a
rethrowErrM m = runErrM m >>= either throwIO pure

assocM :: (AssocPtr x) => (PtrAssoc x -> ErrM y) -> x -> ErrM y
assocM f fp = unRunErrM (withAssocPtr fp (runErrM . f))

checkM :: IO Err -> IO (Either Err ())
checkM one = do
  e@(Err x) <- liftIO one
  pure (if x == 0 then Right () else Left e)

checkAndThenM :: IO Err -> IO a -> IO (Either Err a)
checkAndThenM one two = do
  e@(Err x) <- liftIO one
  if x == 0
    then fmap Right two
    else pure (Left e)

textM :: (Ptr CString -> Ptr CSize -> IO Err) -> ErrM Text
textM f = unRunErrM $
  allocaPtr $ \sptr -> do
    alloca $ \lptr -> do
      checkAndThenM (f sptr lptr) $ do
        s <- peek sptr
        l <- peek lptr
        TF.fromPtr (coerce s) (fromIntegral l)

takeM :: (Ptr (Ptr x) -> IO Err) -> ErrM (ForeignPtr x)
takeM f = unRunErrM $
  allocaPtr $ \pptr -> do
    checkAndThenM (f pptr) $ do
      ptr <- peek pptr
      fp <- newForeignPtr finalizerFree ptr
      poke pptr nullPtr
      pure fp

toCBool :: Bool -> CBool
toCBool = \case
  False -> CBool 0
  True -> CBool 1

fromCBool :: CBool -> Bool
fromCBool = \case
  0 -> False
  _ -> True

fromCLong :: CLong -> Int64
fromCLong (CLong x) = x

toCLong :: Int64 -> CLong
toCLong = CLong

fromCInt :: CInt -> Int32
fromCInt (CInt x) = x

toCInt :: Int32 -> CInt
toCInt = CInt

fromCSize :: CSize -> Word64
fromCSize (CSize x) = x

toCSize :: Word64 -> CSize
toCSize = CSize
