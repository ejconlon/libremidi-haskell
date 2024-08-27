module Libremidi.Common where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT, MonadFree (..), liftF, runFreeT)
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..), CLong (..), CSize (..))
import Foreign.Concurrent qualified as FC
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, newForeignPtr, touchForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

class (Integral a, Enum b, Bounded b) => BitEnum a b | b -> a where
  fromBitEnum :: a -> Maybe b
  toBitEnum :: b -> a

class AssocPtr (fp :: Type) where
  type PtrAssoc fp :: Type
  withAssocPtr :: fp -> (PtrAssoc fp -> IO a) -> IO a
  touchAssocPtr :: fp -> IO ()

instance AssocPtr (ForeignPtr x) where
  type PtrAssoc (ForeignPtr x) = Ptr x
  withAssocPtr = withForeignPtr
  touchAssocPtr = touchForeignPtr

class MallocPtr (p :: Type) where
  mallocPtr :: Proxy p -> IO (ForeignPtr p)

newtype Cb x = Cb {unCb :: ForeignPtr x}
  deriving stock (Eq, Show)

instance AssocPtr (Cb x) where
  type PtrAssoc (Cb x) = FunPtr x
  withAssocPtr (Cb fp) f = withForeignPtr fp (f . castPtrToFunPtr)
  touchAssocPtr = touchForeignPtr . unCb

cbMalloc :: (x -> IO (FunPtr x)) -> x -> IO (Cb x)
cbMalloc g x = do
  y <- g x
  fp <- FC.newForeignPtr (castFunPtrToPtr y) (freeHaskellFunPtr y)
  pure (Cb fp)

newtype Field a b = Field (Ptr a -> Ptr b)

mkField :: Int -> Field a b
mkField i = Field (`plusPtr` i)

pokeField :: (Storable b) => Field a b -> Ptr a -> b -> IO ()
pokeField (Field f) = poke . f

ptrSize :: Int
ptrSize = sizeOf nullPtr

mallocForeignPtrBytes0 :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes0 len = do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp (\p -> fillBytes p 0 len)
  pure fp

allocaPtr :: (Ptr (Ptr x) -> IO a) -> IO a
allocaPtr f = alloca (\p -> poke p nullPtr >> f p)

takePtr :: Ptr (Ptr x) -> IO (ForeignPtr x)
takePtr pptr = do
  ptr <- peek pptr
  fp <- newForeignPtr finalizerFree ptr
  poke pptr nullPtr
  pure fp

newtype Err = Err CInt
  deriving stock (Eq, Ord, Show)

instance Exception Err

newtype ErrM a = ErrM {unErrM :: ExceptT Err IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError Err)

runErrM :: ErrM a -> IO (Either Err a)
runErrM = runExceptT . unErrM

data ForeignF a where
  ForeignFCont :: (forall r. (a -> ErrM r) -> ErrM r) -> ForeignF a

instance Functor ForeignF where
  fmap f = \case
    ForeignFCont k -> ForeignFCont (\g -> k (g . f))

newtype ForeignM a = ForeignM {unForeignM :: ExceptT Err (FreeT ForeignF IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFree ForeignF, MonadError Err, MonadIO)

runForeignM :: ForeignM a -> ErrM a
runForeignM = go . runExceptT . unForeignM
 where
  go n = do
    o <- liftIO (runFreeT n)
    case o of
      Pure ea -> ErrM (ExceptT (pure ea))
      Free f -> case f of
        ForeignFCont k -> k go

errM :: ErrM a -> ForeignM a
errM e = liftIO (runErrM e) >>= either throwError pure

abuseM :: (forall r. (a -> ErrM r) -> ErrM r) -> ForeignM a
abuseM k = liftF (ForeignFCont k)

useM :: (forall r. (a -> IO r) -> IO r) -> ForeignM a
useM k = abuseM (\f -> ErrM (ExceptT (k (runErrM . f))))

scopeM :: ForeignM a -> ForeignM a
scopeM m = liftIO (runErrM (runForeignM m)) >>= either throwError pure

guardM :: IO Err -> ForeignM ()
guardM eact = do
  e@(Err x) <- liftIO eact
  when (x /= 0) (throwError e)

textM :: (Ptr CString -> Ptr CSize -> IO Err) -> ForeignM Text
textM f = scopeM $ do
  sptr <- useM allocaPtr
  lptr <- useM alloca
  guardM (f sptr lptr)
  liftIO $ do
    s <- peek sptr
    l <- peek lptr
    TF.fromPtr (coerce s) (fromIntegral l)

takeM :: (Ptr (Ptr x) -> IO Err) -> ForeignM (ForeignPtr x)
takeM f = scopeM $ do
  ptr <- useM allocaPtr
  guardM (f (coerce ptr))
  liftIO (takePtr ptr)

assocM :: (AssocPtr fp) => fp -> ForeignM (PtrAssoc fp)
assocM fp = useM (withAssocPtr fp)

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
