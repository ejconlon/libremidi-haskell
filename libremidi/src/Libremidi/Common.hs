module Libremidi.Common where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT, MonadFree (..), liftF, runFreeT)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt, CSize)
import Foreign.Concurrent qualified as FC
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, newForeignPtr, touchForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

class (Integral a, Enum b, Bounded b) => BitEnum a b | b -> a where
  fromBitEnum :: a -> b
  fromBitEnum = toEnum . fromIntegral
  toBitEnum :: b -> a
  toBitEnum = fromIntegral . fromEnum

class AssocPtr (fp :: Type) where
  type PtrAssoc fp :: Type
  withAssocPtr :: fp -> (PtrAssoc fp -> IO a) -> IO a
  touchAssocPtr :: fp -> IO ()

instance AssocPtr (ForeignPtr x) where
  type PtrAssoc (ForeignPtr x) = Ptr x
  withAssocPtr = withForeignPtr
  touchAssocPtr = touchForeignPtr

class (AssocPtr fp) => MallocPtr (fp :: Type) where
  mallocPtr :: Proxy fp -> IO fp

class (AssocPtr fp) => CallbackPtr (fp :: Type) where
  type PtrCallback fp :: Type
  callbackPtr :: PtrCallback fp -> IO fp

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

newtype Field a b = Field (Ptr a -> b)

mkField :: Int -> Field a (Ptr b)
mkField i = Field (`plusPtr` i)

mkFunField :: Int -> Field a (FunPtr b)
mkFunField i = Field (\pa -> castPtrToFunPtr (plusPtr pa i))

pokeField :: (Storable b) => Field a (Ptr b) -> Ptr a -> b -> IO ()
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

data F a where
  FCont :: (forall r. (a -> IO r) -> IO r) -> F a

instance Functor F where
  fmap f = \case
    FCont k -> FCont (\g -> k (g . f))

newtype M a = M {unM :: ExceptT Err (FreeT F IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFree F, MonadError Err, MonadIO)

runM :: M a -> IO (Either Err a)
runM (M m) = go (runExceptT m)
 where
  go n = do
    o <- runFreeT n
    case o of
      Pure a -> pure a
      Free fa -> case fa of
        FCont k -> k go

useM :: (forall r. (a -> IO r) -> IO r) -> M a
useM k = liftF (FCont k)

guardM :: IO Err -> M ()
guardM eact = do
  e@(Err x) <- liftIO eact
  when (x /= 0) (throwError e)

textM :: (Ptr CString -> Ptr CSize -> IO Err) -> M Text
textM f = do
  sptr <- useM allocaPtr
  lptr <- useM alloca
  guardM (f sptr lptr)
  liftIO $ do
    s <- peek sptr
    l <- peek lptr
    TF.fromPtr (coerce s) (fromIntegral l)

takeM :: (Ptr (Ptr x) -> IO Err) -> M (ForeignPtr x)
takeM f = do
  ptr <- useM allocaPtr
  guardM (f (coerce ptr))
  liftIO (takePtr ptr)

assocM :: (AssocPtr fp) => fp -> M (PtrAssoc fp)
assocM fp = useM (withAssocPtr fp)

toCBool :: Bool -> CBool
toCBool = \case
  False -> CBool 0
  True -> CBool 1
