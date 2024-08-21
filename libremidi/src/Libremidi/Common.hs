module Libremidi.Common where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Free (FreeF (..), FreeT, MonadFree (..), liftF, runFreeT)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Concurrent qualified as FC
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes, finalizerFree)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, poke)

class (Integral a, Enum b, Bounded b) => BitEnum a b | b -> a where
  fromBitEnum :: a -> b
  fromBitEnum = toEnum . fromIntegral
  toBitEnum :: b -> a
  toBitEnum = fromIntegral . fromEnum

class AssocPtr fp p | fp -> p, fp -> p where
  withAssocPtr :: fp -> (p -> IO a) -> IO a

instance AssocPtr (ForeignPtr x) (Ptr x) where
  withAssocPtr = withForeignPtr

type Field a b = a -> Ptr b

ptrSize :: Int
ptrSize = 8 -- hope you're on a 64-bit machine!

mallocForeignPtrBytes0 :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes0 len = do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp (\p -> fillBytes p 0 len)
  pure fp

allocaPtr :: (Ptr x -> IO a) -> IO a
allocaPtr f = allocaBytes ptrSize (\p -> fillBytes p 0 ptrSize >> f p)

newtype Cb x = Cb {unCb :: ForeignPtr x}

cbWithPtr :: Cb x -> (FunPtr x -> IO a) -> IO a
cbWithPtr (Cb fp) f = withForeignPtr fp (f . castPtrToFunPtr)

cbMalloc :: (x -> IO (FunPtr x)) -> x -> IO (Cb x)
cbMalloc g x = do
  y <- g x
  fp <- FC.newForeignPtr (castFunPtrToPtr y) (freeHaskellFunPtr y)
  pure (Cb fp)

takePtr :: Ptr (Ptr x) -> IO (ForeignPtr x)
takePtr pptr = do
  ptr <- peek pptr
  fp <- newForeignPtr finalizerFree ptr
  poke pptr nullPtr
  pure fp

type CErr = CInt

newtype Err = Err CErr
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

guardM :: IO CErr -> M ()
guardM eact = do
  e <- liftIO eact
  when (e /= 0) (throwError (Err e))

textM :: (Ptr CString -> Ptr CSize -> IO CErr) -> M Text
textM f = do
  sptr <- useM allocaPtr
  lptr <- useM allocaPtr
  guardM (f sptr lptr)
  liftIO $ do
    s <- peek sptr
    l <- peek lptr
    TF.fromPtr (coerce s) (fromIntegral l)

takeM :: (Ptr (Ptr x) -> IO CErr) -> M (ForeignPtr x)
takeM f = do
  ptr <- useM allocaPtr
  guardM (f (coerce ptr))
  liftIO (takePtr ptr)

assocM :: (AssocPtr fp p) => fp -> M p
assocM fp = useM (withAssocPtr fp)
