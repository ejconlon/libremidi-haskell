module Libremidi.Common where

class (Integral a, Enum b, Bounded b) => BitEnum a b | b -> a where
  fromBitEnum :: a -> b
  fromBitEnum = toEnum . fromIntegral
  toBitEnum :: b -> a
  toBitEnum = fromIntegral . fromEnum
