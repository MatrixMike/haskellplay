{-# LANGUAGE DerivingVia #-}

module Module_1635282489_21129 where


import Numeric ( showHex )

newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show
    via (Hex Int)

-- >>> euroSign
-- 0x20ac

euroSign :: Unicode
euroSign = U 0x20ac
