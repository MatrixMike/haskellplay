
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Advanced_Pattern_Synonyms where

-- data Pair a b = Pair a b

-- pattern EmbedBoxedObj2 :: (a, b) -> Pair ab
-- pattern EmbedBoxedObj2 (a,b) = Pair a b


data Pair a b = Pair a b

pattern T2 :: (a,b) -> Pair a b
pattern T2 t <- ((\(Pair a b) -> (a,b)) -> t) where
  T2 (a,b) = Pair a b

-- data Pair a b = Pair a b

-- toTuple :: Pair a b -> (a,b)
-- toTuple (Pair a b) = (a,b)
-- pattern T2 :: (a,b) -> Pair a b
-- pattern T2 t <- (toTuple -> t) where
--   T2 (a,b) = Pair a b




foo :: Pair a b -> b
foo (T2 (a,b)) = b

-- >>> foo (Pair 'a' 1)
-- 1



