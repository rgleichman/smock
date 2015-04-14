{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MockWrappers where

import Apply
import MockTags

--WrapDef
newtype WrapDef a = WrapDef a deriving (Show)

instance Wrap WrapDef where
  unwrap (WrapDef a) = a
  wrap = WrapDef

-- WrapMath
newtype WrapMath a = WrapMath a deriving (Show)

instance Wrap WrapMath where
  unwrap (WrapMath a) = a
  wrap = WrapMath

--WrapRepeat
newtype WrapRepeat a = WrapRepeat a deriving (Show)

instance Wrap WrapRepeat where
  wrap = WrapRepeat
  unwrap (WrapRepeat a) = a

instance App TAa_a WrapRepeat a a where
  apply = makeApplyInst (\f -> f.f)

instance App TAb_b WrapRepeat a a where
  apply = makeApplyInst (\f -> f.f.f)

--WrapRepeat'
newtype WrapRepeat' a = WrapRepeat' a deriving (Show)

instance Wrap WrapRepeat' where
  wrap = WrapRepeat'
  unwrap (WrapRepeat' a) = a

instance App TAa_a WrapRepeat' a a where
  apply = makeApplyInst (\f -> f.f.f.f)
