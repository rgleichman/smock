{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Readme where

import Apply

--Our function where we want to mock
mathOp_0 :: Integer -> Integer
mathOp_0 = op
  where op = (+2)

t0 = 5 == mathOp_0 3

--Declare a tag
data Ta_a

--This tag mocks functions of type: a -> a
instance TagC Ta_a a a where
  data TagD Ta_a a a = A_a

--mathOp' :: (Wrap w, Num b) => w b -> w b
--Warning: The above type is inferred when there are no non-default App instances in scope
mathOp_1 :: (App Ta_a w b b, Num b) => w b -> w b
mathOp_1 x = apply (wrap op) A_a x
  where op = (+2)

-- Define a wrapper
newtype WrapAlpha a = WrapAlpha a deriving (Show, Eq)

instance Wrap WrapAlpha where
  unwrap (WrapAlpha a) = a
  wrap = WrapAlpha

-- Define an A_a WrapAlpha mock implementation
instance Num a => App Ta_a WrapAlpha a a where
  apply = makeApplyInst (const (*2))

t1 = (WrapDef 5) == mathOp_1 (WrapDef 3)

t2 = (WrapAlpha 6) == mathOp_1 (WrapAlpha 3)

--Let's make a new tag
data Tb_b

--This tag mocks functions of type: a -> a
instance TagC Tb_b b b where
  data TagD Tb_b b b = B_b

--And add a WrapAlpha mock instance for it
instance Num b => App Tb_b WrapAlpha b b where
  apply = makeApplyInst (const (^2))

--A function that uses the B_b tag

twoTags :: (App Tb_b w b b, App Ta_a w b b, Num b) => w b -> (w b, w b)
twoTags x = (apply (wrap op) A_a x, apply (wrap op) B_b x)
  where op = (+2)

t3 = (WrapDef 5, WrapDef 5) == twoTags (WrapDef 3)
t4 = (WrapAlpha 6, WrapAlpha 9) == twoTags (WrapAlpha 3)

-- Let's make a new wrapper

newtype WrapBeta a = WrapBeta a deriving (Show, Eq)

instance Wrap WrapBeta where
  unwrap (WrapBeta a) = a
  wrap = WrapBeta

-- Add a mock Tb_b WrapBeta instance
instance Num b => App Tb_b WrapBeta b b where
  apply = makeApplyInst $ \_ x -> x-5

-- If there is no App instance for a specific tag and wrapper,
-- the original function is used
t5 = (WrapBeta 5, WrapBeta (-2)) == twoTags (WrapBeta 3)

newtype WrapConfused a = WrapConfused a deriving (Show, Eq)

instance Wrap WrapConfused where
  unwrap (WrapConfused a) = a
  wrap = WrapConfused

-- Mocks can use the original function
instance Num a => App Ta_a WrapConfused a a where
  apply = makeApplyInst(\originalFunction ->
                         (*3).originalFunction
                       )

t6 = (WrapConfused 15) == mathOp_1 (WrapConfused 3)

-- Main --
testList :: [(Int, Bool)]
testList = zip ([0,1..] :: [Int]) [t0
                                  ,t1
                                  ,t2
                                  ,t3
                                  ,t4
                                  ,t5
                                  ,t6
                                  ]

main :: IO ()
main = putStrLn $ if all snd testList then
                    "Pass"
                  else
                    "Fail"
