{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MockTests where

import MockWrappers
import MockTags
import Apply

--Method and WrapMath
data Method = Negate | Square | PowerOfTwo

instance TagC TMet_aa Method (a->a) where
  data TagD TMet_aa Method (a->a) = TMet_aa

instance Num a => App TMet_aa WrapMath Method (a -> a) where
  apply = makeApplyInst
          (\f m -> case m of
                    Negate -> (\x -> f Square x * (f Negate x + 1))
                    _ -> f m)

mathObject :: Method -> Integer -> Integer
mathObject Negate x = -x
mathObject Square x = x^(2::Integer)
mathObject PowerOfTwo x = 2^(x::Integer)

-- TESTS --

testMockObject :: (App TAa_a w Integer Integer, App TMet_aa w Method (Integer -> Integer)) => w Integer -> String
testMockObject x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> wApply2 TMet_aa Aa_a mathObject (wrap method) x) methods
    showX = unwrap $ show #@ ADef & x
    methods = [Negate, Square, PowerOfTwo]

mockTwice :: (App TAa_a w Integer Integer, App TMet_aa w Method (Integer -> Integer)) => w Integer -> String
mockTwice x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap func methods
    child1 = partial $ wApply mathObject TMet_aa
    func method = child1 @# TMet_aa & method @@ Aa_a & x
    showX = unwrap $ show #@ ADef & x
    methods = [Negate, Square, PowerOfTwo]

mockIncrement :: (App TAa_a w t t, Show t, Num t) => w t -> (String, t)
mockIncrement x = (unwrap (wApply show ADef x), unwrap $ wApply (+1) Aa_a x)

mockIncrementTwoTags :: (App TAa_a w b b, App TAb_b w b b, Num b) => w b -> (w b, w b)
mockIncrementTwoTags x = (wApply (+1) Aa_a x, wApply (+1) Ab_b x)

testValues :: [String]
testValues = [
  "testMockObject:"
  ,"(WrapDef 3)"
  ,show $ testMockObject (WrapDef 3)
  ,"(WrapMath 3)"
  ,show $ testMockObject (WrapMath 3)
  ,"(WrapRepeat 3)"
  ,show $ testMockObject (WrapRepeat 3)
  ,"testTwoTags"
  ,"mockIncrementTwoTags (WrapRepeat 3)"
  ,show $ mockIncrementTwoTags (WrapRepeat 3)
  ,"mockIncrementTwoTags (WrapRepeat' 3)"
  ,show $ mockIncrementTwoTags (WrapRepeat' 3)
   ]

main :: IO ()
main = mapM_ putStrLn testValues

-- ERRORS --

-- These should be compiler errors

-- This should be an error since the tag type does not match the type of the
-- mocked function
--badMock x = unwrap (wApply show TMet_aa x)

-- This should also be an error, since there is no instance (TagC String Int)
--instance App TAa_a WrapRepeat String Int where
--  apply (WrapRepeat f) _ (WrapRepeat a) = WrapRepeat $ f a
