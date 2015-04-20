{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Apply
       (App(..)
       ,Wrap(..)
       ,TagC(..)
       ,TagD(..)
       ,TDef
       ,WrapDef(..)
       ,makeApplyInst
       ,wApply
       ,applyW
       ,wApplyW
       ,wApply2
       ,partial
       ,(@@)
       ,(#@)
       ,(#@#)
       ,(@#)
       ,(&)
       ) where

import ApplyInternal

--If there is no instance for a tag and wrapper, use the default wrapper
--This is declared here since ApplyInternal does not have OverlappingInstances
instance (Wrap w, TagC t a b) => App t w a b where
  apply f _ = apply f ADef
