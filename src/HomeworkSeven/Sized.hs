{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module HomeworkSeven.Sized where

newtype Size 
    = Size Int
    deriving (Eq, Ord, Show, Num)

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

instance Sized Int where
    size = Size

instance Sized String where
    size = Size . length

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

getSize :: Size -> Int
getSize (Size i) = i