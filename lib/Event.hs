{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Event where

class Event e m where
  handle :: e -> m ()

-- | EventTy is a wrapper around any datatype that supports
-- Event. It's needed because GHC doesn't support impredicative
-- polymorphism. This is equivalent to:
-- @data EventBuffer m = EventBuffer [forall e. Event e m => e]@
data EventTy m = forall e. Event e m => EventTy e
data EventBuffer m = EventBuffer [EventTy m]

instance Event (EventTy m) m where
  handle (EventTy e) = handle e

class NatTransform f g where
  nt :: forall a. f a -> g a


data EventToBeHandled m = EventToBeHandled (m ())

instance Event (EventToBeHandled m) m where
  handle (EventToBeHandled ma) = ma

transformEvent :: forall m m'. (forall a. m a -> m' a) -> EventTy m -> EventTy m'
transformEvent nt (EventTy e) = EventTy $ EventToBeHandled (nt $ handle e)
