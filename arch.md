- Event-driven
- Events are either a datatype, or type class with `handle` method
- Handler can have state; provide lens from whole game state (allows both local and global state)
- `runSubStateT :: (MonadState s m, MonadState t m') => Lens' s t -> m a -> m' a`
- Writer monad for text output and sending new events
- Don't know about time delays