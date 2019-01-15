module Utils where

import Control.Monad.Except
import Data.Char
import Data.Bifunctor
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.String

showT :: Show a => a -> Text
showT = pack . show

-- | Make a parser from a constructor, expecting the @show@
-- representation of it (case-insensitive).
mkSimple :: Show a => a -> Parser a
mkSimple x = try (x <$ string (map toLower (show x)) <* eof)

withError :: (Monad m, MonadError e' m) => (e -> e') -> Either e a -> m a
withError f = liftEither . first f

maybeThrow :: MonadError e m => e -> Maybe a -> m a
maybeThrow e = maybe (throwError e) return
