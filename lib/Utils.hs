module Utils where

import Data.Char
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.String

showT :: Show a => a -> Text
showT = pack . show

-- | Make a parser from a constructor, expecting the @show@
-- representation of it (case-insensitive).
mkSimple :: Show a => a -> Parser a
mkSimple x = try (x <$ string (map toLower (show x)) <* eof)
