module Utils (stringtoTitle, toTitle) where

import Data.Text (Text)
import qualified Data.Text as T


stringtoTitle :: String -> Text
stringtoTitle = toTitle . T.pack

toTitle :: Text -> Text
toTitle = T.toTitle
