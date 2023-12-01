module TsBridge.Types.RecordUnion
  ( RecordUnion(..)
  )
  where

import Data.Variant.Encodings.Flat (class IsRecordWithoutKey)
import TsBridge (class TsBridge)
import Type.Proxy (Proxy(..))
import Untagged.Union (type (|+|))

-- | For the rare cases where you need to use a union of records
-- | where each record is guaranteed to not have a specific key.
newtype RecordUnion a = RecordUnion a

derive newtype instance TsBridge a => TsBridge (RecordUnion a)

instance
  ( IsRecordWithoutKey sym a
  , IsRecordWithoutKey sym b
  ) =>
  IsRecordWithoutKey sym (RecordUnion (a |+| b))
  where
  isRecordWithoutKey _ = Proxy