-- |
-- A DSL for declaring the specs.
module Hasql.Cursor.Specs
(
  CursorQuery(..),
  ReducingDecoder(..),
  BatchSize(..),
  EncodedParams,
  encodedParams,
)
where

import Hasql.Cursor.Private.Specs

