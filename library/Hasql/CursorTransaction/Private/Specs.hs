module Hasql.CursorTransaction.Private.Specs
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B


-- |
-- Spefifies how many rows to fetch in a single DB roundtrip.
newtype BatchSize =
  BatchSize Int64

-- |
-- Batch size of 10.
batchSize_10 :: BatchSize
batchSize_10 =
  BatchSize 10

-- |
-- Batch size of 100.
batchSize_100 :: BatchSize
batchSize_100 =
  BatchSize 100

-- |
-- Batch size of 1000.
batchSize_1000 :: BatchSize
batchSize_1000 =
  BatchSize 1000

-- |
-- Batch size of 10000.
batchSize_10000 :: BatchSize
batchSize_10000 =
  BatchSize 10000


-- |
-- A parameters encoder immediately supplied with parameters.
newtype EncodedParams =
  EncodedParams (Supplied A.Params)
  deriving (Monoid)

-- |
-- Pack the params encoder and params into EncodedParams.
encodedParams :: A.Params params -> params -> EncodedParams
encodedParams encoder params =
  EncodedParams (Supplied encoder params)
