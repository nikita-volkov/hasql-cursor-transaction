module Hasql.CursorTransaction.Private.Transactions
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Queries as A
import qualified Hasql.CursorTransaction.Private.Specs as B
import qualified Hasql.Transaction as C
import qualified Hasql.Decoders as E
import qualified Hasql.Encoders as F


declareCursor :: ByteString -> ByteString -> F.Params params -> params -> C.Transaction ()
declareCursor cursorName template paramsEncoder params =
  C.query params (A.declareCursor cursorName template paramsEncoder)

closeCursor :: ByteString -> C.Transaction ()
closeCursor cursorName =
  C.query () (A.closeCursor cursorName)

fetchFromCursor :: ByteString -> B.BatchSize -> E.Result result -> C.Transaction result
fetchFromCursor cursorName batchSize decoder =
  C.query () (A.fetchFromCursor cursorName batchSize decoder)
