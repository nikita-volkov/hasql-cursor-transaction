module Hasql.CursorTransaction.Private.Transactions where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Specs as B
import qualified Hasql.CursorTransaction.Private.Statements as A
import qualified Hasql.Decoders as E
import qualified Hasql.Encoders as F
import qualified Hasql.Transaction as C

declareCursor :: ByteString -> ByteString -> F.Params params -> params -> C.Transaction ()
declareCursor cursorName template paramsEncoder params =
  C.statement params (A.declareCursor cursorName template paramsEncoder)

closeCursor :: ByteString -> C.Transaction ()
closeCursor cursorName =
  C.statement () (A.closeCursor cursorName)

fetchFromCursor :: ByteString -> B.BatchSize -> E.Result result -> C.Transaction result
fetchFromCursor cursorName batchSize decoder =
  C.statement () (A.fetchFromCursor cursorName batchSize decoder)
