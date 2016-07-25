module Hasql.CursorTransaction.Private.Transactions
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Queries as A
import qualified Hasql.CursorTransaction.Private.Specs as B
import qualified Hasql.Transaction as C
import qualified Hasql.Decoders as E
import qualified Hasql.Encoders as F
import qualified Control.Foldl as D


-- |
-- Fetch and fold the data from cursor until it dries out.
fetchAndFoldCursor :: ByteString -> B.BatchSize -> E.Row row -> D.Fold row result -> C.Transaction result
fetchAndFoldCursor cursorName batchSize rowDecoder (D.Fold progress enter exit) =
  fmap exit $
  fetchAndFoldMore enter
  where
    fetchAndFoldMore batch =
      do
        (null, fetchedBatch) <- fetchBatch
        if null
          then return batch
          else fetchAndFoldMore fetchedBatch
      where
        fetchBatch =
          fetchAndFoldCursorBatch cursorName batchSize rowDecoder fold
          where
            fold =
              (,) <$> D.null <*> D.Fold progress batch id

fetchAndFoldCursorBatch :: ByteString -> B.BatchSize -> E.Row row -> D.Fold row result -> C.Transaction result
fetchAndFoldCursorBatch cursorName batchSize rowDecoder rowsFold =
  C.query (batchSize, cursorName) (A.fetchFromCursor_fold rowsFold rowDecoder)

declareCursor :: ByteString -> ByteString -> F.Params params -> params -> C.Transaction ()
declareCursor cursorName template paramsEncoder params =
  C.query params (A.declareCursor cursorName template paramsEncoder)

closeCursor :: ByteString -> C.Transaction ()
closeCursor cursorName =
  C.query cursorName A.closeCursor

-- |
-- Executes CursorQuery in Transaction provided the parameters.
cursorQuery :: params -> B.CursorQuery params result -> C.Transaction result
cursorQuery params (B.CursorQuery template encoder (B.ReducingDecoder rowDecoder rowsFold) batchSize) =
  declareCursor cursorName template encoder params *>
  fetchAndFoldCursor cursorName batchSize rowDecoder rowsFold <*
  closeCursor cursorName
  where
    cursorName =
      "Hasql.CursorTransaction"
