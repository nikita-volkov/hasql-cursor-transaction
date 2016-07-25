module Hasql.CursorTransaction.Private.Queries
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Query as A
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.CursorTransaction.Private.Specs as F
import qualified ByteString.TreeBuilder as D
import qualified Control.Foldl as E


declareCursor :: ByteString -> ByteString -> B.Params a -> A.Query a ()
declareCursor name sql encoder =
  A.statement sql' encoder C.unit False
  where
    sql' =
      D.toByteString $
      "DECLARE " <> D.byteString name <> " NO SCROLL CURSOR FOR " <> D.byteString sql

closeCursor :: A.Query ByteString ()
closeCursor =
  A.statement "CLOSE $1" (B.value B.bytea) C.unit True

fetchFromCursor_decoder :: C.Result result -> A.Query (F.BatchSize, ByteString) result
fetchFromCursor_decoder decoder =
  A.statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (B.value batchSize)
        (B.value B.bytea)
      where
        batchSize =
          contramap batchSizeToInt64 B.int8
          where
            batchSizeToInt64 (F.BatchSize a) =
              a

fetchFromCursor_foldl :: (b -> a -> b) -> b -> C.Row a -> A.Query (F.BatchSize, ByteString) b
fetchFromCursor_foldl step init rowDec =
  fetchFromCursor_decoder (C.foldlRows step init rowDec)

fetchFromCursor_fold :: E.Fold row result -> C.Row row -> A.Query (F.BatchSize, ByteString) result
fetchFromCursor_fold (E.Fold progress enter exit) =
  fmap exit . fetchFromCursor_foldl progress enter
      
