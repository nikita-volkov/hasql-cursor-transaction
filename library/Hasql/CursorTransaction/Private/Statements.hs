module Hasql.CursorTransaction.Private.Statements where

import qualified ByteString.TreeBuilder as D
import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Specs as F
import qualified Hasql.Decoders as C
import qualified Hasql.Encoders as B
import Hasql.Statement

declareCursor :: ByteString -> ByteString -> B.Params a -> Statement a ()
declareCursor name sql encoder =
  Statement sql' encoder C.noResult False
  where
    sql' =
      D.toByteString
        $ "DECLARE "
        <> D.byteString name
        <> " NO SCROLL CURSOR FOR "
        <> D.byteString sql

closeCursor :: ByteString -> Statement () ()
closeCursor name =
  Statement sql B.noParams C.noResult True
  where
    sql =
      "CLOSE " <> name

fetchFromCursor :: ByteString -> F.BatchSize -> C.Result result -> Statement () result
fetchFromCursor name (F.BatchSize batchSize) decoder =
  Statement sql encoder decoder True
  where
    sql =
      D.toByteString
        $ "FETCH FORWARD "
        <> D.asciiIntegral batchSize
        <> " FROM "
        <> D.byteString name
    encoder =
      B.noParams
