module Hasql.CursorTransaction.Private.Statements where

import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Specs as F
import qualified Hasql.Decoders as C
import qualified Hasql.Encoders as B
import Hasql.Statement

declareCursor :: ByteString -> ByteString -> B.Params a -> Statement a ()
declareCursor name sql encoder =
  unpreparable sql' encoder C.noResult
  where
    sql' =
      "DECLARE "
        <> decodeUtf8 name
        <> " NO SCROLL CURSOR FOR "
        <> decodeUtf8 sql

closeCursor :: ByteString -> Statement () ()
closeCursor name =
  preparable sql B.noParams C.noResult
  where
    sql =
      "CLOSE " <> decodeUtf8 name 
fetchFromCursor :: ByteString -> F.BatchSize -> C.Result result -> Statement () result
fetchFromCursor name (F.BatchSize batchSize) decoder =
  preparable sql encoder decoder
  where
    sql =
      "FETCH FORWARD "
        <> pack (show batchSize)
        <> " FROM "
        <> decodeUtf8 name
    encoder =
      B.noParams
