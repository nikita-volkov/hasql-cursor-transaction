module Hasql.CursorTransaction.Private.Statements where

import qualified Data.Text as Text
import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Specs as F
import qualified Hasql.Decoders as C
import qualified Hasql.Encoders as B
import Hasql.Statement

declareCursor :: Text -> Text -> B.Params a -> Statement a ()
declareCursor name sql encoder =
  unpreparable sql' encoder C.noResult
  where
    sql' =
      "DECLARE "
        <> name
        <> " NO SCROLL CURSOR FOR "
        <> sql

closeCursor :: Text -> Statement () ()
closeCursor name =
  preparable sql B.noParams C.noResult
  where
    sql =
      "CLOSE " <> name

fetchFromCursor :: Text -> F.BatchSize -> C.Result result -> Statement () result
fetchFromCursor name (F.BatchSize batchSize) decoder =
  preparable sql encoder decoder
  where
    sql =
      "FETCH FORWARD "
        <> Text.pack (show batchSize)
        <> " FROM "
        <> name
    encoder =
      B.noParams
