module Hasql.Cursor.Private.CursorTransaction
where

import Hasql.Cursor.Private.Prelude
import qualified Hasql.Transaction as A
import qualified Hasql.Encoders as D
import qualified Hasql.Decoders as F
import qualified Hasql.Cursor.Private.Queries as B
import qualified Hasql.Cursor.Private.Transactions as C
import qualified Hasql.Cursor.Private.Specs as G
import qualified ByteString.TreeBuilder as E


-- |
-- Context for fetching from multiple cursors in an intertwined fashion.
newtype CursorTransaction s result =
  CursorTransaction (StateT Int A.Transaction result)
  deriving (Functor, Applicative, Monad)

-- |
-- Cursor reference.
newtype Cursor s =
  Cursor ByteString

declareCursor :: ByteString -> D.Params params -> params -> CursorTransaction s (Cursor s)
declareCursor template encoder params =
  CursorTransaction $
  do
    name <- fmap name (state ((,) <$> id <*> succ))
    lift (C.declareCursor name template encoder params)
    return (Cursor name)
  where
    name inc =
      E.toByteString $
      E.byteString "Hasql.Cursor.CursorTransaction." <> E.asciiIntegral inc

closeCursor :: Cursor s -> CursorTransaction s ()
closeCursor (Cursor name) =
  liftTransaction (C.closeCursor name)

-- |
-- Given a template, encoded params and a Cursor-handling continuation,
-- executes it,
-- while automatically declaring and closing the cursor behind the scenes.
withCursor :: ByteString -> G.EncodedParams -> (forall s. Cursor s -> CursorTransaction s result) -> CursorTransaction s result
withCursor template (G.EncodedParams (Supplied encoder params)) continuation =
  do
    cursor <- declareCursor template encoder params
    result <- continuation cursor
    closeCursor cursor
    return result

-- |
-- Fetch from a cursor a batch of the given size and decode it using the specified result decoder.
fetchBatch :: Cursor s -> G.BatchSize -> F.Result result -> CursorTransaction s result
fetchBatch (Cursor name) batchSize decoder =
  liftTransaction $
  A.query (batchSize, name) (B.fetchFromCursor_decoder decoder)

-- |
-- Lift a standard transaction.
-- Note that the transaction must not execute other CursorTransactions.
liftTransaction :: A.Transaction result -> CursorTransaction s result
liftTransaction =
  CursorTransaction . lift

run :: (forall s. CursorTransaction s result) -> A.Transaction result
run (CursorTransaction stack) =
  evalStateT stack 1
