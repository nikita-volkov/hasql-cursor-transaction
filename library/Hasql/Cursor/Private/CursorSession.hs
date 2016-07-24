module Hasql.Cursor.Private.CursorSession
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
newtype CursorSession s result =
  CursorSession (StateT Int A.Transaction result)
  deriving (Functor, Applicative, Monad)

-- |
-- Cursor reference.
newtype Cursor s =
  Cursor ByteString

declareCursor :: ByteString -> D.Params params -> params -> CursorSession s (Cursor s)
declareCursor template encoder params =
  CursorSession $
  do
    name <- fmap name (state ((,) <$> id <*> succ))
    lift (C.declareCursor name template encoder params)
    return (Cursor name)
  where
    name inc =
      E.toByteString $
      E.byteString "Hasql.Cursor.CursorSession." <> E.asciiIntegral inc

closeCursor :: Cursor s -> CursorSession s ()
closeCursor (Cursor name) =
  liftTransaction (C.closeCursor name)

-- |
-- Given a template, a params encoder, params and a Cursor-handling continuation,
-- executes it,
-- while automatically declaring and closing the cursor behind the scenes.
withCursor :: ByteString -> D.Params params -> params -> (forall s. Cursor s -> CursorSession s result) -> CursorSession s result
withCursor template encoder params continuation =
  do
    cursor <- declareCursor template encoder params
    result <- continuation cursor
    closeCursor cursor
    return result

-- |
-- Fetch from a cursor a batch of the given size and decode it using the specified result decoder.
fetchBatch :: Cursor s -> G.BatchSize -> F.Result result -> CursorSession s result
fetchBatch (Cursor name) batchSize decoder =
  liftTransaction $
  A.query (batchSize, name) (B.fetchFromCursor_decoder decoder)

-- |
-- Lift a transaction.
liftTransaction :: A.Transaction result -> CursorSession s result
liftTransaction =
  CursorSession . lift

run :: (forall s. CursorSession s result) -> A.Transaction result
run (CursorSession stack) =
  evalStateT stack 1
