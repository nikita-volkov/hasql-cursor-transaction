module Hasql.CursorTransaction.Private.CursorTransaction where

import qualified ByteString.TreeBuilder as E
import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.Specs as G
import qualified Hasql.CursorTransaction.Private.Transactions as C
import qualified Hasql.Decoders as F
import qualified Hasql.Encoders as D
import qualified Hasql.Transaction as A

-- |
-- Context for fetching from multiple cursors in an intertwined fashion.
newtype CursorTransaction s result
  = CursorTransaction (StateT (Int, A.Transaction ()) A.Transaction result)
  deriving (Functor, Applicative, Monad)

-- |
-- Cursor reference.
newtype Cursor s
  = Cursor ByteString

-- |
-- Given a template and encoded params produces a cursor,
-- while automating its resource management.
declareCursor :: ByteString -> G.EncodedParams -> CursorTransaction s (Cursor s)
declareCursor template (G.EncodedParams (Supplied encoder params)) =
  CursorTransaction
    $ do
      name <- state $ do
        (inc, finaliser) <- id
        name <- return (incToName inc)
        newFinaliser <- return (finaliser *> C.closeCursor name)
        return (name, (succ inc, newFinaliser))
      lift (C.declareCursor name template encoder params)
      return (Cursor name)
  where
    incToName inc =
      E.toByteString
        $ E.byteString "Hasql_CursorTransaction_"
        <> E.asciiIntegral inc

-- |
-- Fetch from a cursor a batch of the given size and decode it using the specified result decoder.
fetchBatch :: Cursor s -> G.BatchSize -> F.Result result -> CursorTransaction s result
fetchBatch (Cursor name) batchSize decoder =
  transaction (C.fetchFromCursor name batchSize decoder)

-- |
-- Lift a standard transaction.
-- Note that the transaction must not execute other CursorTransactions.
transaction :: A.Transaction result -> CursorTransaction s result
transaction =
  CursorTransaction . lift

run :: (forall s. CursorTransaction s result) -> A.Transaction result
run (CursorTransaction stack) =
  do
    (result, (_, finaliser)) <- runStateT stack (1, pure ())
    finaliser
    return result
