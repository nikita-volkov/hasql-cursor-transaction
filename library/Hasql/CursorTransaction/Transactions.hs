module Hasql.CursorTransaction.Transactions
  ( cursorTransaction,
  )
where

import qualified Hasql.CursorTransaction.Private.CursorTransaction as CursorTransaction
import qualified Hasql.Transaction as Transaction

-- |
-- Executes CursorTransaction in Transaction.
cursorTransaction :: (forall s. CursorTransaction.CursorTransaction s result) -> Transaction.Transaction result
cursorTransaction =
  CursorTransaction.run
