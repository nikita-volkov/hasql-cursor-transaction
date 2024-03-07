module Hasql.CursorTransaction.Transactions
  ( cursorTransaction,
  )
where

import qualified Hasql.CursorTransaction.Private.CursorTransaction as B
import qualified Hasql.CursorTransaction.Private.Transactions as A

-- |
-- Executes CursorTransaction in Transaction.
cursorTransaction =
  B.run
