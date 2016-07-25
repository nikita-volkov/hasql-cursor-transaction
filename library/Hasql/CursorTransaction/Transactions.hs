module Hasql.CursorTransaction.Transactions
(
  cursorTransaction,
)
where

import qualified Hasql.CursorTransaction.Private.Transactions as A
import qualified Hasql.CursorTransaction.Private.CursorTransaction as B

-- |
-- Executes CursorTransaction in Transaction.
cursorTransaction =
  B.run
