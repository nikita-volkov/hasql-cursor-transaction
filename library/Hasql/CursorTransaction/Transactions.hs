module Hasql.CursorTransaction.Transactions
(
  cursorSession,
)
where

import qualified Hasql.CursorTransaction.Private.Transactions as A
import qualified Hasql.CursorTransaction.Private.CursorTransaction as B

-- |
-- Executes CursorTransaction in Transaction.
cursorSession =
  B.run
