module Hasql.CursorTransaction.Transactions
(
  A.cursorQuery,
  cursorSession,
)
where

import qualified Hasql.CursorTransaction.Private.Transactions as A
import qualified Hasql.CursorTransaction.Private.CursorTransaction as B

-- |
-- Executes CursorTransaction in Transaction.
cursorSession =
  B.run
