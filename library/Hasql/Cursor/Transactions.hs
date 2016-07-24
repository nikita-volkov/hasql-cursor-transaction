module Hasql.Cursor.Transactions
(
  A.cursorQuery,
  cursorSession,
)
where

import qualified Hasql.Cursor.Private.Transactions as A
import qualified Hasql.Cursor.Private.CursorTransaction as B

-- |
-- Executes CursorTransaction in Transaction.
cursorSession =
  B.run
