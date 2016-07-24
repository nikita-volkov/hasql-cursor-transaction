module Hasql.Cursor.Transactions
(
  A.cursorQuery,
  cursorSession,
)
where

import qualified Hasql.Cursor.Private.Transactions as A
import qualified Hasql.Cursor.Private.CursorSession as B

-- |
-- Executes CursorSession in Transaction.
cursorSession =
  B.run
