module Hasql.CursorTransaction.Sessions
(
  cursorTransaction,
)
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.CursorTransaction.Private.CursorTransaction as A
import qualified Hasql.Transaction as B
import qualified Hasql.Session as C

-- |
-- Executes CursorTransaction in Session.
-- 
-- During the execution it establishes a Read transaction with the ReadCommitted isolation level.
cursorTransaction :: (forall s. A.CursorTransaction s result) -> C.Session result
cursorTransaction cursorTransaction =
  B.run (A.run cursorTransaction) B.ReadCommitted B.Read
