module Hasql.CursorTransaction.Sessions
  ( cursorTransaction,
  )
where

import qualified Hasql.CursorTransaction.Private.CursorTransaction as A
import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Session as C
import qualified Hasql.Transaction.Sessions as D

-- |
-- Executes CursorTransaction in Session.
--
-- During the execution it establishes a Read transaction with the ReadCommitted isolation level.
cursorTransaction :: (forall s. A.CursorTransaction s result) -> C.Session result
cursorTransaction cursorTransaction =
  D.transaction D.ReadCommitted D.Read (A.run cursorTransaction)
