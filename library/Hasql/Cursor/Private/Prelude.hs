module Hasql.Cursor.Private.Prelude
( 
  module Exports,
  Supplied(..),
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, error)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (shift)
import Control.Monad.Trans.Except as Exports hiding (liftCallCC, liftListen, liftPass)
import Control.Monad.Trans.Maybe as Exports hiding (liftCallCC, liftCatch, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Writer.Strict as Exports hiding (liftCallCC, liftCatch)
import Data.Functor.Identity as Exports

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- contravariant-extras
-------------------------
import Contravariant.Extras as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- Custom
-------------------------

-- |
-- A combination of a divisible functor with some input for it.
-- Allows to use the 'Monoid' API for composition.
data Supplied divisible =
  forall input. Supplied !(divisible input) !input

instance Divisible divisible => Monoid (Supplied divisible) where
  mempty =
    Supplied conquer ()
  mappend (Supplied divisible1 input1) (Supplied divisible2 input2) =
    Supplied divisible3 input3
    where
      divisible3 =
        divide id divisible1 divisible2
      input3 =
        (input1, input2)

