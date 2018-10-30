module L2.Runtime where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import           L2.Syntax

type Address = Int

type StackFrame = (Address, Value)

type Heap = M.Map Address Object

data Value
  = TrueV
  | FalseV
  | NullV
  | Address Address

type Object = (ClassId, M.Map FieldId Value)

data Deviation
  = NullPntrExc
  | StuckErr

type Result = Either Deviation Value
