module L1.Runtime where

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import L1.Syntax

type Address = Int

type StackFrame = (Address, Value)

type Heap = M.Map Address Object

data Value
  = TrueV
  | FalseV
  | NullV
  | Address Address
  deriving Eq

type Object = (ClassId, M.Map FieldId Value)

data Deviation
  = NullPntrExc
  | StuckErr
  deriving Eq

data Result
  = Deviation Deviation
  | Value Value
  deriving Eq
