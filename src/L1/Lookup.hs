module L1.Lookup where

import Data.Functor ( (<&>) )
import qualified Data.Map as M

import L1.Syntax
import L1.Runtime

field :: Program -> ClassId -> FieldId -> Type
field p c f
  = fst (p M.! c) M.! f

fields :: Program -> ClassId -> [FieldId]
fields p c
  = M.keys $ fst $ p M.! c

method :: Program -> ClassId -> MethId -> Method
method p c m
  = snd (p M.! c) M.! m

objectField :: Object -> FieldId -> Value
objectField o f
  = snd o M.! f

objectUpdate :: Object -> FieldId -> Value -> Object
objectUpdate o f v
  = o <&> M.insert f v

heapUpdate :: Heap -> Address -> Object -> Heap
heapUpdate h a o
  = M.insert a o h

heapObjectField :: Heap -> Address -> FieldId -> Value
heapObjectField h a f
  = snd (h M.! a) M.! f

heapFieldUpdate :: Heap -> Address -> FieldId -> Value -> Heap
heapFieldUpdate h a f v =
  heapUpdate h a (objectUpdate (h M.! a) f v)

stackThis :: StackFrame -> Address
stackThis = fst

stackX :: StackFrame -> Value
stackX = snd
