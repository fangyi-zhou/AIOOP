module L1.Lookup where

import           Data.Functor ((<&>))
import qualified Data.Map     as M

import           L1.Runtime
import           L1.Syntax

field :: Program -> ClassId -> FieldId -> Type
field p c f
  = fst (p M.! c) M.! f

fields :: Program -> ClassId -> [FieldId]
fields p c
  = M.keys $ fst $ p M.! c

method :: Program -> ClassId -> MethId -> Method
method p c m
  = snd (p M.! c) M.! m

methodBody :: Program -> ClassId -> MethId -> Expr
methodBody p c m
  = methBody $ method p c m

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

heapClass :: Heap -> Address -> ClassId
heapClass h a
  = fst (h M.! a)

heapFieldUpdate :: Heap -> Address -> FieldId -> Value -> Heap
heapFieldUpdate h a f v =
  heapUpdate h a (objectUpdate (h M.! a) f v)

stackThis :: StackFrame -> Address
stackThis = fst

stackX :: StackFrame -> Value
stackX = snd

newAddress :: Heap -> Address
newAddress h
  = 1 + maybe 0 fst (M.lookupMax h)

newFieldValue :: Type -> Value
newFieldValue Bool      = FalseV
newFieldValue (Class _) = NullV

newObject :: Program -> ClassId -> Object
newObject p c
  = (c, M.fromList fieldValues)
  where
    fieldValues = map (\f -> (f, newFieldValue (field p c f))) (fields p c)
