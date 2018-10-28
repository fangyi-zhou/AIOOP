module L1.Semantics (eval) where

import           L1.Lookup
import           L1.Runtime
import           L1.Syntax

mkVal :: Value -> Heap -> (Result, Heap)
mkVal v h
  = (Value v, h)

mkStuck :: Heap -> (Result, Heap)
mkStuck
  = mkDev StuckErr

mkNPE :: Heap -> (Result, Heap)
mkNPE
  = mkDev NullPntrExc

mkDev :: Deviation -> Heap -> (Result, Heap)
mkDev d h
  = (Deviation d, h)

eval :: Program -> Expr -> StackFrame -> Heap -> (Result, Heap)
eval p TrueE _ h
  = mkVal TrueV h
eval p FalseE _ h
  = mkVal FalseV h
eval p Null _ h
  = mkVal NullV h
eval p This s h
  = mkVal (Address $ stackThis s) h
eval p X s h
  = mkVal (stackX s) h
eval p (If e e1 e2) s h
  = case eval p e s h of
    (Value TrueV, h1) ->
      eval p e1 s h1
    (Value FalseV, h1) ->
      eval p e2 s h1
    (Deviation d, h1) ->
      mkDev d h1
    (_, h1) ->
      mkStuck h1
  where
    (r1, h1) = eval p e s h
eval p (FieldAccess e f) s h
  = case eval p e s h of
    (Value NullV, h1) ->
      mkNPE h1
    (Value (Address a), h1) ->
      mkVal (heapObjectField h a f) h1
    (Deviation d, h1) ->
      mkDev d h1
    (_, h1) ->
      mkStuck h1
eval p (FieldAssign e f e') s h
  = case eval p e s h of
    (Value (Address a), h1) ->
      case eval p e' s h1 of
        (Value v, h2) ->
          mkVal v (heapFieldUpdate h a f v)
        (Deviation d, h2) ->
          mkDev d h2
    (Value NullV, h1) ->
      mkNPE h1
    (Deviation d, h1) ->
      mkDev d h1
    (_, h1) ->
      mkStuck h1
eval p (New c) s h
  = mkVal (Address a) (heapUpdate h a o)
  where
    a = newAddress h
    o = newObject p c
eval p (MethCall e0 m e1) s h
  = case eval p e0 s h of
    (Value (Address a), h1) ->
      case eval p e1 s h of
        (Value v1, h2) ->
          eval p (methodBody p (heapClass h2 a) m) (a, v1) h2
        (Deviation d, h2) ->
          mkDev d h2
    (Value NullV, h1) ->
      mkNPE h1
    (Deviation d, h1) ->
      mkDev d h1
    (_, h1) ->
      mkStuck h1
