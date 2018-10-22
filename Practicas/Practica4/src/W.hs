{-|
Module      : W
Description : Provides the implementation of functions that transform expressions from MinHs to UMinHs and vice versa.
Maintainer  : pablog@ciencias.unam.mx
-}
module W (erase, w) where

import Syntax.Syntax
import Syntax.Type as T
import Syntax.UMinHs as UMH
import Syntax.MinHs as MH
import Unifier

-- |The context is represented as a set of different declarations of variables with their respective types
type Ctx = [(Identifier, Type)]
-- |Just to take the variable type
type VType = Type

-- |A Judgement is an Assertion (G, e, T) that represents that in the context G, e has type T
data Judgement = Assertion (Ctx, MH.Expr, Type)

instance Show Judgement where
  -- |The 'show' function returns a String representation of a Judgement.
  show (Assertion (ctx, e, t)) = show ctx ++ " |- " ++ show e ++ " : " ++ show t

-- |The 'erase' function transforms a MinHs expression to a UMinHs expression.
erase :: MH.Expr -> UMH.Expr
erase e = case e of
  MH.V x -> UMH.V x
  MH.I n -> UMH.I n
  MH.B b -> UMH.B b
  MH.Add e1 e2 -> UMH.Add (erase e1)(erase e2)
  MH.Mul e1 e2 -> UMH.Mul (erase e1)(erase e2)
  MH.Succ e1 -> UMH.Succ (erase e1)
  MH.Pred e1 -> UMH.Pred (erase e1)
  MH.And e1 e2 -> UMH.And (erase e1)(erase e2)
  MH.Or e1 e2 -> UMH.Or (erase e1)(erase e2)
  MH.Not e1 -> UMH.Not (erase e1)
  MH.Lt e1 e2 -> UMH.Lt (erase e1)(erase e2)
  MH.Gt e1 e2 -> UMH.Gt (erase e1)(erase e2)
  MH.Eq e1 e2 -> UMH.Eq (erase e1)(erase e2)
  MH.If b e1 e2 -> UMH.If (erase b)(erase e1)(erase e2)
  MH.Let x _ e1 e2 -> UMH.Let x(erase e1)(erase e2)
  MH.Fun x _ e1 -> UMH.Fun x (erase e1)
  MH.FunF n x _ _ e1 -> UMH.FunF n x (erase e1)
  MH.App e1 e2 -> UMH.App (erase e1)(erase e2)

-- |The 'erase' function transforms a UMinHs expression to a MinHs expression.
--
-- >>> w $ UMH.Fun "x" $ UMH.Fun "y" $ UMH.V "y"
-- [] |- f((x : T1).f((y : T0).V "y")) : T1 -> (T0 -> T0)
-- >>> w $ UMH.App (UMH.App (UMH.V "x") (UMH.V "y")) (UMH.V "z")
-- [("x",T3 -> (T4 -> T0)),("y",T3),("z",T4)] |- ((V "x" $ V "y") $ V "z") : X0
-- >>> w $ UMH.App (UMH.V "x") (UMH.V "x")
-- *** Exception: Unification Fails.
-- >>> w $ UMH.Fun "s" $ UMH.Fun "z" $ UMH.App (UMH.V "s") (UMH.V "z")
-- [] |- f((s : T2 -> T0).f((z . T2).(V "s" $ V "z"))) : (T2 -> T0) -> (T2 -> T0)
-- >>> w $ UMH.App (UMH.App (UMH.V "x") (UMH.V "z")) (UMH.App (UMH.V "y") (UMH.V "z"))
-- [("x",T6 -> (T4 -> T0)),("z",T6),("y",T6 -> T4),("z",T6)] |- ((V "x" $ V "z") $ (V "y" $ V "z")) : T0
-- >>> w $ UMH.Fun "f" $ UMH.Fun "x" $ UMH.Fun "y" $ UMH.App (UMH.V "f") (UMH.Add (UMH.V "x") (UMH.V "y"))
-- [] |- f((f : Integer -> T0).f((x : Integer).f((y : Integer).(V "f" $ add(V "x", V "y"))))) : (Integer -> T0) -> (Integer -> Integer -> T0)
-- >>> w $ UMH.App (UMH.V "g") (UMH.App (UMH.V "f") (UMH.Mul (UMH.I 3) (UMH.V "z")))
-- [("g",T2 -> T0),("f",Integer -> T2),("z",Integer)] |- (V "g" $ (V "f" $ mul(I 3, V "z"))) : T0
-- >>> w $ UMH.Fun "x" $ UMH.Fun "y" $ UMH.If (UMH.B True) (UMH.App (UMH.V "f") (UMH.V "x")) (UMH.V "y")
-- [("f",T2 -> T3)] |- f((x : X2).f((y : T3).if(B True, (V "f" $ V "x"), V "y"))) : T2 -> (T3 -> T3)
-- >>> w $ UMH.App (UMH.Add (UMH.I 1) (UMH.V "n")) (UMH.V "w")
-- *** Exception: Unification Fails.
-- >>> w $ UMH.Let "x" (UMH.I 2) (UMH.Fun "y" (UMH.Add (UMH.V "x") (UMH.V "y")))
-- [] |- let(I 2, (x : Integer).f((y : Integer).add(V "x", V y))) : Integer -> Integer
w :: UMH.Expr -> Judgement
--w e = error "Not yet implemented."
w e = case e of
  UMH.V x -> Assertion([], MH.V x, newVType [])
  UMH.I n -> Assertion([], MH.I n, Integer)
  UMH.B b -> Assertion([], MH.B b, Boolean)
  UMH.Add (UMH.I n1) (UMH.I n2) -> Assertion([],MH.Add (MH.I n1)(MH.I n2), Integer)
  UMH.Add (UMH.V x) (UMH.V y) -> Assertion([(x, Integer),(y, Integer)], MH.Add (MH.V x) (MH.V y), Integer)
  UMH.Add (UMH.I n) (UMH.V x) -> Assertion([(x, Integer)],MH.Add(MH.I n)(MH.V x),Integer)
  UMH.Add (UMH.V x) (UMH.I n) -> Assertion([(x, Integer)],MH.Add(MH.V x)(MH.I n),Integer)
  -- todo cool, el problema es que pasa si nos pasan Add(Pred n)(Succ n), no se como hacerlo con dos expr

  --UMH.Add e1 e2 -> Assertion([(e1, Integer)], (MH.Add (w e1) (w e2)), newVType [])

-- |The 'newVType' function returns a new variable type that is not contained in the given set.
newVType :: [VType] -> VType
newVType vs = case vs of
  [] -> T 1
  (T x : xs) -> if (T (x + 1)`elem` xs)
                then newVType xs
                else T(x + 1)
  (_:xs) -> newVType xs


