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
erase e = error "Not yet implemented."

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
w e = error "Not yet implemented."

-- |The 'newVType' function returns a new variable type that is not contained in the given set.
newVType :: [VType] -> VType
--newVType vs = error "Not yet implemented."
newVType vs = case vs of
  [] -> T 1
  (T x : xs) -> if (T (x + 1)`elem` xs)
                then newVType xs
                else T(x + 1)
  (_:xs) -> newVType xs


