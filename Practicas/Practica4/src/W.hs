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
  (_:xs) -> newVType xs{-|
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
import Data.List (union)

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
w e = let (Assertion (ctx,e',t),_) = algW e [] in Assertion (deleteDuplicate ctx, e',t) where
                                                            deleteDuplicate [] = []
                                                            deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)

algW :: UMH.Expr ->[VType]->(Judgement,[VType])
algW (UMH.I i) vars = (Assertion ([], MH.I i, Integer), vars)
algW (UMH.B b) vars = (Assertion ([], MH.B b, Boolean), vars)
algW (UMH.V x) c = let v = newVType c in (Assertion ([(x,v)], MH.V x, v),v:c)
algW (UMH.Add e1 e2) vars = if umg == []
                            then error "Unification Fails."
                            else (Assertion (ctx', MH.Add e1' e2', Integer), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg = µ((buscaSimilares ctx1 ctx2) ++ [(t1, Integer), (t2, Integer)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Mul e1 e2) vars =if umg == []

                           then error "Unification Fails."
                           else (Assertion (ctx', MH.Mul e1' e2', Integer), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Integer), (t2, Integer)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Succ e) vars = if umg == []
                         then error "Unification Fails."
                         else (Assertion(ctx', MH.Succ e1, Integer), vars1) where
  (Assertion(ctx, e1, t1), vars1) = algW e vars
  umg = µ[(t1, Integer)]
  e1' = subst e1' (head umg)
  ctx' = map(\(p1, p2) -> (p1, subst p2 (head umg))) ctx
algW (UMH.Pred e) vars = if umg == []
                         then error "Unification Fails."
                         else (Assertion(ctx', MH.Pred e1, Integer), vars1) where
  (Assertion(ctx1, e1, t1), vars1) = algW e vars
  umg = µ[(t1, Integer)]
  e1' = subst e1' (head umg)
  ctx' = map(\(p1, p2) -> (p1, subst p2 (head umg))) ctx1
algW (UMH.And e1 e2) vars = if umg == []
                            then error "Unification Fails."
                            else (Assertion (ctx', MH.And e1' e2', Boolean), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Boolean), (t2, Boolean)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Or e1 e2) vars = if umg == []
                           then error "Unification Fails."
                           else (Assertion (ctx', MH.Or e1' e2', Boolean), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Boolean), (t2, Boolean)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Not e) vars = if umg == []
                        then error "Unification Fails."
                        else (Assertion(ctx', MH.Not e1, Boolean), vars1) where
  (Assertion(ctx1, e1, t), vars1) = algW e vars
  umg = µ[(t,Boolean)]
  e1' = subst e1' (head umg)
  ctx' = map(\(p1, p2) -> (p1, subst p2 (head umg))) ctx1
algW (UMH.Lt e1 e2) vars = if umg == []
                           then error "Unification Fails."
                           else (Assertion (ctx', MH.Lt e1' e2', Boolean), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Integer), (t2, Integer)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Gt e1 e2) vars = if umg == []
                           then error "Unification Fails."
                           else (Assertion (ctx', MH.Gt e1' e2', Boolean), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Integer), (t2, Integer)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.Eq e1 e2) vars = if umg == []
                           then error "Unification Fails."
                           else (Assertion (ctx', MH.Eq e1' e2', Boolean), vars2) where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  umg =  µ((buscaSimilares ctx1 ctx2) ++ [(t1, Integer), (t2, Integer)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
algW (UMH.If e1 e2 e3) vars = if umg == []
                              then error "Unification Fails."
                              else (Assertion (ctx', MH.If e1' e2' e3', t'), vars') where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  (Assertion (ctx3, ex3, t3), vars3) = algW e3 vars2
  similaresI = union (buscaSimilares ctx1 ctx2) (buscaSimilares ctx1 ctx3)
  similaresF = union similaresI (buscaSimilares ctx2 ctx3)
  umg = µ (similaresF ++ [(t1, Boolean), (t2, t3)])
  ctx' = union (union (sustCtx ctx1 umg) (sustCtx ctx2 umg)) (sustCtx ctx3 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  e3' = sustLT ex3 umg
  t' = sustTT t2 umg
  vars' = vars3
algW (UMH.Let x e1 e2) vars = if umg == []
                              then error "Unification Fails."
                              else (Assertion (ctx', (MH.Let x t' e1' e2'), t'), vars2) where
  (Assertion(ctx1, ex2, t1), vars1) = algW e2 vars
  (Assertion(ctx2, ex1, t2), vars2) = algW e1 vars1
  umg = µ[(t1, t2)]
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  t' = sustTT t2 umg

algW (UMH.Fun x e) c = if elem x vars then (Assertion (g', MH.Fun x r ea, r :-> t), v) else (Assertion (g, MH.Fun x nx ea, nx :-> t), nx:v) where
    (Assertion (g, ea, t), v) = algW e c
    vars = map (\(p, _) -> p) g
    nx = newVType v
    (_, r) = head $ filter (\p -> fst p == x) g
    g' = filter (\p -> p /= (x, r)) g

algW (UMH.App e1 e2) vars = if umg == []
                            then error "Unification Fails."
                            else (Assertion (ctx', MH.App e1' e2', t'), vars') where
  (Assertion (ctx1, ex1, t1), vars1) = algW e1 vars
  (Assertion (ctx2, ex2, t2), vars2) = algW e2 vars1
  nT = newVType vars2
  umg = µ((buscaSimilares ctx1 ctx2) ++ [(t1, t2 :-> nT)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  t' = sustTT nT umg
  vars' = union vars2 [nT]

algW (UMH.FunF f x e) c = if elem x vars then (Assertion (g', MH.FunF f x (r :-> t) r ea, t), v) else (Assertion (g', MH.FunF f x (nx :-> t) nx ea, t), nx:v) where
    (Assertion (g, ea, t), v) = algW e c
    vars = map (\(p, _) -> p) g
    nx = newVType v
    (_, r) = head $ filter (\(p, _) -> p == x) g
    g' = filter (\(p, _) -> p /= x && p /= f) g

newVType :: [VType] -> VType
newVType vars = searchType (T 0) vars where
  searchType(T n) vars = if elem (T n) vars then searchType (T $ n+1) vars else T n


elimVarT (n, t) ctx = [(n', t') | (n', t') <- ctx, n' /= n, t' /= t]

--Funcion auxliar que realiza la sustitucion en un tipo
sustTT :: VType -> [ Substitution] -> VType
sustTT t [] = t
sustTT t (x:xs) = sustTT (subst t x) xs

--Busca tuplas con el mismo nombre en 2 contextos y "une" los tipos
buscaSimilares :: Ctx -> Ctx -> [(VType, VType)]
buscaSimilares ctx1 ctx2 = [(t, t') | (n, t) <- ctx1, (n', t') <- ctx2, n == n']

--Realiza la sustitucion en un contexto
sustCtx :: Ctx -> [ Substitution] -> Ctx
sustCtx ctx s = [(n, (sustTT t s)) | (n, t) <- ctx]

sustLT :: MH.Expr -> [ Substitution]-> MH.Expr
sustLT e s = case e of
  MH.V x -> MH.V x
  MH.I n -> MH.I n
  MH.B b -> MH.B b
  MH.Add e1 e2 -> MH.Add (sustLT e1 s)(sustLT e2 s)
  MH.Mul e1 e2 -> MH.Mul (sustLT e1 s)(sustLT e2 s)
  MH.Succ e1 -> MH.Succ (sustLT e1 s)
  MH.Pred e1 -> MH.Pred (sustLT e1 s)
  MH.And e1 e2 -> MH.And (sustLT e1 s)(sustLT e2 s)
  MH.Or e1 e2 -> MH.Or (sustLT e1 s)(sustLT e2 s)
  MH.Not e1 -> MH.Not (sustLT e1 s)
  MH.Lt e1 e2 -> MH.Lt (sustLT e1 s)(sustLT e2 s)
  MH.Gt e1 e2 -> MH.Gt (sustLT e1 s)(sustLT e2 s)
  MH.Eq e1 e2 -> MH.Eq (sustLT e1 s)(sustLT e2 s)
  MH.If b e1 e2 -> MH.If (sustLT b s)(sustLT e1 s)(sustLT e2 s)
  MH.Let x t e1 e2 -> MH.Let x (sustTT t s) (sustLT e1 s)(sustLT e2 s)
  MH.Fun x t e1 -> MH.Fun x (sustTT t s) (sustLT e1 s)
  MH.FunF n x t t1 e -> MH.FunF n x (sustTT t s) (sustTT t1 s) (sustLT e s)
  MH.App e1 e2 -> MH.App (sustLT e1 s)(sustLT e2 s)




