{-|
Module      : Unifier
Description : Provides the implementation of Martelli Montanari Unification Algorithm for Types.
Maintainer  : pablog@ciencias.unam.mx
-}
module Unifier (µ) where

import Syntax.Type

-- |The 'µ' function returns the most general unifier of a set of type equations.
µ :: [(Type, Type)] -> [Substitution]
µ [] = [[]]
µ ((t1, t2) : ts) = [o s1 s2 | s1 <- unify t1 t2, s2 <- µ [(subst (fst t) s1, subst (snd t) s1) | t <- ts]]

-- |The 'unify' function unify a pair of types.
unify :: Type -> Type -> [Substitution]
unify (T x) (T y) = if x == y then [[]] else [[(x, T y)]]
unify (T x) t = if elem x (ids t) then
                  error ("Unification Fails. (" ++ show (T x) ++ "=" ++ show t ++ ")")
                else return [(x, t)] where
                 ids t
                  = case t of
                    T x -> [x]
                    t1 :-> t2 -> ids t1 ++ ids t2
                    _ -> []
unify t (T x) = unify (T x) t
unify (t1 :-> t2) (t3 :-> t4) = [o s1 s2 | s1 <- (unify t1 t3), s2 <- (unify (subst t2 s1) (subst t4 s1))]
unify t s = if t == s then [[]] else error ("Unification Fails. (" ++ show t ++ "=" ++ show s ++ ")")