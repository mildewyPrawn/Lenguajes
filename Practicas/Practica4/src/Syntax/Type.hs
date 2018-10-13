{-|
Module      : Syntax.Type
Description : Provides the definition of the types of MinHs expressions with their respective substitution function.
Maintainer  : pablog@ciencias.unam.mx
-}
module Syntax.Type (Type (..), Substitution, subst, o) where

-- |Identifiers for type variables
type Identifier = Int

-- |Operator for the function type (right-associative)
infixr :->

data Type
  -- | Integer type
  = Integer
  -- | Boolean type
  | Boolean
  -- | Variable type
  | T Identifier
  -- | Function type
  | Type :-> Type
  deriving Eq

instance Show Type where
  -- |The 'show' function returns a String representation of the types.
  show t = case t of
    Integer -> "Integer"
    Boolean -> "Boolean"
    T i -> "T" ++ show i
    (t1 :-> t2) :-> (t3 :-> t4) -> "(" ++ show t1 ++ " -> " ++ show t2 ++ ")" ++ " -> (" ++ show t3 ++ " -> " ++ show t4 ++ ")"
    (t1 :-> t2) :-> t3 -> "(" ++ show t1 ++ " -> " ++ show t2 ++ ") -> " ++ show t3
    t1 :-> (t2 :-> t3) -> show t1 ++ " -> (" ++ show t2 ++ " -> " ++ show t3 ++ ")"
    t1 :-> t2 -> show t1 ++ " -> " ++ show t2

-- |A substitution is represented as a set of different pairs (Identifier, Type)
type Substitution = [(Identifier, Type)]

---------------------------
-- Substitucion Function --
---------------------------

-- |The 'subst' function apply a substitution on a type.
subst :: Type -> Substitution -> Type
subst t s
  = case t of
    Integer -> Integer
    Boolean -> Boolean
    T x
      -> case s of
        [] -> T x 
        ((y, t') : ss) -> if x == y then t' else subst t ss
    t1 :-> t2 -> subst t1 s :-> subst t2 s

--------------------------
-- Composition Function --
--------------------------

-- |The 'o' function defines the composition of substitutions.
o :: Substitution -> Substitution -> Substitution
o s1 s2 = simplSubst [(x, subst t s2) | (x, t) <- s1] ++ [(y, t) | (y, t) <- s2, notElem y [x | (x, t) <- s1]]

-- |The 'simplSubst' function remove the repeated pairs in a substitution.
simplSubst :: Substitution -> Substitution
simplSubst [] = []
simplSubst ((x, t) : ss)
  = case t of
    T y -> if x == y then simplSubst ss else (x, t) : simplSubst ss
    _ -> (x, t) : simplSubst ss