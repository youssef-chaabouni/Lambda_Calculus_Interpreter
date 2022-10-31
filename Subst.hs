module Subst (subst) where

import Expr

-- substitution of a term for a variable in an expression
-- case 1: e = \x.e' a lambda expression binding x, then x is shadowed by the abstraction and we return the abstraction unchanged
-- case2: e = \y.e' a lambda expression binding y, that is not in the free variables of d; then we can safely substitue into the body of the abtraction and abstract the result. 
-- case 3: e = \y.e' a lambda expression binding y, that IS free in d, we alpha-convert he lambda expression to use a fresh variable z e'{z/y} -> swap z for y in e'

subst :: (LExp,Var) -> LExp -> LExp
subst (d, x) e = case e of
    V y -> if x == y then d else e
    A e1 e2 -> A (subst (d,x) e1) (subst (d,x) e2)
    L y e1 -> if x == y then
        let z = fresh (free d) in
            L z (subst (d,x) (swapname (y,z) e1))
        else L y (subst (d,x) e1)

-- compute a fresh variable name
fresh :: [Var] -> Var
fresh xs = head (filter (`notElem` xs) names)
  where
    names = "x" : map (\n -> "x" ++ show n) [0..]

-- ################### 
-- Test for sbst function 
-- test cases
-- write the following lambsa expressions using types defined in Expre.hs
-- e = (x(\xy.x))[d/x] = d(\xy.x)
-- subst ()
e1 = L "x" (A (V "x") (V "y"))
e2 = V "x"

-- finish test cases, create some for alpha-conversion
-- make sure fresh function is efficient 
-- subst (e1, "y") e2
-- subst (e2, "y") e1
-- subst (e1, "x") e2