module Eval (normalize) where

import Expr
import Subst

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show,Eq)

-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LExpCxt -> LExp -> LExp
plug Hole       d = d
plug (A'1 c e2) d = plug c (A d e2)
plug (A'2 e1 c) d = plug c (A e1 d)
plug (L' x c)   d = plug c (L x d)

-- a pointer to a subexpression (a.k.a. "zipper") is a pair of a context and an expression
type LExpPtr = (LExpCxt,LExp)

-- a template for implementing normalize, as described in the
-- mini-project page...


--Write a function subexp :: LExp -> [LExpPtr] 
--that generates pointers to all subexpressions of a given expression, 
--in a left-to-right, preorder traversal. 
--Under Haskell's lazy semantics, subexp e may be seen as a computation that non-deterministically focuses on a subexpression of e, 
--trying the leftmost outermost subexpressions first.

subexp :: LExp -> [LExpPtr]
subexp e = subexp' Hole e
  where
    subexp' :: LExpCxt -> LExp -> [LExpPtr]
    subexp' c e = (c,e) : case e of
        V _ -> []
        A e1 e2 -> subexp' (A'1 c e2) e1 ++ subexp' (A'2 e1 c) e2
        L x e1 -> subexp' (L' x c) e1

--Use subexp to write a function redex :: LExp -> [LExpPtr] 
--that generates pointers to all beta-redices in a given expression, 
--starting with the leftmost outermost redices.

redex :: LExp -> [LExpPtr]
redex e = [p | p@(c,e) <- subexp e, isRedex p]
  where
    isRedex :: LExpPtr -> Bool
    isRedex (_,A (L _ _) _) = True
    isRedex _ = False

--Use redex to write a function stepBeta :: LExp -> [LExp] 
--that non-deterministically performs a beta-reduction on the input expression, 
-- trying the leftmost outermost beta-reductions first.
stepBeta :: LExp -> [LExp]
stepBeta e = [plug c (subst x e2 e1) | (c,A (L x e1) e2) <- redex e]
  where
    subst :: Var -> LExp -> LExp -> LExp
    subst x e1 e2 = subst' e2
      where
        subst' :: LExp -> LExp
        subst' e = case e of
            V y -> if y == x then e1 else e
            A e1 e2 -> A (subst' e1) (subst' e2)
            L y e1 -> if y == x then e else L y (subst' e1)


--Use stepBeta to write normalize by repeatedly performing leftmost outermost beta-reductions 
--until you reach a normal form (or the universe collapses).

normalize :: LExp -> LExp
normalize e = case stepBeta e of
    [] -> e
    (e':_) -> normalize e'


e1 = L "x" (A (V "x") (V "y")) -- (\x.x y)
