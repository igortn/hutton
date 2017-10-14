-- Tautology Checker
-- Checks if a proposition is true for all possible values of variables

module TautCheck where

-- definition of a logical proposition
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- lookup table
type Subst = [(Char, Bool)]

find :: Char -> Subst -> Bool
find c s = head [b | (c',b) <- s, c'==c]

-- we want to evaluate a proposition for a given substitution
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find c s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Imply p1 p2) = eval s p1 <= eval s p2


-- to decide if a proposition is a tautology, we need to consider
-- all possible values for the variables that it contains

-- returns the list of (duplicate) vars in a substitution
vars :: Prop -> [Char]
vars (Const b) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

-- all possible permutations of n boolean variables
perms :: Int -> [[Bool]]
perms 1 = [[True], [False]]
perms n = map (True :) bs ++ map (False :) bs
  where bs = perms (n-1)

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) = x : filter (x /=) (removeDupes xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (perms (length vs))
  where vs = removeDupes (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

------- Tests ---------

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
