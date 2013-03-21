{-
Student: Gregor Wegberg, wegbergg
Sheet: 05
-}

-- Assignment 3
data Prop a = Var a | Conj (Prop a) (Prop a) | Disj (Prop a) (Prop a) | Neg (Prop a)
    deriving Show

foldProp fVar fConj fDisj fNeg (Var a) = fVar a
foldProp fVar fConj fDisj fNeg (Conj a b) = fConj (foldProp fVar fConj fDisj fNeg a) (foldProp fVar fConj fDisj fNeg b)
foldProp fVar fConj fDisj fNeg (Disj a b) = fDisj (foldProp fVar fConj fDisj fNeg a) (foldProp fVar fConj fDisj fNeg b)
foldProp fVar fConj fDisj fNeg (Neg a) = fNeg (foldProp fVar fConj fDisj fNeg a)

evalProp :: (a -> Bool) -> Prop a -> Bool
evalProp v p = foldProp v (&&) (||) (not) p

propVars :: Eq a => Prop a -> [a]
propVars p = foldProp (\x -> [x]) (++) (++) id p

-- satProp :: Eq a => Prop a -> Bool
-- ??

-- test stuff
testP a b = Conj (Neg (Var a)) (Var b)
testPTrue = testP "F" "T"
testPFalse = testP "T" "T"
testV a = a == "T"

-- Assignment 4
data Tree t = Leaf | Node t (Tree t) (Tree t)
    deriving Show

breadthFirst Leaf = ""
breadthFirst (Node a (Node b b1 b2) (Node c c1 c2)) = show a ++ show b ++ show c ++ breadthFirst b1 ++ breadthFirst b2 ++ breadthFirst c1 ++ breadthFirst c2
breadthFirst (Node a (Node b b1 b2) Leaf) = show a ++ show b ++ breadthFirst b1 ++ breadthFirst b2
breadthFirst (Node a Leaf (Node b b1 b2)) = show a ++ show b ++ breadthFirst b1 ++ breadthFirst b2
breadthFirst (Node a Leaf Leaf) = show a

-- test stuff
testTree = Node "r" (Node "a" (Node "e" Leaf Leaf) Leaf) (Node "b" (Node "c" Leaf Leaf) (Node "d" Leaf Leaf))
