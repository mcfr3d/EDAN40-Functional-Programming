-- String Alignment assignment

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] _ = [("","")]
optAlignments _ [] = [("","")]
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) $match++xSpacematch++ySpacematch
    where match = attachHeads x y $ optAlignments xs ys
          xSpacematch = attachHeads x '-' $optAlignments xs (y:ys)
          ySpacematch = attachHeads '-' y $optAlignments (x:xs) ys




similarityScore :: String -> String -> Int
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore (x:xs) (y:ys) = max match $max xSpacematch ySpacematch
    where match = similarityScore xs ys + score x y
          xSpacematch = similarityScore xs (y:ys) + score x '-'
          ySpacematch = similarityScore (x:xs) ys + score '-' y



score :: Char -> Char -> Int  
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y = scoreMatch
    | otherwise = scoreMismatch

-- attachHeads functions attaches the two arguments to the front of each list in the tuple
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = [a| a <- xs, valueFcn a == maxVal ] 
    where maxVal = maximum [a| a <- map valueFcn xs]

--outputOptAlignments string1 string2