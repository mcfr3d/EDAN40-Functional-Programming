-- String Alignment assignment

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

type AlignmentType = (String,String)

--optAlignments :: String -> String -> [AlignmentType]
--optAlignments string1 string2



similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = max match $max xSpacematch ySpacematch
    where match = similarityScore xs ys + (score x y)
          xSpacematch = similarityScore xs (y:ys) + (score x '-')
          ySpacematch = similarityScore (x:xs) ys + (score '-' y)



score :: Char -> Char -> Int  
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y = scoreMatch
    | otherwise = scoreMismatch


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
--maximaBy valueFcn xs

--outputOptAlignments string1 string2