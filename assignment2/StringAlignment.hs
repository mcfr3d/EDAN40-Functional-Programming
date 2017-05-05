-- String Alignment assignment

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [][] = [("","")]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs "")
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments "" ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry totalScore) $match++xSpacematch++ySpacematch
    where match = attachHeads x y $optAlignments xs ys
          xSpacematch = attachHeads x '-' $optAlignments xs (y:ys)
          ySpacematch = attachHeads '-' y $optAlignments (x:xs) ys

max' :: Ord a => a -> a -> a -> a
max' x y z = max x $max y z

similarityScore :: String -> String -> Int
similarityScore [][] = 0
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = max' match xSpacematch ySpacematch
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
    where maxVal = maximum $map valueFcn xs
    
    
totalScore :: String -> String -> Int
totalScore x y = sum $map (uncurry score) $zip x y

getAlignments :: AlignmentType -> IO()
getAlignments (x,y) = do
    putStrLn " "
    putStrLn x
    putStrLn y
    putStrLn " "

    
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
    putStrLn ("There are " ++ show number ++ " optimal alignments:")
    mapM_ getAlignments alignments
    putStrLn ("There were " ++ show number ++ " optimal alignments!")
    where
    alignments = optAlignments string1 string2
    number = length alignments

similarityScore2 :: String -> String -> Int
similarityScore2 xs ys = simScr (length xs) (length ys)
    where
        simScr i j = simScrTable!!i!!j
        simScrTable = [[simEntry i j | j<-[0..]] | i<-[0..] ]
    
        simEntry :: Int -> Int -> Int
        simEntry 0 0 = 0
        simEntry x 0 = x*scoreSpace
        simEntry 0 y = y*scoreSpace
        simEntry i j  = max' scoreDiag scoreDown scoreLeft

            where
                x = xs!!(i-1)
                y = ys!!(j-1)
                scoreDiag =  simScr (i-1) (j-1) + score x y
                scoreDown =  simScr (i-1) j + scoreSpace
                scoreLeft =  simScr i (j-1) + scoreSpace
    
mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)    