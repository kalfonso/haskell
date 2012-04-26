import Data.List
import Test.QuickCheck

type Anagrams = [(String,[String])]

findAnagrams :: [String] -> Anagrams
findAnagrams [] = []
findAnagrams xs = findAnagramsAcc xs []

findAnagramsAcc :: [String] -> Anagrams -> [[String]]
findAnagramsAcc [] anagrams = filter (\l -> length l == 1) $ map snd anagrams
findAnagramsAcc (x:xs) anagrams | not $ containsAnagram x anagrams = findAnagramsAcc xs $ insertAnagram x anagrams
                                | otherwise findAnagramsAcc xs anagrams
                                  
containsAnagram :: String -> Anagrams -> Bool
containsAnagram _ [] = False
containsAnagram x ((a,as):restAnagrams) 
  | (sort x) == a && x `elem` as = True
  | otherwise = containsAnagram x restAnagrams
                                                      
insertAnagram :: String -> Anagrams -> Anagrams
insertAnagram anagram [] = [(sort anagram,[anagram])]
insertAnagram anagram ((a,as):restAnagrams) | sortedAnagram == a && not $ anagram `elem` as = ((a,(anagram:as)):restAnagrams)
                                            | otherwise = (a,as):(insertAnagram anagram restAnagrams)


  