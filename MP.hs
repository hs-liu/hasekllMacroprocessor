module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp s xs = [snd x | x<-xs, s==fst x ]


splitText :: [Char] -> String -> (String, [String])
--base case
splitText s "" = ("",[""])
splitText s (x:xs) 
--can also use ++ here:(x:s',[""]++x':xs'), or concat ->(x:s', concat[[""],x':xs'])
        | x `elem` s = (x:s', "":(x':xs'))
--also here: (s',[[x]++x']++xs')

--explore this 
--can use list@(x:xs)
        | otherwise = (s', concat[[[x]++x'],xs'])
        where (s', x':xs') = splitText s x
       
combine :: String -> [String] -> [String]
combine "" xs = xs
--concat is meant for combining two lists
--: is meant for prepending the element to the list -> adding one thing to the list -> use : not concat
--concat is slower than : (depending the length of first variable -> depends on x this time)
combine (sep:seps) (x:xs) = concat[[x],[[sep]],new_str]
        where new_str = combine seps xs

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
--this zip -> can just use (a,b) #dont use built-in function unless its necessary -> more complicated and slower i think 
getKeywordDefs (x:xs) = concat [zip [c] [a],getKeywordDefs xs]
        where a = concat (combine (bs) (cs))
--using " " is easier but this does not cover all possible cases of inputs
--eg: ["$a b\n$c d"], in this case the line below can't make it ["$a", "b","$c","d"]
--therefore the keyword pairs returned is not accurate
              ((b:bs),(c:cs)) = splitText " " x 

--easier way/cleanest way 
--try this 
--$ does everything on the right side first 
getKeyWord (line:lines) = (keyword, unwords def) : getKeywords lines
where (keyword:def) = snd $ splitText " " line

expand :: FileContents -> FileContents -> FileContents
expand text info = replaceWord text (getKeywordDefs info1)
--i personally feel should avoid using "\n" here (coz magic word)?
        where info1 = snd(splitText "\n" info)

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
--replaceWord "" keys= ""
--another way is to use unwords here: unwords(uncurry combine ((seps,xs2)))
replaceWord text keys = concat (uncurry combine ((seps,xs2)))
        where (seps, xs1) = splitText separators text
--list comprehension used to check for keywords - i like this haha 
              xs2=[if ('$' `elem` x) then unwords(lookUp x keys) else x | x<-xs1]

--just cleaner way
expand template info = concat (combine separators replaced)
        where 
          keywordTemplate = snd $ splitText '\n' info 
          keywords = getKeyWordefs keywords 
          (seps, word) = splitText separators template 
          replaced = map ((flip replaceWord) keywords) words

--only puts the replace word here 
replaceWord w defs
--just get the first one, instead of having all concated together 
        | head w == '$' = head (lookUp w defs)
        | otherwise = w 
-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

--expand the text for three times based on each of the keyword list 
advancedExpand template info = expandlist template (snd $ spliteText '#' info)
        where expandList _ [] = ""
              expandList template (firstkey:lastkey)
                               = (expand template firstkey) ++ "\...\" ++ expand template last key

x:xs -> split the list 
list@(x:xs) -> to get the whole list, list refers to the whole list, x and xs head and tail 


isPalindrome 
