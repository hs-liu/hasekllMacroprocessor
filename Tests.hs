module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    , ("abc", [("abc", 8)]) ==> [8]
    ]

splitTextTestCases
  = [ (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    , ("\n", "a\nb\nc")
        ==> ("\n\n",["a","b","c"])
    , (".,<>", "I like, <imperial>.how about, u?")
        ==> (",<>.,",["I like"," ","imperial","","how about"," u?"])  
    , (" ", " A B")
        ==> ("  ",["","A","B"])    
    , (" .", " .")
        ==> (" .",["","",""])
    , (".", "")
        ==> ("",[""])
    ]

combineTestCases
  = [ (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A","B"])
        ==> ["A",".","B"]
    , (" ", ["", "A"])
        ==> [""," ","A"]
    , (" .", ["","",""])
        ==> [""," ","",".",""]
    , ("\n\n", ["a","b","c"])
        ==> ["a","\n","b","\n","c"]
    , ("\n\n", ["a","b","c"])
        ==> ["a","\n","b","\n","c"]
    , ("  ", ["","A","B"])
        ==> [""," ","A"," ","B"]
    , (" .",["","",""])
        ==> [""," ","",".",""]
    ]

getKeywordDefsTestCases
  = [ ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    , ["$1  I like Imperial!"]
        ==> [("$1"," I like Imperial!")]
    , ["$a  123,12 123s"]
        ==> [("$a"," 123,12 123s")]
    , ["$1a   "]
        ==> [("$1a","  ")]

    ]

expandTestCases
  = [ ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
    , (".$1\n$2!$3",
       "$2 $1\n$1 $a\n$3 $ye")
        ==>".$a\n$1!$ye"
    , ("a b c",
       "$a $1\n$b $2\n$c $3")
        ==>"a b c"
    , ("$1$2$3",
       "$1 $a\n$2 $b\n$3 $c")
        ==> ""
    , ("$1$2$3",
       "$1$2$3 $a")
        ==> "$a"
    , ("$1 $2$3",
       "$1 $a")
        ==> "$a "

    ]

allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "splitText"   (uncurry splitText)
                         splitTextTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
