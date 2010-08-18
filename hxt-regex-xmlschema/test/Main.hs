module Main
where

import Control.Arrow

import System

import Text.Regex.XMLSchema.String
import Text.Regex.XMLSchema.String.Regex

import Test.HUnit

-- ------------------------------------------------------------

parseTestsStd              :: Test
parseTestsStd
    = TestLabel "standard XML parse tests" $
      TestList $
      map parseTest $ tests
    where
    parseTest (re, rep)
        = TestCase $
          assertEqual (show re ++ " must be parsed as " ++ show rep)
                      rep
                      (show . parseRegexExt $ re)

    tests = [ ("",              "()")
            , (".",             ".")
            , (".*",            "(.*)")
            , ("(())",          "()")
            , ("(a*)*",         "(a*)")
            , ("(a*)+",         "(a*)")
            , ("(a+)*",         "(a*)")
            , ("(a+)+",         "(a+)")
            , ("(a?){2,}",              "(a*)")
            , ("((a?){2,}){0,}",        "(a*)")
            , ("((a?){2,}){3,}",        "(a*)")
            , ("(a{0,}){2,}",           "(a*)")
            , ("(a{2,}){3,}",           "(a{6,})")
            , ("[9-0]",                 "{empty char range}")
            , ("[0-9]",                 "[0-9]")
            , ("[0-99-0]",              "[0-9]")
            , ("[abc]",                 "[a-c]")
            , ("[\0-\1114111]",         "\\a")
            , ("[\0-\1114111]|[0-9]",   "\\a")
            , ("[\0-\1114110]",         "[&#0;-&#1114110;]"     )
            , ("[abc-[b]]",             "[ac]" )
            , ("a|b|c|d",               "[a-d]" )
            , ("(a|b)|c",               "[a-c]" )
            , ("a|(b|c)",               "[a-c]" )
            , ("abc",                   "(a(bc))"       )                               -- seq is right ass
            , ("({1}a+)",               "({1}(a+))"     )                               -- extension: labeled subexpressions
            , ("({1}({2}({3}a)))",      "({1}({2}({3}a)))"      )
            ]

parseTestsExt              :: Test
parseTestsExt
    = TestLabel "extended parse tests" $
      TestList $
      map parseTest $ tests
    where
    parseTest (re, rep)
        = TestCase $
          assertEqual (show re ++ " must be parsed as " ++ show rep)
                      rep
                      (show . parseRegexExt $ re)

    tests = [ ("",              "()")
            , (".",             ".")
            , (".*",            "(.*)")
            , ("\\a",           "\\a")
            , ("\\A",           "\\A")
            , ("(())",          "()")
            , ("(a*)*",         "(a*)")
            , ("(a*)+",         "(a*)")
            , ("(a+)*",         "(a*)")
            , ("(a+)+",         "(a+)")
            , ("(a?){2,}",              "(a*)")
            , ("((a?){2,}){0,}",        "(a*)")
            , ("((a?){2,}){3,}",        "(a*)")
            , ("(a{0,}){2,}",           "(a*)")
            , ("(a{2,}){3,}",           "(a{6,})")
            , ("[9-0]",                 "{empty char range}")
            , ("[0-9]",                 "[0-9]")
            , ("[0-99-0]",              "[0-9]")
            , ("[abc]",                 "[a-c]")
            , ("[\0-\1114111]",         "\\a")
            , ("[\0-\1114111]|[0-9]",   "\\a")
            , ("[\0-\1114110]",         "[&#0;-&#1114110;]"     )
            , ("[abc-[b]]",             "[ac]" )
            , ("a|b|c|d",               "[a-d]" )
            , ("(a|b)|c",               "[a-c]" )
            , ("a|(b|c)",               "[a-c]" )
            , ("abc",                   "(a(bc))"       )                               -- seq is right ass
            , ("a*{^}b*",               "((a*){^}(b*))" )                               -- extension: exor
            , ("a*{^}b*{^}c*",          "((a*){^}((b*){^}(c*)))")
            , ("a*{&}b*",               "((a*){&}(b*))" )                               -- extension: intersection
            , ("a*{&}b*{&}c*",          "((a*){&}((b*){&}(c*)))")
            , ("a*{\\}b*",              "((a*){\\}(b*))"        )                       -- extension: set difference
            , ("a*{\\}b*{\\}c*",        "(((a*){\\}(b*)){\\}(c*))"      )
            , ("(a|b)*{\\}(.*aa.*)",    "(([ab]*){\\}((.*)(a(a(.*)))))" )
            , ("({1}a+)",               "({1}(a+))"     )                               -- extension: labeled subexpressions
            , ("({1}({2}({3}a)))",      "({1}({2}({3}a)))"      )
            , ("({1}do){|}({2}[a-z]+)", "(({1}(do)){|}({2}([a-z]+)))"   )               -- deterministic choice of submatches
            , ("a{:}b{:}c",             "(a{:}(b{:}c))" )                               -- interleave
            ]

simpleMatchTests        :: Test
simpleMatchTests
    = TestLabel "simple match tests" $
      TestList $
      concatMap matchTest $ tests
    where
    matchTest (re, ok, er)
        = map (matchOK re) ok
          ++
          map (matchErr re) er
    matchOK  re s       = TestCase $ assertBool (show s ++ " must match "     ++ show re)      (matchExt re s)
    matchErr re s       = TestCase $ assertBool (show s ++ " must not match " ++ show re) (not (matchExt re s))

    tests = [ ( ""
              , [""]
              , ["a"]
              )
            , ( "a"
              , ["a"]
              , ["", "b", "ab"]
              )
            , ( "()"
              , [""]
              , ["a"]
              )
            , ( "ab"
              , ["ab"]
              , ["", "b", "abc"]
              )
            , ( "."
              , [".","a","\0","\1114111"]
              , ["\n","\r","",".."]
              )
            , ( "\\a"
              , [".","a","\n","\r","\0","\1114111"]
              , ["",".."]
              )
            , ( "\\A"
              , ["",".","a","\n","\r","\0","\1114111",".."]
              , []
              )
            , ( "a*"
              , ["", "a", "aa"]
              , ["b", "ab", "aab"]
              )
            , ( "a+"
              , ["a", "aa", "aaa"]
              , ["", "b", "ab"]
              )
            , ( "a?"
              , ["", "a"]
              , ["b", "ab"]
              )
            , ( "a{2}"
              , ["aa"]
              , ["", "a", "aaa"]
              )
            , ( "a{2,}"
              , ["aa","aaa"]
              , ["", "a", "aaab"]
              )
            , ( "a{2,4}"
              , ["aa", "aaa", "aaaa"]
              , ["", "a", "aaaaa", "ab"]
              )
            , ( "a|b"
              , ["a", "b"]
              , ["", "c", "ab", "abc"]
              )
            , ( "[0-9]"
              , ["0", "5", "9"]
              , ["", "a", "00"]
              )
            , ( "[^0-9]"
              , ["a"]
              , ["", "0", "9", "00"])
            , ( "\32"
              , [" "]
              , []
              )
            , ( "[\0-\1114111]"
              , ["\0","\1114111","a"]
              , ["","aaa"]
              )
            , ( "[^\0-\1114111]"
              , []
              , ["","aaa","\0","\1114111","a"]
              )
            , ( ".*a.*|.*b.*|.*c.*"
              , ["a", "abc", "acdc"]
              , ["", "dddd"]
              )
            , ( ".*a.*{&}.*b.*{&}.*c.*"
              , ["abc", "abcd", "abcabcd"]
              , ["", "a", "bc", "acdc", "dddd"]
              )
            , ( ".*a.*{&}.*b.*{&}.*c.*{&}.{3}"          -- all permutations of "abc"
              , ["abc", "acb", "bac", "bca", "cab", "cba"]
              , ["", "a", "bc", "acd", "aaaa", "aba"]
              )
            , ( ".*a.*{&}.*b.*{&}.*c.*"                 -- all words containing at least 1 a, 1 b and 1 c
              , ["abc", "acb", "bac", "bca", "cab", "cba", "abcd", "abcabc"]
              , ["", "a", "bc", "acd", "aaaa"]
              )
            , ( ".*a.*{^}.*b.*"                         -- all words containing at least 1 a or 1 b but not both a's and b's
              , ["a", "b", "ac", "bc", "aaaa", "bbb", "aacc", "ccbb", "acdc"]
              , ["", "ab", "abc", "dddd"]
              )
            , ( "/[*](.*{\\}(.*[*]/.*))[*]/"            -- single line C comment of form /*...*/, but without any */ in the comment body
                                                        -- this is the way to specify none greedy expessions
                                                        -- if multy line comment are required, substitute .* by \A, so newlines are allowed
              , ["/**/","/***/","/*x*/","/*///*/"]
              , ["", "/", "/*", "/*/", "/**/*/", "/*xxx*/xxx*/"]
              )
            , ( "a{:}b{:}c"
              , ["abc", "acb", "bac", "bca", "cab", "cba"]
              , ["", "a", "ab", "abcc", "abca", "aba"]
              )
            ]

-- ------------------------------------------------------------

simpleSplitTests        :: Test
simpleSplitTests
    = TestLabel "simple split tests" $
      TestList $
      map splitTest $ tests
    where
    splitTest (re, inp, tok)
        = TestCase $
          assertEqual ("split " ++ show re ++ show inp ++ " = (" ++ show tok ++ ", ... )")
                      tok
                      (fst (split re inp))
    tests = [ ("",      "a",    ""      )
            , ("a*b",   "abc",  "ab"    )
            , ("a*",    "bc",   ""      )
            , ("a+",    "bc",   ""      )
            , ("a{2}",  "aaa",  "aa"    )
            , ("a{2,}", "aaa",  "aaa"   )
            , ("a|b",   "ab",   "a"     )
            , ("a|b*",  "bbba", "bbb"   )
            , ("abc",   "abcd", "abc"   )
            ]

-- ------------------------------------------------------------

simpleTokenTests        :: Test
simpleTokenTests
    = TestLabel "simple token tests" $
      TestList $
      map tokenTest $ tests
    where
    tokenTest (re, inp, toks)
        = TestCase $
          assertEqual ("tokenize " ++ show re ++ " " ++ show inp ++ " = " ++ show toks)
                      toks
                      (tokenize re inp)
    tests = [ ("",      "",     []      )
            , ("a",     "aba",  ["a", "a"]      )
            , ("a",     "b",    []      )
            , ("a",     "ba",   ["a"]   )
            , ("a*",    "a",    ["a"]   )
            , ("a*",    "ba",   ["","a"]        )
            , ("a*",    "aba",  ["a", "a"]      )
            , ("a*",    "abba", ["a", "", "a"]  )
            , ("a+",    "abba", ["a", "a"]      )
            , ("a*b",   "abba", ["ab", "b"]     )
            , (".*",    "a\n\nb",       ["a", "", "b"]  )
            , (".*",    "a\n\nb\n",     ["a", "", "b"]  )
            , ("\\w+",  "a\n\nb\n",     ["a", "b"]      )
            , ("\\w|ab",        "aaa\n\nabc\n", ["a", "a", "a", "ab", "c"]      )
            , ("\\w|ab",        "aaa abc",      ["a", "a", "a", "ab", "c"]      )
            ]

-- ------------------------------------------------------------

allTests        :: Test
allTests
    = TestList
      [ parseTestsStd
      , parseTestsExt
      , simpleMatchTests
      , simpleSplitTests
      , simpleTokenTests
      ]

main    :: IO ()
main
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
          fails = failures c
      System.exitWith (codeGet errs fails)

codeGet :: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ------------------------------------------------------------

deltaTrc                :: Regex -> String -> [(String, Regex)]
deltaTrc re ""          = [("", re)]
deltaTrc re s@(c:cs)    =  (s,  re)
                           :
                           ( if isZero re'
                             then [("",re')]
                             else deltaTrc re' cs
                           )
                          where
                          re' = delta1 re c

matchTrc                :: String -> String -> (Bool, [(String, Regex)])
matchTrc re             = ( parseRegex >>> deltaTrc$ re )
                          >>>
                          ( (last >>> snd >>> nullable) &&& id )


trcMatch                :: String -> String -> IO()
trcMatch re             = putStrLn . showTrc . matchTrc re
                          where
                          showTrc =
                              ( (show >>> (++ "\n"))
                                ***
                                (concatMap ( ((++ "\t") *** (show >>> (++"\n")))
                                             >>> uncurry (++)
                                           )
                                )
                              )
                              >>>
                              uncurry (flip (++))

-- ------------------------------------------------------------
