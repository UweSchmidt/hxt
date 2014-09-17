{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Arrow

import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy.Char8              as BL
import           Data.String                             (IsString (..))
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as TL

import           System.Exit                             (ExitCode (..),
                                                          exitWith)

import           Text.Regex.XMLSchema.Generic
import           Text.Regex.XMLSchema.Generic.Regex
import           Text.Regex.XMLSchema.Generic.StringLike

import           Test.HUnit

-- ------------------------------------------------------------

newtype Test' a = Test' {unTest' :: Test}

type BS    = B.ByteString
type BL    = BL.ByteString
type Text  = T.Text
type TextL = TL.Text

-- ------------------------------------------------------------

parseTestsStdLatin1 :: forall s . StringLike s => Test' s
parseTestsStdLatin1 = parseTestsStd' testsLatin1

parseTestsStdUnicode :: forall s . StringLike s => Test' s
parseTestsStdUnicode = parseTestsStd' testsUnicode

parseTestsStd' :: forall s . StringLike s => [(String, String)] -> Test' s
parseTestsStd' tests
    = Test' $
      TestLabel "standard XML parse tests" $
      TestList $
      map parseTest $ tests
    where
    parseTest (re0, rep)
        = TestCase $
          assertEqual (show re ++ " must be parsed as " ++ show rep)
                      rep
                      (show . parseRegexExt $ re)
        where
          re :: s
          re = fromString re0

testsLatin1 :: [(String, String)]
testsLatin1
    = [ ("",              "()")
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
      , ("[abc-[b]]",             "[ac]" )
      , ("a|b|c|d",               "[a-d]" )
      , ("(a|b)|c",               "[a-c]" )
      , ("a|(b|c)",               "[a-c]" )
      , ("abc",                   "(a(bc))"       )                               -- seq is right ass
      , ("({1}a+)",               "({1}(a+))"     )                               -- extension: labeled subexpressions
      , ("({1}({2}({3}a)))",      "({1}({2}({3}a)))"      )
      ]

testsUnicode :: [(String, String)]
testsUnicode
    = [ ("[\0-\1114111]",         "\\a")
      , ("[\0-\1114111]|[0-9]",   "\\a")
      , ("[\0-\1114110]",         "[&#0;-&#1114110;]"     )
      ]

parseTestsExtLatin1 :: forall s . StringLike s => Test' s
parseTestsExtLatin1 = parseTestsExt' testsExtLatin1

parseTestsExtUnicode :: forall s . StringLike s => Test' s
parseTestsExtUnicode = parseTestsExt' testsExtUnicode

parseTestsExt' :: forall s . StringLike s => [(String, String)] -> Test' s
parseTestsExt' tests
    = Test' $
      TestLabel "extended parse tests" $
      TestList $
      map parseTest $ tests
    where
    parseTest (re0, rep)
        = TestCase $
          assertEqual (show re ++ " must be parsed as " ++ show rep)
                      rep
                      (show . parseRegexExt $ re)
        where
          re :: s
          re = fromString re0

testsExtLatin1 :: [(String, String)]
testsExtLatin1
    = [ ("",              "()")
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

testsExtUnicode :: [(String, String)]
testsExtUnicode
    = [ ("[\0-\1114111]",         "\\a")
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

simpleMatchTests :: forall a . StringLike a => Test' a
simpleMatchTests
    = Test' $
      TestLabel "simple match tests" $
      TestList $
      concatMap matchTest $ testsMatch
    where
      matchTest :: (String, [String], [String]) -> [Test]
      matchTest (re0, ok, er)
          = map (matchOK  re . fromString) ok
            ++
            map (matchErr re . fromString) er
          where
            re :: a
            re = fromString re0

      matchOK :: a -> a -> Test
      matchOK  re xs
          = TestCase $ assertBool (show xs ++ " must match "     ++ show re)      (matchExt re xs)
      matchErr re xs
          = TestCase $ assertBool (show xs ++ " must not match " ++ show re) (not (matchExt re xs))

testsMatch :: [(String, [String], [String])]
testsMatch
    = [ ( ""
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
                                                  -- if multi-line comment are required, substitute .* by \A, so newlines are allowed
        , ["/**/","/***/","/*x*/","/*///*/"]
        , ["", "/", "/*", "/*/", "/**/*/", "/*xxx*/xxx*/"]
        )
      , ( "a{:}b{:}c"
        , ["abc", "acb", "bac", "bca", "cab", "cba"]
        , ["", "a", "ab", "abcc", "abca", "aba"]
        )
      ]

-- ------------------------------------------------------------

simpleSplitTests :: forall a . StringLike a => Test' a
simpleSplitTests
    = Test' $
      TestLabel "simple split tests" $
      TestList $
      map splitTest $ testsSplit
    where
      splitTest (re0, inp0, tok0, rest0)
          = TestCase $
            assertEqual
            ("split " ++ show re ++ " " ++ show inp0 ++ " = " ++ show (tok, rest))
            (tok, rest)
            (split re (fromString inp0))
          where
            re, tok, rest :: a
            re   = fromString re0
            tok  = fromString tok0
            rest = fromString rest0

testsSplit :: [(String, String, String, String)]
testsSplit
    = [ ("",      "a",    "",   "a"    )
      , ("a*b",   "abc",  "ab",  "c"   )
      , ("a*",    "bc",   "",    "bc"  )
      , ("a+",    "bc",   "",    "bc"  )
      , ("[",     "bc",   "",    "bc"  )
      , ("a{2}",  "aaa",  "aa",  "a"   )
      , ("a{2,}", "aaa",  "aaa", ""    )
      , ("a|b",   "ab",   "a",   "b"   )
      , ("a|b*",  "bbba", "bbb", "a"   )
      , ("abc",   "abcd", "abc", "d"   )
      ]

-- ------------------------------------------------------------

simpleTokenTests :: forall a . StringLike a => Test' a
simpleTokenTests
    = Test' $
      TestLabel "simple token tests" $
      TestList $
      map tokenTest $ testsToken
    where
      tokenTest (re0, inp0, toks0)
          = TestCase $
            assertEqual
            ("tokenize " ++ show re ++ " " ++ show inp ++ " = " ++ show toks)
            toks
            (tokenize re inp)
          where
            re, inp :: a
            re   = fromString re0
            inp  = fromString inp0
            toks :: [a]
            toks = map fromString toks0

testsToken :: [(String, String, [String])]
testsToken
    = [ ("",      "",     []      )
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

genericTest :: (forall a . StringLike a => Test' a) -> Test
genericTest t
    = TestList $
      [ TestLabel "Test with 'String'"          $ unTest' (t :: Test' String)
      , TestLabel "Test with 'Text'"            $ unTest' (t :: Test' Text)
      , TestLabel "Test with 'Text.Lazy'"       $ unTest' (t :: Test' TextL)
      , TestLabel "Test with 'ByteString'"      $ unTest' (t :: Test' BS)
      , TestLabel "Test with 'ByteString.Lazy'" $ unTest' (t :: Test' BL)
      ]

unicodeTest :: (forall a . StringLike a => Test' a) -> Test
unicodeTest t
    = TestList $
      [ TestLabel "Test with 'String'"          $ unTest' (t :: Test' String)
      , TestLabel "Test with 'Text'"            $ unTest' (t :: Test' Text)
      , TestLabel "Test with 'Text.Lazy'"       $ unTest' (t :: Test' TextL)
      ]

allTests        :: Test
allTests
    = TestList
      [ genericTest parseTestsStdLatin1
      , unicodeTest parseTestsStdUnicode
      , genericTest parseTestsExtLatin1
      , unicodeTest parseTestsExtUnicode
      , genericTest simpleMatchTests
      , genericTest simpleSplitTests
      , genericTest simpleTokenTests
      ]

main    :: IO ()
main
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
          fails = failures c
      exitWith (codeGet errs fails)

codeGet :: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ------------------------------------------------------------

deltaTrc                :: StringLike s => s -> GenRegex s -> [(s, GenRegex s)]
deltaTrc s@(uncons -> Just (c, cs)) re
                        =  (s,  re)
                           :
                           ( if isZero re'
                             then [(emptyS,re')]
                             else deltaTrc cs re'
                           )
                          where
                          re' = delta1 c s re
deltaTrc _        re    = [(emptyS, re)]

matchTrc                :: StringLike s => s -> s -> (Bool, [(s, GenRegex s)])
matchTrc re s           = (nullable . snd . last $ res, res)
                          where
                            res = deltaTrc s (parseRegex re)


trcMatch                :: StringLike s => s -> s -> IO()
trcMatch re             = putStrLn . showTrc . matchTrc re
                          where
                          showTrc =
                              ( (show >>> (++ "\n"))
                                ***
                                (concatMap ( ( (toString >>> (++ "\t"))
                                               ***
                                               (show >>> (++"\n"))
                                             )
                                             >>> uncurry (++)
                                           )
                                )
                              )
                              >>>
                              uncurry (flip (++))

-- ------------------------------------------------------------
