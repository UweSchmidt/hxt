-- ------------------------------------------------------------

{- |
   Module     : ColorizeSourceCode
   Copyright  : Copyright (C) 2009 Uwe Schmidt
   License    : BSD3

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Portability: portable

   Colorize Source Code

   Supports Java and Haskell

-}

-- ------------------------------------------------------------

module Main
where

import Control.Arrow

import Data.List

import System.Environment
import System.IO                        -- import the IO and commandline option stuff
import System.Console.GetOpt
import System.Exit

import Text.Regex.XMLSchema.String
import Text.XML.HXT.Core
import Text.XML.HXT.Parser.XhtmlEntities


-- ------------------------------------------------------------

data Process            = P { inFilter    :: String -> String
                            , tokenRE     :: Regex
                            , markupRE    :: Regex -> Regex
                            , formatToken :: (String, String) -> String
                            , formatDoc   :: [String] -> String
                            , outFilter   :: String -> String
                            , input       :: Handle
                            , output      :: Handle
                            , inputFile   :: String
                            }

defaultProcess          :: Process
defaultProcess          = P { inFilter    = id
                            , tokenRE     = plainRE
                            , markupRE    = id
                            , formatToken = uncurry (++)
                            , formatDoc   = unlines
                            , outFilter   = id
                            , input       = stdin
                            , output      = stdout
                            , inputFile   = " "
                            }

-- ------------------------------------------------------------

main                    :: IO ()
main                    = do
                          argv <- getArgs
                          p    <- evalArgs (getOpt Permute options argv)
                          s <- hGetContents (input p)
                          hPutStr (output p) (process p s)
                          hFlush (output p)
                          hClose (output p)
                          exitWith ExitSuccess

options                 :: [OptDescr (String, String)]
options                 = [ Option "h?" ["help"]    (NoArg ("help",    "1"))    "this message"
                          , Option ""   ["plain"]   (NoArg ("plain",   "1"))    "don't colorize lines"
                          , Option ""   ["haskell"] (NoArg ("haskell", "1"))    "colorize haskell"
                          , Option ""   ["java"]    (NoArg ("java",    "1"))    "colorize java"
                          , Option ""   ["cpp"]     (NoArg ("cpp",     "1"))    "colorize C or C++"
                          , Option ""   ["sh"]      (NoArg ("sh",      "1"))    "colorize sh or bash"
                          , Option ""   ["ruby"]    (NoArg ("ruby",    "1"))    "colorize ruby"
                          , Option ""   ["bnf"]     (NoArg ("bnf",     "1"))    "colorize extended BNF grammar rules"
                          , Option ""   ["ppl"]     (NoArg ("ppl",     "1"))    "colorize ppl"
                          , Option ""   ["pplass"]  (NoArg ("pplass",  "1"))    "colorize ppl assembler"
                          , Option "n"  ["number"]  (NoArg ("number",  "1"))    "with line numbers"
                          , Option "t"  ["tabs"]    (NoArg ("tabs",    "1"))    "substitute tabs by blanks"
                          , Option "m"  ["markup"]  (NoArg ("markup",  "1"))    "text contains embedded markup"
                          , Option "e"  ["erefs"]   (NoArg ("erefs",   "1"))    "resolve HTML entity refs before processing"
                          , Option "o"  ["output"]  (ReqArg ((,) "output")      "FILE") "output file, \"-\" stands for stdout"
                          , Option "s"  ["scan"]    (NoArg ("scan",    "1"))    "just scan input, for testing"
                          , Option "x"  ["html"]    (NoArg ("html",    "1"))    "html output"
                          , Option "f"  ["full"]    (NoArg ("full",    "1"))    "full HTML document with header and css"
                          ]

exitErr                 :: String -> IO a
exitErr msg             = do
                          hPutStrLn stderr msg
                          usage
                          exitWith (ExitFailure (-1))

evalArgs                :: ([(String, String)], [FilePath], [String]) -> IO Process
evalArgs (opts, files, errs)
    | not (null errs)           = exitErr ("illegal arguments " ++ show errs)
    | null files                = evalOpts opts defaultProcess
    | not (null fns)            = exitErr ("only one input file allowed")
    | otherwise                 = do
                                  inp <- openFile fn ReadMode
                                  evalOpts opts (defaultProcess { input = inp
                                                                , inputFile = fn
                                                                }
                                                )
    where
    (fn:fns)                    = files

evalOpts                        :: [(String, String)] -> Process -> IO Process
evalOpts [] res                 = return res
evalOpts (o:os) res             = do
                                  res' <- evalOpt o res
                                  evalOpts os res'

evalOpt                         :: (String, String) -> Process -> IO Process
evalOpt ("help","1") _          = do
                                  usage
                                  exitWith ExitSuccess

evalOpt ("output", "-") p       = return $ p {output = stdout}

evalOpt ("output", fn)  p       = do
                                  outp <- openFile fn WriteMode
                                  return $ p {output = outp}

evalOpt ("haskell","1") p       = return $ p { tokenRE     = haskellRE }
evalOpt ("java",   "1") p       = return $ p { tokenRE     = javaRE    }
evalOpt ("cpp",    "1") p       = return $ p { tokenRE     = cppRE     }
evalOpt ("sh",     "1") p       = return $ p { tokenRE     = shRE      }
evalOpt ("ruby",   "1") p       = return $ p { tokenRE     = rubyRE    }
evalOpt ("bnf",    "1") p       = return $ p { tokenRE     = bnfRE     }
evalOpt ("ppl",    "1") p       = return $ p { tokenRE     = pplRE     }
evalOpt ("pplass", "1") p       = return $ p { tokenRE     = pplassRE  }
evalOpt ("plain",  "1") p       = return $ p { tokenRE     = plainRE   }
evalOpt ("scan",   "1") p       = return $ p { tokenRE     = plainRE
                                             , formatToken = uncurry formatTok
                                             , formatDoc   = formatHList                        }
evalOpt ("number", "1") p       = return $ p { formatDoc   = numberLines >>> formatDoc p        }
evalOpt ("tabs",   "1") p       = return $ p { inFilter    = inFilter p >>> substTabs           }
evalOpt ("erefs",  "1") p       = return $ p { inFilter    = resolveHtmlEntities >>> inFilter p }
evalOpt ("markup", "1") p       = return $ p { markupRE    = addMarkup                          }
evalOpt ("html",   "1") p       = return $ p { formatToken = formatHtmlTok
                                             , formatDoc   = formatHtmlDoc                      }
evalOpt ("full",   "1") p       = return $ p { outFilter   = outFilter p >>> fullHtml (inputFile p)     }
evalOpt (opt,      _v ) p       = exitErr ("illegal option " ++ show opt) >> return p

usage                   :: IO ()
usage                   = hPutStrLn stderr use
                          where
                          use = usageInfo header options
                          header = "colorizeSourceCode - colorize source code with HTML, version 0.1.1"

-- ------------------------------------------------------------

process         :: Process -> String -> String
process p       = inFilter p
                  >>> tokenizeSubexRE (markupRE p (tokenRE p))
                  >>> map (formatToken p)
                  >>> concat
                  >>> lines
                  >>> formatDoc p
                  >>> outFilter p

addMarkup       :: Regex -> Regex
addMarkup       = mkElse (parseRegexExt . mkLE $ markupT)

tokenizeLines   :: String -> [(String, String)]
tokenizeLines   = map (\ l -> ("",l ++ "\n")) . lines

numberLines     :: [String] -> [String]
numberLines     = zipWith addNum [(1::Int)..]
                  where
                  addNum i l = "<span class=\"linenr\">" ++ fmt 4 i ++ "</span>" ++ l
                  fmt l = sed (const "&nbsp;") " "
                          . reverse
                          . take l
                          . reverse
                          . (replicate l ' ' ++)
                          . show

substTabs       :: String -> String
substTabs       = subs 0

subs            :: Int -> String -> String
subs _ ""       = ""
subs i (x:xs)
    | x == '\t' = replicate (8 - (i `mod` 8)) ' ' ++ subs 0 xs
    | x == '\n' = x : subs 0 xs
    | otherwise = x : subs (i+1) xs

-- ------------------------------------------------------------

resolveHtmlEntities     :: String -> String
resolveHtmlEntities     = sed (replaceEntity . drop 1 . init) "&\\i\\c*;"
                          where
                          replaceEntity e = maybe ("&" ++ e ++ ";") ((:[]) . toEnum)
                                            . lookup e $ xhtmlEntities

-- ------------------------------------------------------------

formatHList     :: [String] -> String
formatHList     = ("[" ++) . (++ "\n]") . intercalate "\n, "

formatTok       :: String -> String -> String
formatTok kw tok = " (" ++ show kw ++ ",\t" ++ show tok ++ "\t)\n"

formatHtmlDoc   :: [String] -> String
formatHtmlDoc   = map (("<div class=\"codeline\">" ++) . (++ "</div>") . preserveEmptyLines)
                  >>> ("<div class=\"codeblock\">" :)
                  >>> (++ ["</div>"])
                  >>> unlines
                  where
                  preserveEmptyLines "" = "&nbsp;"
                  preserveEmptyLines l  = l

formatHtmlTok   :: (String, String) -> String
formatHtmlTok ("markup", t@(x:_))
    | x `elem` "<&"     = t
formatHtmlTok (m, t)
    | otherwise         = colorizeTokens m (escapeText >>> sed (const "&nbsp;") " " $ t)

escapeText      :: String -> String
escapeText      = foldr cquote ""
    where
      cquote    = fst escapeHtmlRefs

-- escapeText      = concat . runLA (xshowEscapeXml mkText)


fullHtml        :: String -> String -> String
fullHtml fn s   = unlines
                  [ "<html>"
                  , "<head>"
                  , "<title>" ++ fn ++ "</title>"
                  , "<style>"
                  , css
                  , "</style>"
                  , "</head>"
                  , "<body>"
                  , s
                  , "</body>"
                  , "</html>"
                  ]

css             :: String
css             = unlines
                  [ ".typename          { color: #0000dd; }"
                  , ".varname           { color: #000000; }"
                  , ".opname            { color: #770000; }"
                  , ".operator          { color: #770000; /* font-weight:bold; */ }"
                  , ".keyglyph          { color: #3070A0; /* font-weight:bold; */ }"
                  , ".par               {  }"
                  , ""
                  , ".keyword           { color: #3070A0; /* font-weight:bold; */ }"
                  , ".typekeyword       { color: #3070A0; /* font-weight:bold; */ }"
                  , ".strconst          { color: #228B22; }"
                  , ".charconst         { color: #228B22; }"
                  , ".labelname         { color: #FF00FF; font-weight:bold; }"
                  , ".cppcommand        { color: #0000CD; }"
                  , ".specialword       { color: #c80000; }"
                  , ".classname         { color: #8B2323; }"
                  , ".comment           { color: #00008B; }"
                  , ".bnfnt             { color: #0000CD; }"
                  , ".bnfmeta           { color: #ff0000; font-weight:bold; }"
                  , ".bnfterminal       { color: #008800; font-weight:bold; }"
                  , ".tclproc           { color: #FF6000; }"
                  , ".tclvar            { color: #0000CD; }"
                  , ".tclcomment        { color: #c80000; }"
                  , ""
                  , ".linenr            { color: #909090; padding-right: 2em; }"
                  , "div.codeline       { font-family: monospace; width: 100%; white-space: pre; border-width: 1px; border-style: solid; border-color: transparent; padding-left: 0.3em; }"
                  , "div.codeline:hover { background-color:#ddddff; color:#c80000; border-width: 1px; border-style: solid; border-color: #c80000; }"

                  ]

-- ------------------------------------------------------------

colorizeTokens  :: String -> String -> String
colorizeTokens tok
    | tok `elem` [ "comment"
                 , "keyword"
                 , "keyglyph"
                 , "typekeyword"
                 , "varname", "typename", "labelname", "instancename", "globalname"
                 , "opname"
                 , "par"
                 , "operator"
                 , "strconst", "charconst"
                 , "bnfnt", "bnfmeta"
                 , "cppcommand"
                 , "specialword"
                 ]
                                = wrap
    | tok == "longcomment"      = wrap' "comment" . mlc
    | tok == "bnfterminal"      = wrap . drop 1 . init
    -- | tok == "markupstart"   = (("<span class=\"" ) ++) . (++ ("\">")) . drop 4 . init
    -- | tok == "markupend"        = const "</span>"
    | null tok                  = const ""
    | otherwise                 = id
    where
    wrap       = wrap' tok
    wrap' tok' = (("<span class=\"" ++ tok' ++ "\">") ++) . (++ "</span>")
    mlc        = sed (("</span>" ++) . (++ "<span class=\"comment\">")) "(\\n\r?)"

-- ------------------------------------------------------------

buildRegex              :: [(String, String)] -> Regex
buildRegex              = foldr1 mkElse . map (uncurry mkBr') . map (second parseRegexExt)
                          where
                          mkBr' ""      = id
                          mkBr' l       = mkBr l


buildKeywords           :: [String] -> String
buildKeywords           = intercalate "|"

untilRE                 :: String -> String
untilRE re              = "(\\A{" ++ "\\}\\A" ++ re ++ "\\A)" ++ re

mkLE                    :: (String, String) -> String
mkLE (l, re)            = "({" ++ l ++ "}(" ++ re ++ "))"

ws1RE, ws1RE',ws0RE     :: String
ws1RE                   = "\\s+"
ws1RE'                  = "[ \t]+"
ws0RE                   = "[ \t]*"

ws, ws', javacmt1, javacmt, shcmt1, strconst,
  markupT,
  charconst, number,
  par, xxx              :: (String, String)

-- markupS                      = ("markupstart",       "<[a-zA-Z0-9]+>"        )
-- markupE                      = ("markupend",         "</[a-zA-Z0-9]+>"       )
markupT                 = ("markup",            ( "</?" ++ xname ++ "(" ++ xattr ++ ")*" ++ "\\s*>"
                                                  ++ "|" ++
                                                  "&" ++ xname ++ ";"
                                                )
                          )
                          where
                          xname = "[A-Za-z][-_:A-Za-z0-9]*"
                          xattr = ws1RE ++ xname ++ eq ++ "(" ++ dq ++ "|" ++ sq ++ ")"
                          eq    = "\\s*=\\s*"
                          dq    = "\"[^\"]*\""
                          sq    = "\'[^\']*\'"

ws                      = ("ws",                ws1RE           )
ws'                     = ("ws",                ws1RE'          )
javacmt1                = ("comment",           "//.*"          )
javacmt                 = ("longcomment",       "/\\*" ++ untilRE "\\*/"        )
shcmt1                  = ("comment",           "#.*"           )
strconst                = ("strconst",          "\"([^\"\\\\\n\r]|\\\\.)*\""    )
charconst               = ("charconst",         "\'([^\'\\\\\n\r]|\\\\.)*\'"    )
number                  = ("number",            "[0-9]+(\\.[0-9]*([eE][-+]?[0-9]+)?)?"  )
par                     = ("par",               "[\\(\\)\\[\\]\\{\\}]"          )
xxx                     = ("xxx",               "."     )

-- ------------------------------------------------------------

plainRE                 :: Regex
plainRE                 = buildRegex
                          [ ("xxx",             "[^<&\n]+"              )
                          , ("xxx",             "[<&\n]"                )
                          ]

-- ------------------------------------------------------------

haskellRE               :: Regex
haskellRE               = buildRegex
                          [ ws
                          , ("comment",         "(-)- .*"       )
                          , ("longcomment",     "\\{" ++ untilRE "-\\}" )
                          , ("keyword",         buildKeywords
                                                [ "case", "class"
                                                , "data", "default", "deriving", "do"
                                                , "else"
                                                , "forall"
                                                , "if", "import", "in"
                                                , "infix", "infixl", "infixr"
                                                , "instance"
                                                , "let"
                                                , "module"
                                                , "newtype"
                                                , "of"
                                                , "qualified"
                                                , "then", "type"
                                                , "where"
                                                , "_"
                                                , "as", "ccall", "foreign", "hiding", "proc", "safe", "unsafe"
                                                ]
                            )
                          , ("keyglyph",        buildKeywords
                                                ["\\.\\.","::","=","\\\\","\\|","<-","->","-<","@","~","=>","!",",",";"]
                            )
                          , ("varname"  ,       varname )
                          , ("typename",        "[A-Z_][a-zA-Z0-9_]*[']*"       )
                          , ("opname",          "`" ++ varname ++ "`"           )
                          , strconst
                          , charconst
                          , number
                          , par
                          , ("operator",        "[-!#$%&\\*\\+./<=>\\?@\\\\^\\|~]+")
                          , xxx
                          ]
                        where
                        varname = "[a-z_][a-zA-Z0-9_]*[']*"

-- ------------------------------------------------------------

javaRE                  :: Regex
javaRE                  = buildRegex
                          [ ws
                          , javacmt1
                          , javacmt
                          , ("keyword", buildKeywords
                                        [ "abstract", "assert"
                                        , "break"
                                        , "case", "catch", "class", "continue"
                                        , "default", "do"
                                        , "else", "extends"
                                        , "final", "finally", "for"
                                        , "if", "implements", "import", "instanceof", "interface"
                                        , "native", "new"
                                        , "package", "private", "protected", "public"
                                        , "return"
                                        , "static", "super", "switch", "synchronized"
                                        , "this", "throw", "throws", "transient", "try"
                                        , "volatile"
                                        , "while"
                                        ]       )
                          , ("typekeyword",
                                        buildKeywords
                                        [ "boolean", "byte"
                                        , "char"
                                        , "double"
                                        , "false", "float"
                                        , "int"
                                        , "long"
                                        , "null"
                                        , "short"
                                        , "true"
                                        , "void"
                                        ]       )
                          , ("labelname",       "(" ++ varname ++ "{\\}default):"               )
                          , ("",                ( mkLE ("keyword", "break|continue")
                                                  ++ mkLE ws ++
                                                  mkLE ("labelname", varname)
                                                )
                            )
                          , ("varname",         varname                 )
                          , ("typename",        "[A-Z][a-zA-Z0-9_]*"    )
                          , strconst
                          , charconst
                          , number
                          , par
                          , ("delimiter",       "[.,;]"                         )
                          , ("operator",        "[-+!%&/=\\*\\?~|<>:]+" )
                          , xxx
                          ]
                          where
                          varname = "[a-z][a-zA-Z0-9_]*"

-- ------------------------------------------------------------

bnfRE                   :: Regex
bnfRE                   = buildRegex
                          [ ws
                          , ("bnfnt"            , "[A-Z][a-zA-Z0-9_]*"  )
                          , ("bnfterminal",     "\"([^\"\\\\\n\r]|\\\\.)*\""    )
                          , ("bnfmeta",         buildKeywords
                                                [ "\\["
                                                , "\\]"
                                                , "::="
                                                , "\\|"
                                                , "\\{"
                                                , "\\}"
                                                ]
                            )
                          , xxx
                          ]

-- ------------------------------------------------------------

cppRE                   :: Regex
cppRE                   = buildRegex
                          [ ws
                          , javacmt1
                          , javacmt
                          , ("keyword", buildKeywords
                                        [ "asm" , "auto"
                                        , "break"
                                        , "case" , "catch" , "class" , "const" , "continue"
                                        , "default" , "delete" , "do"
                                        , "else" , "extern"
                                        , "for" , "friend"
                                        , "goto"
                                        , "if" , "inline"
                                        , "new"
                                        , "operator" , "overload"
                                        , "private" , "protected" , "public"
                                        , "register" , "return"
                                        , "sizeof" , "static" , "switch"
                                        , "template" , "this" , "typedef" , "throw" , "try"
                                        , "virtual" , "volatile"
                                        , "while"
                                        ]
                            )
                          , ("typekeyword",
                                        buildKeywords
                                        [ "char"
                                        , "double"
                                        , "enum"
                                        , "float"
                                        , "int"
                                        , "long"
                                        , "short"
                                        , "signed"
                                        , "struct"
                                        , "union"
                                        , "unsigned"
                                        , "void"
                                        ]
                            )
                          , ("cppcommand",      ( "#" ++ ws0RE ++ "("
                                                  ++
                                                  buildKeywords
                                                  [ "define"
                                                  , "else"
                                                  , "endif"
                                                  , "if"
                                                  , "ifdef"
                                                  , "ifndef"
                                                  , "(include[ \t].*)"
                                                  , "undef"
                                                  ]
                                                  ++ ")"
                                                )
                            )
                          , ("specialword",     buildKeywords
                                                [ "assert"
                                                , "exit"
                                                , "free"
                                                , "main"
                                                , "malloc"
                                                ]
                            )
                          , ("varname",         varname                 )
                          , ("typename",        "[A-Z][a-zA-Z0-9_]*"    )
                          , strconst
                          , charconst
                          , number
                          , par
                          , ("delimiter",       "[.,;]"                         )
                          , ("operator",        "[-+!%&/=\\*\\?~|<>:]+" )
                          , xxx
                          ]
                          where
                          varname = "[a-z][a-zA-Z0-9_]*"

-- ------------------------------------------------------------

shRE                    :: Regex
shRE                    = buildRegex
                          [ ws
                          , shcmt1
                          , ("keyword",         buildKeywords
                                                [ "alias"
                                                , "break" , "bg"
                                                , "case" , "cd" , "continue"
                                                , "declare" , "do" , "done"
                                                , "echo" , "elif" , "else" , "env" , "esac" , "eval" , "exec" , "exit" , "export"
                                                , "false" , "fg" , "fi" , "for" , "function"
                                                , "if" , "in"
                                                , "jobs"
                                                , "kill"
                                                , "local"
                                                , "pwd"
                                                , "return"
                                                , "set" , "shift"
                                                , "test" , "then" , "trap" , "true"
                                                , "unalias" , "unset"
                                                , "while" , "wait"
                                                ]
                            )
                          , ("varname",         "[A-Za-z_][a-zA-Z0-9_]*"        )
                          , ("operator",        "[-+!%&=\\\\\\*\\?~|<>:@$]+"    )
                          , ("operator",        "[\\(\\)\\[\\]\\{\\}]+"         )
                          , strconst
                          , charconst
                          , xxx
                          ]

-- ------------------------------------------------------------

rubyRE                  :: Regex
rubyRE                  = buildRegex
                          [ ws
                          , rubycmt
                          , ("keyword",         buildKeywords
                                                [ "begin" , "break"
                                                , "catch" , "case" , "class"
                                                , "def" , "do"
                                                , "else" , "elif" , "end" , "ensure"
                                                , "false" , "for"
                                                , "if" , "in" , "include" , "initialize"
                                                , "loop"
                                                , "module"
                                                , "new" , "nil"
                                                , "raise" , "require" , "rescue"
                                                , "self"
                                                , "then" , "true" , "type"
                                                , "until"
                                                , "when" , "while"
                                                , "yield"
                                                ]
                            )
                          , ("typename",        "[A-Z][A-Za-z0-9]*"     )
                          , ("varname",         "[A-Za-z_][a-zA-Z0-9_]*(!|\\?)?"        )
                          , ("instancename",    "(@{1,2}|$)[A-Za-z_][a-zA-Z0-9_]*"      )
                          , ("strconst",        "%[qQx]\\{.*\\}"        )
                          , ("strconst",        "#\\{.*\\}"     )
                          , ("strconst",        ":[a-z][A-Za-z0-9]*"    )
                          , strconst
                          , charconst
                          , regex
                          , xxx
                          ]
                        where
                        rubycmt                = ("comment",    "#(.{\\}\\{)*" )
                        regex                  = ("strconst",   "/([^/\\\\\n\r]|\\\\.)*/"    )

-- ------------------------------------------------------------

pplRE                   :: Regex
pplRE                   = buildRegex
                          [ ws
                          , ("comment",         "(-)- .*"       )
                          , ("keyword",         buildKeywords
                                                [ "and"
                                                , "begin"
                                                , "div" , "do"
                                                , "else" , "elseif" , "endif" , "endwhile" , "end"
                                                , "function"
                                                , "if"
                                                , "max" , "min" , "mod"
                                                , "not"
                                                , "of" , "or"
                                                , "procedure"
                                                , "repeat"
                                                , "return"
                                                , "then"
                                                , "until"
                                                , "var"
                                                , "while"
                                                , "xor"
                                                ]
                            )
                          , ("typekeyword",     buildKeywords
                                                [ "boolean"
                                                , "false" , "float"
                                                , "int"
                                                , "list"
                                                , "picture"
                                                , "string"
                                                , "true"
                                                ]
                            )
                          , ("varname",        "[A-Za-z_][a-zA-Z0-9_]*"         )
                          , strconst
                          , number
                          , xxx
                          ]

-- ------------------------------------------------------------

pplassRE                :: Regex
pplassRE                = buildRegex
                          [ ws
                          , ("comment",         "(-)- .*"       )
                          , ("keyword",         buildKeywords
                                                [ "loadi" , "loadf"
                                                , "loads" , "emptyl"
                                                , "undef" , "load"
                                                ]
                            )
                          , ("typename",        buildKeywords
                                                [ "store"
                                                , "pop"
                                                ]
                            )
                          , ("typekeyword",     buildKeywords
                                                [ "jmp"
                                                , "brfalse"
                                                , "brtrue"
                                                , "pushj"
                                                , "popj"
                                                , "svc"
                                                ]
                            )
                          , ("labelname",       "(l[0-9]+:?)|([se]?_[A-Za-z0-9]*:?)"    )
                          , ("varname",        "[A-Za-z_][a-zA-Z0-9_]*"         )
                          , strconst
                          , xxx
                          ]

-- ------------------------------------------------------------
