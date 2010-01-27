{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-----------------------------------------------------------
 Daan Leijen (c) 1999-2000, daan@cs.uu.nl

 $version: 23 Feb 2000, release version 0.2$

 Parsec, the Fast Monadic Parser combinator library. 
 http://wwww.cs.uu.nl/~daan/parsec.html

 Inspired by:

    Graham Hutton and Erik Meijer:
    Monadic Parser Combinators.
    Technical report NOTTCS-TR-96-4. 
    Department of Computer Science, University of Nottingham, 1996. 
    http://www.cs.nott.ac.uk/Department/Staff/gmh/monparsing.ps

 and:
 
    Andrew Partridge, David Wright: 
    Predictive parser combinators need four values to report errors.
    Journal of Functional Programming 6(2): 355-364, 1996
-----------------------------------------------------------}

module Network.Server.HWS.Parser ( 
             --operators: label a parser, alternative
               (<?>), (<|>)

             --basic types
             , Parser, parse, parseFromFile

             , ParseError, errorPos, errorMessages             
             , SourcePos, sourceName, sourceLine, sourceColumn             
             , SourceName, Source, Line, Column             
             , Message(SysUnExpect,UnExpect,Expect,Message)
             , messageString, messageCompare, messageEq, showErrorMessages

             --general combinators  
             , skipMany, skipMany1      
             , many, many1, manyTill
             , sepBy, sepBy1
             , count
             , chainr1, chainl1
             , option, optional
             , choice, between
             , oneOf, noneOf
             , anySymbol
             , notFollowedBy

             --language dependent character parsers           
             , letter, alphaNum, lower, upper, newline, tab
             , digit, hexDigit, octDigit
             , space, spaces 
             , char, anyChar 
             , string
             , eof

             --primitive
             , satisfy
             , try
             , token --obsolete, use try instead
             , pzero, onFail, unexpected

             , getPosition, setPosition
             , getInput, setInput

             , getState, setState
             ) where

import Monad
import Char

import Network.Server.HWS.ParseError

-----------------------------------------------------------
-- Operators:
-- <?>  gives a name to a parser (which is used in error messages)
-- <|>  is the choice operator
-----------------------------------------------------------
infix  0 <?>
infixr 1 <|>


(<?>) :: Parser a -> String -> Parser a
p <?> msg           = onFail p msg

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2           = mplus p1 p2


-----------------------------------------------------------
-- Character parsers
-----------------------------------------------------------
spaces :: Parser ()
spaces              = skipMany space       <?> "white space"
space :: Parser Char
space               = satisfy (isSpace)     <?> "space"

newline :: Parser Char
newline             = char '\n'             <?> "new-line"
tab :: Parser Char
tab                 = char '\t'             <?> "tab"

upper :: Parser Char
upper               = satisfy (isUpper)     <?> "uppercase letter"
lower :: Parser Char
lower               = satisfy (isLower)     <?> "lowercase letter"
alphaNum :: Parser Char
alphaNum            = satisfy (isAlphaNum)  <?> "letter or digit"
letter :: Parser Char
letter              = satisfy (isAlpha)     <?> "letter"
digit :: Parser Char
digit               = satisfy (isDigit)     <?> "digit"
hexDigit :: Parser Char
hexDigit            = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit :: Parser Char
octDigit            = satisfy (isOctDigit)  <?> "octal digit"


-- char c              = satisfy (==c)  <?> show [c]
char :: Char -> Parser Char
char c              = do{ string [c]; return c}  <?> show [c]        
anyChar :: Parser Char
anyChar             = anySymbol

-- string :: String -> Parser String
-- string is defined later as a primitive for speed reasons.


-----------------------------------------------------------
-- General parser combinators
-----------------------------------------------------------
noneOf :: [Char] -> Parser Char
noneOf cs           = satisfy (\c -> not (c `elem` cs))
oneOf :: [Char] -> Parser Char
oneOf cs            = satisfy (\c -> c `elem` cs)

anySymbol :: Parser Char
anySymbol           = satisfy (const True)


choice :: [Parser a] -> Parser a
choice ps           = foldr (<|>) mzero ps

option :: a -> Parser a -> Parser a
option x p          = p <|> return x

optional :: Parser a -> Parser ()
optional p          = do{ p; return ()} <|> return ()

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p
                    = do{ open; x <- p; close; return x }
                
                
skipMany,skipMany1 :: Parser a -> Parser ()
skipMany1 p         = do{ p; skipMany p }
skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()

many1,many :: Parser a -> Parser [a]
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }

many p              = scan id
                    where
                      scan f    = do{ x <- p
                                    ; scan (\t -> f (x:t))
                                    }
                                <|> return (f [])

sepBy1,sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }

count :: Int -> Parser a -> Parser [a]
count n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)


--chainr p op x       = chainr1 p op <|> return x
--chainl p op x       = chainl1 p op <|> return x

chainr1,chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x

chainr1 p op        = scan
                    where
                      scan      = do{ x <- p; rest x }

                      rest x    = do{ f <- op
                                    ; y <- scan
                                    ; return (f x y)
                                    }
                                <|> return x


-----------------------------------------------------------
-- Tricky combinators
-----------------------------------------------------------
eof :: Parser ()
eof                 = notFollowedBy anySymbol <?> "end of input"   

notFollowedBy :: Parser Char -> Parser ()   
notFollowedBy p     = try (do{ c <- p; unexpected (show [c]) }
                           <|> return ()
                          )

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end      = scan
                    where
                      scan  = do{ end; return [] }
                            <|>
                              do{ x <- p; xs <- scan; return (x:xs) }


-- lookAhead :: Parser a -> Parser a
-- lookAhead p         = do{ state <- getState
--                         ; x <- p
--                         ; setState state
--                         ; return x
--                         }


-----------------------------------------------------------
-- Parser state combinators
-----------------------------------------------------------
getPosition :: Parser SourcePos
getPosition         = do{ state <- getState; return (statePos state) }

getInput :: Parser Source
getInput            = do{ state <- getState; return (stateInput state) }


setPosition :: SourcePos -> Parser ()
setPosition pos     = do{ updateState (\(State input _) -> State input pos)
                        ; return ()
                        }
                        
setInput :: Source -> Parser ()
setInput input      = do{ updateState (\(State _ pos)   -> State input pos)
                        ; return ()
                        }

getState :: Parser State
getState            = updateState id

setState :: State -> Parser State
setState state      = updateState (const state)

-----------------------------------------------------------
-- Parser definition.
-----------------------------------------------------------
data Parser a    = Parser (State -> Consumed (Reply a))

runP :: Parser t -> State -> Consumed (Reply t)
runP (Parser p)     = p

data Consumed a     = Consumed a                --input is consumed
                    | Empty !a                  --no input is consumed

data Reply a        = Ok !a !State ParseError   --parsing succeeded with @a@
                    | Error ParseError          --parsing failed

data State          = State { stateInput :: !Source
                            , statePos   :: !SourcePos
                            }
type Source         = String

setExpectError :: String -> ParseError -> ParseError
setExpectError msg err  = setErrorMessage (Expect msg) err

sysUnExpectError :: String -> SourcePos -> Reply a
sysUnExpectError msg pos= Error (newErrorMessage (SysUnExpect msg) pos)

unknownError :: State -> ParseError
unknownError state      = newErrorUnknown (statePos state)

-----------------------------------------------------------
-- run a parser
-----------------------------------------------------------
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- readFile fname
        ; return (parse p fname input)
        }

parse :: Parser a -> SourceName -> Source -> Either ParseError a
parse p name input
    = case parserReply (runP p (State input (initialPos name))) of
        Ok x _ _    -> Right x
        Error err   -> Left err

parserReply :: Consumed t -> t
parserReply result
    = case result of
        Consumed reply -> reply
        Empty reply    -> reply


-----------------------------------------------------------
-- Functor: fmap
-----------------------------------------------------------
instance Functor Parser where
  fmap f (Parser p)
    = Parser (\state -> 
        case (p state) of
          Consumed reply -> Consumed (mapReply reply)
          Empty    reply -> Empty    (mapReply reply)
      )
    where
      mapReply reply
        = case reply of
            Ok x state err -> let fx = f x 
                              in seq fx (Ok fx state err)
            Error err      -> Error err
           

-----------------------------------------------------------
-- Monad: return, sequence (>>=) and fail
-----------------------------------------------------------    
instance Monad Parser where
  return x
    = Parser (\state -> Empty (Ok x state (unknownError state)))   
    
  (Parser p) >>= f
    = Parser (\state ->
        case (p state) of                 
          Consumed reply1 
            -> Consumed $
               case (reply1) of
                 Ok x state1 err1 -> case runP (f x) state1 of
                                       Empty reply2    -> mergeErrorReply err1 reply2
                                       Consumed reply2 -> reply2
                 Error err1       -> Error err1

          Empty reply1    
            -> case (reply1) of
                 Ok x state1 err1 -> case runP (f x) state1 of
                                       Empty reply2 -> Empty (mergeErrorReply err1 reply2)
                                       other        -> other                                                    
                 Error err1       -> Empty (Error err1)
      )                                                              

  
  fail msg
    = Parser (\state -> 
        Empty (Error (newErrorMessage (Message msg) (statePos state))))

mergeErrorReply :: ParseError -> Reply t -> Reply t
mergeErrorReply err1 reply
  = case reply of
      Ok x state err2 -> Ok x state (mergeError err1 err2)
      Error err2      -> Error (mergeError err1 err2)


-----------------------------------------------------------
-- MonadPlus: alternative (mplus) and mzero
-----------------------------------------------------------
pzero :: Parser a
pzero = mzero

instance MonadPlus Parser where
  mzero
    = Parser (\state -> Empty (Error (unknownError state)))
 
  mplus (Parser p1) (Parser p2)
    = Parser (\state ->
        case (p1 state) of        
          Empty (Error err) -> case (p2 state) of
                                 Empty reply -> Empty (mergeErrorReply err reply)
                                 consumed    -> consumed
          other             -> other
      )
      
-----------------------------------------------------------
-- Primitive Parsers: 
--  try, satisfy, onFail, unexpected and updateState
-----------------------------------------------------------
try :: Parser a -> Parser a
try (Parser p)
    = Parser (\state@(State _ pos) ->     
        case (p state) of
          Consumed (Error err)  -> Empty (Error (setErrorPos pos err))
          Consumed ok           -> Empty ok
          empty                 -> empty
      )

token :: Parser a -> Parser a
token p --obsolete, use "try" instead
    = try p

satisfy :: (Char -> Bool) -> Parser Char
satisfy test
    = Parser (\(State input pos) -> 
        case input of
          (c:cs) | test c    -> let newpos   = updatePos pos c
                                    newstate = State cs newpos
                                in seq newpos $ seq newstate $ 
                                   Consumed (Ok c newstate (newErrorUnknown newpos))
                 | otherwise -> Empty (sysUnExpectError (show [c]) pos)
          []     -> Empty (sysUnExpectError "" pos)
      )


onFail :: Parser a -> String -> Parser a    
onFail (Parser p) msg
    = Parser (\state -> 
        case (p state) of
          Empty reply 
            -> Empty $ 
               case (reply) of
                 Error err        -> Error (setExpectError msg err)
                 Ok x state1 err  | errorIsUnknown err -> reply
                                  | otherwise -> Ok x state1 (setExpectError msg err)
          other       -> other
      )


updateState :: (State -> State) -> Parser State
updateState f 
    = Parser (\state -> Empty (Ok state (f state) (unknownError state)))

unexpected :: String -> Parser a
unexpected msg
    = Parser (\state -> Empty (Error (newErrorMessage (UnExpect msg) (statePos state))))

-----------------------------------------------------------
-- Parsers unfolded for speed: 
--  string
-----------------------------------------------------------    

{- specification of @string@:
string s            = scan s
                    where
                      scan []     = return s
                      scan (c:cs) = do{ char c <?> show s; scan cs }                      
-}

string :: String -> Parser String
string s
    = Parser (\(State input pos) -> 
       let
        ok cs             = let newpos   = updatePosString pos s
                                newstate = State cs newpos
                            in seq newpos $ seq newstate $ 
                               (Ok s newstate (newErrorUnknown newpos))

        errEof            = Error (setErrorMessage (Expect (show s))
                                     (newErrorMessage (SysUnExpect "") pos))
        errExpect c       = Error (setErrorMessage (Expect (show s))
                                     (newErrorMessage (SysUnExpect (show [c])) pos))

        walk [] cs        = ok cs
        walk _ []        = errEof
        walk (x:xs) (c:cs)| x == c        = walk xs cs
                          | otherwise     = errExpect c

        walk1 [] cs        = Empty (ok cs)
        walk1 _ []        = Empty (errEof)
        walk1 (x:xs) (c:cs)| x == c        = Consumed (walk xs cs)
                           | otherwise     = Empty (errExpect c)

       in walk1 s input)

