-- ------------------------------------------------------------

{- |
   Module     : RelaxSchemaToXmlTree
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Generation of a Haskell module representing a DOM for a given Relax NG schema

-}

-- ------------------------------------------------------------

-- |
-- Creates the simple form of the Relax NG specification grammar
-- with a grammar-pattern as the start-element
-- and prints it to stdout.
--

module Main
where

import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Core

import System.Exit
import System.Environment

import Data.Function    ( on )
import Data.List        ( nub, sort, sortBy )
import Data.Maybe

-- ------------------------------------------------------------

main :: IO String
main
  = do
    argv <- getArgs
    [rc]  <- runX $ writeSpecification $ if length argv > 0
                                         then head argv
                                         else relaxSchemaGrammarFile
    exitProg (rc >= c_err)



writeSpecification :: String -> IOSArrow b Int
writeSpecification schemaName
  -- = readDocument [withValidate no] (schemaName ++ ".rng")
  = readForRelax (schemaName ++ ".rng")
    >>>
    setTraceLevel 1
    >>>
    resetStates
    >>>
    createSimpleForm True True True
    >>>
    fromLA (xmlTree2Arrow schemaName)
    >>>
    traceDoc "Haskell arrow expr"
    >>>
    writeDocument [withOutputPLAIN] (schemaName ++ ".hs")
    >>>
    getErrStatus


exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess

xmlTree2Arrow           :: String -> LA XmlTree XmlTree
xmlTree2Arrow mn
    = replaceChildren ( txt (moduleHeader mn)
                        <+>
                        ( listA (xmlTree2ArrowStr 2 $< listA (getChildren >>> getQNames) )
                          >>>
                          arr concat
                          >>>
                          mkText
                        )
                      )

getQNames       :: LA XmlTree QName
getQNames
    = getQNames'
      >>>
      getQName
    where
    getQNames'
        = multi ( isElem <+> isAttr
                  <+>
                  ( isElem `guards` (getAttrl >>> getQNames' ) )
                )

xmlTree2ArrowStr        :: Int -> [QName] -> LA XmlTree String
xmlTree2ArrowStr indent qNames
    = constA (linebreak indent "let" ++ nsDefs ++ qnDefs ++ linebreak indent "in" ++ linebreak indent "")
      <+>
      xmlTree2ArrowStr' (indent + 2)

    where

    qnms  = nub . sortBy (compare `on` qualifiedName) $ qNames
    qnEnv = zip qnms (map (("qn" ++) . show) [(1::Int)..])
    nsEnv = zip (filter (not . null) . nub . sort . map namespaceUri $ qnms) (map (("ns" ++) . show) [(1::Int)..])

    qnDefs = concatMap qn2Str qnEnv
    qn2Str (qn, qnid)
        = linebreak indent "" ++ qnid        ++ "       = "           ++ qname qn ++
          linebreak indent "" ++ "mkelem_" ++ qnid ++ " = mkElement " ++ qnid     ++
          linebreak indent "" ++ "mkattr_" ++ qnid ++ " = mkAttr    " ++ qnid

    nsDefs = concatMap ns2Str nsEnv
    ns2Str (ns, nsid) = linebreak indent "" ++ nsid ++ " = " ++ show ns

    linebreak   :: Int -> String -> String
    linebreak ind del
        = "\n" ++ replicate ind ' ' ++ del

    qname       :: QName -> String
    qname qn
        | null ns       = "mkSNsName " ++ show nm
        | otherwise     = "mkNsName "  ++ show nm ++ " " ++ fromJust (lookup ns nsEnv)
        where
        ns = namespaceUri qn
        nm = qualifiedName qn

    qname'      :: QName -> String
    qname' qn
        = fromJust . lookup qn $ qnEnv

    xmlTree2ArrowStr'   :: Int -> LA XmlTree String
    xmlTree2ArrowStr' indent'
        = choiceA
          [ isRoot      :-> ( root2AExpr                  <+> body2AExpr )
          , isElem      :-> ( qname2AExpr <+> attrl2AExpr <+> body2AExpr )
          , isAttr  :-> attr2AExpr
          , isText      :-> ( getText >>> arr (("mkText " ++) . show) )
          -- , isCmt    :-> ( getCmt  >>> arr (("mkCmt "  ++) . show) )
          , this        :-> none
          ]
        where

        root2AExpr      :: LA XmlTree String
        root2AExpr
            = constA "mkRoot []"

        qname2AExpr     :: LA XmlTree String
        qname2AExpr
            = getQName >>> arr (("mkelem_" ++) . qname')

        body2AExpr      :: LA XmlTree String
        body2AExpr      = expr2AExpr getChildren

        attrl2AExpr     :: LA XmlTree String
        attrl2AExpr     = expr2AExpr getAttrl

        expr2AExpr      :: LA XmlTree XmlTree -> LA XmlTree String
        expr2AExpr get
            = ifA get
                ( (get >>. zip ("[ " : repeat ", ") >>> arr2 a2AExpr )
                  <+>
                  constA (linebreak indent' "]")
                )
                ( constA " []" )
            where
            a2AExpr del t
                = linebreak indent' del ++ concat (runLA (xmlTree2ArrowStr' (indent' + 2)) t)

        attr2AExpr      :: LA XmlTree String
        attr2AExpr
            = getQName &&& xshow getChildren >>> arr2 a2AExpr
            where
            a2AExpr qn val
                = "mkattr_" ++ qname' qn ++ " [ mkText " ++ show val ++ " ]"

moduleHeader    :: String -> String
moduleHeader mn
    = unlines
      [ "{-# OPTIONS_GHC -fno-warn-unused-binds #-}"
      , ""
      , "{" ++ "- |"
      , "   Module     : Text.HXT.RelaxNG." ++ mn
      , ""
      , "   Don't edit this module, it's generated by RelaxSchemaToXmlTree"
      , ""
      , "-}"
      , ""
      , "module Text.XML.HXT.RelaxNG." ++ mn
      , "    ( relaxSchemaTree, relaxSchemaArrow )"
      , "where"
      , ""
      , "import Text.XML.HXT.DOM.TypeDefs"
      , "import Text.XML.HXT.DOM.XmlNode (mkRoot, mkElement, mkAttr, mkText)"
      , ""
      , "import Control.Arrow.ListArrows"
      , ""
      , "relaxSchemaArrow :: ArrowList a => a b XmlTree"
      , "relaxSchemaArrow = constA relaxSchemaTree"
      , ""
      , "relaxSchemaTree :: XmlTree"
      ]
      ++
      "relaxSchemaTree ="
