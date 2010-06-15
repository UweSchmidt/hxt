-- |
-- DTDtoHXml - A program for generating access functions for the Haskell XML Toolbox
-- from a DTD
--
-- Author : Uwe Schmidt
--
-- Version : $Id: DTDtoHXT.hs,v 1.2 2005/04/14 12:52:51 hxml Exp $
--
-- this program may be used as example main program for the
-- Haskell XML Toolbox

module Main
where

import Text.XML.HXT.Parser              -- import all stuff for parsing, validating, and transforming XML

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe
import Data.Char
import Data.List

-- ------------------------------------------------------------

-- |
-- the main program

main :: IO ()
main
    = do
      argv <- getArgs                                   -- get the commandline arguments
      al   <- cmdlineOpts argv                          -- and evaluate them, return a key-value list
      res  <- run'                                      -- build a XML root from the list and start parsing
              $ getDocumentAndGenerateHaskellCode al emptyRoot
      exitProg (null res)                               -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

uppercaseInitials, namespaceAware, prefixUnderline      :: String

uppercaseInitials       = "uppercase-initials"
namespaceAware          = "namespace-aware"
prefixUnderline         = "prefix-underline"

-- ------------------------------------------------------------

generateHXmlToolboxAccessFuntions       :: XmlFilter
generateHXmlToolboxAccessFuntions t
    = processChildren (isXDTD `guards` genHaskell) t
    where
    genHaskell dt
        = cat
          [ code [ sepl
                 , "--"
                 , "-- don't edit this module"
                 , "-- generated with " ++ progName
                 , "-- simple access function for Haskell XML Toolbox"
                 , "-- generated from DTD of document: " ++ show source
                 , ""
                 , "module " ++ hsmodul
                 , "where"
                 , ""
                 , "import Text.XML.HXT.DOM.XmlTree (XmlFilter)"
                 , "import qualified Text.XML.HXT.DOM.XmlTree as X hiding (XmlFilter)"
                 ]

          , comm "namespace declarations"
          , code (map nsType nsPrefixes)
          , comm ""
          , code (zipWith nsDef nsPrefixes nsUris)

          , comm "element names (tag names)"
          , code (map (constNameType maxElemLen "tag") elements)
          , comm ""
          , code (map (constNameDef  maxElemLen "tag") elements)

          , comm "isTag test filter"
          , code (map isTagType elements)
          , comm ""
          , code (map isTagDef  elements)

          , comm "make tag nodes constructor filter"
          , code (map mkTagType elements)
          , comm ""
          , code (map mkTagDef  elements)

          , comm "attribute names"
          , code (map (constNameType maxAttrLen "attr") attributes)
          , comm ""
          , code (map (constNameDef  maxAttrLen "attr") attributes)

          , comm "get attribute value access filter"
          , code (map getValueType attributes)
          , comm ""
          , code (map getValueDef  attributes)

          , comm "has attribute test filter"
          , code (map hasAttrType attributes)
          , comm ""
          , code (map hasAttrDef  attributes)

          , comm "make attribute nodes constructor filter for computed attribute values"
          , code (map mkAttrType attributes)
          , comm ""
          , code (map mkAttrDef attributes)
          , comm "make attribute nodes constructor filter for string attribute values"
          , code (map mkSAttrType attributes)
          , comm ""
          , code (map mkSAttrDef attributes)

          , comm ("end of module " ++ hsmodul)

          ]
          $ dt
        where
        code    = txt . concatMap (++ "\n")

        comm cm = code [ "", sepl, "--", "-- " ++ cm, ""]               -- generate a headline comment

        sepl    = "-- ----------------------------------------"

        nn      = trInitial . concatMap nc                              -- normalize names

        nc c
            | c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"    = [c]
            | c == ':' || c == '-'                                      = "_"
            | otherwise                                                 = ("_" ++) . show . fromEnum $ c

        trInitial str
            | null str  = str
            | underLn   = '_' : str
            | upperCs   = toUpper (head str) : tail str
            | otherwise = str

        modname = reverse
                  . (\ n -> if '.' `elem` n                             -- remove extension
                            then drop 1 . dropWhile (/= '.') $ n
                            else n
                    )
                  . takeWhile (/= '/')                                  -- remove dir path
                  . reverse

        maxLen          = maximum . (0 :) . map length

        padd n x        = take n (x ++ repeat ' ')

        tagPrefix       = "tag"
        attrPrefix      = "attr"
        nsPrefix        = "ns"

        tagName n       = tagPrefix  ++ nn n
        attName a       = attrPrefix ++ nn a
        nspName n       = nsPrefix   ++ nn n

        constNameType ln prefix n
            = prefix ++ padd ln (nn n) ++ " :: String"

        constNameDef ln prefix n
            = prefix ++ padd ln (nn n) ++ " =  " ++ show n

        isTagType n
            = "is" ++ padd maxElemLen (nn n) ++ " :: XmlFilter"

        isTagDef n
            = "is" ++ padd maxElemLen (nn n) ++ " =  " ++
              ( either
                (const ("X.isTag " ++ tagPrefix ++ nn n))
                (\ (ns, pr, lp)
                 -> "X.isNsTag " ++ ( if null pr
                                    then tagName n
                                    else show lp
                                  ) ++ " " ++ nspName ns
                )
                . quName
                $ n
              )

        mkTagType n
            = "t" ++ padd maxElemLen (nn n) ++ " :: XmlFilter"

        mkTagDef n
            = "t" ++ padd maxElemLen (nn n) ++ " = " ++
              ( either
                (const ("X.etag " ++ tagName n))
                (\ (ns, _pr, _lp)
                 -> "X.mkXNsTag " ++ tagName n ++ " " ++ nspName ns ++ " X.none X.none"
                )
                . quName
                $ n
              )

        hasAttrType n
            = "has" ++ padd maxAttrLen (nn n) ++ " :: XmlFilter"

        hasAttrDef n
            = "has" ++ padd maxAttrLen (nn n) ++ " = " ++
              ( either
                (const ("X.hasAttr " ++ attName n))
                (\ (ns, _pr, lp)
                 -> "X.hasNsAttr " ++ show lp ++ " " ++ nspName ns
                )
                . quNameAttr
                $ n
              )

        mkAttrType n
            = "af" ++ padd maxAttrLen (nn n) ++ " :: XmlFilter -> XmlFilter"

        mkAttrDef n
            = "af" ++ padd maxAttrLen (nn n) ++ " = " ++
              ( either
                (const ("X.mkXAttr " ++ attName n))
                (\ (ns, _pr, _lp)
                 -> "X.mkXNsAttr " ++ attName n ++ " " ++ nspName ns
                )
                . quNameAttr
                $ n
              )

        mkSAttrType n
            = "a" ++ padd maxAttrLen (nn n) ++ " :: String -> XmlFilter"

        mkSAttrDef n
            = "a" ++ padd maxAttrLen (nn n) ++ " = \\ v -> " ++
              ( either
                (const ( "X.mkXAttr " ++ attName n ++ " (X.txt v)" ))
                (\ (ns, _pr, _lp)
                 -> "X.mkXNsAttr " ++ attName n ++ " " ++ nspName ns ++ " (X.txt v)"
                )
                . quNameAttr
                $ n
              )

        getValueType n
            = "get" ++ padd maxAttrLen (nn n) ++ " :: XmlFilter"

        getValueDef n
            = "get" ++ padd maxAttrLen (nn n) ++ " =  " ++
              ( either
                (const ("X.getValue " ++ attName n))
                (\ (ns, _pr, lp)
                 -> "X.getNsValue " ++ show lp ++ " " ++ nspName ns
                )
                . quNameAttr
                $ n
              )

        nsType n
            = nsPrefix ++ padd maxNsLen (nn n) ++ " :: String"

        nsDef n v
            = nsPrefix ++ padd maxNsLen (nn n) ++ " =  " ++ show v

        source  = xshow . getValue a_source                     $ t
        modul   = modname . xshow . getValue a_output_file      $ t
        root    = xshow . (isDoctype .> getDTDValue a_name)     $ dt
        hsmodul
            | null modul        = "DTD" ++ nn root
            | otherwise         = modul

        upperCs = satisfies (hasAttr uppercaseInitials) $ t
        underLn = satisfies (hasAttr prefixUnderline)   $ t
        nsAware = satisfies (hasAttr namespaceAware)            $ t

        elements                                                        -- select all element names from dtd
            = nub
              . sort
              . map ( xshow . this)
              . (deep isElement .> getDTDValue a_name)
              $ dt

        maxElemLen = maxLen . map nn $ elements

        attributes
            = nub
              . sort
              . map ( xshow . this)
              . (deep isAttlist .> getDTDValue a_value)
              $ dt

        maxAttrLen = maxLen . map nn $ attributes

        nsName [] = root
        nsName n  = n

        namespaces
            = map (\ (n,v) -> (drop 6 n, v))
              . map (\ t' -> (xshow . getDTDValue a_value $ t', xshow . getDTDValue a_default $ t'))
              . isNSAttr
              $ dt

        nsPrefixes = map (nsName . fst) namespaces
        nsUris     = map snd namespaces

        maxNsLen = maxLen . map nn $ nsPrefixes

        quName  :: String -> Either String (String, String, String)
        quName  n
            | not nsAware                = Left n
            | null lp && isNothing defns = Left n
            | null lp                    = Right (nsName "", "", pr)
            |            isNothing expns = Left n
            |            otherwise       = Right (nsName pr, pr, tail lp)
              where
              (pr, lp) = span (/= ':') n
              defns    = lookup "" namespaces
              expns    = lookup pr namespaces

        quNameAttr      :: String -> Either String (String, String, String)
        quNameAttr an
            | ':' `elem` an     = quName an
            | otherwise         = Left an

        isNSAttr        :: XmlFilter
        isNSAttr
            = deep isAttlist
              .>
              ( ( getDTDValue a_value
                  .>
                  isOfText (\ s' -> s' == "xmlns" || "xmlns:" `isPrefixOf` s')
                )
                `guards`
                ( isFixedAttrKind `guards` this)
              )

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- get wellformed document, validates document, propagates and check namespaces
-- and controls output

getDocumentAndGenerateHaskellCode       :: Attributes -> XmlStateFilter state
getDocumentAndGenerateHaskellCode al
    = parseDocument ( al
                      ++
                      [ (a_parse_html, v_0)
                      , (a_canonicalize, v_0)
                      , (a_remove_whitespace, v_0)
                      ]
                    )
      .>>
      traceMsg 1 "start processing"
      .>>
      liftMf generateHXmlToolboxAccessFuntions
      .>>
      traceMsg 1 "processing finished"
      .>>
      traceSource
      .>>
      traceTree
      .>>
      writeDocument [ (a_output_xml, v_0)
                    ]
      .>>
      checkStatus

-- ------------------------------------------------------------
--
-- the boring option definition and evaluation part
--
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "DTDtoHXml"

options         :: [OptDescr (String, String)]
options
    = selectOptions [ a_help
                    ] generalOptions
      ++
      selectOptions [ a_trace
                    , a_proxy
                    , a_use_curl
                    , a_options_curl
                    , a_encoding
                    , a_validate
                    , a_check_namespaces
                    ] inputOptions
      ++
      selectOptions [ a_output_file
                    ] outputOptions
      ++
      [ Option "u"      [prefixUnderline]       (NoArg  (prefixUnderline,   v_1))       "separate tag and attribute names with a '_'"
      , Option "U"      [uppercaseInitials]     (NoArg  (uppercaseInitials, v_1))       "transform the first char of tag and attribute names to uppercase"
      , Option "N"      [namespaceAware]        (NoArg  (namespaceAware,    v_1))       "filter are namespace aware, if namespace attributes occur in the DTD"
      ]
      ++
      showOptions

usage           :: [String] -> IO a
usage errl
    | null errl
        = do
          hPutStrLn stdout use
          exitProg False
    | otherwise
        = do
          hPutStrLn stderr (concat errl ++ "\n" ++ use)
          exitProg True
    where
    header = "DTDtoHXml - Generation of access function for the Haskell XML Toolbox from a DTD\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
          -> do
             sa <- src n
             help (lookup a_help ol)
             return (ol ++ sa)
      (_,_,errs)
          -> usage errs
    where
    src []      = return [(a_source, "")]
    src [uri]   = return [(a_source, uri)]
    src _       = usage  ["only one input url or file allowed\n"]

    help Nothing        = return ()
    help (Just _)       = usage []

-- ------------------------------------------------------------
