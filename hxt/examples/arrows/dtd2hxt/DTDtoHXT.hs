-- |
-- DTDtoHXT - A program for generating access functions for the Haskell XML Toolbox
-- from a DTD (Arrow Version)
--
-- Author : Uwe Schmidt
--
-- this program may be used as example main program for the
-- Haskell XML Toolbox

module Main
where

import Text.XML.HXT.Core                -- import all stuff for parsing, validating, and transforming XML
import Text.XML.HXT.Curl

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Char
import Data.List

-- ------------------------------------------------------------

-- |
-- the main program

main :: IO ()
main
    = do
      argv <- getArgs                                   -- get the commandline arguments
      (al, src) <- cmdlineOpts argv                             -- and evaluate them, return a key-value list
      [rc] <- runX (dtd2hxt al src)
      exitProg (rc >= c_err)

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------
--
-- options

uppercaseInitials, namespaceAware, prefixUnderline      :: String

uppercaseInitials       = "uppercase-initials"
namespaceAware          = "namespace-aware"
prefixUnderline         = "prefix-underline"

-- name prefixes

tagPrefix, attPrefix, nsPrefix, isPrefix, mkPrefix, hasPrefix, getPrefix
 , mkAttPrefix, mkSAttPrefix
 , nsDefault    :: String

tagPrefix       = "tag"
attPrefix       = "attr"
nsPrefix        = "ns"
isPrefix        = "is"
mkPrefix        = "e"
hasPrefix       = "has"
getPrefix       = "get"
mkAttPrefix     = "a"
mkSAttPrefix    = "sa"
nsDefault       = "default"

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- get wellformed document, validates document, but not canonicalize
-- (this would remove the DTD),
-- and controls output

dtd2hxt :: SysConfigList -> String -> IOSArrow b Int
dtd2hxt config src
    = configSysVars config                            -- set all global config options
      >>>
      readDocument [withCanonicalize no
                   ,withCurl []
                   ] src
      >>>
      traceMsg 1 "start processing DTD"
      >>>
      processChildren (isDTD `guards` genHXT)
      >>>
      traceMsg 1 "processing finished"
      >>>
      traceSource
      >>>
      traceTree
      >>>
      ( writeDocument [withOutputPLAIN] $< getSysAttr "output-file" )
      >>>
      getErrStatus
    where
    genHXT
        = catA $ map (>>> mkText) $
          [ getModuleName                               -- the module header
            >>>
            arr genModHead

          , constA $ comm "namespace declarations"

          , getNSAttr                                   -- namespace constants
            >>>                                         -- declared as "xmlns" or "xmlns:<ns>" attribute with #FIXED values
            arr2 genNSCode

          , constA $ comm "element arrows"

          , getElems >>. sort                           -- element processing
            >>>
            arr genElemCode

          , getAttrs >>. ( sort . nub )                 -- attribute processing
            >>>
            arr genAttrCode

          , getModuleName                               -- module footer
            >>> arr genModFoot
          ]

    -- auxiliary arrows --------------------------------------------------

    getModuleName       :: (ArrowXml a, ArrowDTD a) => a XmlTree String
    getModuleName
        = isDTDDoctype
          >>>
          getDTDAttrValue a_name
          >>>
          arr moduleName

    -- filter namespace attributes ----------------------------------------

    getNSAttr   ::  (ArrowXml a, ArrowDTD a) => a XmlTree (String, String)
    getNSAttr
        = deep isDTDAttlist
          >>>
          ( ( getDTDAttrValue a_value >>> isA (\ s -> s == "xmlns" || "xmlns:" `isPrefixOf` s)
            )
            `guards`
            ( ( getDTDAttrValue a_kind >>> isA (== k_fixed)
              )
              `guards`
              ( ( getDTDAttrValue a_value >>> arr (drop 6) )                            -- remove "xmlns:" prefix
                &&&
                getDTDAttrValue a_default
              )
            )
          )

    getElems    ::  (ArrowXml a, ArrowDTD a) => a XmlTree String
    getElems
        = deep isDTDElement
          >>>
          getDTDAttrValue a_name

    getAttrs    ::  (ArrowXml a, ArrowDTD a) => a XmlTree String
    getAttrs
        = deep isDTDAttlist
          >>>
          getDTDAttrValue a_value

    -- code generation ------------------------------------------------------------

    genModHead  :: String -> String
    genModHead rootElem
        = code [ sepl
               , "--"
               , "-- don't edit this module"
               , "-- generated with " ++ progName
               , "-- simple access function for Haskell XML Toolbox"
               , "-- generated from DTD of document: " ++ show src
               , ""
               , "module " ++ rootElem ++ " ( module " ++ rootElem ++ " )"
               , "where"
               , ""
               , "import           Text.XML.HXT.Core      (XmlTree, ArrowXml, (>>>))"
               , "import qualified Text.XML.HXT.Core as X (attr, eelem, getAttrValue, hasAttr, hasName, isElem, sattr)"
               ]

    genNSCode   :: String -> String -> String
    genNSCode prefix ns
        = code [ ns' ++ "\t:: String"
               , ns' ++ "\t=  " ++ show ns
               ]
        where
        ns' = nsPrefix ++ nn (if null prefix then nsDefault else prefix)

    genElemCode :: String -> String
    genElemCode n
        = code [ comm ("arrows for element " ++ show n)
               , tagN ++ "\t:: String"
               , tagN ++ "\t=  " ++ show n
               , ""
               , isN  ++ "\t:: ArrowXml a => a XmlTree XmlTree"
               , isN  ++ "\t=  X.isElem >>> X.hasName " ++ tagN
               , ""
               , mkN  ++ "\t:: ArrowXml a => a n XmlTree"
               , mkN  ++ "\t=  X.eelem " ++ tagN
               ]
        where
        tagN    = tagPrefix ++ nn n
        isN     = isPrefix  ++ nn n
        mkN     = mkPrefix  ++ nn n

    genAttrCode :: String -> String
    genAttrCode n
        = code [ comm ("arrows for attribute " ++ show n)
               , attN ++ "\t:: String"
               , attN ++ "\t=  " ++ show n
               , ""
               , hasN ++ "\t:: ArrowXml a => a XmlTree XmlTree"
               , hasN ++ "\t=  X.hasAttr " ++ attN
               , ""
               , getN ++ "\t:: ArrowXml a => a XmlTree String"
               , getN ++ "\t=  X.getAttrValue " ++ attN
               , ""
               , mkN  ++ "\t:: ArrowXml a => a n XmlTree -> a n XmlTree"
               , mkN  ++ "\t=  X.attr " ++ attN
               , ""
               , mksN ++ "\t:: ArrowXml a => String -> a n XmlTree"
               , mksN ++ "\t=  X.sattr " ++ attN
               ]
        where
        attN    = attPrefix ++ nn n
        hasN    = hasPrefix  ++ nn n
        getN    = getPrefix  ++ nn n ++ nn "value"
        mkN     = mkAttPrefix ++ nn n
        mksN    = mkSAttPrefix ++ nn n

    genModFoot  :: String -> String
    genModFoot rootElem
        = comm ( "end of module " ++ rootElem)

    -- string manipulation --------------------------------------------------

    code        :: [String] -> String
    code        = concatMap (++ "\n")

    comm        :: String -> String
    comm cm     =  code [ "", sepl, "--", "-- " ++ cm, ""]

    sepl        :: String
    sepl        = "-- ----------------------------------------"

    moduleName  :: String -> String
    moduleName rootElem
        = modname . (\ x -> if null x then rootElem else x) . getConfigAttr "output_file" $ config

    modname
        = (\ x -> toUpper (head x) : tail x)
          . reverse
          . (\ n -> if '.' `elem` n                             -- remove extension
                    then drop 1 . dropWhile (/= '.') $ n
                    else n
            )
          . takeWhile (/= '/')                                  -- remove dir path
          . reverse

    nn          :: String -> String
    nn
        = trInitial . concatMap nc                              -- normalize names

    nc          :: Char -> String
    nc c
        | c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"        = [c]
        | c == ':' || c == '-'                                  = "_" 
        | otherwise                                                     = ("_" ++) . show . fromEnum $ c

    trInitial   :: String -> String
    trInitial str
        | null str      = str
        | underLn       = '_' : str
        | upperCs       = toUpper (head str) : tail str
        | otherwise     = str

    upperCs, underLn {-, nsAware -}     :: Bool
    upperCs     = (== "1") . getConfigAttr uppercaseInitials    $ config
    underLn     = (== "1") . getConfigAttr prefixUnderline      $ config
    _nsAware    = (== "1") . getConfigAttr namespaceAware       $ config

-- ------------------------------------------------------------
--
-- the boring option definition and evaluation part
--
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "DTDtoHXT"
    
options         :: [OptDescr SysConfig]
options
    = selectOptions [ a_help
                    ] generalOptions
      ++
      selectOptions [ a_trace
                    , a_proxy
                    , a_encoding
                    , a_validate
                    , a_check_namespaces
                    ] inputOptions
      ++
      selectOptions [ "output-file"
                    ] outputOptions
      ++
      [ Option "u"      [prefixUnderline]       (NoArg $ withSysAttr prefixUnderline "1")       "separate tag and attribute names with a '_'"
      , Option "U"      [uppercaseInitials]     (NoArg $ withSysAttr uppercaseInitials "1")     "transform the first char of tag and attribute names to uppercase"
      , Option "N"      [namespaceAware]        (NoArg $ withSysAttr namespaceAware "1")        "filter are namespace aware, if namespace attributes occur in the DTD"
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

cmdlineOpts     :: [String] -> IO (SysConfigList, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
          -> do
             sa <- src n
             help (getConfigAttr a_help ol)
             return (ol, sa)
      (_,_,errs)
          -> usage errs
    where
    src [uri]   = return uri
    src []      = usage ["input file/uri missing"]
    src _       = usage ["only one input url or file allowed\n"]

    help "1"    = usage []
    help _      = return ()

-- ------------------------------------------------------------
