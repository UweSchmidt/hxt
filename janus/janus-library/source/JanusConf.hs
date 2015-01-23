-- ------------------------------------------------------------

{- |
   Module     : JanusConf
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Module for generation a Janus Main program from a Janus server configuration

   With the use of this program the hs-plugins package is not required for running Janus.
   The plugins module was always a source of trouble with ghc versions and bytesting versions.
   This program collects all modules needed by a configuration, the builtin Janus handlers and shaders
   and the application specific handlers and shaders and generated a Haskell main prog for a specific
   application server.
-}

-- ------------------------------------------------------------

module Main where

import Data.Maybe
import Data.List

import Network.Server.Janus.ServerVersion

import System.Environment
import System.Exit
import System.Locale	( rfc822DateFormat
			, defaultTimeLocale
			)
import System.Time	( formatCalendarTime
			, toCalendarTime
			, getClockTime
			)

import Text.XML.HXT.Core
import Text.XML.HXT.XPath

-- ------------------------------------------------------------

main	:: IO ()
main	= do
	  daytime <- getClockTime
                     >>= toCalendarTime
                     >>= (return . formatCalendarTime defaultTimeLocale rfc822DateFormat)
	  args <- getArgs
	  let confName = fromMaybe "./conf/server.xml" . listToMaybe $ args
	  let progName = fromMaybe "Janus.hs" . listToMaybe . drop 1 $ args
	  [rc]  <- runX (processJanusConf confName progName daytime)
	  exitProg (rc >= c_err)

-- ------------------------------------------------------------

exitProg	:: Bool -> IO a
exitProg True	= exitWith (ExitFailure 1)
exitProg False	= exitWith ExitSuccess

-- ------------------------------------------------------------

processJanusConf	:: String -> String -> String -> IOSArrow b Int
processJanusConf cn on dt
    = readDocument [withValidate no] cn
      >>>
      checkJanusServerConfig
      >>>
      genJanusMain
      >>>
      writeDocument [ withXmlPi no
		    , withOutputEncoding isoLatin1
		    , withOutputPLAIN
		    ] on
      >>>
      getErrStatus
    where

    checkJanusServerConfig	:: IOSArrow XmlTree XmlTree
    checkJanusServerConfig
	= ( perform ( checkHandlersUsed
		      <+>
		      checkShadersUsed
		    )
	    >>>
	    setDocumentStatusFromSystemState "check Janus server configuration"
	  )
          `when` documentStatusOk

    genJanusMain		:: IOSArrow XmlTree XmlTree
    genJanusMain
	= fromLA (processChildren (processDocument cn dt))
	  `when`
	  documentStatusOk

    checkUsed		:: String -> String -> String -> IOSArrow XmlTree XmlTree
    checkUsed def use emsg
	= chk $< listA (getNames def)
	where
	chk dhl
	    = getNames use
	      >>>
	      isA (not . (`elem` dhl))
	      >>>
	      arr ((emsg ++) . show)
	      >>>
	      errMsg
	      
    checkHandlersUsed		:: IOSArrow XmlTree XmlTree
    checkHandlersUsed
	= checkUsed "/janus/block//loadhandler/@reference/text()"
                    "/janus//handler/config/handler/config/@type/text()"
	            "handler type in a handler element without a corresponding loadhandler element "
	      
    checkShadersUsed		:: IOSArrow XmlTree XmlTree
    checkShadersUsed
	= checkUsed "/janus/block//loadshader/@reference/text()"
                    "/janus//shader/@type/text()"
                    "shader type in a shader element without a corresponding loadshader element "

errMsg		:: IOSArrow String a
errMsg		= ( (\ s -> issueErr s) $< this ) >>> none

-- ------------------------------------------------------------

processDocument		:: String -> String -> LA XmlTree XmlTree
processDocument cn dt
    = catA $
      [ txt (part1 cn dt)
      , importHandler
      , importShader
      , handlerRepo
      , shaderRepo
      , txt (part2 cn dt)
      ]

-- ------------------------------------------------------------

importHandler		:: LA XmlTree XmlTree
importHandler
    = catA $
      [ ln "-- required handler modules"
      , ( getNames "/janus/block//loadhandler/@module/text()"
	  >>^
	  ("import " ++)
	)
	>>> mkLn
      , ln ""
      ]

importShader		:: LA XmlTree XmlTree
importShader
    = catA $
      [ ln "-- required shader modules"
      , ( getNames "/janus/block//loadshader/@module/text()"
	  >>^
	  ("import " ++)
	)
	>>> mkLn
      , ln ""
      ]

getNames	:: ArrowXml a => String -> a XmlTree String
getNames path
    = fromLA $
      ( getXPathTrees path
	>>>
	( getText >>^ stringTrim )
      )
      >>. (nub . sort)

handlerRepo		:: LA XmlTree XmlTree
handlerRepo
    = catA $
      [ ln "-- handler repository"
      , ln "setHandlerRepo\t:: JanusStateArrow a a"
      , ln "setHandlerRepo"
      , ln "  = seqA . map (uncurry addHandlerCreator) $"
      , ( getXPathTrees "/janus/block//loadhandler"
	  >>>
	  ( getAttrValue' "reference"
	    &&&
	    getAttrValue' "object"
	    &&&
	    getAttrValue' "module"
	  )
	  >>>
	  arr (\ (r,(o,m)) -> "( " ++ show r ++ ",\t" ++ m ++ "." ++ o ++ "\t )")
	)
        >.
	formatList 4
	>>>
	mkText
      , ln ""
      ]

shaderRepo		:: LA XmlTree XmlTree
shaderRepo
    = catA $
      [ ln "-- shader repository"
      , ln "setShaderRepo\t:: JanusStateArrow a a"
      , ln "setShaderRepo"
      , ln "  = seqA . map (uncurry addShaderCreator) $"
      , ( getXPathTrees "/janus/block//loadshader"
	  >>>
	  ( getAttrValue' "reference"
	    &&&
	    getAttrValue' "object"
	    &&&
	    getAttrValue' "module"
	  )
	  >>>
	  arr (\ (r,(o,m)) -> "( " ++ show r ++ ",\t" ++ m ++ "." ++ o ++ "\t )")
	)
        >.
	formatList 4
	>>>
	mkText
      , ln ""
      ]

formatList	:: Int -> [String] -> String
formatList i
    = ((indent ++ "[ ") ++) . (++ (indent' ++ "]\n")) . intercalate (indent' ++ ", ")
    where
    indent = replicate i ' '
    indent' = '\n' : indent


getAttrValue'		:: String -> LA XmlTree String
getAttrValue' an	= getAttrValue an >>^ stringTrim

-- ------------------------------------------------------------

ln	:: String -> LA b XmlTree
ln	= txt . (++ "\n")

mkLn	:: LA String XmlTree
mkLn	= (++ "\n") ^>> mkText

-- ------------------------------------------------------------

part1	:: String -> String -> String
part1 cf dt
	= unlines $
	  [ "{- |"
	  , "  Janus server main program"
	  , ""
	  , "  Do not edit this source,"
	  , "  it is generated by janus-conf"
	  , "  from server configuration in file " ++ show cf
	  , "  at " ++ dt
	  , ""
	  , "  Required Janus Library version: " ++ build_version
	  , ""
	  , "-}"
	  , ""
	  , "module Main where"
	  , ""
	  , "import Network.Server.Janus.Core"
	  , "import Network.Server.Janus.Server    ( serverArrow )"
	  , "import Network.Server.Janus.XmlHelper ( evalXml )"
	  , ""
	  , "import Text.XML.HXT.Core"
	  , ""
	  ]

part2	:: String -> String -> String
part2 cf dt
	= unlines $
	  [ ""
	  , "main :: IO ()"
	  , "main ="
	  , "  do"
	  , "  initContext <- emptyContext"
	  , "  evalXml ( constA ()"
	  , "            >>>"
	  , "            setHandlerRepo"
	  , "            >>>"
	  , "            setShaderRepo"
	  , "            >>>"
	  , "            serverArrow " ++ show dt ++ " "  ++ show cf
	  , "          ) initContext"
	  , "    >> return ()"
	  , ""
	  ]

-- ------------------------------------------------------------
