-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ExpatInterface
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Internal Interface for Expat Parser

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.ExpatInterface
where

import           Text.XML.Expat.Tree

import           Text.XML.HXT.Core
import qualified Text.XML.HXT.Core                      as X
import qualified Text.XML.HXT.DOM.XmlNode               as XN
import           Text.XML.HXT.Arrow.XmlState.TypeDefs
import           Text.XML.HXT.Parser.XhtmlEntities      ( xhtmlEntities )

-- import Debug.Trace

-- ------------------------------------------------------------

{- |
   The system config option to enable the expat parser

Here is an example, how to use it:

> ...
> import Text.XML.HXT.Core
> import Text.XML.HXT.Expat
> ...
>
> readDocument [ withExpat True ] "some-file.xml"
> ...

reads the given document and parses it with the expat parser.
There is no validation enabled. The parameter to @withExpat@ determines, whether parsing
is done strict. Here strict parsing is enabled. When strict parsing is used,
the parse is immediately checked for parser errors, and possible errors are issued.
When set to non-strict parsing, error checking is delayed and may be done later
with the @issueExpatErr@ arrow.

When HTML parsing is enabled, the expat parser will be configured with the HTML enitity reference
resolver, else only the predefined XML enitities will be substituted.
-}

withExpat               :: Bool -> SysConfig
withExpat strict        = setS (theExpat       .&&&.
                                theTagSoup     .&&&.
                                theExpatParser .&&&.
                                theExpatErrors
                               ) (True, (False, (parseExpat strict, none)))

-- | Turns off expat parsing. The build in HXT parsers will be used.

withoutExpat            :: SysConfig
withoutExpat            = setS theExpat False

-- ------------------------------------------------------------

parseExpat              :: Bool -> IOSArrow XmlTree XmlTree
parseExpat strict       = parse1 $<< ( getAttrValue  transferEncoding
                                       &&&
                                       getSysVar theLowerCaseNames
                                     )
    where
    parse1 enc isHtml   = traceMsg 1 ( "parse document with expat parser, encoding is " ++
                                       show enc ++ ", issue errors is " ++ show strict ++
                                       ", HTML entity subst is " ++ show isHtml
                                     )
                          >>>
                          ( substContents $< parse2 )
        where
        substContents (t, e)
            | strict    = case e of
                          Nothing -> setChildren [t]
                          Just _  -> ee e
                                     >>>
                                     setChildren []
            | otherwise = perform ( constA (ee e)
                                    >>>
                                    traceMsg 1 "set expat error"
                                    >>>
                                    setSysVar theExpatErrors
                                    >>>
                                    none
                                  )
                          >>>
                          setChildren [t]
            where
            ee  Nothing = none
            ee (Just (XMLParseError msg loc))
                        = issueErr ("Expat error at " ++ show (xmlLineNumber loc) ++
                                    ":" ++ show (xmlColumnNumber loc) ++ ":" ++ msg
                                   )

        parse2          :: IOSArrow XmlTree (XmlTree, Maybe XMLParseError)
        parse2          = xshowBlob X.getChildren       -- expat parser wants bytestrings as input
                          >>>
                          arr ( parse parseOptions
                                >>>
                                first uNodeStringToXmlTree
                              )

        parseOptions
            | isHtml    = parseO { entityDecoder = Just htmlEncoder }
            | otherwise = parseO
            where
            parseO      = defaultParseOptions { overrideEncoding = expatEnc }

            htmlEncoder :: String -> Maybe String
            htmlEncoder ent
                        = -- traceShow ("\n" ++ ent ++ "\n") $
                          fmap (toEnum >>> (:[])) . lookup ent $ xhtmlEntities

            expatEnc    = lookup enc [ (X.usAscii,   ASCII)
                                     , (X.utf8,      UTF8)
                                     , (X.utf16,     UTF16)
                                     , (X.isoLatin1, ISO88591)
                                     ]

-- | In case of lazy parsing check for possible errors

issueExpatErr           :: IOStateArrow s b b
issueExpatErr           = withoutUserState $ perform $
                          constA undefined >>> applyA (getSysVar theExpatErrors)


-- ------------------------------------------------------------

uNodeStringToXmlTree :: UNode String -> XmlTree
uNodeStringToXmlTree (Element n al cl)
    = XN.mkElement (mkName n)
                   (map (\ (an, av) -> XN.mkAttr (mkName an) [XN.mkText av]) al)
                   (map uNodeStringToXmlTree cl)
uNodeStringToXmlTree (Text t)
    = XN.mkText t

-- ------------------------------------------------------------
