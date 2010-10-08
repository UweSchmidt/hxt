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

-- import qualified Data.ByteString.Lazy                   as L
import qualified Data.ByteString.Lazy.Char8             as LC
import           Text.XML.Expat.Tree

import           Text.XML.HXT.Core
import qualified Text.XML.HXT.Core                      as X
import qualified Text.XML.HXT.DOM.XmlNode               as XN
import           Text.XML.HXT.Arrow.XmlState.TypeDefs
import           Text.XML.HXT.Parser.XhtmlEntities      ( xhtmlEntities )

-- ------------------------------------------------------------

withExpat               :: Bool -> SysConfig
withExpat strict        = setS (theExpat       .&&&.
                                theTagSoup     .&&&.
                                theExpatParser .&&&.
                                theExpatErrors
                               ) (True, (False, (parseExpat strict, none)))

withoutExpat            :: SysConfig
withoutExpat            = setS theExpat False

-- ------------------------------------------------------------

parseExpat              :: Bool -> IOSArrow XmlTree XmlTree
parseExpat strict       = parse1 $<< ( getAttrValue  transferEncoding
                                       &&&
                                       getSysVar theParseHTML
                                     )
    where
    parse1 enc isHtml   = traceMsg 1 ( "parse document with expat parser, encoding is " ++
                                       show enc ++ ", issue errors is " ++ show strict ++
                                       ", HTML entity subst is " ++ show isHtml
                                     )
                          >>>
                          replaceChildren
                          ( applyA
                            (  xshow X.getChildren
                               >>>
                               arr ( LC.pack            -- TODO eliminate this
                                     >>>
                                     parse parseOptions
                                     >>>
                                     first uNodeStringToXmlTree
                                     >>>
                                     uncurry evalRes
                                   )
                            )
                          )
        where
        parseOptions
            | isHtml    = parseO { entityDecoder = Just htmlEncoder }
            | otherwise = parseO
            where
            parseO      = defaultParseOptions { overrideEncoding = expatEnc }

            htmlEncoder :: String -> Maybe String
            htmlEncoder ent
                        = fmap (toEnum >>> (:[])) . lookup ent $ xhtmlEntities

            expatEnc    = lookup enc [ (X.usAscii,   ASCII)
                                     , (X.utf8,      UTF8)
                                     , (X.utf16,     UTF16)
                                     , (X.isoLatin1, ISO88591)
                                     ]

    evalRes t er        = constA t <+> evalErrs er

    evalErrs Nothing    = none
    evalErrs (Just (XMLParseError msg loc))
        | strict        = ee
        | otherwise     = constA ee
                          >>>
                          traceMsg 1 ("set expat error: " ++ msg)
                          >>>
                          setSysVar theExpatErrors 
                          >>>
                          none
        where
        ee               = issueErr ("Expat error at " ++ show (xmlLineNumber loc) ++ ":" ++ show (xmlColumnNumber loc) ++ ":" ++ msg)

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
