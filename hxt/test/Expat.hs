-- | A "hello world" example of hexpat that lazily parses a document, printing
-- it to standard out.

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as L

import           Text.XML.HXT.Core
import qualified Text.XML.HXT.Core                      as X
import qualified Text.XML.HXT.DOM.XmlNode               as XN
import           Text.XML.HXT.Arrow.XmlState.TypeDefs

main :: IO ()
main = do
     args <- getArgs
     case args of
         [filename] -> process filename
         _otherwise  -> do
             hPutStrLn stderr "Usage: helloworld <file.xml>"
             exitWith $ ExitFailure 1

process :: String -> IO ()
process filename = do
     inputText <- L.readFile filename

     let (xml, mErr) = parseWithExpat inputText

     -- Process document before handling error, so we get lazy processing.
     L.hPutStr stdout $ format xml
     putStrLn ""
     case mErr of
         Nothing -> return ()
         Just er -> do
             hPutStrLn stderr $ "XML parse failed: "++show er
             exitWith $ ExitFailure 2

-- ------------------------------------------------------------

parseWithExpat :: L.ByteString -> (UNode String, Maybe XMLParseError)
parseWithExpat bs
    = parse defaultParseOptions bs


uNodeStringToXmlTree :: UNode String -> XmlTree
uNodeStringToXmlTree (Element n al cl)
    = XN.mkElement (mkName n)
                   (map (\ (an, av) -> XN.mkAttr (mkName an) [XN.mkText av]) al)
                   (map uNodeStringToXmlTree cl)
uNodeStringToXmlTree (Text t)
    = XN.mkText t

-- ------------------------------------------------------------

withExpat		:: Bool -> SysConfig
withExpat strict	= setS (theExpat       .&&&.
                                theTagSoup     .&&&.
                                theExpatParser .&&&.
                                theExpatErrors
                               ) (True, (False, (parseExpat strict, none)))

withoutExpat            :: SysConfig
withoutExpat            = setS theExpat False

-- ------------------------------------------------------------

parseExpat		:: Bool -> IOSArrow XmlTree XmlTree
parseExpat strict	= traceMsg 1 ("parse document with expat parser")
                          >>>
                          replaceChildren
                          ( applyA
                            (  xshow X.getChildren
                               >>>
                               arr ( stringToByteString		-- TODO eliminate this
                                     >>>
                                     parse defaultParseOptions
                                     >>>
                                     first uNodeStringToXmlTree
                                     >>>
                                     uncurry evalRes
                                   )
                            )
                          )
    where
    evalRes t er	= constA t <+> evalErrs er

    evalErrs Nothing	= none
    evalErrs (Just (XMLParseError msg loc))
        | strict	= issueErr ("Expat error at " ++ show (xmlLineNumber loc) ++ ":" ++ show (xmlColumnNumber loc) ++ ":" ++ msg)
        | otherwise	= constA (issueErr ("Expat error at " ++ show (xmlLineNumber loc) ++ ":" ++ show (xmlColumnNumber loc) ++ ":" ++ msg))
                          >>>
                          setSysVar theExpatErrors 
                          >>>
                          none

issueExpatErr		:: IOStateArrow s b b
issueExpatErr		= withoutUserState $ perform $
                          constA undefined >>> applyA (getSysVar theExpatErrors)


-- ------------------------------------------------------------

stringToByteString	:: String -> L.ByteString
stringToByteString	= map (fromEnum >>> toEnum) >>> L.pack	-- TODO eliminate this

theExpat                :: Selector XIOSysState Bool
theExpat                = undefined

theExpatParser          :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theExpatParser          = undefined

theExpatErrors          :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theExpatErrors          = undefined

-- ------------------------------------------------------------
