-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.URIHandling
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   the basic state arrows for URI handling

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState.URIHandling
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf

import Control.Arrow.ArrowIO

import Control.Monad                    ( mzero
                                        , mplus )

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlState.TraceHandling

import Data.Maybe

import Network.URI                      ( URI
                                        , escapeURIChar
                                        , isUnescapedInURI
                                        , nonStrictRelativeTo
                                        , parseURIReference
                                        , uriAuthority
                                        , uriFragment
                                        , uriPath
                                        , uriPort
                                        , uriQuery
                                        , uriRegName
                                        , uriScheme
                                        , uriUserInfo
                                        )

import System.Directory                 ( getCurrentDirectory )

-- ------------------------------------------------------------

-- | set the base URI of a document, used e.g. for reading includes, e.g. external entities,
-- the input must be an absolute URI

setBaseURI              :: IOStateArrow s String String
setBaseURI              = setSysVar theBaseURI
                          >>>
                          traceValue 2 (("setBaseURI: new base URI is " ++) . show)

-- | read the base URI from the globale state

getBaseURI              :: IOStateArrow s b String
getBaseURI              = getSysVar theBaseURI
                          >>>
                          ( ( getDefaultBaseURI
                              >>>
                              setBaseURI
                              >>>
                              getBaseURI
                            )
                            `when`
                            isA null                                -- set and get it, if not yet done
                          )

-- | change the base URI with a possibly relative URI, can be used for
-- evaluating the xml:base attribute. Returns the new absolute base URI.
-- Fails, if input is not parsable with parseURIReference
--
-- see also: 'setBaseURI', 'mkAbsURI'

changeBaseURI           :: IOStateArrow s String String
changeBaseURI           = mkAbsURI >>> setBaseURI

-- | set the default base URI, if parameter is null, the system base (@ file:\/\/\/\<cwd\>\/ @) is used,
-- else the parameter, must be called before any document is read

setDefaultBaseURI       :: String -> IOStateArrow s b String
setDefaultBaseURI base  = ( if null base
                            then arrIO getDir
                            else constA base
                          )
                          >>>
                          setSysVar theDefaultBaseURI
                          >>>
                          traceValue 2 (("setDefaultBaseURI: new default base URI is " ++) . show)
    where
    getDir _            = do
                          cwd <- getCurrentDirectory
                          return ("file://" ++ normalize cwd ++ "/")

    -- under Windows getCurrentDirectory returns something like: "c:\path\to\file"
    -- backslaches are not allowed in URIs and paths must start with a /
    -- so this is transformed into "/c:/path/to/file"

    normalize wd'@(d : ':' : _)
        | d `elem` ['A'..'Z']
          ||
          d `elem` ['a'..'z']
                        = '/' : concatMap win32ToUriChar wd'
    normalize wd'       = concatMap escapeNonUriChar wd'

    win32ToUriChar '\\' = "/"
    win32ToUriChar c    = escapeNonUriChar c

    escapeNonUriChar c  = escapeURIChar isUnescapedInURI c   -- from Network.URI


-- | get the default base URI

getDefaultBaseURI       :: IOStateArrow s b String
getDefaultBaseURI       = getSysVar theDefaultBaseURI            -- read default uri in system  state
                          >>>
                          ( ( setDefaultBaseURI ""                  -- set the default uri in system state
                              >>>
                              getDefaultBaseURI
                            )
                            `when` isA null
                          )                                         -- when uri not yet set

-- ------------------------------------------------------------

-- | remember base uri, run an arrow and restore the base URI, used with external entity substitution

runInLocalURIContext    :: IOStateArrow s b c -> IOStateArrow s b c
runInLocalURIContext f  = localSysVar theBaseURI f

-- ----------------------------------------------------------

-- | parse a URI reference, in case of a failure,
-- try to escape unescaped chars, convert backslashes to slashes for windows paths,
-- and try parsing again

parseURIReference'      :: String -> Maybe URI
parseURIReference' uri
    = parseURIReference uri
      `mplus`
      ( if unesc
        then parseURIReference uri'
        else mzero
      )
    where
    unesc       = not . all isUnescapedInURI $ uri

    escape '\\' = "/"
    escape c    = escapeURIChar isUnescapedInURI c

    uri'        = concatMap escape uri

-- | compute the absolut URI for a given URI and a base URI

expandURIString :: String -> String -> Maybe String
expandURIString uri base
    = do
      base' <- parseURIReference' base
      uri'  <- parseURIReference' uri
      --  abs' <- nonStrictRelativeTo uri' base'
      let abs' =  nonStrictRelativeTo uri' base'
      return $ show abs'

-- | arrow variant of 'expandURIString', fails if 'expandURIString' returns Nothing

expandURI               :: ArrowXml a => a (String, String) String
expandURI
    = arrL (maybeToList . uncurry expandURIString)

-- | arrow for expanding an input URI into an absolute URI using global base URI, fails if input is not a legal URI

mkAbsURI                :: IOStateArrow s String String
mkAbsURI
    = ( this &&& getBaseURI ) >>> expandURI

-- | arrow for selecting the scheme (protocol) of the URI, fails if input is not a legal URI.
--
-- See Network.URI for URI components

getSchemeFromURI        :: ArrowList a => a String String
getSchemeFromURI        = getPartFromURI scheme
    where
    scheme = init . uriScheme

-- | arrow for selecting the registered name (host) of the URI, fails if input is not a legal URI

getRegNameFromURI       :: ArrowList a => a String String
getRegNameFromURI       = getPartFromURI host
    where
    host = maybe "" uriRegName . uriAuthority

-- | arrow for selecting the port number of the URI without leading \':\', fails if input is not a legal URI

getPortFromURI          :: ArrowList a => a String String
getPortFromURI          = getPartFromURI port
    where
    port = dropWhile (==':') . maybe "" uriPort . uriAuthority

-- | arrow for selecting the user info of the URI without trailing \'\@\', fails if input is not a legal URI

getUserInfoFromURI              :: ArrowList a => a String String
getUserInfoFromURI              = getPartFromURI ui
    where
    ui = reverse . dropWhile (=='@') . reverse . maybe "" uriUserInfo . uriAuthority

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPathFromURI          :: ArrowList a => a String String
getPathFromURI          = getPartFromURI uriPath

-- | arrow for computing the query component of an URI, fails if input is not a legal URI

getQueryFromURI         :: ArrowList a => a String String
getQueryFromURI         = getPartFromURI uriQuery

-- | arrow for computing the fragment component of an URI, fails if input is not a legal URI

getFragmentFromURI      :: ArrowList a => a String String
getFragmentFromURI      = getPartFromURI uriFragment

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPartFromURI          :: ArrowList a => (URI -> String) -> a String String
getPartFromURI sel
    = arrL (maybeToList . getPart)
      where
      getPart s = do
                  uri <- parseURIReference' s
                  return (sel uri)

-- ------------------------------------------------------------
