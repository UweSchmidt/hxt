-- ------------------------------------------------------------
--
-- defaut uri computation
-- this routine is platform dependent
-- and must be exchanged when porting the toolbox e.g. to windows

module Text.XML.HXT.Parser.DefaultURI
    ( setDefaultURI
    )

where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.XmlState

import System.Directory
    ( getCurrentDirectory
    )

import Network.URI
    ( escapeURIChar
    , isUnescapedInURI
    )

setDefaultURI   :: XState state ()
setDefaultURI
     = do
       wd <- io getCurrentDirectory
       setSysParam transferDefaultURI ("file://" ++ normalize wd ++ "/")

       where

       -- under Windows getCurrentDirectory returns something like: "c:\path\to\file"
       -- backslaches are not allowed in URIs and paths must start with a /
       -- so this is transformed into "/c:/path/to/file"

       normalize wd'@(d : ':' : _)
	   | d `elem` ['A'..'Z'] || d `elem` ['a'..'z']
	       = '/' : concatMap win32ToUriChar wd'
       normalize wd'
	   = concatMap escapeNonUriChar wd'
				 
       win32ToUriChar '\\' = "/"
       win32ToUriChar c    = escapeNonUriChar c

       escapeNonUriChar c  = escapeURIChar isUnescapedInURI c   -- from Network.URI
