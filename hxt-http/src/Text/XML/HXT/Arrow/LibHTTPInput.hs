-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.LibCurlInput
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   libcurl input
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.LibHTTPInput
    ( getHTTPNativeContents
    , withHTTP
    , httpOptions
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import qualified Data.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8     as C

import System.Console.GetOpt

import Text.XML.HXT.Arrow.DocumentInput         ( addInputError )
import Text.XML.HXT.IO.GetHTTPNative            ( getCont )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ----------------------------------------------------------

getHTTPNativeContents      :: IOSArrow XmlTree XmlTree
getHTTPNativeContents
    = getC
      $<<
      ( getAttrValue transferURI
        &&&
        getSysVar (theInputOptions .&&&.
                   theProxy        .&&&.
                   theStrictInput  .&&&.
		   theRedirect
                  )
      )
      where
      getC uri (options, (proxy, (strictInput, redirect)))
          = applyA ( ( traceMsg 2 ( "get HTTP via native HTTP interface, uri=" ++ show uri ++ " options=" ++ show options )
                       >>>
                       arrIO0 (getCont strictInput proxy uri redirect options)
                     )
                     >>>
                     ( arr (uncurry addInputError)
                       |||
                       arr addContent
                     )
                   )

addContent        :: (Attributes, B.ByteString) -> IOSArrow XmlTree XmlTree
addContent (al, bc)
    = replaceChildren (txt $ C.unpack bc)       -- add the contents, TODO eliminate unpack
      >>>
      seqA (map (uncurry addAttr) al)           -- add the meta info (HTTP headers, ...)

-- ------------------------------------------------------------

a_use_http              :: String
a_use_http              = "use-http"

withHTTP               :: Attributes -> SysConfig
withHTTP httpOpts      = setS theHttpHandler getHTTPNativeContents
                         >>>
                         withInputOptions httpOpts

httpOptions            :: [OptDescr SysConfig]
httpOptions            = [ Option "" [a_use_http]  (NoArg (withHTTP []))  "enable HTTP input with native Haskell HTTP package" ]

-- ------------------------------------------------------------

