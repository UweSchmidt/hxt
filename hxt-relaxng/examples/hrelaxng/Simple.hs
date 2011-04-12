module Simple where

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG

process schema =
  runX
  ( readDocument [ withTrace 2
                 , withRelaxNG schema
                 ] "simple.xml"
    >>>
    writeDocument [withIndent yes] ""
  )

main
    = -- process "simple-unqualified.rng"
      -- >>
      process "simple-qualified.rng"
      >>
      return ()
