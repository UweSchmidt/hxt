module Main
where

import Text.XML.HXT.Arrow hiding (readDocument)
import Text.XML.HXT.Arrow.XmlCache

main = runX ( readDocument [ (a_parse_html, v_1)
                           , (a_issue_warnings, v_0)
                           , (a_remove_whitespace, v_1)
                           , (a_cache, "/tmp")
                           , (a_document_age, "60")
                           , (a_trace, "2")
                           ] "http://www.fh-wedel.de/"
              >>>
              processChildren (hasName "html" /> hasName "body" >>> deep isText)
              >>>
              writeDocument [] ""
            )
       >> return ()
