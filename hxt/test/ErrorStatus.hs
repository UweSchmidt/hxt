module ErrorStatus
where

import Text.XML.HXT.Core

-- ------------------------------------------------------------

main = runX (transformDoc [] this "emil" "")

transformDoc cfg rules src dst =
    configSysVars (withTrace 4 : cfg) >>>
    readDocument  [] src >>>
    perform (getErrStatus >>> arrIO print) >>>
    rules >>> -- some transformations
    writeDocument [] dst >>>
    getErrStatus
