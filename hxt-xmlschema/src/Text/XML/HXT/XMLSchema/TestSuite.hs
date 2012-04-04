{- |
   Module     : Text.XML.HXT.XMLSchema.TestSuite
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

-}

module Text.XML.HXT.XMLSchema.TestSuite

  ( runTestSuite )

where

import Text.XML.HXT.XMLSchema.Validation ( validateWithSchema
                                         , SValResult
                                         )

import Test.HUnit                        ( Test (TestList)
                                         , (~:)
                                         , (@?=)
                                         , runTestTT
                                         )

-- ----------------------------------------

-- | Create a HUnit test for SValResults
mkSValTest :: String -> String -> String-> SValResult -> Test
mkSValTest label descName instName expectedRes
  = label ~:
    do
    res <- validateWithSchema ("./tests/" ++ descName ++ ".xsd") ("./tests/" ++ instName ++ ".xml")
    res @?= expectedRes

-- | A test for SimpleTypes as element values which match
simpleTypesElemsOk :: Test
simpleTypesElemsOk
  = mkSValTest "SimpleTypes inside elems ok" "simpleTypesElems" "simpleTypesElemsOk" $
    (True, [])

-- | A test for SimpleTypes as element values which do not match
simpleTypesElemsErrors :: Test
simpleTypesElemsErrors
  = mkSValTest "SimpleTypes inside elems with errors" "simpleTypesElems" "simpleTypesElemsErrors" $
    (False, [ ( "/root[1]/summerMonth[1]/child::text()"
              , "Parameter restriction: \"minExclusive = 5\" does not hold for value = \"3\".")
            , ( "/root[1]/temperature[1]/child::text()"
              , "Parameter restriction: \"totalDigits = 3\" does not hold for value = \"42\".")
            , ( "/root[1]/password[1]/child::text()"
              , "Parameter restriction: \"maxLength = 10\" does not hold for value = \"a wrong password\".")
            , ( "/root[1]/monthByName[1]/child::text()"
              , "Parameter restriction: \"length = 3\" does not hold for value = \"wrong\".")
            , ( "/root[1]/plz[1]/child::text()"
              , "Parameter restriction: \"pattern = (D )?[0-9]{5}\" does not hold for value = \"42\".")
            , ( "/root[1]/monthList[1]/child::text()"
              , "value does not match list type.")
            , ( "/root[1]/month[1]/child::text()"
              , "value does not match union type.")
            ])

-- | A test for SimpleTypes as attribute values which match
simpleTypesAttrsOk :: Test
simpleTypesAttrsOk
  = mkSValTest "SimpleTypes inside attrs ok" "simpleTypesAttrs" "simpleTypesAttrsOk" $
    (True, [])

-- | A test for SimpleTypes as attribute values which do not match
simpleTypesAttrsErrors :: Test
simpleTypesAttrsErrors
  = mkSValTest "SimpleTypes inside attrs with errors" "simpleTypesAttrs" "simpleTypesAttrsErrors" $
    (False, [ ( "/root[1]/@summerMonth"
              , "Parameter restriction: \"maxExclusive = 9\" does not hold for value = \"10\".")
            , ( "/root[1]/@temperature"
              , "Parameter restriction: \"fractionDigits = 1\" does not hold for value = \"421\".")
            , ( "/root[1]/@password"
              , "Parameter restriction: \"minLength = 5\" does not hold for value = \"safe\".")
            , ( "/root[1]/@plz"
              , "Parameter restriction: \"pattern = (D )?[0-9]{5}\" does not hold for value = \"42\".")
            , ( "/root[1]/@monthList"
              , "value does not match list type.")
            , ( "/root[1]/@month"
              , "value does not match union type.")
            ])

-- | A test for ComplexTypes which match
complexTypesOk :: Test
complexTypesOk
  = mkSValTest "ComplexTypes ok" "complexTypes" "complexTypesOk" $
    (True, [])

-- | A test for ComplexTypes which do not match
complexTypesErrors :: Test
complexTypesErrors
  = mkSValTest "ComplexTypes with errors" "complexTypes" "complexTypesErrors" $
    (False, [ ( "/root[1]/password[1]/child::text()"
              , "Parameter restriction: \"maxLength = 10\" does not hold for value = \"wrong password\".")
            , ( "/root[1]/login[1]/@username"
              , "required attribute is missing.")
            , ( "/root[1]/login[1]/child::text()"
              , "Parameter restriction: \"minLength = 5\" does not hold for value = \"foo\".")
            , ( "/root[1]/customer[1]/@age"
              , "\"wrong\" is no valid integer.")
            , ( "/root[1]/customer[1]/*"
              , "content does not match content model.\ninput does not match {single tree pred}{single tree pred}")
            , ( "/root[1]/shoppingList[1]/*"
              , "content does not match content model.\nunexpected tree NTree (XTag \"butter\" []) [NTree (XText \"1\") []]")
            , ( "/root[1]/computer[1]/*"
              , "content does not match content model.\nunexpected tree NTree (XTag \"tablet\" []) []")
            , ( "/root[1]/computerData[1]/*"
              , "content does not match content model.\nunexpected tree NTree (XTag \"ram\" []) [NTree (XText \"64GB\") []]")
            , ( "/root[1]/computerDataWithGPU[1]/*"
              , "content does not match content model.\ninput does not match " ++ 
                "{single tree pred}({single tree pred})?({single tree pred})?({single tree pred})*{single tree pred}")
            , ( "/root[1]/computerDataWithoutComments[1]/*"
              , "content does not match content model.\nunexpected tree " ++ 
                "NTree (XTag \"comment\" []) [NTree (XText \"good display!\") []]")
            ])

-- | A test for redefinitions
redefine :: Test
redefine
  = mkSValTest "Redefine" "redefine" "redefine" $
    (False, [ ( "/root[1]/validCustomer[2]/child::text()"
              , "Parameter restriction: \"maxInclusive = 18\" does not hold for value = \"21\".")
            ])

-- ----------------------------------------

-- | The hxt-xmlschema test suite
testSuite :: Test
testSuite
  = TestList [ simpleTypesElemsOk
             , simpleTypesElemsErrors
             , simpleTypesAttrsOk
             , simpleTypesAttrsErrors
             , complexTypesOk
             , complexTypesErrors
             , redefine
             ]

-- ----------------------------------------

-- | Run the hxt-xmlschema test suite
runTestSuite :: IO ()
runTestSuite
  = do
    _ <- runTestTT testSuite
    return ()

