import Debug.Trace
import Text.XML.HXT.Core

trc msg x = traceShow (msg,x) $ x

main :: IO ()
main = do
  let str = "<literal classes='hs'>Blah</literal>"
      transf = getChildren
               >>> getText
               >>> arr (const "<div> x <span> blah blah </span></div>")
               >>> hread
               >>> arr (trc "hread res=")
  [xs] <- runX
          (readString [ withValidate no, withSubstDTDEntities no ] str
           >>> processChildren (transf `when` isElem)
           >>> writeDocumentToString []
          )
  putStrLn xs
