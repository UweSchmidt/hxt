-- |
-- TreeVisualisation - A Haskell XML Toolbox application for
-- a graphic representation of n-ary trees, e.g. XmlTrees
-- with SGML. Input is a XML or HTML document, output a SGML document
-- consisting of a graphical representation of the tree
--
-- Author: Uwe Schmidt uwe@fh-wedel.de
--
-- Version : $Id: TreeVisualisation.hs,v 1.9 2004/09/02 19:11:50 hxml Exp $

module Main where

import Text.XML.HXT.Parser

import System.IO
import System.Environment
import System.Console.GetOpt

import System.Exit

import Data.Maybe

-- ----------------------------------------------------------

-- |
-- compute a layout tree for a given NTree
--
-- the layout tree has a stucture isomophic to the input tree
-- the layout is a pair of (x,y) coordinates. for every node
-- a weight is computed, the space for a tree will be proprtional to
-- the sum of the weight in the tree

treeToWeightedTree      :: (Num b) => (a -> b) -> NTree a -> NTree (b, b)
treeToWeightedTree wf (NTree n cs)
    = (NTree (wot, woc) cs')
      where
      cs' = map (treeToWeightedTree wf) cs
      woc = sum . map (\ (NTree (sum', _) _) -> sum') $ cs'
      wot = woc + wf n

-- |
-- auxiliary function called by 'horizontalLayout' for doint the hard work

weightedTreeToHLayout   :: (Fractional a) => a -> a -> NTree (a,a) -> NTree (a,a)
weightedTreeToHLayout from to (NTree (_wot, woc) cs)
    = NTree (from, to) cs'
      where
      len = to - from
      cs' = layoutChildren (fromInteger 0) cs
      layoutChildren _ []
          = []
      layoutChildren start (t1@(NTree (wot1, _woc1) _cs1) : ts)
          = weightedTreeToHLayout from' to' t1 : layoutChildren start' ts
            where
            from'  = (start  / woc) * len + from
            to'    = (start' / woc) * len + from
            start' = start + wot1

-- |
-- A horizontal layout for a tree generated with 'treeToWeightedTree'
-- is computed. The nodes are positioned in the unit square ((0,0),(1,1))
-- with the origin in the left upper corner. The root node is centered
-- at the top of the square

horizontalLayout        :: (Fractional b) => (a -> b) -> NTree a -> NTree (b,b)
horizontalLayout weightFct
    = weightedTreeToHLayout (fromInteger 0) (fromInteger 1) . treeToWeightedTree weightFct

-- |
-- layout of a tree in a 1.0 x 1.0 square
-- with the root node centered at the top and the
-- deepest leaves at the bottom

vTreeLayout             :: (Fractional b) => (a -> b) -> NTree a -> NTree (b,b)
vTreeLayout weightFct t
    = vLayout (0::Int) . horizontalLayout weightFct $ t
      where
      d = (depthNTree t - 1) `max` 1
      vLayout i (NTree (l,u) cs)
          = NTree (x, y) cs'
            where
            x = (l + u) / fromInteger 2
            y = fromInteger (toInteger i) / fromInteger (toInteger d)
            cs' = map (vLayout (i + 1)) cs

mirrorLayout            :: NTree (a,a) -> NTree (a,a)
mirrorLayout
    = mapNTree (\ (x,y) -> (y, x))

scaleLayout     :: (RealFrac a) => Int -> Int -> Int -> NTree (a,a) -> NTree (Int,Int)
scaleLayout border dx dy
    = mapNTree scale
      where
      scale (x,y) = (truncate (x * dx') + border, truncate (y * dy') + border)
      dx' = fromIntegral (dx - 2 * border)
      dy' = fromIntegral (dy - 2 * border)

centricLayout   :: (Floating a) => NTree (a,a) -> NTree (a,a)
centricLayout
    = mapNTree center
      where
      center (x, y)
          = ((x' + 1) /2, (y' + 1) /2)
            where
            phi = (2 * x - 0.5) * pi
            r   = y
            x'  = cos phi * r
            y'  = sin phi * r
-- |
-- map nodes of a NTree to SVG tags

mapNodesToXml   :: (a -> (Int, Int) -> XmlTrees) -> NTree a -> NTree (Int, Int) -> XmlTrees
mapNodesToXml formatNode layoutTree orgTree
    = concat . zipWith formatNode (nTreeToList layoutTree) $ (nTreeToList orgTree)

mapNodesToSVG   :: (a -> XmlFilter) -> NTree a -> NTree (Int, Int) -> XmlTrees
mapNodesToSVG circleLayout
    = mapNodesToXml drawNode
      where
      drawNode n (x,y)
          = atag "circle" [ sattr "cx" (show x)
                          , sattr "cy" (show y)
                          , circleLayout n
                          ] $ undefined

mapEdgesToSVG   :: ((Int, Int) -> (Int, Int) -> XmlTrees) -> NTree (Int, Int) -> XmlTrees
mapEdgesToSVG draw (NTree p1 cs)
    = concatMap (\ t' -> mkLine t' ++ mapEdgesToSVG draw t') cs
      where
      mkLine (NTree p2 _) = draw p1 p2

drawLine        :: XmlFilter -> (Int, Int) -> (Int, Int) -> XmlTrees
drawLine lineAttrl p1 p2
    = drawLines lineAttrl [p1,p2]

drawHPath       :: XmlFilter -> (Int, Int) -> (Int, Int) -> XmlTrees
drawHPath lineAttrl p1@(x1, y1) p2@(x2, y2)
    = drawLines lineAttrl [p1, (x1,y12), (x2,y12), p2]
      where
      y12 = (y1 + y2) `div` 2

drawVPath       :: XmlFilter -> (Int, Int) -> (Int, Int) -> XmlTrees
drawVPath lineAttrl p1@(x1, y1) p2@(x2, y2)
    = drawLines lineAttrl [p1, (x12,y1), (x12,y2), p2]
      where
      x12 = (x1 + x2) `div` 2

drawLines       :: XmlFilter -> [(Int, Int)] -> XmlTrees
drawLines attl ps
    = concat . zipWith draw ps $ tail ps
      where
      draw (x1, y1) (x2, y2)
          = atag "line" [ sattr "x1" (show x1)
                        , sattr "y1" (show y1)
                        , sattr "x2" (show x2)
                        , sattr "y2" (show y2)
                        , attl
                        ] $ undefined

-- |
-- filter for copying the attributes into the content part
-- the result does not correspont to a wellformed XML document
-- but for drawing the tree inclusive attributes this works fine

moveAttrlToContent      :: XmlFilter
moveAttrlToContent
    = processChildren
      ( processBottomUp
        ( substChildren (getAttrl +++ getChildren) `when` isXTag ) )

-- |
-- the main transformation filter

xmlTreeToSVG    :: XmlFilter
xmlTreeToSVG t
    = rootTag [ getAttrl ]
      [ getChildren
        .>
        isXTag                                          -- get the root tag
        .>
        cat [ dtd DOCTYPE [ sattr a_name "svg"
                          , sattr k_public "-//W3C//DTD SVG 1.1//EN"
                          , sattr k_system "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
                          ] []
            , tag "svg"
              [ sattr "width"           (show widthInt)
              , sattr "height"          (show heightInt)
              , sattr "version"         "1.1"
              , sattr "xmlns"           "http://www.w3.org/2000/svg"
              , sattr "xmlns:xlink"     "http://www.w3.org/1999/xlink"          -- currently not used
              ]
              [ stag "title" [ txt source]
              , tag "text" [ sattr "x" "10"             -- draw source uri
                           , sattr "y" "22"
                           , sattr "font-size" "12"
                           , sattr "fill" "grey"
                           ] [ txt ("source: " ++ source)
                             ]
              , legend (10, heightInt - 5 * pixelPerLine - 10)
                [ (tagColor,  "tag")
                , (textColor, "text")
                , (attrColor, "attribute")
                , (cmtColor,  "comment")
                , (defColor,  "others")
                                ]
              , nodesToSVG
                . scaleLayout borderWidthInt widthInt heightInt
                . layoutFct
                . vTreeLayout nSize
              ]
            ]
      ] .> addXmlPiToDoc $ t
      where
      t1 = head . (getChildren .> isXTag) $ t           -- the root tag

      pixelPerLine = 15

      legend    :: (Int, Int) -> [(String, String)] -> XmlFilter
      legend (x1, y1) ls
          = cat (zipWith legend1 ps ls)
            where
            ps = map (\ y' -> (x1, y1 + y' * pixelPerLine)) [0..]
            legend1 (x', y') (color, text)
                = cat [ atag "circle" [ sattr "cx" (show (x' + 5))
                                      , sattr "cy" (show (y' + 10))
                                      , sattr "r" "5"
                                      , sattr "fill" color
                                      , sattr "stroke-width" lineWidth
                                      , sattr "stroke" lineColor
                                      ]
                      , tag "text" [ sattr "x" (show (x' + pixelPerLine))
                                   , sattr "y" (show (y' + pixelPerLine))
                                   , sattr "font-size" "12"
                                   , sattr "fill" "grey"
                                   ] [ txt text
                                     ]
                      ]

      defValueOf attr' def' t'
          | null res  = def'
          | otherwise = res
          where
          res = valueOf attr' t'

      width         = defValueOf "width"                "800"           t
      height        = defValueOf "height"               "600"           t
      nodeScale     = defValueOf "nodeSize"             "1"             t
      borderWidth   = defValueOf "borderWidth"          "20"            t
      lineWidth     = defValueOf "lineWidht"            "1"             t
      lineColor     = defValueOf "lineColor"            "grey"          t
      textColor     = defValueOf "textNodeColor"        "blue"          t
      tagColor      = defValueOf "tagNodeColor"         "red"           t
      attrColor     = defValueOf "attrNodeColor"        "green"         t
      cmtColor      = defValueOf "cmtNodeColor"         "magenta"       t
      defColor      = defValueOf "defaultNodeColor"     "grey"          t
      layout        = defValueOf "layout"               "vertical"      t
      lineStyle     = defValueOf "lineStyle"            "line"          t
      source        = valueOf a_source                                  t

      widthInt, heightInt, borderWidthInt           :: Int

      widthInt      = read width  `max` 100
      heightInt     = read height `max` 100
      borderWidthInt= read borderWidth

      nodesToSVG t' = mapEdgesToSVG (edgeLayoutFct edgeLayout) t'
                      ++
                      mapNodesToSVG nodeLayout t1 t'

      nodeLayout n
          = cat [ sattr "fill"         nColor
                , sattr "stroke"       ( if r < 5       -- if circle too small
                                         then nColor    -- border is drawn in the same color as cirle
                                         else lineColor
                                       )
                , sattr "stroke-width" lineWidth
                , sattr "r"            (show r)
                ]
            where
            (nColor, nSz) = nLayout n
            nSz1          = nSz * (read nodeScale)
            r             = (round (sqrt (fromInteger (toInteger nSz1))::Double))::Int

      nSize     :: XNode -> Double
      nSize     = fromInteger . toInteger . snd . nLayout

      nLayout (XTag _tn _)      = (tagColor,    9)              -- constant size for tags
      nLayout (XText tx)        = (textColor,   length tx)
      nLayout (XAttr _an)       = (attrColor,   4)              -- attributes are smaller than tags
      nLayout (XCmt c)          = (cmtColor,    length c)
      nLayout _                 = (defColor,    1)              -- entities, PI's, dtd stuff, ...

      layoutFct     :: NTree (Double,Double) -> NTree (Double,Double)
      layoutFct
          = fromJust . lookup layout
            $ [ ("vertical",               id)
              , ("horizontal",  mirrorLayout )
              , ("centric",    centricLayout )
              ]

      edgeLayoutFct
          = fromJust . lookup (layout, lineStyle)
            $ [ (("vertical",   "line"),        drawLine  )
              , (("vertical",   "fork"),        drawHPath )
              , (("horizontal", "line"),        drawLine  )
              , (("horizontal", "fork"),        drawVPath )
              , (("centric",    "line"),        drawLine  )
              , (("centric",    "fork"),        drawLine  )
              ]

      edgeLayout
          = cat [ sattr "stroke"       lineColor
                , sattr "stroke-width" "1"
                ]

-- ----------------------------------------------------------

-- |
-- the main program of the Haskell XML Validating Parser

main :: IO ()
main
    = do
      argv <- getArgs
      al   <- cmdlineOpts argv
      res  <- run' (xmlToSVG al emptyRoot)
      exitProg (null res)                               -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure 1)
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- runs in the trivial XmlState monad (with user state set to ())
-- so IO and access to global options is possible

xmlToSVG        :: Attributes -> XmlStateFilter state
xmlToSVG al
    = parseDocument al                                  -- read document
      .>>
      traceMsg 1 "start processing"
      .>>
      liftMf
      ( canonicalizeAllNodes
        .>
        ( moveAttrlToContent `when` hasAttr "withAttr" )
        .>
        xmlTreeToSVG
        .>
        indentDoc
      )
      .>>
      traceMsg 1 "processing finished"
      .>>
      traceSource
      .>>
      traceTree
      .>>
      ( writeDocument [ (a_indent, v_1) ]
        `whenM`
        getChildren
      )
      .>>
      checkStatus

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "TreeVisualisation"

options         :: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions
      ++
      showOptions
      ++
      [ Option ""       ["width"]               (ReqArg (nat "width")   "NUM")          "SVG image width in pixel, default: 800"
      , Option ""       ["height"]              (ReqArg (nat "height")  "NUM")          "SVG image height in pixel, default: 600"
      , Option ""       ["withAttr"]            (NoArg  (att "withAttr" "1"))           "show tag attributes, default: no"
      , Option ""       ["nodeSize"]            (ReqArg (nat "nodeSize") "NUM")         "scale size of nodes, default: 1"
      , Option ""       ["lineWidth"]           (ReqArg (nat "lineWidth") "NUM")        "width of edges in pixel, default: 1\n"
      , Option ""       ["border"]              (ReqArg (nat "borderWidth") "NUM")      "border around tree in pixel, default: 20\n"

      , Option ""       ["verticalTree"]        (NoArg  (att "layout" "vertical"))      "vertical tree layout, root centered at the top border"
      , Option ""       ["horizontalTree"]      (NoArg  (att "layout" "horizontal"))    "horizontal tree layout, root centered at the left border"
      , Option ""       ["centricTree"]         (NoArg  (att "layout" "centric"))       "layout with root in the center"
      , Option ""       ["directLines"]         (NoArg  (att "lineStyle" "line"))       "edges are drawn as direct lines"
      , Option ""       ["horizontalLines"]     (NoArg  (att "lineStyle" "fork"))       "edges are drawn as a path of horizontal and vertical lines\n"

      , Option ""       ["textNodeColor"]       (ReqArg (att "textNodeColor") "COLOR")  "text node: color name or RGB value in format \"#RRGGBB\""
      , Option ""       ["tagNodeColor"]        (ReqArg (att "tagNodeColor") "COLOR")   "tag node color"
      , Option ""       ["attrNodeColor"]       (ReqArg (att "attrNodeColor") "COLOR")  "attribute node color"
      , Option ""       ["cmtNodeColor"]        (ReqArg (att "cmtNodeColor") "COLOR")   "comment node color"
      , Option ""       ["defaultNodeColor"]    (ReqArg (att "defNodeColor") "COLOR")   "other node color"
      ]
    where
    att n v     = (n, v)
    nat n v     = (n, filter (`elem` "0123456789") v)

usage           :: [String] -> IO a
usage errl
    | null errl
        = do
          hPutStrLn stdout use
          exitProg False
    | otherwise
        = do
          hPutStrLn stderr (concat errl ++ "\n" ++ use)
          exitProg True
    where
    header = "TreeVisualisation - An example application of the Haskell XML Toolbox\n" ++
             "for the visualisation of the structure of a XML document with SVG\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
          -> do
             sa <- src n
             help (lookup a_help ol)
             return (ol ++ sa)
      (_,_,errs)
          -> usage errs
    where
    src []      = usage ["no input url or file given\n"]
    src [url]   = return [("source", url)]
    src _       = usage ["only one input uri or file allowed\n"]

    help Nothing        = return ()
    help (Just _)       = usage []

-- ------------------------------------------------------------


