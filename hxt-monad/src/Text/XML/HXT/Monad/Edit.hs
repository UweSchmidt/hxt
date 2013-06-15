{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ------------------------------------------------------------

{- |
   common edit arrows
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Monad.Edit
    ( canonicalizeAllNodes
    , canonicalizeForXPath
    , canonicalizeContents
    , collapseAllXText
    , collapseXText

    , xshowEscapeXml

    , escapeXmlRefs
    , escapeHtmlRefs

    , haskellRepOfXmlDoc
    , treeRepOfXmlDoc
    , addHeadlineToXmlDoc

    , indentDoc
    , numberLinesInXmlDoc
    , preventEmptyElements

    , removeComment
    , removeAllComment
    , removeWhiteSpace
    , removeAllWhiteSpace
    , removeDocWhiteSpace

    , transfCdata
    , transfAllCdata
    , transfCharRef
    , transfAllCharRef

    , substAllXHTMLEntityRefs
    , substXHTMLEntityRef

    , rememberDTDAttrl
    , addDefaultDTDecl

    , hasXmlPi
    , addXmlPi
    , addXmlPiEncoding

    , addDoctypeDecl
    , addXHtmlDoctypeStrict
    , addXHtmlDoctypeTransitional
    , addXHtmlDoctypeFrameset
    )
where
import qualified Control.Arrow                     as A
import           Control.Monad.Arrow

import           Data.Char.Properties.XMLCharProps (isXmlSpaceChar)
import           Data.List                         (isPrefixOf)
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Sequence.ArrowTypes

import           Text.XML.HXT.DOM.FormatXmlTree    (formatXmlTree)
import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.ShowXml          as XS
import qualified Text.XML.HXT.DOM.XmlNode          as XN

import           Text.XML.HXT.Monad.ArrowXml

import           Text.XML.HXT.Parser.HtmlParsec    (emptyHtmlTags)
import           Text.XML.HXT.Parser.XhtmlEntities (xhtmlEntities)
import           Text.XML.HXT.Parser.XmlEntities   (xmlEntities)

-- ------------------------------------------------------------

-- |
-- Applies some "Canonical XML" rules to a document tree.
--
-- The rules differ slightly for canonical XML and XPath in handling of comments
--
-- Note: This is not the whole canonicalization as it is specified by the W3C
-- Recommendation. Adding attribute defaults or sorting attributes in lexicographic
-- order is done by the @transform@ function of module @Text.XML.HXT.Validator.Validation@.
-- Replacing entities or line feed normalization is done by the parser.
--
--
-- Not implemented yet:
--
--  - Whitespace within start and end tags is normalized
--
--  - Special characters in attribute values and character content are replaced by character references
--
-- see 'canonicalizeAllNodes' and 'canonicalizeForXPath'

canonicalizeTree'       :: LA XmlTree XmlTree -> LA XmlTree XmlTree
canonicalizeTree' toBeRemoved
    = processChildren
      ( (none `when` (isText <++> isXmlPi))      -- remove XML PI and all text around XML root element
        >=>
        (deep isPi `when` isDTD)                -- remove DTD parts, except PIs whithin DTD
      )
      >=>
      canonicalizeNodes toBeRemoved

canonicalizeNodes       :: LA XmlTree XmlTree -> LA XmlTree XmlTree
canonicalizeNodes toBeRemoved
    = editNTreeA $
      [ toBeRemoved     :-> none
      , ( isElem >=> getAttrl >=> getChildren >=> isCharRef )   -- canonicalize attribute list
                        :-> ( processAttrl
                              ( processChildren transfCharRef
                                >=>
                                collapseXText'                  -- combine text in attribute values
                              )
                              >=>
                              ( collapseXText'                  -- and combine text in content
                                `when`
                                (getChildren >>. has2XText)
                              )
                            )
      , ( isElem >=> (getChildren >>. has2XText) )
                        :-> collapseXText'                      -- combine text in content

      , isCharRef       :-> ( getCharRef
                              >=>
                              return . (\ i -> [toEnum i])
                              >=>
                              mkText
                            )
      , isCdata         :-> ( getCdata
                              >=>
                              mkText
                            )
      ]

-- |
-- Applies some "Canonical XML" rules to a document tree.
--
-- The rule differ slightly for canonical XML and XPath in handling of comments
--
-- Note: This is not the whole canonicalization as it is specified by the W3C
-- Recommendation. Adding attribute defaults or sorting attributes in lexicographic
-- order is done by the @transform@ function of module @Text.XML.HXT.Validator.Validation@.
-- Replacing entities or line feed normalization is done by the parser.
--
-- Rules: remove DTD parts, processing instructions, comments and substitute char refs in attribute
-- values and text
--
-- Not implemented yet:
--
--  - Whitespace within start and end tags is normalized
--
--  - Special characters in attribute values and character content are replaced by character references

canonicalizeAllNodes    :: SeqA m XmlTree XmlTree
canonicalizeAllNodes    = fromLA $
                          canonicalizeTree' isCmt                       -- remove comment
{-# INLINE canonicalizeAllNodes #-}

-- |
-- Canonicalize a tree for XPath
-- Like 'canonicalizeAllNodes' but comment nodes are not removed
--
-- see 'canonicalizeAllNodes'

canonicalizeForXPath    :: SeqA m XmlTree XmlTree
canonicalizeForXPath    = fromLA $ canonicalizeTree' none               -- comment remains there
{-# INLINE canonicalizeForXPath #-}

-- |
-- Canonicalize the contents of a document
--
-- substitutes all char refs in text and attribute values,
-- removes CDATA section and combines all sequences of resulting text
-- nodes into a single text node
--
-- see 'canonicalizeAllNodes'

canonicalizeContents    :: SeqA m XmlTree XmlTree
canonicalizeContents    = fromLA $
                          canonicalizeNodes none
{-# INLINE canonicalizeContents #-}

-- ------------------------------------------------------------

has2XText               :: XmlTrees -> XmlTrees
has2XText ts0@(t1 : ts1@(t2 : ts2))
    | XN.isText t1      = if XN.isText t2
                          then ts0
                          else has2XText ts2
    | otherwise         = has2XText ts1
has2XText _             = []

collapseXText'          :: LA XmlTree XmlTree
collapseXText'
    = replaceChildren ( listA getChildren >=> arrL (foldr mergeText' []) )
    where
    mergeText'  :: XmlTree -> XmlTrees -> XmlTrees
    mergeText' t1 (t2 : ts2)
        | XN.isText t1 && XN.isText t2
            = let
              s1 = fromJust . XN.getText $ t1
              s2 = fromJust . XN.getText $ t2
              t  = XN.mkText (s1 ++ s2)
              in
              t : ts2
    mergeText' t1 ts
        = t1 : ts

-- |
-- Collects sequences of text nodes in the list of children of a node into one single text node.
-- This is useful, e.g. after char and entity reference substitution

collapseXText           :: SeqA m XmlTree XmlTree
collapseXText           = fromLA collapseXText'

-- |
-- Applies collapseXText recursively.
--
--
-- see also : 'collapseXText'

collapseAllXText        :: SeqA m XmlTree XmlTree
collapseAllXText        = fromLA $ processBottomUp collapseXText'

-- ------------------------------------------------------------

-- | apply an arrow to the input and convert the resulting XML trees into an XML escaped string
--
-- This is a save variant for converting a tree into an XML string representation
-- that is parsable with 'Text.XML.HXT.Arrow.ReadDocument'.
-- It is implemented with 'Text.XML.HXT.Arrow.XmlArrow.xshow',
-- but xshow does no XML escaping. The XML escaping is done with
-- 'Text.XML.HXT.Arrow.Edit.escapeXmlDoc' before xshow is applied.
--
-- So the following law holds
--
-- > xshowEscapeXml f >=> xread == f

xshowEscapeXml :: MonadSeq m => (b -> m XmlTree) -> (b -> m String)
xshowEscapeXml f        = f >. (uncurry XS.xshow'' escapeXmlRefs)

-- ------------------------------------------------------------

-- |
-- escape XmlText,
-- transform all special XML chars into char- or entity- refs

type EntityRefTable     = M.Map Int String

xmlEntityRefTable
 , xhtmlEntityRefTable  :: EntityRefTable

xmlEntityRefTable       = buildEntityRefTable $ xmlEntities
xhtmlEntityRefTable     = buildEntityRefTable $ xhtmlEntities

buildEntityRefTable     :: [(String, Int)] -> EntityRefTable
buildEntityRefTable     = M.fromList . map (\ (x,y) -> (y,x) )

type EntitySubstTable   = M.Map String String

xhtmlEntitySubstTable   :: EntitySubstTable
xhtmlEntitySubstTable   = M.fromList . map (A.second $ (:[]) . toEnum) $ xhtmlEntities

-- ------------------------------------------------------------

substXHTMLEntityRef	:: SeqA m XmlTree XmlTree
substXHTMLEntityRef
    = ( getEntityRef
        >=>
        arrL subst
        >=>
        mkText
      )
      `orElse` this
    where
      subst name
          = maybe [] (:[]) $ M.lookup name xhtmlEntitySubstTable

substAllXHTMLEntityRefs	:: SeqA m XmlTree XmlTree
substAllXHTMLEntityRefs
    = fromLA $
      processBottomUp substXHTMLEntityRef

-- ------------------------------------------------------------

escapeXmlRefs           :: (Char -> String -> String, Char -> String -> String)
escapeXmlRefs           = (cquote, aquote)
    where
    cquote c
        | c `elem` "<&" = ('&' :)
                          . ((lookupRef c xmlEntityRefTable) ++)
                          . (';' :)
        | otherwise     = (c :)
    aquote c
        | c `elem` "<>\"\'&\n\r\t"
                        = ('&' :)
                          . ((lookupRef c xmlEntityRefTable) ++)
                          . (';' :)
        | otherwise     = (c :)

escapeHtmlRefs          :: (Char -> String -> String, Char -> String -> String)
escapeHtmlRefs          = (cquote, aquote)
    where
    cquote c
        | isHtmlTextEsc c
                        = ('&' :)
                          . ((lookupRef c xhtmlEntityRefTable) ++)
                          . (';' :)
        | otherwise     = (c :)
    aquote c
        | isHtmlAttrEsc c
                        = ('&' :)
                          . ((lookupRef c xhtmlEntityRefTable) ++)
                          . (';' :)
        | otherwise     = (c :)

    isHtmlTextEsc c     = c >= toEnum(128) || ( c `elem` "<&" )
    isHtmlAttrEsc c     = c >= toEnum(128) || ( c `elem` "<>\"\'&\n\r\t" )

lookupRef               :: Char -> EntityRefTable -> String
lookupRef c             = fromMaybe ('#' : show (fromEnum c))
                          . M.lookup (fromEnum c)
{-# INLINE lookupRef #-}

-- ------------------------------------------------------------

preventEmptyElements    :: (MonadSeq m) => [String] -> Bool -> XmlTree -> m XmlTree
preventEmptyElements ns isHtml
    = fromLA $
      editNTreeA [ ( isElem
                     >=>
                     isNoneEmpty
                     >=>
                     neg getChildren
                   )
                   :-> replaceChildren (txt "")
                 ]
    where
    isNoneEmpty
        | not (null ns) = hasNameWith ((`elem` ns) . localPart)
        | isHtml        = hasNameWith ((`notElem` emptyHtmlTags) . localPart)
        | otherwise     = this

-- ------------------------------------------------------------

-- |
-- convert a document into a Haskell representation (with show).
--
-- Useful for debugging and trace output.
-- see also : 'treeRepOfXmlDoc', 'numberLinesInXmlDoc'

haskellRepOfXmlDoc      :: SeqA m XmlTree XmlTree
haskellRepOfXmlDoc
    = fromLA $
      root [getAttrl] [show ^=> mkText]

-- |
-- convert a document into a text and add line numbers to the text representation.
--
-- Result is a root node with a single text node as child.
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'treeRepOfXmlDoc'

numberLinesInXmlDoc     :: SeqA m XmlTree XmlTree
numberLinesInXmlDoc
    = fromLA $
      processChildren (changeText numberLines)
    where
    numberLines :: String -> String
    numberLines str
        = concat $
          zipWith (\ n l -> lineNr n ++ l ++ "\n") [1..] (lines str)
        where
        lineNr   :: Int -> String
        lineNr n = (reverse (take 6 (reverse (show n) ++ replicate 6 ' '))) ++ "  "

-- |
-- convert a document into a text representation in tree form.
--
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'numberLinesInXmlDoc'

treeRepOfXmlDoc :: SeqA m XmlTree XmlTree
treeRepOfXmlDoc
    = fromLA $
      root [getAttrl] [formatXmlTree ^=> mkText]

addHeadlineToXmlDoc     :: SeqA m XmlTree XmlTree
addHeadlineToXmlDoc
    = fromLA $ ( addTitle $< (getAttrValue a_source >=^ formatTitle) )
    where
    addTitle str
        = replaceChildren ( txt str <++> getChildren <++> txt "\n" )
    formatTitle str
        = "\n" ++ headline ++ "\n" ++ underline ++ "\n\n"
        where
        headline  = "content of: " ++ str
        underline = map (const '=') headline

-- ------------------------------------------------------------

-- |
-- remove a Comment node

removeComment           :: SeqA m XmlTree XmlTree
removeComment           = none `when` isCmt

-- |
-- remove all comments in a tree recursively

removeAllComment        :: SeqA m XmlTree XmlTree
removeAllComment        = fromLA $ editNTreeA [isCmt :-> none]

-- ------------------------------------------------------------

-- |
-- simple filter for removing whitespace.
--
-- no check on sigificant whitespace, e.g. in HTML \<pre\>-elements, is done.
--
--
-- see also : 'removeAllWhiteSpace', 'removeDocWhiteSpace'

removeWhiteSpace        :: SeqA m XmlTree XmlTree
removeWhiteSpace        = fromLA $ none `when` isWhiteSpace

-- |
-- simple recursive filter for removing all whitespace.
--
-- removes all text nodes in a tree that consist only of whitespace.
--
--
-- see also : 'removeWhiteSpace', 'removeDocWhiteSpace'

removeAllWhiteSpace     :: SeqA m XmlTree XmlTree
removeAllWhiteSpace     = fromLA $ editNTreeA [isWhiteSpace :-> none]
                       -- fromLA $ processBottomUp removeWhiteSpace'    -- less efficient

-- ------------------------------------------------------------

-- |
-- filter for removing all not significant whitespace.
--
-- the tree traversed for removing whitespace between elements,
-- that was inserted for indentation and readability.
-- whitespace is only removed at places, where it's not significat
-- preserving whitespace may be controlled in a document tree
-- by a tag attribute @xml:space@
--
-- allowed values for this attribute are @default | preserve@
--
-- input is root node of the document to be cleaned up,
-- output the semantically equivalent simplified tree
--
--
-- see also : 'indentDoc', 'removeAllWhiteSpace'

removeDocWhiteSpace     :: SeqA m XmlTree XmlTree
removeDocWhiteSpace     = fromLA $ removeRootWhiteSpace


removeRootWhiteSpace    :: SeqA m XmlTree XmlTree
removeRootWhiteSpace
    =  processChildren processRootElement
       `when`
       isRoot
    where
    -- processRootElement  :: XmlTree -> XmlTree
    processRootElement
        = removeWhiteSpace >=> processChild
        where
        processChild
            = choiceA [ isDTD
                        :-> removeAllWhiteSpace                 -- whitespace in DTD is redundant
                      , this
                        :-> replaceChildren ( getChildren
                                              >>. indentTrees insertNothing False 1
                                            )
                      ]

-- ------------------------------------------------------------

-- |
-- filter for indenting a document tree for pretty printing.
--
-- the tree is traversed for inserting whitespace for tag indentation.
--
-- whitespace is only inserted or changed at places, where it isn't significant,
-- is's not inserted between tags and text containing non whitespace chars.
--
-- whitespace is only inserted or changed at places, where it's not significant.
-- preserving whitespace may be controlled in a document tree
-- by a tag attribute @xml:space@
--
-- allowed values for this attribute are @default | preserve@.
--
-- input is a complete document tree or a document fragment
-- result is the semantically equivalent formatted tree.
--
--
-- see also : 'removeDocWhiteSpace'

indentDoc               :: SeqA m XmlTree XmlTree
indentDoc               = fromLA $
                          ( ( isRoot `guards` indentRoot )
                            `orElse`
                            (root [] [this] >=> indentRoot >=> getChildren)
                          )

-- ------------------------------------------------------------

indentRoot              :: SeqA m XmlTree XmlTree
indentRoot              = processChildren indentRootChildren
    where
    indentRootChildren
        = removeText >=> indentChild >=> insertNL
        where
        removeText      = none `when` isText
        insertNL        = this <++> txt "\n"
        indentChild     = ( replaceChildren
                            ( getChildren
                              >>.
                              indentTrees (insertIndentation 2) False 1
                            )
                            `whenNot` isDTD
                          )

-- ------------------------------------------------------------
--
-- copied from EditFilter and rewritten for arrows
-- to remove dependency to the filter module

indentTrees     :: (Int -> LA XmlTree XmlTree) -> Bool -> Int -> XmlTrees -> XmlTrees
indentTrees _ _ _ []
    = []
indentTrees indentFilter preserveSpace level ts
    = runLAs lsf ls
      ++
      indentRest rs
      where
      runLAs f l
          = runLA (constL l >=> f) undefined

      (ls, rs)
          = break XN.isElem ts

      isSignificant     :: Bool
      isSignificant
          = preserveSpace
            ||
            (not . null . runLAs isSignificantPart) ls

      isSignificantPart :: LA XmlTree XmlTree
      isSignificantPart
          = catA
            [ isText `guards` neg isWhiteSpace
            , isCdata
            , isCharRef
            , isEntityRef
            ]

      lsf       :: LA XmlTree XmlTree
      lsf
          | isSignificant
              = this
          | otherwise
              = (none `when` isWhiteSpace)
                >=>
                (indentFilter level <++> this)

      indentRest        :: XmlTrees -> XmlTrees
      indentRest []
          | isSignificant
              = []
          | otherwise
              = runLA (indentFilter (level - 1)) undefined

      indentRest (t':ts')
          = runLA ( ( indentElem
                      >=>
                      lsf
                    )
                    `when` isElem
                  ) t'
            ++
            ( if null ts'
              then indentRest
              else indentTrees indentFilter preserveSpace level
            ) ts'
          where
          indentElem
              = replaceChildren ( getChildren
                                  >>.
                                  indentChildren
                                )

          xmlSpaceAttrValue     :: String
          xmlSpaceAttrValue
              = concat . runLA (getAttrValue "xml:space") $ t'

          preserveSpace'        :: Bool
          preserveSpace'
              = ( fromMaybe preserveSpace
                  .
                  lookup xmlSpaceAttrValue
                ) [ ("preserve", True)
                  , ("default",  False)
                  ]

          indentChildren        :: XmlTrees -> XmlTrees
          indentChildren cs'
              | all (maybe False (all isXmlSpaceChar) . XN.getText) cs'
                  = []
              | otherwise
                  = indentTrees indentFilter preserveSpace' (level + 1) cs'


-- filter for indenting elements

insertIndentation       :: Int -> Int -> LA XmlTree XmlTree
insertIndentation indentWidth level
    = txt ('\n' : replicate (level * indentWidth) ' ')

-- filter for removing all whitespace

insertNothing           :: Int -> LA a XmlTree
insertNothing _         = none

-- ------------------------------------------------------------

-- |
-- converts a CDATA section into normal text nodes

transfCdata             :: SeqA m XmlTree XmlTree
transfCdata             = fromLA $
                          (getCdata >=> mkText) `when` isCdata

-- |
-- converts CDATA sections in whole document tree into normal text nodes

transfAllCdata          :: SeqA m XmlTree XmlTree
transfAllCdata          = fromLA $ editNTreeA [isCdata :-> (getCdata >=> mkText)]

-- |
-- converts a character reference to normal text

transfCharRef           :: SeqA m XmlTree XmlTree
transfCharRef           = fromLA $
                          ( getCharRef >=> return . (\ i -> [toEnum i]) >=> mkText )
                          `when`
                          isCharRef

-- |
-- recursively converts all character references to normal text

transfAllCharRef        :: SeqA m XmlTree XmlTree
transfAllCharRef        = fromLA $
                          editNTreeA [isCharRef :-> (getCharRef
                                                     >=>
                                                     return . (\ i -> [toEnum i])
                                                     >=> mkText
                                                    )
                                     ]

-- ------------------------------------------------------------

rememberDTDAttrl        :: SeqA m XmlTree XmlTree
rememberDTDAttrl
    = fromLA $
      ( ( addDTDAttrl $< ( getChildren >=> isDTDDoctype >=> getDTDAttrl ) )
        `orElse`
        this
      )
    where
    addDTDAttrl al
        = seqA . map (uncurry addAttr) . map (A.first (dtdPrefix ++)) $ al

addDefaultDTDecl        :: SeqA m XmlTree XmlTree
addDefaultDTDecl
    = fromLA $
      ( addDTD $< listA ( getAttrl
                          >=>
                          (getName &=& xshow getChildren)
                          >=>
                          hasDtdPrefix
                        )
      )
    where
    hasDtdPrefix
        = isA ((dtdPrefix `isPrefixOf`) . fst)
          >=>
          return . (A.first (drop (length dtdPrefix)))
    addDTD []
        = this
    addDTD al
        = replaceChildren
          ( mkDTDDoctype al none
            <++>
            txt "\n"
            <++>
            ( getChildren >=> (none `when` isDTDDoctype) )      -- remove old DTD decl
          )

-- ------------------------------------------------------------

hasXmlPi                :: SeqA m XmlTree XmlTree
hasXmlPi
    = fromLA
      ( getChildren
        >=>
        isPi
        >=>
        hasName t_xml
      )

-- | add an \<?xml version=\"1.0\"?\> processing instruction
-- if it's not already there

addXmlPi                :: SeqA m XmlTree XmlTree
addXmlPi
    = fromLA
      ( insertChildrenAt 0 ( ( mkPi (mkName t_xml) none
                               >=>
                               addAttr a_version "1.0"
                             )
                             <++>
                             txt "\n"
                           )
        `whenNot`
        hasXmlPi
      )

-- | add an encoding spec to the \<?xml version=\"1.0\"?\> processing instruction

addXmlPiEncoding :: MonadSeq m => String -> XmlTree -> m XmlTree
addXmlPiEncoding enc
    = fromLA $
      processChildren ( addAttr a_encoding enc
                        `when`
                        ( isPi >=> hasName t_xml )
                      )

-- | add an XHTML strict doctype declaration to a document

addXHtmlDoctypeStrict
  , addXHtmlDoctypeTransitional
  , addXHtmlDoctypeFrameset     :: SeqA m XmlTree XmlTree

-- | add an XHTML strict doctype declaration to a document

addXHtmlDoctypeStrict
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"

-- | add an XHTML transitional doctype declaration to a document

addXHtmlDoctypeTransitional
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"

-- | add an XHTML frameset doctype declaration to a document

addXHtmlDoctypeFrameset
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"

-- | add a doctype declaration to a document
--
-- The arguments are the root element name, the PUBLIC id and the SYSTEM id

addDoctypeDecl :: MonadSeq m => String -> String -> String -> XmlTree -> m XmlTree
addDoctypeDecl rootElem public system
    = fromLA $
      replaceChildren
      ( mkDTDDoctype ( ( if null public then id else ( (k_public, public) : ) )
                       .
                       ( if null system then id else ( (k_system, system) : ) )
                       $  [ (a_name, rootElem) ]
                     ) none
        <++>
        txt "\n"
        <++>
        getChildren
      )

-- ------------------------------------------------------------
