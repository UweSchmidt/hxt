-- |
-- XML editing filters

module Text.XML.HXT.DOM.EditFilters
    ( canonicalizeTree
    , canonicalizeAllNodes
    , canonicalizeForXPath

    , collapseXText
    , collapseAllXText

    , indentDoc

    , removeWhiteSpace
    , removeAllWhiteSpace
    , removeDocWhiteSpace

    , removeComment
    , removeAllComment

    , transfCdata               , transfAllCdata        -- CDATA to text
    , transfCdataEscaped        , transfAllCdataEscaped -- CDATA to text
    , transfCharRef             , transfAllCharRef      -- &#nnn; to text

    , escapeXmlDoc
    , escapeXmlText
    , escapeXmlAttrValue

    , unparseXmlDoc
    , numberLinesInXmlDoc       , numberLines
    , treeRepOfXmlDoc
    , haskellRepOfXmlDoc
    , addHeadlineToXmlDoc
    , addXmlPiToDoc
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.Unicode
    ( getOutputEncodingFct)

import Text.XML.HXT.DOM.FormatXmlTree
    ( formatXmlContents )

import Data.Maybe

-- ------------------------------------------------------------

-- |
-- remove Comments

removeComment           :: XmlFilter
removeComment           = none `when` isXCmt

-- |
-- remove all Comments recursively

removeAllComment        :: XmlFilter
removeAllComment        = processBottomUp removeComment

-- ------------------------------------------------------------

-- |
-- simple filter for removing whitespace.
--
-- no check on sigificant whitespace is done.
--
--
-- see also : 'removeAllWhiteSpace', 'removeDocWhiteSpace'

removeWhiteSpace        :: XmlFilter
removeWhiteSpace        = none `when` isWhiteSpace

-- |
-- simple recursive filter for removing all whitespace.
--
-- removes all text nodes in a tree that consist only of whitespace.
--
--
-- see also : 'removeWhiteSpace', 'removeDocWhiteSpace'

removeAllWhiteSpace     :: XmlFilter
removeAllWhiteSpace     = processBottomUp removeWhiteSpace

-- ------------------------------------------------------------
-- |
-- converts CDATA sections in whole document tree into normal text nodes

transfAllCdataEscaped   :: XmlFilter
transfAllCdataEscaped   = processBottomUp transfCdataEscaped

-- |
-- converts CDATA section in normal text nodes

transfCdataEscaped      :: XmlFilter
transfCdataEscaped (NTree (XCdata str) _)
    = xtext str

transfCdataEscaped n
    = [n]


-- ------------------------------------------------------------
-- |
-- converts CDATA sections in whole document tree

transfAllCdata          :: XmlFilter
transfAllCdata          = processBottomUp transfCdata

-- |
-- converts CDATA section in normal text sections

transfCdata             :: XmlFilter
transfCdata (NTree (XCdata str) _)
    = xtext str

transfCdata n
    = [n]


-- ------------------------------------------------------------
-- |
-- converts character references to normal text

transfCharRef           :: XmlFilter
transfCharRef (NTree (XCharRef i) _)
    = xtext $ [toEnum i]

transfCharRef n
    = [n]

-- ------------------------------------------------------------
-- |
-- recursively converts all character references to normal text

transfAllCharRef        :: XmlFilter
transfAllCharRef        = processBottomUp transfCharRef

-- ------------------------------------------------------------

-- |
-- Applies some "Canonical XML" rules to the nodes of a tree.
--
-- The rule differ slightly for canonical XML and XPath in handling of comments
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

canonicalizeTree        :: XmlFilter -> XmlFilter
canonicalizeTree toBeRemoved
    = processChildren (none `when` isXText)
      .>
      processBottomUp canonicalize1Node
      where
      canonicalize1Node :: XmlFilter
      canonicalize1Node
          = (deep isXPi `when` isXDTD)          -- remove DTD parts, except PIs
            .>
            (none `when` toBeRemoved)           -- remove unintersting nodes
            .>
            ( processAttr ( processChildren transfCharRef
                            .>
                            collapseXText
                          ) `when` isXTag)
            .>
            transfCdata                         -- CDATA -> text
            .>
            transfCharRef                       -- Char refs -> text
            .>
            collapseXText                       -- combine text


-- |
-- canonicalize tree and remove comments and \<?xml ... ?> declarations
--
-- see 'canonicalizeTree'

canonicalizeAllNodes    :: XmlFilter
canonicalizeAllNodes
    = canonicalizeTree (isXCmt          -- remove comment
                        +++
                        isPi t_xml      -- remove xml declaration
                       )

-- |
-- Canonicalize a tree for XPath
-- Comment nodes are not removed
--
-- see 'canonicalizeTree'

canonicalizeForXPath    :: XmlFilter
canonicalizeForXPath
    = canonicalizeTree (isPi t_xml)

-- ------------------------------------------------------------

-- |
-- Collects sequences of child XText nodes into one XText node.

collapseXText           :: XmlFilter
collapseXText n
    = replaceChildren (collapseXText' $ getChildren n) n
    where
    collapseXText' :: XmlSFilter
    collapseXText' ((NTree n1 _) : (NTree n2 _) : zs)
        | isXTextNode n1 && isXTextNode n2
            = collapseXText' $ (xtext (t1 ++ t2)) ++ zs
              where
              t1 = textOfXNode n1
              t2 = textOfXNode n2

    collapseXText' (x:xs)
        = x : (collapseXText' xs)

    collapseXText' []
        = []


-- |
-- Applies collapseXText recursively.
--
--
-- see also : 'collapseXText'

collapseAllXText        :: XmlFilter
collapseAllXText        = processBottomUp collapseXText


-- ------------------------------------------------------------
--

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
-- input is a complete document tree.
-- result the semantically equivalent formatted tree.
--
--
-- see also : 'removeDocWhiteSpace'

indentDoc       :: XmlFilter
indentDoc
    = modifyChildren (indentRootChildren $$)
      `when`
      isRoot
      where
      indentRootChildren        :: XmlFilter
      indentRootChildren
          = insertNL `o` indentChild `o` removeText
            where
            removeText
                = none `when` isXText
            insertNL
                = this +++ txt "\n"
            indentChild
                = modifyChildren (indentTrees (insertIndentation 2) False 1)
                  `whenNot`
                  isXDTD                -- DTD elements are not indented

-- |
-- filter for removing all not significant whitespace.
--
-- the tree traversed for removing whitespace between tags,
-- that was inserted for indentation and readability.
-- whitespace is only removed at places, where it's not significat
-- preserving whitespace may be controlled in a document tree
-- by a tag attribute @xml:space@
--
-- allowed values for this attribute are @default | preserve@
--
-- input is root node of the document to be cleaned up
-- output the semantically equivalent simplified tree
--
--
-- see also : 'indentDoc', 'removeAllWhiteSpace'

removeDocWhiteSpace     :: XmlFilter
removeDocWhiteSpace
    =  processChildren processRootElement
      `when`
      isRoot
      where
      processRootElement        :: XmlFilter
      processRootElement
          = removeText .> processChild
            where
            removeText
                = none `when` isWhiteSpace
            processChild
                = choice [ isXDTD
                           :-> removeAllWhiteSpace                      -- whitespace in DTD is redundant
                         , this
                           :-> modifyChildren                           -- same strategie as for indentation
                                 (indentTrees insertNothing False 1)
                         ]

indentTrees     :: (Int -> XmlFilter) -> Bool -> Int -> XmlTrees -> XmlTrees
indentTrees _ _ _ []
    = []
indentTrees indentFilter preserveSpace level ts
    = (lsf $$ ls) ++ indentRest rs
      where
      (ls, rs)
          = break (satisfies isXTag) ts

      isSignificant     :: Bool
      isSignificant
          = preserveSpace
            ||
            any (satisfies isSignificantPart) ls

      isSignificantPart :: XmlFilter
      isSignificantPart
          = cat [ isXText `guards` neg isWhiteSpace
                , isXCdata
                , isXCharRef
                , isXEntityRef
                ]

      lsf       :: XmlFilter
      lsf
          | isSignificant
              = this
          | otherwise
              = (none `when` isWhiteSpace)
                .>
                (indentFilter level +++ this)

      indentRest        :: XmlSFilter

      indentRest []
          | isSignificant
              = []
          | otherwise
              = indentFilter (level - 1) (mkXTextTree "")

      indentRest (t':ts')
              = ( (modifyChildren indentChildren .> lsf)
                  `when`
                  isXTag
                  $ t'
                )
                ++
                ( if null ts'
                  then indentRest ts'
                  else indentTrees indentFilter preserveSpace level ts'
                )
                where
                xmlSpaceAttrName  = "xml:space"
                xmlSpaceAttrValue = valueOf xmlSpaceAttrName t'
                preserveSpace'
                    = fromMaybe preserveSpace
                      .
                      lookup xmlSpaceAttrValue
                      $ [ ("preserve", True)
                        , ("default",  False)
                        ]
                indentChildren
                    | all (satisfies isWhiteSpace) $ getChildren t'
                        = (none $$)
                    | otherwise
                        = indentTrees indentFilter preserveSpace' (level + 1)

-- filter for indenting tags

insertIndentation       :: Int -> Int -> XmlFilter
insertIndentation indentWidth level
    = txt ('\n' : replicate (level * indentWidth) ' ')

-- filter for removing all whitespace

insertNothing           :: Int -> XmlFilter
insertNothing _         = none

-- ------------------------------------------------------------

-- |
-- escape XmlText,
-- transform all special XML chars into CharRefs

escapeString    :: (Char -> Bool) -> String -> XmlTrees
escapeString _isEsc []
    = []
escapeString isEsc (c:s1)
    | isEsc c
        = mkXCharRefTree (fromEnum c) : escapeString isEsc s1
escapeString isEsc s
    = mkXTextTree s1 : escapeString isEsc s2
      where
      (s1, s2) = break isEsc s

escapeText              :: (Char -> Bool) -> XmlFilter
escapeText isEsc (NTree n _)
    | isXTextNode n
        = escapeString isEsc (textOfXNode n)

escapeText isEsc (NTree (XCmt c) _)
    = xcmt (xshow . escapeString isEsc $ c)

escapeText _ t
    = [t]

-- |
-- convert the special XML chars in a text or comment node
-- into character references
--
-- see also 'escapeXmlDoc'

escapeXmlText           :: XmlFilter
escapeXmlText           = escapeText (`elem` "<&")      -- no escape for ", ' and > required: XML standard 2.4

-- |
-- convert the special XML chars in an attribute value into
-- charachter references. Not only the XML specials but also \\n, \\r and \\t are converted
--
-- see also: 'escapeXmlDoc', 'escapeXmlText'

escapeXmlAttrValue      :: XmlFilter
escapeXmlAttrValue      = escapeText (`elem` "<>\"\'&\n\r\t")

-- |
-- convert the special XML chars \", \<, \>, & and \' in a document to char references,
-- attribute values are converted with 'escapeXmlAttrValue'
--
-- see also: 'escapeXmlText', 'escapeXmlAttrValue'

escapeXmlDoc            :: XmlFilter
escapeXmlDoc
    = choice [ isXTag  :-> (processChildren (escapeXmlDoc)
                            .>
                            processAttr escVal
                           )
             , isXText :-> escapeXmlText
             -- , isXCmt  :-> escapeXmlText -- no escape in comments reqired: XML standard 2.4
             , isXDTD  :-> processTopDown escDTD
             , this    :-> this
             ]
      where
      escVal   = processChildren escapeXmlAttrValue
      escDTD   = escVal `when` (isEntity +++ isParameterEntity)

-- ------------------------------------------------------------

-- |
-- convert a document tree into an output string representation
-- with respect to the output encoding.
--
-- The children of the document root are stubstituted by
-- a single text node for the text representation of the document.
--
-- Encoding of the document is performed with respect
-- to the @output-encoding@ attribute in the root node, or if not present,
-- of the @encoding@ attribute for the original input encoding.
-- If the encoding is not specified or not supported, UTF-8 is taken.

unparseXmlDoc   :: XmlFilter
unparseXmlDoc n
    = modifyChildren ((modifyText encFct $$) . xmlTreesToText) n
    where
    encFct = fromMaybe id (getOutputEncodingFct (encSpec n))

encSpec :: XmlTree -> String
encSpec n
    = head . filter (not . null)
      $ [ valueOf a_output_encoding n
        , valueOf a_encoding n
        , utf8
        ]

-- ------------------------------------------------------------
--
-- add or modify a XML directive for a document
-- for specifying the encoding scheme

addXmlPiToDoc           :: XmlFilter
addXmlPiToDoc n
    = ( modifyChildren addX                             -- add <?xml ...>, if neccessary
        `whenNot`
        (getChildren .> isPi t_xml)
      )
      .>
      processChildren ( addAttr a_encoding enc          -- set or replace encoding="..."
                        `when`
                        isPi t_xml
                      )
      $ n
    where
    enc = encSpec n
    addX cs = mkXmlDeclTree (xattr a_version "1.0") : xtext "\n" ++ cs

-- ------------------------------------------------------------

-- |
-- convert a document into a text and add line numbers to the text representation.
--
-- Result is a root node with a single text node as child.
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'treeRepOfXmlDoc'

numberLinesInXmlDoc     :: XmlFilter
numberLinesInXmlDoc
    = modifyChildren ((modifyText numberLines $$) . xmlTreesToText)

numberLines     :: String -> String
numberLines str
    = concat $
      zipWith (\ n l -> lineNr n ++ l ++ "\n") [1..] (lines str)
      where
      lineNr    :: Int -> String
      lineNr n  = (reverse (take 6(reverse (show n) ++ replicate 6 ' '))) ++ "  "

-- ------------------------------------------------------------

-- |
-- convert a document into a text representation in tree form.
--
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'numberLinesInXmlDoc'

treeRepOfXmlDoc         :: XmlFilter
treeRepOfXmlDoc
    = rootTag [getAttrl] [formatXmlContents]

-- |
-- convert a document into a Haskell representation (with show).
--
-- Useful for debugging and trace output.
-- see also : 'treeRepOfXmlDoc', 'numberLinesInXmlDoc'

haskellRepOfXmlDoc      :: XmlFilter
haskellRepOfXmlDoc n
    = rootTag [getAttrl] [txt $ show n] n

-- ------------------------------------------------------------

addHeadlineToXmlDoc     :: XmlFilter
addHeadlineToXmlDoc n
    = replaceChildren (xtext title ++ getChildren n ++ xtext "\n") n
      where
      headline  = "content of: " ++ valueOf a_source n
      underline = map (\_ -> '=') headline
      title     = "\n" ++ headline ++ "\n" ++ underline ++ "\n\n"

-- ------------------------------------------------------------
