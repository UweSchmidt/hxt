module Main
where

import Text.XML.HXT.Arrow

-- ein einfacher Kombinator, der die beiden Ergebnisse konkateniert

mergeA		:: (ArrowList a) => a (b, b) b
mergeA		= arrL $ runLA (arr fst <+> arr snd)

-- ein verallgemeinerter Kombinator,
-- der einen 2-stelligen arrow als parameter hat

-- mergeA = mergeA' (<+>)

mergeA'		:: (ArrowList a) =>
                   (LA (a1, b1) a1 -> LA (a1, b1) b1 -> LA (a1, b1) c) -> a (a1, b1) c
mergeA' op	= arrL $ runLA (arr fst `op` arr snd)

-- zwei kleine Tests

test1 = runLA
	( xshow				-- nur für die lesbare Ausgabe
	  ( mkText			-- Text Knoten
	    >>>
	    ( selem "foo" [this]	-- foo element bauen
	      &&&			-- 2 mal verarbeiten
	      selem "bar" [txt "23"]	-- bar element bauen
	    )
	    >>> mergeA' (<+>)		-- paar mischen zu einer Sequenz
	  )
	) $ "42"

test2 = runLA
	( xshow
	  ( mkText
	    >>>
	    ( selem "foo" [this] &&& selem "bar" [txt "23"] )
	    >>> mergeA' (+=)		-- wie oben, nur bar kommt in den Inhalt von foo rein
	  )
	) $ "42"

-- das gleiche mit $<

mergeX	:: (ArrowList a) => a (b, b) b
mergeX	= (\ x -> arr fst <+> constA (snd x)) $< this

mergeX'		:: (ArrowList a) =>
                   (a (a1, b1) a1 -> a (a1, b1) b1 -> a (a1, b1) c) -> a (a1, b1) c

mergeX' op	= (\ x -> arr fst `op` constA (snd x)) $< this


test3 = runLA
	( xshow
	  ( mkText
	    >>>
	    ( selem "foo" [this] &&& selem "bar" [txt "23"] )
	    >>> mergeX' (<+>)
	  )
	) $ "42"

test4 = runLA
	( xshow
	  ( mkText
	    >>>
	    ( selem "foo" [this] &&& selem "bar" [txt "23"] )
	    >>> mergeX' (+=)
	  )
	) $ "42"

test5 = runLA
	( 
	  ( mkText
	    >>>
	    ( (selem "foo" [this] <+> selem "baf" [this]) &&& (selem "bar" [txt "23"] <+> txt "49") )
	    >>> mergeX' (+=)
	    >>> xshow this
	  )
	) $ "42"

