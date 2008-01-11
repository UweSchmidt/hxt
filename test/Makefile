

mergeAtest	: ArrowMerge.hs
	for i in test{1,2,3,4,5}; do ghc -e $$i ArrowMerge.hs; done


spanTest	: TestHtmlPi.hs hw2.html
	runghc TestHtmlPi.hs hw2.html -
