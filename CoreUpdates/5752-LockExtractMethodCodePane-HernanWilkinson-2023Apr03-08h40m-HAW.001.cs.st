'From Cuis 6.0 [latest update: #5738] on 3 April 2023 at 8:45:09 am'!

!ExtractMethodReplacementsWindow methodsFor: 'as yet unclassified' stamp: 'HAW 4/3/2023 08:43:25'!
createCodePaneMorph

	^super createCodePaneMorph
		lock;
		yourself! !


!ExtractMethodMessageSet methodsFor: 'source code ranges' stamp: 'HAW 4/3/2023 08:42:58'!
messageSendsRangesOf: aSelector

	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^Array with: replacement intervalToExtract.! !

