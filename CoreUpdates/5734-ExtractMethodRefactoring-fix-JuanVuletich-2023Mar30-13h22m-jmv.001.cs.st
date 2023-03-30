'From Cuis 6.0 [latest update: #5733] on 30 March 2023 at 1:22:39 pm'!

!ExtractMethodMessageSet methodsFor: 'source code ranges' stamp: 'jmv 3/30/2023 13:22:03'!
messageSendsRangesOf: aSelector

	| replacement |
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	replacement methodToExtractFrom == currentCompiledMethod ifFalse: [ ^#() ].
	^Array with: replacement intervalToExtract.! !

