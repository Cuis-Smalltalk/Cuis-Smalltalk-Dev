'From Cuis 6.0 [latest update: #5943] on 24 July 2023 at 3:57:53 pm'!

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:56:17'!
isExtractingHole: statement

	| ranges |
		
	ranges := completeSourceRanges at: statement ifAbsent: [^false].
	^ranges anySatisfy: [ :range | range = intervalToExtract ]! !

