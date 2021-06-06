'From Cuis 5.0 [latest update: #4630] on 6 June 2021 at 7:13:11 pm'!

!Editor methodsFor: 'new selection' stamp: 'jmnv 6/6/2021 19:12:36'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."
	^ self
		wordRangeLeftDelimiters: leftDelimiters
		rightDelimiters: rightDelimiters 
		startingAt: self pointIndex.! !

