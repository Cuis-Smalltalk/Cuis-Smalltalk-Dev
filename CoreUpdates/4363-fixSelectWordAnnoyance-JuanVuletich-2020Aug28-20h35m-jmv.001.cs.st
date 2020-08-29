'From Cuis 5.0 [latest update: #4362] on 28 August 2020 at 8:36:03 pm'!

!Editor methodsFor: 'new selection' stamp: 'jmv 8/28/2020 20:35:53'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	^ self
		wordRangeLeftDelimiters: leftDelimiters
		rightDelimiters: rightDelimiters 
		startingAt: self pointIndex + self markIndex // 2.! !

