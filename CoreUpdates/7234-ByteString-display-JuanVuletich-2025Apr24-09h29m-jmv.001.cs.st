'From Cuis7.3 [latest update: #7146] on 24 April 2025 at 9:30:55 am'!

!VectorEngine methodsFor: 'private' stamp: 'jmv 4/24/2025 09:29:51'!
displayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours |
	nextGlyphX := destX.
	startIndex to: stopIndex do: [ :charIndex | | latin1Code i |
		latin1Code := (aString at: charIndex) codePoint.
		i := contourDataIndexes at: latin1Code + 1.
		i < 1 ifTrue: [ i := 1 ].
		advanceWidth := contourData at: i. i := i + 1.
		boundsLeft := contourData at: i. i := i + 1.
		boundsRight := contourData at: i. i := i + 1.
		boundsBottom := contourData at: i. i := i + 1.
		boundsTop := contourData at: i. i := i + 1.
		numContours := (contourData at: i) asInteger. i := i + 1.
		i := self glyphContours: numContours atx: nextGlyphX y: destY scalex: sx y: sy contourData: contourData i: i.
		nextGlyphX := nextGlyphX + (advanceWidth * sx) ].
	^nextGlyphX! !

