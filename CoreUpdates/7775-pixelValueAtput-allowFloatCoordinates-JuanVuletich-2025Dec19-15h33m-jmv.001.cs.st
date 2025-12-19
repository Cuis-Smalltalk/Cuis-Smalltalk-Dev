'From Cuis7.5 [latest update: #7774] on 19 December 2025 at 3:54:20 pm'!

!Form methodsFor: 'pixel access' stamp: 'jmv 19/Dec/2025 15:52:32'!
pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	aPoint isIntegerPoint ifFalse: [
		^self pixelValueAt: aPoint asIntegerPoint put: pixelValue ].
	depth = 32 ifTrue: [^ bits basicAt: aPoint y * width + aPoint x + 1 put: pixelValue].
	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue! !

