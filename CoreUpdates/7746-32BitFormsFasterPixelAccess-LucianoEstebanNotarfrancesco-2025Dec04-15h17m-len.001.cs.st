'From Cuis7.5 [latest update: #7739] on 4 December 2025 at 3:33:22 pm'!

!Form methodsFor: 'pixel access' stamp: 'len 4/Dec/2025 15:30:19'!
pixelValueAt: aPoint
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	depth = 32 ifTrue: [^ bits basicAt: aPoint y * width + aPoint x + 1].
	^ self primPixelValueAtX: aPoint x y: aPoint y! !

!Form methodsFor: 'pixel access' stamp: 'len 4/Dec/2025 15:30:46'!
pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	depth = 32 ifTrue: [^ bits basicAt: aPoint y * width + aPoint x + 1 put: pixelValue].
	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue! !


!Bitmap reorganize!
('accessing' atAllPut: bitPatternForDepth: byteAt:bigEndian: byteAt:put:bigEndian: bytesAt: bytesAt:put: defaultElement integerAt: integerAt:put: pixelValueForDepth: primFill: replaceFrom:to:with:startingAt:)
('converting' asByteArray)
('printing' printOn:)
('testing' isColormap)
!

