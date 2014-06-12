'From Cuis 4.2 of 25 July 2013 [latest update: #2009] on 6 June 2014 at 10:20:04.547568 pm'!

!Form methodsFor: 'pixel access' stamp: 'tpr 3/26/2013 11:10'!
primPixelValueAtX: x y: y
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color. Make sure the colorMap is nil for ColorForms "

	<primitive: 'primitivePixelValueAt' module:'BitBltPlugin'>
	^(BitBlt bitPeekerFromForm: self) colorMap: nil;  pixelAt: x@y! !


!Form methodsFor: 'pixel access' stamp: 'tpr 3/14/2013 17:36'!
pixelValueAt: aPoint
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	^ self primPixelValueAtX: aPoint x y: aPoint y! !

!methodRemoval: ColorForm #pixelValueAt:!
ColorForm removeSelector: #pixelValueAt:!
