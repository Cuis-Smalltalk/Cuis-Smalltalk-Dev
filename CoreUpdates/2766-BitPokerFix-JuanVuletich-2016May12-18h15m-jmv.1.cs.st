'From Cuis 4.2 of 25 July 2013 [latest update: #2765] on 12 May 2016 at 6:24:30.735554 pm'!

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 5/12/2016 18:18'!
bitPeekerFromForm: sourceForm
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord answer |
	pixPerWord _ 32 // sourceForm depth.
	answer _ self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: nil 			"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: (0@0 extent: pixPerWord@1).
	"To ensure no colormap set"
	answer sourceForm: sourceForm.
	^ answer! !

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 5/12/2016 18:24'!
bitPokerToForm: destForm
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord answer |
	pixPerWord _ 32//destForm depth.
	answer _ self destForm: destForm
	 	sourceForm: nil 		"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: (pixPerWord-1)@0
		extent: 1@1
		clipRect: (0@0 extent: destForm extent).
	"To ensure no colormap set"
	answer sourceForm: (Form extent: pixPerWord@1 depth: destForm depth).
	^ answer! !


!Form methodsFor: 'pixel access' stamp: 'jmv 5/12/2016 18:19'!
primPixelValueAtX: x y: y
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth.
	Typical clients use colorAt: to get a Color"

	<primitive: 'primitivePixelValueAt' module:'BitBltPlugin'>
	^(BitBlt bitPeekerFromForm: self) pixelAt: x@y! !

