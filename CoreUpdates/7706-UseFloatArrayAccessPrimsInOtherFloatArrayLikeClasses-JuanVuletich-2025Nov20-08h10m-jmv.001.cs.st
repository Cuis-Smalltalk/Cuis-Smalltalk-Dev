'From Cuis7.5 [latest update: #7703] on 20 November 2025 at 8:14:37 am'!

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 11/20/2025 08:12:41'!
at: index
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 11/20/2025 08:13:25'!
at: index put: value
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !


!Homography methodsFor: 'private access' stamp: 'jmv 11/20/2025 08:12:47'!
at: index
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Homography methodsFor: 'private access' stamp: 'jmv 11/20/2025 08:13:20'!
at: index put: value
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !


!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 11/20/2025 08:12:17'!
float32SlotAt: index
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 11/20/2025 08:13:13'!
float32SlotAt: index put: value
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self float32SlotAt: index put: value asFloat ].
	^value! !

