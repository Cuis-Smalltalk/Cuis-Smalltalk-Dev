'From Cuis 5.0 [latest update: #4460] on 19 November 2020 at 1:17:25 pm'!

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 11/19/2020 11:42:10'!
storeSmall1BitBitsOn: aStream
	"Only valid for 1 bit narrow Forms."

	| shift |
	shift _ 32 - width.
	bits do: [ :word | 
		"Print binary with radix, but padded, so the bit pattern is easy to see."
		aStream newLineTab: 2.
		aStream nextPut: $2. 
		aStream nextPut: $r. 
		word >> shift printOn: aStream base: 2 length: width padded: true ]! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 11/19/2020 11:40:21'!
storeSmall1BitOn: aStream
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."
	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'small1BitExtent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'pixelBits: #('.
	self storeSmall1BitBitsOn: aStream.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).
! !

!Form methodsFor: 'private' stamp: 'jmv 11/19/2020 11:03:44'!
fillSmall1BitWith: pixelBits
	"Fill a narrow 1 bit Form.
	Each value in argument holds pixels (i.e. bits) for one line.
	Useful for Cursors and small icons."

	| shift |
	self assert: depth = 1.
	self assert: width <= 32.						"meaning self wordsPerLine = 1."
	shift _ 32 - width.
	1 to: (height min: pixelBits size) do: [ :i |
		bits at: i put: (pixelBits at: i) << shift ].! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 11/19/2020 12:53:49'!
small1BitExtent: extentPoint pixelBits: pixelBits
	"Answer an instance of me of depth 1 with bitmap initialized from pixelBits.
	Requested width can be no more than 32. Result has one 32 bit word per line."

	^ (self extent: extentPoint depth: 1)
		fillSmall1BitWith: pixelBits! !

!Form class methodsFor: 'instance creation' stamp: 'jmv 11/19/2020 11:21:04'!
small1BitExtent: extentPoint pixelBits: pixelBits offset: offsetPoint
	"Answer an instance of me of depth 1 with bitmap initialized from pixelBits.
	Requested width can be no more than 32. Result has one 32 bit word per line."

	^ (self extent: extentPoint depth: 1)
		offset: offsetPoint;
		fillSmall1BitWith: pixelBits! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 11/19/2020 12:34:21'!
storeOn: aStream base: anInteger 
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	(depth = 1 and: [ width <= 32 ]) ifTrue: [
		^self storeSmall1BitOn: aStream ].

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'depth: '.
	self nativeDepth printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn: aStream base: anInteger.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).
! !


!CursorWithMask methodsFor: 'mask' stamp: 'jmv 11/19/2020 11:45:59'!
storeOn: aStream base: anInteger

	aStream nextPut: $(.
	super storeOn: aStream base: anInteger.
	aStream newLine; nextPutAll: ' setMaskForm: '.
	maskForm storeOn: aStream base: anInteger.
	aStream nextPut: $)! !

