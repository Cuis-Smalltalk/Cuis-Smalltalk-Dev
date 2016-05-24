'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 17 May 2016 at 4:22:34.850911 pm'!

!Bitmap methodsFor: 'as yet unclassified' stamp: 'jmv 5/17/2016 16:02'!
asByteArray
	"Faster way to make a byte array from me.
	copyFromByteArray:, if receiver is BigEndian makes equal Bitmap.
	Assume receiver bytes-in-word mapping is BigEndian:
		Most significant bye of first word in self goes to first position in result.
	This means that for a BigEndian 8bpp Form, pixels are in the right order in the ByteArray
	
	Form lena asGrayForm bits asByteArray copyFrom: 1 to: 4.
	(Form lena asGrayForm asFormOfDepth: 8) bits asByteArray copyFrom: 1 to: 4.
	(0 to: 3) collect: [ :x | ((Form lena asGrayForm colorAt: x@0) luminance * 255) rounded ].
	"
	| f bytes hack |
	f _ Form extent: 4@self size depth: 8 bits: self.
	bytes _ ByteArray new: self size * 4.
	hack _ Form new hackBits: bytes.
	Smalltalk isLittleEndian ifTrue: [hack swapEndianness].
	hack copyBits: f boundingBox
		from: f
		at: (0@0)
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes! !


!Form methodsFor: 'private' stamp: 'jmv 5/17/2016 16:01'!
copyFromByteArray2: bigEndianByteArray to: i
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at:  0@i! !

!Form methodsFor: 'private' stamp: 'jmv 5/17/2016 16:21'!
copyFromByteArray: bigEndianByteArray
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray2:to:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack.
! !

