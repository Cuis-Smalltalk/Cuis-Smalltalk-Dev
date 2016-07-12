'From Cuis 4.2 of 25 July 2013 [latest update: #2845] on 11 July 2016 at 4:50:25 pm'!

!Form methodsFor: 'private' stamp: 'jmv 7/11/2016 16:41:01'!
copyFromByteArray2: bigEndianByteArray to: i
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: 0@i rule: Form over! !

!Form methodsFor: 'private' stamp: 'jmv 7/11/2016 16:45:49'!
copyFromByteArray: bigEndianByteArray
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray2:to:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: 0 @ 0 rule: Form over! !

