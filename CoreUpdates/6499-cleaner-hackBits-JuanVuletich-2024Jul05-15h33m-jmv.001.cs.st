'From Cuis7.1 [latest update: #6498] on 5 July 2024 at 3:34:11 pm'!

!Form methodsFor: 'private' stamp: 'jmv 7/5/2024 15:22:29'!
hackBits64: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 8 wide, 8 deep, and bitThing-size-in-words high."
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	depth := 8.
	width := 8.
	height := bitThing byteSize // width.
	bits := bitThing! !

!Form methodsFor: 'private' stamp: 'jmv 7/5/2024 15:22:23'!
hackBits: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high."
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	depth := 8.
	width := 4.
	height := bitThing byteSize // width.
	bits := bitThing! !

