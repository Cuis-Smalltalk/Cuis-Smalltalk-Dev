'From Cuis 5.0 of 7 November 2016 [latest update: #3226] on 4 December 2017 at 10:51:49 am'!

!Form methodsFor: 'private' stamp: 'jmv 12/4/2017 10:09:15'!
hackBits64: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 8 wide, 8 deep, and bitThing-size-in-words high."
	width _ 8.
	depth _ 8.
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	bitThing class isBytes
		ifTrue: [height _ bitThing basicSize // 8]
		ifFalse: [height _ bitThing basicSize // 2].
	bits _ bitThing! !


!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 12/4/2017 10:14:38'!
swapBytesIn64BitWords: aNonPointerThing
	"Perform a bigEndian/littleEndian byte reversal of my 64 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] copy.
	BitBlt swapBytesIn64BitWords: ba.
	ba
	"

	self swapBytesIn64BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 12/4/2017 10:13:45'!
swapBytesIn64BitWords: aNonPointerThing from: start to: stop
	"Perform a bigEndian/littleEndian byte reversal of my 64 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits64: aNonPointerThing.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 7"
	blt sourceX: 0; destX: 7; copyBits.
	blt sourceX: 7; destX: 0; copyBits.
	blt sourceX: 0; destX: 7; copyBits.

	"Exchange bytes 1 and 6"
	blt sourceX: 1; destX: 6; copyBits.
	blt sourceX: 6; destX: 1; copyBits.
	blt sourceX: 1; destX: 6; copyBits.

	"Exchange bytes 2 and 5"
	blt sourceX: 2; destX: 5; copyBits.
	blt sourceX: 5; destX: 2; copyBits.
	blt sourceX: 2; destX: 5; copyBits.

	"Exchange bytes 3 and 4"
	blt sourceX: 3; destX: 4; copyBits.
	blt sourceX: 4; destX: 3; copyBits.
	blt sourceX: 3; destX: 4; copyBits.! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 12/4/2017 10:15:23'!
swapHalvesIn64BitWords: aNonPointerThing
	"Swap 32 bit halves in each 64 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] copy.
	BitBlt swapHalvesIn64BitWords: ba.
	ba
	"

	self swapHalvesIn64BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 12/4/2017 10:16:27'!
swapHalvesIn64BitWords: aNonPointerThing from: start to: stop
	"Swap 32 bit halves in each 64 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits64: aNonPointerThing.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 8.

	"Exchange bytes 0123 with 4567"
	blt sourceX: 0; destX: 4; copyBits.
	blt sourceX: 4; destX: 0; copyBits.
	blt sourceX: 0; destX: 4; copyBits! !


!Float64Array methodsFor: 'private' stamp: 'jmv 12/4/2017 10:51:19'!
swapWords
	"This could call #swapHalvesIn64BitWords:"
	| tmp |
	1 to: self size do: [ :i |
		tmp _ self rawBasicAt: i*2.
		self rawBasicAt: i*2 put: (self rawBasicAt: i*2-1).
		self rawBasicAt: i*2-1 put: tmp ]! !

