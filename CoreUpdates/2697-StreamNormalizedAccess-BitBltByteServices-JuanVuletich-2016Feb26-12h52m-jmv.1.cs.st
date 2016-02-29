'From Cuis 4.2 of 25 July 2013 [latest update: #2690] on 26 February 2016 at 12:56:35.554731 pm'!

!ByteArray commentStamp: '<historical>' prior: 0!
I represent an ArrayedCollection whose elements are integers between 0 and 255.

We have a literal syntax:
	#[1 2 3 4]!


!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:48'!
swapBytesIn16BitHalfWords: aNonPointerThing
	"Perform a bigEndian/littleEndian byte reversal of my 16 bit short words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.
	
	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapBytesIn16BitHalfWords: ba.
	ba
	"

	self swapBytesIn16BitHalfWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:14'!
swapBytesIn16BitHalfWords: aNonPointerThing from: start to: stop
	"Perform a bigEndian/littleEndian byte reversal of my 16 bit short words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits: aNonPointerThing.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 1"
	blt sourceX: 0; destX: 1; copyBits.
	blt sourceX: 1; destX: 0; copyBits.
	blt sourceX: 0; destX: 1; copyBits.

	"Exchange bytes 2 and 3"
	blt sourceX: 2; destX: 3; copyBits.
	blt sourceX: 3; destX: 2; copyBits.
	blt sourceX: 2; destX: 3; copyBits.! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:49'!
swapBytesIn32BitWords: aNonPointerThing
	"Perform a bigEndian/littleEndian byte reversal of my 32 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapBytesIn32BitWords: ba.
	ba
	"

	self swapBytesIn32BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:19'!
swapBytesIn32BitWords: aNonPointerThing from: start to: stop
	"Perform a bigEndian/littleEndian byte reversal of my 32 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits: aNonPointerThing.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 3"
	blt sourceX: 0; destX: 3; copyBits.
	blt sourceX: 3; destX: 0; copyBits.
	blt sourceX: 0; destX: 3; copyBits.

	"Exchange bytes 1 and 2"
	blt sourceX: 1; destX: 2; copyBits.
	blt sourceX: 2; destX: 1; copyBits.
	blt sourceX: 1; destX: 2; copyBits.! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:50'!
swapHalvesIn32BitWords: aNonPointerThing
	"Swap 16 bit halves in each 32 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapHalvesIn32BitWords: ba.
	ba
	
	| sb |
	sb := SoundBuffer new: 12.
	1 to: sb size do: [ :i | sb at: i put: i ].
	BitBlt swapHalvesIn32BitWords: sb.
	sb
	"

	self swapHalvesIn32BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize! !

!BitBlt class methodsFor: 'byte and word utilities' stamp: 'jmv 2/25/2016 10:21'!
swapHalvesIn32BitWords: aNonPointerThing from: start to: stop
	"Swap 16 bit halves in each 32 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits: aNonPointerThing.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 2.

	"Exchange bytes 0&1 with 2&3"
	blt sourceX: 0; destX: 2; copyBits.
	blt sourceX: 2; destX: 0; copyBits.
	blt sourceX: 0; destX: 2; copyBits! !


!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 2/24/2016 16:11'!
floatAt: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self unsignedLongAt: index bigEndian: bool.
	^ Float fromIEEE32Bit: w! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 2/24/2016 16:12'!
floatAt: index put: value bigEndian: bool 
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w _ value asIEEE32BitWord.
	self unsignedLongAt: index put: w bigEndian: bool.
	^ value! !


!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:42'!
nextDouble64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ])
			readStream nextDouble64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ])
			readStream nextDouble64BigEndian: true
	"
	| bytes |
	bytes _ self next: 8.
	^ bytes doubleAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:40'!
nextDouble64Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 8.
	bytes doubleAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:38'!
nextFloat32BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ])
			readStream nextFloat32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ])
			readStream nextFloat32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes floatAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:34'!
nextFloat32Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ]) hex 'DB0F4940'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ]) hex '40490FDB'
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes floatAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt16BigEndian: bigEndian
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) 
			readStream nextSignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) 
			readStream nextSignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes shortAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes shortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:26'!
nextSignedInt32BigEndian: bigEndian
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) 
			readStream nextSignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) 
			readStream nextSignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes longAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextSignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes longAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:18'!
nextUnsignedInt16BigEndian: bigEndian
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) 
			readStream nextUnsignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) 
			readStream nextUnsignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes unsignedShortAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextUnsignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes unsignedShortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:19'!
nextUnsignedInt32BigEndian: bigEndian
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) 
			readStream nextUnsignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) 
			readStream nextUnsignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes unsignedLongAt: 1 bigEndian: bigEndian! !

!PositionableStream methodsFor: 'normalized access' stamp: 'jmv 2/25/2016 11:27'!
nextUnsignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes unsignedLongAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !


!ArrayedCollection methodsFor: 'objects from disk' stamp: 'jmv 2/25/2016 10:26'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^ self].

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]! !

!methodRemoval: Bitmap class #swapBytesIn:from:to:!
Bitmap class removeSelector: #swapBytesIn:from:to:!
!methodRemoval: ArrayedCollection #swapHalves!
ArrayedCollection removeSelector: #swapHalves!

!BitBlt class reorganize!
('instance creation' bitPeekerFromForm: bitPokerToForm: destForm:sourceForm:combinationRule:destOrigin:sourceOrigin:extent:clipRect: destForm:sourceForm:fillColor:combinationRule:destOrigin:sourceOrigin:extent:clipRect: toForm:)
('examples' alphaBlendDemo exampleColorMap)
('cached state access' releaseClassCachedState)
('byte and word utilities' swapBytesIn16BitHalfWords: swapBytesIn16BitHalfWords:from:to: swapBytesIn32BitWords: swapBytesIn32BitWords:from:to: swapHalvesIn32BitWords: swapHalvesIn32BitWords:from:to:)
!


!Bitmap class reorganize!
('instance creation' newFromStream:)
!


!ByteArray reorganize!
('accessing' atAllPut: byteAt: byteAt:put: byteSize bytesPerElement indexOf:startingAt:)
('platform independent access' doubleAt:bigEndian: doubleAt:put:bigEndian: floatAt:bigEndian: floatAt:put:bigEndian: longAt:bigEndian: longAt:put:bigEndian: shortAt:bigEndian: shortAt:put:bigEndian: unsignedLongAt:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:bigEndian: unsignedShortAt:put:bigEndian:)
('converting' asByteArray asString base64Encoded hex)
('private' defaultElement replaceFrom:to:with:startingAt:)
('comparing' hash)
('printing' printOn: storeOn:)
('testing' isLiteral)
('initialization' readHexFrom:)
!


!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isCharacters isEmpty notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: copyMethodChunkFrom: fileIn fileInAnnouncing: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('nonhomogeneous accessing' nextInt32 nextInt32Put: nextLittleEndianNumber: nextLittleEndianNumber:put: nextNumber nextNumber: nextNumber:put: nextString nextStringOld nextStringPut: nextWord nextWordPut:)
('data get/put' boolean boolean: int16 int16: int32 int32: string string: uint16 uint16: uint24 uint24: uint32 uint32:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
('normalized access' nextDouble64BigEndian: nextDouble64Put:bigEndian: nextFloat32BigEndian: nextFloat32Put:bigEndian: nextSignedInt16BigEndian: nextSignedInt16Put:bigEndian: nextSignedInt32BigEndian: nextSignedInt32Put:bigEndian: nextUnsignedInt16BigEndian: nextUnsignedInt16Put:bigEndian: nextUnsignedInt32BigEndian: nextUnsignedInt32Put:bigEndian:)
!

