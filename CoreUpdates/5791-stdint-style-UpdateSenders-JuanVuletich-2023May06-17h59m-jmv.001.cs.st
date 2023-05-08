'From Cuis 6.0 [latest update: #5789] on 6 May 2023 at 6:02:51 pm'!

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 20:39:18'!
floatSlotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self float32SlotAt: index put: value asFloat ].
	^value! !


!Color methodsFor: 'access' stamp: 'jmv 5/5/2023 17:30:34'!
blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 3! !

!Color methodsFor: 'access' stamp: 'jmv 5/5/2023 17:30:48'!
green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 2! !

!Color methodsFor: 'access' stamp: 'jmv 5/5/2023 17:31:07'!
red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 1! !

!Color methodsFor: 'conversions' stamp: 'jmv 5/5/2023 17:31:03'!
pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self float32SlotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self float32SlotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self float32SlotAt: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val _ LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue !!= 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val := ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val := ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val := ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString! !

!Color methodsFor: 'queries' stamp: 'jmv 5/5/2023 17:30:53'!
isBlack
	"Return true if the receiver represents black"
	(self float32SlotAt: 1) = 0.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 2) = 0.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 3) = 0.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'queries' stamp: 'jmv 5/5/2023 17:30:58'!
isWhite
	"Return true if the receiver represents white"
	(self float32SlotAt: 1) = 1.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 2) = 1.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 3) = 1.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'private' stamp: 'jmv 5/5/2023 17:31:27'!
basicSetRed: r green: g blue: b
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		float32SlotAt: 1 put: r;
		float32SlotAt: 2 put: g;
		float32SlotAt: 3 put: b! !

!Color methodsFor: 'private' stamp: 'jmv 5/5/2023 17:30:45'!
clipToValidValues
	| v |
	1 to: self size do: [ :i |
		v := self float32SlotAt: i.
		v > 1 ifTrue: [self float32SlotAt: i put: 1.0].
		v < 0 ifTrue: [self float32SlotAt: i put: 0.0]]! !

!Color methodsFor: 'object serialization' stamp: 'jmv 5/6/2023 16:31:12'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	aStream nextInt32Put: self basicSize bigEndian: true.
	aStream nextUint32WordsPutAll: self.! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 5/5/2023 17:15:22'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !


!TranslucentColor methodsFor: 'accessing' stamp: 'jmv 5/5/2023 17:31:10'!
alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self float32SlotAt: 4! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 5/5/2023 17:31:31'!
setRed: r green: g blue: b alpha: alphaValue

	self basicSetRed: r green: g blue: b.
	self float32SlotAt: 4 put: alphaValue! !


!UnicodeString class methodsFor: 'services' stamp: 'jmv 5/5/2023 17:28:40'!
utf8FromUtf16: utf16ByteArray
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present."

	| codePoint isBigEndian startIndex |
	isBigEndian := false. "best guess"
	startIndex := 1.
	(utf16ByteArray uint16At: 1 bigEndian: true) = 16rFEFF ifTrue: [
		isBigEndian := true.
		startIndex := 3 ].
	(utf16ByteArray uint16At: 1 bigEndian: false) = 16rFEFF  ifTrue: [
		isBigEndian := false.
		startIndex := 3 ].
	^ByteArray streamContents: [ :strm |
		startIndex to: utf16ByteArray size by: 2 do: [ :index |
			codePoint := utf16ByteArray uint16At: index bigEndian: isBigEndian.
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]].! !


!ArrayedCollection methodsFor: 'objects from disk' stamp: 'jmv 5/6/2023 16:31:15'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	self class isPointers | self class isWords not ifTrue: [^ super writeOn: aStream].
				"super may cause an error, but will not be called."
	aStream nextInt32Put: self basicSize bigEndian: true.
	aStream nextUint32WordsPutAll: self.! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 5/5/2023 17:15:28'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	self isPointers | self isWords not ifTrue: [^ super newFromStream: s].
		"super may cause an error, but will not be called."

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skipBack.
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !


!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 16:40:25'!
bitAt: bitIndex
	"Answer the bit (0 or 1) at a bit index.
	This way, the receiver behaves as a BitArray.
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8."
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	^ self bitAt: index bitPosition: bitPosition! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 16:40:44'!
bitAt: bitIndex put: aBit
	"Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8.
	#[1 0 0 ] bitAt: 1
	#[0 1  0 ] bitAt: 9
	#[0 0 128 ] bitAt: 24
	"
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	self bitAt: index bitPosition: bitPosition put: aBit! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:37:52'!
doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self uint32At: index bigEndian: bool.
	w2 := self uint32At: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:33:50'!
doubleAt: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self uint32At: index put: w1 bigEndian: bool.
	self uint32At: index + 4 put: w2 bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:37:55'!
floatAt: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self uint32At: index bigEndian: bool.
	^ Float fromIEEE32Bit: w! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:33:53'!
floatAt: index put: value bigEndian: bool 
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w := value asIEEE32BitWord.
	self uint32At: index put: w bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:37:22'!
long64At: index bigEndian: bigEndian
	"Return a 64-bit signed integer quantity starting from the given byte index."

	| value |
	value := self uint64At: index bigEndian: bigEndian.
	value digitLength < 8 ifTrue: [ ^value ].
	(value digitAt: 8) < 16r80 ifTrue: [ ^value ].
	^value - 16r10000000000000000! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/5/2023 20:38:44'!
long64At: index put: value bigEndian: bigEndian
	"Store a 64-bit signed integer quantity starting from the given byte index."
	
	^self
		uint64At: index
		put: (value negative
			ifFalse: [ value ]
			ifTrue: [ value + 16r10000000000000000 ])
		bigEndian: bigEndian! !


!Float64Array methodsFor: 'objects from disk' stamp: 'jmv 5/5/2023 20:10:58'!
writeOn: aStream
	"Store self on the argument, aStream.
	Write bigEndian / PowerPC order."

	aStream nextUint32Put: self size bigEndian: true.
	self do: [ :f |
		aStream nextFloat64Put: f bigEndian: true ]! !


!Float64Array class methodsFor: 'objects from disk' stamp: 'jmv 5/5/2023 17:19:59'!
newFromStream: aStream
	"Read bigEndian / PowerPC order."

	| len answer |
	len := aStream nextUint32BigEndian: true.
	answer := self new: len.
	1 to: len do: [ :i |
		answer at: i put: (aStream nextFloat64BigEndian: true) ].
	^ answer! !


!ShortIntegerArray methodsFor: 'objects from disk' stamp: 'jmv 5/6/2023 16:03:53'!
writeOn: aStream 

	| w |
	aStream nextInt32Put: self basicSize bigEndian: true.

	1 to: self basicSize do: [ :i |
		w := self basicAt: i.
		Smalltalk isLittleEndian
			ifFalse: [ aStream nextUint32Put: w bigEndian: true ]
			ifTrue: [ aStream
				nextPut: (w digitAt: 2);
				nextPut: (w digitAt: 1);
				nextPut: (w digitAt: 4);
				nextPut: (w digitAt: 3) ]].! !


!Bitmap class methodsFor: 'instance creation' stamp: 'jmv 5/5/2023 17:15:35'!
newFromStream: s
	| len |
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self new: len)! !


!RunArray methodsFor: 'printing' stamp: 'jmv 5/5/2023 17:18:16'!
writeOn: aStream

	aStream nextUint16Put: runs size bigEndian: true.
	1 to: runs size do: [ :x |
		aStream nextUint16Put: (runs at: x) bigEndian: true.
		aStream nextUint16Put: (values at: x) bigEndian: true ]! !


!RunArray class methodsFor: 'instance creation' stamp: 'jmv 5/5/2023 17:17:35'!
readFrom: aStream
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size := aStream nextUint16BigEndian: true.
	runs := Array new: size.
	values := Array new: size.
	1 to: size do: [ :x |
		runs at: x put: (aStream nextUint16BigEndian: true).
		values at: x put: (aStream nextUint16BigEndian: true) ].
	^ self runs: runs values: values! !


!SystemDictionary methodsFor: 'image format' stamp: 'jmv 5/5/2023 17:27:51'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ Smalltalk imageName asFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			uint32At: 1
			bigEndian: Smalltalk isBigEndian ]! !


!Stream methodsFor: 'testing' stamp: 'jmv 5/5/2023 20:12:58'!
nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextUint32Put: (aCollection basicAt: i) bigEndian: true ].
	^aCollection! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:37:34'!
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
	bytes := self next: 8.
	^ bytes float64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:37:04'!
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
	bytes := ByteArray new: 8.
	bytes float64At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:26:41'!
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
	bytes := self next: 4.
	^ bytes float32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:28:46'!
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
	bytes := ByteArray new: 4.
	bytes float32At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:22:12'!
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
	bytes := self next: 2.
	^ bytes int16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:36:45'!
nextSignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes int16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:23:22'!
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
	bytes := self next: 4.
	^ bytes int32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:36:21'!
nextSignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes int32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:38:18'!
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
	bytes := self next: 2.
	^ bytes uint16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:38:30'!
nextUnsignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes uint16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:37:59'!
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
	bytes := self next: 4.
	^ bytes uint32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 20:33:56'!
nextUnsignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes uint32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:13:41'!
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextUint32Put: anInteger + 1 bigEndian: true.

	self nextPut: aClass name! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:13:45'!
outputReference: referencePosn
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextUint32Put: referencePosn bigEndian: true	"relative position"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:17'!
readArray
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count := byteStream nextUint32BigEndian: true.

	refPosn := self beginReference: (array := Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:28'!
readByteArray
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count := byteStream nextUint32BigEndian: true.
	^ byteStream next: count  "assume stream is in binary mode"
! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:32'!
readCharacter
    "PRIVATE -- Read a Character."

    ^ Character numericValue: (byteStream nextUint32BigEndian: true)! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 17:12:54'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order."

	^ byteStream nextFloat64BigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:35'!
readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk at: aSymbol asSymbol.
	anObject := newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:18:41'!
readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextInt32BigEndian: true	"signed!!!!!!"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:39'!
readMethod
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	className := self next.
	newClass := Smalltalk at: className asSymbol.

	xxHeader := self next.
		"nArgs _ (xxHeader >> 24) bitAnd: 16rF."
		"nTemps _ (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit _ (xxHeader >> 17) bitAnd: 1."
	nLits := (xxHeader >> 9) bitAnd: 16rFF.
		"primBits _ ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer := instSize - (newClass instSize "0") - (nLits + 1 * Smalltalk wordSize).

	newMethod := newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits := newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*Smalltalk wordSize+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:43'!
readReference
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |

	^ (referencePosition := (byteStream nextUint32BigEndian: true)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:17:23'!
readShortRef
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextUint16BigEndian: true)! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:13:48'!
writeArray: anArray
	"PRIVATE -- Write the contents of an Array."

	byteStream nextUint32Put: anArray size bigEndian: true.
	self nextPutAll: anArray.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:13:51'!
writeByteArray: aByteArray
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextUint32Put: aByteArray size bigEndian: true.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:13:54'!
writeCharacter: aCharacter
	"PRIVATE -- Write a Character."

	byteStream nextUint32Put: aCharacter numericValue bigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 17:13:02'!
writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here.
	Write in bigEndian / PowerPC order."

	byteStream nextFloat64Put: aFloat bigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:19:46'!
writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextInt32Put: anInteger bigEndian: true	"signed!!!!!!!!!!"! !


!ReferenceStream methodsFor: 'writing' stamp: 'jmv 5/5/2023 20:15:22'!
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."
	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.
	In ReferenceStream, class is just 5 bytes for shared symbol.
	SmartRefStream puts out the names and number of class's instances variables for checking.
6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "

	| short ref |
	short := true.	"All tests for object header that can be written in 4 bytes"
	anInteger <= 254 ifFalse: [short := false].	"one byte size"
	ref := references at: aClass name ifAbsent: [short := false. nil].
	ref isInteger ifFalse: [short := false].
	short ifTrue: [short := (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"
	short ifTrue: [
		byteStream skipBack.
		short := byteStream next = 9.
		byteStream skip: 0].	"ugly workaround"
	short 
		ifTrue: ["passed all the tests!!"
			byteStream skipBack; nextPut: 16; "type = short header"
				nextPut: anInteger + 1;	"size is short"
				nextUint16Put: ref bigEndian: true ]
		ifFalse: [
			"default to normal longer object header"
			byteStream nextUint32Put: anInteger + 1 bigEndian: true.
			self nextPut: aClass name].! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 5/5/2023 20:16:46'!
readInstance
	"Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!!  See setStream:"
	| instSize className refPosn |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	className := self next asSymbol.
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn
! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 5/5/2023 20:20:29'!
readAttributesFrom: aBinaryStream
	| offsetX offsetY |
	depth := aBinaryStream next.
	(self depth isPowerOfTwo and: [self depth between: 1 and: 32])
		ifFalse: [self error: 'invalid depth; bad Form file?'].
	width := aBinaryStream nextUint16BigEndian: true.
	height := aBinaryStream nextUint16BigEndian: true.
	offsetX  := aBinaryStream nextInt16BigEndian: true.
	offsetY := aBinaryStream nextInt16BigEndian: true.
	offset := Point x: offsetX y: offsetY! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 5/5/2023 20:21:03'!
writeAttributesOn: file

	file nextPut: depth.
	file nextUint16Put: width bigEndian: true.
	file nextUint16Put: height bigEndian: true.
	file nextInt16Put: self offset x bigEndian: true.
	file nextInt16Put: self offset y bigEndian: true! !


!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 5/5/2023 20:16:50'!
readAttributesFrom: aBinaryStream
	super readAttributesFrom: aBinaryStream.
	colors := ColorArray new: (2 raisedTo: depth).
	1 to: colors size do: [:idx | 
		colors basicAt: idx put: (aBinaryStream nextUint32BigEndian: false) ]! !

!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 5/5/2023 20:14:02'!
writeAttributesOn: file
	| colorArray |
	super writeAttributesOn: file.
	colorArray := self colors asColorArray.
	1 to: (2 raisedTo: depth) do: [:idx |
		file nextUint32Put: (colorArray basicAt: idx) bigEndian: false ]! !


!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 5/5/2023 20:16:54'!
nextUnsignedInt32
	"Read a 32-bit unsigned quantity from the stream.
	Big Endian"

	^ stream nextUint32BigEndian: true! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 5/5/2023 20:14:19'!
nextUnsignedInt32Put: aNumber
	"Write out a 32-bit integer as 32 bits.
	Big Endian"

	stream nextUint32Put: aNumber bigEndian: true.
	^aNumber! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 5/5/2023 20:17:33'!
readHeader
	| reserved |
	bfType := stream nextUint16BigEndian: false.
	bfSize := stream nextUint32BigEndian: false.
	reserved := stream nextUint32BigEndian: false.
	bfOffBits := stream nextUint32BigEndian: false.
	biSize := stream nextUint32BigEndian: false.
	biWidth := stream nextUint32BigEndian: false.
	biHeight := stream nextUint32BigEndian: false.
	biPlanes := stream nextUint16BigEndian: false.
	biBitCount := stream nextUint16BigEndian: false.
	biCompression := stream nextUint32BigEndian: false.
	biSizeImage := stream nextUint32BigEndian: false.
	biXPelsPerMeter := stream nextUint32BigEndian: false.
	biYPelsPerMeter := stream nextUint32BigEndian: false.
	biClrUsed := stream nextUint32BigEndian: false.
	biClrImportant := stream nextUint32BigEndian: false.
! !

!BMPReadWriter methodsFor: 'writing' stamp: 'jmv 5/5/2023 20:15:35'!
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	stream position: 0.
	stream nextUint16Put: 19778 bigEndian: false.  "bfType = BM"
	stream nextUint32Put: bfOffBits + biSizeImage bigEndian: false.  "Entire file size in bytes"
	stream nextUint32Put: 0 bigEndian: false.  "bfReserved"
	stream nextUint32Put: bfOffBits bigEndian: false.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextUint32Put: biSize bigEndian: false.  "info header size in bytes"
	stream nextUint32Put: image width bigEndian: false.  "biWidth"
	stream nextUint32Put: image height bigEndian: false.  "biHeight"
	stream nextUint16Put: 1 bigEndian: false.  "biPlanes"
	stream nextUint16Put: (depth min: 24) bigEndian: false.  "biBitCount"
	stream nextUint32Put: 0 bigEndian: false.  "biCompression"
	stream nextUint32Put: biSizeImage bigEndian: false.  "size of image section in bytes"
	stream nextUint32Put: 2800 bigEndian: false.  "biXPelsPerMeter"
	stream nextUint32Put: 2800 bigEndian: false.  "biYPelsPerMeter"
	stream nextUint32Put: biClrUsed bigEndian: false.
	stream nextUint32Put: 0 bigEndian: false.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [ :i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [ :j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth = 32 ifTrue: [
		"original depth 16 or 32"
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do: [ :i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	] ifFalse: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		scanLineLen := image wordsPerLine * 4.  "# of bytes in line"
		1 to: biHeight do: [ :i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure']! !


!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 5/6/2023 16:31:18'!
writeOn: aStream
	aStream nextUint32WordsPutAll: self.! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/5/2023 20:23:42'!
getTableDirEntry: tagString from: fontData offset: offset
	"Find the table named tagString in fontData and return a table directory entry for it."

	| numTables pos currentTag tag |
	numTables := fontData int16At: 5 + offset bigEndian: true.
	tag := ByteArray new: 4.
	1 to: 4 do: [ :i | tag at: i put: (tagString at: i) asciiValue ].
	tag := tag int32At: 1 bigEndian: true.
	pos := 13 + offset.
	1 to: numTables do:[:i|
		currentTag := fontData int32At: pos bigEndian: true.
		currentTag = tag ifTrue: [^TTFontTableDirEntry on: fontData at: pos].
		pos := pos+16].
	^nil! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:37:18'!
processCharacterMappingTable: entry
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| initialOffset nSubTables platformID platformSpecificEncodingID offset offsets platformIDsAndEncodings orderOfPreference found |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUint16.
	platformIDsAndEncodings := Array new: nSubTables.
	offsets := Array new: nSubTables.
	1 to: nSubTables do: [ :i |
		platformID := entry nextUint16.
		platformSpecificEncodingID := entry nextUint16.
		offset := entry nextUint32.
		platformIDsAndEncodings at: i put: {platformID. platformSpecificEncodingID}.
		offsets  at: i put: offset  ].
	"see, for instance, https://docs.microsoft.com/en-us/typography/opentype/spec/cmap"
	orderOfPreference := 
		#(
			#(0 4)  			"platform=Unicode, encoding=4 (Unicode, full range)"
			#(0 10) 			"platform=Unicode, encoding=10 (Undocumented but widely used. Appears equivalent to #(0 4) or #(3 10))"
			#(3 10)  			"platform=Windows, encoding=10(Unicode, full range), format=12(Segment coverage)"
			#(0 3)  			"platform=Unicode, encoding=3 (Unicode, BMP only)"
			#(3 1)  			"platform=Windows, encoding=1 (Unicode, BMP only)"
			#(0 0)  			"platform=Unicode, encoding=3(Unicode 1.0, deprecated)"
		).
	orderOfPreference do: [ :preferredPlatformAndEncoding |
		found := platformIDsAndEncodings indexOf: preferredPlatformAndEncoding.
		found = 0 ifFalse: [
			"Go to the beginning of the table"
			offset := offsets at: found.
			entry offset: initialOffset + offset.
			^ self decodeCmapFmtTable: entry ]].

	^nil! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:36:16'!
processCompositeGlyphFrom: entry
	"Read a composite glyph from the font data."
	| glyph flags glyphIndex hasInstructions ofsX ofsY iLen geometryTransformation bb bl br bt |
	glyph := TTCompositeGlyph new.
	bl := entry nextInt16.
	bt := entry nextInt16.
	br := entry nextInt16.
	bb := entry nextInt16.
	glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
	hasInstructions := false.
	[
		flags := entry nextUint16.
		glyphIndex := entry nextUint16 + 1.

		(flags allMask: 1 "ARG_1_AND_2_ARE_WORDS") ifTrue: [
			"'Hay Offset short x@y' print."
			ofsX := entry nextInt16.
			ofsY := entry nextInt16.
		] ifFalse: [
			"'Hay Offset byte x@y' print."
			(ofsX := entry nextByte) > 127 ifTrue: [ofsX := ofsX - 256].
			(ofsY := entry nextByte) > 127 ifTrue: [ofsY := ofsY - 256]].

		(flags allMask: 2 "ARGS_ARE_XY_VALUES") ifFalse: [self error: 'Unsupported TrueType format']. "Unsupported"
		"ROUND_XY_TO_GRID = 4. Ignored"
		(flags anyMask: 8+64+128) ifFalse: [
			geometryTransformation := MorphicTranslation withTranslation: ofsX@ofsY ].
		(flags allMask: 8 "WE_HAVE_A_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation withScale: entry nextInt16 asFloat / 16r4000 ].
		(flags allMask: 64 "WE_HAVE_AN_X_AND_Y_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation
				withPointScale: (entry nextInt16 asFloat / 16r4000)@(entry nextInt16 asFloat / 16r4000)
				position: ofsX@ofsY ].
		(flags allMask: 128 "WE_HAVE_A_TWO_BY_TWO") ifTrue: [
			"2x2 transformation"
			geometryTransformation := AffineTransformation new
				a11: entry nextInt16 asFloat / 16r4000;
				a21: entry nextInt16 asFloat / 16r4000;
				a12: entry nextInt16 asFloat / 16r4000;
				a22: entry nextInt16 asFloat / 16r4000;
				a13: ofsX;
				a23: ofsY ].

		glyph addGlyph: glyphIndex transformation: geometryTransformation.
		hasInstructions := hasInstructions or: [ flags allMask: 256 "WE_HAVE_INSTRUCTIONS"].
		"Continue as long as the MORE_COMPONENTS bit is set"
		flags allMask: 32 "MORE_COMPONENTS"
	] whileTrue.
	hasInstructions ifTrue: [
		"Ignore them"
		iLen := entry nextUint16.
		entry skip: iLen].
	^glyph! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:06'!
processFontHeaderTable: entry
"Value				Data Type    Description
unitsPerEm			USHORT      Granularity of the font's em square.
xMax				USHORT      Maximum X-coordinate for the entire font.
xMin				USHORT      Minimum X-coordinate for the entire font.
yMax				USHORT      Maximum Y-coordinate for the entire font.
yMin				USHORT      Minimum Y-coordinate for the entire font.
indexToLocFormat	SHORT       Used when processing the Index To Loc Table."
	| origin corner units indexToLocFormat |
	entry skip: 4. "Skip table version number"
	entry skip: 4. "Skip font revision number"
	entry skip: 4. "Skip check sum adjustment"
	entry skip: 4. "Skip magic number"
	entry skip: 2. "Skip flags"

	units := entry nextUint16.

	entry skip: 8. "Skip creation date"
	entry skip: 8. "Skip modification date"

	"Get min/max values of all glyphs"
	origin := entry nextInt16 @ entry nextInt16.
	corner := entry nextInt16 @ entry nextInt16.

	entry skip: 2. "Skip mac style"
	entry skip: 2. "Skip lowest rec PPEM"
	entry skip: 2. "Skip font direction hint"
	indexToLocFormat := entry nextInt16.

	fontDescription setBounds: (origin corner: corner) unitsPerEm: units.
	^indexToLocFormat! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:33:21'!
processGlyphDataTable: entry offsets: offsetArray
	"Read the actual glyph data from the font.
	offsetArray contains the start offsets in the data for each glyph."
	| initialOffset |
	initialOffset := entry offset.
	glyphs := Array new: nGlyphs.
	fontDescription fullName, ' - Reading glyph data' 
		displayProgressAt: 100@100
		from: 1 
		to: nGlyphs 
		during: [ :bar |
			| glyph glyphOffset nextOffset glyphLength numberOfContours |
			1 to: nGlyphs do: [ :glyphIndex |
				bar value: glyphIndex.
				glyphOffset := offsetArray at: glyphIndex.
				nextOffset := offsetArray at: glyphIndex+1.
				glyphLength := nextOffset - glyphOffset.
				glyph := glyphLength = 0
					ifTrue: [ TTGlyph new. ]
					ifFalse: [
						entry offset: initialOffset + glyphOffset.
						numberOfContours := entry nextInt16.
						numberOfContours >= 0
							ifTrue: [ self processSimpleGlyphContours: numberOfContours from: entry ]
							ifFalse: [ self processCompositeGlyphFrom: entry ]].
				glyphs at: glyphIndex put: glyph ]].
	glyphs do: [ :g | g replaceIndexesByComponentGlyphsIn: glyphs ]
	! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:09'!
processHorizontalHeaderTable: entry
"See 
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://docs.microsoft.com/en-us/typography/opentype/spec/hhea
https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html
http://chanae.walon.org/pub/ttf/ttf_glyphs.htm

https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6.html
"
"
ascent  						SHORT          Distance from baseline of highest ascender.
descent  					SHORT          Distance from baseline of lowest descender.
lineGap  					SHORT          Typographic line gap.
numOfLongHorMetrics  	USHORT       Number of advance widths in metrics table (HTMX Table); may be smaller than the total number of glyphs.
"
	| ascent descent lineGap numOfLongHorMetrics |
	entry skip: 4. "Skip table version"
	ascent := entry nextInt16.
	descent := entry nextInt16.
	lineGap := entry nextInt16.
	entry skip: 2. "Skip advanceWidthMax"
	entry skip: 2. "Skip minLeftSideBearing"
	entry skip: 2. "Skip minRightSideBearing"
	entry skip: 2. "Skip xMaxExtent"
	entry skip: 2. "Skip caretSlopeRise"
	entry skip: 2. "Skip caretSlopeRun"
	entry skip: 10. "Skip 5 reserved shorts"
	entry skip: 2. "Skip metricDataFormat"

	numOfLongHorMetrics := entry nextUint16.
	fontDescription setAscent: ascent descent: descent lineGap: lineGap.
	^numOfLongHorMetrics! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:13'!
processHorizontalMetricsTable: entry length: numHMetrics
	| lastAW glyph |
	1
		to: numHMetrics
		do: [ :i |
			glyph := glyphs at: i.
			glyph advanceWidth: entry nextUint16.
			glyph leftSideBearing: entry nextInt16.
			glyph updateRightSideBearing ].
	numHMetrics = nGlyphs ifTrue: [ ^ true ].
	lastAW := (glyphs at: numHMetrics) advanceWidth.
	numHMetrics + 1
		to: nGlyphs
		do: [ :i |
			glyph := glyphs at: i.
			glyph advanceWidth: lastAW.
			glyph leftSideBearing: entry nextInt16.
			glyph updateRightSideBearing ].! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:37:24'!
processIndexToLocationTable: entry format: indexToLocFormat
"glyphOffset    ULONG[numGlyphs]   An array that contains each glyph's
                                 offset into the Glyph Data Table.
"	| glyphOffset offset|
	glyphOffset := Array new: nGlyphs+1.
	1 to: nGlyphs+1 do:[:i|
		(indexToLocFormat = 0) ifTrue:[ "Format0: offset/2 is stored"
			offset := entry nextUint16 * 2.
		] ifFalse:["Format1: store actual offset"
			offset := entry nextUint32].
		glyphOffset at: i put: offset].
	^glyphOffset! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:22'!
processKerningSubTableType0: entry

	| kp nKernPairs |
	nKernPairs := entry nextUint16.
	entry skip: 2. "Skip search range"
	entry skip: 2. "Skip entry selector"
	entry skip: 2. "Skip range shift"
	kernPairs := Array new: nKernPairs.
	1 to: nKernPairs do:[:i|
		kp := TTKernPair new.
		kp left: entry nextUint16.
		kp right: entry nextUint16.
		kp value: entry nextInt16.
		kernPairs at: i put: kp].
	^true
! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:26'!
processKerningTable: entry
	"Extract the kerning information for pairs of glyphs."
	| version nTables covLow covHigh |
	version := entry nextUint16. "Skip table version"
	nTables := entry nextUint16. "Skip number of sub tables -- we're using the first one only"
	nTables = 0 ifTrue: [" This detection is hacky "
		entry skip: -4. "rewind"
		^ self processOSXKerningTable: entry].
	entry skip: 2. "Skip current subtable number"
	entry skip: 2. "Skip length of subtable"
	covHigh := entry nextByte.
	covLow := entry nextByte.

	"Make sure the format is right (kerning table and format type 0)"
	((covLow allMask: 2) or: [ covHigh ~= 0]) ifTrue: [^false].
	"Subtable"
	^ self processKerningSubTableType0: entry.
! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:32'!
processMaximumProfileTable: entry
"
numGlyphs         USHORT      The number of glyphs in the font.
"
	entry skip: 4. "Skip Table version number"
	nGlyphs := entry nextUint16.! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:35:47'!
processNamingTable: entry
"copyright         CHARPTR     The font's copyright notice.
familyName        CHARPTR     The font's family name.
subfamilyName     CHARPTR     The font's subfamily name.
uniqueName        CHARPTR     A unique identifier for this font.
fullName          CHARPTR     The font's full name (a combination of
                                          familyName and subfamilyName).
versionName       CHARPTR     The font's version string.
"	| nRecords initialOffset format storageOffset  strings |
	strings := Array new: 8.
	strings atAllPut:''.
	initialOffset := entry offset.
	format := entry nextUint16.
	format = 0 ifFalse: [self error: 'Cannot handle format 1 naming tables'].
	"Get the number of name records"
	nRecords := entry nextUint16.
	"Offset from the beginning of this table"
	storageOffset := entry nextUint16 + initialOffset.
	1 to: nRecords do:[:i| | pID sID lID nID length offset multiBytes string |
		pID := entry nextUint16.
		sID := entry nextUint16.
		lID := entry nextUint16.
		nID := entry nextUint16.
		length := entry nextUint16.
		offset := entry nextUint16.
		"Read only Macintosh or Microsoft strings"
		(pID = 1 or:[pID = 3 and:[sID = 1]]) ifTrue:[
			"MS uses Unicode all others single byte"
			multiBytes := pID = 3.
			string := entry stringAt: storageOffset + offset length: length multiByte: multiBytes.

			"Select only English names.
			Note: We prefer Macintosh strings about everything."
			((pID = 1 and: [lID = 0]) or: [pID = 3 and: [lID = 16r0409]]) ifTrue: [
				"Put the name at the right location."
				nID < strings size ifTrue:[
					(pID = 1 or:[(strings at: nID+1) = ''])
						ifTrue:[strings at: nID+1 put: string].
				].
			].
		].
	].
	fontDescription setStrings: strings.! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:37:30'!
processOSXKerningTable: entry
	"From Apple's TrueType reference:
	
	Previous versions of the 'kern' table defined both the version and nTables fields in the header as UInt16 values and not UInt32 values. Use of the older format on OS X is discouraged (although AAT can sense an old kerning table and still make correct use of it). Microsoft Windows still uses the older format for the 'kern' table and will not recognize the newer one. Fonts targeted for OS X only should use the new format; fonts targeted for both OS X and Windows should use the old format.
	"
	"Extract the kerning information for pairs of glyphs."
	| version  nTables length coverage tupleIndex |
	version := entry nextUint32. "Skip table version"
	nTables := entry nextUint32. "Skip number of sub tables -- we're using the first one only"

	length := entry nextUint32. "Skip length of subtable"
	coverage := entry nextUint16.
	tupleIndex := entry nextUint16. "Skip tuple index".
	"Make sure the format is right (kerning table and format type 0)"
	coverage = 0 ifFalse: [^false].
	
	"Subtable"
	^ self processKerningSubTableType0: entry.
! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/6/2023 16:36:06'!
processSimpleGlyphContours: nContours from: entry

	| glyph endPts nPts iLength flags bb bl br bt |
	glyph := TTGlyph new.
	bl := entry nextInt16.
	bt := entry nextInt16.
	br := entry nextInt16.
	bb := entry nextInt16.
	glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
	endPts := Array new: nContours.
	1 to: nContours do: [ :i | endPts at: i put: entry nextUint16 ].
	glyph initializeContours: nContours with: endPts.
	nContours = 0 ifTrue: [^ self].
	nPts := endPts last + 1.
	iLength := entry nextUint16. "instruction length"
	entry skip: iLength.
	flags := self getGlyphFlagsFrom: entry size: nPts.
	self readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	self readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	glyph makePointsAbsolute.
	^glyph! !

!TTFontReader methodsFor: 'public' stamp: 'jmv 5/5/2023 17:28:48'!
readTTCFrom: fontData
	"
	Answer an Array of TTFontDescription.
	Note: The purpose of TTC is completely defeated: there will be no memory savings, as each
		TTFontDescription will include the glyph data for all the glyphs it includes.
		Actually sharing glyph data between TTFontDescriptions needs some work (jmv, 16-Sep-2022).
	https://docs.fileformat.com/font/ttc/
	https://docs.microsoft.com/en-us/typography/opentype/spec/otff#font-collections
	"
	| offset tag majorVersion minorVersion numFonts fontDescriptions |
	tag := fontData copyFrom: 1 to: 4.
	tag = `'ttcf' asUtf8Bytes` ifFalse: [
		^self error:'This TTF Collection does not have a correct header'].
	majorVersion := fontData uint16At: 5 bigEndian: true.
	minorVersion := fontData uint16At: 7 bigEndian: true.
	numFonts := fontData uint32At: 9 bigEndian: true.
	(majorVersion = 1 and: [ minorVersion = 0 ]) ifFalse: [
		^self error:'This TTF Collection has an unsupported header version'].
	fontDescriptions := Array new: numFonts.
	1 to: numFonts do: [ :i |
		fontDescription := TTFontDescription new.
		offset := fontData uint32At: i*4+9 bigEndian: true.
		self readFrom: fontData fromOffset: offset.
		fontDescriptions at: i put: fontDescription.
		fontDescription := nil. "To avoid any confusion"
	].
	^fontDescriptions! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:34:15'!
decodeCmapFmtTable: entry

	| cmapFmt |
	cmapFmt := entry nextUint16.
	cmapFmt = 0 ifTrue: [ "byte encoded table"
		^self decodeCmapFmtTableFormat0: entry ].
	cmapFmt = 4 ifTrue: [ "segment mapping to deltavalues"
		^ self decodeCmapFmtTableFormat4: entry ].
	cmapFmt = 6 ifTrue: [ "trimmed table"
		^self decodeCmapFmtTableFormat6: entry ].
	cmapFmt = 10 ifTrue: [ "trimmed array"
		^self decodeCmapFmtTableFormat10: entry ].
	cmapFmt = 12 ifTrue: [ "segmented coverage"
		^ self decodeCmapFmtTableFormat12: entry ].
	^ nil! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:34:20'!
decodeCmapFmtTableFormat0: entry
	"byte encoded table"

	| length cmap |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	length := length - 6. 		"should be always 256"
	length <= 0 ifTrue: [ ^ nil ].	"but sometimes, this table is empty"
	cmap := WordArray new: length.
	entry nextBytes: length into: cmap startingAt: entry offset.
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:37:01'!
decodeCmapFmtTableFormat10: entry
	"trimmed array"

	| length entryCount cmap firstCode |
	entry skip: 2. "skip reserved"
	length := entry nextUint32.
	entry skip: 4. "skip language"
	firstCode := entry nextUint32.
	entryCount := entry nextUint32.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUint16 ].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:37:12'!
decodeCmapFmtTableFormat12: entry
	"segmented coverage"

	| length cmap numGroups |
	entry skip: 2. "reserved"
	length := entry nextUint32.
	entry skip: 4. "language"
	numGroups := entry nextUint32.
	cmap := WordArray new.
	numGroups timesRepeat: [ | endCharCode prev startCharCode startGlyphId |
		startCharCode := entry nextUint32.
		endCharCode := entry nextUint32.
		startGlyphId := entry nextUint32.
		"
{startCharCode. endCharCode. startGlyphId} print.
"
"de ahi para arriba es banana
http://www.alanwood.net/unicode/unicode_samples.html
"
endCharCode < 16rE0000 ifTrue: [
		prev := cmap.
		cmap := WordArray new: endCharCode+1.
		cmap replaceFrom: 1 to: prev size with: prev. 
		startCharCode+1 to: endCharCode+1 do: [ :i |
			cmap at: i put: startGlyphId.
			startGlyphId := startGlyphId + 1 ]]
].
"
self halt.
"
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:34:39'!
decodeCmapFmtTableFormat4: entry
	"segment mapping to deltavalues"

	| length entryCount segCount segments offset cmap |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	segCount := entry nextUint16 // 2.
	entry skip: 6. "skip searchRange, entrySelector, rangeShift"
	segments := Array new: segCount.
	segments := (1 to: segCount) collect: [ :e | Array new: 4 ].
	1 to: segCount do: [ :i | (segments at: i) at: 2 put: entry nextUint16 ]. "endCount"
	entry skip: 2. "skip reservedPad"
	1 to: segCount do: [ :i | (segments at: i) at: 1 put: entry nextUint16]. "startCount"
	1 to: segCount do: [ :i | (segments at: i) at: 3 put: entry nextInt16 ]. "idDelta"
	offset := entry offset.
	1 to: segCount do: [ :i | (segments at: i) at: 4 put: entry nextUint16 ]. "idRangeOffset"
	"do not process the end-of-search segment"
	segments := segments reject:[:each | (each at:1) = 0xFFFF and:[(each at:2) = 0xFFFF] ].
	entryCount := segments inject: 0 into: [ :max :seg | max max: seg second ].
	cmap := WordArray new: entryCount+1.
	segments withIndexDo: [ :seg :si | | code |
		seg first to: seg second do: [ :i |
			seg last > 0 ifTrue: ["offset to glypthIdArray - this is really C-magic!!"
				entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 
				code := entry nextUint16.
				code > 0 ifTrue: [code := code + seg third ]]
			ifFalse: ["simple offset"
				code := i + seg third ].
			cmap at: i + 1 put: code ]].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:34:51'!
decodeCmapFmtTableFormat6: entry
	"trimmed table"

	| length entryCount cmap firstCode |
	length := entry nextUint16.
	entry skip: 2. "skip version"
	firstCode := entry nextUint16.
	entryCount := entry nextUint16.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUint16 ].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:33:50'!
readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
	"Read the x coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits xValue contour |
	startPoint := 1.
	1 to: nContours do: [ :i |
		contour := glyph contours at: i.
		"Get the end point"
		endPoint := (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits := flags at: j.
			"If bit zero in the flag is set then this point is an on-curve
			point, if not, then it is an off-curve point."
			contour controlPointFlags bitAt: (j - startPoint + 1) put: 1-(flagBits bitAnd: 1).
			"First we check to see if bit one is set. This would indicate that
			the corresponding coordinate data in the table is 1 byte long.
			If the bit is not set, then the coordinate data is 2 bytes long."
			xValue := (flagBits allMask: 2)
				ifTrue: [ "one byte"
					(flagBits allMask: 16)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					"If bit four is set, then this coordinate is the same as the
					last one, so the relative offset (of zero) is stored. If bit
					is not set, then read in two bytes and store it as a signed value."
					(flagBits allMask: 16)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextInt16 ]].
			contour points at: (j - startPoint + 1) putX: xValue.
		].
		startPoint := endPoint + 1 ]! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/6/2023 16:33:54'!
readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
	"Read the y coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits yValue contour |
	startPoint := 1.
	1 to: nContours do: [ :i |
		contour := glyph contours at: i.
		"Get the end point"
		endPoint := (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits := flags at: j.
			"Check if this value one or two byte encoded"
			yValue := (flagBits allMask: 4)
				ifTrue: [ "one byte"
					(flagBits allMask: 32)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					(flagBits allMask: 32)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextInt16 ]].
			contour points at: (j - startPoint + 1) putY: yValue.
		].
		startPoint := endPoint + 1 ]! !


!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/5/2023 20:23:46'!
nextLong

	| value |
	value := fontData int32At: offset bigEndian: true.
	offset := offset + 4.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/5/2023 20:22:21'!
nextShort

	| value |
	value := fontData int16At: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/5/2023 17:28:03'!
nextULong

	| value |
	value := fontData uint32At: offset bigEndian: true.
	offset := offset + 4.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/5/2023 17:28:51'!
nextUShort

	| value |
	value := fontData uint16At: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'initialize-release' stamp: 'jmv 5/5/2023 20:23:53'!
on: fd at: index

	fontData := fd.
	tag := fontData int32At: index bigEndian: true.
	checkSum := fontData int32At: index+4 bigEndian: true.
	offset := (fontData int32At: index+8 bigEndian: true) + 1.
	length := fontData int32At: index+12 bigEndian: true.! !


!ByteArray reorganize!
('accessing' atAllPut: bitSize bytesPerElement indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent:)
('access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At: int8At:put: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At: uint8At:put:)
('platform independent access' doubleAt:bigEndian: doubleAt:put:bigEndian: floatAt:bigEndian: floatAt:put:bigEndian: long64At:bigEndian: long64At:put:bigEndian: longAt:bigEndian: longAt:put:bigEndian: shortAt:bigEndian: shortAt:put:bigEndian: unsignedLong64At:bigEndian: unsignedLong64At:put:bigEndian: unsignedLongAt:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:bigEndian: unsignedShortAt:put:bigEndian:)
('converting' asByteArray asHex asString asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement replaceFrom:to:with:startingAt:)
('comparing' = hash)
('printing' printOn: storeOn:)
('testing' is: isLiteral)
('initialization' readHexFrom:)
!

