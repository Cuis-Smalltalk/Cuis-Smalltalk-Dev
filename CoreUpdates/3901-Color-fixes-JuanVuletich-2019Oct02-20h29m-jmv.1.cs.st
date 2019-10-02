'From Cuis 5.0 [latest update: #3900] on 2 October 2019 at 8:48:36 pm'!

!Color methodsFor: 'comparing' stamp: 'jmv 10/2/2019 20:31:03'!
= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	^self primitiveEqual: another! !

!Color methodsFor: 'comparing' stamp: 'jmv 10/2/2019 20:37:59'!
hash
	| hash |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	hash _ (self species hash + self size hash) hashMultiply.
	1 to: self size do: [ :i | hash _ (hash + (self basicAt: i)) hashMultiply].
	^hash! !

!Color methodsFor: 'comparing' stamp: 'jmv 10/2/2019 20:37:24'!
primitiveEqual: aColor 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aColor class == self class ifFalse: [^ false].
	length _ self size.
	length = aColor size ifFalse: [^ false].
	1 to: self size do: [ :i |
		(self basicAt: i) = (aColor basicAt: i) ifFalse: [^ false]].
	^ true! !

!Color methodsFor: 'object serialization' stamp: 'jmv 10/2/2019 20:45:01'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]! !

!Color methodsFor: 'object serialization' stamp: 'jmv 10/2/2019 20:46:32'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	aStream nextSignedInt32Put: self basicSize bigEndian: true.
	aStream nextWordsPutAll: self.! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 10/2/2019 20:47:49'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |
	len _ s nextSignedInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !

!methodRemoval: Color #hashFull!
Color removeSelector: #hashFull!

!Color reorganize!
('access' alpha blue brightness chroma color green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('comparing' = hash primitiveEqual:)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isCyan isGreen isMagenta isRed isYellow)
('testing' is: isCollection mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: clipToValidValues setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('object serialization' convertToCurrentVersion:refStream: restoreEndianness writeOn:)
!

