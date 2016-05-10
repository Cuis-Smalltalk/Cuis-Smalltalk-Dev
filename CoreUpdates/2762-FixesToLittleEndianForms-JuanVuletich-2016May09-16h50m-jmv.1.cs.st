'From Cuis 4.2 of 25 July 2013 [latest update: #2760] on 9 May 2016 at 4:53:50.20838 pm'!

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 5/9/2016 13:52'!
formContents
	^Form fromFileEntry: self! !


!Form methodsFor: 'scaling, rotation' stamp: 'jmv 5/9/2016 16:14'!
flippedBy: direction
	"Return a copy of the receiver flipped either #vertical or #horizontal."
	| newForm quad |
	newForm _ self class extent: self extent depth: depth.
	quad _ self boundingBox innerCorners.
	quad _ (direction = #vertical ifTrue: [#(2 1 4 3)] ifFalse: [#(4 3 2 1)])
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"! !

!Form methodsFor: 'private' stamp: 'jmv 5/9/2016 14:01'!
copyFromByteArray2: byteArray to: i
	"This method should work with either byte orderings"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: byteArray.
	Smalltalk  isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at:  0@i! !

!Form methodsFor: 'private' stamp: 'jmv 5/9/2016 14:31'!
copyFromByteArray: byteArray
	"This method should work with either byte orderings"
	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: byteArray.
	Smalltalk  isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack.
! !


!ColorForm methodsFor: 'scaling, rotation' stamp: 'jmv 5/9/2016 16:14'!
flippedBy: direction
	| oldColors newForm |
	oldColors _ colors.
	self colors: nil.
	newForm _ super flippedBy: direction.
	self colors: oldColors.
	newForm colors: oldColors.
	^newForm! !


!ColorForm class methodsFor: 'instance creation' stamp: 'jmv 5/9/2016 16:53'!
grayScaleExtent: aPoint bits: eightBitPixels
"Native depth"
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)
	Assume pixel data is in linear scale. Convert to gamma space.
	For example, for reading a raw file:

	| bits height width image |
	bits := ('C:\Users\Juan-Tuerca\Dropbox\STL-ya\image.bits' asFileEntry binaryContents).
	height := bits size sqrt roundTo: 1024.
	width := bits size / height.
	image := ColorForm grayScaleExtent: width@height bits: bits.
	image display.
	image writePNGfileNamed: 'raw.png'.
	"

	| grays result |
	result _ self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]) bits: eightBitPixels.
	grays _ (0 to: 255) collect: [ :brightness | Color gray: brightness asFloat / 255.0].
	result colors: grays.
	^result! !

!ColorForm class methodsFor: 'instance creation' stamp: 'jmv 5/9/2016 16:04'!
grayScaleExtent: aPoint bitsLinear: eightBitPixels
"Native depth"
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)
	For example, for reading a raw file:

	| bits height width image |
	bits := ('C:\Users\Juan-Tuerca\Dropbox\STL-ya\image.bits' asFileEntry binaryContents).
	height := bits size sqrt roundTo: 1024.
	width := bits size / height.
	image := ColorForm grayScaleExtent: width@height bitsLinear: bits.
	image display.
	image writePNGfileNamed: 'withGamma.png'.
	"

	| grays result |
	result _ self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]) bits: eightBitPixels.
	grays _ (0 to: 255) collect: [ :brightness | Color gray: (Color linearTosRGBGamma: brightness asFloat /255.0)].
"	grays _ (0 to: 255) collect: [ :brightness | Color gray: (Color linearTosRGBGamma: (brightness asFloat -15 / 240.0 max: 0))]."
	result colors: grays.
	^result! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 5/9/2016 16:51'!
pixelAt: aPoint put: pixelValue
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX _ aPoint x.
	destY _ aPoint y.
	sourceForm bits at: 1 put: pixelValue.
	self copyBits
"
| bb |
bb _ (BitBlt bitPokerToForm: Display).
[Sensor isAnyButtonPressed] whileFalse:
	[bb pixelAt: Sensor mousePoint put: 55. Display forceToScreen]
"! !


!Bitmap methodsFor: 'as yet unclassified' stamp: 'jmv 5/9/2016 14:39'!
asByteArray
	"Faster way to make a byte array from me.
	copyFromByteArray: makes equal Bitmap."
	| f bytes hack |
	f _ Form extent: 4@self size nativeDepthFrom: 8 bits: self.
	bytes _ ByteArray new: self size * 4.
	hack _ Form new hackBits: bytes.
	hack copyBits: f boundingBox
		from: f
		at: (0@0)
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes.
! !


!ColorForm methodsFor: 'converting' stamp: 'jmv 5/9/2016 14:38'!
asFormOfDepth: d
	| answer |
	d = depth ifTrue: [ ^self ].
	"Same depth, but opposite endianness."
	d abs = depth abs ifTrue: [
		answer _ self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 5/9/2016 16:50'!
readIndexedBmpFile: colors
	"Read uncompressed pixel data of depth d from the given BMP stream, where d is 1, 4, 8, or 16"
	| form bytesPerRow pixelData pixelLine startIndex map bitBlt mask |
	colors 
		ifNil:[form := Form extent: biWidth@biHeight depth: biBitCount]
		ifNotNil:[form := ColorForm extent: biWidth@biHeight nativeDepthFrom: biBitCount.
				form colors: colors].
	bytesPerRow := (((biBitCount* biWidth) + 31) // 32) * 4.
	pixelData := ByteArray new: bytesPerRow * biHeight.
	biHeight to: 1 by: -1 do: [:y |
		pixelLine := stream next: bytesPerRow.
		startIndex := ((y - 1) * bytesPerRow) + 1.
		pixelData 
			replaceFrom: startIndex 
			to: startIndex + bytesPerRow - 1 
			with: pixelLine 
			startingAt: 1].
	form copyFromByteArray: pixelData.
	biBitCount = 16 ifTrue:[
		map := ColorMap shifts: #(8 -8 0 0) masks: #(16rFF 16rFF00 0 0).
		mask := 16r80008000.
	].
	biBitCount = 32 ifTrue:[
		map := ColorMap shifts: #(24 8 -8 -24) masks: #(16rFF 16rFF00 16rFF0000 16rFF000000).
		mask := 16rFF000000.
	].
	map ifNotNil:[
		bitBlt := BitBlt toForm: form.
		bitBlt sourceForm: form.
		bitBlt colorMap: map.
		bitBlt combinationRule: Form over.
		bitBlt copyBits.
	].
	mask ifNotNil:[
		bitBlt := BitBlt toForm: form.
		bitBlt combinationRule: 7 "bitOr:with:".
		bitBlt fillBitmap: (Bitmap with: mask).
		bitBlt copyBits.
	].
	^ form
! !

!methodRemoval: ColorForm #flippedBy:centerAt:!
ColorForm removeSelector: #flippedBy:centerAt:!
!methodRemoval: Form #flippedBy:centerAt:!
Form removeSelector: #flippedBy:centerAt:!
!methodRemoval: Bitmap #copyFromByteArray2:to:!
Bitmap removeSelector: #copyFromByteArray2:to:!
!methodRemoval: Bitmap #copyFromByteArray:!
Bitmap removeSelector: #copyFromByteArray:!
