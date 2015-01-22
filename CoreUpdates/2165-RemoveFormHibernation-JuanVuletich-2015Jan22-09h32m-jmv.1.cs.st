'From Cuis 4.2 of 25 July 2013 [latest update: #2164] on 22 January 2015 at 9:55:16.044728 am'!

!Form methodsFor: 'comparing' stamp: 'jmv 1/22/2015 09:54'!
hash
	"Hash is re-implemented because #= is re-implemented"

	^bits hash! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 1/21/2015 23:52'!
copyBits
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"No alpha specified -- re-run with alpha = 1.0"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue: [
		^ self copyBitsTranslucent: 255].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [^ self oldPaintBits].
	combinationRule = Form oldErase1bitShape ifTrue: [^ self oldErase1bitShapeBits].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and:[colorMap isColormap]) ifTrue:[
		colorMap _ colorMap colors.
		^self copyBits].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue:[self roundVariables. ^self copyBitsAgain].

	self error: 'Bad BitBlt arg (Fraction?); proceed to convert.'.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 1/21/2015 23:52'!
copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 1/21/2015 23:53'!
pixelAt: aPoint
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPeekerFromForm:.  Returns the pixel at aPoint."
	sourceX _ aPoint x.
	sourceY _ aPoint y.
	destForm bits at: 1 put: 0.  "Just to be sure"
	self copyBits.
	^ destForm bits at: 1! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 1/21/2015 23:53'!
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
	[bb pixelAt: Sensor mousePoint put: 55]
"! !


!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 1/21/2015 23:53'!
bitPeekerFromForm: sourceForm
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord |
	pixPerWord _ 32 // sourceForm depth.
	^ self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: sourceForm
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: (0@0 extent: pixPerWord@1)
! !

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 1/21/2015 23:53'!
bitPokerToForm: destForm
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord |
	pixPerWord _ 32//destForm depth.
	^ self destForm: destForm
	 	sourceForm: (Form extent: pixPerWord@1 depth: destForm depth)
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: (pixPerWord-1)@0
		extent: 1@1
		clipRect: (0@0 extent: destForm extent)
! !


!WarpBlt methodsFor: 'primitives' stamp: 'jmv 1/21/2015 23:56'!
warpBitsSmoothing: n sourceMap: sourceMap
	| deltaP12 deltaP43 pA pB deltaPAB sp fixedPtOne picker poker pix nSteps |
	<primitive: 'primitiveWarpBits' module: 'BitBltPlugin'>

	(width < 1) | (height < 1) ifTrue: [^ self].
	fixedPtOne _ 16384.  "1.0 in fixed-pt representation"
	n > 1 ifTrue:
		[(destForm depth < 16 and: [colorMap == nil])
			ifTrue: ["color map is required to smooth non-RGB dest"
					^ self primitiveFail].
		pix _ Array new: n*n].

	nSteps _ height-1 max: 1.
	deltaP12 _ (self deltaFrom: p1x to: p2x nSteps: nSteps)
			@ (self deltaFrom: p1y to: p2y nSteps: nSteps).
	pA _ (self startFrom: p1x to: p2x offset: nSteps*deltaP12 x)
		@ (self startFrom: p1y to: p2y offset: nSteps*deltaP12 y).
	deltaP43 _ (self deltaFrom: p4x to: p3x nSteps: nSteps)
			@ (self deltaFrom: p4y to: p3y nSteps: nSteps).
	pB _ (self startFrom: p4x to: p3x offset: nSteps*deltaP43 x)
		@ (self startFrom: p4y to: p3y offset: nSteps*deltaP43 y).

	picker _ BitBlt bitPeekerFromForm: sourceForm.
	poker _ BitBlt bitPokerToForm: destForm.
	poker clipRect: self clipRect.
	nSteps _ width-1 max: 1.
	destY to: destY+height-1 do:
		[:y |
		deltaPAB _ (self deltaFrom: pA x to: pB x nSteps: nSteps)
				@ (self deltaFrom: pA y to: pB y nSteps: nSteps).
		sp _ (self startFrom: pA x to: pB x offset: nSteps*deltaPAB x)
			@ (self startFrom: pA y to: pB y offset: nSteps*deltaPAB x).
		destX to: destX+width-1 do:
			[:x | 
			n = 1
			ifTrue:
				[poker pixelAt: x@y
						put: (picker pixelAt: sp // fixedPtOne asPoint)]
			ifFalse:
				[0 to: n-1 do:
					[:dx | 0 to: n-1 do:
						[:dy |
						pix at: dx*n+dy+1 put:
								(picker pixelAt: sp
									+ (deltaPAB*dx//n)
									+ (deltaP12*dy//n)
										// fixedPtOne asPoint)]].
				poker pixelAt: x@y put: (self mixPix: pix
										sourceMap: sourceMap
										destMap: colorMap)].
			sp _ sp + deltaPAB].
		pA _ pA + deltaP12.
		pB _ pB + deltaP43]! !


!Bitmap methodsFor: 'filing' stamp: 'jmv 1/21/2015 23:38'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream."

	aStream nextInt32Put: self size.
	aStream nextPutAll: self
! !


!Bitmap class methodsFor: 'instance creation' stamp: 'jmv 1/21/2015 23:47'!
newFromStream: s
	| len |
	len _ s nextInt32.
	^ s nextWordsInto: (self new: len)! !


!Form methodsFor: 'converting' stamp: 'jmv 1/21/2015 23:53'!
asGrayScale: component
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)
	
	If component = 1, take the red component
	If component = 2, take the green component
	If component = 3, take the blue component
	"
	| f32 srcForm result map bb grays |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScale: component ].
	srcForm _ Form extent: (width * 4)@height depth: 8.
	srcForm bits: bits.
	result _ ColorForm extent: width@height depth: 8.
	map _ Bitmap new: 256.
	2 to: 256 do: [:i | map at: i put: i - 1].
	map at: 1 put: 1.  "map zero pixel values to near-black"
	bb _ (BitBlt toForm: result)
		sourceForm: srcForm;
		combinationRule: Form over;
		colorMap: map.
	0 to: width - 1 do: [ :dstX |
		bb  sourceRect: (((dstX * 4) + component)@0 extent: 1@height);
			destOrigin: dstX@0;
			copyBits].

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: 0@0;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	
	grays _ (0 to: 255) collect: [:brightness | Color gray: brightness asFloat / 255.0].
	grays at: 1 put: Color transparent.
	result colors: grays.
	^ result! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 1/21/2015 23:54'!
storeOn: aStream base: anInteger 
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'depth: '.
	self depth printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn:aStream base:anInteger.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).
! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 1/21/2015 23:54'!
writeAttributesOn: file

	file nextPut: depth.
	file nextWordPut: width.
	file nextWordPut: height.
	file nextWordPut: ((self offset x) >=0
					ifTrue: [self offset x]
					ifFalse: [self offset x + 65536]).
	file nextWordPut: ((self offset y) >=0
					ifTrue: [self offset y]
					ifFalse: [self offset y + 65536]).
	! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 1/21/2015 23:40'!
writeOn: file
	"Write the receiver on the file in the format
		depth, extent, offset, bits."
	self writeAttributesOn: file.
	bits writeOn: file! !

!Form methodsFor: 'testing' stamp: 'jmv 1/21/2015 23:53'!
isAllWhite
	"Answer whether all bits in the receiver are white (=0)."

	1 to: bits size do: [:i | (bits at: i) = 0 ifFalse: [^ false]].
	^ true! !


!CursorWithMask methodsFor: 'primitives' stamp: 'jmv 1/21/2015 23:53'!
beCursor

	^ self beCursorWithMask: maskForm! !


!BMPReadWriter methodsFor: 'writing' stamp: 'jmv 1/21/2015 23:51'!
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image ppw scanLineLen pixline |
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
	stream nextLittleEndianNumber: 2 put: 19778.  "bfType = BM"
	stream nextLittleEndianNumber: 4 put: bfOffBits + biSizeImage.  "Entire file size in bytes"
	stream nextLittleEndianNumber: 4 put: 0.  "bfReserved"
	stream nextLittleEndianNumber: 4 put: bfOffBits.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextLittleEndianNumber: 4 put: biSize.  "info header size in bytes"
	stream nextLittleEndianNumber: 4 put: image width.  "biWidth"
	stream nextLittleEndianNumber: 4 put: image height.  "biHeight"
	stream nextLittleEndianNumber: 2 put: 1.  "biPlanes"
	stream nextLittleEndianNumber: 2 put: (depth min: 24).  "biBitCount"
	stream nextLittleEndianNumber: 4 put: 0.  "biCompression"
	stream nextLittleEndianNumber: 4 put: biSizeImage.  "size of image section in bytes"
	stream nextLittleEndianNumber: 4 put: 2800.  "biXPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: 2800.  "biYPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: biClrUsed.
	stream nextLittleEndianNumber: 4 put: 0.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [:i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [:j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth < 32 ifTrue: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		ppw := 32 // depth.
		scanLineLen := biWidth + ppw - 1 // ppw * 4.  "# of bytes in line"
		1 to: biHeight do: [:i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	] ifFalse: [
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do:[:i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure'].
	stream close.! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 1/21/2015 23:54'!
compress: aForm quality: quality
	"Encode the given Form and answer the compressed ByteArray. Quality goes from 0 (low) to 100 (high), where -1 means default."

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	"odd width images of depth 16 give problems; avoid them."
	sourceForm _ (aForm depth = 32) | (aForm width even & (aForm depth = 16))
		ifTrue: [aForm]
		ifFalse: [aForm asFormOfDepth: 32].
	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer _ ByteArray new: sourceForm width * sourceForm height.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: false
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	^ buffer copyFrom: 1 to: byteCount
! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 1/21/2015 23:55'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	"odd width images of depth 16 give problems; avoid them."
	sourceForm _ (aForm depth = 32) | (aForm width even & (aForm depth = 16))
		ifTrue: [aForm]
		ifFalse: [aForm asFormOfDepth: 32].
	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer _ ByteArray new: sourceForm width * sourceForm height.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	stream next: byteCount putAll: buffer startingAt: 1.
	self close.
! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 1/21/2015 23:55'!
uncompress: aByteArray into: aForm
	"Uncompress an image from the given ByteArray into the given Form.
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16, do ordered dithering."

	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w _ self primImageWidth: jpegDecompressStruct.
	h _ self primImageHeight: jpegDecompressStruct.
	((aForm width = w) & (aForm height = h)) ifFalse: [
		^ self error: 'form dimensions do not match'].

	"odd width images of depth 16 give problems; avoid them"
	w odd
		ifTrue: [
			aForm depth = 32 ifFalse: [^ self error: 'must use depth 32 with odd width']]
		ifFalse: [
			((aForm depth = 16) | (aForm depth = 32)) ifFalse: [^ self error: 'must use depth 16 or 32']].

	self primJPEGReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: aForm
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
! !

!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 1/21/2015 23:55'!
uncompress: aByteArray into: aForm doDithering: ditherFlag
	"Uncompress an image from the given ByteArray into the given Form. 
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16 and ditherFlag = true, do ordered dithering."

	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w _ self primImageWidth: jpegDecompressStruct.
	h _ self primImageHeight: jpegDecompressStruct.
	((aForm width = w) & (aForm height = h)) ifFalse: [
		^ self error: 'form dimensions do not match'].

	"odd width images of depth 16 give problems; avoid them"
	w odd
		ifTrue: [
			aForm depth = 32 ifFalse: [^ self error: 'must use depth 32 with odd width']]
		ifFalse: [
			((aForm depth = 16) | (aForm depth = 32)) ifFalse: [^ self error: 'must use depth 16 or 32']].

	self primJPEGReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: aForm
		doDithering: ditherFlag
		errorMgr: jpegErrorMgr2Struct.
! !

!methodRemoval: ColorForm #hibernate!
ColorForm removeSelector: #hibernate!
!methodRemoval: ColorForm #unhibernate!
ColorForm removeSelector: #unhibernate!

!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:offset: fakeSphereAt:diameter:color: fromBinaryStream: fromDisplay: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent: verticalGradient:topColor:bottomColor:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' exampleBorder exampleSketch exampleSpaceFill makeStar toothpaste: xorHack:)
('class initialization' unload)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
('import / export' generateContentPack)
!

!methodRemoval: Form #displayResourceFormOn:!
Form removeSelector: #displayResourceFormOn:!
!methodRemoval: Form #hibernate!
Form removeSelector: #hibernate!
!methodRemoval: Form #shouldPreserveContents!
Form removeSelector: #shouldPreserveContents!
!methodRemoval: Form #unhibernate!
Form removeSelector: #unhibernate!
!methodRemoval: Form #writeBitsOn:!
Form removeSelector: #writeBitsOn:!
!methodRemoval: Form #writeOnMovie:!
Form removeSelector: #writeOnMovie:!
!methodRemoval: Form #writeUncompressedOn:!
Form removeSelector: #writeUncompressedOn:!

!Form reorganize!
('accessing' bits bits: bitsSize center depth depth: extent getCanvas height nativeDepth offset offset: size width)
('analyzing' cgForPixelValue:orNot: colorsUsed dominantColor innerPixelRectFor:orNot: pixelCompare:with:at: primCountBits rectangleEnclosingPixelsNotOfColor: tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width:rule:fillColor: borderFormOfWidth:sharpCorners: borderWidth: shapeBorder:width: shapeBorder:width:interiorPoint:sharpCorners:internal:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: makeBWForm: mapColor:to: mapColors:to: maskingMap newColorMap pixelValueFor: reducedPaletteOfSize:)
('converting' as8BitColorForm asCursorForm asFormOfDepth: asFormOfNativeDepth: asGrayScale asGrayScale: colorReduced convertAlphaToZeroValueTransparency copyWithColorsReducedTo: icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: contentsOfArea:into: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule:fillColor: copyBits:from:at:clippingBox:rule:fillColor:map: copyBits:from:at:colorMap: postCopy)
('display box access' boundingBox)
('displaying' displayOn:at:clippingBox:rule:fillColor: paintBits:at:translucent:)
('encoding' addDeltasFrom: deltaFrom: deltaFrom:at:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: readFromOldFormat: storeBitsOn:base: storeOn: storeOn:base: writeAttributesOn: writeBMPfileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn:)
('filling' anyShapeFill convexShapeFill: eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock: findShapeAroundSeedBlock: floodFill2:at: floodFillMapFrom:to:mappingColorsWithin:to: shapeFill:interiorPoint: shapeFill:seedBlock:)
('image manipulation' replaceColor:withColor: smear:distance: tallyPixelValues trimBordersOfColor:)
('other' fixAlpha formForColorCount: preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: isTransparentAt: pixelValueAt: pixelValueAt:put: primPixelValueAtX:y:)
('scaling, rotation' flippedBy:centerAt: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotatedBy:smoothing: rotatedByDegrees: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent)
('transitions' fadeImage:at:indexAndMaskDo: fadeImageCoarse:at: fadeImageFine:at: fadeImageHor:at: fadeImageHorFine:at: fadeImageSquares:at: fadeImageVert:at: pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' hackBits: initFromArray: privateFloodFillValue: setExtent:depth: setExtent:depth:bits:)
('comparing' = hash)
('initialization' fromDisplay: swapEndianness)
!

!methodRemoval: DisplayObject #writeUncompressedOnFileNamed:!
DisplayObject removeSelector: #writeUncompressedOnFileNamed:!
!methodRemoval: Bitmap class #decodeIntFrom:!
Bitmap class removeSelector: #decodeIntFrom:!
!methodRemoval: Bitmap class #decompressFromByteArray:!
Bitmap class removeSelector: #decompressFromByteArray:!
!methodRemoval: Bitmap #compress:toByteArray:!
Bitmap removeSelector: #compress:toByteArray:!
!methodRemoval: Bitmap #compressToByteArray!
Bitmap removeSelector: #compressToByteArray!
!methodRemoval: Bitmap #decompress:fromByteArray:at:!
Bitmap removeSelector: #decompress:fromByteArray:at:!
!methodRemoval: Bitmap #encodeBytesOf:in:at:!
Bitmap removeSelector: #encodeBytesOf:in:at:!
!methodRemoval: Bitmap #encodeInt:!
Bitmap removeSelector: #encodeInt:!
!methodRemoval: Bitmap #encodeInt:in:at:!
Bitmap removeSelector: #encodeInt:in:at:!
!methodRemoval: Bitmap #fromByteStream:!
Bitmap removeSelector: #fromByteStream:!
!methodRemoval: Bitmap #readCompressedFrom:!
Bitmap removeSelector: #readCompressedFrom:!
!methodRemoval: Bitmap #writeUncompressedOn:!
Bitmap removeSelector: #writeUncompressedOn:!

!Bitmap reorganize!
('filing' restoreEndianness writeOn:)
('printing' printOn:)
('accessing' atAllPut: bitPatternForDepth: byteAt: byteAt:put: byteSize copyFromByteArray2:to: copyFromByteArray: defaultElement integerAt: integerAt:put: pixelValueForDepth: primFill: replaceFrom:to:with:startingAt:)
('testing' isColormap)
('as yet unclassified' asByteArray copy)
!

