'From Cuis7.5 [latest update: #7641] on 8 October 2025 at 1:27:01 pm'!
!classDefinition: #BitBlt category: #'Graphics-Primitives'!
Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap '
	classVariableNames: 'CachedFontColorMaps ColorConvertingMaps AccessProtect '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Form methodsFor: 'system primitives' stamp: 'jmv 10/8/2025 13:25:45'!
primitivePixelValueAt
	"Separated in it's own method so it can be protected with a BitBlt class-wide MutEx.
	nil answer means primitive failure"
	<primitive: 'primitivePixelValueAt' module: 'BitBltPlugin'>
	^nil! !


!BitBlt methodsFor: 'system primitives' stamp: 'jmv 10/8/2025 13:24:28'!
primitiveCopyBits
	"Separated in it's own method so it can be protected with a class-wide MutEx.
	nil answer means primitive failure"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	^nil! !

!BitBlt methodsFor: 'system primitives' stamp: 'jmv 10/8/2025 13:25:05'!
primitiveDrawLoopX: xDelta Y: yDelta
	"Separated in it's own method so it can be protected with a class-wide MutEx.
	nil answer means primitive failure"
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	^nil! !


!BitBlt class methodsFor: 'private' stamp: 'jmv 10/8/2025 13:09:21'!
accessProtect
	"Needed because the plugin has static state, and modern VMs may multithread long running primitives.
	See local senders."
	AccessProtect ifNil: [
		AccessProtect := Semaphore forMutualExclusion ].
	^AccessProtect! !


!WarpBlt methodsFor: 'system primitives' stamp: 'jmv 10/8/2025 13:26:02'!
primitiveWarpBitsSmoothing: n sourceMap: sourceMap
	"Separated in it's own method so it can be protected with a class-wide MutEx.
	nil answer means primitive failure"
	<primitive: 'primitiveWarpBits' module: 'BitBltPlugin'>
	^nil! !


!Form methodsFor: 'pixel access' stamp: 'jmv 10/8/2025 13:19:21'!
primPixelValueAtX: x y: y
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth.
	Typical clients use colorAt: to get a Color"

	| primAnswer |
	BitBlt accessProtect critical: [
		primAnswer := self primitivePixelValueAt ].
	primAnswer ifNotNil: [ ^primAnswer ].

	^(BitBlt bitPeekerFromForm: self) pixelAt: x@y! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 10/8/2025 13:15:26'!
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
	| primAnswer |
	self class accessProtect critical: [
		primAnswer := self primitiveCopyBits ].
	primAnswer ifNotNil: [ ^primAnswer ].

	"No alpha specified -- re-run with alpha = 1.0"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue: [
		^ self copyBitsTranslucent: 255].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [^ self oldPaintBits].
	combinationRule = Form oldErase1bitShape ifTrue: [^ self oldErase1bitShapeBits].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and:[colorMap isColormap]) ifTrue:[
		colorMap := colorMap colors.
		^self copyBits].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue:[self roundVariables. ^self copyBitsAgain].

	'Bad BitBlt argument (Maybe a Float or Fraction?); will retry rounding.' print.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 10/8/2025 13:16:01'!
copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	| primAnswer |
	self class accessProtect critical: [
		primAnswer := self primitiveCopyBits ].
	primAnswer ifNotNil: [ ^primAnswer ].

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"! !

!BitBlt methodsFor: 'line drawing' stamp: 'jmv 10/8/2025 13:26:27'!
drawLoopX: xDelta Y: yDelta
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| primAnswer dx dy px py P |

	self class accessProtect critical: [
		primAnswer := self primitiveDrawLoopX: xDelta Y: yDelta ].
	primAnswer ifNotNil: [ ^primAnswer ].

	dx := xDelta sign.
	dy := yDelta sign.
	px := yDelta abs.
	py := xDelta abs.
	"self copyBits."
	py > px
		ifTrue: 
			["more horizontal"
			P := py // 2.
			1 to: py do: 
				[:i |
				destX := destX + dx.
				(P := P - px) < 0 ifTrue: 
						[destY := destY + dy.
						P := P + py].
				i < py ifTrue: [self copyBits]]]
		ifFalse: 
			["more vertical"
			P := px // 2.
			1 to: px do:
				[:i |
				destY := destY + dy.
				(P := P - py) < 0 ifTrue: 
						[destX := destX + dx.
						P := P + px].
				i < px ifTrue: [self copyBits]]]! !

!BitBlt methodsFor: 'private' stamp: 'jmv 10/8/2025 13:15:54'!
copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	| primAnswer |
	self class accessProtect critical: [
		primAnswer := self primitiveCopyBits ].
	primAnswer ifNotNil: [ ^primAnswer ].

	'#copyBitsAgain failed. Requested BitBlt operation not performed. Call stack follows:' print.
	thisContext printStack: 15.! !


!WarpBlt methodsFor: 'primitives' stamp: 'jmv 10/8/2025 13:22:39'!
warpBitsSmoothing: n sourceMap: sourceMap
	| primAnswer deltaP12 deltaP43 pA pB deltaPAB sp fixedPtOne picker poker pix nSteps |

	self class accessProtect critical: [
		primAnswer := self primitiveWarpBitsSmoothing: n sourceMap: sourceMap ].
	primAnswer ifNotNil: [ ^primAnswer ].

	(width < 1) | (height < 1) ifTrue: [^ self].
	fixedPtOne := 16384.  "1.0 in fixed-pt representation"
	n > 1 ifTrue:
		[(destForm depth < 16 and: [colorMap == nil])
			ifTrue: ["color map is required to smooth non-RGB dest"
					^ self primitiveFail].
		pix := Array new: n*n].

	nSteps := height-1 max: 1.
	deltaP12 := (self deltaFrom: p1x to: p2x nSteps: nSteps)
			@ (self deltaFrom: p1y to: p2y nSteps: nSteps).
	pA := (self startFrom: p1x to: p2x offset: nSteps*deltaP12 x)
		@ (self startFrom: p1y to: p2y offset: nSteps*deltaP12 y).
	deltaP43 := (self deltaFrom: p4x to: p3x nSteps: nSteps)
			@ (self deltaFrom: p4y to: p3y nSteps: nSteps).
	pB := (self startFrom: p4x to: p3x offset: nSteps*deltaP43 x)
		@ (self startFrom: p4y to: p3y offset: nSteps*deltaP43 y).

	picker := BitBlt bitPeekerFromForm: sourceForm.
	poker := BitBlt bitPokerToForm: destForm.
	poker clipRect: self clipRect.
	nSteps := width-1 max: 1.
	destY to: destY+height-1 do:
		[:y |
		deltaPAB := (self deltaFrom: pA x to: pB x nSteps: nSteps)
				@ (self deltaFrom: pA y to: pB y nSteps: nSteps).
		sp := (self startFrom: pA x to: pB x offset: nSteps*deltaPAB x)
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
			sp := sp + deltaPAB].
		pA := pA + deltaP12.
		pB := pB + deltaP43]! !

!methodRemoval: BitBlt #primitivePixelValueAt stamp: 'jmv 10/8/2025 13:19:28'!
BitBlt removeSelector: #primitivePixelValueAt!

!Form reorganize!
('accessing' bits bits: bitsSize bitsSizeForWidth:height:depth: center depth extent getCanvas height nativeDepth offset offset: pixelsPerWord pixelsPerWord: size width wordsPerLine wordsPerLine:depth:)
('analyzing' cgForPixelValue:orNot: dominantColor innerPixelRectFor:orNot: isAnyPixel: pixelCompare:with:at: primCountBits tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width: border:width:borderHeight:fillColor: border:width:fillColor: border:width:rule:fillColor: border:widthRectangle:rule:fillColor: borderWidth:borderHeight:fillColor:)
('coloring' fill:fillColor: fillBlack fillBlack: fillColor: fillGray fillGray: fillShape:fillColor: fillShape:fillColor:at: fillWhite fillWhite: fillWithColor: reverse reverse: reverse:fillColor:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: colormapIfNeededForGray8bpp colormapIfNeededFrom: mapColor:to: maskingMap newColorMap pixelValueFor:)
('converting' as8BitColorForm as8BitStandardPaletteColorForm asColorFormOfDepth: asCursorForm asForm asFormAutoRange asFormOfDepth: asGrayForm asGrayForm: asGrayScale asGrayScaleAndTransparent asGrayScaleAndTransparent: convertAlphaToZeroValueTransparency icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: postCopy)
('display box access' boundingBox)
('displaying' displayAutoRangeAt: displayAutoRangeAt:zoom: displayOn:at:clippingBox:rule: paintBits:at:translucent:)
('displaying-generic' display displayAt: displayOn: displayOn:at: displayOn:at:rule:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: storeBitsOn:base: storeOn: storeOn:base: storeSmall1BitBitsOn: storeSmall1BitOn: writeAttributesOn: writeBMPfileNamed: writeFileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnFileNamed:)
('filling' eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock:)
('image manipulation' smear:distance: tallyPixelValues)
('other' divideByAlpha fixAlpha makeAllPixelsAlpha: makeAllPixelsOpaque makeAllPixelsOpaqueBlack preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: colorInterpolatedAt: i:j: i:j:put: isTransparentAt: pixelValueAt: pixelValueAt:put: primPixelValueAtX:y:)
('scaling, rotation' flippedBy: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotated180Degrees rotated270Degrees rotated90Degrees rotatedBy:smoothing: rotatedByDegrees: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent)
('transitions' pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' copyFromByteArray2:to: copyFromByteArray: fillSmall1BitWith: hackBits64: hackBits: initFromArray: setExtent:depth: setExtent:depth:bits:)
('comparing' = hash)
('initialization' fromDisplay: swapEndianness)
('encoding' addDeltasFrom:)
('system primitives' primitivePixelValueAt)
!

!classDefinition: #BitBlt category: #'Graphics-Primitives'!
Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap'
	classVariableNames: 'AccessProtect CachedFontColorMaps ColorConvertingMaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!WarpBlt reorganize!
('setup' cellSize cellSize:)
('smoothing' mixPix:sourceMap:destMap: rgbMap:from:to:)
('primitives' copyQuad:toRect: deltaFrom:to:nSteps: sourceForm:destRect: sourceQuad:destRect: startFrom:to:offset: warpBits warpBitsSmoothing:sourceMap:)
('system primitives' primitiveWarpBitsSmoothing:sourceMap:)
!

