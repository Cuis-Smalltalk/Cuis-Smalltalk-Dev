'From Cuis 4.2 of 25 July 2013 [latest update: #2164] on 22 January 2015 at 10:25:07.894728 am'!
!classDefinition: #Form category: #'Graphics-Display Objects'!
Object subclass: #Form
	instanceVariableNames: 'bits width height depth offset '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!Form methodsFor: 'bordering' stamp: 'jmv 1/22/2015 10:22'!
border: aRectangle width: borderWidth 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses black for 
	drawing the border."

	self border: aRectangle width: borderWidth fillColor: Color black! !

!Form methodsFor: 'bordering' stamp: 'jmv 1/22/2015 10:22'!
border: aRectangle width: borderWidth fillColor: aHalfTone 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: Form over
		fillColor: aHalfTone! !

!Form methodsFor: 'bordering' stamp: 'jmv 1/22/2015 10:22'!
border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	aRectangle
		areasOutside: (aRectangle insetBy: insets)
		do: [ :edgeStrip |
			self fill: edgeStrip rule: combinationRule fillColor: aHalfTone ]! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fill: aRectangle fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by 
	aForm according to the rule over."

	self fill: aRectangle rule: Form over fillColor: aForm! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillBlack
	"Set all bits in the receiver to black (ones)."

	self fill: self boundingBox fillColor: Color black! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillBlack: aRectangle 
	"Set all bits in the receiver's area defined by aRectangle to black (ones)."

	self fill: aRectangle rule: Form over fillColor: Color black! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillColor: aColor
	"Set all pixels in the receiver to the color.  Must be a correct color for this depth of medium.  TK 1 Jun 96"

	self fill: self boundingBox fillColor: aColor! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillGray
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: Color gray! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillGray: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: Color gray! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillShape: aShapeForm fillColor: aColor
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: 0@0! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillShape: aShapeForm fillColor: aColor at: location
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillWhite
	"Set all bits in the form to white."

	self fill: self boundingBox fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillWhite: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillWithColor: aColor
	"Fill the receiver's bounding box with the given color."

	self fill: self boundingBox fillColor: aColor! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse
	"

	self fill: self boundingBox rule: Form reverse fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
reverse: aRectangle
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
reverse: aRectangle fillColor: aMask	
	"Change all the bits in the receiver's area that intersects with aRectangle 
	according to the mask. Black does not necessarily turn to white, rather it 
	changes with respect to the rule and the bit in a corresponding mask 
	location. Bound to give a surprise."

	self fill: aRectangle rule: Form reverse fillColor: aMask! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:19'!
display 
	"Display the receiver on the Display at location 0,0."

	self displayOn: Display! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:18'!
displayAt: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display
		at: aDisplayPoint
		clippingBox: Display boundingBox
		rule: Form over
		fillColor: nil! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:19'!
displayOn: aDisplayMedium
	"Simple default display in order to see the receiver in the upper left 
	corner of screen."

	self displayOn: aDisplayMedium at: 0 @ 0! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:19'!
displayOn: aForm at: aDisplayPoint
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn _ self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule _ Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing this might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn _ self asFormOfDepth: aForm depth ]."
			rule _ Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule
		fillColor: nil! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:19'!
displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
		fillColor: nil! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 1/22/2015 10:19'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."
	| file |
	file _ FileStream newFileNamed: fileName.
	file binary.
	file nextPut: 2.  "file code = 2"
	self writeOn: file.
	file close
"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !


!Form methodsFor: 'bordering' stamp: 'jmv 1/22/2015 10:22'!
border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: combinationRule
		fillColor: aHalfTone! !

!methodRemoval: DisplayScreen class #boundingBox!
DisplayScreen class removeSelector: #boundingBox!
!methodRemoval: DisplayScreen #height!
DisplayScreen removeSelector: #height!
!methodRemoval: DisplayScreen #width!
DisplayScreen removeSelector: #width!
!classDefinition: #Form category: #'Graphics-Display Objects'!
Object subclass: #Form
	instanceVariableNames: 'bits width height depth offset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!Form reorganize!
('accessing' bits bits: bitsSize center depth depth: extent getCanvas height nativeDepth offset offset: size width)
('analyzing' cgForPixelValue:orNot: colorsUsed dominantColor innerPixelRectFor:orNot: pixelCompare:with:at: primCountBits rectangleEnclosingPixelsNotOfColor: tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width: border:width:fillColor: border:width:rule:fillColor: border:widthRectangle:rule:fillColor: borderFormOfWidth:sharpCorners: borderWidth: shapeBorder:width: shapeBorder:width:interiorPoint:sharpCorners:internal:)
('coloring' fill:fillColor: fillBlack fillBlack: fillColor: fillGray fillGray: fillShape:fillColor: fillShape:fillColor:at: fillWhite fillWhite: fillWithColor: reverse reverse: reverse:fillColor:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: makeBWForm: mapColor:to: mapColors:to: maskingMap newColorMap pixelValueFor: reducedPaletteOfSize:)
('converting' as8BitColorForm asCursorForm asFormOfDepth: asFormOfNativeDepth: asGrayScale asGrayScale: colorReduced convertAlphaToZeroValueTransparency copyWithColorsReducedTo: icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: contentsOfArea:into: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule:fillColor: copyBits:from:at:clippingBox:rule:fillColor:map: copyBits:from:at:colorMap: postCopy)
('display box access' boundingBox)
('displaying' displayOn:at:clippingBox:rule:fillColor: paintBits:at:translucent:)
('displaying-generic' display displayAt: displayOn: displayOn:at: displayOn:at:rule:)
('encoding' addDeltasFrom: deltaFrom: deltaFrom:at:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: readFromOldFormat: storeBitsOn:base: storeOn: storeOn:base: writeAttributesOn: writeBMPfileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnFileNamed:)
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
('*Graphics-Files-Additional' writePNGfileNamed:)
('*extendedClipboard-Win32' fromWin32BitMap:)
!

!methodRemoval: Color #display!
Color removeSelector: #display!
!classRemoval: #DisplayMedium!
Smalltalk removeClassNamed: #DisplayMedium!
!classRemoval: #DisplayObject!
Smalltalk removeClassNamed: #DisplayObject!
