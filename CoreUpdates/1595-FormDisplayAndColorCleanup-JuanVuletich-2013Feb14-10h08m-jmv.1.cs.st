'From Cuis 4.1 of 12 December 2012 [latest update: #1594] on 14 February 2013 at 10:27:01 am'!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: 'clippingBox extraRegions '
	classVariableNames: 'DeferringUpdates DisplayChangeSignature ScreenSave '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 2/2/2013 00:21'!
copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf 
	(BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: clipRect) copyBits! !

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 2/2/2013 00:22'!
copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf map: map
	((BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: clipRect) colorMap: map) copyBits! !

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 2/2/2013 00:13'!
forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."

	allDamage do: [ :r | self forceToScreen: r ]! !

!DisplayScreen methodsFor: 'private' stamp: 'jmv 2/2/2013 00:22'!
setExtent: aPoint depth: bitsPerPixel  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth = bitsPerPixel and: [aPoint = self extent and: 
					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits _ nil.  "Free up old bitmap in case space is low"
		(self supportsDisplayDepth: bitsPerPixel)
			ifTrue:[super setExtent: aPoint depth: bitsPerPixel]
			ifFalse:[(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[super setExtent: aPoint depth: bitsPerPixel negated]
				ifFalse:["Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth]].
	]! !


!FormCanvas methodsFor: 'private' stamp: 'jmv 2/1/2013 23:52'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		(paintColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].
		^port fillPattern: paintColor; combinationRule: Form paint ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 16]) ifTrue: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillPattern: paintColor.
	self depth = 16
		ifTrue: [ port alphaBits: paintColor privateAlpha; combinationRule: 31 ]
		ifFalse: [ port combinationRule: Form blend ]! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 2/2/2013 00:14'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [ self assuredNonDisplayCanvas ].

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	handsToDraw reverseDo: [ :h | canvas fullDraw: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ Display forceDamageToScreen: allDamage ]
		ifFalse: [ canvas showAt: aWorld viewBox origin invalidRects: allDamage ].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!methodRemoval: WorldState #forceDamageToScreen:!
WorldState removeSelector: #forceDamageToScreen:!
!methodRemoval: TranslucentColor #balancedPatternForDepth:!
TranslucentColor removeSelector: #balancedPatternForDepth:!
!methodRemoval: TranslucentColor #scaledPixelValue32!
TranslucentColor removeSelector: #scaledPixelValue32!
!methodRemoval: Form class #exampleEdits!
Form class removeSelector: #exampleEdits!
!methodRemoval: Form class #exampleMagnify!
Form class removeSelector: #exampleMagnify!
!methodRemoval: Form class #exampleShrink!
Form class removeSelector: #exampleShrink!
!methodRemoval: Form class #extent:fromStipple:!
Form class removeSelector: #extent:fromStipple:!
!methodRemoval: Form class #fromDisplay:using:!
Form class removeSelector: #fromDisplay:using:!

!Form class reorganize!
('instance creation' dotOfSize: extent: extent:depth: extent:depth:bits: extent:depth:fromArray:offset: extent:fromArray:offset: extent:offset: fromBinaryStream: fromDisplay: fromFileNamed: fromFileNamedOrNil: fromUser fromUserWithExtent:)
('mode constants' and blend blendAlpha erase erase1bitShape oldErase1bitShape oldPaint over paint paintAlpha reverse rgbMul under)
('examples' exampleBorder exampleSketch exampleSpaceFill makeStar toothpaste: xorHack:)
('shut down' shutDown)
('class initialization' unload)
('creation - anti aliased' bottomLeftCorner:height:gradientTop:gradientBottom: bottomRightCorner:height:gradientTop:gradientBottom: topLeftCorner:height:gradientTop:gradientBottom: topRightCorner:height:gradientTop:gradientBottom:)
('instance protocol testing' gatherProtocols)
!

!methodRemoval: DisplayScreen #addExtraRegion:for:!
DisplayScreen removeSelector: #addExtraRegion:for:!
!methodRemoval: DisplayScreen #boundingBox!
DisplayScreen removeSelector: #boundingBox!
!methodRemoval: DisplayScreen #clippingTo:do:!
DisplayScreen removeSelector: #clippingTo:do:!
!methodRemoval: DisplayScreen #copyFrom:!
DisplayScreen removeSelector: #copyFrom:!
!methodRemoval: DisplayScreen #deferUpdatesIn:while:!
DisplayScreen removeSelector: #deferUpdatesIn:while:!
!methodRemoval: DisplayScreen #displayChangeSignature!
DisplayScreen removeSelector: #displayChangeSignature!
!methodRemoval: DisplayScreen #flash:andWait:!
DisplayScreen removeSelector: #flash:andWait:!
!methodRemoval: DisplayScreen #fullBoundingBox!
DisplayScreen removeSelector: #fullBoundingBox!
!methodRemoval: DisplayScreen #fullScreen!
DisplayScreen removeSelector: #fullScreen!
!methodRemoval: DisplayScreen #replacedBy:do:!
DisplayScreen removeSelector: #replacedBy:do:!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'DeferringUpdates'
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!
!methodRemoval: ColorForm #colorsFromArray:!
ColorForm removeSelector: #colorsFromArray:!
!methodRemoval: ColorForm #decodeArray!
ColorForm removeSelector: #decodeArray!
!methodRemoval: ColorForm #getTransparencyUnificationLUT!
ColorForm removeSelector: #getTransparencyUnificationLUT!
!methodRemoval: ColorForm #mapTransparencies:!
ColorForm removeSelector: #mapTransparencies:!
!methodRemoval: ColorForm #transparentColorIndexes!
ColorForm removeSelector: #transparentColorIndexes!

!ColorForm reorganize!
('accessing' colors colors:)
('displaying' displayOn:at:clippingBox:rule:fillColor: displayOnPort:at: maskingMap)
('pixel accessing' colorAt: colorAt:put: isTransparentAt: pixelValueAt:)
('color manipulation' asGrayScale colormapIfNeededForDepth: colorsUsed ensureTransparentColor indexOfColor: mapColor:to: replaceColor:with: replaceColorAt:with: replaceColorAtIndex:with: transparentAllPixelsLike: transparentColor: twoToneFromDisplay:backgroundColor:)
('copying' asCursorForm blankCopyOf:scaledBy: copy: postCopy)
('private' clearColormapCache depth: ensureColorArrayExists setColors:cachedColormap:depth: setExtent:depth: unusedColormapEntry)
('scaling, rotation' flippedBy:centerAt:)
('fileIn/Out' hibernate readAttributesFrom: storeOn: unhibernate writeAttributesOn:)
('color mapping' colormapIfNeededFor:)
('testing' mightBeTranslucent)
!

!methodRemoval: Form #balancedPatternFor:!
Form removeSelector: #balancedPatternFor:!
!methodRemoval: Form #bitsPerComponent!
Form removeSelector: #bitsPerComponent!
!methodRemoval: Form #borderWidth:color:!
Form removeSelector: #borderWidth:color:!
!methodRemoval: Form #borderWidth:fillColor:!
Form removeSelector: #borderWidth:fillColor:!
!methodRemoval: Form #contentsOfArea:!
Form removeSelector: #contentsOfArea:!
!methodRemoval: Form #decodeArray!
Form removeSelector: #decodeArray!
!methodRemoval: Form #drawLine:from:to:clippingBox:rule:fillColor:!
Form removeSelector: #drawLine:from:to:clippingBox:rule:fillColor:!
!methodRemoval: Form #graphicForViewerTab!
Form removeSelector: #graphicForViewerTab!
!methodRemoval: Form #hasBeenModified!
Form removeSelector: #hasBeenModified!
!methodRemoval: Form #hasBeenModified:!
Form removeSelector: #hasBeenModified:!
!methodRemoval: Form #numComponents!
Form removeSelector: #numComponents!
!methodRemoval: Form #paddedWidth!
Form removeSelector: #paddedWidth!
!methodRemoval: Form #readNativeResourceFrom:!
Form removeSelector: #readNativeResourceFrom:!
!methodRemoval: Form #readResourceFrom:!
Form removeSelector: #readResourceFrom:!
!methodRemoval: Form #replaceByResource:!
Form removeSelector: #replaceByResource:!
!methodRemoval: Form #resourceTag!
Form removeSelector: #resourceTag!
!methodRemoval: Form #rowPadding!
Form removeSelector: #rowPadding!
!methodRemoval: Form #setResourceBits:!
Form removeSelector: #setResourceBits:!
!methodRemoval: Form #store15To24HexBitsOn:!
Form removeSelector: #store15To24HexBitsOn:!
!methodRemoval: Form #store32To24HexBitsOn:!
Form removeSelector: #store32To24HexBitsOn:!
!methodRemoval: Form #storeHexBitsOn:!
Form removeSelector: #storeHexBitsOn:!
!methodRemoval: Form #storePostscriptHexOn:!
Form removeSelector: #storePostscriptHexOn:!
!methodRemoval: Form #storeResourceOn:!
Form removeSelector: #storeResourceOn:!

!Form reorganize!
('accessing' bits bits: bitsSize center depth depth: extent form getCanvas height nativeDepth offset offset: size width)
('analyzing' cgForPixelValue:orNot: colorsUsed dominantColor innerPixelRectFor:orNot: pixelCompare:with:at: primCountBits rectangleEnclosingPixelsNotOfColor: tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width:rule:fillColor: borderFormOfWidth:sharpCorners: borderWidth: shapeBorder:width: shapeBorder:width:interiorPoint:sharpCorners:internal:)
('color mapping' bitPatternFor: colormapFromARGB colormapIfNeededFor: colormapIfNeededForDepth: colormapToARGB makeBWForm: mapColor:to: mapColors:to: maskingMap newColorMap pixelValueFor: pixelWordFor: reducedPaletteOfSize: rgbaBitMasks)
('converting' as8BitColorForm asCursorForm asFormOfDepth: asFormOfNativeDepth: asGrayScale colorReduced copyWithColorsReducedTo: orderedDither32To16)
('copying' blankCopyOf:scaledBy: contentsOfArea:into: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule:fillColor: copyBits:from:at:clippingBox:rule:fillColor:map: copyBits:from:at:colorMap: postCopy)
('display box access' boundingBox computeBoundingBox)
('displaying' displayOn:at:clippingBox:rule:fillColor: displayOnPort:at: displayResourceFormOn: paintBits:at:translucent:)
('encoding' addDeltasFrom: deltaFrom: deltaFrom:at:)
('fileIn/Out' hibernate printOn: readAttributesFrom: readBitsFrom: readFrom: readFromOldFormat: storeBits:to:on: storeBitsOn:base: storeOn: storeOn:base: unhibernate writeAttributesOn: writeBMPfileNamed: writeBitsOn: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnMovie: writePNGfileNamed: writeUncompressedOn:)
('filling' anyShapeFill bitPatternForDepth: convexShapeFill: eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock: findShapeAroundSeedBlock: floodFill2:at: floodFillMapFrom:to:mappingColorsWithin:to: shapeFill:interiorPoint: shapeFill:seedBlock:)
('image manipulation' replaceColor:withColor: smear:distance: tallyPixelValues trimBordersOfColor:)
('initialize-release' fromDisplay: shutDown swapEndianness)
('other' fixAlpha formForColorCount: preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: isTransparentAt: pixelValueAt: pixelValueAt:put:)
('scaling, rotation' flippedBy:centerAt: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotateBy: rotateBy:smoothing: shrink:by:)
('testing' couldBeTranslucent hasNonStandardPalette isAllWhite isBigEndian isLittleEndian mightBeTranslucent shouldPreserveContents)
('transitions' fadeImage:at:indexAndMaskDo: fadeImageCoarse:at: fadeImageFine:at: fadeImageHor:at: fadeImageHorFine:at: fadeImageSquares:at: fadeImageVert:at: pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' hackBits: initFromArray: privateFloodFillValue: setExtent:depth: setExtent:depth:bits:)
('comparing' =)
!

!methodRemoval: DisplayMedium #deferUpdatesIn:while:!
DisplayMedium removeSelector: #deferUpdatesIn:while:!
!methodRemoval: DisplayMedium #drawLine:from:to:clippingBox:rule:fillColor:!
DisplayMedium removeSelector: #drawLine:from:to:clippingBox:rule:fillColor:!
!methodRemoval: Color #asColorref!
Color removeSelector: #asColorref!
!methodRemoval: Color #balancedPatternForDepth:!
Color removeSelector: #balancedPatternForDepth:!
!methodRemoval: Color #pixelValue32!
Color removeSelector: #pixelValue32!
!methodRemoval: Color #rgbTriplet!
Color removeSelector: #rgbTriplet!
!methodRemoval: Color #scaledPixelValue32!
Color removeSelector: #scaledPixelValue32!
!methodRemoval: Color #setRGB:!
Color removeSelector: #setRGB:!
