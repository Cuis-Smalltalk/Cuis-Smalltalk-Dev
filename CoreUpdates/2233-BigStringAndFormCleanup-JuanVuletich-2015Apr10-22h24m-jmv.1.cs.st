'From Cuis 4.2 of 25 July 2013 [latest update: #2232] on 10 April 2015 at 11:47:06.118297 pm'!

!Form methodsFor: 'copying' stamp: 'jmv 4/10/2015 23:19'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule
	"Make up a BitBlt table and copy the bits."

	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) copyBits! !

!Form methodsFor: 'copying' stamp: 'jmv 4/10/2015 23:24'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map
	"Make up a BitBlt table and copy the bits.  Use a colorMap."

	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) colorMap: map) copyBits! !

!Form methodsFor: 'displaying' stamp: 'jmv 4/10/2015 23:25'!
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: rule

	aDisplayMedium copyBits: self boundingBox
		from: self
		at: aDisplayPoint + self offset
		clippingBox: clipRectangle
		rule: rule
		map: (self colormapIfNeededFor: aDisplayMedium).
! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 4/10/2015 23:38'!
replaceColor: oldColor with: newColor
	"Replace one color with another everywhere is this form"

	| cm newInd target ff |
	self depth = 32
		ifTrue: [cm _ (Color  cachedColormapFrom: 16 to: 32) copy]
		ifFalse: [cm _ Bitmap new: (1 bitShift: (self depth min: 15)).
				1 to: cm size do: [:i | cm at: i put: i - 1]].
	newInd _ newColor pixelValueForDepth: self depth.
	cm at: (oldColor pixelValueForDepth: (self depth min: 16))+1 put: newInd.
	target _ newColor isTransparent 
		ifTrue: [ff _ Form extent: self extent depth: depth.
			ff fillWithColor: newColor.  ff]
		ifFalse: [self].
	(BitBlt toForm: target)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form paint;
		destX: 0 destY: 0 width: width height: height;
		colorMap: cm;
		copyBits.
	newColor = Color transparent 
		ifTrue: [target displayOn: self].! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 4/10/2015 23:21'!
makeBoldGlyphs
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"
	| g bonkForm |
	g _ glyphs copy.
	bonkForm _ (Form extent: 1@16) fillBlack offset: -1@0.
	self bonk: g with: bonkForm.
	glyphs depth = 1 ifTrue: [
		g copyBits: g boundingBox from: g at: (1@0)
			clippingBox: g boundingBox rule: Form under ]
		ifFalse: [
			0 to: g width - 2 do: [ :x | 0 to: g height-1 do: [ :y |
				(glyphs colorAt:  x@y) = Color white ifFalse: [
					g colorAt: x+1@y put: 
						((glyphs colorAt: x+1@y) = Color white 
							ifTrue: [glyphs colorAt:  x@y]
							ifFalse: [Color black])]]]].
	glyphs _ g.
	self isSynthetic: true! !


!BitBlt methodsFor: 'private' stamp: 'jmv 4/10/2015 23:14'!
setDestForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect

	| aPoint |
	destForm _ df.
	sourceForm _ sf.
	self fillColor: fc.
	combinationRule _ cr.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ extent x.
	height _ extent y.
	aPoint _ clipRect origin.
	clipX _ aPoint x.
	clipY _ aPoint y.
	aPoint _ clipRect corner.
	clipWidth _ aPoint x - clipX.
	clipHeight _ aPoint y - clipY.
	sourceForm ifNotNil: [
		colorMap _ sourceForm colormapIfNeededFor: destForm]! !


!Bitmap methodsFor: 'as yet unclassified' stamp: 'jmv 4/10/2015 23:22'!
asByteArray
	"Faster way to make a byte array from me.
	copyFromByteArray: makes equal Bitmap."
	| f bytes hack |
	f _ Form extent: 4@self size depth: 8 bits: self.
	bytes _ ByteArray new: self size * 4.
	hack _ Form new hackBits: bytes.
	Smalltalk isLittleEndian ifTrue:[hack swapEndianness].
	hack copyBits: f boundingBox
		from: f
		at: (0@0)
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes.
! !


!String methodsFor: 'converting' stamp: 'jmv 4/10/2015 22:42'!
asUnaccented
	"
	'Hello' asUnaccented
	'αιξτσό' asUnaccented
	"
	^(self anySatisfy: [ :c | c isAccented])
		ifFalse: [ self ]
		ifTrue: [ self collect: [ :c | c asUnaccented ]]! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 4/10/2015 22:25'!
copyright
	"The Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2015.'! !


!Form methodsFor: 'copying' stamp: 'jmv 4/10/2015 23:20'!
contentsOfArea: aRect into: newForm
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	^ newForm copyBits: aRect from: self at: 0@0
		clippingBox: newForm boundingBox rule: Form over! !

!Form methodsFor: 'copying' stamp: 'jmv 4/10/2015 23:20'!
copy: aRect
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	^ newForm copyBits: aRect from: self at: 0@0
		clippingBox: newForm boundingBox rule: Form over! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 4/10/2015 23:23'!
displayAt: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display
		at: aDisplayPoint
		clippingBox: Display boundingBox
		rule: Form over! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 4/10/2015 23:24'!
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
		rule: rule! !

!Form methodsFor: 'filling' stamp: 'jmv 4/10/2015 23:25'!
eraseShape: bwForm
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt destForm: self sourceForm: bwForm
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits.
! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 4/10/2015 23:27'!
smear: dir distance: dist
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb _ BitBlt destForm: self sourceForm: self
		combinationRule: Form under destOrigin: 0@0 sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox.
	skew _ 1.
	[skew < dist] whileTrue:
		[bb destOrigin: dir*skew; copyBits.
		skew _ skew+skew]! !

!Form methodsFor: 'transitions' stamp: 'jmv 4/10/2015 23:21'!
slideImage: otherImage at: topLeft delta: delta
	"Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@-4"
	| bb nSteps clipRect |
	bb _ otherImage boundingBox.
	clipRect _ topLeft extent: otherImage extent.
	nSteps _ 1.
	delta x = 0 ifFalse: [nSteps _ nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps _ nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do:
			[:i | self copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceDisplayUpdate]! !

!Form methodsFor: 'transitions' stamp: 'jmv 4/10/2015 23:21'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect _ clipRect intersect: clipBox ].
	[ rectsOrNil _ rectForIndexBlock value: (i _ i + 1) ]
		whileNotNil: [
			t _ Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
			Display forceDisplayUpdate.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]! !

!Form methodsFor: 'initialization' stamp: 'jmv 4/10/2015 23:20'!
fromDisplay: aRectangle 
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: 0 @ 0
		clippingBox: self boundingBox
		rule: Form over! !


!Form class methodsFor: 'examples' stamp: 'jmv 4/10/2015 23:21'!
xorHack: size  "Display restoreAfter: [Form xorHack: 256]"
	"Draw a smiley face or stick figure, and end with option-click.
	Thereafter image gets 'processed' as long as you have button down.
	If you stop at just the right time, you'll see you figure upside down,
	and at the end of a full cycle, you'll see it perfectly restored.
	Dude -- this works in color too!!"
	| rect form i bb |
	rect _ 5@5 extent: size@size.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	Display border: (rect topRight - (0@2) extent: rect extent*2 + 4) width: 2.
	Form exampleSketch.
	form _ Form fromDisplay: rect.
	bb _ form boundingBox.
	i _ 0.
	[Sensor isMouseButton2Pressed] whileFalse:
		[[Sensor isMouseButton1Pressed] whileTrue:
			[i _ i + 1.
			(Array with: 0@1 with: 0@-1 with: 1@0 with: -1@0) do:
				[:d | form copyBits: bb from: form at: d
					clippingBox: bb rule: Form reverse ].
			form displayAt: rect topLeft.
			i+2\\size < 4 ifTrue: [(Delay forMilliseconds: 300) wait]].
		(form magnify: form boundingBox by: 2@2) displayAt: rect topRight + (2@0).
		Sensor waitButton].! !


!ColorForm methodsFor: 'copying' stamp: 'jmv 4/10/2015 23:19'!
copy: aRect
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm
! !


!ColorForm class methodsFor: 'as yet unclassified' stamp: 'jmv 4/10/2015 23:20'!
mappingWhiteToTransparentFrom: aFormOrCursor
	"Return a ColorForm copied from the given Form or Cursor with white mapped to transparent."

	| f map |
	aFormOrCursor depth <= 8 ifFalse: [
		^ self error: 'argument depth must be 8-bits per pixel or less'].
	(aFormOrCursor is: #ColorForm) ifTrue: [
		f _ aFormOrCursor copy.
		map _ aFormOrCursor colors.
	] ifFalse: [
		f _ ColorForm extent: aFormOrCursor extent depth: aFormOrCursor depth.
		f copyBits: aFormOrCursor boundingBox
			from: aFormOrCursor
			at: 0@0
			clippingBox: aFormOrCursor boundingBox
			rule: Form over.
		map _ Color indexedColors copyFrom: 1 to: (1 bitShift: aFormOrCursor depth)].
	map _ map collect: [:c |
		c = Color white ifTrue: [Color transparent] ifFalse: [c]].
	f colors: map.
	^ f
! !


!Morph class reorganize!
('instance creation' initializedInstance)
('new-morph participation' includeInNewMorphMenu)
('release' unload)
!

!methodRemoval: ColorForm class #twoToneFromDisplay:using:backgroundColor:!
ColorForm class removeSelector: #twoToneFromDisplay:using:backgroundColor:!
!methodRemoval: ColorForm #colorsUsed!
ColorForm removeSelector: #colorsUsed!
!methodRemoval: ColorForm #ensureTransparentColor!
ColorForm removeSelector: #ensureTransparentColor!
!methodRemoval: ColorForm #mapColor:to:!
ColorForm removeSelector: #mapColor:to:!
!methodRemoval: ColorForm #replaceColorAt:with:!
ColorForm removeSelector: #replaceColorAt:with:!
!methodRemoval: ColorForm #transparentAllPixelsLike:!
ColorForm removeSelector: #transparentAllPixelsLike:!
!methodRemoval: ColorForm #transparentColor:!
ColorForm removeSelector: #transparentColor:!
!methodRemoval: ColorForm #twoToneFromDisplay:backgroundColor:!
ColorForm removeSelector: #twoToneFromDisplay:backgroundColor:!
!methodRemoval: Form class #exampleBorder!
Form class removeSelector: #exampleBorder!
!methodRemoval: Form class #exampleSpaceFill!
Form class removeSelector: #exampleSpaceFill!
!methodRemoval: Form #addDeltasFrom:!
Form removeSelector: #addDeltasFrom:!
!methodRemoval: Form #anyShapeFill!
Form removeSelector: #anyShapeFill!
!methodRemoval: Form #as8BitColorForm!
Form removeSelector: #as8BitColorForm!
!methodRemoval: Form #borderFormOfWidth:sharpCorners:!
Form removeSelector: #borderFormOfWidth:sharpCorners:!
!methodRemoval: Form #borderWidth:!
Form removeSelector: #borderWidth:!
!methodRemoval: Form #colorReduced!
Form removeSelector: #colorReduced!
!methodRemoval: Form #colorsUsed!
Form removeSelector: #colorsUsed!
!methodRemoval: Form #convexShapeFill:!
Form removeSelector: #convexShapeFill:!
!methodRemoval: Form #copyBits:at:translucent:!
Form removeSelector: #copyBits:at:translucent:!
!methodRemoval: Form #copyBits:from:at:clippingBox:rule:fillColor:!
Form removeSelector: #copyBits:from:at:clippingBox:rule:fillColor:!
!methodRemoval: Form #copyBits:from:at:clippingBox:rule:fillColor:map:!
Form removeSelector: #copyBits:from:at:clippingBox:rule:fillColor:map:!
!methodRemoval: Form #copyBits:from:at:colorMap:!
Form removeSelector: #copyBits:from:at:colorMap:!
!methodRemoval: Form #copyWithColorsReducedTo:!
Form removeSelector: #copyWithColorsReducedTo:!
!methodRemoval: Form #deltaFrom:!
Form removeSelector: #deltaFrom:!
!methodRemoval: Form #deltaFrom:at:!
Form removeSelector: #deltaFrom:at:!
!methodRemoval: Form #displayOn:at:clippingBox:rule:fillColor:!
Form removeSelector: #displayOn:at:clippingBox:rule:fillColor:!
!methodRemoval: Form #displayOn:at:rule:!
Form removeSelector: #displayOn:at:rule:!
!methodRemoval: Form #fadeImage:at:indexAndMaskDo:!
Form removeSelector: #fadeImage:at:indexAndMaskDo:!
!methodRemoval: Form #fadeImageCoarse:at:!
Form removeSelector: #fadeImageCoarse:at:!
!methodRemoval: Form #fadeImageFine:at:!
Form removeSelector: #fadeImageFine:at:!
!methodRemoval: Form #fadeImageHor:at:!
Form removeSelector: #fadeImageHor:at:!
!methodRemoval: Form #fadeImageHorFine:at:!
Form removeSelector: #fadeImageHorFine:at:!
!methodRemoval: Form #fadeImageSquares:at:!
Form removeSelector: #fadeImageSquares:at:!
!methodRemoval: Form #fadeImageVert:at:!
Form removeSelector: #fadeImageVert:at:!
!methodRemoval: Form #findShapeAroundSeedBlock:!
Form removeSelector: #findShapeAroundSeedBlock:!
!methodRemoval: Form #floodFill2:at:!
Form removeSelector: #floodFill2:at:!
!methodRemoval: Form #floodFillMapFrom:to:mappingColorsWithin:to:!
Form removeSelector: #floodFillMapFrom:to:mappingColorsWithin:to:!
!methodRemoval: Form #formForColorCount:!
Form removeSelector: #formForColorCount:!
!methodRemoval: Form #makeBWForm:!
Form removeSelector: #makeBWForm:!
!methodRemoval: Form #mapColor:to:!
Form removeSelector: #mapColor:to:!
!methodRemoval: Form #mapColors:to:!
Form removeSelector: #mapColors:to:!
!methodRemoval: Form #privateFloodFillValue:!
Form removeSelector: #privateFloodFillValue:!
!methodRemoval: Form #rectangleEnclosingPixelsNotOfColor:!
Form removeSelector: #rectangleEnclosingPixelsNotOfColor:!
!methodRemoval: Form #reducedPaletteOfSize:!
Form removeSelector: #reducedPaletteOfSize:!
!methodRemoval: Form #replaceColor:withColor:!
Form removeSelector: #replaceColor:withColor:!
!methodRemoval: Form #shapeBorder:width:!
Form removeSelector: #shapeBorder:width:!
!methodRemoval: Form #shapeBorder:width:interiorPoint:sharpCorners:internal:!
Form removeSelector: #shapeBorder:width:interiorPoint:sharpCorners:internal:!
!methodRemoval: Form #shapeFill:interiorPoint:!
Form removeSelector: #shapeFill:interiorPoint:!
!methodRemoval: Form #shapeFill:seedBlock:!
Form removeSelector: #shapeFill:seedBlock:!
!methodRemoval: Form #trimBordersOfColor:!
Form removeSelector: #trimBordersOfColor:!

!Form reorganize!
('accessing' bits bits: bitsSize center depth depth: extent getCanvas height nativeDepth offset offset: size width)
('analyzing' cgForPixelValue:orNot: dominantColor innerPixelRectFor:orNot: pixelCompare:with:at: primCountBits tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width: border:width:fillColor: border:width:rule:fillColor: border:widthRectangle:rule:fillColor:)
('coloring' fill:fillColor: fillBlack fillBlack: fillColor: fillGray fillGray: fillShape:fillColor: fillShape:fillColor:at: fillWhite fillWhite: fillWithColor: reverse reverse: reverse:fillColor:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: maskingMap newColorMap pixelValueFor:)
('converting' asCursorForm asFormOfDepth: asFormOfNativeDepth: asGrayScale asGrayScale: convertAlphaToZeroValueTransparency icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: contentsOfArea:into: copy: copy:from:in:rule: copy:from:to:rule: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: postCopy)
('display box access' boundingBox)
('displaying' displayOn:at:clippingBox:rule: paintBits:at:translucent:)
('displaying-generic' display displayAt: displayOn: displayOn:at:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: storeBitsOn:base: storeOn: storeOn:base: writeAttributesOn: writeBMPfileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnFileNamed:)
('filling' eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock:)
('image manipulation' replaceColor:with: smear:distance: tallyPixelValues)
('other' fixAlpha makeAllPixelsOpaque preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: isTransparentAt: pixelValueAt: pixelValueAt:put: primPixelValueAtX:y:)
('scaling, rotation' flippedBy:centerAt: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotatedBy:smoothing: rotatedByDegrees: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent)
('transitions' pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' hackBits: initFromArray: setExtent:depth: setExtent:depth:bits:)
('comparing' = hash)
('initialization' fromDisplay: swapEndianness)
!


!SystemDictionary class reorganize!
('initialization' initialize)
('copyright' copyright)
!

!methodRemoval: Symbol #stringhash!
Symbol removeSelector: #stringhash!
!methodRemoval: String class #example!
String class removeSelector: #example!
!methodRemoval: String class #fromPacked:!
String class removeSelector: #fromPacked:!

!String class reorganize!
('instance creation' addUnicodeCodePoint:to:hex: crString crlfString fromString: fromUtf8: fromUtf8:hex:trimLastNull: lfString newLineString readFrom: tab value:)
('initialization' initialize)
('primitives' findFirstInString:inSet:startingAt: indexOfAscii:inString:startingAt: stringHash:initialHash: translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
!

!methodRemoval: String #asPacked!
String removeSelector: #asPacked!
!methodRemoval: String #asSignedInteger!
String removeSelector: #asSignedInteger!
!methodRemoval: String #do:toFieldNumber:!
String removeSelector: #do:toFieldNumber:!
!methodRemoval: String #encodeDoublingQuoteOn:!
String removeSelector: #encodeDoublingQuoteOn:!
!methodRemoval: String #endsWithAnyOf:!
String removeSelector: #endsWithAnyOf:!
!methodRemoval: String #endsWithDigit!
String removeSelector: #endsWithDigit!
!methodRemoval: String #findCloseParenthesisFor:!
String removeSelector: #findCloseParenthesisFor:!
!methodRemoval: String #findTokens:includes:!
String removeSelector: #findTokens:includes:!
!methodRemoval: String #findWordStart:startingAt:!
String removeSelector: #findWordStart:startingAt:!
!methodRemoval: String #getInteger32:!
String removeSelector: #getInteger32:!
!methodRemoval: String #hasAccents!
String removeSelector: #hasAccents!
!methodRemoval: String #indexOfAnyOf:!
String removeSelector: #indexOfAnyOf:!
!methodRemoval: String #indexOfAnyOf:ifAbsent:!
String removeSelector: #indexOfAnyOf:ifAbsent:!
!methodRemoval: String #indexOfAnyOf:startingAt:!
String removeSelector: #indexOfAnyOf:startingAt:!
!methodRemoval: String #isAllDigits!
String removeSelector: #isAllDigits!
!methodRemoval: String #isAllSeparators!
String removeSelector: #isAllSeparators!
!methodRemoval: String #lastSpacePosition!
String removeSelector: #lastSpacePosition!
!methodRemoval: String #numericSuffix!
String removeSelector: #numericSuffix!
!methodRemoval: String #onlyLetters!
String removeSelector: #onlyLetters!
!methodRemoval: String #periodPrefixAndSuffix!
String removeSelector: #periodPrefixAndSuffix!
!methodRemoval: String #prefixAndSuffix:!
String removeSelector: #prefixAndSuffix:!
!methodRemoval: String #putInteger32:at:!
String removeSelector: #putInteger32:at:!
!methodRemoval: String #splitInteger!
String removeSelector: #splitInteger!
!methodRemoval: String #startsWithDigit!
String removeSelector: #startsWithDigit!
!methodRemoval: String #stemAndNumericSuffix!
String removeSelector: #stemAndNumericSuffix!
!methodRemoval: String #stringhash!
String removeSelector: #stringhash!
!methodRemoval: String #tabDelimitedFieldsDo:!
String removeSelector: #tabDelimitedFieldsDo:!
!methodRemoval: String #unescapePercents!
String removeSelector: #unescapePercents!
!methodRemoval: String #unparenthetically!
String removeSelector: #unparenthetically!
!methodRemoval: String #withNoLineLongerThan:!
String removeSelector: #withNoLineLongerThan:!
!methodRemoval: String #withoutLeadingDigits!
String removeSelector: #withoutLeadingDigits!
!methodRemoval: String #withoutPrefix:!
String removeSelector: #withoutPrefix:!
!methodRemoval: String #withoutQuoting!
String removeSelector: #withoutQuoting!
!methodRemoval: String #withoutTrailingDigits!
String removeSelector: #withoutTrailingDigits!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , copyReplaceTokens:with: padded:to:with:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asCamelCase asCharacter asDate asFileName asHex asIdentifier: asInteger asLegalSelector asLowercase asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: base64Decoded base64Encoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector initialIntegerOrNil keywords romanNumber sansPeriodSuffix squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withFirstCharacterDownshifted withLineEndings: withNewLines withoutSuffix: withoutTrailingBlanks)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
!

