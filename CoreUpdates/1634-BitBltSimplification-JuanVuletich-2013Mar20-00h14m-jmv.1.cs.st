'From Cuis 4.1 of 12 December 2012 [latest update: #1630] on 20 March 2013 at 1:01 am'!
!classDefinition: #GrafPort category: #'Morphic-Support'!
BitBlt subclass: #GrafPort
	instanceVariableNames: 'alpha '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!GrafPort commentStamp: '<historical>' prior: 0!
Add services for FormCanvas!


!BitBlt methodsFor: 'accessing' stamp: 'jmv 3/20/2013 00:28'!
fillBitmap: aBitmap
	"Sets the receivers half tone form. See class commment."
	
	halftoneForm _ aBitmap! !

!BitBlt methodsFor: 'accessing' stamp: 'jmv 3/20/2013 00:50'!
fillColor
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ Color black].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth! !

!BitBlt methodsFor: 'private' stamp: 'jmv 3/20/2013 00:38'!
oldErase1bitShapeBits
	"Perform the erase operation, which puts 0's in the destination
	wherever the source (which is assumed to be just 1 bit deep)
	has a 1.  This requires the colorMap to be set in order to AND
	all 1's into the destFrom pixels regardless of their size."
	| oldFillBitmap oldMap |
	oldFillBitmap _ halftoneForm.
	halftoneForm _ nil.
	oldMap _ colorMap.
	colorMap _ Bitmap with: 0 with: 16rFFFFFFFF.
	combinationRule _ Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"
	halftoneForm _ oldFillBitmap.
	colorMap _ oldMap! !

!BitBlt methodsFor: 'private' stamp: 'jmv 3/20/2013 00:37'!
oldPaintBits
	"Perform the old paint operation, which requires two calls to BitBlt."
	| requestedFill oldMap requestedRule |
	sourceForm depth = 1 ifFalse: [
		^ self halt: 'paint operation is only defined for 1-bit deep sourceForms'].

	requestedRule _ combinationRule.
	requestedFill _ halftoneForm.
	oldMap _ colorMap.

	halftoneForm _ nil.
	colorMap _ Bitmap with: 0 with: 16rFFFFFFFF.	"Map 1's to ALL ones, not just one"
	combinationRule _ Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"

	halftoneForm _ requestedFill.
	combinationRule _ Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap _ oldMap.
	combinationRule _ requestedRule

" | dot |
dot _ Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor mousePoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"! !


!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 00:24'!
destForm: df sourceForm: sf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect 
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: nil
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect! !


!GrafPort methodsFor: 'text' stamp: 'jmv 3/20/2013 00:46'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: kernDelta.
	^ destX@destY! !

!GrafPort methodsFor: 'text' stamp: 'jmv 3/20/2013 00:45'!
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font color: foregroundColor kern: kernDelta
	"If required, do a second pass with new rule and colorMap"

	| answer prevRule secondPassMap sourceDepth destDepth |
	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^self].
	clipWidth = 0 ifTrue: [^self].

	self installStrikeFont: font foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font kern: kernDelta.
		combinationRule := prevRule ].
	^answer! !

!GrafPort methodsFor: 'private' stamp: 'jmv 3/20/2013 00:47'!
cachedFontColormapFrom1BitTo: destDepth

	| map |
	CachedFontColorMaps 
		ifNil: [CachedFontColorMaps _ Array new: 32].
	(CachedFontColorMaps at: destDepth) ifNotNil: [ :m | ^ m ].

	map _ (Color cachedColormapFrom: 1 to: destDepth) copy.
	CachedFontColorMaps at: destDepth put: map.
	^ map! !

!GrafPort methodsFor: 'private' stamp: 'jmv 3/20/2013 00:48'!
colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	
	| srcIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps 
		ifNil: [ColorConvertingMaps _ (1 to: 10) collect: [:i | Array new: 32]].
		
	srcIndex _ sourceDepth.
	sourceDepth > 8 ifTrue: [ srcIndex _ keepSubPix ifTrue: [9] ifFalse: [10] ].
	mapsForSource _ ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest _ mapsForSource at: destDepth) ifNil: [
		mapsForSourceAndDest _ mapsForSource at: destDepth put: Dictionary new ].
	
	map _ mapsForSourceAndDest at: targetColor ifAbsentPut: [
		Color 
			computeColorConvertingMap: targetColor 
			from: sourceDepth 
			to: destDepth 
			keepSubPixelAA: keepSubPix ].

	^ map! !

!GrafPort methodsFor: 'private' stamp: 'jmv 3/20/2013 00:47'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	| lastSourceDepth targetColor sourceDepth destDepth |
	sourceForm ifNotNil: [ lastSourceDepth _ sourceForm depth ].
	sourceForm _ aStrikeFont glyphs.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"
	sourceY _ 0.
	height _ aStrikeFont height.

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		(colorMap isNil or: [ lastSourceDepth ~= sourceDepth ]) ifTrue: [
			"Set up color map for a different source depth (color font)"
			"Uses caching for reasonable efficiency
			Warning: We are not considering that destDepth could change too, requiring a new colormap
			This seems not to cause any problems..."
			colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
			colorMap at: 1 put: (destForm pixelValueFor: Color transparent)].
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(Preferences subPixelRenderFonts and: [ foregroundColor = Color black or: [ 
				Preferences subPixelRenderColorFonts and: [ foregroundColor isOpaque ]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= Color black or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:kern:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = Color black ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display. TTCFont uses it, but it builds a glyphs cache for each color used!!"
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 3/20/2013 00:28'!
read24BmpFile
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex bitBlt |
	form := Form extent: biWidth@biHeight depth: 32.
	pixelLine := ByteArray new: (((24 * biWidth) + 31) // 32) * 4.
	bitsIndex := form height - 1 * biWidth + 1.
	formBits := form bits.
	1 to: biHeight do: [:i |
		pixelLine := stream nextInto: pixelLine.
		self read24BmpLine: pixelLine into: formBits startingAt: bitsIndex width: biWidth.
		bitsIndex := bitsIndex - biWidth.
	].
	bitBlt := BitBlt toForm: form.
	bitBlt combinationRule: 7 "bitOr:with:".
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits.
	^ form
! !

!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 3/20/2013 00:28'!
readIndexedBmpFile: colors
	"Read uncompressed pixel data of depth d from the given BMP stream, where d is 1, 4, 8, or 16"
	| form bytesPerRow pixelData pixelLine startIndex map bitBlt mask |
	colors 
		ifNil:[form := Form extent: biWidth@biHeight depth: biBitCount]
		ifNotNil:[form := ColorForm extent: biWidth@biHeight depth: biBitCount.
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
	form bits copyFromByteArray: pixelData.
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


!BitBlt methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:29'!
copy: destRectangle from: sourcePt in: srcForm
	| destOrigin |
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ 3.  "store"
	destOrigin _ destRectangle origin.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceX _ sourcePt x.
	sourceY _ sourcePt y.
	width _ destRectangle width.
	height _ destRectangle height.
	self copyBits! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:38'!
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

	"Check for compressed source, destination or halftone forms"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue:
		["No alpha specified -- re-run with alpha = 1.0"
		^ self copyBitsTranslucent: 255].
	((sourceForm is: #Form) and: [sourceForm unhibernate])
		ifTrue: [^ self copyBits].
	((destForm is: #Form) and: [destForm unhibernate])
		ifTrue: [^ self copyBits].
	((halftoneForm is: #Form) and: [halftoneForm unhibernate])
		ifTrue: [^ self copyBits].

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

!BitBlt methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:33'!
copyForm: srcForm to: destPt rule: rule colorMap: map
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	colorMap _ map.
	self copyBits! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:30'!
copyFrom: sourceRectangle in: srcForm to: destPt
	| sourceOrigin |
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ 3.  "store"
	destX _ destPt x.
	destY _ destPt y.
	sourceOrigin _ sourceRectangle origin.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ sourceRectangle width.
	height _ sourceRectangle height.
	colorMap _ srcForm colormapIfNeededFor: destForm.
	self copyBits! !


!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 00:24'!
bitPeekerFromForm: sourceForm
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord |
	pixPerWord _ 32 // sourceForm depth.
	sourceForm unhibernate.
	^ self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: sourceForm
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: (0@0 extent: pixPerWord@1)
! !

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 00:24'!
bitPokerToForm: destForm
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord |
	pixPerWord _ 32//destForm depth.
	destForm unhibernate.
	^ self destForm: destForm
	 	sourceForm: (Form extent: pixPerWord@1 depth: destForm depth)
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: (pixPerWord-1)@0
		extent: 1@1
		clipRect: (0@0 extent: destForm extent)
! !

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 00:54'!
destForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect 
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: fc
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect! !


!Form methodsFor: 'analyzing' stamp: 'jmv 3/20/2013 00:52'!
tallyPixelValuesInRect: destRect into: valueTable
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: 0@0;
		colorMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor anyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/20/2013 00:25'!
xTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: 1@height.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: 0@0 sourceOrigin: 0@0 extent: 1 @ slice height
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (0@0 extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect:
		[:x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/20/2013 00:25'!
yTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: 0@0 sourceOrigin: 0@0 extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (0@0 extent: slice extent);
				combinationRule: 32.
	^ (0 to: height-1) collect:
		[:y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:24'!
copyBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'copying' stamp: 'jmv 3/20/2013 00:25'!
copyBits: sourceRect from: sourceForm at: destOrigin colorMap: map 
	"Make up a BitBlt table and copy the bits with the given colorMap."
	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: Form over
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: self boundingBox)
			colorMap: map)
				copyBits! !

!Form methodsFor: 'displaying' stamp: 'jmv 3/20/2013 00:25'!
paintBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !


!FormCanvas methodsFor: 'private' stamp: 'jmv 3/20/2013 00:19'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		^self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	port combinationRule: Form blend! !


!Pen methodsFor: 'operations' stamp: 'jmv 3/20/2013 00:17'!
fill: drawBlock color: color
	self color: color.
	drawBlock value: self! !

!methodRemoval: GrafPort #alphaBits:!
GrafPort removeSelector: #alphaBits:!
!methodRemoval: GrafPort #copyBits!
GrafPort removeSelector: #copyBits!
!classDefinition: #GrafPort category: #'Morphic-Support'!
BitBlt subclass: #GrafPort
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!GrafPort reorganize!
('copying' clippedBy:)
('drawing support' fillOval: fillRect: frameOval:borderWidth: frameRect:borderWidth: frameRectBottom:height: frameRectRight:width: frameRectTopLeft:borderWidth: image:at:sourceRect:rule: stencil:at:sourceRect:)
('text' basicDisplayString:from:to:at:strikeFont:kern: displayString:from:to:at:strikeFont:color:kern:)
('private' cachedFontColormapFrom1BitTo: colorConvertingMap:from:to:keepSubPixelAA: installStrikeFont:foregroundColor:)
!

!methodRemoval: BitBlt class #destForm:sourceForm:halftoneForm:combinationRule:destOrigin:sourceOrigin:extent:clipRect:!
BitBlt class removeSelector: #destForm:sourceForm:halftoneForm:combinationRule:destOrigin:sourceOrigin:extent:clipRect:!
!methodRemoval: BitBlt class #exampleAt:rule:fillColor:!
BitBlt class removeSelector: #exampleAt:rule:fillColor:!
!methodRemoval: BitBlt class #exampleOne!
BitBlt class removeSelector: #exampleOne!
!methodRemoval: BitBlt class #exampleTwo!
BitBlt class removeSelector: #exampleTwo!

!BitBlt class reorganize!
('instance creation' bitPeekerFromForm: bitPokerToForm: destForm:sourceForm:combinationRule:destOrigin:sourceOrigin:extent:clipRect: destForm:sourceForm:fillColor:combinationRule:destOrigin:sourceOrigin:extent:clipRect: toForm:)
('examples' alphaBlendDemo exampleColorMap)
('class initialization' recreateColorMaps)
!

!methodRemoval: BitBlt #basicDisplayString:from:to:at:strikeFont:kern:!
BitBlt removeSelector: #basicDisplayString:from:to:at:strikeFont:kern:!
!methodRemoval: BitBlt #cachedFontColormapFrom1BitTo:!
BitBlt removeSelector: #cachedFontColormapFrom1BitTo:!
!methodRemoval: BitBlt #clipHeight!
BitBlt removeSelector: #clipHeight!
!methodRemoval: BitBlt #clipHeight:!
BitBlt removeSelector: #clipHeight:!
!methodRemoval: BitBlt #clipWidth!
BitBlt removeSelector: #clipWidth!
!methodRemoval: BitBlt #clipWidth:!
BitBlt removeSelector: #clipWidth:!
!methodRemoval: BitBlt #clipX!
BitBlt removeSelector: #clipX!
!methodRemoval: BitBlt #clipX:!
BitBlt removeSelector: #clipX:!
!methodRemoval: BitBlt #clipY!
BitBlt removeSelector: #clipY!
!methodRemoval: BitBlt #clipY:!
BitBlt removeSelector: #clipY:!
!methodRemoval: BitBlt #color!
BitBlt removeSelector: #color!
!methodRemoval: BitBlt #colorConvertingMap:from:to:keepSubPixelAA:!
BitBlt removeSelector: #colorConvertingMap:from:to:keepSubPixelAA:!
!methodRemoval: BitBlt #copy:from:in:halftoneForm:rule:!
BitBlt removeSelector: #copy:from:in:halftoneForm:rule:!
!methodRemoval: BitBlt #copyBitsFrom:to:at:!
BitBlt removeSelector: #copyBitsFrom:to:at:!
!methodRemoval: BitBlt #copyForm:to:rule:color:!
BitBlt removeSelector: #copyForm:to:rule:color:!
!methodRemoval: BitBlt #destRect!
BitBlt removeSelector: #destRect!
!methodRemoval: BitBlt #displayString:from:to:at:strikeFont:color:kern:!
BitBlt removeSelector: #displayString:from:to:at:strikeFont:color:kern:!
!methodRemoval: BitBlt #eraseBits!
BitBlt removeSelector: #eraseBits!
!methodRemoval: BitBlt #halftoneForm:!
BitBlt removeSelector: #halftoneForm:!
!methodRemoval: BitBlt #installStrikeFont:foregroundColor:!
BitBlt removeSelector: #installStrikeFont:foregroundColor:!
!methodRemoval: BitBlt #paintBits!
BitBlt removeSelector: #paintBits!
!methodRemoval: BitBlt #setHalftoneForm:!
BitBlt removeSelector: #setHalftoneForm:!
!methodRemoval: BitBlt #tallyMap!
BitBlt removeSelector: #tallyMap!
!methodRemoval: BitBlt #tallyMap:!
BitBlt removeSelector: #tallyMap:!
!classRemoval: #PenPointRecorder!
Smalltalk removeClassNamed: #PenPointRecorder!
