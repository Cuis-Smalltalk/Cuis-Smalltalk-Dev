"
Add services for BitBltCanvas
"
Class {
	#name : 'BitBltCanvasEngine',
	#superclass : 'BitBlt',
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'private' }
BitBltCanvasEngine >> basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont [
	"Answer position to place next glyph"

	| fdestX glyph firstByte maxHeight byteIndex n ascent lineGap deltaXToCenter deltaXForBaseGlyph deltaXForDiacriticalMark baseGlyphTopOffset deltaYForDiacriticalMark |
	deltaXForBaseGlyph := 0.
	baseGlyphTopOffset := 0.
	ascent := aTrueTypeFont ascent.
	lineGap := aTrueTypeFont lineGap.
	destY := aPoint y.
	fdestX := aPoint x.
	maxHeight := 0.
	byteIndex := byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph := aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4.
				firstByte := byte1 ] 
			into: nil.

		"$i followed by a Combining Diacritical Mark is replaced by CodePoint 305 Dotless i, UTF-8 #[196 177]
		(UnicodeString fromUtf8Bytes: #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137	]) edit
		This should also be done for VectorEngineWithPlugin at
			displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:
		"
		(firstByte = 105 and: [ byteIndex < (byteStopIndex-1) ] and: [
			(aByteArray at: byteIndex+1) = 204 or: [ (aByteArray at: byteIndex+1) = 205 and: [ (aByteArray at: byteIndex+2) < 176 ]]])
				ifTrue: [ glyph := aTrueTypeFont glyphAtUtf8Byte1: 196 byte2: 177 byte3: nil byte4: nil ].

		"Center diacriticals marks.
		See building of FormGlyph at
		formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:
		Implement equivalent behavior at 
			displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:
		(Done in Smalltalk VectorEngine, still to be done for VectorEngineWithPlugin)."
		"Tested at various sizes of DejaVu Sans, DejaVu Serif, JetBrains Mono NL, Kiwi Maru Light (except ~ and `), Kurinto Sans, Neo Euler, Source Sans 3
		Test text (at sizes 10, 13, 144):
		(UnicodeString fromUtf8Bytes: #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97	]) edit
		"
		deltaXToCenter := glyph deltaXToCenter.
		deltaXForDiacriticalMark := 0.
		deltaYForDiacriticalMark := 0.
		glyph advanceWidth = 0.0
			ifFalse: [ "Regular glyph"
				deltaXForBaseGlyph := deltaXToCenter.
				baseGlyphTopOffset := glyph topOffset ]
			ifTrue: [ "Diacritical mark"
				deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph.
				"Alternative implementation, perhaps closer to what  is done at VectorEngine >> #displayUtf8:fromByte:toByte:at:trueTypeFont:color:scale:preventMirroring:"
				"
				xtraSpaceForAntiAliasingAndRounding := 3. 'see #formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:'.
				baseBoundsTop := baseGlyphTopOffset-xtraSpaceForAntiAliasingAndRounding.
				boundsBottom := glyph topOffset-xtraSpaceForAntiAliasingAndRounding - (glyph form height-xtraSpaceForAntiAliasingAndRounding).
				boundsBottom := glyph topOffset - glyph form height.
				baseBoundsTop * 0.9 > boundsBottom ifTrue: [
					deltaYForDiacriticalMark :=  baseBoundsTop - boundsBottom ].
				"
				"Only if placed over a glyph with a top offset"
				baseGlyphTopOffset = 0 					ifFalse: [
					deltaYForDiacriticalMark :=  baseGlyphTopOffset - (glyph topOffset * 0.76).
					deltaYForDiacriticalMark := deltaYForDiacriticalMark max: 0 ]].
		destX := (fdestX + glyph leftOffset + deltaXForDiacriticalMark) roundedHAFZ.
		destY := (aPoint y + ascent + lineGap-1 - glyph topOffset - deltaYForDiacriticalMark) roundedHAFZ.
		"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
		aTrueTypeFont isSuperscript
			ifTrue: [destY := destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY := destY + (ascent*0.3)]].
		destY := destY roundedHAFZ.
		sourceForm := glyph form.
		height := sourceForm height.
		width := sourceForm width.
		self copyBits.
		fdestX := fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight := maxHeight max: height.
		byteIndex := byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight).
]

{ #category : 'private' }
BitBltCanvasEngine >> cachedFontColormapFrom1BitTo: destDepth [

	| map dstIndex |
	CachedFontColorMaps 
		ifNil: [CachedFontColorMaps := Array new: 6].

	dstIndex := destDepth highBit.
	(CachedFontColorMaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map := (Color cachedColormapFrom: 1 to: destDepth) copy.
	CachedFontColorMaps at: dstIndex put: map.
	^ map
]

{ #category : 'copying' }
BitBltCanvasEngine >> clippedBy: aRectangle [
	^ self copy clipBy: aRectangle
]

{ #category : 'private' }
BitBltCanvasEngine >> colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix [
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	
	| srcIndex dstIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps 
		ifNil: [ColorConvertingMaps := (1 to: 6) collect: [:i | Array new: 6]].

	srcIndex := sourceDepth highBit.
	sourceDepth > 8 ifTrue: [ srcIndex := keepSubPix ifTrue: [5] ifFalse: [6] ].
	dstIndex := destDepth highBit.
	
	mapsForSource := ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest := mapsForSource at: dstIndex) ifNil: [
		mapsForSourceAndDest := mapsForSource at: dstIndex put: Dictionary new ].
	
	map := mapsForSourceAndDest at: targetColor ifAbsentPut: [
		Color 
			computeColorConvertingMap: targetColor 
			from: sourceDepth 
			to: destDepth 
			keepSubPixelAA: keepSubPix ].

	^ map
]

{ #category : 'text' }
BitBltCanvasEngine >> drawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor [
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	sourceForm := nil. "Set it for each glyph"
	sourceX := 0.
	sourceY := 0.
	height := nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap := colorMap.
	sourceDepth := aTrueTypeFont cachedFormDepth.
	destDepth := destForm depth.
	colorMap := sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDrawUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer
]

{ #category : 'drawing support' }
BitBltCanvasEngine >> fillOval: rect [
	| centerX centerY nextY yBias xBias outer nextOuterX |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	nextY := rect height // 2.
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := rect width.
	destX := rect left.
	self copyBits.

]

{ #category : 'drawing support' }
BitBltCanvasEngine >> fillRect: rect [

	rect hasPositiveExtent ifFalse: [^ self].
	destX := rect left.
	destY := rect top.
	sourceX := 0.
	sourceY := 0.
	width := rect width.
	height := rect height.
	self copyBits
]

{ #category : 'drawing support' }
BitBltCanvasEngine >> frameOval: rect borderWidth: borderWidth [
	| centerX centerY nextY yBias xBias wp outer inner nextOuterX nextInnerX fillAlpha |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	wp := borderWidth asPoint.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	inner := EllipseMidpointTracer new on: (rect insetBy: wp).
	nextY := rect height // 2.
	1 to: (wp y min: nextY) do:[:i|
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		nextInnerX := inner stepInY.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		width := nextOuterX - nextInnerX.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		destX := centerX - nextOuterX.
		destY := centerY + nextY + yBias.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := wp x.
	destX := rect left.
	self copyBits.
	destX := rect right - wp x.
	self copyBits.

]

{ #category : 'drawing support' }
BitBltCanvasEngine >> frameRect: rect borderWidth: borderWidth [
	"Paint a border whose rectangular area is defined by rect. The
	width of the border of each side is borderWidth."

	rect hasPositiveExtent ifFalse: [^ self].

	sourceX := 0.
	sourceY := 0.
	
	"for top and bottom, the following are the same"
	height := borderWidth. 
	width := rect width. 
	destX := rect left.

	"top"
	destY := rect top.
	self copyBits.

	"bottom"
	destY := rect bottom - borderWidth.
	self copyBits.

	"for left & right, the following are the same"
	height := rect height-borderWidth-borderWidth.
	width := borderWidth. 
	destY := rect top+borderWidth.

	"left"
	destX := rect left.
	self copyBits.

	"right"
	destX := rect right - borderWidth.
	self copyBits
]

{ #category : 'drawing support' }
BitBltCanvasEngine >> frameRectBottom: rect height: h [

	destX := rect left + 1.
	destY := rect bottom - 1.
	width := rect width - 2.
	height := 1.
	1 to: h do: [:i |
		self copyBits.
		destX := destX + 1.
		destY := destY - 1.
		width := width - 2].

]

{ #category : 'drawing support' }
BitBltCanvasEngine >> frameRectRight: rect width: w [

	width := 1.
	height := rect height - 1.
	destX := rect right - 1.
	destY := rect top + 1.
	1 to: w do: [:i |
		self copyBits.
		destX := destX - 1.
		destY := destY + 1.
		height := height - 2].

]

{ #category : 'drawing support' }
BitBltCanvasEngine >> frameRectTopLeft: rect borderWidth: borderWidth [
	"Paint the top and left edges of a border whose rectangular area is defined by rect.
	 The width of the border of each side is borderWidth."

	sourceX := 0.
	sourceY := 0.
	
	"top"
	height := borderWidth. 
	width := rect width. 
	destX := rect left.
	destY := rect top.
	self copyBits.

	"left"
	height := rect height. 
	width := borderWidth. 
	destY := rect top.
	destX := rect left.
	self copyBits
]

{ #category : 'drawing support' }
BitBltCanvasEngine >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm := aForm.
	combinationRule := rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBits
]

{ #category : 'private' }
BitBltCanvasEngine >> setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor [

	| targetColor destDepth |
	destDepth := destForm depth.
	halftoneForm := nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap := self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: `Color transparent`).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			(Preferences at: #subPixelRenderFonts) and: [ foregroundColor = `Color black` or: [ 
				(Preferences at: #subPixelRenderColorFonts) and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap := (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences at: #properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See senders"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor := foregroundColor = `Color black` ifFalse: [ foregroundColor ].
				colorMap := self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display."
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap := self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]
]

{ #category : 'drawing support' }
BitBltCanvasEngine >> stencil: stencilForm at: aPoint sourceRect: aRect [
	"Paint using aColor wherever stencilForm has non-zero pixels"
	self sourceForm: stencilForm;
		destOrigin: aPoint;
		sourceRect: aRect.
	self copyBits
]
