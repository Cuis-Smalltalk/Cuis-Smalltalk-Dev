'From Cuis 4.1 of 12 December 2012 [latest update: #1629] on 18 March 2013 at 11:45:30 pm'!

!BitBlt methodsFor: 'copying' stamp: 'jmv 3/18/2013 23:12'!
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


!BitBlt methodsFor: 'private' stamp: 'jmv 3/18/2013 23:44'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	| lastSourceDepth targetColor sourceDepth destDepth |
	sourceForm ifNotNil: [ lastSourceDepth _ sourceForm depth ].
	sourceForm _ aStrikeFont glyphs.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	"Ignore any halftone pattern since we use a color map approach here"
	halftoneForm _ nil.
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


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/18/2013 23:38'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	| p1 font kern insideColor |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		color: insideColor
		kern: kern! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/18/2013 23:37'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor kern: kernOrNil
	| font portRect bounds kern insideColor |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft
		strikeFont: font
		color: insideColor
		kern: kern.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/18/2013 23:36'!
drawStringEmbossed: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor
	| font portRect insideColor bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft
			strikeFont: font
			color: topColor
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft + (0@2)
			strikeFont: font
			color: bottomColor
			kern: font baseKern negated ].
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft + (0@1)
		strikeFont: font
		color: insideColor
		kern: font baseKern negated.
	port clipRect: portRect! !


!MorphicScanner methodsFor: 'private' stamp: 'jmv 3/18/2013 22:57'!
setFont 
	foregroundColor _ paragraphColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil: [ destY _ lineY + line baseline - font ascent ]! !

!methodRemoval: BitBlt #displayString:from:to:at:strikeFont:kern:!
BitBlt removeSelector: #displayString:from:to:at:strikeFont:kern:!
