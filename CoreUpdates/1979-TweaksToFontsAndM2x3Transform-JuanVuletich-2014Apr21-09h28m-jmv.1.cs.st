'From Cuis 4.2 of 25 July 2013 [latest update: #1978] on 21 April 2014 at 9:31:57.926936 am'!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoAutoHint DoIntegerTransformations '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!AbstractFont methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2014 17:35'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	| char fallbackFont x y |
	fallbackFont _ (AbstractFont familyName: 'DejaVu' aroundPointSize: self pointSize*0.9) emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].! !


!StrikeFont methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2014 16:50'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	aGrafPort
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!GrafPort methodsFor: 'private' stamp: 'jmv 4/17/2014 16:57'!
setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: Color transparent).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			Preferences subPixelRenderFonts and: [ foregroundColor = Color black or: [ 
				Preferences subPixelRenderColorFonts and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= Color black or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:"
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


!MatrixTransform2x3 methodsFor: 'as yet unclassified' stamp: 'jmv 4/12/2014 12:07'!
italics
	"a little shear"
	self a12: self a12  + (self scale*0.2)! !

!MatrixTransform2x3 methodsFor: 'as yet unclassified' stamp: 'jmv 4/6/2014 19:17'!
negateYAxis
	"Swap inneer point Y sign.
	Make y increment up."
	self a12: self a12  negated.
	self a22: self a22  negated.! !

!MatrixTransform2x3 methodsFor: 'as yet unclassified' stamp: 'jmv 4/6/2014 19:19'!
negateYAxisAndAngle
	"Swap inneer point Y sign.
	Make y increment up.
	Also negate angle sign. Make positives angles be counterclockwise.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)"
	self a22: self a22  negated.
	self a21: self a21  negated.! !


!AbstractFont methodsFor: 'accessing' stamp: 'jmv 4/17/2014 18:13'!
baseKern
	"Required to answer a number if #xTable is provided"
	^nil! !


!GrafPort methodsFor: 'private' stamp: 'jmv 4/17/2014 16:58'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	sourceForm _ aStrikeFont glyphs.
	sourceY _ 0.
	height _ aStrikeFont height.
	self setRuleAndMapFor: sourceForm depth foregroundColor: foregroundColor! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/17/2014 16:50'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| p1 font colorToUse |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	colorToUse _ shadowColor ifNil: [ aColor ].
	font on:	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: colorToUse! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:29'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint! !


!MatrixTransform2x3 class methodsFor: 'class initialization' stamp: 'jmv 4/11/2014 09:33'!
initialize
	DoIntegerTransformations _ true.! !

!methodRemoval: MatrixTransform2x3 class #doAutoHinting:!
MatrixTransform2x3 class removeSelector: #doAutoHinting:!
MatrixTransform2x3 initialize!
!methodRemoval: MatrixTransform2x3 #autoHint:!
MatrixTransform2x3 removeSelector: #autoHint:!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
