'From Cuis 4.1 of 12 December 2012 [latest update: #1635] on 20 March 2013 at 11:32:10 am'!
!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas bitBlt lineY runX foregroundColor lineHeight paragraphColor paraTopLeft ignoreColorChanges '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!BitBlt methodsFor: 'initialize-release' stamp: 'jmv 3/20/2013 11:32'!
initialize

	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	self sourceX: 0; width: 0! !


!GrafPort methodsFor: 'private' stamp: 'jmv 3/20/2013 11:07'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	| targetColor sourceDepth destDepth |
	sourceForm _ aStrikeFont glyphs.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"
	sourceY _ 0.
	height _ aStrikeFont height.

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


!MorphicScanner methodsFor: 'accessing' stamp: 'jmv 3/20/2013 11:30'!
canvas: aCanvas

	canvas _ aCanvas! !

!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY runX foregroundColor lineHeight paragraphColor paraTopLeft ignoreColorChanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!BitBlt reorganize!
('accessing' clipBy: clipByX1:y1:x2:y2: clipRect clipRect: colorMap colorMap: combinationRule: destForm destOrigin: destRect: destX: destX:destY:width:height: destY: fillBitmap: fillColor fillColor: height: sourceForm sourceForm: sourceOrigin: sourceRect: sourceX: sourceY: width:)
('copying' copy:from:in: copy:from:in:fillColor:rule: copyBits copyBitsSimulated copyBitsTranslucent: copyForm:to:rule: copyForm:to:rule:colorMap: copyForm:to:rule:fillColor: copyFrom:in:to: fill:fillColor:rule: pixelAt: pixelAt:put:)
('line drawing' drawFrom:to: drawFrom:to:withFirstPoint: drawLoopX:Y:)
('private' clipRange copyBitsAgain getPluginName oldErase1bitShapeBits oldPaintBits primDisplayString:from:to:map:xTable:kern: roundVariables setDestForm: setDestForm:sourceForm:fillColor:combinationRule:destOrigin:sourceOrigin:extent:clipRect:)
('initialize-release' initialize)
!

