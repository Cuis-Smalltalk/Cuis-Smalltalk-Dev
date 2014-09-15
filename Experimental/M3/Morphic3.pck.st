'From Cuis 4.2 of 25 July 2013 [latest update: #2089] on 2 September 2014 at 9:19:33.558947 am'!
'Description Please enter a description for this package '!
!provides: 'Morphic3' 1 2!
!requires: 'YAXO' 1 nil nil!
!requires: 'PluginMaker' 1 nil nil!
!requires: 'LinearAlgebra' 1 nil nil!
!classDefinition: #StrokeFont category: #'Morphic3-Text'!
AbstractFont subclass: #StrokeFont
	instanceVariableNames: 'name emphasis baseFont derivativeFonts pointSize characterToGlyphMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Text'!
!classDefinition: 'StrokeFont class' category: #'Morphic3-Text'!
StrokeFont class
	instanceVariableNames: ''!

!classDefinition: #SVGError category: #'Morphic3-SVG'!
Error subclass: #SVGError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGError class' category: #'Morphic3-SVG'!
SVGError class
	instanceVariableNames: ''!

!classDefinition: #SVGWarning category: #'Morphic3-SVG'!
Warning subclass: #SVGWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGWarning class' category: #'Morphic3-SVG'!
SVGWarning class
	instanceVariableNames: ''!

!classDefinition: #Morphic3Canvas category: #'Morphic3-Kernel'!
FormCanvas subclass: #Morphic3Canvas
	instanceVariableNames: 'boundsLeft boundsTop boundsRight boundsBottom fillColor borderColor auxAlphas auxEdgeCounts firstX firstY currentX currentY borderWidth computeFill auxDataBitBlt lastDisplayY lastX lastY butLastX butLastY fAlpha fRed fGreen fBlue bAlpha bRed bGreen bBlue formWidth formHeight trajectoryLength strokeDashArray strokeDashArrayOffset alphas alphasIfOpaqueColors lastControl2Point hopsFactor fontForeColor currentClientX currentClientY autoHintText morphAtPixel morphBoundsLeft morphBoundsRight morphBoundsTop morphBoundsBottom'
	classVariableNames: 'AAWidth SubPixDelta'
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'Morphic3Canvas class' category: #'Morphic3-Kernel'!
Morphic3Canvas class
	instanceVariableNames: ''!

!classDefinition: #BoundsFinderCanvas category: #'Morphic3-Kernel'!
Morphic3Canvas subclass: #BoundsFinderCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'BoundsFinderCanvas class' category: #'Morphic3-Kernel'!
BoundsFinderCanvas class
	instanceVariableNames: ''!

!classDefinition: #Morphic3Canvas2 category: #'Morphic3-Kernel'!
Morphic3Canvas subclass: #Morphic3Canvas2
	instanceVariableNames: 'componentAlphas componentAlphasIfOnlyOpaqueColors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'Morphic3Canvas2 class' category: #'Morphic3-Kernel'!
Morphic3Canvas2 class
	instanceVariableNames: ''!

!classDefinition: #M3Exp01Morph category: #'Morphic3-Experiments'!
Morph subclass: #M3Exp01Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Experiments'!
!classDefinition: 'M3Exp01Morph class' category: #'Morphic3-Experiments'!
M3Exp01Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp02Morph category: #'Morphic3-Experiments'!
Morph subclass: #M3Exp02Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Experiments'!
!classDefinition: 'M3Exp02Morph class' category: #'Morphic3-Experiments'!
M3Exp02Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp03Morph category: #'Morphic3-Experiments'!
Morph subclass: #M3Exp03Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Experiments'!
!classDefinition: 'M3Exp03Morph class' category: #'Morphic3-Experiments'!
M3Exp03Morph class
	instanceVariableNames: ''!

!classDefinition: #SVGElementMorph category: #'Morphic3-SVG'!
Morph subclass: #SVGElementMorph
	instanceVariableNames: 'defs opacity strokeWidth strokeColorOrGradient strokeOpacity fillColorOrGradient fillOpacity strokeDashArray strokeDashArrayOffset'
	classVariableNames: 'DefaultedAtributes IgnoredAttributes IgnoredElements IgnoredPrefixes'
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGElementMorph class' category: #'Morphic3-SVG'!
SVGElementMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGCircle category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGCircle
	instanceVariableNames: 'cx cy r'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGCircle class' category: #'Morphic3-SVG shapes'!
SVGCircle class
	instanceVariableNames: ''!

!classDefinition: #SVGEllipse category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGEllipse
	instanceVariableNames: 'cx cy rx ry'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGEllipse class' category: #'Morphic3-SVG shapes'!
SVGEllipse class
	instanceVariableNames: ''!

!classDefinition: #SVGGroupMorph category: #'Morphic3-SVG'!
SVGElementMorph subclass: #SVGGroupMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGGroupMorph class' category: #'Morphic3-SVG'!
SVGGroupMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGMainMorph category: #'Morphic3-SVG'!
SVGGroupMorph subclass: #SVGMainMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGMainMorph class' category: #'Morphic3-SVG'!
SVGMainMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGLine category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGLine
	instanceVariableNames: 'x1 y1 x2 y2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGLine class' category: #'Morphic3-SVG shapes'!
SVGLine class
	instanceVariableNames: ''!

!classDefinition: #SVGPath category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGPath
	instanceVariableNames: 'commands'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGPath class' category: #'Morphic3-SVG shapes'!
SVGPath class
	instanceVariableNames: ''!

!classDefinition: #SVGPolyLine category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGPolyLine
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGPolyLine class' category: #'Morphic3-SVG shapes'!
SVGPolyLine class
	instanceVariableNames: ''!

!classDefinition: #SVGPolygon category: #'Morphic3-SVG shapes'!
SVGPolyLine subclass: #SVGPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGPolygon class' category: #'Morphic3-SVG shapes'!
SVGPolygon class
	instanceVariableNames: ''!

!classDefinition: #SVGRect category: #'Morphic3-SVG shapes'!
SVGElementMorph subclass: #SVGRect
	instanceVariableNames: 'xMin xMax yMin yMax rx ry'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG shapes'!
!classDefinition: 'SVGRect class' category: #'Morphic3-SVG shapes'!
SVGRect class
	instanceVariableNames: ''!

!classDefinition: #Morphic3Gradient category: #'Morphic3-Kernel'!
Object subclass: #Morphic3Gradient
	instanceVariableNames: 'stopFractions stopReds stopGreens stopBlues stopAlphas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'Morphic3Gradient class' category: #'Morphic3-Kernel'!
Morphic3Gradient class
	instanceVariableNames: ''!

!classDefinition: #Morphic3LinearGradient category: #'Morphic3-Kernel'!
Morphic3Gradient subclass: #Morphic3LinearGradient
	instanceVariableNames: 'vectorX1 vectorY1 vectorX2 vectorY2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'Morphic3LinearGradient class' category: #'Morphic3-Kernel'!
Morphic3LinearGradient class
	instanceVariableNames: ''!

!classDefinition: #Morphic3RadialGradient category: #'Morphic3-Kernel'!
Morphic3Gradient subclass: #Morphic3RadialGradient
	instanceVariableNames: 'cx cy rx ry angleCos angleSin'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'Morphic3RadialGradient class' category: #'Morphic3-Kernel'!
Morphic3RadialGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGDefElement category: #'Morphic3-SVG'!
Object subclass: #SVGDefElement
	instanceVariableNames: 'def'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGDefElement class' category: #'Morphic3-SVG'!
SVGDefElement class
	instanceVariableNames: ''!

!classDefinition: #SVGGradient category: #'Morphic3-SVG'!
SVGDefElement subclass: #SVGGradient
	instanceVariableNames: 'stops name inheritFrom'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGGradient class' category: #'Morphic3-SVG'!
SVGGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGLinearGradient category: #'Morphic3-SVG'!
SVGGradient subclass: #SVGLinearGradient
	instanceVariableNames: 'vectorX1 vectorY1 vectorX2 vectorY2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGLinearGradient class' category: #'Morphic3-SVG'!
SVGLinearGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGRadialGradient category: #'Morphic3-SVG'!
SVGGradient subclass: #SVGRadialGradient
	instanceVariableNames: 'cx cy r'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGRadialGradient class' category: #'Morphic3-SVG'!
SVGRadialGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGGradientStop category: #'Morphic3-SVG'!
SVGDefElement subclass: #SVGGradientStop
	instanceVariableNames: 'offset color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGGradientStop class' category: #'Morphic3-SVG'!
SVGGradientStop class
	instanceVariableNames: ''!

!classDefinition: #SVGDefs category: #'Morphic3-SVG'!
Object subclass: #SVGDefs
	instanceVariableNames: 'defs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGDefs class' category: #'Morphic3-SVG'!
SVGDefs class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCommand category: #'Morphic3-SVG path commands'!
Object subclass: #SVGPathCommand
	instanceVariableNames: 'coordinatesAreRelative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathCommand class' category: #'Morphic3-SVG path commands'!
SVGPathCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathArcCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathArcCommand
	instanceVariableNames: 'rs xAxisRotations toPoints doLargerArcs useIncreasingAngless'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathArcCommand class' category: #'Morphic3-SVG path commands'!
SVGPathArcCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCloseCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathCloseCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathCloseCommand class' category: #'Morphic3-SVG path commands'!
SVGPathCloseCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCurveToCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathCurveToCommand
	instanceVariableNames: 'toPoints control1Points control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathCurveToCommand class' category: #'Morphic3-SVG path commands'!
SVGPathCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCurveToSmoothCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathCurveToSmoothCommand
	instanceVariableNames: 'toPoints control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathCurveToSmoothCommand class' category: #'Morphic3-SVG path commands'!
SVGPathCurveToSmoothCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathLineToCommand class' category: #'Morphic3-SVG path commands'!
SVGPathLineToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToHCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToHCommand
	instanceVariableNames: 'xs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathLineToHCommand class' category: #'Morphic3-SVG path commands'!
SVGPathLineToHCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToVCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToVCommand
	instanceVariableNames: 'ys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathLineToVCommand class' category: #'Morphic3-SVG path commands'!
SVGPathLineToVCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathMoveToCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathMoveToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathMoveToCommand class' category: #'Morphic3-SVG path commands'!
SVGPathMoveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathQuadraticCurveToCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathQuadraticCurveToCommand
	instanceVariableNames: 'toPoints controlPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathQuadraticCurveToCommand class' category: #'Morphic3-SVG path commands'!
SVGPathQuadraticCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathQuadraticCurveToSmoothCommand category: #'Morphic3-SVG path commands'!
SVGPathCommand subclass: #SVGPathQuadraticCurveToSmoothCommand
	instanceVariableNames: 'toPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG path commands'!
!classDefinition: 'SVGPathQuadraticCurveToSmoothCommand class' category: #'Morphic3-SVG path commands'!
SVGPathQuadraticCurveToSmoothCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGTransform category: #'Morphic3-SVG'!
Object subclass: #SVGTransform
	instanceVariableNames: 'a'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGTransform class' category: #'Morphic3-SVG'!
SVGTransform class
	instanceVariableNames: ''!

!classDefinition: #Morphic3GeometryTest category: #'Morphic3-Tests'!
TestCase subclass: #Morphic3GeometryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Tests'!
!classDefinition: 'Morphic3GeometryTest class' category: #'Morphic3-Tests'!
Morphic3GeometryTest class
	instanceVariableNames: ''!


!Morphic3Canvas commentStamp: '<historical>' prior: 0!
A canvas specially built for Morphic 3.

Preferences m3Fonts.
(Morphic3Canvas onForm: Display) fullDraw: self runningWorld.
"
Preferences standardFonts
And reopen windows..."



En un morph cualquiera
| c |
[
	Preferences m3Fonts.
	c _ FormCanvas onForm: Display.
	c _ Morphic3Canvas onForm: Display.
	self allOwnersReverseDo: [ :m | c into: m ].
	c into: self.
	self fullDrawOn: c ] ensure:
[ Preferences standardFonts ]!

!SVGElementMorph commentStamp: 'jmv 5/2/2011 09:46' prior: 0!
See class methods in 'examples' category!

!SVGMainMorph commentStamp: '<historical>' prior: 0!
| m |
m _ SVGMainMorph fromFileNamed: 'Ejemplos/Lionx.svg'.
m privateOwner: World.
m fullDrawOn: Display getCanvas.
Display forceToScreen
"m exportAs: 'pp.png'"
!

!Morphic3LinearGradient commentStamp: '<historical>' prior: 0!
To be used by Morphic3Canvas. Vector specified in Display (or target) coordinates. stopFractions are float, [0.0 .. 1.0], stopColorComponents are [0 .. 255]!

!Color methodsFor: '*Morphic3' stamp: 'jmv 7/30/2012 17:42'!
externalizedWith: aMatrixTransform2x3
	^self! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:02'!
ascent
	"Answer the receiver's maximum extent of characters above the baseline."

	^pointSize! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:54'!
baseFont
	^self isBaseFont
		ifTrue: [ self ]
		ifFalse: [ baseFont ]! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:19'!
baseFont: aStrikeFont

	baseFont _ aStrikeFont.
	baseFont ifNotNil: [
		derivativeFonts _ nil ]! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:27'!
characterToGlyphMap
	^characterToGlyphMap ifNil:[characterToGlyphMap _ self createCharacterToGlyphMap].! !

!StrokeFont methodsFor: 'private' stamp: 'jmv 4/15/2014 09:29'!
createCharacterToGlyphMap
	"Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."
	| map |
	map _ Array new: 256.
	0 to: self minAscii - 1 do:[:i| map at: i + 1 put: self maxAscii + 1].
	self minAscii to: self maxAscii do:[:i| map at: i + 1 put: i].
	self maxAscii + 1 to: 255 do:[:i| map at: i + 1 put: self maxAscii + 1].
	^map! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/17/2014 17:44'!
descent
	"Answer the receiver's maximum extent of characters below the baseline."
	"por ahora, entero..."
	^6*self ascent // 10! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:08'!
emphasis
	"Answer the integer code for synthetic bold, italic, underline, and 
	strike-out."
	^emphasis! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:19'!
emphasis: code 
	"Set the integer code for synthetic bold, itallic, underline, and strike-out, 
	where bold=1, italic=2, underlined=4, and struck out=8."

	emphasis _ code! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:10'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom safeCode |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].
	
	derivativeFonts ifNil: [derivativeFonts := Array new: 16].
	derivative _ derivativeFonts at: (safeCode _ code min: derivativeFonts size).
	derivative ifNotNil: [^ derivative].  "Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: safeCode highBit - 1.
	derivedFrom _ self emphasized: safeCode - addedEmphasis.  "Order is Bold, Italic, Underlined"
	derivative _ derivedFrom copy.
	addedEmphasis = 1 ifTrue: [
		derivative name: derivedFrom name , 'B' ].
	addedEmphasis = 2 ifTrue: [
		derivative name: derivedFrom name , 'I' ].
	addedEmphasis = 4 ifTrue: [
		derivative name: derivedFrom name , 'U' ].
	addedEmphasis = 8 ifTrue: [
		derivative name: derivedFrom name , 'X' ].
	derivative baseFont: self.
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:10'!
familyName
"	^'Technical'"
"?"
	^'M3Sans'! !

!StrokeFont methodsFor: 'initialization' stamp: 'jmv 4/15/2014 12:11'!
initializeM3Sans: aPointSize
"Pensar bien el nombre, relacionar con los servicios del canvas (quizas una ivar para el prefijo), etc"
	name _ 'M3Sans ', aPointSize printString.
	emphasis _ 0.
	baseFont _ nil.
	derivativeFonts _ nil.
	pointSize _ aPointSize.
	characterToGlyphMap _ nil! !

!StrokeFont methodsFor: 'testing' stamp: 'jmv 4/15/2014 09:54'!
isBaseFont
emphasis ifNil: [ emphasis _ 0 ].
	^emphasis = 0! !

!StrokeFont methodsFor: 'testing' stamp: 'jmv 4/15/2014 09:58'!
isBold
emphasis ifNil: [ emphasis _ 0 ].
	^emphasis allMask: 1! !

!StrokeFont methodsFor: 'testing' stamp: 'jmv 4/15/2014 10:36'!
isItalic
emphasis ifNil: [ emphasis _ 0 ].
	^emphasis allMask: 2! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:12'!
maxAscii
"	^255"
	^127! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 09:29'!
minAscii
	^32! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:19'!
name: aString
	"Set the receiver's name."

	name _ aString! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 4/15/2014 12:12'!
pointSize
	^pointSize! !

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 7/6/2014 19:57'!
widthOf: aCharacter
"En el array guardo anchos medidos en steps (Cuadraditos en la norma din).
Sumo 2 cuadraditos de separacion entre caracteres"
"
Nota: a la l minuscula solo le sumo un cuadradito de separacion!! (5 2 7)
Nota: El espacio en blanco tiene el mismo ancho que los digitos (monoespaciados!!)
"
| separation |
aCharacter asciiValue-31 < 1 ifTrue:[^0].
aCharacter asciiValue-31 >96 ifTrue:[^0].
separation _ 1.5.
separation _ 2.
^(((
#[
	2 1 3 6 5 9 7 1 3 3 6 6 2 6 1 5 6 6 6 6 6 6 6 6 6 6 1 2 5 5 5 6 
	10 6 6 6 6 6 6 6 6 1 5 6 6 7 6 7 6 7 6 6 6 6 6 9 6 6 6 3 5 3 5 9 	"@, A - Z - _"
	2 5 5 4 5 5 4 5 5 1 2 5 2 7 5 5 5 5 4 5 4 5 5 7 5 5 5 4 1 4 5 5 	"`, a to z -  "
]
 at: aCharacter asciiValue-31) +separation) * pointSize / 10.0)
"la separacion segun la norma DIN es +2. Pero para fonts chiquitos ensanchamos la linea, y se amontonan los trazos. Se puede agregar una opcion por algun lado 'extra kern' o algo asi...
Mhhhh. Creo que me gusta mas con 1.5. Creo que tiene que ser un parametro!!
"
"
self new widthOf: ${
self new widthOf: $_
self new widthOf: $9
self new widthOf: $J
self new widthOf: $O
"

"
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789
"! !

!StrokeFont class methodsFor: 'instance creation' stamp: 'jmv 4/15/2014 11:57'!
m3Sans: pointSize
	^self basicNew initializeM3Sans: pointSize! !

!StrokeFont class methodsFor: 'instance creation' stamp: 'jmv 4/15/2014 11:57'!
new
	^self m3Sans: 10! !

!Morphic3Canvas methodsFor: 'trajectories - aux' stamp: 'jmv 2/15/2013 17:43'!
addCurrentToTrajectory

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| dilatedHalfWidth distanceToEdge alpha t b l r erodedHalfWidth constant1 alphaWord doUpdate redPart greenPart bluePart dySquared dx dxSquared dxp dy pixelIndex rest |
	dilatedHalfWidth _ borderWidth + AAWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
	erodedHalfWidth _ borderWidth - AAWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
	constant1 _ 127.0 / AAWidth.


	"Esto de #lastAndButLast es para que en #addCurrentToTrajectory podamos saber el punto anterior y el proximo. Podria servir para optimizar: si ambos segmentos estan a 180 grados (estamos en una recta y no en una curva), la bolita se vuelve 1d en vez de 2d: es un segmento perpendicular a la trayectoria. De cualquier forma, esto no esta aun implementado.
	Adicionalmente, conocer el anterior (sin hacer falta el proximo) nos permite calcular la longitud de la trayectoria"
lastX ifNotNil: [
	butLastX ifNotNil: [
		trajectoryLength _ trajectoryLength + ((lastX-butLastX)@(lastY-butLastY)) r ].
	
	computeFill ifTrue: [
		self computeCrossCountsForX: lastX y: lastY].

	"ok agregar estos bounds? Probar con trayectorias que se arriman a los extremos del espacio"
	t _ (lastY -dilatedHalfWidth) truncated +1.
	t < 0 ifTrue: [ t _ 0 ].
	b _ (lastY +dilatedHalfWidth) truncated.
	b > (formHeight - 1) ifTrue: [ b _ formHeight - 1 ].
	l _ (lastX -dilatedHalfWidth -SubPixDelta) truncated +1.
	l < 0 ifTrue: [ l _ 0 ].
	r _ (lastX +dilatedHalfWidth +SubPixDelta) truncated.
	r > (formWidth - 1) ifTrue: [ r _ formWidth - 1 ].

	l < boundsLeft ifTrue: [ boundsLeft _ l ].
	r > boundsRight ifTrue: [ boundsRight _ r ].
	t < boundsTop ifTrue: [ boundsTop _ t ].
	b > boundsBottom ifTrue: [ boundsBottom _ b ].

"	self
		overTop: t bottom: b left: l right: r
		do: [ :displayX :displayY :pixelIndex |"
	t to: b do: [ :displayY |
		pixelIndex _ displayY * formWidth +  l.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.


			alphaWord _ auxAlphas at: pixelIndex.
			alphaWord = 16r7F7F7F ifFalse: [
				doUpdate _ false.
				dy _ displayY - lastY.
				dySquared _ dy * dy.
				dx _ displayX - lastX.
				"Red"
				dxp _ dx - SubPixDelta.
				dxSquared _ dxp * dxp.
				" 'octogonal' distance!! Makes circles (especially with wide border) horrible!!"
				"distanceToEdge _ (dx abs max: dy abs) max: 0.7 * (dx abs + dy abs)."
				distanceToEdge _ (dxSquared + dySquared) sqrt.
				distanceToEdge < dilatedHalfWidth
					ifFalse: [ redPart _ alphaWord bitAnd: 16r7F0000 ]
					ifTrue: [
						doUpdate _ true.
						distanceToEdge <= erodedHalfWidth
							ifTrue: [ redPart _ 16r7F0000 ]
							ifFalse: [
								redPart _ alphaWord bitAnd: 16r7F0000.
								alpha _ (dilatedHalfWidth - distanceToEdge * constant1) rounded.
								alpha _ alpha bitShift: 16.
								alpha > redPart ifTrue: [
									redPart _ alpha]]].
				"Green"
				dxSquared _ dx * dx.
				distanceToEdge _ (dxSquared + dySquared) sqrt.
				distanceToEdge < dilatedHalfWidth
					ifFalse: [ greenPart _ alphaWord bitAnd: 16r7F00 ]
					ifTrue: [
						doUpdate _ true.
						distanceToEdge <= erodedHalfWidth
							ifTrue: [ greenPart _ 16r7F00 ]
							ifFalse: [
								greenPart _ alphaWord bitAnd: 16r7F00.
								alpha _ (dilatedHalfWidth - distanceToEdge * constant1) rounded.
								alpha _ alpha bitShift: 8.
								alpha > greenPart ifTrue: [
									greenPart _ alpha]]].
				"Blue"
				dxp _ dx + SubPixDelta.
				dxSquared _ dxp * dxp.
				distanceToEdge _ (dxSquared + dySquared) sqrt.
				distanceToEdge < dilatedHalfWidth
					ifFalse: [ bluePart _ alphaWord bitAnd: 16r7F ]
					ifTrue: [
						doUpdate _ true.
						distanceToEdge <= erodedHalfWidth
							ifTrue: [ bluePart _ 16r7F ]
							ifFalse: [
								bluePart _ alphaWord bitAnd: 16r7F.
								alpha _ (dilatedHalfWidth - distanceToEdge * constant1) rounded.
								alpha > bluePart ifTrue: [
									bluePart _ alpha]]].
				doUpdate ifTrue: [
					(strokeDashArray isNil  or: [
						rest _ trajectoryLength + strokeDashArrayOffset \\ strokeDashArray last.
						(strokeDashArray findLast: [ :each | rest - each > 0 ]) even ]) ifTrue: [
							auxAlphas at: pixelIndex put: ((redPart bitOr: greenPart) bitOr: bluePart)]]
				]]]
].
butLastX _ lastX.
butLastY _ lastY.
lastX _ currentX.
lastY _ currentY! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:21'!
blendAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
"La float es ligeramente mas lenta, pero lo importante es que permite blendear sobre fondos con transparencia... Eso es muy bueno!! (p.ej. para clinton)"

self blendFloatAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
alphasIfOpaqueColors ifNotNil: [
	self blendFloatAlphaIfOpaqueColorsAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside ].

"
self blendNoBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
"! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 7/8/2010 18:33'!
blendFloatAlphaIfOpaqueColorsAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Diferencia con la implementacion en super:
	tenemos alpha channel para cada componente de color, en componentAlphas"
	| resultWord f1resultRA f1resultGA f1resultBA alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside backgroundAlphasWord f1backgroundColorRA f1backgroundColorGA f1backgroundColorBA resultColorRA resultColorGA resultColorBA hh |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.

	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	"ATENCION. Esto SOLO se puede hacer si fillcolor = strokecolor.
	Si NO fueran iguales, entonces pedir mas ancho
	ver #experLinesAndShapes3 "
"	borderWidth = 0.0 ifTrue: ["
	borderWidth < AAWidth ifTrue: [
		hh _ borderWidth / AAWidth * 64 + 63.
		hh _ hh * 2.
		isxRedInside ifTrue: [ redAlpha _ hh-redAlpha min: 127. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ hh-greenAlpha min: 127. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ hh-blueAlpha min: 127. isBlueInside _ false ].].

	backgroundAlphasWord _ alphasIfOpaqueColors at: pixelIndex.
	f1backgroundColorRA _ ((backgroundAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	f1backgroundColorGA _ ((backgroundAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	f1backgroundColorBA _ (backgroundAlphasWord bitAnd: 16rFF) / 255.0.

	isRedInside
		ifTrue: [ f1resultRA _ 1.0 ]
		ifFalse: [ f1resultRA _ 1.0 - ((1.0 - (redAlpha/127.0)) * (1.0  - f1backgroundColorRA)) ].

	isGreenInside
		ifTrue: [ f1resultGA _ 1.0 ]
		ifFalse: [ f1resultGA _ 1.0 - ((1.0 - (greenAlpha/127.0)) * (1.0  - f1backgroundColorGA)) ].

	isBlueInside
		ifTrue: [ f1resultBA _ 1.0 ]
		ifFalse: [ f1resultBA _ 1.0 - ((1.0 - (blueAlpha/127.0)) * (1.0  - f1backgroundColorBA)) ].

	resultColorRA _ (f1resultRA *255.0) rounded bitAnd: 16rFF.
	resultColorGA _ (f1resultGA *255.0) rounded bitAnd: 16rFF.
	resultColorBA _ (f1resultBA *255.0) rounded bitAnd: 16rFF.
	resultWord _ (resultColorRA bitShift: 16) +(resultColorGA bitShift: 8) + resultColorBA.
	alphasIfOpaqueColors at: pixelIndex put: resultWord! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 7/8/2010 18:32'!
blendFloatAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Diferencia con la implementacion en super:
	tenemos alpha channel para cada componente de color, en componentAlphas"
	| backgroundWord f1backgroundColorR f1backgroundColorG f1backgroundColorB resultColorR resultColorG resultColorB resultWord bits f1borderFillAbyBorderA f1borderFillUbyFillA f1borderBackgroundAbyBorderA f1foregroundA f1resultRA f1resultGA f1resultBA f1foregroundAByR f1resultR f1foregroundAByG f1resultG f1foregroundAByB f1resultB alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside f1fillComponent f1borderComponent f1borderAlpha f1fillAlpha backgroundAlphasWord f1backgroundColorRA f1backgroundColorGA f1backgroundColorBA resultColorRA resultColorGA resultColorBA subpixX formResultColorA hh |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	"ATENCION. Esto SOLO se puede hacer si fillcolor = strokecolor.
	Si NO fueran iguales, entonces pedir mas ancho
	ver #experLinesAndShapes3 "
"	borderWidth = 0.0 ifTrue: ["
	borderWidth < AAWidth ifTrue: [
		hh _ borderWidth / AAWidth * 64 + 63.
		hh _ hh * 2.
		isxRedInside ifTrue: [ redAlpha _ hh-redAlpha min: 127. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ hh-greenAlpha min: 127. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ hh-blueAlpha min: 127. isBlueInside _ false ].].

	form depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			greenAlpha > 63 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"
				form pixelValueAt: (pixelIndex \\ formWidth) @(pixelIndex // formWidth) put: 1
				"form
					pixelValueAt: pixelIndex >> 5
					put: 1"	"Color black pixelValueForDepth: 1" ].
			^ self].

	bits _ form bits.
	backgroundWord _ bits at: pixelIndex.
	f1backgroundColorR _ ((backgroundWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	f1backgroundColorG _ ((backgroundWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	f1backgroundColorB _ (backgroundWord bitAnd: 16rFF) / 255.0.

	"Take target surface alphas either from 32bpp form or from alphas (alphas per component)"
	self flag: #revisarM3.
	"Esto ES CARO si no me importa el alpha del destino. La version entera vieja lo evitaba, simplificaba cuentas y evitaba una division."
	alphas
		ifNil: [
			f1backgroundColorGA _ ((backgroundWord bitShift: -24) bitAnd: 16rFF) / 255.0.
			f1backgroundColorRA _ f1backgroundColorGA.
			f1backgroundColorBA _ f1backgroundColorGA ]
		ifNotNil: [
			backgroundAlphasWord _ alphas at: pixelIndex.
			f1backgroundColorRA _ ((backgroundAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
			f1backgroundColorGA _ ((backgroundAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
			f1backgroundColorBA _ (backgroundAlphasWord bitAnd: 16rFF) / 255.0 ].

	subpixX _ x-SubPixDelta.
	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isRedInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			f1borderFillAbyBorderA _ (redAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- redAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ redAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultRA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorRA)).
	f1resultRA = 0.0
		ifTrue: [ f1resultR _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bRed ifNil: [borderColor redAtX: subpixX y: y]) / 255.0.
			isRedInside
				ifTrue: [
					f1fillComponent _ (fRed ifNil: [fillColor redAtX: subpixX y: y]) / 255.0.
					f1foregroundAByR _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByR _ f1borderBackgroundAbyBorderA * f1borderComponent ].
			f1resultR _( f1foregroundAByR  + (f1backgroundColorRA * f1backgroundColorR * (1.0 - f1foregroundA))) / f1resultRA ].

	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: x y: y]) / 255.0.
	isGreenInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: x y: y]) / 255.0.
			f1borderFillAbyBorderA _ (greenAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- greenAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ greenAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultGA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorGA)).
	f1resultGA = 0.0
		ifTrue: [ f1resultG _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bGreen ifNil: [borderColor greenAtX: x y: y])/ 255.0.
			isGreenInside
				ifTrue: [
					f1fillComponent _ (fGreen ifNil: [fillColor greenAtX: x y: y]) / 255.0.
					f1foregroundAByG _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByG _ f1borderBackgroundAbyBorderA * f1borderComponent ].
		f1resultG _ (f1foregroundAByG + (f1backgroundColorGA * f1backgroundColorG * (1.0 - f1foregroundA))) / f1resultGA ].

	subpixX _ x+SubPixDelta.
	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isBlueInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			f1borderFillAbyBorderA _ (blueAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- blueAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ blueAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultBA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorBA)).
	f1resultBA = 0.0
		ifTrue: [ f1resultB _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bBlue ifNil: [borderColor blueAtX: subpixX y: y])/ 255.0.
			isBlueInside
				ifTrue: [
					f1fillComponent _ (fBlue ifNil: [fillColor blueAtX: subpixX y: y]) / 255.0.
					f1foregroundAByB _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByB _ f1borderBackgroundAbyBorderA * f1borderComponent ].
			f1resultB _ (f1foregroundAByB + (f1backgroundColorBA * f1backgroundColorB * (1.0 - f1foregroundA))) / f1resultBA ].

	"Store result alphas either in 32bpp form or in alphas (alphas per component)"
	alphas
		ifNil: [
			formResultColorA _ (f1resultGA *255.0) rounded bitAnd: 16rFF ]
		ifNotNil: [
			formResultColorA _ 255.
			resultColorRA _ (f1resultRA *255.0) rounded bitAnd: 16rFF.
			resultColorGA _ (f1resultGA *255.0) rounded bitAnd: 16rFF.
			resultColorBA _ (f1resultBA *255.0) rounded bitAnd: 16rFF.
			resultWord _ (resultColorRA bitShift: 16) +(resultColorGA bitShift: 8) + resultColorBA.
			alphas at: pixelIndex put: resultWord ].

	resultColorR _ (f1resultR *255.0) rounded bitAnd: 16rFF.
	resultColorG _ (f1resultG *255.0) rounded bitAnd: 16rFF.
	resultColorB _ (f1resultB *255.0) rounded bitAnd: 16rFF.
	resultWord _ (formResultColorA bitShift: 24) bitOr: (resultColorR bitShift: 16) + (resultColorG bitShift: 8) + resultColorB.
	bits at: pixelIndex put: resultWord.! !

!Morphic3Canvas methodsFor: 'private' stamp: 'jmv 2/15/2013 17:44'!
blendOver: aColor

	| gAlpha alphasWord word red green blue rAlpha blueAlpha backR backG backB pixelIndex |
	backR _ aColor red.
	backG _ aColor green.
	backB _ aColor blue.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			alphasWord _ alphas at: pixelIndex.
			rAlpha _ ((alphasWord bitAnd: 16rFF0000) >> 16) / 255.0.
			gAlpha _ ((alphasWord bitAnd: 16rFF00) >> 8) / 255.0.
			blueAlpha _ (alphasWord bitAnd: 16rFF) / 255.0.

			word _ form bits at: pixelIndex.
			red _ ((word bitAnd: 16rFF0000) >> 16) / 255.0.
			green _ ((word bitAnd: 16rFF00) >> 8) / 255.0.
			blue _ (word bitAnd: 16rFF) / 255.0.
					
			red _ rAlpha * red + (1.0-rAlpha * backR).
			green _ gAlpha * green + (1.0-gAlpha * backG).
			blue _ blueAlpha * blue + (1.0-blueAlpha * backB).

			red _ (red * 255) rounded bitAnd: 16rFF.
			green _ (green * 255) rounded bitAnd: 16rFF.
			blue _ (blue * 255) rounded bitAnd: 16rFF.

			word _ (((255 bitShift: 24) bitOr: (red bitShift: 16)) bitOr: (green bitShift: 8)) bitOr: blue.
					
			form bits at: pixelIndex put: word ]]! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 2/15/2013 17:44'!
blendTrajectoryAndFill

	| redCrosses greenCrosses blueCrosses isRedInside isGreenInside isBlueInside edgeCountWord redEdgeCount greenEdgeCount blueEdgeCount zlastDisplayY pixelIndex |

	zlastDisplayY _ nil.

"	self
		overTop: boundsTop bottom: boundsBottom left: boundsLeft right: boundsRight
		do: [ :displayX :displayY :pixelIndex |"
	boundsTop to: boundsBottom do: [ :displayY |
		pixelIndex _ displayY * formWidth +  boundsLeft.
		boundsLeft to: boundsRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgeCountWord _ auxEdgeCounts at: pixelIndex.
			redEdgeCount _ (edgeCountWord bitShift: -16) bitAnd: 16rFF.
			greenEdgeCount _ (edgeCountWord bitShift: -8) bitAnd: 16rFF.
			blueEdgeCount _ edgeCountWord bitAnd: 16rFF.

			"Not needed in C or asm, use signed char, i.e. a type where 16rFF means -1 and not 255"
			redEdgeCount > 127 ifTrue: [ redEdgeCount _ redEdgeCount - 256 ].
			greenEdgeCount > 127 ifTrue: [ greenEdgeCount _ greenEdgeCount - 256 ].
			blueEdgeCount > 127 ifTrue: [ blueEdgeCount _ blueEdgeCount - 256 ].

			"feo..."
			displayY = zlastDisplayY ifFalse: [
				zlastDisplayY _ displayY.
				redCrosses _ 0.
				greenCrosses _ 0.
				blueCrosses _ 0 ].

			redCrosses _ redCrosses + redEdgeCount.
			greenCrosses _ greenCrosses + greenEdgeCount.
			blueCrosses _ blueCrosses + blueEdgeCount.
			isRedInside _ redCrosses ~= 0.
			isGreenInside _ greenCrosses ~= 0.
			isBlueInside _ blueCrosses ~= 0.
			self
				blendAtX: displayX y: displayY pixelIndex: pixelIndex
				redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
			]].
	auxDataBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1+1 height: boundsBottom-boundsTop+1.
	auxDataBitBlt destForm bits: auxAlphas.
	auxDataBitBlt copyBits.
	auxDataBitBlt destForm bits: auxEdgeCounts.
	auxDataBitBlt copyBits! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:42'!
cDx: dx dy: dy
	"Curve comes from an horizontal segment, and is followed by a vertical segment"
	"Does not autoHint y"

	self
		curveToX: currentClientX+dx y: currentClientY+dy
		control1X: currentClientX+(dx*0.5) y: currentClientY 
		control2X: currentClientX+dx y: currentClientY+(dy*0.5).
	currentClientX _ currentClientX+dx.
	currentClientY _ currentClientY+dy! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:42'!
cDy: dy dx: dx
	"Curve comes from a vertical segment, and is followed by an horizontal segment"
	"Does not autoHint y"
	self
		curveToX: currentClientX+dx y: currentClientY+dy
		control1X: currentClientX y: currentClientY+(dy*0.5)
		control2X: currentClientX+(dx*0.5) y: currentClientY+dy.
	currentClientX _ currentClientX+dx.
	currentClientY _ currentClientY+dy! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:32'!
cdl: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX - r.
	y3 _ currentClientY - r.
	x1 _ currentClientX.
	y1 _ currentClientY - hr.
	x2 _ currentClientX - hr.
	y2 _ currentClientY - r.
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:32'!
cdr: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX + r.
	y3 _ currentClientY - r.
	x1 _ currentClientX.
	y1 _ currentClientY - hr.
	x2 _ currentClientX + hr.
	y2 _ currentClientY - r.
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.	
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:32'!
cdru: r
	"juntar las 2 curvas en una, y hintear los extremos. No se hintea en el medio"

	| p3 p1 p2 displayY1 displayY2 displayY3 dr cr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	p3 _ currentTransformation externalizePosition: currentClientX+dr@currentClientY.
	p1 _ currentTransformation externalizePosition: (currentClientX )@(currentClientY-cr).
	p2 _ currentTransformation externalizePosition: (currentClientX+dr)@(currentClientY-cr).
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ currentClientX +dr! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:32'!
cld: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX - r.
	y3 _ currentClientY - r.
	x1 _ currentClientX - hr.
	y1 _ currentClientY.
	x2 _ currentClientX - r.
	y2 _ currentClientY - hr.	
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:32'!
cldr: r
	"juntar las 2 curvas en una, y hintear los extremos. No se hintea en el medio, permit $a chiquitas bonitas"

	| p3 p1 p2 displayY1 displayY2 displayY3  cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	p3 _ currentTransformation externalizePosition: currentClientX@(currentClientY-dr).
	p1 _ currentTransformation externalizePosition: (currentClientX-cr)@currentClientY.
	p2 _ currentTransformation externalizePosition: (currentClientX-cr)@(currentClientY-dr).
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY - 3! !

!Morphic3Canvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 21:35'!
clippingRectForCurrentMorph


	^morphBoundsLeft @ morphBoundsTop corner: morphBoundsRight @ morphBoundsBottom! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 18:49'!
closeTrajectory

	self displayCoordlineToX: firstX y: firstY! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:33'!
clu: r
	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |

	hr _ 0.5 * r.
	x3 _ currentClientX - r.
	y3 _ currentClientY + r.
	x1 _ currentClientX - hr.
	y1 _ currentClientY.
	x2 _ currentClientX - r.
	y2 _ currentClientY + hr.	
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:33'!
clur: r
	"juntar las 2 curvas en una, y hintear los extremos. No se hintea en el medio, permit $a chiquitas bonitas"

	| p3 p1 p2 displayY1 displayY2 displayY3 cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	p3 _ currentTransformation externalizePosition: currentClientX@(currentClientY+dr).
	p1 _ currentTransformation externalizePosition: (currentClientX-cr )@currentClientY.
	p2 _ currentTransformation externalizePosition: (currentClientX-cr)@(currentClientY+dr).
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY + 3! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 2/15/2013 17:45'!
computeCrossCountsForX: x y: y
	| thisDisplayY yCrossed piBase redPixelIndex greenPixelIndex bluePixelIndex redIncrement greenIncrement blueIncrement countWord redCount greenCount blueCount rest redOffset greenOffest blueOffset |

	"Atencion!! hacerlo SOLO si hay fill!! si no hay fill, ni perder tiempo en la inicializacion, etc"
	thisDisplayY _ y truncated.
	lastDisplayY ifNil: [ 
		lastDisplayY _ thisDisplayY.
		^self ].

	(x + SubPixDelta) > (formWidth - 1) ifTrue: [
	"It would be more correct to do
	(x - SubPixDelta) > (formWidth - 1 - origin x) ifTrue: [
	and later consider adding for the subpixels that don't fall off the form,
	but the difference is almost nil."
		^self ].	

	thisDisplayY = lastDisplayY ifFalse: [
		thisDisplayY > lastDisplayY ifTrue: [
			yCrossed _ thisDisplayY.
			redIncrement _ 16r010000.
			greenIncrement _ 16r0100.
			blueIncrement _ 16r01 ]
		ifFalse: [
			yCrossed _ lastDisplayY.
			redIncrement _ 16rFF0000.
			greenIncrement _ 16rFF00.
			blueIncrement _ 16rFF ].
		(yCrossed >= 0 and: [ yCrossed <= (formHeight - 1) ]) ifTrue: [
			piBase _ yCrossed * formWidth + 1.
			redOffset _ (x+SubPixDelta) truncated + 1 max: 0.
			greenOffest _  x truncated + 1 max: 0.
			blueOffset _ (x-SubPixDelta) truncated + 1 max: 0.
			redPixelIndex _ piBase + redOffset.
			greenPixelIndex _ piBase + greenOffest.
			bluePixelIndex _ piBase + blueOffset.

			"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
			redPixelIndex = bluePixelIndex
				ifTrue: [						"First case: RGB in the same word"
					countWord _ auxEdgeCounts at: redPixelIndex.
					redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
					greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
					blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
					countWord _ (redCount bitOr: greenCount) bitOr: blueCount.
					auxEdgeCounts at: redPixelIndex put: countWord.
				]
				ifFalse: [
					redPixelIndex = greenPixelIndex
						ifTrue: [				"Second case: RG in one word, B in following"
							countWord _ auxEdgeCounts at: redPixelIndex.
							redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
							greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
							rest _ countWord  bitAnd: 16rFF.
							countWord _ (redCount bitOr: greenCount) bitOr: rest.
							auxEdgeCounts at: redPixelIndex put: countWord.
					
							countWord _ auxEdgeCounts at: bluePixelIndex.
							rest _ countWord bitAnd: 16rFFFF00.
							blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
							countWord _ rest bitOr: blueCount.
							auxEdgeCounts at: bluePixelIndex put: countWord.
						]
						ifFalse: [			"Third case: R in one word, GB in the following"
							countWord _ auxEdgeCounts at: redPixelIndex.
							redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
							rest _ countWord bitAnd: 16rFFFF.
							countWord _ redCount bitOr: rest.
							auxEdgeCounts at: redPixelIndex put: countWord.

							countWord _ auxEdgeCounts at: bluePixelIndex.
							rest _ countWord bitAnd: 16rFF0000.
							greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
							blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
							countWord _ (rest bitOr: greenCount) bitOr: blueCount.
							auxEdgeCounts at: bluePixelIndex put: countWord.
						]
				].
		].
		lastDisplayY _ thisDisplayY ]! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:33'!
crd: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX + r.
	y3 _ currentClientY - r.
	x1 _ currentClientX + hr.
	y1 _ currentClientY.
	x2 _ currentClientX + r.
	y2 _ currentClientY - hr.
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:33'!
crdl: r
	"juntar las 2 curvas en una, y hintear los extremos. No se hintea en el medio, permit $a chiquitas bonitas"

	| p3 p1 p2 displayY1 displayY2 displayY3 cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	p3 _ currentTransformation externalizePosition: currentClientX@(currentClientY-dr).
	p1 _ currentTransformation externalizePosition: (currentClientX+cr)@currentClientY.
	p2 _ currentTransformation externalizePosition: (currentClientX+cr)@(currentClientY-dr).
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY - dr! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:33'!
cru: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX + r.
	y3 _ currentClientY + r.
	x1 _ currentClientX + hr.
	y1 _ currentClientY.
	x2 _ currentClientX + r.
	y2 _ currentClientY + hr.	
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 19:11'!
cubicBezier: points borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	| displayPoints bw fc bc sda sdao |
	displayPoints _ points collect: [ :p |
		currentTransformation externalizePosition: p ].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self displayCoordcubicBezier: displayPoints borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:34'!
cul: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX - r.
	y3 _ currentClientY + r.
	x1 _ currentClientX.
	y1 _ currentClientY + hr.
	x2 _ currentClientX - hr.
	y2 _ currentClientY + r.
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:34'!
cur: r

	| p3 p1 p2 x3 y3 x1 x2 y1 y2 displayY1 displayY2 displayY3 hr |
	hr _ 0.5 * r.
	x3 _ currentClientX + r.
	y3 _ currentClientY + r.
	x1 _ currentClientX.
	y1 _ currentClientY + hr.
	x2 _ currentClientX + hr.
	y2 _ currentClientY + r.
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.	
	currentClientX _ x3.
	currentClientY _ y3! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:34'!
curd: r
	"obviamente solo hintear para tamanios chicos, y con angulo cero..."
	"juntar las 2 curvas en una, y hintear los extremos. No se hintea en el medio"

	| p3 p1 p2 displayY1 displayY2 displayY3 cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	p3 _ currentTransformation externalizePosition: currentClientX+dr@currentClientY.
	p1 _ currentTransformation externalizePosition: (currentClientX )@(currentClientY+cr).
	p2 _ currentTransformation externalizePosition: (currentClientX+dr)@(currentClientY+cr).
	displayY3 _ p3 y.
	displayY1 _ p1 y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY3 _ displayY3 rounded.
		displayY1 _ displayY1 rounded.
		displayY2 _ displayY2 rounded ].
	self displayCoordcurveToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ currentClientX +dr! !

!Morphic3Canvas methodsFor: 'accessing' stamp: 'jmv 6/11/2010 14:31'!
currentPoint

	currentX ifNil: [ ^nil ].
	^currentX@currentY! !

!Morphic3Canvas methodsFor: 'categorizar' stamp: 'jmv 11/8/2012 23:17'!
curveToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	| p3 p1 p2 |
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	self displayCoordcurveToX: p3 x y: p3 y control1X: p1 x y: p1 y control2X: p2 x y: p2 y! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:43'!
d: dy

	| newY p2 displayY2 |
	newY _ currentClientY - dy.
	p2 _ currentTransformation externalizePosition: currentClientX@newY.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	self displayCoordlineToX: p2 x y: displayY2.
	currentClientY _ newY! !

!Morphic3Canvas methodsFor: 'trajectories - paths - display coordinates' stamp: 'jmv 11/8/2012 18:50'!
displayCoordarcCenterX: mcx centerY: mcy radiusX: rx radiusY: ry rotationAngle: theta start: startAngle sweep: sweepAngle

	| center cx cy d hops angle thetaCos thetaSin xp yp x0 y0 |
	center _ mcx@mcy.
	cx _ center x.
	cy _ center y.
	
	"Por supuesto probar tambien rx no igual a ry, y uno (o ambos) menor que el ancho de trazo..."
	
	"Ojo. Si es una elipse muy chiquita, tomo demasiados steps... Todo esto esta muy lejos de lo optimo"
	"OJO!! si es una fraccion de la elipse, usar menos hops!!"
	thetaCos _ theta cos.
	thetaSin _ theta sin.
	angle _ startAngle.
	xp _ angle cos * rx.
	yp _ angle sin * ry.
	x0 _ thetaCos * xp - (thetaSin * yp) + cx.
	y0 _ thetaSin * xp + (thetaCos * yp) + cy.
	firstX
		ifNotNil: [ self displayCoordlineToX: x0 y: y0 ]
		ifNil: [
			firstX _ x0.
			firstY _ y0.
			currentX _ x0.
			currentY _ y0.	
			trajectoryLength _ 0.0.
			self addCurrentToTrajectory ].
	hops _ (rx+ry * 4 "?" * sweepAngle abs / (2*Float pi)) truncated * hopsFactor + 1.
	d _ sweepAngle / hops.
	hops timesRepeat: [
		angle _ angle + d.
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		currentX _ thetaCos * xp - (thetaSin * yp) + cx.
		currentY _ thetaSin * xp + (thetaCos * yp) + cy.
		self addCurrentToTrajectory ].

	lastControl2Point _ nil! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 19:04'!
displayCoordarcToX: x2 y: y2 radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
	"No digo si esta en el morph o en la pantalla... ver eso despues"
	"Calcular: cx, cy, rx, ry, phi, theta1, deltaTheta"
	| x1 y1 cosPhi sinPhi x12hd y12hd x1p y1p rxSq rySq x1pSq y1pSq bigSqR bigSqRNumerator bigSqRDenominator sign cxp cyp cx cy theta1 theta2 deltaTheta scaleSq scale newRx newRy |
	x1 _ currentX.
	y1 _ currentY.
	"Handle out of range parameters as requested by the w3 implementation notes"
	(x2 = x1 and: [ y2 = y1 ]) ifTrue: [ ^self ].
	(rx = 0.0 or: [ ry = 0.0 ]) ifTrue: [ ^self displayCoordlineToX: x2 y: y2 ].
	rx < 0 ifTrue: [
		^self displayCoordarcToX: x2 y: y2 radiusX: 0.0 -rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].
	ry < 0 ifTrue: [
		^self displayCoordarcToX: x2 y: y2 radiusX: rx radiusY: 0.0-ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].
	
	cosPhi _ phi cos.
	sinPhi _ phi sin.
	"step 1 in http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes"
	x12hd _ (x1-x2) * 0.5.
	y12hd _ (y1-y2) * 0.5.
	x1p _ (cosPhi * x12hd) + (sinPhi * y12hd).
	y1p _ (cosPhi * y12hd) - (sinPhi * x12hd).

	"step 2"
	rxSq _ rx * rx.
	rySq _ ry * ry.
	x1pSq _ x1p * x1p.
	y1pSq _ y1p * y1p.
	"ensure radii are large enough"
	scaleSq _ (x1pSq / rxSq) + (y1pSq / rySq).
	scaleSq > 1 ifTrue: [
		scale _ scaleSq sqrt.
		newRx _ scale * rx.
		newRy _ scale * ry.
		(newRx = rx or: [ newRy = ry ]) ifTrue: [		"This happens when scaleSq is so close to 1, that the multiplication rounds to the same numbers"
			newRx _ scaleSq * rx.
			newRy _ scaleSq * ry ].
		^self displayCoordarcToX: x2 y: y2 radiusX: newRx radiusY: newRy angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
		].
	bigSqRNumerator _ (rxSq * rySq) - (rxSq * y1pSq)-(rySq * x1pSq).
	bigSqRDenominator _ (rxSq * y1pSq) + (rySq * x1pSq).
	sign _ doLargerArc = useIncreasingAngles ifTrue: [ -1 ] ifFalse: [ 1 ].
	bigSqR _ (bigSqRNumerator / bigSqRDenominator max: 0.0) sqrt * sign.		"This only can be < 0.0 because of rounding errors. Take 0.0 in such cases."
	cxp _ bigSqR * rx * y1p / ry.
	cyp _ 0.0 - (bigSqR * ry * x1p / rx).

	"step 3"
	cx _ (cosPhi * cxp) - (sinPhi * cyp) + ((x1+x2) * 0.5).
	cy _ (sinPhi * cxp) + (cosPhi * cyp) + ((y1+y2) * 0.5).
	
"
| u v a |
Transcript clear.
u _ 1@4. v _ 7@2.
a _ ((u dotProduct: v) / (u r * v r) ) arcCos.
a _ a * (((u x * v y) - (u y * v x)) sign).
a print.
(v theta - u theta) print.
Ojo al primitivizar. Para ellos, el theta va entre -pi y pi. Para Squeak va entre cero y 2pi...
"

	"step 4"
	"Ojo signo del angulo y del eje y!!!!!!!!!!"
	theta1 _ (((x1p - cxp) / rx) @ ((y1p - cyp) / ry)) theta.
	theta2 _ (((0 - x1p - cxp) / rx) @ (( 0 -y1p - cyp) / ry)) theta.
	deltaTheta _ theta2 - theta1.
	useIncreasingAngles = (deltaTheta > 0) ifFalse: [
		deltaTheta > 0
			ifTrue: [ deltaTheta _ deltaTheta - Float pi - Float pi ]
			ifFalse: [ deltaTheta _ deltaTheta + Float pi + Float pi ]
		].
	"Y aca calcular como dar vuelta deltaTheta, ver."


	"Finalmente, estudiar la correccion de valores fuera de rango"
	

	self displayCoordarcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: phi start: theta1 sweep:  deltaTheta! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 19:02'!
displayCoordbasicLineToX: x2 y: y2

	| dx dy hops incrementX incrementY |
	dx _ x2 - currentX.
	dy _ y2 - currentY.
	hops _ (dx abs max: dy abs) truncated * hopsFactor + 1.

	incrementX _ dx *1.0 / hops.
	incrementY _ dy *1.0 / hops.
	hops timesRepeat: [
		self addCurrentToTrajectory.
		currentX _ currentX + incrementX.
		currentY _ currentY + incrementY ].
	currentX _ x2.
	currentY _ y2.
	self addCurrentToTrajectory.

	lastControl2Point _ nil! !

!Morphic3Canvas methodsFor: 'trajectories - display coordinates' stamp: 'jmv 11/8/2012 18:58'!
displayCoordcubicBezier: points borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	bc ifNil: [ fc ifNil: [ ^self ]].

	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	self displayCoordmoveToX: points first x y: points first y.
	self
		displayCoordcurveToX: points fourth x y: points fourth y
		control1X: points second x y: points second y
		control2X: points third x y: points third y.

	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 18:47'!
displayCoordcurveToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2

	| dx dy hops increment t x0 y0 oneLessT f2 f3 f1 f23 f4 |

	dx _ (x1-currentX) abs + (x2-x1) abs + (x3-x2) abs.
	dy _ (y1-currentY) abs + (y2-y1) abs + (y3-y2) abs.
	hops _ (dx max: dy) truncated * hopsFactor + 1.
	increment _ 1.0 / hops.
	t _ 0.0.
	x0 _ currentX.
	y0 _ currentY.
	self addCurrentToTrajectory.
	hops-1 timesRepeat: [
		t _ t + increment.
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		currentX _ (f1 * x0) + (f2 * x1) + (f3 * x2) + (f4 * x3).
		currentY _ (f1 * y0) + (f2 * y1) + (f3 * y2) + (f4 * y3).
		self addCurrentToTrajectory ].
	currentX _ x3.
	currentY _ y3.
	self addCurrentToTrajectory.
	lastControl2Point _ x2@y2! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 19:02'!
displayCoordlineToX: x2 y: y2

	self displayCoordbasicLineToX: x2 y: y2! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 18:58'!
displayCoordmoveToX: x y: y

	"A discontinuity in the trajectory"
	
	self flag: #lastAndButLast.	"Remove if we just use current (and not last) in #addCurrentToTrajectory"
	currentX _ nil.
	currentY _ nil.
	self addCurrentToTrajectory.

	lastDisplayY _ nil.
	firstX _ x.
	firstY _ y.
	currentX _ x.
	currentY _ y.
	trajectoryLength _ 0.0.

	lastControl2Point _ nil! !

!Morphic3Canvas methodsFor: 'trajectories - display coordinates' stamp: 'jmv 11/8/2012 19:01'!
displayCoordquadraticBezier: points borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	bc ifNil: [ fc ifNil: [ ^self ]].

	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	self displayCoordmoveToX: points first x y: points first y.
	self
		displayCoordquadraticBezierCurveToX: points third x y: points third y
		controlX: points second x y: points second y.

	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'trajectories - path commands' stamp: 'jmv 11/8/2012 19:01'!
displayCoordquadraticBezierCurveToX: x2 y: y2 controlX: x1 y: y1

	| dx dy hops increment t x0 y0 oneLessT f1 f3 f2 |
	dx _ (x1-currentX) abs + (x2-x1) abs.
	dy _ (y1-currentY) abs + (y2-y1) abs.
	hops _ (dx max: dy) truncated * hopsFactor + 1.
	increment _ 1.0 / hops.
	t _ 0.0.
	x0 _ currentX.
	y0 _ currentY.
	self addCurrentToTrajectory.
	hops-1 timesRepeat: [
		t _ t + increment.
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2* oneLessT * t.
		f3 _ t * t.
		currentX _ (f1 * x0) + (f2 * x1) + (f3 * x2).
		currentY _ (f1 * y0) + (f2 * y1) + (f3 * y2).
		self addCurrentToTrajectory ].
	currentX _ x2.
	currentY _ y2.
	self addCurrentToTrajectory.
	
	lastControl2Point _ x1@y1! !

!Morphic3Canvas methodsFor: 'trajectories - display coordinates' stamp: 'jmv 11/8/2012 18:58'!
displayCoordspolyLine: points borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao

	| p |
	bc ifNil: [ fc ifNil: [ ^self ]].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	self displayCoordmoveToX: points first x y: points first y.
	2 to: points size do: [ :i |
		p _ points at: i.
		self displayCoordlineToX: p x y: p y ].

	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'drawing-text' stamp: 'jmv 7/10/2014 14:36'!
displayString: aString from: startIndex to: stopIndex at: aPoint font: font
	| ascii destX destY displayWidth ps isBold scaledPS wheight isItalic char |
	"
	En un inspector sobre un InnerTextMorph (o un TextModelMorph para ver clipping correcto!!, ehem, si no hay rotacion... Anda hasta con las window y con el world!! Ver comentario en Morphic3Canvas!!!!!!) hacer
| c |
c _ FormCanvas onForm: Display.
c _ Morphic3Canvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self

Ver AbstractFont class>>default
Si hay un StrokeFont, mejor, no? Digo, para que el widthOf: sea el correcto!!
(si no se ve todo raro)
	"
"font class print."
	destY _ aPoint y.
	destX _ aPoint x.
	ps _ font pointSize.
	isBold _ font isBold.
	isItalic _ font isItalic.

	scaledPS _ currentTransformation scale * ps.
	wheight _ isBold ifTrue: [ scaledPS/10 * 1.5 ] ifFalse: [ scaledPS/10 ].
	(scaledPS > 3 and: [ scaledPS < "10"14 and: [ currentTransformation doesNotRotate ]]) ifTrue: [
		scaledPS = 10.0 ifFalse: [
			"mhhh. para 10, p.ej, quisiera que no joda el fin de curva..."
			autoHintText _ true ]].
	
	"Podriamos meter scaledPS en este 1.1... Pero es mejor que se haga con la location bien."
	"Ah, tener que crear una location para cada char suena medio guarango... mejorar"
	"NECESITAMOS pasar el bw en coordenadas nuestras!!!!!!!!!!!!"
	self setupBorderWidth: wheight borderColor: fontForeColor fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil.
	startIndex to: stopIndex do: [ :charIndex |
		char _ aString at: charIndex.
		ascii _ char asciiValue.
		displayWidth _ font widthOf: char.
		self drawM3SansCharCode: ascii atX: destX y: destY pointSize: ps italics: isItalic.
		destX _ destX + displayWidth.
	].
	autoHintText _ false! !

!Morphic3Canvas methodsFor: 'drawing-icons' stamp: 'jmv 7/30/2014 20:03'!
drawButtonIconFromCurrentMorph

	currentMorph iconName ifNotNil: [ :selector |
		self perform: selector.
		^true ].
	^false! !

!Morphic3Canvas methodsFor: 'glyphs' stamp: 'jmv 11/9/2012 00:37'!
drawChar64atX: x0
"SOLO DEMO"
"
| c mt2x3 |
mt2x3 _ MatrixTransform2x3 identity.
mt2x3 setRadians: 15/180 * Float pi.
mt2x3 setPosition: 10@10.
c _ Morphic3Canvas onForm: Display.
c instVarNamed: 'currentTransformation' put: mt2x3.
c setupBorderWidth: 1.0 borderColor: nil fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
10 to: 300 by: 34/34.0 / 4 do: [ :x0 |
	c 
		polyLine: {x0@0. x0+40@0. x0+40@50. x0@50. x0@0} 
		borderWidth: 0 
		borderColor: Color white 
		fillColor: Color white 
		strokeDashArray: nil 
		strokeDashArrayOffset: nil .
	c setupBorderWidth: 1.0 borderColor: nil fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	c initializeTrajectory.
	c drawChar64atX: x0.
	c finishTrajectory.
	Display forceToScreen.
	].
"
"
| c mt2x3 |
mt2x3 _ MatrixTransform2x3 identity.
mt2x3 setRadians: 15/180 * Float pi.
mt2x3 setPosition: 10@10.
c _ Morphic3Canvas onForm: Display.
c instVarNamed: 'currentTransformation' put: mt2x3.
c setupBorderWidth: 1.0 borderColor: nil fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
1 to: 30 by: 34/34.0 / 20 do: [ :x0 |
	mt2x3 _ MatrixTransform2x3 identity.
	mt2x3 setPosition: 100@10.
	mt2x3 setScale: x0/10.
	mt2x3 setRadians: x0 * 5/180 * Float pi.
	c instVarNamed: 'currentTransformation' put: mt2x3.
	c 
		polyLine: {x0@0. x0+34@0. x0+34@40. x0@40. x0@0} 
		borderWidth: 0 
		borderColor: Color white 
		fillColor: Color white 
		strokeDashArray: nil 
		strokeDashArrayOffset: nil.
	c setupBorderWidth: 1.0 borderColor: nil fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	c initializeTrajectory.
	c drawChar64atX: x0.
	c finishTrajectory.
	Display forceToScreen.
	].
"
	self
		moveToX: x0 + 12.94 y: 23.84;
		curveToX: x0 + 14.19 y: 27.80 control1X: x0 + 12.94 y: 25.52 control2X: x0 + 13.36 y: 26.84;
		curveToX: x0 + 17.62 y: 29.22 control1X: x0 + 15.02 y: 28.75 control2X: x0 + 16.17 y: 29.22;
		curveToX: x0 + 21.01 y: 27.78 control1X: x0 + 19.06 y: 29.22 control2X: x0 + 20.19 y: 28.74;
		curveToX: x0 + 22.26 y: 23.84 control1X: x0 + 21.84 y: 26.82 control2X: x0 + 22.26 y: 25.51;
		curveToX: x0 + 20.99 y: 19.94 control1X: x0 + 22.26 y: 22.20 control2X: x0 + 21.84 y: 20.90;
		curveToX: x0 + 17.58 y: 18.48 control1X: x0 + 20.15 y: 18.97 control2X: x0 + 19.01 y: 18.48;
		curveToX: x0 + 14.19 y: 19.92 control1X: x0 + 16.17 y: 18.48 control2X: x0 + 15.04 y: 18.96;
		curveToX: x0 + 12.94 y: 23.84 control1X: x0 + 13.36 y: 20.88 control2X: x0 + 12.94 y: 22.19;
		moveToX: x0 + 22.52 y: 29.10;
		curveToX: x0 + 20.10 y: 31.10 control1X: x0 + 21.82 y: 30.00 control2X: x0 + 21.01 y: 30.67;
		curveToX: x0 + 16.93 y: 31.74 control1X: x0 + 19.20 y: 31.52 control2X: x0 + 18.14 y: 31.74;
		curveToX: x0 + 12.01 y: 29.56 control1X: x0 + 14.92 y: 31.74 control2X: x0 + 13.28 y: 31.01;
		curveToX: x0 + 10.13 y: 23.84 control1X: x0 + 10.76 y: 28.09 control2X: x0 + 10.13 y: 26.19;
		curveToX: x0 + 12.03 y: 18.13 control1X: x0 + 10.13 y: 21.50 control2X: x0 + 10.76 y: 19.60;
		curveToX: x0 + 16.93 y: 15.93 control1X: x0 + 13.30 y: 16.67 control2X: x0 + 14.93 y: 15.93;
		curveToX: x0 + 20.12 y: 16.60 control1X: x0 + 18.14 y: 15.93 control2X: x0 + 19.20 y: 16.16;
		curveToX: x0 + 22.52 y: 18.59 control1X: x0 + 21.03 y: 17.03 control2X: x0 + 21.83 y: 17.70;
		lineToX: x0 + 22.52 y: 16.28;
		lineToX: x0 + 25.04 y: 16.28;
		lineToX: x0 + 25.04 y: 29.22;
		curveToX: x0 + 29.05 y: 26.88 control1X: x0 + 26.75 y: 28.96 control2X: x0 + 28.08 y: 28.18;
		curveToX: x0 + 30.50 y: 21.80 control1X: x0 + 30.02 y: 25.57 control2X: x0 + 30.50 y: 23.88;
		curveToX: x0 + 29.94 y: 18.27 control1X: x0 + 30.50 y: 20.55 control2X: x0 + 30.32 y: 19.37;
		curveToX: x0 + 28.27 y: 15.21 control1X: x0 + 29.58 y: 17.17 control2X: x0 + 29.02 y: 16.15;
		curveToX: x0 + 23.81 y: 11.70 control1X: x0 + 27.05 y: 13.68 control2X: x0 + 25.57 y: 12.51;
		curveToX: x0 + 18.09 y: 10.47 control1X: x0 + 22.06 y: 10.88 control2X: x0 + 20.16 y: 10.47;
		curveToX: x0 + 13.95 y: 11.05 control1X: x0 + 16.65 y: 10.47 control2X: x0 + 15.27 y: 10.66;
		curveToX: x0 + 10.27 y: 12.73 control1X: x0 + 12.62 y: 11.42 control2X: x0 + 11.40 y: 11.98;
		curveToX: x0 + 5.95 y: 17.44 control1X: x0 + 8.43 y: 13.93 control2X: x0 + 6.99 y: 15.50;
		curveToX: x0 + 4.40 y: 23.74 control1X: x0 + 4.92 y: 19.38 control2X: x0 + 4.40 y: 21.48;
		curveToX: x0 + 5.40 y: 28.98 control1X: x0 + 4.40 y: 25.60 control2X: x0 + 4.74 y: 27.35;
		curveToX: x0 + 8.34 y: 33.28 control1X: x0 + 6.08 y: 30.60 control2X: x0 + 7.06 y: 32.04;
		curveToX: x0 + 12.61 y: 36.06 control1X: x0 + 9.57 y: 34.50 control2X: x0 + 10.99 y: 35.43;
		curveToX: x0 + 17.80 y: 37.03 control1X: x0 + 14.23 y: 36.70 control2X: x0 + 15.96 y: 37.03;
		curveToX: x0 + 22.24 y: 36.25 control1X: x0 + 19.31 y: 37.03 control2X: x0 + 20.79 y: 36.77;
		curveToX: x0 + 26.27 y: 34.07 control1X: x0 + 23.71 y: 35.75 control2X: x0 + 25.05 y: 35.02;
		lineToX: x0 + 27.85 y: 36.02;
		drawChar64xtra1atX: x0
! !

!Morphic3Canvas methodsFor: 'glyphs' stamp: 'jmv 11/9/2012 00:36'!
drawChar64xtra1atX: x0
"SOLO DEMO"
	self
		curveToX: x0 + 23.05 y: 38.63 control1X: x0 + 26.39 y: 37.16 control2X: x0 + 24.79 y: 38.03;
		curveToX: x0 + 17.80 y: 39.54 control1X: x0 + 21.33 y: 39.24 control2X: x0 + 19.58 y: 39.54;
		curveToX: x0 + 11.66 y: 38.38 control1X: x0 + 15.63 y: 39.54 control2X: x0 + 13.58 y: 39.15;
		curveToX: x0 + 6.53 y: 35.04 control1X: x0 + 9.74 y: 37.62 control2X: x0 + 8.03 y: 36.51;
		curveToX: x0 + 3.10 y: 29.96 control1X: x0 + 5.03 y: 33.58 control2X: x0 + 3.89 y: 31.88;
		curveToX: x0 + 1.92 y: 23.74 control1X: x0 + 2.32 y: 28.03 control2X: x0 + 1.92 y: 25.95;
		curveToX: x0 + 3.12 y: 17.64 control1X: x0 + 1.92 y: 21.60 control2X: x0 + 2.32 y: 19.57;
		curveToX: x0 + 6.53 y: 12.54 control1X: x0 + 3.91 y: 15.70 control2X: x0 + 5.05 y: 14.01;
		curveToX: x0 + 11.77 y: 9.13 control1X: x0 + 8.04 y: 11.05 control2X: x0 + 9.79 y: 9.92;
		curveToX: x0 + 18.06 y: 7.93 control1X: x0 + 13.75 y: 8.33 control2X: x0 + 15.84 y: 7.93;
		curveToX: x0 + 24.97 y: 9.46 control1X: x0 + 20.54 y: 7.93 control2X: x0 + 22.85 y: 8.44;
		curveToX: x0 + 30.33 y: 13.81 control1X: x0 + 27.10 y: 10.48 control2X: x0 + 28.89 y: 11.93;
		curveToX: x0 + 32.33 y: 17.55 control1X: x0 + 31.21 y: 14.95 control2X: x0 + 31.88 y: 16.20;
		curveToX: x0 + 33.04 y: 21.73 control1X: x0 + 32.80 y: 18.90 control2X: x0 + 33.04 y: 20.29;
		curveToX: x0 + 30.24 y: 29.03 control1X: x0 + 33.04 y: 24.82 control2X: x0 + 32.10 y: 27.25;
		curveToX: x0 + 22.52 y: 31.81 control1X: x0 + 28.38 y: 30.81 control2X: x0 + 25.81 y: 31.74;
		lineToX: x0 + 22.52 y: 29.10! !

!Morphic3Canvas methodsFor: 'drawing-text' stamp: 'jmv 7/2/2014 08:55'!
drawCharCode: charCode atX: x0 y: y0 pointSize: ps
"Primer intento de fonts scalables en M3. Vera Sans, importado asi: TTF -> PDF -> SVG -> Codigo Smalltalk.
Anda mal para tamaños pequeños, porque es un outline font y no un stroke font...
"
"
(Morphic3Canvas onForm: Display) 
	setupBorderWidth: 0 borderColor: Color black fillColor: Color blue strokeDashArray: nil strokeDashArrayOffset: nil;
	drawCharCode: 167 atX: 150 y: 200 pointSize: 200. 
Display forceToScreen
"
"
(Morphic3Canvas onForm: Display) 
	setupBorderWidth: 0.25 borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil;
	drawCharCode: 97 atX: 150 y: 200 pointSize: 10. 
Display forceToScreen
"
"
(Morphic3Canvas onForm: Display) 
	setupBorderWidth: 0 borderColor: Color blue fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil;
	drawCharCode: 97 atX: 150 y: 200 pointSize: 12;
	drawCharCode: 97 atX: 158 y: 200.25 pointSize: 12;
	drawCharCode: 97 atX: 166 y: 200.5 pointSize: 12;
	drawCharCode: 97 atX: 174 y: 200.75 pointSize: 12;
	drawCharCode: 97 atX: 182 y: 201 pointSize: 12. 
Display forceToScreen
"
	| symbol |
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].
	
	symbol _ ('drawVeraSans', charCode printString, 'pointSize:atX:y:') asSymbol.
	
	self initializeTrajectory.
	
	self perform: symbol withArguments: {ps. x0. y0}.
	
	"hacer esto 1 vez para todo el parrafo?"
	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:44'!
drawM3Box

	-0.5 to: 9.5 do: [ :x |
		self
			moveToX: x y: -0.5;
			lineToX: x y: 9.5 ].
	-3.5 to: 9.5 do: [ :y |
		self
			moveToX: -0.5 y: y;
			lineToX: 5.5 y: y ]! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:44'!
drawM3Sans100
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans100;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 4 y: 6;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:44'!
drawM3Sans101
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans101;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 3;
		r: 4;
		u: 1.5;
		cul: 1.5;
		l: 1;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans102
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans102;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1 y: 0;
		u: 7.5;
		cur: 1.5;
		r: 0.5;
		m: 0 y: 6;
		r: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans103
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans103;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 4 y: 0;
		l: 2.5;
		clu: 1.5;
		u: 3;
		cur: 1.5;
		r: 2.5;
		d: 7.5;
		cdl: 1.5;
		l: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans104
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans104;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		m: 0 y: 6;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans105
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans105;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 6;
		m: 0 y: 8.5;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans106
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans106;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1 y: 6;
		d: 7.5;
		cdl: 1.5;
		l: 0.5;
		m: 1 y: 8.5;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans107
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans107;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3;
		li: 4 y: 6;
		m: 1.4 y: 4.05;
		li: 4 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans108
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans108;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans109
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans109;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 6;
		r: 4.5;
		crd: 1.5;
		d: 4.5;
		m: 3 y: 6;
		d: 6! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans110
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans110;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 6;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans111
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans111;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1.5 y: 0;
		clu: 1.5;
		u: 3;
		cur: 1.5;
		r: 1;
		crd: 1.5;
		d: 3;
		cdl: 1.5;
		l: 1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans112
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans112;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: -3;
		u: 9;
		r: 2.5;
		crd: 1.5;
		d: 3;
		cdl: 1.5;
		l: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:45'!
drawM3Sans113
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans113;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 4 y: -3;
		u: 9;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans114
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans114;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 6;
		r: 1.5;
		cDx: 1.5 dy: -0.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans115
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans115;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 3;
		l: 0.5;
		clur: 1.5;
		r: 1;
		cDx: 1.5 dy: -0.5;
		
		m: 2 y: 3;
		r: 0.5;
		crdl: 1.5;
		l: 1;
		cDx: -1.5 dy: 0.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans116
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans116;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5;
		m: 0 y: 6;
		r: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans117
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans117;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		d: 4.5;
		cdr: 1.5;
		r: 2.5;
		u: 6! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans118
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans118;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		li: 2 y: 0;
		li: 4 y: 6! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans119
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans119;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		li: 1.5 y: 0;
		li: 3 y: 4;
		li: 4.5 y: 0;
		li: 6 y: 6! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans120
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans120;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		li: 4 y: 0;
		m: 4 y: 6;
		li: 0 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans121
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans121;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		li: 2 y: 0;
		m: 4 y: 6;
		li: 1 y: -3;
		l: 1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans122
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans122;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 6;
		r: 4;
		li: 0 y: 0;
		r: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans123
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans123;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 4;
		cru: 1.5;
		u: 3;
		cur: 1.5;
		m: 0 y: 4;
		crd: 1.5;
		d: 3;
		cdr: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans124
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans124;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 10;
		d: 11! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans125
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans125;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 3 y: 4;
		clu: 1.5;
		u: 3;
		cul: 1.5;
		m: 3 y: 4;
		cld: 1.5;
		d: 3;
		cdl: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans126
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans126;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 3;
		curd: 1;
		cdru: 1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:46'!
drawM3Sans127
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans127;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 6;
		r: 4;
		d: 6;
		l: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/7/2014 22:27'!
drawM3Sans32! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans33
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans33;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 6;
		m: 0 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans34
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans34;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 2;
		m: 2 y: 9;
		d: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans35
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans35;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0.5 y: 0;
		li: 2 y: 9;
		m: 3 y: 0;
		li: 4.5 y: 9;
		m: 0 y: 3;
		r: 5;
		m: 0 y: 6;
		r: 5
		! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans36
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans36;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 3.5;
		l: 0.5;
		clur: 1.5;
		r: 1;
		cDx: 1.5 dy: -0.5;
		
		m: 2 y: 3.5;
		r: 0.5;
		crdl: 1.5;
		l: 1;
		cDx: -1.5 dy: 0.5;
		
		m: 2 y: 9;
		d: 11! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans37
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans37;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1.5 y: 0;
		li: 6.5 y: 9;
		m: 1.5 y: 5.7;
		clur: 1.5;
		crdl: 1.5;
		m: 6.5 y: 0.3;
		clur: 1.5;
		crdl: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans38
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans38;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 6 y: 0;
		li: 1 y: 6;
		li: 0.5 y: 7;
		u: 0.5;
		cur: 1.5;
		crd: 1.5;
		d: 0.5;
		li: 0 y: 2.5;
		d: 1;
		cdr: 1.5;
		r: 1;
		li: 4 y: 1;
		li: 5 y: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans39
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans39;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans40
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans40;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 10;
		cld: 1.5;
		li: 0 y: 6;
		d: 4;
		li: 0.5 y: -0.5;
		cdr: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans41
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans41;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 10;
		crd: 1.5;
		li: 2 y: 6;
		d: 4;
		li: 1.5 y: -0.5;
		cdl: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans42
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans42;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 7.5;
		li: 5 y: 4.5;
		m: 0 y: 4.5;
		li: 5 y: 7.5;
		m: 2.5 y: 9;
		d: 6! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans43
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans43;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2.5 y: 0;
		u: 6;
		m: 0 y: 3;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans44
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans44;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1.25 y: 0;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		li: 0 y: -2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans45
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans45;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 3;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:47'!
drawM3Sans46
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans46;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans47
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans47;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: -1;
		li: 4 y: 10! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans48
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans48;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 7;
		cur: 2;
		r: 1;
		crd: 2;
		d: 5;
		cdl: 2;
		l: 1;
		clu: 2;
		u: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans49
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans49;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1 y: 7;
		li: 3 y: 9;
		d: 9;
		m: 1 y: 0;
		r: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans50
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans50;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 7;
		cur: 2;
		r: 1;
		crd: 2;
		d: 1;
		li: 0 y: 0;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans51
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans51;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		r: 3;
		crd: 2;
		cdl: 2;
		l: 2;
		r: 2;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans52
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans52;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 9;
		li: 0 y: 2;
		r: 5;
		m: 4 y: 0;
		u: 4.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 7/11/2014 11:05'!
drawM3Sans53
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans53;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 4.3 y: 9;
		l: 4.3;
		d: 4;
		r: 3;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 1.5;
		cDx: -1.5 dy: 0.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans54
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans54;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 5;
		r: 3;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 1;
		clu: 2;
		u: 4;
		cur: 3;
		r: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans55
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans55;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		r: 5;
		li: 1 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans56
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans56;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 3 y: 9;
		crd: 2;
		cdl: 2;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 1;
		clu: 2;
		u: 1;
		cur: 2;
		r: 1;
		l: 1;
		clu: 2;
		cur: 2;
		r: 1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans57
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans57;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 5 y: 4;
		l: 3;
		clu: 2;
		u: 1;
		cur: 2;
		r: 1;
		crd: 2;
		d: 4;
		cdl: 3;
		l: 1.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans58
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans58;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 5;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25;
		m: 0 y: 1;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans59
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans59;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 1.25 y: 4;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25;
		m: 1.25 y: 0;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		li: 0 y: -2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:48'!
drawM3Sans60
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans60;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 4 y: 5;
		li: 0 y: 2.5;
		li: 4 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans61
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans61;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 4;
		r: 4;
		m: 0 y: 1;
		r: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans62
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans62;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 5;
		li: 4 y: 2.5;
		li: 0 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans63
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans63;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 7;
		curd: 2;
		d: 0.5;
		cdl: 1.5;
		l: 0.5;
		d: 2;
		m: 2 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans64
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans64;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 5 y: 5.5;
		crdl: 1.5;
		m: 5 y: 5.5;
		cldr: 1.5;
		m: 6.5 y: 4;
		d: 2;
		r: 0.5;
	cDx: 2 dy: 3;
	cDy: 4 dx: -4;
	cDx:  -5 dy: -5;
	cDy: -5 dx: 5;
	r: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans65
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans65;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		li: 2.5 y: 9;
		li: 5 y: 0;
		m: 0.56 y: 2;
		r: 3.88! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans66
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans66;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		r: 2.5;
		crd: 2;
		cdl: 2;
		l: 2.5;
		r: 3;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans67
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans67;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 5 y: 9;
		l: 3;
		cld: 2;
		d: 5;
		cdr: 2;
		r: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans68
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans68;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		r: 2;
		crd: 3;
		d: 3;
		cdl: 3;
		l: 2;
		u: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans69
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans69;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 5 y: 0;
		l: 5;
		u: 9;
		r: 5;
		m: 0 y: 5;
		r: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans70
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans70;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		r: 5;
		m: 0 y: 5;
		r: 4! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans71
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans71;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 5 y: 9;
		l: 3;
		cld: 2;
		d: 5;
		cdr: 2;
		r: 3;
		u: 5;
		l: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans72
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans72;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		m: 5 y: 0;
		u: 9;
		m: 0 y: 5;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:49'!
drawM3Sans73
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans73;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans74
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans74;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		r: 2;
		cru: 2;
		u: 7! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans75
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans75;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3.5;
		li: 5 y: 9;
		m: 1.93 y: 5.6;
		li: 5 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans76
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans76;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 9;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans77
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans77;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		li: 3 y: 4;
		li: 6 y: 9;
		d: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans78
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans78;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		li: 5 y: 0;
		u: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans79
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans79;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans80
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans80;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans81
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans81;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3;
		m: 3 y: 2;
		li: 6 y: -1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans82
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans82;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5;
		m: 3 y: 4;
		li: 5 y: 0! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans83
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans83;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2.5 y: 5;
		l: 1;
		clu: 1.5;
		u: 1;
		cur: 1.5;
		r: 2;
		cDx: 1.5 dy: -0.5;
		
		m: 2.5 y: 5;
		r: 1;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 2;
		cDx: -1.5 dy: 0.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans84
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans84;
	finishTrajectory.
Display forceToScreen
"
	self
		m: -0.5 y: 9;
		r: 6;
		m: 2.5 y: 9;
		d: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans85
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans85;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 7;
		cdr: 2;
		r: 1;
		cru: 2;
		u: 7! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans86
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans86;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		li: 2.5 y: 0;
		li: 5 y: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:50'!
drawM3Sans87
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans87;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		li: 2 y: 0;
		li: 4 y: 6;
		li: 6 y: 0;
		li: 8 y: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans88
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans88;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		li: 5 y: 0;
		m: 0 y: 0;
		li: 5 y: 9! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans89
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans89;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		li: 2.5 y: 4.5;
		li: 5 y: 9;
		m: 2.5 y: 4.5;
		d: 4.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans90
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans90;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		r: 5;
		li: 0 y: 0;
		r: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans91
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans91;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 10;
		l: 2;
		d: 12;
		r: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans92
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans92;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 10;
		li: 4  y: -1! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans93
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans93;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 10;
		r: 2;
		d: 12;
		l: 2! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans94
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans94;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 2 y: 0;
		u: 9;
		m: 0 y: 7;
		li: 2 y: 9;
		li: 4 y: 7! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans95
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans95;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 3;
		r: 8;
		m: 2 y: 1;
		li: 0 y: 3;
		li: 2 y: 5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans96
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans96;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		li: 1 y: 8! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans97
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans97;
	finishTrajectory.
Display forceToScreen
"
"
| loc x s |
loc _ MatrixTransform2x3 withRadians: 0 scale: 160 position: 30@30.
x _ 10.
s _ 1.
0 to: 60 do: [ :i |
	s _ i//4+5.
	s _ s / 10.0.
	false ifTrue: [ s _ 1 ].
	loc _ (MatrixTransform2x3 withRadians: 0.0 scale: s position: (x + 30)@(i/4.0  + 120)) negateYAxisAndAngle.
	(Morphic3Canvas onForm: Display)
		intoLocation: loc;
		setupBorderWidth: (loc externalizeScalar: 1)  borderColor: Color black fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
		initializeTrajectory;
		drawM3Sans97;
		finishTrajectory.
	loc _ (MatrixTransform2x3 withRadians: 0.0 scale: s position: (x + 30)@(i/4.0  + 200)) negateYAxisAndAngle.
	(Morphic3Canvas onForm: Display)
		autoHintText: true;
		intoLocation: loc;
		setupBorderWidth: (loc externalizeScalar: 1)  borderColor: Color black fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
		initializeTrajectory;
		drawM3Sans97;
		finishTrajectory.
	loc _ (MatrixTransform2x3 withRadians: 0.0 scale: s position: (x + 30)@(i/4.0  + 280)) negateYAxisAndAngle.
	(Morphic3Canvas onForm: Display)
		autoHintText: false;
		intoLocation: loc;
		setupBorderWidth: (loc externalizeScalar: 0.1)  borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
		drawM3Box;
		finishTrajectory;
		setupBorderWidth: (loc externalizeScalar: 1)  borderColor: Color black fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
		initializeTrajectory;
		drawM3Sans97;
		finishTrajectory.
		Display forceToScreen.
		x _ 10*s + x.
		].

	"
	self
		m: 0.5 y: 6;
		r: 2;
		crd: 1.5;
		d: 4.5;
		l: 2.5; 
		clur: 1.5;
		r: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans98
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans98;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 0 y: 9;
		d: 9;
		r: 2.5;
		cru: 1.5;
		u: 3;
		cul: 1.5;
		l: 2.5! !

!Morphic3Canvas methodsFor: 'm3 stroke font' stamp: 'jmv 4/12/2014 18:51'!
drawM3Sans99
"
(Morphic3Canvas onForm: Display)
	intoLocation: (MatrixTransform2x3 withRadians: 0 scale: 16 position: 38@182) negateYAxisAndAngle;
	setupBorderWidth: 2 borderColor: Color red fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	drawM3Box;
	finishTrajectory;
	setupBorderWidth: 16 borderColor: Color green fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil;
	initializeTrajectory;
	drawM3Sans99;
	finishTrajectory.
Display forceToScreen
"
	self
		m: 3 y: 6;
		l: 1.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 1.5! !

!Morphic3Canvas methodsFor: 'drawing-text' stamp: 'jmv 7/11/2014 11:16'!
drawM3SansCharCode: charCode atX: x0 y: y0 pointSize: ps italics: doItalics
	| symbol loc |
	"revisar todo este metodo"
	self revisar.
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].
	"Implement them!!"
	charCode > 127 ifTrue: [ ^self ].
	
	"Recibir la location y modificarla, no?"
	loc _ (MatrixTransform2x3 withRadians: 0.0 scale: ps/10 position: x0@(y0+ps)) negateYAxisAndAngle.
	doItalics ifTrue: [ loc italics ].
	self intoLocation: loc.

	symbol _ ('drawM3Sans', charCode printString) asSymbol.
	self initializeTrajectory.
	self perform: symbol.
	"hacer esto 1 vez para todo el parrafo?"
	self finishTrajectory.
	
	self outOfLocation! !

!Morphic3Canvas methodsFor: 'drawing-text' stamp: 'jmv 4/15/2014 15:54'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c

	| font |
"	port colorMap: nil."
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	"mhhhhhhhh. Creo que es mejor que venga un font siempre. Y uno de M3!!"
	font _ fontOrNil ifNil: [ AbstractFont default ].
	fontForeColor _ shadowColor ifNil: [ c ].
	self
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint
		font: font! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/10/2014 22:47'!
drawStringEmbossed: s at: aPoint font: fontOrNil color: c
super drawStringEmbossed: s at: aPoint font: fontOrNil color: c! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:27'!
drawVeraSans100pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4919 * ps + x0 y: 0.4923 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.8742 * ps + y0;
		curveToX: 0.4201 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4730 * ps + x0 y: 0.9067 * ps + y0 control2X: 0.4491 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3914 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3569 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1550 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.2505 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1966 * ps + x0 y: 0.9440 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1136 * ps + x0 y: 0.8385 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7692 * ps + y0;
		curveToX: 0.1550 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5973 * ps + y0 control2X: 0.1136 * ps + x0 y: 0.5280 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.1966 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2505 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4201 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.3569 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.3914 * ps + x0 y: 0.4041 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.4923 * ps + y0 control1X: 0.4491 * ps + x0 y: 0.4357 * ps + y0 control2X: 0.4730 * ps + x0 y: 0.4598 * ps + y0;
		moveToX: 0.1857 * ps + x0 y: 0.6832 * ps + y0;
		curveToX: 0.2262 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.1857 * ps + x0 y: 0.7493 * ps + y0 control2X: 0.1992 * ps + x0 y: 0.8012 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2536 * ps + x0 y: 0.8764 * ps + y0 control2X: 0.2910 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4509 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4235 * ps + x0 y: 0.8764 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4782 * ps + x0 y: 0.8012 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.7493 * ps + y0;
		curveToX: 0.4509 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.4919 * ps + x0 y: 0.6172 * ps + y0 control2X: 0.4782 * ps + x0 y: 0.5654 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4235 * ps + x0 y: 0.4902 * ps + y0 control2X: 0.3861 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2262 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.2910 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2536 * ps + x0 y: 0.4902 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1992 * ps + x0 y: 0.5654 * ps + y0 control2X: 0.1857 * ps + x0 y: 0.6172 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:28'!
drawVeraSans101pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.1867 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.2424 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.1906 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.2092 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2759 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.3224 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4821 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4499 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.5788 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.5147 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.5469 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.5788 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.5466 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.5135 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.4115 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1691 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6881 * ps + y0 control1X: 0.1183 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.1652 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5986 * ps + y0 control2X: 0.1170 * ps + x0 y: 0.5276 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4927 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5783 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.5998 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.4684 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.5093 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.4954 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4417 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4063 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2414 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2717 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.1942 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:29'!
drawVeraSans102pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4089 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.4089 * ps + x0 y: 0.2711 * ps + y0;
		lineToX: 0.3229 * ps + x0 y: 0.2711 * ps + y0;
		curveToX: 0.2555 * ps + x0 y: 0.2907 * ps + y0 control1X: 0.2907 * ps + x0 y: 0.2711 * ps + y0 control2X: 0.2682 * ps + x0 y: 0.2776 * ps + y0;
		curveToX: 0.2370 * ps + x0 y: 0.3610 * ps + y0 control1X: 0.2432 * ps + x0 y: 0.3037 * ps + y0 control2X: 0.2370 * ps + x0 y: 0.3271 * ps + y0;
		lineToX: 0.2370 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3849 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3849 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.2370 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.2370 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1467 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1467 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.0607 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.0607 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1467 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1467 * ps + x0 y: 0.3712 * ps + y0;
		curveToX: 0.1891 * ps + x0 y: 0.2384 * ps + y0 control1X: 0.1467 * ps + x0 y: 0.3103 * ps + y0 control2X: 0.1608 * ps + x0 y: 0.2661 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.1964 * ps + y0 control1X: 0.2175 * ps + x0 y: 0.2104 * ps + y0 control2X: 0.2624 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.4089 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:30'!
drawVeraSans103pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4919 * ps + x0 y: 0.6764 * ps + y0;
		curveToX: 0.4513 * ps + x0 y: 0.5250 * ps + y0 control1X: 0.4919 * ps + x0 y: 0.6113 * ps + y0 control2X: 0.4784 * ps + x0 y: 0.5608 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4246 * ps + x0 y: 0.4892 * ps + y0 control2X: 0.3871 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2258 * ps + x0 y: 0.5250 * ps + y0 control1X: 0.2904 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2528 * ps + x0 y: 0.4892 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.6764 * ps + y0 control1X: 0.1991 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.1857 * ps + x0 y: 0.6113 * ps + y0;
		curveToX: 0.2258 * ps + x0 y: 0.8273 * ps + y0 control1X: 0.1857 * ps + x0 y: 0.7412 * ps + y0 control2X: 0.1991 * ps + x0 y: 0.7915 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.8810 * ps + y0 control1X: 0.2528 * ps + x0 y: 0.8631 * ps + y0 control2X: 0.2904 * ps + x0 y: 0.8810 * ps + y0;
		curveToX: 0.4513 * ps + x0 y: 0.8273 * ps + y0 control1X: 0.3871 * ps + x0 y: 0.8810 * ps + y0 control2X: 0.4246 * ps + x0 y: 0.8631 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.6764 * ps + y0 control1X: 0.4784 * ps + x0 y: 0.7915 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.7412 * ps + y0;
		moveToX: 0.5817 * ps + x0 y: 0.8883 * ps + y0;
		curveToX: 0.5197 * ps + x0 y: 1.0958 * ps + y0 control1X: 0.5817 * ps + x0 y: 0.9814 * ps + y0 control2X: 0.5610 * ps + x0 y: 1.0506 * ps + y0;
		curveToX: 0.3298 * ps + x0 y: 1.1642 * ps + y0 control1X: 0.4784 * ps + x0 y: 1.1414 * ps + y0 control2X: 0.4150 * ps + x0 y: 1.1642 * ps + y0;
		curveToX: 0.2404 * ps + x0 y: 1.1569 * ps + y0 control1X: 0.2982 * ps + x0 y: 1.1642 * ps + y0 control2X: 0.2684 * ps + x0 y: 1.1617 * ps + y0;
		curveToX: 0.1589 * ps + x0 y: 1.1354 * ps + y0 control1X: 0.2124 * ps + x0 y: 1.1523 * ps + y0 control2X: 0.1852 * ps + x0 y: 1.1451 * ps + y0;
		lineToX: 0.1589 * ps + x0 y: 1.0480 * ps + y0;
		curveToX: 0.2370 * ps + x0 y: 1.0797 * ps + y0 control1X: 0.1852 * ps + x0 y: 1.0623 * ps + y0 control2X: 0.2113 * ps + x0 y: 1.0729 * ps + y0;
		curveToX: 0.3156 * ps + x0 y: 1.0900 * ps + y0 control1X: 0.2627 * ps + x0 y: 1.0866 * ps + y0 control2X: 0.2889 * ps + x0 y: 1.0900 * ps + y0;
		curveToX: 0.4479 * ps + x0 y: 1.0436 * ps + y0 control1X: 0.3745 * ps + x0 y: 1.0900 * ps + y0 control2X: 0.4186 * ps + x0 y: 1.0745 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.9044 * ps + y0 control1X: 0.4772 * ps + x0 y: 1.0130 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.9666 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.8600 * ps + y0;
		curveToX: 0.4206 * ps + x0 y: 0.9323 * ps + y0 control1X: 0.4733 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4496 * ps + x0 y: 0.9163 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.3916 * ps + x0 y: 0.9482 * ps + y0 control2X: 0.3569 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.1545 * ps + x0 y: 0.8795 * ps + y0 control1X: 0.2495 * ps + x0 y: 0.9562 * ps + y0 control2X: 0.1955 * ps + x0 y: 0.9306 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6764 * ps + y0 control1X: 0.1135 * ps + x0 y: 0.8284 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7607 * ps + y0;
		curveToX: 0.1545 * ps + x0 y: 0.4728 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5918 * ps + y0 control2X: 0.1135 * ps + x0 y: 0.5239 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.1955 * ps + x0 y: 0.4217 * ps + y0 control2X: 0.2495 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4206 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.3569 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.3916 * ps + x0 y: 0.4041 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.4923 * ps + y0 control1X: 0.4496 * ps + x0 y: 0.4360 * ps + y0 control2X: 0.4733 * ps + x0 y: 0.4601 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.8883 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:31'!
drawVeraSans104pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0;
		lineToX: 0.5866 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.6290 * ps + y0;
		curveToX: 0.4665 * ps + x0 y: 0.5128 * ps + y0 control1X: 0.4968 * ps + x0 y: 0.5773 * ps + y0 control2X: 0.4867 * ps + x0 y: 0.5385 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.4463 * ps + x0 y: 0.4871 * ps + y0 control2X: 0.4160 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.5206 * ps + y0 control1X: 0.3272 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.2889 * ps + x0 y: 0.4897 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6471 * ps + y0 control1X: 0.2329 * ps + x0 y: 0.5516 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.5937 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4943 * ps + y0;
		curveToX: 0.2946 * ps + x0 y: 0.4205 * ps + y0 control1X: 0.2404 * ps + x0 y: 0.4614 * ps + y0 control2X: 0.2656 * ps + x0 y: 0.4368 * ps + y0;
		curveToX: 0.3957 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3239 * ps + x0 y: 0.4043 * ps + y0 control2X: 0.3576 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5383 * ps + x0 y: 0.4547 * ps + y0 control1X: 0.4585 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5060 * ps + x0 y: 0.4157 * ps + y0;
		curveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0 control1X: 0.5705 * ps + x0 y: 0.4935 * ps + y0 control2X: 0.5866 * ps + x0 y: 0.5506 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:31'!
drawVeraSans105pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:31'!
drawVeraSans106pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9659 * ps + y0;
		curveToX: 0.1818 * ps + x0 y: 1.1173 * ps + y0 control1X: 0.2218 * ps + x0 y: 1.0356 * ps + y0 control2X: 0.2085 * ps + x0 y: 1.0861 * ps + y0;
		curveToX: 0.0539 * ps + x0 y: 1.1642 * ps + y0 control1X: 0.1554 * ps + x0 y: 1.1486 * ps + y0 control2X: 0.1128 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.0197 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.0197 * ps + x0 y: 1.0880 * ps + y0;
		lineToX: 0.0436 * ps + x0 y: 1.0880 * ps + y0;
		curveToX: 0.1135 * ps + x0 y: 1.0641 * ps + y0 control1X: 0.0778 * ps + x0 y: 1.0880 * ps + y0 control2X: 0.1011 * ps + x0 y: 1.0800 * ps + y0;
		curveToX: 0.1320 * ps + x0 y: 0.9659 * ps + y0 control1X: 0.1258 * ps + x0 y: 1.0485 * ps + y0 control2X: 0.1320 * ps + x0 y: 1.0158 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:32'!
drawVeraSans107pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1286 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.6451 * ps + y0;
		lineToX: 0.4870 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.6017 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3117 * ps + x0 y: 0.6652 * ps + y0;
		lineToX: 0.6139 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.6891 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:32'!
drawVeraSans108pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:33'!
drawVeraSans109pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5578 * ps + x0 y: 0.5143 * ps + y0;
		curveToX: 0.6384 * ps + x0 y: 0.4249 * ps + y0 control1X: 0.5802 * ps + x0 y: 0.4739 * ps + y0 control2X: 0.6071 * ps + x0 y: 0.4441 * ps + y0;
		curveToX: 0.7487 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.6696 * ps + x0 y: 0.4057 * ps + y0 control2X: 0.7064 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.8805 * ps + x0 y: 0.4562 * ps + y0 control1X: 0.8057 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.8496 * ps + x0 y: 0.4161 * ps + y0;
		curveToX: 0.9269 * ps + x0 y: 0.6261 * ps + y0 control1X: 0.9115 * ps + x0 y: 0.4959 * ps + y0 control2X: 0.9269 * ps + x0 y: 0.5525 * ps + y0;
		lineToX: 0.9269 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.8366 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.8366 * ps + x0 y: 0.6290 * ps + y0;
		curveToX: 0.8088 * ps + x0 y: 0.5123 * ps + y0 control1X: 0.8366 * ps + x0 y: 0.5766 * ps + y0 control2X: 0.8273 * ps + x0 y: 0.5377 * ps + y0;
		curveToX: 0.7238 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.7902 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.7619 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.6135 * ps + x0 y: 0.5206 * ps + y0 control1X: 0.6773 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.6405 * ps + x0 y: 0.4897 * ps + y0;
		curveToX: 0.5729 * ps + x0 y: 0.6471 * ps + y0 control1X: 0.5864 * ps + x0 y: 0.5516 * ps + y0 control2X: 0.5729 * ps + x0 y: 0.5937 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4826 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4826 * ps + x0 y: 0.6290 * ps + y0;
		curveToX: 0.4548 * ps + x0 y: 0.5123 * ps + y0 control1X: 0.4826 * ps + x0 y: 0.5763 * ps + y0 control2X: 0.4733 * ps + x0 y: 0.5374 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.4362 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4076 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.5211 * ps + y0 control1X: 0.3229 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.2865 * ps + x0 y: 0.4899 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6471 * ps + y0 control1X: 0.2324 * ps + x0 y: 0.5520 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.5940 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4943 * ps + y0;
		curveToX: 0.2926 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.2394 * ps + x0 y: 0.4607 * ps + y0 control2X: 0.2640 * ps + x0 y: 0.4360 * ps + y0;
		curveToX: 0.3947 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3213 * ps + x0 y: 0.4041 * ps + y0 control2X: 0.3553 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4958 * ps + x0 y: 0.4264 * ps + y0 control1X: 0.4344 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4681 * ps + x0 y: 0.4062 * ps + y0;
		curveToX: 0.5578 * ps + x0 y: 0.5143 * ps + y0 control1X: 0.5238 * ps + x0 y: 0.4466 * ps + y0 control2X: 0.5444 * ps + x0 y: 0.4759 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:33'!
drawVeraSans110pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0;
		lineToX: 0.5866 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.6290 * ps + y0;
		curveToX: 0.4665 * ps + x0 y: 0.5128 * ps + y0 control1X: 0.4968 * ps + x0 y: 0.5773 * ps + y0 control2X: 0.4867 * ps + x0 y: 0.5385 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.4463 * ps + x0 y: 0.4871 * ps + y0 control2X: 0.4160 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.5206 * ps + y0 control1X: 0.3272 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.2889 * ps + x0 y: 0.4897 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6471 * ps + y0 control1X: 0.2329 * ps + x0 y: 0.5516 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.5937 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4943 * ps + y0;
		curveToX: 0.2946 * ps + x0 y: 0.4205 * ps + y0 control1X: 0.2404 * ps + x0 y: 0.4614 * ps + y0 control2X: 0.2656 * ps + x0 y: 0.4368 * ps + y0;
		curveToX: 0.3957 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3239 * ps + x0 y: 0.4043 * ps + y0 control2X: 0.3576 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5383 * ps + x0 y: 0.4547 * ps + y0 control1X: 0.4585 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5060 * ps + x0 y: 0.4157 * ps + y0;
		curveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0 control1X: 0.5705 * ps + x0 y: 0.4935 * ps + y0 control2X: 0.5866 * ps + x0 y: 0.5506 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:33'!
drawVeraSans111pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:34'!
drawVeraSans112pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2189 * ps + x0 y: 0.8742 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4923 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.2378 * ps + x0 y: 0.4598 * ps + y0 control2X: 0.2616 * ps + x0 y: 0.4357 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3192 * ps + x0 y: 0.4041 * ps + y0 control2X: 0.3537 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.4601 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5140 * ps + x0 y: 0.4225 * ps + y0;
		curveToX: 0.6178 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5970 * ps + x0 y: 0.5280 * ps + y0 control2X: 0.6178 * ps + x0 y: 0.5973 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.6178 * ps + x0 y: 0.7692 * ps + y0 control2X: 0.5970 * ps + x0 y: 0.8385 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5140 * ps + x0 y: 0.9440 * ps + y0 control2X: 0.4601 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.3537 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.3192 * ps + x0 y: 0.9625 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8742 * ps + y0 control1X: 0.2616 * ps + x0 y: 0.9310 * ps + y0 control2X: 0.2378 * ps + x0 y: 0.9067 * ps + y0;
		moveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.5246 * ps + x0 y: 0.6172 * ps + y0 control2X: 0.5109 * ps + x0 y: 0.5654 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4565 * ps + x0 y: 0.4902 * ps + y0 control2X: 0.4193 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.3242 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2868 * ps + x0 y: 0.4902 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2324 * ps + x0 y: 0.5654 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.6172 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.7493 * ps + y0 control2X: 0.2324 * ps + x0 y: 0.8012 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2868 * ps + x0 y: 0.8764 * ps + y0 control2X: 0.3242 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4193 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4565 * ps + x0 y: 0.8764 * ps + y0;
		curveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5109 * ps + x0 y: 0.8012 * ps + y0 control2X: 0.5246 * ps + x0 y: 0.7493 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:34'!
drawVeraSans113pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1857 * ps + x0 y: 0.6832 * ps + y0;
		curveToX: 0.2262 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.1857 * ps + x0 y: 0.7493 * ps + y0 control2X: 0.1992 * ps + x0 y: 0.8012 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2536 * ps + x0 y: 0.8764 * ps + y0 control2X: 0.2910 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4509 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4235 * ps + x0 y: 0.8764 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4782 * ps + x0 y: 0.8012 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.7493 * ps + y0;
		curveToX: 0.4509 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.4919 * ps + x0 y: 0.6172 * ps + y0 control2X: 0.4782 * ps + x0 y: 0.5654 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4235 * ps + x0 y: 0.4902 * ps + y0 control2X: 0.3861 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2262 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.2910 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2536 * ps + x0 y: 0.4902 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1992 * ps + x0 y: 0.5654 * ps + y0 control2X: 0.1857 * ps + x0 y: 0.6172 * ps + y0;
		moveToX: 0.4919 * ps + x0 y: 0.8742 * ps + y0;
		curveToX: 0.4201 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4730 * ps + x0 y: 0.9067 * ps + y0 control2X: 0.4491 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3914 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3569 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1550 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.2505 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1966 * ps + x0 y: 0.9440 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1136 * ps + x0 y: 0.8385 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7692 * ps + y0;
		curveToX: 0.1550 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5973 * ps + y0 control2X: 0.1136 * ps + x0 y: 0.5280 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.1966 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2505 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4201 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.3569 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.3914 * ps + x0 y: 0.4041 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.4923 * ps + y0 control1X: 0.4491 * ps + x0 y: 0.4357 * ps + y0 control2X: 0.4730 * ps + x0 y: 0.4598 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.4919 * ps + x0 y: 0.8742 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:35'!
drawVeraSans114pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4489 * ps + x0 y: 0.4933 * ps + y0;
		curveToX: 0.4157 * ps + x0 y: 0.4806 * ps + y0 control1X: 0.4388 * ps + x0 y: 0.4874 * ps + y0 control2X: 0.4277 * ps + x0 y: 0.4832 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.4762 * ps + y0 control1X: 0.4040 * ps + x0 y: 0.4777 * ps + y0 control2X: 0.3910 * ps + x0 y: 0.4762 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.5260 * ps + y0 control1X: 0.3259 * ps + x0 y: 0.4762 * ps + y0 control2X: 0.2868 * ps + x0 y: 0.4928 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6681 * ps + y0 control1X: 0.2324 * ps + x0 y: 0.5589 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.6062 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4943 * ps + y0;
		curveToX: 0.2926 * ps + x0 y: 0.4205 * ps + y0 control1X: 0.2378 * ps + x0 y: 0.4611 * ps + y0 control2X: 0.2624 * ps + x0 y: 0.4365 * ps + y0;
		curveToX: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3229 * ps + x0 y: 0.4043 * ps + y0 control2X: 0.3597 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4235 * ps + x0 y: 0.3976 * ps + y0 control1X: 0.4092 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4160 * ps + x0 y: 0.3966 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.4010 * ps + y0 control1X: 0.4310 * ps + x0 y: 0.3982 * ps + y0 control2X: 0.4393 * ps + x0 y: 0.3994 * ps + y0;
		lineToX: 0.4489 * ps + x0 y: 0.4933 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:36'!
drawVeraSans115pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4806 * ps + x0 y: 0.4254 * ps + y0;
		lineToX: 0.4806 * ps + x0 y: 0.5104 * ps + y0;
		curveToX: 0.4015 * ps + x0 y: 0.4811 * ps + y0 control1X: 0.4552 * ps + x0 y: 0.4974 * ps + y0 control2X: 0.4289 * ps + x0 y: 0.4876 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.3742 * ps + x0 y: 0.4746 * ps + y0 control2X: 0.3459 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2160 * ps + x0 y: 0.4918 * ps + y0 control1X: 0.2720 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2385 * ps + x0 y: 0.4782 * ps + y0;
		curveToX: 0.1828 * ps + x0 y: 0.5533 * ps + y0 control1X: 0.1939 * ps + x0 y: 0.5055 * ps + y0 control2X: 0.1828 * ps + x0 y: 0.5260 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.6027 * ps + y0 control1X: 0.1828 * ps + x0 y: 0.5742 * ps + y0 control2X: 0.1908 * ps + x0 y: 0.5906 * ps + y0;
		curveToX: 0.3029 * ps + x0 y: 0.6364 * ps + y0 control1X: 0.2227 * ps + x0 y: 0.6144 * ps + y0 control2X: 0.2547 * ps + x0 y: 0.6256 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.6432 * ps + y0;
		curveToX: 0.4694 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.3975 * ps + x0 y: 0.6569 * ps + y0 control2X: 0.4427 * ps + x0 y: 0.6762 * ps + y0;
		curveToX: 0.5099 * ps + x0 y: 0.8053 * ps + y0 control1X: 0.4964 * ps + x0 y: 0.7260 * ps + y0 control2X: 0.5099 * ps + x0 y: 0.7607 * ps + y0;
		curveToX: 0.4494 * ps + x0 y: 0.9259 * ps + y0 control1X: 0.5099 * ps + x0 y: 0.8561 * ps + y0 control2X: 0.4898 * ps + x0 y: 0.8963 * ps + y0;
		curveToX: 0.2839 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4093 * ps + x0 y: 0.9555 * ps + y0 control2X: 0.3542 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1921 * ps + x0 y: 0.9616 * ps + y0 control1X: 0.2546 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2240 * ps + x0 y: 0.9674 * ps + y0;
		curveToX: 0.0920 * ps + x0 y: 0.9362 * ps + y0 control1X: 0.1605 * ps + x0 y: 0.9560 * ps + y0 control2X: 0.1271 * ps + x0 y: 0.9476 * ps + y0;
		lineToX: 0.0920 * ps + x0 y: 0.8434 * ps + y0;
		curveToX: 0.1901 * ps + x0 y: 0.8825 * ps + y0 control1X: 0.1252 * ps + x0 y: 0.8606 * ps + y0 control2X: 0.1579 * ps + x0 y: 0.8737 * ps + y0;
		curveToX: 0.2858 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2223 * ps + x0 y: 0.8909 * ps + y0 control2X: 0.2542 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.3835 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.3281 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3607 * ps + x0 y: 0.8880 * ps + y0;
		curveToX: 0.4176 * ps + x0 y: 0.8121 * ps + y0 control1X: 0.4063 * ps + x0 y: 0.8590 * ps + y0 control2X: 0.4176 * ps + x0 y: 0.8385 * ps + y0;
		curveToX: 0.3927 * ps + x0 y: 0.7560 * ps + y0 control1X: 0.4176 * ps + x0 y: 0.7877 * ps + y0 control2X: 0.4093 * ps + x0 y: 0.7690 * ps + y0;
		curveToX: 0.2848 * ps + x0 y: 0.7184 * ps + y0 control1X: 0.3765 * ps + x0 y: 0.7430 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.7304 * ps + y0;
		lineToX: 0.2536 * ps + x0 y: 0.7111 * ps + y0;
		curveToX: 0.1330 * ps + x0 y: 0.6574 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6993 * ps + y0 control2X: 0.1577 * ps + x0 y: 0.6814 * ps + y0;
		curveToX: 0.0959 * ps + x0 y: 0.5573 * ps + y0 control1X: 0.1082 * ps + x0 y: 0.6329 * ps + y0 control2X: 0.0959 * ps + x0 y: 0.5996 * ps + y0;
		curveToX: 0.1506 * ps + x0 y: 0.4381 * ps + y0 control1X: 0.0959 * ps + x0 y: 0.5058 * ps + y0 control2X: 0.1141 * ps + x0 y: 0.4661 * ps + y0;
		curveToX: 0.3058 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.1870 * ps + x0 y: 0.4101 * ps + y0 control2X: 0.2388 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.3996 * ps + x0 y: 0.4034 * ps + y0 control1X: 0.3390 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.3703 * ps + x0 y: 0.3986 * ps + y0;
		curveToX: 0.4806 * ps + x0 y: 0.4254 * ps + y0 control1X: 0.4289 * ps + x0 y: 0.4083 * ps + y0 control2X: 0.4559 * ps + x0 y: 0.4157 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:36'!
drawVeraSans116pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2209 * ps + x0 y: 0.2540 * ps + y0;
		lineToX: 0.2209 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.4059 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.4059 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.2209 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.2209 * ps + x0 y: 0.7760 * ps + y0;
		curveToX: 0.2389 * ps + x0 y: 0.8619 * ps + y0 control1X: 0.2209 * ps + x0 y: 0.8206 * ps + y0 control2X: 0.2269 * ps + x0 y: 0.8492 * ps + y0;
		curveToX: 0.3136 * ps + x0 y: 0.8810 * ps + y0 control1X: 0.2513 * ps + x0 y: 0.8746 * ps + y0 control2X: 0.2762 * ps + x0 y: 0.8810 * ps + y0;
		lineToX: 0.4059 * ps + x0 y: 0.8810 * ps + y0;
		lineToX: 0.4059 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3136 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.1701 * ps + x0 y: 0.9176 * ps + y0 control1X: 0.2443 * ps + x0 y: 0.9562 * ps + y0 control2X: 0.1965 * ps + x0 y: 0.9433 * ps + y0;
		curveToX: 0.1305 * ps + x0 y: 0.7760 * ps + y0 control1X: 0.1437 * ps + x0 y: 0.8916 * ps + y0 control2X: 0.1305 * ps + x0 y: 0.8444 * ps + y0;
		lineToX: 0.1305 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.0646 * ps + x0 y: 0.4791 * ps + y0;
		lineToX: 0.0646 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1305 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1305 * ps + x0 y: 0.2540 * ps + y0;
		lineToX: 0.2209 * ps + x0 y: 0.2540 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:36'!
drawVeraSans117pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7369 * ps + y0;
		curveToX: 0.2428 * ps + x0 y: 0.8536 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7887 * ps + y0 control2X: 0.2227 * ps + x0 y: 0.8276 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2630 * ps + x0 y: 0.8794 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.3822 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4204 * ps + x0 y: 0.8768 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7194 * ps + y0 control1X: 0.4767 * ps + x0 y: 0.8149 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7728 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.8722 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.9054 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9301 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.9624 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.9117 * ps + y0 control1X: 0.2518 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.9508 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0 control1X: 0.1390 * ps + x0 y: 0.8727 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.8156 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:36'!
drawVeraSans118pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0676 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.8683 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3947 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2726 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.4093 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:37'!
drawVeraSans119pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0798 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1696 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2819 * ps + x0 y: 0.8361 * ps + y0;
		lineToX: 0.3937 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.4997 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.6120 * ps + x0 y: 0.8361 * ps + y0;
		lineToX: 0.7238 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.8136 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.6706 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5646 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4469 * ps + x0 y: 0.5079 * ps + y0;
		lineToX: 0.3288 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2228 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0798 * ps + x0 y: 0.4093 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:37'!
drawVeraSans120pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5866 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3888 * ps + x0 y: 0.6754 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3317 * ps + x0 y: 0.7413 * ps + y0;
		lineToX: 0.1725 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0666 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2790 * ps + x0 y: 0.6700 * ps + y0;
		lineToX: 0.0846 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1906 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3356 * ps + x0 y: 0.6041 * ps + y0;
		lineToX: 0.4806 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5866 * ps + x0 y: 0.4093 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:37'!
drawVeraSans121pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0;
		curveToX: 0.2853 * ps + x0 y: 1.1344 * ps + y0 control1X: 0.3342 * ps + x0 y: 1.0721 * ps + y0 control2X: 0.3094 * ps + x0 y: 1.1145 * ps + y0;
		curveToX: 0.1886 * ps + x0 y: 1.1642 * ps + y0 control1X: 0.2612 * ps + x0 y: 1.1543 * ps + y0 control2X: 0.2290 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.0890 * ps + y0;
		lineToX: 0.1696 * ps + x0 y: 1.0890 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 1.0714 * ps + y0 control1X: 0.1943 * ps + x0 y: 1.0890 * ps + y0 control2X: 0.2135 * ps + x0 y: 1.0831 * ps + y0;
		curveToX: 0.2726 * ps + x0 y: 0.9884 * ps + y0 control1X: 0.2409 * ps + x0 y: 1.0597 * ps + y0 control2X: 0.2560 * ps + x0 y: 1.0320 * ps + y0;
		lineToX: 0.2887 * ps + x0 y: 0.9474 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.8370 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:37'!
drawVeraSans122pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0929 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.4913 * ps + y0;
		lineToX: 0.1818 * ps + x0 y: 0.8844 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.8844 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0807 * ps + x0 y: 0.8742 * ps + y0;
		lineToX: 0.4186 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.0929 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.0929 * ps + x0 y: 0.4093 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:38'!
drawVeraSans123pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5490 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 1.1193 * ps + y0;
		lineToX: 0.5187 * ps + x0 y: 1.1193 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 1.0831 * ps + y0 control1X: 0.4377 * ps + x0 y: 1.1193 * ps + y0 control2X: 0.3833 * ps + x0 y: 1.1072 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9391 * ps + y0 control1X: 0.3283 * ps + x0 y: 1.0590 * ps + y0 control2X: 0.3146 * ps + x0 y: 1.0110 * ps + y0;
		lineToX: 0.3146 * ps + x0 y: 0.8224 * ps + y0;
		curveToX: 0.2883 * ps + x0 y: 0.7203 * ps + y0 control1X: 0.3146 * ps + x0 y: 0.7732 * ps + y0 control2X: 0.3058 * ps + x0 y: 0.7392 * ps + y0;
		curveToX: 0.1926 * ps + x0 y: 0.6920 * ps + y0 control1X: 0.2707 * ps + x0 y: 0.7015 * ps + y0 control2X: 0.2388 * ps + x0 y: 0.6920 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.6920 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.6222 * ps + y0;
		lineToX: 0.1926 * ps + x0 y: 0.6222 * ps + y0;
		curveToX: 0.2883 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.2391 * ps + x0 y: 0.6222 * ps + y0 control2X: 0.2710 * ps + x0 y: 0.6129 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4933 * ps + y0 control1X: 0.3058 * ps + x0 y: 0.5755 * ps + y0 control2X: 0.3146 * ps + x0 y: 0.5418 * ps + y0;
		lineToX: 0.3146 * ps + x0 y: 0.3761 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2325 * ps + y0 control1X: 0.3146 * ps + x0 y: 0.3042 * ps + y0 control2X: 0.3283 * ps + x0 y: 0.2563 * ps + y0;
		curveToX: 0.5187 * ps + x0 y: 0.1964 * ps + y0 control1X: 0.3833 * ps + x0 y: 0.2085 * ps + y0 control2X: 0.4377 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.5158 * ps + x0 y: 0.2662 * ps + y0;
		curveToX: 0.4260 * ps + x0 y: 0.2877 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.2662 * ps + y0 control2X: 0.4399 * ps + x0 y: 0.2734 * ps + y0;
		curveToX: 0.4050 * ps + x0 y: 0.3781 * ps + y0 control1X: 0.4120 * ps + x0 y: 0.3020 * ps + y0 control2X: 0.4050 * ps + x0 y: 0.3322 * ps + y0;
		lineToX: 0.4050 * ps + x0 y: 0.4992 * ps + y0;
		curveToX: 0.3825 * ps + x0 y: 0.6105 * ps + y0 control1X: 0.4050 * ps + x0 y: 0.5503 * ps + y0 control2X: 0.3975 * ps + x0 y: 0.5874 * ps + y0;
		curveToX: 0.3068 * ps + x0 y: 0.6574 * ps + y0 control1X: 0.3678 * ps + x0 y: 0.6336 * ps + y0 control2X: 0.3426 * ps + x0 y: 0.6492 * ps + y0;
		curveToX: 0.3830 * ps + x0 y: 0.7052 * ps + y0 control1X: 0.3429 * ps + x0 y: 0.6661 * ps + y0 control2X: 0.3683 * ps + x0 y: 0.6821 * ps + y0;
		curveToX: 0.4050 * ps + x0 y: 0.8160 * ps + y0 control1X: 0.3976 * ps + x0 y: 0.7283 * ps + y0 control2X: 0.4050 * ps + x0 y: 0.7653 * ps + y0;
		lineToX: 0.4050 * ps + x0 y: 0.9371 * ps + y0;
		curveToX: 0.4260 * ps + x0 y: 1.0275 * ps + y0 control1X: 0.4050 * ps + x0 y: 0.9830 * ps + y0 control2X: 0.4120 * ps + x0 y: 1.0131 * ps + y0;
		curveToX: 0.5158 * ps + x0 y: 1.0490 * ps + y0 control1X: 0.4399 * ps + x0 y: 1.0418 * ps + y0 control2X: 0.4699 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 1.0490 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:38'!
drawVeraSans124pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2477 * ps + x0 y: 0.1920 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 1.1920 * ps + y0;
		lineToX: 0.1647 * ps + x0 y: 1.1920 * ps + y0;
		lineToX: 0.1647 * ps + x0 y: 0.1920 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 0.1920 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:39'!
drawVeraSans125pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1628 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.1969 * ps + x0 y: 1.0490 * ps + y0;
		curveToX: 0.2858 * ps + x0 y: 1.0280 * ps + y0 control1X: 0.2425 * ps + x0 y: 1.0490 * ps + y0 control2X: 0.2721 * ps + x0 y: 1.0420 * ps + y0;
		curveToX: 0.3068 * ps + x0 y: 0.9371 * ps + y0 control1X: 0.2998 * ps + x0 y: 1.0140 * ps + y0 control2X: 0.3068 * ps + x0 y: 0.9837 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.8160 * ps + y0;
		curveToX: 0.3288 * ps + x0 y: 0.7052 * ps + y0 control1X: 0.3068 * ps + x0 y: 0.7653 * ps + y0 control2X: 0.3141 * ps + x0 y: 0.7283 * ps + y0;
		curveToX: 0.4050 * ps + x0 y: 0.6574 * ps + y0 control1X: 0.3434 * ps + x0 y: 0.6821 * ps + y0 control2X: 0.3688 * ps + x0 y: 0.6661 * ps + y0;
		curveToX: 0.3288 * ps + x0 y: 0.6105 * ps + y0 control1X: 0.3688 * ps + x0 y: 0.6492 * ps + y0 control2X: 0.3434 * ps + x0 y: 0.6336 * ps + y0;
		curveToX: 0.3068 * ps + x0 y: 0.4992 * ps + y0 control1X: 0.3141 * ps + x0 y: 0.5874 * ps + y0 control2X: 0.3068 * ps + x0 y: 0.5503 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.3781 * ps + y0;
		curveToX: 0.2858 * ps + x0 y: 0.2877 * ps + y0 control1X: 0.3068 * ps + x0 y: 0.3318 * ps + y0 control2X: 0.2998 * ps + x0 y: 0.3017 * ps + y0;
		curveToX: 0.1969 * ps + x0 y: 0.2662 * ps + y0 control1X: 0.2721 * ps + x0 y: 0.2734 * ps + y0 control2X: 0.2425 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.1935 * ps + x0 y: 0.1964 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2325 * ps + y0 control1X: 0.2746 * ps + x0 y: 0.1964 * ps + y0 control2X: 0.3286 * ps + x0 y: 0.2085 * ps + y0;
		curveToX: 0.3967 * ps + x0 y: 0.3761 * ps + y0 control1X: 0.3830 * ps + x0 y: 0.2563 * ps + y0 control2X: 0.3967 * ps + x0 y: 0.3042 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.4933 * ps + y0;
		curveToX: 0.4230 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.3967 * ps + x0 y: 0.5418 * ps + y0 control2X: 0.4054 * ps + x0 y: 0.5755 * ps + y0;
		curveToX: 0.5187 * ps + x0 y: 0.6222 * ps + y0 control1X: 0.4406 * ps + x0 y: 0.6129 * ps + y0 control2X: 0.4725 * ps + x0 y: 0.6222 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.6222 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.6920 * ps + y0;
		lineToX: 0.5187 * ps + x0 y: 0.6920 * ps + y0;
		curveToX: 0.4230 * ps + x0 y: 0.7203 * ps + y0 control1X: 0.4725 * ps + x0 y: 0.6920 * ps + y0 control2X: 0.4406 * ps + x0 y: 0.7015 * ps + y0;
		curveToX: 0.3967 * ps + x0 y: 0.8224 * ps + y0 control1X: 0.4054 * ps + x0 y: 0.7392 * ps + y0 control2X: 0.3967 * ps + x0 y: 0.7732 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.9391 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 1.0831 * ps + y0 control1X: 0.3967 * ps + x0 y: 1.0110 * ps + y0 control2X: 0.3830 * ps + x0 y: 1.0590 * ps + y0;
		curveToX: 0.1935 * ps + x0 y: 1.1193 * ps + y0 control1X: 0.3286 * ps + x0 y: 1.1072 * ps + y0 control2X: 0.2746 * ps + x0 y: 1.1193 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 1.1193 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 1.0490 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:39'!
drawVeraSans126pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.7697 * ps + x0 y: 0.5573 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6442 * ps + y0;
		curveToX: 0.6745 * ps + x0 y: 0.6993 * ps + y0 control1X: 0.7355 * ps + x0 y: 0.6699 * ps + y0 control2X: 0.7038 * ps + x0 y: 0.6883 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.7159 * ps + y0 control1X: 0.6455 * ps + x0 y: 0.7104 * ps + y0 control2X: 0.6152 * ps + x0 y: 0.7159 * ps + y0;
		curveToX: 0.4587 * ps + x0 y: 0.6871 * ps + y0 control1X: 0.5479 * ps + x0 y: 0.7159 * ps + y0 control2X: 0.5062 * ps + x0 y: 0.7063 * ps + y0;
		curveToX: 0.4509 * ps + x0 y: 0.6842 * ps + y0 control1X: 0.4551 * ps + x0 y: 0.6858 * ps + y0 control2X: 0.4525 * ps + x0 y: 0.6849 * ps + y0;
		curveToX: 0.4401 * ps + x0 y: 0.6803 * ps + y0 control1X: 0.4486 * ps + x0 y: 0.6832 * ps + y0 control2X: 0.4450 * ps + x0 y: 0.6819 * ps + y0;
		curveToX: 0.3185 * ps + x0 y: 0.6500 * ps + y0 control1X: 0.3897 * ps + x0 y: 0.6601 * ps + y0 control2X: 0.3491 * ps + x0 y: 0.6500 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.6691 * ps + y0 control1X: 0.2899 * ps + x0 y: 0.6500 * ps + y0 control2X: 0.2616 * ps + x0 y: 0.6564 * ps + y0;
		curveToX: 0.1437 * ps + x0 y: 0.7282 * ps + y0 control1X: 0.2056 * ps + x0 y: 0.6814 * ps + y0 control2X: 0.1756 * ps + x0 y: 0.7011 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6412 * ps + y0;
		curveToX: 0.2385 * ps + x0 y: 0.5861 * ps + y0 control1X: 0.1779 * ps + x0 y: 0.6155 * ps + y0 control2X: 0.2095 * ps + x0 y: 0.5971 * ps + y0;
		curveToX: 0.3298 * ps + x0 y: 0.5690 * ps + y0 control1X: 0.2677 * ps + x0 y: 0.5747 * ps + y0 control2X: 0.2982 * ps + x0 y: 0.5690 * ps + y0;
		curveToX: 0.4552 * ps + x0 y: 0.5983 * ps + y0 control1X: 0.3656 * ps + x0 y: 0.5690 * ps + y0 control2X: 0.4074 * ps + x0 y: 0.5787 * ps + y0;
		curveToX: 0.4626 * ps + x0 y: 0.6012 * ps + y0 control1X: 0.4585 * ps + x0 y: 0.5996 * ps + y0 control2X: 0.4609 * ps + x0 y: 0.6006 * ps + y0;
		curveToX: 0.4733 * ps + x0 y: 0.6051 * ps + y0 control1X: 0.4652 * ps + x0 y: 0.6022 * ps + y0 control2X: 0.4688 * ps + x0 y: 0.6035 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6354 * ps + y0 control1X: 0.5238 * ps + x0 y: 0.6253 * ps + y0 control2X: 0.5643 * ps + x0 y: 0.6354 * ps + y0;
		curveToX: 0.6779 * ps + x0 y: 0.6168 * ps + y0 control1X: 0.6229 * ps + x0 y: 0.6354 * ps + y0 control2X: 0.6506 * ps + x0 y: 0.6292 * ps + y0;
		curveToX: 0.7697 * ps + x0 y: 0.5573 * ps + y0 control1X: 0.7056 * ps + x0 y: 0.6045 * ps + y0 control2X: 0.7362 * ps + x0 y: 0.5846 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:40'!
drawVeraSans128pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5046 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.4992 * ps + y0;
		lineToX: 0.6716 * ps + x0 y: 0.4992 * ps + y0;
		lineToX: 0.4567 * ps + x0 y: 0.3063 * ps + y0;
		lineToX: 0.2419 * ps + x0 y: 0.4992 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.4992 * ps + y0;
		lineToX: 0.4089 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:40'!
drawVeraSans129pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5475 * ps + x0 y: 1.1222 * ps + y0;
		lineToX: 0.5475 * ps + x0 y: 1.1920 * ps + y0;
		lineToX: 0.0280 * ps + x0 y: 1.1920 * ps + y0;
		lineToX: 0.0280 * ps + x0 y: 1.1222 * ps + y0;
		lineToX: 0.5475 * ps + x0 y: 1.1222 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:40'!
drawVeraSans130pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4347 * ps + x0 y: 0.2238 * ps + y0;
		lineToX: 0.4787 * ps + x0 y: 0.2238 * ps + y0;
		lineToX: 0.6701 * ps + x0 y: 0.4161 * ps + y0;
		lineToX: 0.6115 * ps + x0 y: 0.4747 * ps + y0;
		lineToX: 0.4982 * ps + x0 y: 0.3605 * ps + y0;
		lineToX: 0.4982 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4152 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4152 * ps + x0 y: 0.3605 * ps + y0;
		lineToX: 0.3010 * ps + x0 y: 0.4747 * ps + y0;
		lineToX: 0.2424 * ps + x0 y: 0.4161 * ps + y0;
		lineToX: 0.4347 * ps + x0 y: 0.2238 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:40'!
drawVeraSans131pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0866 * ps + x0 y: 0.6647 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6207 * ps + y0;
		lineToX: 0.2785 * ps + x0 y: 0.4288 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.4874 * ps + y0;
		lineToX: 0.2233 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.8185 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.8185 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.2233 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.7980 * ps + y0;
		lineToX: 0.2785 * ps + x0 y: 0.8566 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6647 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:41'!
drawVeraSans161pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2878 * ps + x0 y: 0.3512 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.3512 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.3512 * ps + y0;
		moveToX: 0.2878 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.6364 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.4620 * ps + y0;
		lineToX: 0.2780 * ps + x0 y: 0.4620 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.6364 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.9562 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:41'!
drawVeraSans162pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3688 * ps + x0 y: 0.8922 * ps + y0;
		lineToX: 0.3688 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.2570 * ps + x0 y: 0.5382 * ps + y0 control1X: 0.3210 * ps + x0 y: 0.4801 * ps + y0 control2X: 0.2837 * ps + x0 y: 0.5014 * ps + y0;
		curveToX: 0.2170 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2303 * ps + x0 y: 0.5750 * ps + y0 control2X: 0.2170 * ps + x0 y: 0.6233 * ps + y0;
		curveToX: 0.2570 * ps + x0 y: 0.8287 * ps + y0 control1X: 0.2170 * ps + x0 y: 0.7435 * ps + y0 control2X: 0.2303 * ps + x0 y: 0.7920 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2837 * ps + x0 y: 0.8655 * ps + y0 control2X: 0.3210 * ps + x0 y: 0.8867 * ps + y0;
		moveToX: 0.5549 * ps + x0 y: 0.4303 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.5143 * ps + y0;
		curveToX: 0.4855 * ps + x0 y: 0.4840 * ps + y0 control1X: 0.5308 * ps + x0 y: 0.5009 * ps + y0 control2X: 0.5077 * ps + x0 y: 0.4908 * ps + y0;
		curveToX: 0.4191 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4634 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.4412 * ps + x0 y: 0.4733 * ps + y0;
		lineToX: 0.4186 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4860 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4414 * ps + x0 y: 0.8925 * ps + y0 control2X: 0.4639 * ps + x0 y: 0.8883 * ps + y0;
		curveToX: 0.5549 * ps + x0 y: 0.8522 * ps + y0 control1X: 0.5085 * ps + x0 y: 0.8746 * ps + y0 control2X: 0.5314 * ps + x0 y: 0.8649 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.9352 * ps + y0;
		curveToX: 0.4894 * ps + x0 y: 0.9581 * ps + y0 control1X: 0.5337 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.5119 * ps + x0 y: 0.9526 * ps + y0;
		curveToX: 0.4186 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4673 * ps + x0 y: 0.9640 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9681 * ps + y0;
		lineToX: 0.4186 * ps + x0 y: 1.1090 * ps + y0;
		lineToX: 0.3688 * ps + x0 y: 1.1090 * ps + y0;
		lineToX: 0.3688 * ps + x0 y: 0.9684 * ps + y0;
		curveToX: 0.1862 * ps + x0 y: 0.8839 * ps + y0 control1X: 0.2904 * ps + x0 y: 0.9619 * ps + y0 control2X: 0.2295 * ps + x0 y: 0.9337 * ps + y0;
		curveToX: 0.1218 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1432 * ps + x0 y: 0.8338 * ps + y0 control2X: 0.1218 * ps + x0 y: 0.7669 * ps + y0;
		curveToX: 0.1862 * ps + x0 y: 0.4825 * ps + y0 control1X: 0.1218 * ps + x0 y: 0.5992 * ps + y0 control2X: 0.1432 * ps + x0 y: 0.5324 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 0.3971 * ps + y0 control1X: 0.2295 * ps + x0 y: 0.4327 * ps + y0 control2X: 0.2904 * ps + x0 y: 0.4043 * ps + y0;
		lineToX: 0.3688 * ps + x0 y: 0.2570 * ps + y0;
		lineToX: 0.4186 * ps + x0 y: 0.2570 * ps + y0;
		lineToX: 0.4191 * ps + x0 y: 0.3971 * ps + y0;
		curveToX: 0.4880 * ps + x0 y: 0.4074 * ps + y0 control1X: 0.4429 * ps + x0 y: 0.3984 * ps + y0 control2X: 0.4658 * ps + x0 y: 0.4018 * ps + y0;
		curveToX: 0.5549 * ps + x0 y: 0.4303 * ps + y0 control1X: 0.5104 * ps + x0 y: 0.4126 * ps + y0 control2X: 0.5327 * ps + x0 y: 0.4202 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:41'!
drawVeraSans163pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5759 * ps + x0 y: 0.2433 * ps + y0;
		lineToX: 0.5759 * ps + x0 y: 0.3322 * ps + y0;
		curveToX: 0.5055 * ps + x0 y: 0.3024 * ps + y0 control1X: 0.5511 * ps + x0 y: 0.3188 * ps + y0 control2X: 0.5277 * ps + x0 y: 0.3089 * ps + y0;
		curveToX: 0.4426 * ps + x0 y: 0.2921 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.2955 * ps + y0 control2X: 0.4624 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.3302 * ps + y0 control1X: 0.3944 * ps + x0 y: 0.2921 * ps + y0 control2X: 0.3608 * ps + x0 y: 0.3048 * ps + y0;
		curveToX: 0.3136 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.3231 * ps + x0 y: 0.3553 * ps + y0 control2X: 0.3136 * ps + x0 y: 0.4023 * ps + y0;
		lineToX: 0.3136 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.6461 * ps + y0;
		lineToX: 0.3136 * ps + x0 y: 0.6461 * ps + y0;
		lineToX: 0.3136 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5856 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5856 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1008 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1008 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.6461 * ps + y0;
		lineToX: 0.1188 * ps + x0 y: 0.6461 * ps + y0;
		lineToX: 0.1188 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.4601 * ps + y0;
		curveToX: 0.2682 * ps + x0 y: 0.2736 * ps + y0 control1X: 0.2160 * ps + x0 y: 0.3751 * ps + y0 control2X: 0.2334 * ps + x0 y: 0.3130 * ps + y0;
		curveToX: 0.4338 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.3031 * ps + x0 y: 0.2339 * ps + y0 control2X: 0.3582 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5002 * ps + x0 y: 0.2218 * ps + y0 control1X: 0.4536 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4758 * ps + x0 y: 0.2166 * ps + y0;
		curveToX: 0.5759 * ps + x0 y: 0.2433 * ps + y0 control1X: 0.5249 * ps + x0 y: 0.2267 * ps + y0 control2X: 0.5501 * ps + x0 y: 0.2339 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:42'!
drawVeraSans164pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6076 * ps + x0 y: 0.2833 * ps + y0;
		lineToX: 0.6076 * ps + x0 y: 0.3873 * ps + y0;
		curveToX: 0.5217 * ps + x0 y: 0.3141 * ps + y0 control1X: 0.5780 * ps + x0 y: 0.3532 * ps + y0 control2X: 0.5493 * ps + x0 y: 0.3287 * ps + y0;
		curveToX: 0.4308 * ps + x0 y: 0.2921 * ps + y0 control1X: 0.4943 * ps + x0 y: 0.2994 * ps + y0 control2X: 0.4640 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.3409 * ps + y0 control1X: 0.3797 * ps + x0 y: 0.2921 * ps + y0 control2X: 0.3377 * ps + x0 y: 0.3084 * ps + y0;
		curveToX: 0.2399 * ps + x0 y: 0.4811 * ps + y0 control1X: 0.2720 * ps + x0 y: 0.3735 * ps + y0 control2X: 0.2503 * ps + x0 y: 0.4202 * ps + y0;
		lineToX: 0.5217 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.4948 * ps + x0 y: 0.5411 * ps + y0;
		lineToX: 0.2321 * ps + x0 y: 0.5411 * ps + y0;
		curveToX: 0.2306 * ps + x0 y: 0.5641 * ps + y0 control1X: 0.2315 * ps + x0 y: 0.5490 * ps + y0 control2X: 0.2310 * ps + x0 y: 0.5566 * ps + y0;
		curveToX: 0.2306 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2306 * ps + x0 y: 0.5716 * ps + y0 control2X: 0.2306 * ps + x0 y: 0.5810 * ps + y0;
		curveToX: 0.2306 * ps + x0 y: 0.6193 * ps + y0 control1X: 0.2306 * ps + x0 y: 0.6028 * ps + y0 control2X: 0.2306 * ps + x0 y: 0.6118 * ps + y0;
		curveToX: 0.2321 * ps + x0 y: 0.6422 * ps + y0 control1X: 0.2310 * ps + x0 y: 0.6268 * ps + y0 control2X: 0.2315 * ps + x0 y: 0.6344 * ps + y0;
		lineToX: 0.4499 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.4225 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.7023 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8429 * ps + y0 control1X: 0.2503 * ps + x0 y: 0.7631 * ps + y0 control2X: 0.2720 * ps + x0 y: 0.8100 * ps + y0;
		curveToX: 0.4308 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.3377 * ps + x0 y: 0.8758 * ps + y0 control2X: 0.3797 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.5217 * ps + x0 y: 0.8702 * ps + y0 control1X: 0.4640 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4943 * ps + x0 y: 0.8849 * ps + y0;
		curveToX: 0.6076 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.5493 * ps + x0 y: 0.8556 * ps + y0 control2X: 0.5780 * ps + x0 y: 0.8312 * ps + y0;
		lineToX: 0.6076 * ps + x0 y: 0.9000 * ps + y0;
		curveToX: 0.5187 * ps + x0 y: 0.9528 * ps + y0 control1X: 0.5786 * ps + x0 y: 0.9235 * ps + y0 control2X: 0.5490 * ps + x0 y: 0.9410 * ps + y0;
		curveToX: 0.4269 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4888 * ps + x0 y: 0.9645 * ps + y0 control2X: 0.4582 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2355 * ps + x0 y: 0.8995 * ps + y0 control1X: 0.3498 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2860 * ps + x0 y: 0.9467 * ps + y0;
		curveToX: 0.1408 * ps + x0 y: 0.7023 * ps + y0 control1X: 0.1854 * ps + x0 y: 0.8523 * ps + y0 control2X: 0.1538 * ps + x0 y: 0.7866 * ps + y0;
		lineToX: 0.0378 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.0646 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.1325 * ps + x0 y: 0.6422 * ps + y0;
		curveToX: 0.1320 * ps + x0 y: 0.6198 * ps + y0 control1X: 0.1325 * ps + x0 y: 0.6347 * ps + y0 control2X: 0.1323 * ps + x0 y: 0.6272 * ps + y0;
		curveToX: 0.1320 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1320 * ps + x0 y: 0.6119 * ps + y0 control2X: 0.1320 * ps + x0 y: 0.6028 * ps + y0;
		curveToX: 0.1320 * ps + x0 y: 0.5641 * ps + y0 control1X: 0.1320 * ps + x0 y: 0.5810 * ps + y0 control2X: 0.1320 * ps + x0 y: 0.5716 * ps + y0;
		curveToX: 0.1325 * ps + x0 y: 0.5411 * ps + y0 control1X: 0.1323 * ps + x0 y: 0.5563 * ps + y0 control2X: 0.1325 * ps + x0 y: 0.5486 * ps + y0;
		lineToX: 0.0378 * ps + x0 y: 0.5411 * ps + y0;
		lineToX: 0.0646 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.4811 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.2848 * ps + y0 control1X: 0.1538 * ps + x0 y: 0.3974 * ps + y0 control2X: 0.1856 * ps + x0 y: 0.3320 * ps + y0;
		curveToX: 0.4269 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2865 * ps + x0 y: 0.2376 * ps + y0 control2X: 0.3501 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5197 * ps + x0 y: 0.2316 * ps + y0 control1X: 0.4588 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4898 * ps + x0 y: 0.2199 * ps + y0;
		curveToX: 0.6076 * ps + x0 y: 0.2833 * ps + y0 control1X: 0.5500 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.5793 * ps + x0 y: 0.2602 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:43'!
drawVeraSans165pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6066 * ps + x0 y: 0.7340 * ps + y0;
		lineToX: 0.4050 * ps + x0 y: 0.7340 * ps + y0;
		lineToX: 0.4050 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.7340 * ps + y0;
		lineToX: 0.1037 * ps + x0 y: 0.7340 * ps + y0;
		lineToX: 0.1037 * ps + x0 y: 0.6740 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.6740 * ps + y0;
		lineToX: 0.3068 * ps + x0 y: 0.6491 * ps + y0;
		lineToX: 0.2658 * ps + x0 y: 0.5734 * ps + y0;
		lineToX: 0.1037 * ps + x0 y: 0.5734 * ps + y0;
		lineToX: 0.1037 * ps + x0 y: 0.5133 * ps + y0;
		lineToX: 0.2326 * ps + x0 y: 0.5133 * ps + y0;
		lineToX: 0.0778 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1706 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3556 * ps + x0 y: 0.5690 * ps + y0;
		lineToX: 0.5397 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6330 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.5133 * ps + y0;
		lineToX: 0.6066 * ps + x0 y: 0.5133 * ps + y0;
		lineToX: 0.6066 * ps + x0 y: 0.5734 * ps + y0;
		lineToX: 0.4450 * ps + x0 y: 0.5734 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.6491 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.6740 * ps + y0;
		lineToX: 0.6066 * ps + x0 y: 0.6740 * ps + y0;
		lineToX: 0.6066 * ps + x0 y: 0.7340 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:43'!
drawVeraSans166pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5729 * ps + x0 y: 0.2511 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.3473 * ps + y0;
		curveToX: 0.4670 * ps + x0 y: 0.3073 * ps + y0 control1X: 0.5355 * ps + x0 y: 0.3294 * ps + y0 control2X: 0.5002 * ps + x0 y: 0.3160 * ps + y0;
		curveToX: 0.3708 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.4338 * ps + x0 y: 0.2985 * ps + y0 control2X: 0.4017 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2463 * ps + x0 y: 0.3253 * ps + y0 control1X: 0.3171 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.2756 * ps + x0 y: 0.3045 * ps + y0;
		curveToX: 0.2028 * ps + x0 y: 0.4142 * ps + y0 control1X: 0.2173 * ps + x0 y: 0.3462 * ps + y0 control2X: 0.2028 * ps + x0 y: 0.3758 * ps + y0;
		curveToX: 0.2316 * ps + x0 y: 0.4874 * ps + y0 control1X: 0.2028 * ps + x0 y: 0.4464 * ps + y0 control2X: 0.2124 * ps + x0 y: 0.4708 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.5270 * ps + y0 control1X: 0.2511 * ps + x0 y: 0.5037 * ps + y0 control2X: 0.2879 * ps + x0 y: 0.5169 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.5392 * ps + y0;
		curveToX: 0.5641 * ps + x0 y: 0.6134 * ps + y0 control1X: 0.4751 * ps + x0 y: 0.5532 * ps + y0 control2X: 0.5293 * ps + x0 y: 0.5779 * ps + y0;
		curveToX: 0.6169 * ps + x0 y: 0.7550 * ps + y0 control1X: 0.5993 * ps + x0 y: 0.6486 * ps + y0 control2X: 0.6169 * ps + x0 y: 0.6958 * ps + y0;
		curveToX: 0.5456 * ps + x0 y: 0.9157 * ps + y0 control1X: 0.6169 * ps + x0 y: 0.8256 * ps + y0 control2X: 0.5931 * ps + x0 y: 0.8792 * ps + y0;
		curveToX: 0.3376 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4984 * ps + x0 y: 0.9521 * ps + y0 control2X: 0.4290 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 0.9586 * ps + y0 control1X: 0.3031 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2663 * ps + x0 y: 0.9664 * ps + y0;
		curveToX: 0.1066 * ps + x0 y: 0.9240 * ps + y0 control1X: 0.1885 * ps + x0 y: 0.9508 * ps + y0 control2X: 0.1483 * ps + x0 y: 0.9393 * ps + y0;
		lineToX: 0.1066 * ps + x0 y: 0.8224 * ps + y0;
		curveToX: 0.2243 * ps + x0 y: 0.8732 * ps + y0 control1X: 0.1467 * ps + x0 y: 0.8449 * ps + y0 control2X: 0.1859 * ps + x0 y: 0.8618 * ps + y0;
		curveToX: 0.3376 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2627 * ps + x0 y: 0.8846 * ps + y0 control2X: 0.3005 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.4679 * ps + x0 y: 0.8571 * ps + y0 control1X: 0.3939 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.4373 * ps + x0 y: 0.8792 * ps + y0;
		curveToX: 0.5138 * ps + x0 y: 0.7623 * ps + y0 control1X: 0.4985 * ps + x0 y: 0.8349 * ps + y0 control2X: 0.5138 * ps + x0 y: 0.8033 * ps + y0;
		curveToX: 0.4806 * ps + x0 y: 0.6783 * ps + y0 control1X: 0.5138 * ps + x0 y: 0.7265 * ps + y0 control2X: 0.5028 * ps + x0 y: 0.6985 * ps + y0;
		curveToX: 0.3727 * ps + x0 y: 0.6329 * ps + y0 control1X: 0.4588 * ps + x0 y: 0.6582 * ps + y0 control2X: 0.4229 * ps + x0 y: 0.6430 * ps + y0;
		lineToX: 0.3127 * ps + x0 y: 0.6212 * ps + y0;
		curveToX: 0.1530 * ps + x0 y: 0.5524 * ps + y0 control1X: 0.2391 * ps + x0 y: 0.6066 * ps + y0 control2X: 0.1859 * ps + x0 y: 0.5836 * ps + y0;
		curveToX: 0.1037 * ps + x0 y: 0.4220 * ps + y0 control1X: 0.1201 * ps + x0 y: 0.5211 * ps + y0 control2X: 0.1037 * ps + x0 y: 0.4777 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.2697 * ps + y0 control1X: 0.1037 * ps + x0 y: 0.3575 * ps + y0 control2X: 0.1263 * ps + x0 y: 0.3068 * ps + y0;
		curveToX: 0.3595 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2171 * ps + x0 y: 0.2325 * ps + y0 control2X: 0.2798 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4640 * ps + x0 y: 0.2233 * ps + y0 control1X: 0.3937 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4286 * ps + x0 y: 0.2171 * ps + y0;
		curveToX: 0.5729 * ps + x0 y: 0.2511 * ps + y0 control1X: 0.4995 * ps + x0 y: 0.2295 * ps + y0 control2X: 0.5358 * ps + x0 y: 0.2387 * ps + y0;
		moveToX: 0.3097 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2067 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.2746 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.3556 * ps + x0 y: 0.1149 * ps + y0;
		lineToX: 0.4367 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3097 * ps + x0 y: 0.1578 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:53'!
drawVeraSans167pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2228 * ps + x0 y: 0.4992 * ps + y0;
		curveToX: 0.1769 * ps + x0 y: 0.5436 * ps + y0 control1X: 0.2023 * ps + x0 y: 0.5141 * ps + y0 control2X: 0.1870 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.1618 * ps + x0 y: 0.5880 * ps + y0 control1X: 0.1668 * ps + x0 y: 0.5582 * ps + y0 control2X: 0.1618 * ps + x0 y: 0.5730 * ps + y0;
		curveToX: 0.1955 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.1618 * ps + x0 y: 0.6128 * ps + y0 control2X: 0.1730 * ps + x0 y: 0.6360 * ps + y0;
		curveToX: 0.3517 * ps + x0 y: 0.7560 * ps + y0 control1X: 0.2183 * ps + x0 y: 0.6793 * ps + y0 control2X: 0.2704 * ps + x0 y: 0.7120 * ps + y0;
		curveToX: 0.3976 * ps + x0 y: 0.7120 * ps + y0 control1X: 0.3722 * ps + x0 y: 0.7413 * ps + y0 control2X: 0.3875 * ps + x0 y: 0.7267 * ps + y0;
		curveToX: 0.4128 * ps + x0 y: 0.6671 * ps + y0 control1X: 0.4077 * ps + x0 y: 0.6971 * ps + y0 control2X: 0.4128 * ps + x0 y: 0.6821 * ps + y0;
		curveToX: 0.3776 * ps + x0 y: 0.5973 * ps + y0 control1X: 0.4128 * ps + x0 y: 0.6427 * ps + y0 control2X: 0.4010 * ps + x0 y: 0.6194 * ps + y0;
		curveToX: 0.2228 * ps + x0 y: 0.4992 * ps + y0 control1X: 0.3545 * ps + x0 y: 0.5752 * ps + y0 control2X: 0.3029 * ps + x0 y: 0.5424 * ps + y0;
		moveToX: 0.4426 * ps + x0 y: 0.2423 * ps + y0;
		lineToX: 0.4426 * ps + x0 y: 0.3224 * ps + y0;
		curveToX: 0.3669 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.4155 * ps + x0 y: 0.3097 * ps + y0 control2X: 0.3903 * ps + x0 y: 0.3003 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.2843 * ps + y0 control1X: 0.3438 * ps + x0 y: 0.2876 * ps + y0 control2X: 0.3231 * ps + x0 y: 0.2843 * ps + y0;
		curveToX: 0.2311 * ps + x0 y: 0.3038 * ps + y0 control1X: 0.2733 * ps + x0 y: 0.2843 * ps + y0 control2X: 0.2487 * ps + x0 y: 0.2908 * ps + y0;
		curveToX: 0.2048 * ps + x0 y: 0.3580 * ps + y0 control1X: 0.2135 * ps + x0 y: 0.3169 * ps + y0 control2X: 0.2048 * ps + x0 y: 0.3349 * ps + y0;
		curveToX: 0.3053 * ps + x0 y: 0.4581 * ps + y0 control1X: 0.2048 * ps + x0 y: 0.3873 * ps + y0 control2X: 0.2383 * ps + x0 y: 0.4207 * ps + y0;
		curveToX: 0.3249 * ps + x0 y: 0.4694 * ps + y0 control1X: 0.3138 * ps + x0 y: 0.4630 * ps + y0 control2X: 0.3203 * ps + x0 y: 0.4668 * ps + y0;
		curveToX: 0.4596 * ps + x0 y: 0.5651 * ps + y0 control1X: 0.3936 * ps + x0 y: 0.5081 * ps + y0 control2X: 0.4385 * ps + x0 y: 0.5400 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.6520 * ps + y0 control1X: 0.4811 * ps + x0 y: 0.5901 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.6191 * ps + y0;
		curveToX: 0.4694 * ps + x0 y: 0.7301 * ps + y0 control1X: 0.4919 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.4844 * ps + x0 y: 0.7073 * ps + y0;
		curveToX: 0.4015 * ps + x0 y: 0.7902 * ps + y0 control1X: 0.4544 * ps + x0 y: 0.7529 * ps + y0 control2X: 0.4318 * ps + x0 y: 0.7729 * ps + y0;
		curveToX: 0.4455 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.4217 * ps + x0 y: 0.8071 * ps + y0 control2X: 0.4364 * ps + x0 y: 0.8245 * ps + y0;
		curveToX: 0.4596 * ps + x0 y: 0.9000 * ps + y0 control1X: 0.4549 * ps + x0 y: 0.8600 * ps + y0 control2X: 0.4596 * ps + x0 y: 0.8792 * ps + y0;
		curveToX: 0.4098 * ps + x0 y: 1.0104 * ps + y0 control1X: 0.4596 * ps + x0 y: 0.9463 * ps + y0 control2X: 0.4430 * ps + x0 y: 0.9830 * ps + y0;
		curveToX: 0.2756 * ps + x0 y: 1.0514 * ps + y0 control1X: 0.3766 * ps + x0 y: 1.0377 * ps + y0 control2X: 0.3319 * ps + x0 y: 1.0514 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 1.0441 * ps + y0 control1X: 0.2518 * ps + x0 y: 1.0514 * ps + y0 control2X: 0.2274 * ps + x0 y: 1.0490 * ps + y0;
		curveToX: 0.1218 * ps + x0 y: 1.0231 * ps + y0 control1X: 0.1773 * ps + x0 y: 1.0395 * ps + y0 control2X: 0.1504 * ps + x0 y: 1.0325 * ps + y0;
		lineToX: 0.1218 * ps + x0 y: 0.9430 * ps + y0;
		curveToX: 0.2004 * ps + x0 y: 0.9713 * ps + y0 control1X: 0.1501 * ps + x0 y: 0.9557 * ps + y0 control2X: 0.1763 * ps + x0 y: 0.9651 * ps + y0;
		curveToX: 0.2648 * ps + x0 y: 0.9811 * ps + y0 control1X: 0.2245 * ps + x0 y: 0.9778 * ps + y0 control2X: 0.2459 * ps + x0 y: 0.9811 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.9606 * ps + y0 control1X: 0.2980 * ps + x0 y: 0.9811 * ps + y0 control2X: 0.3237 * ps + x0 y: 0.9742 * ps + y0;
		curveToX: 0.3698 * ps + x0 y: 0.9030 * ps + y0 control1X: 0.3605 * ps + x0 y: 0.9469 * ps + y0 control2X: 0.3698 * ps + x0 y: 0.9277 * ps + y0;
		curveToX: 0.2619 * ps + x0 y: 0.7931 * ps + y0 control1X: 0.3698 * ps + x0 y: 0.8698 * ps + y0 control2X: 0.3338 * ps + x0 y: 0.8331 * ps + y0;
		lineToX: 0.2497 * ps + x0 y: 0.7863 * ps + y0;
		curveToX: 0.1139 * ps + x0 y: 0.6906 * ps + y0 control1X: 0.1800 * ps + x0 y: 0.7472 * ps + y0 control2X: 0.1348 * ps + x0 y: 0.7153 * ps + y0;
		curveToX: 0.0827 * ps + x0 y: 0.6032 * ps + y0 control1X: 0.0931 * ps + x0 y: 0.6655 * ps + y0 control2X: 0.0827 * ps + x0 y: 0.6364 * ps + y0;
		curveToX: 0.1051 * ps + x0 y: 0.5241 * ps + y0 control1X: 0.0827 * ps + x0 y: 0.5735 * ps + y0 control2X: 0.0902 * ps + x0 y: 0.5472 * ps + y0;
		curveToX: 0.1725 * ps + x0 y: 0.4650 * ps + y0 control1X: 0.1204 * ps + x0 y: 0.5006 * ps + y0 control2X: 0.1429 * ps + x0 y: 0.4809 * ps + y0;
		curveToX: 0.1291 * ps + x0 y: 0.4161 * ps + y0 control1X: 0.1533 * ps + x0 y: 0.4506 * ps + y0 control2X: 0.1388 * ps + x0 y: 0.4344 * ps + y0;
		curveToX: 0.1149 * ps + x0 y: 0.3551 * ps + y0 control1X: 0.1196 * ps + x0 y: 0.3979 * ps + y0 control2X: 0.1149 * ps + x0 y: 0.3776 * ps + y0;
		curveToX: 0.1637 * ps + x0 y: 0.2531 * ps + y0 control1X: 0.1149 * ps + x0 y: 0.3128 * ps + y0 control2X: 0.1312 * ps + x0 y: 0.2788 * ps + y0;
		curveToX: 0.2936 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.1963 * ps + x0 y: 0.2270 * ps + y0 control2X: 0.2396 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3664 * ps + x0 y: 0.2213 * ps + y0 control1X: 0.3174 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3416 * ps + x0 y: 0.2164 * ps + y0;
		curveToX: 0.4426 * ps + x0 y: 0.2423 * ps + y0 control1X: 0.3914 * ps + x0 y: 0.2259 * ps + y0 control2X: 0.4168 * ps + x0 y: 0.2329 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:54'!
drawVeraSans168pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4806 * ps + x0 y: 0.4254 * ps + y0;
		lineToX: 0.4806 * ps + x0 y: 0.5104 * ps + y0;
		curveToX: 0.4015 * ps + x0 y: 0.4811 * ps + y0 control1X: 0.4552 * ps + x0 y: 0.4974 * ps + y0 control2X: 0.4289 * ps + x0 y: 0.4876 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.3742 * ps + x0 y: 0.4746 * ps + y0 control2X: 0.3459 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2160 * ps + x0 y: 0.4918 * ps + y0 control1X: 0.2720 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2385 * ps + x0 y: 0.4782 * ps + y0;
		curveToX: 0.1828 * ps + x0 y: 0.5533 * ps + y0 control1X: 0.1939 * ps + x0 y: 0.5055 * ps + y0 control2X: 0.1828 * ps + x0 y: 0.5260 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.6027 * ps + y0 control1X: 0.1828 * ps + x0 y: 0.5742 * ps + y0 control2X: 0.1908 * ps + x0 y: 0.5906 * ps + y0;
		curveToX: 0.3029 * ps + x0 y: 0.6364 * ps + y0 control1X: 0.2227 * ps + x0 y: 0.6144 * ps + y0 control2X: 0.2547 * ps + x0 y: 0.6256 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.6432 * ps + y0;
		curveToX: 0.4694 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.3975 * ps + x0 y: 0.6569 * ps + y0 control2X: 0.4427 * ps + x0 y: 0.6762 * ps + y0;
		curveToX: 0.5099 * ps + x0 y: 0.8053 * ps + y0 control1X: 0.4964 * ps + x0 y: 0.7260 * ps + y0 control2X: 0.5099 * ps + x0 y: 0.7607 * ps + y0;
		curveToX: 0.4494 * ps + x0 y: 0.9259 * ps + y0 control1X: 0.5099 * ps + x0 y: 0.8561 * ps + y0 control2X: 0.4898 * ps + x0 y: 0.8963 * ps + y0;
		curveToX: 0.2839 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4093 * ps + x0 y: 0.9555 * ps + y0 control2X: 0.3542 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1921 * ps + x0 y: 0.9616 * ps + y0 control1X: 0.2546 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2240 * ps + x0 y: 0.9674 * ps + y0;
		curveToX: 0.0920 * ps + x0 y: 0.9362 * ps + y0 control1X: 0.1605 * ps + x0 y: 0.9560 * ps + y0 control2X: 0.1271 * ps + x0 y: 0.9476 * ps + y0;
		lineToX: 0.0920 * ps + x0 y: 0.8434 * ps + y0;
		curveToX: 0.1901 * ps + x0 y: 0.8825 * ps + y0 control1X: 0.1252 * ps + x0 y: 0.8606 * ps + y0 control2X: 0.1579 * ps + x0 y: 0.8737 * ps + y0;
		curveToX: 0.2858 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2223 * ps + x0 y: 0.8909 * ps + y0 control2X: 0.2542 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.3835 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.3281 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3607 * ps + x0 y: 0.8880 * ps + y0;
		curveToX: 0.4176 * ps + x0 y: 0.8121 * ps + y0 control1X: 0.4063 * ps + x0 y: 0.8590 * ps + y0 control2X: 0.4176 * ps + x0 y: 0.8385 * ps + y0;
		curveToX: 0.3927 * ps + x0 y: 0.7560 * ps + y0 control1X: 0.4176 * ps + x0 y: 0.7877 * ps + y0 control2X: 0.4093 * ps + x0 y: 0.7690 * ps + y0;
		curveToX: 0.2848 * ps + x0 y: 0.7184 * ps + y0 control1X: 0.3765 * ps + x0 y: 0.7430 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.7304 * ps + y0;
		lineToX: 0.2536 * ps + x0 y: 0.7111 * ps + y0;
		curveToX: 0.1330 * ps + x0 y: 0.6574 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6993 * ps + y0 control2X: 0.1577 * ps + x0 y: 0.6814 * ps + y0;
		curveToX: 0.0959 * ps + x0 y: 0.5573 * ps + y0 control1X: 0.1082 * ps + x0 y: 0.6329 * ps + y0 control2X: 0.0959 * ps + x0 y: 0.5996 * ps + y0;
		curveToX: 0.1506 * ps + x0 y: 0.4381 * ps + y0 control1X: 0.0959 * ps + x0 y: 0.5058 * ps + y0 control2X: 0.1141 * ps + x0 y: 0.4661 * ps + y0;
		curveToX: 0.3058 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.1870 * ps + x0 y: 0.4101 * ps + y0 control2X: 0.2388 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.3996 * ps + x0 y: 0.4034 * ps + y0 control1X: 0.3390 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.3703 * ps + x0 y: 0.3986 * ps + y0;
		curveToX: 0.4806 * ps + x0 y: 0.4254 * ps + y0 control1X: 0.4289 * ps + x0 y: 0.4083 * ps + y0 control2X: 0.4559 * ps + x0 y: 0.4157 * ps + y0;
		moveToX: 0.2629 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1432 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2111 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2990 * ps + x0 y: 0.2760 * ps + y0;
		lineToX: 0.3869 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4548 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3351 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2629 * ps + x0 y: 0.3400 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:55'!
drawVeraSans169pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5378 * ps + x0 y: 0.2311 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.2579 * ps + y0 control1X: 0.5872 * ps + x0 y: 0.2311 * ps + y0 control2X: 0.6333 * ps + x0 y: 0.2400 * ps + y0;
		curveToX: 0.7936 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.7189 * ps + x0 y: 0.2758 * ps + y0 control2X: 0.7581 * ps + x0 y: 0.3025 * ps + y0;
		curveToX: 0.8732 * ps + x0 y: 0.4552 * ps + y0 control1X: 0.8291 * ps + x0 y: 0.3735 * ps + y0 control2X: 0.8556 * ps + x0 y: 0.4126 * ps + y0;
		curveToX: 0.8996 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.8908 * ps + x0 y: 0.4978 * ps + y0 control2X: 0.8996 * ps + x0 y: 0.5442 * ps + y0;
		curveToX: 0.8732 * ps + x0 y: 0.7325 * ps + y0 control1X: 0.8996 * ps + x0 y: 0.6438 * ps + y0 control2X: 0.8908 * ps + x0 y: 0.6899 * ps + y0;
		curveToX: 0.7936 * ps + x0 y: 0.8492 * ps + y0 control1X: 0.8556 * ps + x0 y: 0.7749 * ps + y0 control2X: 0.8291 * ps + x0 y: 0.8138 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.9293 * ps + y0 control1X: 0.7581 * ps + x0 y: 0.8847 * ps + y0 control2X: 0.7189 * ps + x0 y: 0.9114 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.6333 * ps + x0 y: 0.9472 * ps + y0 control2X: 0.5872 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.3991 * ps + x0 y: 0.9293 * ps + y0 control1X: 0.4883 * ps + x0 y: 0.9562 * ps + y0 control2X: 0.4421 * ps + x0 y: 0.9472 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.8492 * ps + y0 control1X: 0.3565 * ps + x0 y: 0.9114 * ps + y0 control2X: 0.3174 * ps + x0 y: 0.8847 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 0.7325 * ps + y0 control1X: 0.2464 * ps + x0 y: 0.8138 * ps + y0 control2X: 0.2199 * ps + x0 y: 0.7749 * ps + y0;
		curveToX: 0.1760 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.1847 * ps + x0 y: 0.6899 * ps + y0 control2X: 0.1760 * ps + x0 y: 0.6438 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 0.4552 * ps + y0 control1X: 0.1760 * ps + x0 y: 0.5442 * ps + y0 control2X: 0.1847 * ps + x0 y: 0.4978 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.2199 * ps + x0 y: 0.4126 * ps + y0 control2X: 0.2464 * ps + x0 y: 0.3735 * ps + y0;
		curveToX: 0.3991 * ps + x0 y: 0.2579 * ps + y0 control1X: 0.3174 * ps + x0 y: 0.3025 * ps + y0 control2X: 0.3565 * ps + x0 y: 0.2758 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.2311 * ps + y0 control1X: 0.4421 * ps + x0 y: 0.2400 * ps + y0 control2X: 0.4883 * ps + x0 y: 0.2311 * ps + y0;
		moveToX: 0.5378 * ps + x0 y: 0.2814 * ps + y0;
		curveToX: 0.4186 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.4951 * ps + x0 y: 0.2814 * ps + y0 control2X: 0.4554 * ps + x0 y: 0.2890 * ps + y0;
		curveToX: 0.3176 * ps + x0 y: 0.3732 * ps + y0 control1X: 0.3818 * ps + x0 y: 0.3196 * ps + y0 control2X: 0.3482 * ps + x0 y: 0.3426 * ps + y0;
		curveToX: 0.2482 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.2870 * ps + x0 y: 0.4038 * ps + y0 control2X: 0.2638 * ps + x0 y: 0.4378 * ps + y0;
		curveToX: 0.2248 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.2326 * ps + x0 y: 0.5123 * ps + y0 control2X: 0.2248 * ps + x0 y: 0.5520 * ps + y0;
		curveToX: 0.2482 * ps + x0 y: 0.7130 * ps + y0 control1X: 0.2248 * ps + x0 y: 0.6364 * ps + y0 control2X: 0.2326 * ps + x0 y: 0.6759 * ps + y0;
		curveToX: 0.3176 * ps + x0 y: 0.8141 * ps + y0 control1X: 0.2638 * ps + x0 y: 0.7498 * ps + y0 control2X: 0.2870 * ps + x0 y: 0.7835 * ps + y0;
		curveToX: 0.4186 * ps + x0 y: 0.8834 * ps + y0 control1X: 0.3482 * ps + x0 y: 0.8447 * ps + y0 control2X: 0.3818 * ps + x0 y: 0.8678 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.9064 * ps + y0 control1X: 0.4554 * ps + x0 y: 0.8987 * ps + y0 control2X: 0.4951 * ps + x0 y: 0.9064 * ps + y0;
		curveToX: 0.6574 * ps + x0 y: 0.8834 * ps + y0 control1X: 0.5807 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.6206 * ps + x0 y: 0.8987 * ps + y0;
		curveToX: 0.7590 * ps + x0 y: 0.8141 * ps + y0 control1X: 0.6945 * ps + x0 y: 0.8678 * ps + y0 control2X: 0.7284 * ps + x0 y: 0.8447 * ps + y0;
		curveToX: 0.8268 * ps + x0 y: 0.7135 * ps + y0 control1X: 0.7892 * ps + x0 y: 0.7838 * ps + y0 control2X: 0.8119 * ps + x0 y: 0.7503 * ps + y0;
		curveToX: 0.8498 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.8421 * ps + x0 y: 0.6767 * ps + y0 control2X: 0.8498 * ps + x0 y: 0.6370 * ps + y0;
		curveToX: 0.8268 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.8498 * ps + x0 y: 0.5511 * ps + y0 control2X: 0.8421 * ps + x0 y: 0.5110 * ps + y0;
		curveToX: 0.7590 * ps + x0 y: 0.3732 * ps + y0 control1X: 0.8115 * ps + x0 y: 0.4371 * ps + y0 control2X: 0.7889 * ps + x0 y: 0.4034 * ps + y0;
		curveToX: 0.6574 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.7284 * ps + x0 y: 0.3426 * ps + y0 control2X: 0.6945 * ps + x0 y: 0.3196 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.2814 * ps + y0 control1X: 0.6206 * ps + x0 y: 0.2890 * ps + y0 control2X: 0.5807 * ps + x0 y: 0.2814 * ps + y0;
		moveToX: 0.6838 * ps + x0 y: 0.4010 * ps + y0;
		lineToX: 0.6838 * ps + x0 y: 0.4640 * ps + y0;
		curveToX: 0.6198 * ps + x0 y: 0.4401 * ps + y0 control1X: 0.6623 * ps + x0 y: 0.4533 * ps + y0 control2X: 0.6410 * ps + x0 y: 0.4453 * ps + y0;
		curveToX: 0.5558 * ps + x0 y: 0.4323 * ps + y0 control1X: 0.5986 * ps + x0 y: 0.4349 * ps + y0 control2X: 0.5773 * ps + x0 y: 0.4323 * ps + y0;
		curveToX: 0.4421 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.5073 * ps + x0 y: 0.4323 * ps + y0 control2X: 0.4694 * ps + x0 y: 0.4466 * ps + y0;
		curveToX: 0.4015 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.4150 * ps + x0 y: 0.5035 * ps + y0 control2X: 0.4015 * ps + x0 y: 0.5433 * ps + y0;
		curveToX: 0.4430 * ps + x0 y: 0.7145 * ps + y0 control1X: 0.4015 * ps + x0 y: 0.6461 * ps + y0 control2X: 0.4154 * ps + x0 y: 0.6862 * ps + y0;
		curveToX: 0.5607 * ps + x0 y: 0.7570 * ps + y0 control1X: 0.4710 * ps + x0 y: 0.7428 * ps + y0 control2X: 0.5103 * ps + x0 y: 0.7570 * ps + y0;
		curveToX: 0.6218 * ps + x0 y: 0.7496 * ps + y0 control1X: 0.5815 * ps + x0 y: 0.7570 * ps + y0 control2X: 0.6019 * ps + x0 y: 0.7545 * ps + y0;
		curveToX: 0.6838 * ps + x0 y: 0.7252 * ps + y0 control1X: 0.6416 * ps + x0 y: 0.7444 * ps + y0 control2X: 0.6623 * ps + x0 y: 0.7363 * ps + y0;
		lineToX: 0.6838 * ps + x0 y: 0.7872 * ps + y0;
		curveToX: 0.6174 * ps + x0 y: 0.8082 * ps + y0 control1X: 0.6620 * ps + x0 y: 0.7967 * ps + y0 control2X: 0.6398 * ps + x0 y: 0.8037 * ps + y0;
		curveToX: 0.5500 * ps + x0 y: 0.8151 * ps + y0 control1X: 0.5952 * ps + x0 y: 0.8128 * ps + y0 control2X: 0.5728 * ps + x0 y: 0.8151 * ps + y0;
		curveToX: 0.3830 * ps + x0 y: 0.7555 * ps + y0 control1X: 0.4797 * ps + x0 y: 0.8151 * ps + y0 control2X: 0.4240 * ps + x0 y: 0.7952 * ps + y0;
		curveToX: 0.3219 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.3423 * ps + x0 y: 0.7158 * ps + y0 control2X: 0.3219 * ps + x0 y: 0.6621 * ps + y0;
		curveToX: 0.3830 * ps + x0 y: 0.4332 * ps + y0 control1X: 0.3219 * ps + x0 y: 0.5263 * ps + y0 control2X: 0.3423 * ps + x0 y: 0.4726 * ps + y0;
		curveToX: 0.5500 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.4240 * ps + x0 y: 0.3938 * ps + y0 control2X: 0.4797 * ps + x0 y: 0.3742 * ps + y0;
		curveToX: 0.6188 * ps + x0 y: 0.3810 * ps + y0 control1X: 0.5737 * ps + x0 y: 0.3742 * ps + y0 control2X: 0.5967 * ps + x0 y: 0.3764 * ps + y0;
		curveToX: 0.6838 * ps + x0 y: 0.4010 * ps + y0 control1X: 0.6410 * ps + x0 y: 0.3855 * ps + y0 control2X: 0.6626 * ps + x0 y: 0.3922 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:56'!
drawVeraSans170pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1056 * ps + x0 y: 0.6671 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.6671 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.7272 * ps + y0;
		lineToX: 0.1056 * ps + x0 y: 0.7272 * ps + y0;
		lineToX: 0.1056 * ps + x0 y: 0.6671 * ps + y0;
		moveToX: 0.4406 * ps + x0 y: 0.3873 * ps + y0;
		lineToX: 0.4406 * ps + x0 y: 0.6061 * ps + y0;
		lineToX: 0.3678 * ps + x0 y: 0.6061 * ps + y0;
		lineToX: 0.3678 * ps + x0 y: 0.5514 * ps + y0;
		curveToX: 0.3112 * ps + x0 y: 0.5988 * ps + y0 control1X: 0.3535 * ps + x0 y: 0.5719 * ps + y0 control2X: 0.3346 * ps + x0 y: 0.5877 * ps + y0;
		curveToX: 0.2306 * ps + x0 y: 0.6154 * ps + y0 control1X: 0.2878 * ps + x0 y: 0.6098 * ps + y0 control2X: 0.2609 * ps + x0 y: 0.6154 * ps + y0;
		curveToX: 0.1310 * ps + x0 y: 0.5826 * ps + y0 control1X: 0.1890 * ps + x0 y: 0.6154 * ps + y0 control2X: 0.1558 * ps + x0 y: 0.6045 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.4943 * ps + y0 control1X: 0.1063 * ps + x0 y: 0.5605 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.5311 * ps + y0;
		curveToX: 0.1403 * ps + x0 y: 0.3947 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4503 * ps + y0 control2X: 0.1094 * ps + x0 y: 0.4171 * ps + y0;
		curveToX: 0.2790 * ps + x0 y: 0.3610 * ps + y0 control1X: 0.1716 * ps + x0 y: 0.3722 * ps + y0 control2X: 0.2178 * ps + x0 y: 0.3610 * ps + y0;
		lineToX: 0.3678 * ps + x0 y: 0.3610 * ps + y0;
		lineToX: 0.3678 * ps + x0 y: 0.3590 * ps + y0;
		curveToX: 0.3390 * ps + x0 y: 0.2926 * ps + y0 control1X: 0.3678 * ps + x0 y: 0.3294 * ps + y0 control2X: 0.3582 * ps + x0 y: 0.3073 * ps + y0;
		curveToX: 0.2536 * ps + x0 y: 0.2701 * ps + y0 control1X: 0.3202 * ps + x0 y: 0.2776 * ps + y0 control2X: 0.2917 * ps + x0 y: 0.2701 * ps + y0;
		curveToX: 0.1901 * ps + x0 y: 0.2784 * ps + y0 control1X: 0.2334 * ps + x0 y: 0.2701 * ps + y0 control2X: 0.2122 * ps + x0 y: 0.2729 * ps + y0;
		curveToX: 0.1237 * ps + x0 y: 0.3033 * ps + y0 control1X: 0.1680 * ps + x0 y: 0.2840 * ps + y0 control2X: 0.1458 * ps + x0 y: 0.2923 * ps + y0;
		lineToX: 0.1237 * ps + x0 y: 0.2413 * ps + y0;
		curveToX: 0.1945 * ps + x0 y: 0.2208 * ps + y0 control1X: 0.1475 * ps + x0 y: 0.2322 * ps + y0 control2X: 0.1711 * ps + x0 y: 0.2254 * ps + y0;
		curveToX: 0.2638 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2183 * ps + x0 y: 0.2163 * ps + y0 control2X: 0.2414 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3967 * ps + x0 y: 0.2570 * ps + y0 control1X: 0.3234 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3677 * ps + x0 y: 0.2283 * ps + y0;
		curveToX: 0.4406 * ps + x0 y: 0.3873 * ps + y0 control1X: 0.4260 * ps + x0 y: 0.2856 * ps + y0 control2X: 0.4406 * ps + x0 y: 0.3291 * ps + y0;
		moveToX: 0.3058 * ps + x0 y: 0.4171 * ps + y0;
		curveToX: 0.1965 * ps + x0 y: 0.4337 * ps + y0 control1X: 0.2534 * ps + x0 y: 0.4171 * ps + y0 control2X: 0.2170 * ps + x0 y: 0.4227 * ps + y0;
		curveToX: 0.1657 * ps + x0 y: 0.4884 * ps + y0 control1X: 0.1760 * ps + x0 y: 0.4445 * ps + y0 control2X: 0.1657 * ps + x0 y: 0.4627 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.5387 * ps + y0 control1X: 0.1657 * ps + x0 y: 0.5092 * ps + y0 control2X: 0.1737 * ps + x0 y: 0.5260 * ps + y0;
		curveToX: 0.2536 * ps + x0 y: 0.5573 * ps + y0 control1X: 0.2056 * ps + x0 y: 0.5511 * ps + y0 control2X: 0.2269 * ps + x0 y: 0.5573 * ps + y0;
		curveToX: 0.3361 * ps + x0 y: 0.5221 * ps + y0 control1X: 0.2874 * ps + x0 y: 0.5573 * ps + y0 control2X: 0.3149 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.4313 * ps + y0 control1X: 0.3573 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.3678 * ps + x0 y: 0.4684 * ps + y0;
		lineToX: 0.3678 * ps + x0 y: 0.4171 * ps + y0;
		lineToX: 0.3058 * ps + x0 y: 0.4171 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:56'!
drawVeraSans171pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5558 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.5558 * ps + x0 y: 0.5324 * ps + y0;
		lineToX: 0.4089 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.5558 * ps + x0 y: 0.7941 * ps + y0;
		lineToX: 0.5558 * ps + x0 y: 0.8873 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.5558 * ps + x0 y: 0.4391 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.5324 * ps + y0;
		lineToX: 0.1969 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.7941 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.8873 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.4391 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:56'!
drawVeraSans172pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1437 * ps + x0 y: 0.5353 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.5353 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.8160 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.8160 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.6183 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6183 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.5353 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:56'!
drawVeraSans173pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0866 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.7223 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.7223 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6422 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:57'!
drawVeraSans174pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5378 * ps + x0 y: 0.2814 * ps + y0;
		curveToX: 0.4186 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.4951 * ps + x0 y: 0.2814 * ps + y0 control2X: 0.4554 * ps + x0 y: 0.2890 * ps + y0;
		curveToX: 0.3176 * ps + x0 y: 0.3732 * ps + y0 control1X: 0.3818 * ps + x0 y: 0.3196 * ps + y0 control2X: 0.3482 * ps + x0 y: 0.3426 * ps + y0;
		curveToX: 0.2482 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.2870 * ps + x0 y: 0.4038 * ps + y0 control2X: 0.2638 * ps + x0 y: 0.4378 * ps + y0;
		curveToX: 0.2248 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.2326 * ps + x0 y: 0.5123 * ps + y0 control2X: 0.2248 * ps + x0 y: 0.5520 * ps + y0;
		curveToX: 0.2482 * ps + x0 y: 0.7130 * ps + y0 control1X: 0.2248 * ps + x0 y: 0.6364 * ps + y0 control2X: 0.2326 * ps + x0 y: 0.6759 * ps + y0;
		curveToX: 0.3176 * ps + x0 y: 0.8141 * ps + y0 control1X: 0.2638 * ps + x0 y: 0.7498 * ps + y0 control2X: 0.2870 * ps + x0 y: 0.7835 * ps + y0;
		curveToX: 0.4186 * ps + x0 y: 0.8834 * ps + y0 control1X: 0.3482 * ps + x0 y: 0.8447 * ps + y0 control2X: 0.3818 * ps + x0 y: 0.8678 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.9064 * ps + y0 control1X: 0.4554 * ps + x0 y: 0.8987 * ps + y0 control2X: 0.4951 * ps + x0 y: 0.9064 * ps + y0;
		curveToX: 0.6574 * ps + x0 y: 0.8834 * ps + y0 control1X: 0.5807 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.6206 * ps + x0 y: 0.8987 * ps + y0;
		curveToX: 0.7590 * ps + x0 y: 0.8141 * ps + y0 control1X: 0.6945 * ps + x0 y: 0.8678 * ps + y0 control2X: 0.7284 * ps + x0 y: 0.8447 * ps + y0;
		curveToX: 0.8268 * ps + x0 y: 0.7135 * ps + y0 control1X: 0.7892 * ps + x0 y: 0.7838 * ps + y0 control2X: 0.8119 * ps + x0 y: 0.7503 * ps + y0;
		curveToX: 0.8498 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.8421 * ps + x0 y: 0.6767 * ps + y0 control2X: 0.8498 * ps + x0 y: 0.6370 * ps + y0;
		curveToX: 0.8268 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.8498 * ps + x0 y: 0.5511 * ps + y0 control2X: 0.8421 * ps + x0 y: 0.5110 * ps + y0;
		curveToX: 0.7590 * ps + x0 y: 0.3732 * ps + y0 control1X: 0.8115 * ps + x0 y: 0.4371 * ps + y0 control2X: 0.7889 * ps + x0 y: 0.4034 * ps + y0;
		curveToX: 0.6574 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.7284 * ps + x0 y: 0.3426 * ps + y0 control2X: 0.6945 * ps + x0 y: 0.3196 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.2814 * ps + y0 control1X: 0.6206 * ps + x0 y: 0.2890 * ps + y0 control2X: 0.5807 * ps + x0 y: 0.2814 * ps + y0;
		moveToX: 0.5378 * ps + x0 y: 0.2311 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.2579 * ps + y0 control1X: 0.5872 * ps + x0 y: 0.2311 * ps + y0 control2X: 0.6333 * ps + x0 y: 0.2400 * ps + y0;
		curveToX: 0.7936 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.7189 * ps + x0 y: 0.2758 * ps + y0 control2X: 0.7581 * ps + x0 y: 0.3025 * ps + y0;
		curveToX: 0.8732 * ps + x0 y: 0.4552 * ps + y0 control1X: 0.8291 * ps + x0 y: 0.3735 * ps + y0 control2X: 0.8556 * ps + x0 y: 0.4126 * ps + y0;
		curveToX: 0.8996 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.8908 * ps + x0 y: 0.4978 * ps + y0 control2X: 0.8996 * ps + x0 y: 0.5442 * ps + y0;
		curveToX: 0.8732 * ps + x0 y: 0.7325 * ps + y0 control1X: 0.8996 * ps + x0 y: 0.6438 * ps + y0 control2X: 0.8908 * ps + x0 y: 0.6899 * ps + y0;
		curveToX: 0.7936 * ps + x0 y: 0.8492 * ps + y0 control1X: 0.8556 * ps + x0 y: 0.7749 * ps + y0 control2X: 0.8291 * ps + x0 y: 0.8138 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.9293 * ps + y0 control1X: 0.7581 * ps + x0 y: 0.8847 * ps + y0 control2X: 0.7189 * ps + x0 y: 0.9114 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.6333 * ps + x0 y: 0.9472 * ps + y0 control2X: 0.5872 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.3991 * ps + x0 y: 0.9293 * ps + y0 control1X: 0.4883 * ps + x0 y: 0.9562 * ps + y0 control2X: 0.4421 * ps + x0 y: 0.9472 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.8492 * ps + y0 control1X: 0.3565 * ps + x0 y: 0.9114 * ps + y0 control2X: 0.3174 * ps + x0 y: 0.8847 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 0.7325 * ps + y0 control1X: 0.2464 * ps + x0 y: 0.8138 * ps + y0 control2X: 0.2199 * ps + x0 y: 0.7749 * ps + y0;
		curveToX: 0.1760 * ps + x0 y: 0.5944 * ps + y0 control1X: 0.1847 * ps + x0 y: 0.6899 * ps + y0 control2X: 0.1760 * ps + x0 y: 0.6438 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 0.4552 * ps + y0 control1X: 0.1760 * ps + x0 y: 0.5442 * ps + y0 control2X: 0.1847 * ps + x0 y: 0.4978 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.2199 * ps + x0 y: 0.4126 * ps + y0 control2X: 0.2464 * ps + x0 y: 0.3735 * ps + y0;
		curveToX: 0.3991 * ps + x0 y: 0.2579 * ps + y0 control1X: 0.3174 * ps + x0 y: 0.3025 * ps + y0 control2X: 0.3565 * ps + x0 y: 0.2758 * ps + y0;
		curveToX: 0.5378 * ps + x0 y: 0.2311 * ps + y0 control1X: 0.4421 * ps + x0 y: 0.2400 * ps + y0 control2X: 0.4883 * ps + x0 y: 0.2311 * ps + y0;
		moveToX: 0.5246 * ps + x0 y: 0.4332 * ps + y0;
		lineToX: 0.4645 * ps + x0 y: 0.4332 * ps + y0;
		lineToX: 0.4645 * ps + x0 y: 0.5680 * ps + y0;
		lineToX: 0.5246 * ps + x0 y: 0.5680 * ps + y0;
		curveToX: 0.5993 * ps + x0 y: 0.5529 * ps + y0 control1X: 0.5604 * ps + x0 y: 0.5680 * ps + y0 control2X: 0.5853 * ps + x0 y: 0.5630 * ps + y0;
		curveToX: 0.6208 * ps + x0 y: 0.5011 * ps + y0 control1X: 0.6136 * ps + x0 y: 0.5428 * ps + y0 control2X: 0.6208 * ps + x0 y: 0.5255 * ps + y0;
		curveToX: 0.5993 * ps + x0 y: 0.4489 * ps + y0 control1X: 0.6208 * ps + x0 y: 0.4764 * ps + y0 control2X: 0.6136 * ps + x0 y: 0.4589 * ps + y0;
		curveToX: 0.5246 * ps + x0 y: 0.4332 * ps + y0 control1X: 0.5850 * ps + x0 y: 0.4384 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.4332 * ps + y0;
		moveToX: 0.5280 * ps + x0 y: 0.3829 * ps + y0;
		curveToX: 0.6564 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.5853 * ps + x0 y: 0.3829 * ps + y0 control2X: 0.6281 * ps + x0 y: 0.3927 * ps + y0;
		curveToX: 0.6989 * ps + x0 y: 0.5001 * ps + y0 control1X: 0.6847 * ps + x0 y: 0.4314 * ps + y0 control2X: 0.6989 * ps + x0 y: 0.4607 * ps + y0;
		curveToX: 0.6730 * ps + x0 y: 0.5695 * ps + y0 control1X: 0.6989 * ps + x0 y: 0.5281 * ps + y0 control2X: 0.6903 * ps + x0 y: 0.5512 * ps + y0;
		curveToX: 0.6008 * ps + x0 y: 0.6051 * ps + y0 control1X: 0.6561 * ps + x0 y: 0.5877 * ps + y0 control2X: 0.6320 * ps + x0 y: 0.5996 * ps + y0;
		curveToX: 0.6286 * ps + x0 y: 0.6281 * ps + y0 control1X: 0.6086 * ps + x0 y: 0.6077 * ps + y0 control2X: 0.6178 * ps + x0 y: 0.6154 * ps + y0;
		curveToX: 0.6676 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.6397 * ps + x0 y: 0.6404 * ps + y0 control2X: 0.6527 * ps + x0 y: 0.6585 * ps + y0;
		lineToX: 0.7345 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.6506 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.5876 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.5407 * ps + x0 y: 0.6300 * ps + y0 control1X: 0.5684 * ps + x0 y: 0.6578 * ps + y0 control2X: 0.5527 * ps + x0 y: 0.6381 * ps + y0;
		curveToX: 0.4968 * ps + x0 y: 0.6173 * ps + y0 control1X: 0.5290 * ps + x0 y: 0.6215 * ps + y0 control2X: 0.5143 * ps + x0 y: 0.6173 * ps + y0;
		lineToX: 0.4645 * ps + x0 y: 0.6173 * ps + y0;
		lineToX: 0.4645 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.3888 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.3888 * ps + x0 y: 0.3829 * ps + y0;
		lineToX: 0.5280 * ps + x0 y: 0.3829 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:58'!
drawVeraSans175pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1418 * ps + x0 y: 0.2111 * ps + y0;
		lineToX: 0.4338 * ps + x0 y: 0.2111 * ps + y0;
		lineToX: 0.4338 * ps + x0 y: 0.2833 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.2833 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.2111 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:58'!
drawVeraSans176pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2878 * ps + x0 y: 0.2770 * ps + y0;
		curveToX: 0.2218 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.2617 * ps + x0 y: 0.2770 * ps + y0 control2X: 0.2398 * ps + x0 y: 0.2861 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.3702 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.3222 * ps + y0 control2X: 0.1950 * ps + x0 y: 0.3442 * ps + y0;
		curveToX: 0.2218 * ps + x0 y: 0.4357 * ps + y0 control1X: 0.1950 * ps + x0 y: 0.3960 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.4178 * ps + y0;
		curveToX: 0.2878 * ps + x0 y: 0.4620 * ps + y0 control1X: 0.2398 * ps + x0 y: 0.4533 * ps + y0 control2X: 0.2617 * ps + x0 y: 0.4620 * ps + y0;
		curveToX: 0.3537 * ps + x0 y: 0.4357 * ps + y0 control1X: 0.3138 * ps + x0 y: 0.4620 * ps + y0 control2X: 0.3358 * ps + x0 y: 0.4533 * ps + y0;
		curveToX: 0.3805 * ps + x0 y: 0.3702 * ps + y0 control1X: 0.3716 * ps + x0 y: 0.4178 * ps + y0 control2X: 0.3805 * ps + x0 y: 0.3960 * ps + y0;
		curveToX: 0.3532 * ps + x0 y: 0.3043 * ps + y0 control1X: 0.3805 * ps + x0 y: 0.3445 * ps + y0 control2X: 0.3714 * ps + x0 y: 0.3226 * ps + y0;
		curveToX: 0.2878 * ps + x0 y: 0.2770 * ps + y0 control1X: 0.3353 * ps + x0 y: 0.2861 * ps + y0 control2X: 0.3135 * ps + x0 y: 0.2770 * ps + y0;
		moveToX: 0.2878 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3478 * ps + x0 y: 0.2262 * ps + y0 control1X: 0.3086 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3286 * ps + x0 y: 0.2181 * ps + y0;
		curveToX: 0.3976 * ps + x0 y: 0.2604 * ps + y0 control1X: 0.3670 * ps + x0 y: 0.2340 * ps + y0 control2X: 0.3836 * ps + x0 y: 0.2454 * ps + y0;
		curveToX: 0.4313 * ps + x0 y: 0.3102 * ps + y0 control1X: 0.4126 * ps + x0 y: 0.2750 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.2916 * ps + y0;
		curveToX: 0.4426 * ps + x0 y: 0.3702 * ps + y0 control1X: 0.4388 * ps + x0 y: 0.3287 * ps + y0 control2X: 0.4426 * ps + x0 y: 0.3488 * ps + y0;
		curveToX: 0.3971 * ps + x0 y: 0.4796 * ps + y0 control1X: 0.4426 * ps + x0 y: 0.4132 * ps + y0 control2X: 0.4274 * ps + x0 y: 0.4497 * ps + y0;
		curveToX: 0.2868 * ps + x0 y: 0.5241 * ps + y0 control1X: 0.3672 * ps + x0 y: 0.5092 * ps + y0 control2X: 0.3304 * ps + x0 y: 0.5241 * ps + y0;
		curveToX: 0.1769 * ps + x0 y: 0.4801 * ps + y0 control1X: 0.2428 * ps + x0 y: 0.5241 * ps + y0 control2X: 0.2062 * ps + x0 y: 0.5094 * ps + y0;
		curveToX: 0.1330 * ps + x0 y: 0.3702 * ps + y0 control1X: 0.1476 * ps + x0 y: 0.4508 * ps + y0 control2X: 0.1330 * ps + x0 y: 0.4142 * ps + y0;
		curveToX: 0.1779 * ps + x0 y: 0.2594 * ps + y0 control1X: 0.1330 * ps + x0 y: 0.3266 * ps + y0 control2X: 0.1480 * ps + x0 y: 0.2897 * ps + y0;
		curveToX: 0.2878 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2079 * ps + x0 y: 0.2291 * ps + y0 control2X: 0.2445 * ps + x0 y: 0.2140 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:59'!
drawVeraSans177pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4977 * ps + x0 y: 0.3292 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.5182 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.5182 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.7902 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.7902 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.5182 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.5182 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.3292 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.3292 * ps + y0;
		moveToX: 0.1437 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.8732 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/8/2012 23:59'!
drawVeraSans178pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1686 * ps + x0 y: 0.5743 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.5743 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.6300 * ps + y0;
		lineToX: 0.0837 * ps + x0 y: 0.6300 * ps + y0;
		lineToX: 0.0837 * ps + x0 y: 0.5763 * ps + y0;
		curveToX: 0.1310 * ps + x0 y: 0.5338 * ps + y0 control1X: 0.0947 * ps + x0 y: 0.5662 * ps + y0 control2X: 0.1105 * ps + x0 y: 0.5520 * ps + y0;
		curveToX: 0.2990 * ps + x0 y: 0.3390 * ps + y0 control1X: 0.2430 * ps + x0 y: 0.4345 * ps + y0 control2X: 0.2990 * ps + x0 y: 0.3696 * ps + y0;
		curveToX: 0.2736 * ps + x0 y: 0.2872 * ps + y0 control1X: 0.2990 * ps + x0 y: 0.3175 * ps + y0 control2X: 0.2905 * ps + x0 y: 0.3003 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.2672 * ps + y0 control1X: 0.2567 * ps + x0 y: 0.2739 * ps + y0 control2X: 0.2344 * ps + x0 y: 0.2672 * ps + y0;
		curveToX: 0.1515 * ps + x0 y: 0.2760 * ps + y0 control1X: 0.1898 * ps + x0 y: 0.2672 * ps + y0 control2X: 0.1714 * ps + x0 y: 0.2701 * ps + y0;
		curveToX: 0.0866 * ps + x0 y: 0.3014 * ps + y0 control1X: 0.1317 * ps + x0 y: 0.2815 * ps + y0 control2X: 0.1100 * ps + x0 y: 0.2900 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.2413 * ps + y0;
		curveToX: 0.1564 * ps + x0 y: 0.2208 * ps + y0 control1X: 0.1117 * ps + x0 y: 0.2322 * ps + y0 control2X: 0.1349 * ps + x0 y: 0.2254 * ps + y0;
		curveToX: 0.2170 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.1782 * ps + x0 y: 0.2163 * ps + y0 control2X: 0.1984 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3302 * ps + x0 y: 0.2462 * ps + y0 control1X: 0.2642 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3019 * ps + x0 y: 0.2247 * ps + y0;
		curveToX: 0.3727 * ps + x0 y: 0.3312 * ps + y0 control1X: 0.3586 * ps + x0 y: 0.2677 * ps + y0 control2X: 0.3727 * ps + x0 y: 0.2960 * ps + y0;
		curveToX: 0.2111 * ps + x0 y: 0.5377 * ps + y0 control1X: 0.3727 * ps + x0 y: 0.3764 * ps + y0 control2X: 0.3189 * ps + x0 y: 0.4453 * ps + y0;
		curveToX: 0.1686 * ps + x0 y: 0.5743 * ps + y0 control1X: 0.1929 * ps + x0 y: 0.5533 * ps + y0 control2X: 0.1787 * ps + x0 y: 0.5656 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:00'!
drawVeraSans179pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2936 * ps + x0 y: 0.4093 * ps + y0;
		curveToX: 0.3630 * ps + x0 y: 0.4450 * ps + y0 control1X: 0.3236 * ps + x0 y: 0.4152 * ps + y0 control2X: 0.3467 * ps + x0 y: 0.4270 * ps + y0;
		curveToX: 0.3879 * ps + x0 y: 0.5114 * ps + y0 control1X: 0.3796 * ps + x0 y: 0.4625 * ps + y0 control2X: 0.3879 * ps + x0 y: 0.4847 * ps + y0;
		curveToX: 0.3415 * ps + x0 y: 0.6046 * ps + y0 control1X: 0.3879 * ps + x0 y: 0.5517 * ps + y0 control2X: 0.3724 * ps + x0 y: 0.5828 * ps + y0;
		curveToX: 0.2087 * ps + x0 y: 0.6373 * ps + y0 control1X: 0.3106 * ps + x0 y: 0.6264 * ps + y0 control2X: 0.2663 * ps + x0 y: 0.6373 * ps + y0;
		curveToX: 0.1501 * ps + x0 y: 0.6325 * ps + y0 control1X: 0.1901 * ps + x0 y: 0.6373 * ps + y0 control2X: 0.1706 * ps + x0 y: 0.6357 * ps + y0;
		curveToX: 0.0856 * ps + x0 y: 0.6173 * ps + y0 control1X: 0.1299 * ps + x0 y: 0.6289 * ps + y0 control2X: 0.1084 * ps + x0 y: 0.6238 * ps + y0;
		lineToX: 0.0856 * ps + x0 y: 0.5582 * ps + y0;
		curveToX: 0.1398 * ps + x0 y: 0.5778 * ps + y0 control1X: 0.1025 * ps + x0 y: 0.5670 * ps + y0 control2X: 0.1206 * ps + x0 y: 0.5735 * ps + y0;
		curveToX: 0.2018 * ps + x0 y: 0.5841 * ps + y0 control1X: 0.1593 * ps + x0 y: 0.5820 * ps + y0 control2X: 0.1800 * ps + x0 y: 0.5841 * ps + y0;
		curveToX: 0.2843 * ps + x0 y: 0.5651 * ps + y0 control1X: 0.2373 * ps + x0 y: 0.5841 * ps + y0 control2X: 0.2648 * ps + x0 y: 0.5778 * ps + y0;
		curveToX: 0.3136 * ps + x0 y: 0.5114 * ps + y0 control1X: 0.3039 * ps + x0 y: 0.5520 * ps + y0 control2X: 0.3136 * ps + x0 y: 0.5341 * ps + y0;
		curveToX: 0.2863 * ps + x0 y: 0.4567 * ps + y0 control1X: 0.3136 * ps + x0 y: 0.4873 * ps + y0 control2X: 0.3045 * ps + x0 y: 0.4690 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.4381 * ps + y0 control1X: 0.2684 * ps + x0 y: 0.4443 * ps + y0 control2X: 0.2419 * ps + x0 y: 0.4381 * ps + y0;
		lineToX: 0.1647 * ps + x0 y: 0.4381 * ps + y0;
		lineToX: 0.1647 * ps + x0 y: 0.3854 * ps + y0;
		lineToX: 0.2106 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.2800 * ps + x0 y: 0.3702 * ps + y0 control1X: 0.2412 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.2643 * ps + x0 y: 0.3803 * ps + y0;
		curveToX: 0.3039 * ps + x0 y: 0.3253 * ps + y0 control1X: 0.2959 * ps + x0 y: 0.3598 * ps + y0 control2X: 0.3039 * ps + x0 y: 0.3449 * ps + y0;
		curveToX: 0.2795 * ps + x0 y: 0.2824 * ps + y0 control1X: 0.3039 * ps + x0 y: 0.3064 * ps + y0 control2X: 0.2957 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.2087 * ps + x0 y: 0.2672 * ps + y0 control1X: 0.2632 * ps + x0 y: 0.2723 * ps + y0 control2X: 0.2396 * ps + x0 y: 0.2672 * ps + y0;
		curveToX: 0.1642 * ps + x0 y: 0.2716 * ps + y0 control1X: 0.1956 * ps + x0 y: 0.2672 * ps + y0 control2X: 0.1808 * ps + x0 y: 0.2687 * ps + y0;
		curveToX: 0.0998 * ps + x0 y: 0.2872 * ps + y0 control1X: 0.1476 * ps + x0 y: 0.2745 * ps + y0 control2X: 0.1261 * ps + x0 y: 0.2797 * ps + y0;
		lineToX: 0.0998 * ps + x0 y: 0.2311 * ps + y0;
		curveToX: 0.1667 * ps + x0 y: 0.2184 * ps + y0 control1X: 0.1235 * ps + x0 y: 0.2256 * ps + y0 control2X: 0.1458 * ps + x0 y: 0.2213 * ps + y0;
		curveToX: 0.2248 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.1875 * ps + x0 y: 0.2155 * ps + y0 control2X: 0.2069 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3361 * ps + x0 y: 0.2428 * ps + y0 control1X: 0.2717 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3088 * ps + x0 y: 0.2236 * ps + y0;
		curveToX: 0.3776 * ps + x0 y: 0.3200 * ps + y0 control1X: 0.3638 * ps + x0 y: 0.2620 * ps + y0 control2X: 0.3776 * ps + x0 y: 0.2877 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.3771 * ps + y0 control1X: 0.3776 * ps + x0 y: 0.3424 * ps + y0 control2X: 0.3703 * ps + x0 y: 0.3615 * ps + y0;
		curveToX: 0.2936 * ps + x0 y: 0.4093 * ps + y0 control1X: 0.3410 * ps + x0 y: 0.3927 * ps + y0 control2X: 0.3203 * ps + x0 y: 0.4034 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:00'!
drawVeraSans180pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0939 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6667 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6667 * ps + x0 y: 0.3024 * ps + y0;
		lineToX: 0.2057 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6779 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6779 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0827 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0827 * ps + x0 y: 0.8810 * ps + y0;
		lineToX: 0.5436 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0939 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0939 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3346 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.2995 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.1149 * ps + y0;
		lineToX: 0.4616 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5295 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4264 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3346 * ps + x0 y: 0.1578 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:01'!
drawVeraSans181pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7492 * ps + y0;
		curveToX: 0.2463 * ps + x0 y: 0.8561 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7964 * ps + y0 control2X: 0.2238 * ps + x0 y: 0.8320 * ps + y0;
		curveToX: 0.3459 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2687 * ps + x0 y: 0.8802 * ps + y0 control2X: 0.3019 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4543 * ps + x0 y: 0.8512 * ps + y0 control1X: 0.3940 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4302 * ps + x0 y: 0.8785 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7282 * ps + y0 control1X: 0.4787 * ps + x0 y: 0.8239 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7828 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.8302 * ps + y0;
		curveToX: 0.5890 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.5807 * ps + x0 y: 0.8497 * ps + y0 control2X: 0.5835 * ps + x0 y: 0.8642 * ps + y0;
		curveToX: 0.6159 * ps + x0 y: 0.8873 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.8828 * ps + y0 control2X: 0.6038 * ps + x0 y: 0.8873 * ps + y0;
		curveToX: 0.6281 * ps + x0 y: 0.8849 * ps + y0 control1X: 0.6188 * ps + x0 y: 0.8873 * ps + y0 control2X: 0.6229 * ps + x0 y: 0.8865 * ps + y0;
		curveToX: 0.6496 * ps + x0 y: 0.8761 * ps + y0 control1X: 0.6333 * ps + x0 y: 0.8829 * ps + y0 control2X: 0.6405 * ps + x0 y: 0.8800 * ps + y0;
		lineToX: 0.6496 * ps + x0 y: 0.9484 * ps + y0;
		curveToX: 0.6115 * ps + x0 y: 0.9650 * ps + y0 control1X: 0.6362 * ps + x0 y: 0.9559 * ps + y0 control2X: 0.6235 * ps + x0 y: 0.9614 * ps + y0;
		curveToX: 0.5768 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5998 * ps + x0 y: 0.9686 * ps + y0 control2X: 0.5882 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.5231 * ps + x0 y: 0.9513 * ps + y0 control1X: 0.5544 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.5365 * ps + x0 y: 0.9640 * ps + y0;
		curveToX: 0.4958 * ps + x0 y: 0.8932 * ps + y0 control1X: 0.5098 * ps + x0 y: 0.9386 * ps + y0 control2X: 0.5007 * ps + x0 y: 0.9192 * ps + y0;
		curveToX: 0.4357 * ps + x0 y: 0.9513 * ps + y0 control1X: 0.4795 * ps + x0 y: 0.9189 * ps + y0 control2X: 0.4595 * ps + x0 y: 0.9383 * ps + y0;
		curveToX: 0.3527 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4123 * ps + x0 y: 0.9640 * ps + y0 control2X: 0.3846 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2677 * ps + x0 y: 0.9513 * ps + y0 control1X: 0.3195 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2912 * ps + x0 y: 0.9640 * ps + y0;
		curveToX: 0.2126 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2446 * ps + x0 y: 0.9386 * ps + y0 control2X: 0.2262 * ps + x0 y: 0.9196 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 1.1642 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:01'!
drawVeraSans182pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3468 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5656 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5656 * ps + x0 y: 1.0524 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 1.0524 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.2872 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.2872 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 1.0524 * ps + y0;
		lineToX: 0.3346 * ps + x0 y: 1.0524 * ps + y0;
		lineToX: 0.3346 * ps + x0 y: 0.6393 * ps + y0;
		curveToX: 0.1720 * ps + x0 y: 0.5773 * ps + y0 control1X: 0.2647 * ps + x0 y: 0.6338 * ps + y0 control2X: 0.2105 * ps + x0 y: 0.6131 * ps + y0;
		curveToX: 0.1149 * ps + x0 y: 0.4332 * ps + y0 control1X: 0.1340 * ps + x0 y: 0.5411 * ps + y0 control2X: 0.1149 * ps + x0 y: 0.4931 * ps + y0;
		curveToX: 0.1784 * ps + x0 y: 0.2838 * ps + y0 control1X: 0.1149 * ps + x0 y: 0.3714 * ps + y0 control2X: 0.1361 * ps + x0 y: 0.3216 * ps + y0;
		curveToX: 0.3468 * ps + x0 y: 0.2272 * ps + y0 control1X: 0.2207 * ps + x0 y: 0.2461 * ps + y0 control2X: 0.2769 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:01'!
drawVeraSans183pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1447 * ps + x0 y: 0.5470 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 0.5470 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 0.6710 * ps + y0;
		lineToX: 0.1447 * ps + x0 y: 0.6710 * ps + y0;
		lineToX: 0.1447 * ps + x0 y: 0.5470 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:02'!
drawVeraSans184pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0929 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.4913 * ps + y0;
		lineToX: 0.1818 * ps + x0 y: 0.8844 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.8844 * ps + y0;
		lineToX: 0.5197 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0807 * ps + x0 y: 0.8742 * ps + y0;
		lineToX: 0.4186 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.0929 * ps + x0 y: 0.4811 * ps + y0;
		lineToX: 0.0929 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.2648 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1452 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2131 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3010 * ps + x0 y: 0.2760 * ps + y0;
		lineToX: 0.3888 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4567 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2648 * ps + x0 y: 0.3400 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:02'!
drawVeraSans185pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1139 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.2135 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.2135 * ps + x0 y: 0.2780 * ps + y0;
		lineToX: 0.1047 * ps + x0 y: 0.2980 * ps + y0;
		lineToX: 0.1047 * ps + x0 y: 0.2413 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.2223 * ps + y0;
		lineToX: 0.2839 * ps + x0 y: 0.2223 * ps + y0;
		lineToX: 0.2839 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.3840 * ps + x0 y: 0.5763 * ps + y0;
		lineToX: 0.3840 * ps + x0 y: 0.6300 * ps + y0;
		lineToX: 0.1139 * ps + x0 y: 0.6300 * ps + y0;
		lineToX: 0.1139 * ps + x0 y: 0.5763 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:02'!
drawVeraSans186pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1056 * ps + x0 y: 0.6671 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.6671 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.7272 * ps + y0;
		lineToX: 0.1056 * ps + x0 y: 0.7272 * ps + y0;
		lineToX: 0.1056 * ps + x0 y: 0.6671 * ps + y0;
		moveToX: 0.2736 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4113 * ps + x0 y: 0.2682 * ps + y0 control1X: 0.3319 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3778 * ps + x0 y: 0.2321 * ps + y0;
		curveToX: 0.4616 * ps + x0 y: 0.4152 * ps + y0 control1X: 0.4448 * ps + x0 y: 0.3040 * ps + y0 control2X: 0.4616 * ps + x0 y: 0.3530 * ps + y0;
		curveToX: 0.4113 * ps + x0 y: 0.5621 * ps + y0 control1X: 0.4616 * ps + x0 y: 0.4773 * ps + y0 control2X: 0.4448 * ps + x0 y: 0.5263 * ps + y0;
		curveToX: 0.2736 * ps + x0 y: 0.6154 * ps + y0 control1X: 0.3778 * ps + x0 y: 0.5976 * ps + y0 control2X: 0.3319 * ps + x0 y: 0.6154 * ps + y0;
		curveToX: 0.1354 * ps + x0 y: 0.5617 * ps + y0 control1X: 0.2153 * ps + x0 y: 0.6154 * ps + y0 control2X: 0.1693 * ps + x0 y: 0.5975 * ps + y0;
		curveToX: 0.0846 * ps + x0 y: 0.4152 * ps + y0 control1X: 0.1016 * ps + x0 y: 0.5258 * ps + y0 control2X: 0.0846 * ps + x0 y: 0.4770 * ps + y0;
		curveToX: 0.1354 * ps + x0 y: 0.2682 * ps + y0 control1X: 0.0846 * ps + x0 y: 0.3530 * ps + y0 control2X: 0.1016 * ps + x0 y: 0.3040 * ps + y0;
		curveToX: 0.2736 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.1693 * ps + x0 y: 0.2321 * ps + y0 control2X: 0.2153 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.2736 * ps + x0 y: 0.2701 * ps + y0;
		curveToX: 0.1916 * ps + x0 y: 0.3097 * ps + y0 control1X: 0.2394 * ps + x0 y: 0.2701 * ps + y0 control2X: 0.2121 * ps + x0 y: 0.2833 * ps + y0;
		curveToX: 0.1608 * ps + x0 y: 0.4152 * ps + y0 control1X: 0.1711 * ps + x0 y: 0.3357 * ps + y0 control2X: 0.1608 * ps + x0 y: 0.3709 * ps + y0;
		curveToX: 0.1916 * ps + x0 y: 0.5192 * ps + y0 control1X: 0.1608 * ps + x0 y: 0.4585 * ps + y0 control2X: 0.1711 * ps + x0 y: 0.4931 * ps + y0;
		curveToX: 0.2736 * ps + x0 y: 0.5582 * ps + y0 control1X: 0.2124 * ps + x0 y: 0.5452 * ps + y0 control2X: 0.2398 * ps + x0 y: 0.5582 * ps + y0;
		curveToX: 0.3551 * ps + x0 y: 0.5192 * ps + y0 control1X: 0.3078 * ps + x0 y: 0.5582 * ps + y0 control2X: 0.3350 * ps + x0 y: 0.5452 * ps + y0;
		curveToX: 0.3859 * ps + x0 y: 0.4152 * ps + y0 control1X: 0.3757 * ps + x0 y: 0.4931 * ps + y0 control2X: 0.3859 * ps + x0 y: 0.4585 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.3092 * ps + y0 control1X: 0.3859 * ps + x0 y: 0.3706 * ps + y0 control2X: 0.3758 * ps + x0 y: 0.3353 * ps + y0;
		curveToX: 0.2736 * ps + x0 y: 0.2701 * ps + y0 control1X: 0.3355 * ps + x0 y: 0.2832 * ps + y0 control2X: 0.3081 * ps + x0 y: 0.2701 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:03'!
drawVeraSans187pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.3610 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.3610 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.8873 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.7941 * ps + y0;
		lineToX: 0.2790 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.5324 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4391 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.8873 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.7941 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.5324 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.4391 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:03'!
drawVeraSans188pointSize: ps atX: x0 y: y0
	self
		moveToX: 1.0348 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 1.0348 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.6725 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.6725 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 1.0197 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 1.0197 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.6725 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.6725 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 1.0436 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 1.0436 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5236 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.2028 * ps + x0 y: 0.8634 * ps + y0 control1X: 0.3827 * ps + x0 y: 0.9562 * ps + y0 control2X: 0.2757 * ps + x0 y: 0.9253 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.1302 * ps + x0 y: 0.8016 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7107 * ps + y0;
		curveToX: 0.2028 * ps + x0 y: 0.3200 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4721 * ps + y0 control2X: 0.1302 * ps + x0 y: 0.3818 * ps + y0;
		curveToX: 0.5236 * ps + x0 y: 0.2272 * ps + y0 control1X: 0.2757 * ps + x0 y: 0.2581 * ps + y0 control2X: 0.3827 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 1.0348 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.5739 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.5109 * ps + x0 y: 0.3102 * ps + y0;
		curveToX: 0.2760 * ps + x0 y: 0.3805 * ps + y0 control1X: 0.4064 * ps + x0 y: 0.3102 * ps + y0 control2X: 0.3281 * ps + x0 y: 0.3336 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.2240 * ps + x0 y: 0.4270 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.4972 * ps + y0;
		curveToX: 0.2760 * ps + x0 y: 0.8029 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6850 * ps + y0 control2X: 0.2240 * ps + x0 y: 0.7557 * ps + y0;
		curveToX: 0.5109 * ps + x0 y: 0.8732 * ps + y0 control1X: 0.3281 * ps + x0 y: 0.8497 * ps + y0 control2X: 0.4064 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5739 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5739 * ps + x0 y: 0.3102 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:04'!
drawVeraSans189pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.9176 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.8766 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.9170 * ps + x0 y: 0.5844 * ps + y0 control2X: 0.9033 * ps + x0 y: 0.5452 * ps + y0;
		curveToX: 0.7697 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.8499 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.8143 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.6496 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.7199 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.6799 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.5978 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.6196 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.6024 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.9176 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 1.0080 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 1.0080 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.5949 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.6506 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.5988 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.6174 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.7897 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.6838 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.7301 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.8903 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.8242 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.8578 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.9870 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.9229 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.9551 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.9870 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.8874 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.9544 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.9212 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.7848 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.8535 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.8193 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.6476 * ps + x0 y: 0.9435 * ps + y0 control1X: 0.7328 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.6870 * ps + x0 y: 0.9614 * ps + y0;
		curveToX: 0.5490 * ps + x0 y: 0.8634 * ps + y0 control1X: 0.6082 * ps + x0 y: 0.9256 * ps + y0 control2X: 0.5754 * ps + x0 y: 0.8989 * ps + y0;
		curveToX: 0.4631 * ps + x0 y: 0.9440 * ps + y0 control1X: 0.5259 * ps + x0 y: 0.8992 * ps + y0 control2X: 0.4972 * ps + x0 y: 0.9261 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4292 * ps + x0 y: 0.9616 * ps + y0 control2X: 0.3895 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4635 * ps + x0 y: 0.4235 * ps + y0 control1X: 0.3895 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4294 * ps + x0 y: 0.4052 * ps + y0;
		curveToX: 0.5475 * ps + x0 y: 0.5031 * ps + y0 control1X: 0.4981 * ps + x0 y: 0.4414 * ps + y0 control2X: 0.5260 * ps + x0 y: 0.4679 * ps + y0;
		curveToX: 0.6423 * ps + x0 y: 0.4235 * ps + y0 control1X: 0.5729 * ps + x0 y: 0.4682 * ps + y0 control2X: 0.6045 * ps + x0 y: 0.4417 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.6800 * ps + x0 y: 0.4052 * ps + y0 control2X: 0.7222 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.9435 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.8423 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.9006 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 1.0080 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.9865 * ps + x0 y: 0.5146 * ps + y0 control2X: 1.0080 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:04'!
drawVeraSans190pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.5270 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6506 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3898 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.4889 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.4889 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.3898 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.3898 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.1989 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.2980 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.2980 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:04'!
drawVeraSans191pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2819 * ps + x0 y: 0.4230 * ps + y0;
		lineToX: 0.3747 * ps + x0 y: 0.4230 * ps + y0;
		lineToX: 0.3747 * ps + x0 y: 0.4992 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.5802 * ps + y0 control1X: 0.3747 * ps + x0 y: 0.5320 * ps + y0 control2X: 0.3701 * ps + x0 y: 0.5590 * ps + y0;
		curveToX: 0.3039 * ps + x0 y: 0.6544 * ps + y0 control1X: 0.3522 * ps + x0 y: 0.6014 * ps + y0 control2X: 0.3332 * ps + x0 y: 0.6261 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.6974 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.7462 * ps + y0 control1X: 0.2411 * ps + x0 y: 0.7146 * ps + y0 control2X: 0.2274 * ps + x0 y: 0.7309 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.7931 * ps + y0 control1X: 0.2108 * ps + x0 y: 0.7615 * ps + y0 control2X: 0.2067 * ps + x0 y: 0.7771 * ps + y0;
		curveToX: 0.2385 * ps + x0 y: 0.8634 * ps + y0 control1X: 0.2067 * ps + x0 y: 0.8221 * ps + y0 control2X: 0.2173 * ps + x0 y: 0.8455 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2599 * ps + x0 y: 0.8813 * ps + y0 control2X: 0.2884 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.4059 * ps + x0 y: 0.8732 * ps + y0 control1X: 0.3493 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.3766 * ps + x0 y: 0.8846 * ps + y0;
		curveToX: 0.4968 * ps + x0 y: 0.8234 * ps + y0 control1X: 0.4352 * ps + x0 y: 0.8618 * ps + y0 control2X: 0.4655 * ps + x0 y: 0.8452 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.9152 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9567 * ps + y0 control1X: 0.4662 * ps + x0 y: 0.9337 * ps + y0 control2X: 0.4352 * ps + x0 y: 0.9476 * ps + y0;
		curveToX: 0.3068 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3727 * ps + x0 y: 0.9658 * ps + y0 control2X: 0.3403 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1623 * ps + x0 y: 0.9230 * ps + y0 control1X: 0.2469 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1987 * ps + x0 y: 0.9546 * ps + y0;
		curveToX: 0.1076 * ps + x0 y: 0.7980 * ps + y0 control1X: 0.1258 * ps + x0 y: 0.8914 * ps + y0 control2X: 0.1076 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1252 * ps + x0 y: 0.7277 * ps + y0 control1X: 0.1076 * ps + x0 y: 0.7732 * ps + y0 control2X: 0.1135 * ps + x0 y: 0.7498 * ps + y0;
		curveToX: 0.1867 * ps + x0 y: 0.6520 * ps + y0 control1X: 0.1372 * ps + x0 y: 0.7052 * ps + y0 control2X: 0.1577 * ps + x0 y: 0.6800 * ps + y0;
		lineToX: 0.2297 * ps + x0 y: 0.6100 * ps + y0;
		curveToX: 0.2624 * ps + x0 y: 0.5743 * ps + y0 control1X: 0.2453 * ps + x0 y: 0.5947 * ps + y0 control2X: 0.2562 * ps + x0 y: 0.5828 * ps + y0;
		curveToX: 0.2756 * ps + x0 y: 0.5490 * ps + y0 control1X: 0.2686 * ps + x0 y: 0.5656 * ps + y0 control2X: 0.2730 * ps + x0 y: 0.5571 * ps + y0;
		curveToX: 0.2804 * ps + x0 y: 0.5241 * ps + y0 control1X: 0.2778 * ps + x0 y: 0.5421 * ps + y0 control2X: 0.2795 * ps + x0 y: 0.5338 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.4830 * ps + y0 control1X: 0.2814 * ps + x0 y: 0.5143 * ps + y0 control2X: 0.2819 * ps + x0 y: 0.5006 * ps + y0;
		lineToX: 0.2819 * ps + x0 y: 0.4230 * ps + y0;
		moveToX: 0.3776 * ps + x0 y: 0.3512 * ps + y0;
		lineToX: 0.2790 * ps + x0 y: 0.3512 * ps + y0;
		lineToX: 0.2790 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3776 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3776 * ps + x0 y: 0.3512 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:05'!
drawVeraSans192pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3517 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4474 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3727 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2604 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3517 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:05'!
drawVeraSans193pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4064 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3854 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3107 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4064 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:05'!
drawVeraSans194pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3337 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4255 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5285 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4606 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.0709 * ps + y0;
		lineToX: 0.2985 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2306 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.0280 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:06'!
drawVeraSans195pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3776 * ps + x0 y: 0.1212 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.1051 * ps + y0;
		curveToX: 0.3298 * ps + x0 y: 0.0958 * ps + y0 control1X: 0.3416 * ps + x0 y: 0.1006 * ps + y0 control2X: 0.3350 * ps + x0 y: 0.0975 * ps + y0;
		curveToX: 0.3166 * ps + x0 y: 0.0929 * ps + y0 control1X: 0.3249 * ps + x0 y: 0.0939 * ps + y0 control2X: 0.3205 * ps + x0 y: 0.0929 * ps + y0;
		curveToX: 0.2892 * ps + x0 y: 0.1051 * ps + y0 control1X: 0.3049 * ps + x0 y: 0.0929 * ps + y0 control2X: 0.2957 * ps + x0 y: 0.0970 * ps + y0;
		curveToX: 0.2795 * ps + x0 y: 0.1393 * ps + y0 control1X: 0.2827 * ps + x0 y: 0.1132 * ps + y0 control2X: 0.2795 * ps + x0 y: 0.1246 * ps + y0;
		lineToX: 0.2795 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2184 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.2433 * ps + x0 y: 0.0641 * ps + y0 control1X: 0.2184 * ps + x0 y: 0.1093 * ps + y0 control2X: 0.2267 * ps + x0 y: 0.0833 * ps + y0;
		curveToX: 0.3107 * ps + x0 y: 0.0353 * ps + y0 control1X: 0.2603 * ps + x0 y: 0.0449 * ps + y0 control2X: 0.2827 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.3429 * ps + x0 y: 0.0392 * ps + y0 control1X: 0.3224 * ps + x0 y: 0.0353 * ps + y0 control2X: 0.3332 * ps + x0 y: 0.0366 * ps + y0;
		curveToX: 0.3815 * ps + x0 y: 0.0563 * ps + y0 control1X: 0.3530 * ps + x0 y: 0.0418 * ps + y0 control2X: 0.3659 * ps + x0 y: 0.0475 * ps + y0;
		lineToX: 0.4093 * ps + x0 y: 0.0709 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.0802 * ps + y0 control1X: 0.4168 * ps + x0 y: 0.0752 * ps + y0 control2X: 0.4233 * ps + x0 y: 0.0783 * ps + y0;
		curveToX: 0.4445 * ps + x0 y: 0.0831 * ps + y0 control1X: 0.4344 * ps + x0 y: 0.0822 * ps + y0 control2X: 0.4396 * ps + x0 y: 0.0831 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.0709 * ps + y0 control1X: 0.4549 * ps + x0 y: 0.0831 * ps + y0 control2X: 0.4634 * ps + x0 y: 0.0791 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.0382 * ps + y0 control1X: 0.4764 * ps + x0 y: 0.0625 * ps + y0 control2X: 0.4797 * ps + x0 y: 0.0516 * ps + y0;
		lineToX: 0.4797 * ps + x0 y: 0.0353 * ps + y0;
		lineToX: 0.5407 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.5143 * ps + x0 y: 0.1134 * ps + y0 control1X: 0.5400 * ps + x0 y: 0.0678 * ps + y0 control2X: 0.5313 * ps + x0 y: 0.0939 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.1422 * ps + y0 control1X: 0.4977 * ps + x0 y: 0.1326 * ps + y0 control2X: 0.4758 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.4172 * ps + x0 y: 0.1383 * ps + y0 control1X: 0.4373 * ps + x0 y: 0.1422 * ps + y0 control2X: 0.4269 * ps + x0 y: 0.1409 * ps + y0;
		curveToX: 0.3776 * ps + x0 y: 0.1212 * ps + y0 control1X: 0.4077 * ps + x0 y: 0.1357 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.1300 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:09'!
drawVeraSans196pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4255 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5246 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5246 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4255 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4255 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.2345 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:09'!
drawVeraSans197pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4538 * ps + x0 y: 0.1622 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.1100 * ps + y0 control1X: 0.4538 * ps + x0 y: 0.1417 * ps + y0 control2X: 0.4465 * ps + x0 y: 0.1243 * ps + y0;
		curveToX: 0.3796 * ps + x0 y: 0.0880 * ps + y0 control1X: 0.4175 * ps + x0 y: 0.0953 * ps + y0 control2X: 0.4001 * ps + x0 y: 0.0880 * ps + y0;
		curveToX: 0.3268 * ps + x0 y: 0.1095 * ps + y0 control1X: 0.3587 * ps + x0 y: 0.0880 * ps + y0 control2X: 0.3412 * ps + x0 y: 0.0952 * ps + y0;
		curveToX: 0.3058 * ps + x0 y: 0.1622 * ps + y0 control1X: 0.3128 * ps + x0 y: 0.1235 * ps + y0 control2X: 0.3058 * ps + x0 y: 0.1411 * ps + y0;
		curveToX: 0.3273 * ps + x0 y: 0.2145 * ps + y0 control1X: 0.3058 * ps + x0 y: 0.1827 * ps + y0 control2X: 0.3130 * ps + x0 y: 0.2002 * ps + y0;
		curveToX: 0.3796 * ps + x0 y: 0.2360 * ps + y0 control1X: 0.3416 * ps + x0 y: 0.2288 * ps + y0 control2X: 0.3591 * ps + x0 y: 0.2360 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2145 * ps + y0 control1X: 0.4001 * ps + x0 y: 0.2360 * ps + y0 control2X: 0.4175 * ps + x0 y: 0.2288 * ps + y0;
		curveToX: 0.4538 * ps + x0 y: 0.1622 * ps + y0 control1X: 0.4465 * ps + x0 y: 0.2002 * ps + y0 control2X: 0.4538 * ps + x0 y: 0.1827 * ps + y0;
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2468 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5129 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3058 * ps + x0 y: 0.2741 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.2262 * ps + y0 control1X: 0.2860 * ps + x0 y: 0.2607 * ps + y0 control2X: 0.2710 * ps + x0 y: 0.2448 * ps + y0;
		curveToX: 0.2458 * ps + x0 y: 0.1622 * ps + y0 control1X: 0.2508 * ps + x0 y: 0.2073 * ps + y0 control2X: 0.2458 * ps + x0 y: 0.1860 * ps + y0;
		curveToX: 0.2843 * ps + x0 y: 0.0670 * ps + y0 control1X: 0.2458 * ps + x0 y: 0.1248 * ps + y0 control2X: 0.2586 * ps + x0 y: 0.0931 * ps + y0;
		curveToX: 0.3796 * ps + x0 y: 0.0280 * ps + y0 control1X: 0.3104 * ps + x0 y: 0.0410 * ps + y0 control2X: 0.3421 * ps + x0 y: 0.0280 * ps + y0;
		curveToX: 0.4743 * ps + x0 y: 0.0675 * ps + y0 control1X: 0.4167 * ps + x0 y: 0.0280 * ps + y0 control2X: 0.4482 * ps + x0 y: 0.0411 * ps + y0;
		curveToX: 0.5138 * ps + x0 y: 0.1622 * ps + y0 control1X: 0.5007 * ps + x0 y: 0.0936 * ps + y0 control2X: 0.5138 * ps + x0 y: 0.1251 * ps + y0;
		curveToX: 0.4982 * ps + x0 y: 0.2252 * ps + y0 control1X: 0.5138 * ps + x0 y: 0.1850 * ps + y0 control2X: 0.5086 * ps + x0 y: 0.2060 * ps + y0;
		curveToX: 0.4538 * ps + x0 y: 0.2741 * ps + y0 control1X: 0.4881 * ps + x0 y: 0.2444 * ps + y0 control2X: 0.4733 * ps + x0 y: 0.2607 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3058 * ps + x0 y: 0.2741 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 11/9/2012 00:10'!
drawVeraSans198pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.9386 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.9386 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.9240 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.9240 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.9479 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.9479 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2199 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0417 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3468 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.9386 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4147 * ps + x0 y: 0.3043 * ps + y0;
		lineToX: 0.2536 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.3043 * ps + y0;
		lineToX: 0.4147 * ps + x0 y: 0.3043 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans199pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6818 * ps + x0 y: 0.2833 * ps + y0;
		lineToX: 0.6818 * ps + x0 y: 0.3873 * ps + y0;
		curveToX: 0.5754 * ps + x0 y: 0.3180 * ps + y0 control1X: 0.6486 * ps + x0 y: 0.3564 * ps + y0 control2X: 0.6131 * ps + x0 y: 0.3333 * ps + y0;
		curveToX: 0.4557 * ps + x0 y: 0.2950 * ps + y0 control1X: 0.5379 * ps + x0 y: 0.3027 * ps + y0 control2X: 0.4981 * ps + x0 y: 0.2950 * ps + y0;
		curveToX: 0.2643 * ps + x0 y: 0.3717 * ps + y0 control1X: 0.3724 * ps + x0 y: 0.2950 * ps + y0 control2X: 0.3086 * ps + x0 y: 0.3206 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2201 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.4961 * ps + y0;
		curveToX: 0.2643 * ps + x0 y: 0.8131 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6884 * ps + y0 control2X: 0.2201 * ps + x0 y: 0.7620 * ps + y0;
		curveToX: 0.4557 * ps + x0 y: 0.8893 * ps + y0 control1X: 0.3086 * ps + x0 y: 0.8639 * ps + y0 control2X: 0.3724 * ps + x0 y: 0.8893 * ps + y0;
		curveToX: 0.5754 * ps + x0 y: 0.8663 * ps + y0 control1X: 0.4981 * ps + x0 y: 0.8893 * ps + y0 control2X: 0.5379 * ps + x0 y: 0.8816 * ps + y0;
		curveToX: 0.6818 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.6131 * ps + x0 y: 0.8510 * ps + y0 control2X: 0.6486 * ps + x0 y: 0.8279 * ps + y0;
		lineToX: 0.6818 * ps + x0 y: 0.9000 * ps + y0;
		curveToX: 0.5719 * ps + x0 y: 0.9528 * ps + y0 control1X: 0.6473 * ps + x0 y: 0.9235 * ps + y0 control2X: 0.6107 * ps + x0 y: 0.9410 * ps + y0;
		curveToX: 0.4499 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5335 * ps + x0 y: 0.9645 * ps + y0 control2X: 0.4928 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1891 * ps + x0 y: 0.8693 * ps + y0 control1X: 0.3395 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2526 * ps + x0 y: 0.9367 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1257 * ps + x0 y: 0.8016 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7093 * ps + y0;
		curveToX: 0.1891 * ps + x0 y: 0.3156 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4752 * ps + y0 control2X: 0.1257 * ps + x0 y: 0.3829 * ps + y0;
		curveToX: 0.4499 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2526 * ps + x0 y: 0.2478 * ps + y0 control2X: 0.3395 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5729 * ps + x0 y: 0.2316 * ps + y0 control1X: 0.4935 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.5345 * ps + x0 y: 0.2199 * ps + y0;
		curveToX: 0.6818 * ps + x0 y: 0.2833 * ps + y0 control1X: 0.6117 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.6480 * ps + x0 y: 0.2602 * ps + y0;
		moveToX: 0.4758 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.5158 * ps + x0 y: 1.0118 * ps + y0 control1X: 0.4937 * ps + x0 y: 0.9764 * ps + y0 control2X: 0.5070 * ps + x0 y: 0.9949 * ps + y0;
		curveToX: 0.5290 * ps + x0 y: 1.0612 * ps + y0 control1X: 0.5246 * ps + x0 y: 1.0291 * ps + y0 control2X: 0.5290 * ps + x0 y: 1.0455 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 1.1266 * ps + y0 control1X: 0.5290 * ps + x0 y: 1.0901 * ps + y0 control2X: 0.5192 * ps + x0 y: 1.1119 * ps + y0;
		curveToX: 0.4128 * ps + x0 y: 1.1491 * ps + y0 control1X: 0.4801 * ps + x0 y: 1.1416 * ps + y0 control2X: 0.4512 * ps + x0 y: 1.1491 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 1.1461 * ps + y0 control1X: 0.3978 * ps + x0 y: 1.1491 * ps + y0 control2X: 0.3831 * ps + x0 y: 1.1481 * ps + y0;
		curveToX: 0.3268 * ps + x0 y: 1.1373 * ps + y0 control1X: 0.3548 * ps + x0 y: 1.1442 * ps + y0 control2X: 0.3408 * ps + x0 y: 1.1412 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 1.0734 * ps + y0;
		curveToX: 0.3615 * ps + x0 y: 1.0851 * ps + y0 control1X: 0.3379 * ps + x0 y: 1.0789 * ps + y0 control2X: 0.3495 * ps + x0 y: 1.0828 * ps + y0;
		curveToX: 0.4025 * ps + x0 y: 1.0890 * ps + y0 control1X: 0.3735 * ps + x0 y: 1.0877 * ps + y0 control2X: 0.3872 * ps + x0 y: 1.0890 * ps + y0;
		curveToX: 0.4460 * ps + x0 y: 1.0773 * ps + y0 control1X: 0.4217 * ps + x0 y: 1.0890 * ps + y0 control2X: 0.4362 * ps + x0 y: 1.0851 * ps + y0;
		curveToX: 0.4606 * ps + x0 y: 1.0431 * ps + y0 control1X: 0.4557 * ps + x0 y: 1.0695 * ps + y0 control2X: 0.4606 * ps + x0 y: 1.0581 * ps + y0;
		curveToX: 0.4499 * ps + x0 y: 1.0070 * ps + y0 control1X: 0.4606 * ps + x0 y: 1.0333 * ps + y0 control2X: 0.4570 * ps + x0 y: 1.0213 * ps + y0;
		curveToX: 0.4176 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.4430 * ps + x0 y: 0.9930 * ps + y0 control2X: 0.4323 * ps + x0 y: 0.9760 * ps + y0;
		lineToX: 0.4758 * ps + x0 y: 0.9562 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans200pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3371 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4328 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3581 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans201pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3918 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4821 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3708 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2961 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3918 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans202pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3190 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4108 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4460 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3649 * ps + x0 y: 0.0709 * ps + y0;
		lineToX: 0.2839 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3190 * ps + x0 y: 0.0280 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans203pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4108 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4108 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4108 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.2199 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3190 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3190 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2199 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2199 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans204pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.1579 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.2536 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.1789 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.0666 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.1579 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans205pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.2126 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3029 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.1916 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans206pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.1398 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.3346 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2668 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.1857 * ps + x0 y: 0.0709 * ps + y0;
		lineToX: 0.1047 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.0368 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.1398 * ps + x0 y: 0.0280 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans207pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.2316 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.0407 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.1398 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.1398 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.0407 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.0407 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans208pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1408 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.2272 * ps + y0;
		curveToX: 0.6545 * ps + x0 y: 0.3156 * ps + y0 control1X: 0.4849 * ps + x0 y: 0.2272 * ps + y0 control2X: 0.5884 * ps + x0 y: 0.2566 * ps + y0;
		curveToX: 0.7536 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.7205 * ps + x0 y: 0.3742 * ps + y0 control2X: 0.7536 * ps + x0 y: 0.4659 * ps + y0;
		curveToX: 0.6540 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7536 * ps + x0 y: 0.7166 * ps + y0 control2X: 0.7204 * ps + x0 y: 0.8089 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.5879 * ps + x0 y: 0.9267 * ps + y0 control2X: 0.4845 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.6144 * ps + y0;
		lineToX: 0.0426 * ps + x0 y: 0.6144 * ps + y0;
		lineToX: 0.0426 * ps + x0 y: 0.5441 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.5441 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.2399 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.5441 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.5441 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.6144 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.6144 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.8751 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.8751 * ps + y0;
		curveToX: 0.5793 * ps + x0 y: 0.8068 * ps + y0 control1X: 0.4592 * ps + x0 y: 0.8751 * ps + y0 control2X: 0.5327 * ps + x0 y: 0.8523 * ps + y0;
		curveToX: 0.6496 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.6261 * ps + x0 y: 0.7612 * ps + y0 control2X: 0.6496 * ps + x0 y: 0.6893 * ps + y0;
		curveToX: 0.5793 * ps + x0 y: 0.3766 * ps + y0 control1X: 0.6496 * ps + x0 y: 0.4933 * ps + y0 control2X: 0.6261 * ps + x0 y: 0.4218 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.3082 * ps + y0 control1X: 0.5327 * ps + x0 y: 0.3310 * ps + y0 control2X: 0.4592 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.3082 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans209pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2687 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5920 * ps + x0 y: 0.8370 * ps + y0;
		lineToX: 0.5920 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.3463 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4098 * ps + x0 y: 0.1212 * ps + y0;
		lineToX: 0.3820 * ps + x0 y: 0.1051 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.0958 * ps + y0 control1X: 0.3739 * ps + x0 y: 0.1006 * ps + y0 control2X: 0.3672 * ps + x0 y: 0.0975 * ps + y0;
		curveToX: 0.3488 * ps + x0 y: 0.0929 * ps + y0 control1X: 0.3571 * ps + x0 y: 0.0939 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.0929 * ps + y0;
		curveToX: 0.3215 * ps + x0 y: 0.1051 * ps + y0 control1X: 0.3371 * ps + x0 y: 0.0929 * ps + y0 control2X: 0.3280 * ps + x0 y: 0.0970 * ps + y0;
		curveToX: 0.3117 * ps + x0 y: 0.1393 * ps + y0 control1X: 0.3149 * ps + x0 y: 0.1132 * ps + y0 control2X: 0.3117 * ps + x0 y: 0.1246 * ps + y0;
		lineToX: 0.3117 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2507 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.2756 * ps + x0 y: 0.0641 * ps + y0 control1X: 0.2507 * ps + x0 y: 0.1093 * ps + y0 control2X: 0.2590 * ps + x0 y: 0.0833 * ps + y0;
		curveToX: 0.3429 * ps + x0 y: 0.0353 * ps + y0 control1X: 0.2925 * ps + x0 y: 0.0449 * ps + y0 control2X: 0.3149 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.3752 * ps + x0 y: 0.0392 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.0353 * ps + y0 control2X: 0.3654 * ps + x0 y: 0.0366 * ps + y0;
		curveToX: 0.4137 * ps + x0 y: 0.0563 * ps + y0 control1X: 0.3853 * ps + x0 y: 0.0418 * ps + y0 control2X: 0.3981 * ps + x0 y: 0.0475 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.0709 * ps + y0;
		curveToX: 0.4611 * ps + x0 y: 0.0802 * ps + y0 control1X: 0.4491 * ps + x0 y: 0.0752 * ps + y0 control2X: 0.4556 * ps + x0 y: 0.0783 * ps + y0;
		curveToX: 0.4767 * ps + x0 y: 0.0831 * ps + y0 control1X: 0.4666 * ps + x0 y: 0.0822 * ps + y0 control2X: 0.4718 * ps + x0 y: 0.0831 * ps + y0;
		curveToX: 0.5021 * ps + x0 y: 0.0709 * ps + y0 control1X: 0.4871 * ps + x0 y: 0.0831 * ps + y0 control2X: 0.4956 * ps + x0 y: 0.0791 * ps + y0;
		curveToX: 0.5119 * ps + x0 y: 0.0382 * ps + y0 control1X: 0.5086 * ps + x0 y: 0.0625 * ps + y0 control2X: 0.5119 * ps + x0 y: 0.0516 * ps + y0;
		lineToX: 0.5119 * ps + x0 y: 0.0353 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.5466 * ps + x0 y: 0.1134 * ps + y0 control1X: 0.5723 * ps + x0 y: 0.0678 * ps + y0 control2X: 0.5635 * ps + x0 y: 0.0939 * ps + y0;
		curveToX: 0.4806 * ps + x0 y: 0.1422 * ps + y0 control1X: 0.5300 * ps + x0 y: 0.1326 * ps + y0 control2X: 0.5080 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.4494 * ps + x0 y: 0.1383 * ps + y0 control1X: 0.4696 * ps + x0 y: 0.1422 * ps + y0 control2X: 0.4592 * ps + x0 y: 0.1409 * ps + y0;
		curveToX: 0.4098 * ps + x0 y: 0.1212 * ps + y0 control1X: 0.4399 * ps + x0 y: 0.1357 * ps + y0 control2X: 0.4268 * ps + x0 y: 0.1300 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans210pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.4040 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4997 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4250 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3127 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans211pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.4587 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4377 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3630 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4587 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans212pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.3859 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.5129 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4318 * ps + x0 y: 0.0709 * ps + y0;
		lineToX: 0.3508 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2829 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3859 * ps + x0 y: 0.0280 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans213pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.4299 * ps + x0 y: 0.1212 * ps + y0;
		lineToX: 0.4020 * ps + x0 y: 0.1051 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.0958 * ps + y0 control1X: 0.3939 * ps + x0 y: 0.1006 * ps + y0 control2X: 0.3872 * ps + x0 y: 0.0975 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 0.0929 * ps + y0 control1X: 0.3771 * ps + x0 y: 0.0939 * ps + y0 control2X: 0.3727 * ps + x0 y: 0.0929 * ps + y0;
		curveToX: 0.3415 * ps + x0 y: 0.1051 * ps + y0 control1X: 0.3571 * ps + x0 y: 0.0929 * ps + y0 control2X: 0.3480 * ps + x0 y: 0.0970 * ps + y0;
		curveToX: 0.3317 * ps + x0 y: 0.1393 * ps + y0 control1X: 0.3350 * ps + x0 y: 0.1132 * ps + y0 control2X: 0.3317 * ps + x0 y: 0.1246 * ps + y0;
		lineToX: 0.3317 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2707 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.2956 * ps + x0 y: 0.0641 * ps + y0 control1X: 0.2707 * ps + x0 y: 0.1093 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.0833 * ps + y0;
		curveToX: 0.3630 * ps + x0 y: 0.0353 * ps + y0 control1X: 0.3125 * ps + x0 y: 0.0449 * ps + y0 control2X: 0.3350 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.3952 * ps + x0 y: 0.0392 * ps + y0 control1X: 0.3747 * ps + x0 y: 0.0353 * ps + y0 control2X: 0.3854 * ps + x0 y: 0.0366 * ps + y0;
		curveToX: 0.4338 * ps + x0 y: 0.0563 * ps + y0 control1X: 0.4053 * ps + x0 y: 0.0418 * ps + y0 control2X: 0.4181 * ps + x0 y: 0.0475 * ps + y0;
		lineToX: 0.4616 * ps + x0 y: 0.0709 * ps + y0;
		curveToX: 0.4811 * ps + x0 y: 0.0802 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.0752 * ps + y0 control2X: 0.4756 * ps + x0 y: 0.0783 * ps + y0;
		curveToX: 0.4968 * ps + x0 y: 0.0831 * ps + y0 control1X: 0.4867 * ps + x0 y: 0.0822 * ps + y0 control2X: 0.4919 * ps + x0 y: 0.0831 * ps + y0;
		curveToX: 0.5221 * ps + x0 y: 0.0709 * ps + y0 control1X: 0.5072 * ps + x0 y: 0.0831 * ps + y0 control2X: 0.5156 * ps + x0 y: 0.0791 * ps + y0;
		curveToX: 0.5319 * ps + x0 y: 0.0382 * ps + y0 control1X: 0.5287 * ps + x0 y: 0.0625 * ps + y0 control2X: 0.5319 * ps + x0 y: 0.0516 * ps + y0;
		lineToX: 0.5319 * ps + x0 y: 0.0353 * ps + y0;
		lineToX: 0.5929 * ps + x0 y: 0.0353 * ps + y0;
		curveToX: 0.5666 * ps + x0 y: 0.1134 * ps + y0 control1X: 0.5923 * ps + x0 y: 0.0678 * ps + y0 control2X: 0.5835 * ps + x0 y: 0.0939 * ps + y0;
		curveToX: 0.5007 * ps + x0 y: 0.1422 * ps + y0 control1X: 0.5500 * ps + x0 y: 0.1326 * ps + y0 control2X: 0.5280 * ps + x0 y: 0.1422 * ps + y0;
		curveToX: 0.4694 * ps + x0 y: 0.1383 * ps + y0 control1X: 0.4896 * ps + x0 y: 0.1422 * ps + y0 control2X: 0.4792 * ps + x0 y: 0.1409 * ps + y0;
		curveToX: 0.4299 * ps + x0 y: 0.1212 * ps + y0 control1X: 0.4600 * ps + x0 y: 0.1357 * ps + y0 control2X: 0.4468 * ps + x0 y: 0.1300 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans214pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.4777 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5768 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.2868 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3859 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3859 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2868 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2868 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans215pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.7389 * ps + x0 y: 0.4191 * ps + y0;
		lineToX: 0.5158 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.7389 * ps + x0 y: 0.8663 * ps + y0;
		lineToX: 0.6808 * ps + x0 y: 0.9254 * ps + y0;
		lineToX: 0.4567 * ps + x0 y: 0.7013 * ps + y0;
		lineToX: 0.2326 * ps + x0 y: 0.9254 * ps + y0;
		lineToX: 0.1750 * ps + x0 y: 0.8663 * ps + y0;
		lineToX: 0.3976 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.1750 * ps + x0 y: 0.4191 * ps + y0;
		lineToX: 0.2326 * ps + x0 y: 0.3600 * ps + y0;
		lineToX: 0.4567 * ps + x0 y: 0.5841 * ps + y0;
		lineToX: 0.6808 * ps + x0 y: 0.3600 * ps + y0;
		lineToX: 0.7389 * ps + x0 y: 0.4191 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans216pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6266 * ps + x0 y: 0.4132 * ps + y0;
		lineToX: 0.2765 * ps + x0 y: 0.8263 * ps + y0;
		curveToX: 0.3459 * ps + x0 y: 0.8742 * ps + y0 control1X: 0.2967 * ps + x0 y: 0.8471 * ps + y0 control2X: 0.3198 * ps + x0 y: 0.8631 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3722 * ps + x0 y: 0.8849 * ps + y0 control2X: 0.4009 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6550 * ps + x0 y: 0.4928 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5560 * ps + y0 control2X: 0.6615 * ps + x0 y: 0.5228 * ps + y0;
		curveToX: 0.6266 * ps + x0 y: 0.4132 * ps + y0 control1X: 0.6488 * ps + x0 y: 0.4629 * ps + y0 control2X: 0.6393 * ps + x0 y: 0.4363 * ps + y0;
		moveToX: 0.5866 * ps + x0 y: 0.3580 * ps + y0;
		curveToX: 0.5173 * ps + x0 y: 0.3107 * ps + y0 control1X: 0.5667 * ps + x0 y: 0.3372 * ps + y0 control2X: 0.5436 * ps + x0 y: 0.3214 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.4912 * ps + x0 y: 0.2996 * ps + y0 control2X: 0.4627 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2072 * ps + x0 y: 0.6930 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6289 * ps + y0 control2X: 0.2010 * ps + x0 y: 0.6624 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.7721 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.7236 * ps + y0 control2X: 0.2233 * ps + x0 y: 0.7500 * ps + y0;
		lineToX: 0.5866 * ps + x0 y: 0.3580 * ps + y0;
		moveToX: 0.1706 * ps + x0 y: 0.8492 * ps + y0;
		curveToX: 0.1130 * ps + x0 y: 0.7350 * ps + y0 control1X: 0.1452 * ps + x0 y: 0.8160 * ps + y0 control2X: 0.1260 * ps + x0 y: 0.7780 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1003 * ps + x0 y: 0.6920 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.6445 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5490 * ps + x0 y: 0.2330 * ps + y0 control1X: 0.4741 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.5132 * ps + x0 y: 0.2203 * ps + y0;
		curveToX: 0.6457 * ps + x0 y: 0.2882 * ps + y0 control1X: 0.5851 * ps + x0 y: 0.2454 * ps + y0 control2X: 0.6174 * ps + x0 y: 0.2638 * ps + y0;
		lineToX: 0.7248 * ps + x0 y: 0.1950 * ps + y0;
		lineToX: 0.7746 * ps + x0 y: 0.2384 * ps + y0;
		lineToX: 0.6916 * ps + x0 y: 0.3351 * ps + y0;
		curveToX: 0.7492 * ps + x0 y: 0.4503 * ps + y0 control1X: 0.7170 * ps + x0 y: 0.3686 * ps + y0 control2X: 0.7362 * ps + x0 y: 0.4070 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7622 * ps + x0 y: 0.4936 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.5410 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.3151 * ps + x0 y: 0.9523 * ps + y0 control1X: 0.3901 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.3512 * ps + x0 y: 0.9643 * ps + y0;
		curveToX: 0.2170 * ps + x0 y: 0.8971 * ps + y0 control1X: 0.2793 * ps + x0 y: 0.9399 * ps + y0 control2X: 0.2466 * ps + x0 y: 0.9215 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.9904 * ps + y0;
		lineToX: 0.0876 * ps + x0 y: 0.9474 * ps + y0;
		lineToX: 0.1706 * ps + x0 y: 0.8492 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans217pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.6700 * ps + y0;
		curveToX: 0.2663 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7482 * ps + y0 control2X: 0.2380 * ps + x0 y: 0.8045 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2946 * ps + x0 y: 0.8732 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.5412 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4671 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5129 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.6700 * ps + y0 control1X: 0.5695 * ps + x0 y: 0.8045 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.7482 * ps + y0;
		lineToX: 0.5837 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.6823 * ps + y0;
		curveToX: 0.6120 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.6828 * ps + x0 y: 0.7773 * ps + y0 control2X: 0.6592 * ps + x0 y: 0.8491 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5651 * ps + x0 y: 0.9461 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2422 * ps + x0 y: 0.9461 * ps + y0;
		curveToX: 0.1247 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.1481 * ps + x0 y: 0.8491 * ps + y0 control2X: 0.1247 * ps + x0 y: 0.7773 * ps + y0;
		lineToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3761 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4718 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3971 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2848 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3761 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans218pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.6700 * ps + y0;
		curveToX: 0.2663 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7482 * ps + y0 control2X: 0.2380 * ps + x0 y: 0.8045 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2946 * ps + x0 y: 0.8732 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.5412 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4671 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5129 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.6700 * ps + y0 control1X: 0.5695 * ps + x0 y: 0.8045 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.7482 * ps + y0;
		lineToX: 0.5837 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.6823 * ps + y0;
		curveToX: 0.6120 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.6828 * ps + x0 y: 0.7773 * ps + y0 control2X: 0.6592 * ps + x0 y: 0.8491 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5651 * ps + x0 y: 0.9461 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2422 * ps + x0 y: 0.9461 * ps + y0;
		curveToX: 0.1247 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.1481 * ps + x0 y: 0.8491 * ps + y0 control2X: 0.1247 * ps + x0 y: 0.7773 * ps + y0;
		lineToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4308 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.5212 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4098 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3351 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4308 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans219pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.6700 * ps + y0;
		curveToX: 0.2663 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7482 * ps + y0 control2X: 0.2380 * ps + x0 y: 0.8045 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2946 * ps + x0 y: 0.8732 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.5412 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4671 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5129 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.6700 * ps + y0 control1X: 0.5695 * ps + x0 y: 0.8045 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.7482 * ps + y0;
		lineToX: 0.5837 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.6823 * ps + y0;
		curveToX: 0.6120 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.6828 * ps + x0 y: 0.7773 * ps + y0 control2X: 0.6592 * ps + x0 y: 0.8491 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5651 * ps + x0 y: 0.9461 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2422 * ps + x0 y: 0.9461 * ps + y0;
		curveToX: 0.1247 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.1481 * ps + x0 y: 0.8491 * ps + y0 control2X: 0.1247 * ps + x0 y: 0.7773 * ps + y0;
		lineToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3581 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.4499 * ps + x0 y: 0.0280 * ps + y0;
		lineToX: 0.5529 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4850 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.4040 * ps + x0 y: 0.0709 * ps + y0;
		lineToX: 0.3229 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2551 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3581 * ps + x0 y: 0.0280 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans220pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.6700 * ps + y0;
		curveToX: 0.2663 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7482 * ps + y0 control2X: 0.2380 * ps + x0 y: 0.8045 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2946 * ps + x0 y: 0.8732 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.5412 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4671 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5129 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.6700 * ps + y0 control1X: 0.5695 * ps + x0 y: 0.8045 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.7482 * ps + y0;
		lineToX: 0.5837 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.6823 * ps + y0;
		curveToX: 0.6120 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.6828 * ps + x0 y: 0.7773 * ps + y0 control2X: 0.6592 * ps + x0 y: 0.8491 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5651 * ps + x0 y: 0.9461 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2422 * ps + x0 y: 0.9461 * ps + y0;
		curveToX: 0.1247 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.1481 * ps + x0 y: 0.8491 * ps + y0 control2X: 0.1247 * ps + x0 y: 0.7773 * ps + y0;
		lineToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4499 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4499 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.4499 * ps + x0 y: 0.0431 * ps + y0;
		moveToX: 0.2590 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3581 * ps + x0 y: 0.0431 * ps + y0;
		lineToX: 0.3581 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2590 * ps + x0 y: 0.1422 * ps + y0;
		lineToX: 0.2590 * ps + x0 y: 0.0431 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans221pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.5270 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6506 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.3708 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.4611 * ps + x0 y: 0.0289 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.2751 * ps + x0 y: 0.1578 * ps + y0;
		lineToX: 0.3708 * ps + x0 y: 0.0289 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans222pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3561 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.3561 * ps + y0;
		curveToX: 0.5436 * ps + x0 y: 0.4113 * ps + y0 control1X: 0.4403 * ps + x0 y: 0.3561 * ps + y0 control2X: 0.5020 * ps + x0 y: 0.3745 * ps + y0;
		curveToX: 0.6066 * ps + x0 y: 0.5734 * ps + y0 control1X: 0.5856 * ps + x0 y: 0.4477 * ps + y0 control2X: 0.6066 * ps + x0 y: 0.5018 * ps + y0;
		curveToX: 0.5436 * ps + x0 y: 0.7360 * ps + y0 control1X: 0.6066 * ps + x0 y: 0.6450 * ps + y0 control2X: 0.5856 * ps + x0 y: 0.6992 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.5020 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.4403 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.7911 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.2345 * ps + x0 y: 0.4371 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.7101 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.7101 * ps + y0;
		curveToX: 0.4650 * ps + x0 y: 0.6744 * ps + y0 control1X: 0.4045 * ps + x0 y: 0.7101 * ps + y0 control2X: 0.4399 * ps + x0 y: 0.6982 * ps + y0;
		curveToX: 0.5026 * ps + x0 y: 0.5734 * ps + y0 control1X: 0.4901 * ps + x0 y: 0.6507 * ps + y0 control2X: 0.5026 * ps + x0 y: 0.6170 * ps + y0;
		curveToX: 0.4650 * ps + x0 y: 0.4728 * ps + y0 control1X: 0.5026 * ps + x0 y: 0.5297 * ps + y0 control2X: 0.4901 * ps + x0 y: 0.4962 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.4371 * ps + y0 control1X: 0.4403 * ps + x0 y: 0.4490 * ps + y0 control2X: 0.4048 * ps + x0 y: 0.4371 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.4371 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans223pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1286 * ps + x0 y: 0.4010 * ps + y0;
		curveToX: 0.1867 * ps + x0 y: 0.2501 * ps + y0 control1X: 0.1286 * ps + x0 y: 0.3359 * ps + y0 control2X: 0.1480 * ps + x0 y: 0.2856 * ps + y0;
		curveToX: 0.3517 * ps + x0 y: 0.1964 * ps + y0 control1X: 0.2258 * ps + x0 y: 0.2143 * ps + y0 control2X: 0.2808 * ps + x0 y: 0.1964 * ps + y0;
		curveToX: 0.5065 * ps + x0 y: 0.2531 * ps + y0 control1X: 0.4194 * ps + x0 y: 0.1964 * ps + y0 control2X: 0.4710 * ps + x0 y: 0.2153 * ps + y0;
		curveToX: 0.5617 * ps + x0 y: 0.4191 * ps + y0 control1X: 0.5423 * ps + x0 y: 0.2908 * ps + y0 control2X: 0.5607 * ps + x0 y: 0.3462 * ps + y0;
		curveToX: 0.4469 * ps + x0 y: 0.4513 * ps + y0 control1X: 0.5125 * ps + x0 y: 0.4217 * ps + y0 control2X: 0.4743 * ps + x0 y: 0.4324 * ps + y0;
		curveToX: 0.4059 * ps + x0 y: 0.5260 * ps + y0 control1X: 0.4196 * ps + x0 y: 0.4699 * ps + y0 control2X: 0.4059 * ps + x0 y: 0.4948 * ps + y0;
		curveToX: 0.4201 * ps + x0 y: 0.5690 * ps + y0 control1X: 0.4059 * ps + x0 y: 0.5413 * ps + y0 control2X: 0.4107 * ps + x0 y: 0.5556 * ps + y0;
		curveToX: 0.4660 * ps + x0 y: 0.6090 * ps + y0 control1X: 0.4295 * ps + x0 y: 0.5820 * ps + y0 control2X: 0.4448 * ps + x0 y: 0.5953 * ps + y0;
		lineToX: 0.4938 * ps + x0 y: 0.6271 * ps + y0;
		curveToX: 0.5983 * ps + x0 y: 0.7135 * ps + y0 control1X: 0.5479 * ps + x0 y: 0.6616 * ps + y0 control2X: 0.5827 * ps + x0 y: 0.6904 * ps + y0;
		curveToX: 0.6218 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.6139 * ps + x0 y: 0.7366 * ps + y0 control2X: 0.6218 * ps + x0 y: 0.7644 * ps + y0;
		curveToX: 0.5666 * ps + x0 y: 0.9259 * ps + y0 control1X: 0.6218 * ps + x0 y: 0.8530 * ps + y0 control2X: 0.6034 * ps + x0 y: 0.8960 * ps + y0;
		curveToX: 0.4089 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5301 * ps + x0 y: 0.9555 * ps + y0 control2X: 0.4775 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.3444 * ps + x0 y: 0.9640 * ps + y0 control1X: 0.3880 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.3665 * ps + x0 y: 0.9682 * ps + y0;
		curveToX: 0.2756 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.3223 * ps + x0 y: 0.9601 * ps + y0 control2X: 0.2993 * ps + x0 y: 0.9542 * ps + y0;
		lineToX: 0.2756 * ps + x0 y: 0.8663 * ps + y0;
		curveToX: 0.3488 * ps + x0 y: 0.8883 * ps + y0 control1X: 0.3016 * ps + x0 y: 0.8761 * ps + y0 control2X: 0.3260 * ps + x0 y: 0.8834 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.3716 * ps + x0 y: 0.8929 * ps + y0 control2X: 0.3936 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.5007 * ps + x0 y: 0.8722 * ps + y0 control1X: 0.4525 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4811 * ps + x0 y: 0.8875 * ps + y0;
		curveToX: 0.5300 * ps + x0 y: 0.8043 * ps + y0 control1X: 0.5202 * ps + x0 y: 0.8566 * ps + y0 control2X: 0.5300 * ps + x0 y: 0.8339 * ps + y0;
		curveToX: 0.5153 * ps + x0 y: 0.7531 * ps + y0 control1X: 0.5300 * ps + x0 y: 0.7838 * ps + y0 control2X: 0.5251 * ps + x0 y: 0.7667 * ps + y0;
		curveToX: 0.4518 * ps + x0 y: 0.7023 * ps + y0 control1X: 0.5059 * ps + x0 y: 0.7394 * ps + y0 control2X: 0.4847 * ps + x0 y: 0.7225 * ps + y0;
		lineToX: 0.4069 * ps + x0 y: 0.6754 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.6139 * ps + y0 control1X: 0.3757 * ps + x0 y: 0.6562 * ps + y0 control2X: 0.3529 * ps + x0 y: 0.6357 * ps + y0;
		curveToX: 0.3176 * ps + x0 y: 0.5363 * ps + y0 control1X: 0.3246 * ps + x0 y: 0.5918 * ps + y0 control2X: 0.3176 * ps + x0 y: 0.5659 * ps + y0;
		curveToX: 0.3581 * ps + x0 y: 0.4323 * ps + y0 control1X: 0.3176 * ps + x0 y: 0.4949 * ps + y0 control2X: 0.3311 * ps + x0 y: 0.4603 * ps + y0;
		curveToX: 0.4728 * ps + x0 y: 0.3761 * ps + y0 control1X: 0.3854 * ps + x0 y: 0.4043 * ps + y0 control2X: 0.4237 * ps + x0 y: 0.3855 * ps + y0;
		curveToX: 0.4367 * ps + x0 y: 0.2985 * ps + y0 control1X: 0.4702 * ps + x0 y: 0.3426 * ps + y0 control2X: 0.4582 * ps + x0 y: 0.3167 * ps + y0;
		curveToX: 0.3498 * ps + x0 y: 0.2711 * ps + y0 control1X: 0.4155 * ps + x0 y: 0.2802 * ps + y0 control2X: 0.3866 * ps + x0 y: 0.2711 * ps + y0;
		curveToX: 0.2531 * ps + x0 y: 0.3053 * ps + y0 control1X: 0.3075 * ps + x0 y: 0.2711 * ps + y0 control2X: 0.2752 * ps + x0 y: 0.2825 * ps + y0;
		curveToX: 0.2199 * ps + x0 y: 0.4030 * ps + y0 control1X: 0.2310 * ps + x0 y: 0.3278 * ps + y0 control2X: 0.2199 * ps + x0 y: 0.3603 * ps + y0;
		lineToX: 0.2199 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4010 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans224pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.2570 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3947 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.3200 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1608 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2570 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans225pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.3957 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4928 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2590 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3957 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans226pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.2917 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3639 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4836 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3278 * ps + x0 y: 0.2203 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1720 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2917 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans227pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.3259 * ps + x0 y: 0.2872 * ps + y0;
		lineToX: 0.2980 * ps + x0 y: 0.2604 * ps + y0;
		curveToX: 0.2790 * ps + x0 y: 0.2462 * ps + y0 control1X: 0.2909 * ps + x0 y: 0.2539 * ps + y0 control2X: 0.2845 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.2648 * ps + x0 y: 0.2413 * ps + y0 control1X: 0.2738 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.2690 * ps + x0 y: 0.2413 * ps + y0;
		curveToX: 0.2375 * ps + x0 y: 0.2594 * ps + y0 control1X: 0.2524 * ps + x0 y: 0.2413 * ps + y0 control2X: 0.2433 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.2277 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.2316 * ps + x0 y: 0.2711 * ps + y0 control2X: 0.2284 * ps + x0 y: 0.2903 * ps + y0;
		lineToX: 0.1667 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.1926 * ps + x0 y: 0.2155 * ps + y0 control1X: 0.1673 * ps + x0 y: 0.2731 * ps + y0 control2X: 0.1760 * ps + x0 y: 0.2392 * ps + y0;
		curveToX: 0.2619 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.2092 * ps + x0 y: 0.1914 * ps + y0 control2X: 0.2323 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.2961 * ps + x0 y: 0.1862 * ps + y0 control1X: 0.2743 * ps + x0 y: 0.1793 * ps + y0 control2X: 0.2857 * ps + x0 y: 0.1816 * ps + y0;
		curveToX: 0.3298 * ps + x0 y: 0.2091 * ps + y0 control1X: 0.3065 * ps + x0 y: 0.1907 * ps + y0 control2X: 0.3177 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3576 * ps + x0 y: 0.2360 * ps + y0;
		curveToX: 0.3761 * ps + x0 y: 0.2506 * ps + y0 control1X: 0.3648 * ps + x0 y: 0.2425 * ps + y0 control2X: 0.3709 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.3908 * ps + x0 y: 0.2550 * ps + y0 control1X: 0.3817 * ps + x0 y: 0.2535 * ps + y0 control2X: 0.3866 * ps + x0 y: 0.2550 * ps + y0;
		curveToX: 0.4181 * ps + x0 y: 0.2374 * ps + y0 control1X: 0.4032 * ps + x0 y: 0.2550 * ps + y0 control2X: 0.4123 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.4279 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.4240 * ps + x0 y: 0.2254 * ps + y0 control2X: 0.4273 * ps + x0 y: 0.2060 * ps + y0;
		lineToX: 0.4889 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.4631 * ps + x0 y: 0.2814 * ps + y0 control1X: 0.4883 * ps + x0 y: 0.2233 * ps + y0 control2X: 0.4797 * ps + x0 y: 0.2573 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.4465 * ps + x0 y: 0.3051 * ps + y0 control2X: 0.4233 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.3595 * ps + x0 y: 0.3102 * ps + y0 control1X: 0.3814 * ps + x0 y: 0.3170 * ps + y0 control2X: 0.3700 * ps + x0 y: 0.3147 * ps + y0;
		curveToX: 0.3259 * ps + x0 y: 0.2872 * ps + y0 control1X: 0.3491 * ps + x0 y: 0.3056 * ps + y0 control2X: 0.3379 * ps + x0 y: 0.2980 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans228pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.3737 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4728 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4728 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3737 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3737 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.1828 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2819 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2819 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1828 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1828 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans229pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0;
		moveToX: 0.4020 * ps + x0 y: 0.2120 * ps + y0;
		curveToX: 0.3805 * ps + x0 y: 0.1598 * ps + y0 control1X: 0.4020 * ps + x0 y: 0.1915 * ps + y0 control2X: 0.3949 * ps + x0 y: 0.1741 * ps + y0;
		curveToX: 0.3278 * ps + x0 y: 0.1383 * ps + y0 control1X: 0.3662 * ps + x0 y: 0.1455 * ps + y0 control2X: 0.3486 * ps + x0 y: 0.1383 * ps + y0;
		curveToX: 0.2746 * ps + x0 y: 0.1598 * ps + y0 control1X: 0.3066 * ps + x0 y: 0.1383 * ps + y0 control2X: 0.2889 * ps + x0 y: 0.1455 * ps + y0;
		curveToX: 0.2536 * ps + x0 y: 0.2120 * ps + y0 control1X: 0.2606 * ps + x0 y: 0.1738 * ps + y0 control2X: 0.2536 * ps + x0 y: 0.1912 * ps + y0;
		curveToX: 0.2746 * ps + x0 y: 0.2653 * ps + y0 control1X: 0.2536 * ps + x0 y: 0.2332 * ps + y0 control2X: 0.2606 * ps + x0 y: 0.2509 * ps + y0;
		curveToX: 0.3278 * ps + x0 y: 0.2863 * ps + y0 control1X: 0.2889 * ps + x0 y: 0.2793 * ps + y0 control2X: 0.3066 * ps + x0 y: 0.2863 * ps + y0;
		curveToX: 0.3805 * ps + x0 y: 0.2648 * ps + y0 control1X: 0.3486 * ps + x0 y: 0.2863 * ps + y0 control2X: 0.3662 * ps + x0 y: 0.2791 * ps + y0;
		curveToX: 0.4020 * ps + x0 y: 0.2120 * ps + y0 control1X: 0.3949 * ps + x0 y: 0.2505 * ps + y0 control2X: 0.4020 * ps + x0 y: 0.2329 * ps + y0;
		moveToX: 0.4616 * ps + x0 y: 0.2120 * ps + y0;
		curveToX: 0.4225 * ps + x0 y: 0.3073 * ps + y0 control1X: 0.4616 * ps + x0 y: 0.2495 * ps + y0 control2X: 0.4486 * ps + x0 y: 0.2812 * ps + y0;
		curveToX: 0.3278 * ps + x0 y: 0.3463 * ps + y0 control1X: 0.3968 * ps + x0 y: 0.3333 * ps + y0 control2X: 0.3652 * ps + x0 y: 0.3463 * ps + y0;
		curveToX: 0.2326 * ps + x0 y: 0.3073 * ps + y0 control1X: 0.2904 * ps + x0 y: 0.3463 * ps + y0 control2X: 0.2586 * ps + x0 y: 0.3333 * ps + y0;
		curveToX: 0.1940 * ps + x0 y: 0.2120 * ps + y0 control1X: 0.2069 * ps + x0 y: 0.2812 * ps + y0 control2X: 0.1940 * ps + x0 y: 0.2495 * ps + y0;
		curveToX: 0.2326 * ps + x0 y: 0.1173 * ps + y0 control1X: 0.1940 * ps + x0 y: 0.1746 * ps + y0 control2X: 0.2069 * ps + x0 y: 0.1430 * ps + y0;
		curveToX: 0.3278 * ps + x0 y: 0.0783 * ps + y0 control1X: 0.2586 * ps + x0 y: 0.0913 * ps + y0 control2X: 0.2904 * ps + x0 y: 0.0783 * ps + y0;
		curveToX: 0.4225 * ps + x0 y: 0.1173 * ps + y0 control1X: 0.3652 * ps + x0 y: 0.0783 * ps + y0 control2X: 0.3968 * ps + x0 y: 0.0913 * ps + y0;
		curveToX: 0.4616 * ps + x0 y: 0.2120 * ps + y0 control1X: 0.4486 * ps + x0 y: 0.1430 * ps + y0 control2X: 0.4616 * ps + x0 y: 0.1746 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans230pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.8766 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.8356 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.8763 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.8626 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.7287 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.8089 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.7733 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.6086 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.6789 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.6388 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.5568 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.5786 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.5614 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.8766 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 0.5236 * ps + x0 y: 0.4860 * ps + y0;
		curveToX: 0.6115 * ps + x0 y: 0.4191 * ps + y0 control1X: 0.5477 * ps + x0 y: 0.4567 * ps + y0 control2X: 0.5770 * ps + x0 y: 0.4344 * ps + y0;
		curveToX: 0.7277 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.6460 * ps + x0 y: 0.4038 * ps + y0 control2X: 0.6847 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.9025 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.8013 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.8595 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.9670 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.9455 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.9670 * ps + x0 y: 0.5789 * ps + y0;
		lineToX: 0.9670 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.5539 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.6095 * ps + x0 y: 0.8463 * ps + y0 control1X: 0.5578 * ps + x0 y: 0.7664 * ps + y0 control2X: 0.5763 * ps + x0 y: 0.8138 * ps + y0;
		curveToX: 0.7487 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.6427 * ps + x0 y: 0.8789 * ps + y0 control2X: 0.6891 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.8483 * ps + x0 y: 0.8825 * ps + y0 control1X: 0.7826 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.8158 * ps + x0 y: 0.8909 * ps + y0;
		curveToX: 0.9460 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.8809 * ps + x0 y: 0.8737 * ps + y0 control2X: 0.9134 * ps + x0 y: 0.8606 * ps + y0;
		lineToX: 0.9460 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.8464 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.9134 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.8802 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.7438 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.8125 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.7783 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.9410 * ps + y0 control1X: 0.6895 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.6421 * ps + x0 y: 0.9606 * ps + y0;
		curveToX: 0.5036 * ps + x0 y: 0.8532 * ps + y0 control1X: 0.5614 * ps + x0 y: 0.9212 * ps + y0 control2X: 0.5287 * ps + x0 y: 0.8919 * ps + y0;
		curveToX: 0.4152 * ps + x0 y: 0.9410 * ps + y0 control1X: 0.4798 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4504 * ps + x0 y: 0.9215 * ps + y0;
		curveToX: 0.2926 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3801 * ps + x0 y: 0.9606 * ps + y0 control2X: 0.3392 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1491 * ps + x0 y: 0.9249 * ps + y0 control1X: 0.2311 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1833 * ps + x0 y: 0.9552 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1149 * ps + x0 y: 0.8943 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8517 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4450 * ps + x0 y: 0.4196 * ps + y0 control1X: 0.3701 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4105 * ps + x0 y: 0.4039 * ps + y0;
		curveToX: 0.5236 * ps + x0 y: 0.4860 * ps + y0 control1X: 0.4798 * ps + x0 y: 0.4352 * ps + y0 control2X: 0.5060 * ps + x0 y: 0.4573 * ps + y0;
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans231pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5256 * ps + x0 y: 0.4303 * ps + y0;
		lineToX: 0.5256 * ps + x0 y: 0.5143 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.4830 * ps + y0 control1X: 0.5002 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.4746 * ps + x0 y: 0.4899 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4235 * ps + x0 y: 0.4759 * ps + y0 control2X: 0.3978 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.3135 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2682 * ps + x0 y: 0.4908 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2038 * ps + x0 y: 0.5647 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6165 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7500 * ps + y0 control2X: 0.2038 * ps + x0 y: 0.8019 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2682 * ps + x0 y: 0.8758 * ps + y0 control2X: 0.3135 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.8839 * ps + y0 control1X: 0.3978 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4235 * ps + x0 y: 0.8908 * ps + y0;
		curveToX: 0.5256 * ps + x0 y: 0.8522 * ps + y0 control1X: 0.4746 * ps + x0 y: 0.8768 * ps + y0 control2X: 0.5002 * ps + x0 y: 0.8662 * ps + y0;
		lineToX: 0.5256 * ps + x0 y: 0.9352 * ps + y0;
		curveToX: 0.4474 * ps + x0 y: 0.9616 * ps + y0 control1X: 0.5005 * ps + x0 y: 0.9469 * ps + y0 control2X: 0.4745 * ps + x0 y: 0.9557 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4207 * ps + x0 y: 0.9674 * ps + y0 control2X: 0.3923 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1657 * ps + x0 y: 0.8927 * ps + y0 control1X: 0.2796 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2142 * ps + x0 y: 0.9445 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1172 * ps + x0 y: 0.8409 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7711 * ps + y0;
		curveToX: 0.1662 * ps + x0 y: 0.4728 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5940 * ps + y0 control2X: 0.1174 * ps + x0 y: 0.5239 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2153 * ps + x0 y: 0.4217 * ps + y0 control2X: 0.2826 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.4049 * ps + y0 control1X: 0.3955 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4225 * ps + x0 y: 0.3991 * ps + y0;
		curveToX: 0.5256 * ps + x0 y: 0.4303 * ps + y0 control1X: 0.4753 * ps + x0 y: 0.4104 * ps + y0 control2X: 0.5008 * ps + x0 y: 0.4189 * ps + y0;
		moveToX: 0.3986 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.4386 * ps + x0 y: 1.0118 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.9764 * ps + y0 control2X: 0.4299 * ps + x0 y: 0.9949 * ps + y0;
		curveToX: 0.4518 * ps + x0 y: 1.0612 * ps + y0 control1X: 0.4474 * ps + x0 y: 1.0291 * ps + y0 control2X: 0.4518 * ps + x0 y: 1.0455 * ps + y0;
		curveToX: 0.4225 * ps + x0 y: 1.1266 * ps + y0 control1X: 0.4518 * ps + x0 y: 1.0901 * ps + y0 control2X: 0.4421 * ps + x0 y: 1.1119 * ps + y0;
		curveToX: 0.3356 * ps + x0 y: 1.1491 * ps + y0 control1X: 0.4030 * ps + x0 y: 1.1416 * ps + y0 control2X: 0.3740 * ps + x0 y: 1.1491 * ps + y0;
		curveToX: 0.2917 * ps + x0 y: 1.1461 * ps + y0 control1X: 0.3206 * ps + x0 y: 1.1491 * ps + y0 control2X: 0.3060 * ps + x0 y: 1.1481 * ps + y0;
		curveToX: 0.2497 * ps + x0 y: 1.1373 * ps + y0 control1X: 0.2777 * ps + x0 y: 1.1442 * ps + y0 control2X: 0.2637 * ps + x0 y: 1.1412 * ps + y0;
		lineToX: 0.2497 * ps + x0 y: 1.0734 * ps + y0;
		curveToX: 0.2843 * ps + x0 y: 1.0851 * ps + y0 control1X: 0.2607 * ps + x0 y: 1.0789 * ps + y0 control2X: 0.2723 * ps + x0 y: 1.0828 * ps + y0;
		curveToX: 0.3254 * ps + x0 y: 1.0890 * ps + y0 control1X: 0.2964 * ps + x0 y: 1.0877 * ps + y0 control2X: 0.3101 * ps + x0 y: 1.0890 * ps + y0;
		curveToX: 0.3688 * ps + x0 y: 1.0773 * ps + y0 control1X: 0.3446 * ps + x0 y: 1.0890 * ps + y0 control2X: 0.3591 * ps + x0 y: 1.0851 * ps + y0;
		curveToX: 0.3835 * ps + x0 y: 1.0431 * ps + y0 control1X: 0.3786 * ps + x0 y: 1.0695 * ps + y0 control2X: 0.3835 * ps + x0 y: 1.0581 * ps + y0;
		curveToX: 0.3727 * ps + x0 y: 1.0070 * ps + y0 control1X: 0.3835 * ps + x0 y: 1.0333 * ps + y0 control2X: 0.3799 * ps + x0 y: 1.0213 * ps + y0;
		curveToX: 0.3405 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.3659 * ps + x0 y: 0.9930 * ps + y0 control2X: 0.3551 * ps + x0 y: 0.9760 * ps + y0;
		lineToX: 0.3986 * ps + x0 y: 0.9562 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans232pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.1867 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.2424 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.1906 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.2092 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2759 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.3224 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4821 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4499 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.5788 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.5147 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.5469 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.5788 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.5466 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.5135 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.4115 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1691 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6881 * ps + y0 control1X: 0.1183 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.1652 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5986 * ps + y0 control2X: 0.1170 * ps + x0 y: 0.5276 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4927 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5783 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.5998 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.4684 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.5093 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.4954 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4417 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4063 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2414 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2717 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.1942 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 0.2848 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4225 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.3478 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2848 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans233pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.1867 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.2424 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.1906 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.2092 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2759 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.3224 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4821 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4499 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.5788 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.5147 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.5469 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.5788 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.5466 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.5135 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.4115 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1691 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6881 * ps + y0 control1X: 0.1183 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.1652 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5986 * ps + y0 control2X: 0.1170 * ps + x0 y: 0.5276 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4927 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5783 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.5998 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.4684 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.5093 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.4954 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4417 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4063 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2414 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2717 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.1942 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 0.4235 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.5207 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3615 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2868 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4235 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans234pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.1867 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.2424 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.1906 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.2092 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2759 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.3224 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4821 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4499 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.5788 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.5147 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.5469 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.5788 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.5466 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.5135 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.4115 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1691 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6881 * ps + y0 control1X: 0.1183 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.1652 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5986 * ps + y0 control2X: 0.1170 * ps + x0 y: 0.5276 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4927 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5783 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.5998 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.4684 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.5093 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.4954 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4417 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4063 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2414 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2717 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.1942 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 0.3195 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3918 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.5114 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4435 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3556 * ps + x0 y: 0.2203 * ps + y0;
		lineToX: 0.2677 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1999 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3195 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans235pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.7042 * ps + y0;
		lineToX: 0.1867 * ps + x0 y: 0.7042 * ps + y0;
		curveToX: 0.2424 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.1906 * ps + x0 y: 0.7661 * ps + y0 control2X: 0.2092 * ps + x0 y: 0.8133 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2759 * ps + x0 y: 0.8781 * ps + y0 control2X: 0.3224 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4821 * ps + x0 y: 0.8815 * ps + y0 control1X: 0.4165 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4499 * ps + x0 y: 0.8899 * ps + y0;
		curveToX: 0.5788 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.5147 * ps + x0 y: 0.8730 * ps + y0 control2X: 0.5469 * ps + x0 y: 0.8603 * ps + y0;
		lineToX: 0.5788 * ps + x0 y: 0.9283 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.9596 * ps + y0 control1X: 0.5466 * ps + x0 y: 0.9420 * ps + y0 control2X: 0.5135 * ps + x0 y: 0.9524 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.9668 * ps + y0 control2X: 0.4115 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1691 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6881 * ps + y0 control1X: 0.1183 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.1652 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5986 * ps + y0 control2X: 0.1170 * ps + x0 y: 0.5276 * ps + y0;
		curveToX: 0.3610 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2137 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.2790 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.4674 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4927 * ps + x0 y: 0.4199 * ps + y0;
		curveToX: 0.5998 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5783 * ps + x0 y: 0.5146 * ps + y0 control2X: 0.5998 * ps + x0 y: 0.5789 * ps + y0;
		moveToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		curveToX: 0.4684 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.5093 * ps + x0 y: 0.5848 * ps + y0 control2X: 0.4954 * ps + x0 y: 0.5455 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4417 * ps + x0 y: 0.4869 * ps + y0 control2X: 0.4063 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2414 * ps + x0 y: 0.5148 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2717 * ps + x0 y: 0.4865 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.6344 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.5431 * ps + y0 control2X: 0.1942 * ps + x0 y: 0.5830 * ps + y0;
		lineToX: 0.5099 * ps + x0 y: 0.6339 * ps + y0;
		moveToX: 0.4015 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.5007 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.5007 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.2106 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3097 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3097 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2106 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2106 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans236pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.1061 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2438 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1691 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.0099 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.1061 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans237pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.2448 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3420 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.1828 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1081 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2448 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans238pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.1408 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2131 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3327 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2648 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1769 * ps + x0 y: 0.2203 * ps + y0;
		lineToX: 0.0890 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.0212 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans239pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2218 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1320 * ps + x0 y: 0.4093 * ps + y0;
		moveToX: 0.2228 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3219 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3219 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2228 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2228 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.0319 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.1310 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.1310 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.0319 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.0319 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans240pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4469 * ps + x0 y: 0.5094 * ps + y0;
		curveToX: 0.4010 * ps + x0 y: 0.4972 * ps + y0 control1X: 0.4307 * ps + x0 y: 0.5039 * ps + y0 control2X: 0.4154 * ps + x0 y: 0.4998 * ps + y0;
		curveToX: 0.3595 * ps + x0 y: 0.4933 * ps + y0 control1X: 0.3867 * ps + x0 y: 0.4946 * ps + y0 control2X: 0.3729 * ps + x0 y: 0.4933 * ps + y0;
		curveToX: 0.2326 * ps + x0 y: 0.5460 * ps + y0 control1X: 0.3052 * ps + x0 y: 0.4933 * ps + y0 control2X: 0.2629 * ps + x0 y: 0.5109 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.2026 * ps + x0 y: 0.5812 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6305 * ps + y0;
		curveToX: 0.2301 * ps + x0 y: 0.8400 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7552 * ps + y0 control2X: 0.2018 * ps + x0 y: 0.8038 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2585 * ps + x0 y: 0.8761 * ps + y0 control2X: 0.2964 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4572 * ps + x0 y: 0.8400 * ps + y0 control1X: 0.3911 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4289 * ps + x0 y: 0.8761 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.4855 * ps + x0 y: 0.8038 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7552 * ps + y0;
		curveToX: 0.4865 * ps + x0 y: 0.5885 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6543 * ps + y0 control2X: 0.4953 * ps + x0 y: 0.6191 * ps + y0;
		curveToX: 0.4469 * ps + x0 y: 0.5094 * ps + y0 control1X: 0.4777 * ps + x0 y: 0.5579 * ps + y0 control2X: 0.4645 * ps + x0 y: 0.5315 * ps + y0;
		moveToX: 0.4777 * ps + x0 y: 0.3991 * ps + y0;
		curveToX: 0.5671 * ps + x0 y: 0.5392 * ps + y0 control1X: 0.5187 * ps + x0 y: 0.4456 * ps + y0 control2X: 0.5485 * ps + x0 y: 0.4923 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.5856 * ps + x0 y: 0.5861 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.6377 * ps + y0;
		curveToX: 0.5256 * ps + x0 y: 0.8947 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7770 * ps + y0 control2X: 0.5718 * ps + x0 y: 0.8439 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4793 * ps + x0 y: 0.9451 * ps + y0 control2X: 0.4188 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1618 * ps + x0 y: 0.8947 * ps + y0 control1X: 0.2687 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2080 * ps + x0 y: 0.9451 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.1159 * ps + x0 y: 0.8439 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7770 * ps + y0;
		curveToX: 0.1603 * ps + x0 y: 0.4962 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.6126 * ps + y0 control2X: 0.1154 * ps + x0 y: 0.5467 * ps + y0;
		curveToX: 0.3356 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.2052 * ps + x0 y: 0.4454 * ps + y0 control2X: 0.2637 * ps + x0 y: 0.4200 * ps + y0;
		curveToX: 0.3571 * ps + x0 y: 0.4215 * ps + y0 control1X: 0.3415 * ps + x0 y: 0.4200 * ps + y0 control2X: 0.3486 * ps + x0 y: 0.4205 * ps + y0;
		curveToX: 0.3903 * ps + x0 y: 0.4249 * ps + y0 control1X: 0.3656 * ps + x0 y: 0.4222 * ps + y0 control2X: 0.3766 * ps + x0 y: 0.4233 * ps + y0;
		lineToX: 0.3127 * ps + x0 y: 0.3370 * ps + y0;
		lineToX: 0.1569 * ps + x0 y: 0.3893 * ps + y0;
		lineToX: 0.1408 * ps + x0 y: 0.3409 * ps + y0;
		lineToX: 0.2780 * ps + x0 y: 0.2960 * ps + y0;
		lineToX: 0.1896 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3010 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3630 * ps + x0 y: 0.2672 * ps + y0;
		lineToX: 0.5256 * ps + x0 y: 0.2130 * ps + y0;
		lineToX: 0.5417 * ps + x0 y: 0.2604 * ps + y0;
		lineToX: 0.3976 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.4777 * ps + x0 y: 0.3991 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans241pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0;
		lineToX: 0.5866 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4968 * ps + x0 y: 0.6290 * ps + y0;
		curveToX: 0.4665 * ps + x0 y: 0.5128 * ps + y0 control1X: 0.4968 * ps + x0 y: 0.5773 * ps + y0 control2X: 0.4867 * ps + x0 y: 0.5385 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.4463 * ps + x0 y: 0.4871 * ps + y0 control2X: 0.4160 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.5206 * ps + y0 control1X: 0.3272 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.2889 * ps + x0 y: 0.4897 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6471 * ps + y0 control1X: 0.2329 * ps + x0 y: 0.5516 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.5937 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4943 * ps + y0;
		curveToX: 0.2946 * ps + x0 y: 0.4205 * ps + y0 control1X: 0.2404 * ps + x0 y: 0.4614 * ps + y0 control2X: 0.2656 * ps + x0 y: 0.4368 * ps + y0;
		curveToX: 0.3957 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3239 * ps + x0 y: 0.4043 * ps + y0 control2X: 0.3576 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5383 * ps + x0 y: 0.4547 * ps + y0 control1X: 0.4585 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5060 * ps + x0 y: 0.4157 * ps + y0;
		curveToX: 0.5866 * ps + x0 y: 0.6261 * ps + y0 control1X: 0.5705 * ps + x0 y: 0.4935 * ps + y0 control2X: 0.5866 * ps + x0 y: 0.5506 * ps + y0;
		moveToX: 0.3600 * ps + x0 y: 0.2872 * ps + y0;
		lineToX: 0.3322 * ps + x0 y: 0.2604 * ps + y0;
		curveToX: 0.3132 * ps + x0 y: 0.2462 * ps + y0 control1X: 0.3250 * ps + x0 y: 0.2539 * ps + y0 control2X: 0.3187 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.2990 * ps + x0 y: 0.2413 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.2413 * ps + y0;
		curveToX: 0.2717 * ps + x0 y: 0.2594 * ps + y0 control1X: 0.2866 * ps + x0 y: 0.2413 * ps + y0 control2X: 0.2775 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.2619 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.2658 * ps + x0 y: 0.2711 * ps + y0 control2X: 0.2625 * ps + x0 y: 0.2903 * ps + y0;
		lineToX: 0.2009 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.2267 * ps + x0 y: 0.2155 * ps + y0 control1X: 0.2015 * ps + x0 y: 0.2731 * ps + y0 control2X: 0.2101 * ps + x0 y: 0.2392 * ps + y0;
		curveToX: 0.2961 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.2433 * ps + x0 y: 0.1914 * ps + y0 control2X: 0.2664 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.3302 * ps + x0 y: 0.1862 * ps + y0 control1X: 0.3084 * ps + x0 y: 0.1793 * ps + y0 control2X: 0.3198 * ps + x0 y: 0.1816 * ps + y0;
		curveToX: 0.3639 * ps + x0 y: 0.2091 * ps + y0 control1X: 0.3407 * ps + x0 y: 0.1907 * ps + y0 control2X: 0.3519 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3918 * ps + x0 y: 0.2360 * ps + y0;
		curveToX: 0.4103 * ps + x0 y: 0.2506 * ps + y0 control1X: 0.3989 * ps + x0 y: 0.2425 * ps + y0 control2X: 0.4051 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.4250 * ps + x0 y: 0.2550 * ps + y0 control1X: 0.4159 * ps + x0 y: 0.2535 * ps + y0 control2X: 0.4207 * ps + x0 y: 0.2550 * ps + y0;
		curveToX: 0.4523 * ps + x0 y: 0.2374 * ps + y0 control1X: 0.4373 * ps + x0 y: 0.2550 * ps + y0 control2X: 0.4465 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.4621 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.4582 * ps + x0 y: 0.2254 * ps + y0 control2X: 0.4614 * ps + x0 y: 0.2060 * ps + y0;
		lineToX: 0.5231 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.4972 * ps + x0 y: 0.2814 * ps + y0 control1X: 0.5225 * ps + x0 y: 0.2233 * ps + y0 control2X: 0.5138 * ps + x0 y: 0.2573 * ps + y0;
		curveToX: 0.4279 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.4806 * ps + x0 y: 0.3051 * ps + y0 control2X: 0.4575 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.3102 * ps + y0 control1X: 0.4155 * ps + x0 y: 0.3170 * ps + y0 control2X: 0.4041 * ps + x0 y: 0.3147 * ps + y0;
		curveToX: 0.3600 * ps + x0 y: 0.2872 * ps + y0 control1X: 0.3833 * ps + x0 y: 0.3056 * ps + y0 control2X: 0.3721 * ps + x0 y: 0.2980 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans242pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		moveToX: 0.2731 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4108 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.3361 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1769 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2731 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans243pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		moveToX: 0.4118 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.5090 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2751 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4118 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans244pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		moveToX: 0.3078 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3801 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4997 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4318 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.2203 * ps + y0;
		lineToX: 0.2560 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1882 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3078 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans245pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		moveToX: 0.3420 * ps + x0 y: 0.2872 * ps + y0;
		lineToX: 0.3141 * ps + x0 y: 0.2604 * ps + y0;
		curveToX: 0.2951 * ps + x0 y: 0.2462 * ps + y0 control1X: 0.3070 * ps + x0 y: 0.2539 * ps + y0 control2X: 0.3006 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.2413 * ps + y0 control1X: 0.2899 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.2852 * ps + x0 y: 0.2413 * ps + y0;
		curveToX: 0.2536 * ps + x0 y: 0.2594 * ps + y0 control1X: 0.2686 * ps + x0 y: 0.2413 * ps + y0 control2X: 0.2594 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.2438 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.2477 * ps + x0 y: 0.2711 * ps + y0 control2X: 0.2445 * ps + x0 y: 0.2903 * ps + y0;
		lineToX: 0.1828 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.2087 * ps + x0 y: 0.2155 * ps + y0 control1X: 0.1834 * ps + x0 y: 0.2731 * ps + y0 control2X: 0.1921 * ps + x0 y: 0.2392 * ps + y0;
		curveToX: 0.2780 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.2253 * ps + x0 y: 0.1914 * ps + y0 control2X: 0.2484 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.3122 * ps + x0 y: 0.1862 * ps + y0 control1X: 0.2904 * ps + x0 y: 0.1793 * ps + y0 control2X: 0.3018 * ps + x0 y: 0.1816 * ps + y0;
		curveToX: 0.3459 * ps + x0 y: 0.2091 * ps + y0 control1X: 0.3226 * ps + x0 y: 0.1907 * ps + y0 control2X: 0.3338 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3737 * ps + x0 y: 0.2360 * ps + y0;
		curveToX: 0.3923 * ps + x0 y: 0.2506 * ps + y0 control1X: 0.3809 * ps + x0 y: 0.2425 * ps + y0 control2X: 0.3871 * ps + x0 y: 0.2474 * ps + y0;
		curveToX: 0.4069 * ps + x0 y: 0.2550 * ps + y0 control1X: 0.3978 * ps + x0 y: 0.2535 * ps + y0 control2X: 0.4027 * ps + x0 y: 0.2550 * ps + y0;
		curveToX: 0.4343 * ps + x0 y: 0.2374 * ps + y0 control1X: 0.4193 * ps + x0 y: 0.2550 * ps + y0 control2X: 0.4284 * ps + x0 y: 0.2492 * ps + y0;
		curveToX: 0.4440 * ps + x0 y: 0.1793 * ps + y0 control1X: 0.4401 * ps + x0 y: 0.2254 * ps + y0 control2X: 0.4434 * ps + x0 y: 0.2060 * ps + y0;
		lineToX: 0.5051 * ps + x0 y: 0.1793 * ps + y0;
		curveToX: 0.4792 * ps + x0 y: 0.2814 * ps + y0 control1X: 0.5044 * ps + x0 y: 0.2233 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.2573 * ps + y0;
		curveToX: 0.4098 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.4626 * ps + x0 y: 0.3051 * ps + y0 control2X: 0.4395 * ps + x0 y: 0.3170 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.3102 * ps + y0 control1X: 0.3975 * ps + x0 y: 0.3170 * ps + y0 control2X: 0.3861 * ps + x0 y: 0.3147 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.2872 * ps + y0 control1X: 0.3652 * ps + x0 y: 0.3056 * ps + y0 control2X: 0.3540 * ps + x0 y: 0.2980 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans246pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.5289 * ps + y0 control1X: 0.2957 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.4912 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.5664 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6178 * ps + y0;
		curveToX: 0.2292 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7487 * ps + y0 control2X: 0.2015 * ps + x0 y: 0.8003 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.2954 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.5294 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6185 * ps + y0 control2X: 0.4857 * ps + x0 y: 0.5672 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4297 * ps + x0 y: 0.4913 * ps + y0 control2X: 0.3918 * ps + x0 y: 0.4723 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4220 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4834 * ps + x0 y: 0.4215 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5726 * ps + x0 y: 0.5231 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.5934 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2655 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2039 * ps + x0 y: 0.9450 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1151 * ps + x0 y: 0.8431 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7728 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		moveToX: 0.3898 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4889 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4889 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3898 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3898 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.1989 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2980 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2980 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans247pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3967 * ps + x0 y: 0.4020 * ps + y0;
		lineToX: 0.5168 * ps + x0 y: 0.4020 * ps + y0;
		lineToX: 0.5168 * ps + x0 y: 0.5221 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.5221 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.4020 * ps + y0;
		moveToX: 0.3967 * ps + x0 y: 0.7633 * ps + y0;
		lineToX: 0.5168 * ps + x0 y: 0.7633 * ps + y0;
		lineToX: 0.5168 * ps + x0 y: 0.8829 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.8829 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.7633 * ps + y0;
		moveToX: 0.1437 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6012 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans248pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4797 * ps + x0 y: 0.5651 * ps + y0;
		lineToX: 0.2419 * ps + x0 y: 0.8541 * ps + y0;
		curveToX: 0.2868 * ps + x0 y: 0.8844 * ps + y0 control1X: 0.2552 * ps + x0 y: 0.8678 * ps + y0 control2X: 0.2702 * ps + x0 y: 0.8779 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.3037 * ps + x0 y: 0.8909 * ps + y0 control2X: 0.3228 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.8375 * ps + y0 control1X: 0.3918 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4297 * ps + x0 y: 0.8753 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.4857 * ps + x0 y: 0.7998 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.7483 * ps + y0;
		curveToX: 0.4948 * ps + x0 y: 0.6163 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.6575 * ps + y0 control2X: 0.4981 * ps + x0 y: 0.6352 * ps + y0;
		curveToX: 0.4797 * ps + x0 y: 0.5651 * ps + y0 control1X: 0.4915 * ps + x0 y: 0.5971 * ps + y0 control2X: 0.4865 * ps + x0 y: 0.5800 * ps + y0;
		moveToX: 0.4450 * ps + x0 y: 0.5123 * ps + y0;
		curveToX: 0.3991 * ps + x0 y: 0.4825 * ps + y0 control1X: 0.4313 * ps + x0 y: 0.4990 * ps + y0 control2X: 0.4160 * ps + x0 y: 0.4891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.3825 * ps + x0 y: 0.4757 * ps + y0 control2X: 0.3641 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2287 * ps + x0 y: 0.5299 * ps + y0 control1X: 0.2948 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2564 * ps + x0 y: 0.4915 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6901 * ps + y0 control1X: 0.2013 * ps + x0 y: 0.5680 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6214 * ps + y0;
		curveToX: 0.1921 * ps + x0 y: 0.7521 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7135 * ps + y0 control2X: 0.1891 * ps + x0 y: 0.7342 * ps + y0;
		curveToX: 0.2067 * ps + x0 y: 0.8014 * ps + y0 control1X: 0.1953 * ps + x0 y: 0.7700 * ps + y0 control2X: 0.2002 * ps + x0 y: 0.7864 * ps + y0;
		lineToX: 0.4450 * ps + x0 y: 0.5123 * ps + y0;
		moveToX: 0.1457 * ps + x0 y: 0.8751 * ps + y0;
		curveToX: 0.1061 * ps + x0 y: 0.7897 * ps + y0 control1X: 0.1281 * ps + x0 y: 0.8501 * ps + y0 control2X: 0.1149 * ps + x0 y: 0.8216 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.0973 * ps + x0 y: 0.7575 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7220 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5934 * ps + y0 control2X: 0.1151 * ps + x0 y: 0.5231 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2039 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2655 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4279 * ps + x0 y: 0.4088 * ps + y0 control1X: 0.3742 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4022 * ps + x0 y: 0.4004 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.4464 * ps + y0 control1X: 0.4539 * ps + x0 y: 0.4170 * ps + y0 control2X: 0.4779 * ps + x0 y: 0.4295 * ps + y0;
		lineToX: 0.5676 * ps + x0 y: 0.3644 * ps + y0;
		lineToX: 0.6139 * ps + x0 y: 0.4030 * ps + y0;
		lineToX: 0.5426 * ps + x0 y: 0.4904 * ps + y0;
		curveToX: 0.5817 * ps + x0 y: 0.5763 * ps + y0 control1X: 0.5599 * ps + x0 y: 0.5154 * ps + y0 control2X: 0.5729 * ps + x0 y: 0.5441 * ps + y0;
		curveToX: 0.5949 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5905 * ps + x0 y: 0.6085 * ps + y0 control2X: 0.5949 * ps + x0 y: 0.6442 * ps + y0;
		curveToX: 0.5280 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.5949 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.5726 * ps + x0 y: 0.8431 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4834 * ps + x0 y: 0.9450 * ps + y0 control2X: 0.4220 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2575 * ps + x0 y: 0.9576 * ps + y0 control1X: 0.3127 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2839 * ps + x0 y: 0.9661 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.9200 * ps + y0 control1X: 0.2315 * ps + x0 y: 0.9492 * ps + y0 control2X: 0.2082 * ps + x0 y: 0.9367 * ps + y0;
		lineToX: 0.1198 * ps + x0 y: 1.0021 * ps + y0;
		lineToX: 0.0729 * ps + x0 y: 0.9640 * ps + y0;
		lineToX: 0.1457 * ps + x0 y: 0.8751 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans249pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7369 * ps + y0;
		curveToX: 0.2428 * ps + x0 y: 0.8536 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7887 * ps + y0 control2X: 0.2227 * ps + x0 y: 0.8276 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2630 * ps + x0 y: 0.8794 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.3822 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4204 * ps + x0 y: 0.8768 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7194 * ps + y0 control1X: 0.4767 * ps + x0 y: 0.8149 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7728 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.8722 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.9054 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9301 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.9624 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.9117 * ps + y0 control1X: 0.2518 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.9508 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0 control1X: 0.1390 * ps + x0 y: 0.8727 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.8156 * ps + y0;
		moveToX: 0.2770 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4147 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.3400 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1808 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2770 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans250pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7369 * ps + y0;
		curveToX: 0.2428 * ps + x0 y: 0.8536 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7887 * ps + y0 control2X: 0.2227 * ps + x0 y: 0.8276 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2630 * ps + x0 y: 0.8794 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.3822 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4204 * ps + x0 y: 0.8768 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7194 * ps + y0 control1X: 0.4767 * ps + x0 y: 0.8149 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7728 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.8722 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.9054 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9301 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.9624 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.9117 * ps + y0 control1X: 0.2518 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.9508 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0 control1X: 0.1390 * ps + x0 y: 0.8727 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.8156 * ps + y0;
		moveToX: 0.4157 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.5129 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3537 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2790 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans251pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7369 * ps + y0;
		curveToX: 0.2428 * ps + x0 y: 0.8536 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7887 * ps + y0 control2X: 0.2227 * ps + x0 y: 0.8276 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2630 * ps + x0 y: 0.8794 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.3822 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4204 * ps + x0 y: 0.8768 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7194 * ps + y0 control1X: 0.4767 * ps + x0 y: 0.8149 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7728 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.8722 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.9054 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9301 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.9624 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.9117 * ps + y0 control1X: 0.2518 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.9508 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0 control1X: 0.1390 * ps + x0 y: 0.8727 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.8156 * ps + y0;
		moveToX: 0.3117 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3840 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.5036 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3478 * ps + x0 y: 0.2203 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.1921 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.3117 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans252pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0;
		lineToX: 0.1227 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.2126 * ps + x0 y: 0.7369 * ps + y0;
		curveToX: 0.2428 * ps + x0 y: 0.8536 * ps + y0 control1X: 0.2126 * ps + x0 y: 0.7887 * ps + y0 control2X: 0.2227 * ps + x0 y: 0.8276 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2630 * ps + x0 y: 0.8794 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4484 * ps + x0 y: 0.8458 * ps + y0 control1X: 0.3822 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4204 * ps + x0 y: 0.8768 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.7194 * ps + y0 control1X: 0.4767 * ps + x0 y: 0.8149 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.7728 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4909 * ps + x0 y: 0.8722 * ps + y0;
		curveToX: 0.4147 * ps + x0 y: 0.9464 * ps + y0 control1X: 0.4691 * ps + x0 y: 0.9054 * ps + y0 control2X: 0.4437 * ps + x0 y: 0.9301 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3861 * ps + x0 y: 0.9624 * ps + y0 control2X: 0.3527 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.9117 * ps + y0 control1X: 0.2518 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.9508 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.7404 * ps + y0 control1X: 0.1390 * ps + x0 y: 0.8727 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.8156 * ps + y0;
		moveToX: 0.3937 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4928 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4928 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3937 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3937 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.2028 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3019 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.3019 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2028 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.2028 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans253pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0;
		curveToX: 0.2853 * ps + x0 y: 1.1344 * ps + y0 control1X: 0.3342 * ps + x0 y: 1.0721 * ps + y0 control2X: 0.3094 * ps + x0 y: 1.1145 * ps + y0;
		curveToX: 0.1886 * ps + x0 y: 1.1642 * ps + y0 control1X: 0.2612 * ps + x0 y: 1.1543 * ps + y0 control2X: 0.2290 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.0890 * ps + y0;
		lineToX: 0.1696 * ps + x0 y: 1.0890 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 1.0714 * ps + y0 control1X: 0.1943 * ps + x0 y: 1.0890 * ps + y0 control2X: 0.2135 * ps + x0 y: 1.0831 * ps + y0;
		curveToX: 0.2726 * ps + x0 y: 0.9884 * ps + y0 control1X: 0.2409 * ps + x0 y: 1.0597 * ps + y0 control2X: 0.2560 * ps + x0 y: 1.0320 * ps + y0;
		lineToX: 0.2887 * ps + x0 y: 0.9474 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.8370 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0;
		moveToX: 0.4015 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.4987 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3395 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.2648 * ps + x0 y: 0.3400 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans254pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2189 * ps + x0 y: 0.8742 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4923 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.2378 * ps + x0 y: 0.4598 * ps + y0 control2X: 0.2616 * ps + x0 y: 0.4357 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3192 * ps + x0 y: 0.4041 * ps + y0 control2X: 0.3537 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.4601 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5140 * ps + x0 y: 0.4225 * ps + y0;
		curveToX: 0.6178 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5970 * ps + x0 y: 0.5280 * ps + y0 control2X: 0.6178 * ps + x0 y: 0.5973 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.6178 * ps + x0 y: 0.7692 * ps + y0 control2X: 0.5970 * ps + x0 y: 0.8385 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5140 * ps + x0 y: 0.9440 * ps + y0 control2X: 0.4601 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.3537 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.3192 * ps + x0 y: 0.9625 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8742 * ps + y0 control1X: 0.2616 * ps + x0 y: 0.9310 * ps + y0 control2X: 0.2378 * ps + x0 y: 0.9067 * ps + y0;
		moveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.5246 * ps + x0 y: 0.6172 * ps + y0 control2X: 0.5109 * ps + x0 y: 0.5654 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4565 * ps + x0 y: 0.4902 * ps + y0 control2X: 0.4193 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.3242 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2868 * ps + x0 y: 0.4902 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2324 * ps + x0 y: 0.5654 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.6172 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.7493 * ps + y0 control2X: 0.2324 * ps + x0 y: 0.8012 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2868 * ps + x0 y: 0.8764 * ps + y0 control2X: 0.3242 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4193 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4565 * ps + x0 y: 0.8764 * ps + y0;
		curveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5109 * ps + x0 y: 0.8012 * ps + y0 control2X: 0.5246 * ps + x0 y: 0.7493 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans255pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0;
		curveToX: 0.2853 * ps + x0 y: 1.1344 * ps + y0 control1X: 0.3342 * ps + x0 y: 1.0721 * ps + y0 control2X: 0.3094 * ps + x0 y: 1.1145 * ps + y0;
		curveToX: 0.1886 * ps + x0 y: 1.1642 * ps + y0 control1X: 0.2612 * ps + x0 y: 1.1543 * ps + y0 control2X: 0.2290 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.1642 * ps + y0;
		lineToX: 0.1169 * ps + x0 y: 1.0890 * ps + y0;
		lineToX: 0.1696 * ps + x0 y: 1.0890 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 1.0714 * ps + y0 control1X: 0.1943 * ps + x0 y: 1.0890 * ps + y0 control2X: 0.2135 * ps + x0 y: 1.0831 * ps + y0;
		curveToX: 0.2726 * ps + x0 y: 0.9884 * ps + y0 control1X: 0.2409 * ps + x0 y: 1.0597 * ps + y0 control2X: 0.2560 * ps + x0 y: 1.0320 * ps + y0;
		lineToX: 0.2887 * ps + x0 y: 0.9474 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.1628 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3337 * ps + x0 y: 0.8370 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.5998 * ps + x0 y: 0.4093 * ps + y0;
		lineToX: 0.3595 * ps + x0 y: 1.0070 * ps + y0;
		moveToX: 0.3796 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4787 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.4787 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.1984 * ps + y0;
		moveToX: 0.1886 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.1984 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.2970 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.1984 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans33pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1886 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.8322 * ps + y0;
		moveToX: 0.1886 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2878 * ps + x0 y: 0.5470 * ps + y0;
		lineToX: 0.2780 * ps + x0 y: 0.7213 * ps + y0;
		lineToX: 0.1989 * ps + x0 y: 0.7213 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.5470 * ps + y0;
		lineToX: 0.1886 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans34pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2170 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.1340 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.1340 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.2272 * ps + y0;
		moveToX: 0.4015 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.3185 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.3185 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans35pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5490 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.4069 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.3659 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.5090 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.5490 * ps + x0 y: 0.5162 * ps + y0;
		moveToX: 0.4758 * ps + x0 y: 0.2384 * ps + y0;
		lineToX: 0.4250 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.5676 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.6188 * ps + x0 y: 0.2384 * ps + y0;
		lineToX: 0.6969 * ps + x0 y: 0.2384 * ps + y0;
		lineToX: 0.6467 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.7990 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.7990 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.6276 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.5876 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.7428 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.7428 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.5685 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.5177 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4396 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4899 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.3468 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.2966 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2179 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2687 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 0.7540 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.2868 * ps + x0 y: 0.6793 * ps + y0;
		lineToX: 0.3278 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.1706 * ps + x0 y: 0.5162 * ps + y0;
		lineToX: 0.1706 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.3468 * ps + x0 y: 0.4410 * ps + y0;
		lineToX: 0.3967 * ps + x0 y: 0.2384 * ps + y0;
		lineToX: 0.4758 * ps + x0 y: 0.2384 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans36pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3757 * ps + x0 y: 1.1032 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 1.1032 * ps + y0;
		lineToX: 0.3263 * ps + x0 y: 0.9562 * ps + y0;
		curveToX: 0.2238 * ps + x0 y: 0.9445 * ps + y0 control1X: 0.2922 * ps + x0 y: 0.9555 * ps + y0 control2X: 0.2580 * ps + x0 y: 0.9516 * ps + y0;
		curveToX: 0.1208 * ps + x0 y: 0.9113 * ps + y0 control1X: 0.1896 * ps + x0 y: 0.9370 * ps + y0 control2X: 0.1553 * ps + x0 y: 0.9259 * ps + y0;
		lineToX: 0.1208 * ps + x0 y: 0.8234 * ps + y0;
		curveToX: 0.2214 * ps + x0 y: 0.8707 * ps + y0 control1X: 0.1540 * ps + x0 y: 0.8442 * ps + y0 control2X: 0.1875 * ps + x0 y: 0.8600 * ps + y0;
		curveToX: 0.3268 * ps + x0 y: 0.8868 * ps + y0 control1X: 0.2555 * ps + x0 y: 0.8811 * ps + y0 control2X: 0.2907 * ps + x0 y: 0.8865 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.6642 * ps + y0;
		curveToX: 0.1696 * ps + x0 y: 0.6046 * ps + y0 control1X: 0.2549 * ps + x0 y: 0.6525 * ps + y0 control2X: 0.2025 * ps + x0 y: 0.6326 * ps + y0;
		curveToX: 0.1208 * ps + x0 y: 0.4894 * ps + y0 control1X: 0.1371 * ps + x0 y: 0.5766 * ps + y0 control2X: 0.1208 * ps + x0 y: 0.5382 * ps + y0;
		curveToX: 0.1740 * ps + x0 y: 0.3639 * ps + y0 control1X: 0.1208 * ps + x0 y: 0.4363 * ps + y0 control2X: 0.1385 * ps + x0 y: 0.3945 * ps + y0;
		curveToX: 0.3268 * ps + x0 y: 0.3112 * ps + y0 control1X: 0.2095 * ps + x0 y: 0.3333 * ps + y0 control2X: 0.2604 * ps + x0 y: 0.3157 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.3097 * ps + y0;
		curveToX: 0.4635 * ps + x0 y: 0.3195 * ps + y0 control1X: 0.4059 * ps + x0 y: 0.3110 * ps + y0 control2X: 0.4352 * ps + x0 y: 0.3143 * ps + y0;
		curveToX: 0.5466 * ps + x0 y: 0.3400 * ps + y0 control1X: 0.4919 * ps + x0 y: 0.3243 * ps + y0 control2X: 0.5195 * ps + x0 y: 0.3312 * ps + y0;
		lineToX: 0.5466 * ps + x0 y: 0.4254 * ps + y0;
		curveToX: 0.4631 * ps + x0 y: 0.3937 * ps + y0 control1X: 0.5195 * ps + x0 y: 0.4117 * ps + y0 control2X: 0.4917 * ps + x0 y: 0.4012 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.3805 * ps + y0 control1X: 0.4347 * ps + x0 y: 0.3862 * ps + y0 control2X: 0.4056 * ps + x0 y: 0.3818 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.5890 * ps + y0;
		curveToX: 0.5387 * ps + x0 y: 0.6500 * ps + y0 control1X: 0.4496 * ps + x0 y: 0.6004 * ps + y0 control2X: 0.5039 * ps + x0 y: 0.6207 * ps + y0;
		curveToX: 0.5910 * ps + x0 y: 0.7701 * ps + y0 control1X: 0.5736 * ps + x0 y: 0.6793 * ps + y0 control2X: 0.5910 * ps + x0 y: 0.7194 * ps + y0;
		curveToX: 0.5353 * ps + x0 y: 0.9005 * ps + y0 control1X: 0.5910 * ps + x0 y: 0.8252 * ps + y0 control2X: 0.5724 * ps + x0 y: 0.8686 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.9552 * ps + y0 control1X: 0.4985 * ps + x0 y: 0.9321 * ps + y0 control2X: 0.4453 * ps + x0 y: 0.9503 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 1.1032 * ps + y0;
		moveToX: 0.3268 * ps + x0 y: 0.5802 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.3800 * ps + y0;
		curveToX: 0.2404 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.2891 * ps + x0 y: 0.3842 * ps + y0 control2X: 0.2603 * ps + x0 y: 0.3950 * ps + y0;
		curveToX: 0.2106 * ps + x0 y: 0.4811 * ps + y0 control1X: 0.2205 * ps + x0 y: 0.4295 * ps + y0 control2X: 0.2106 * ps + x0 y: 0.4524 * ps + y0;
		curveToX: 0.2380 * ps + x0 y: 0.5465 * ps + y0 control1X: 0.2106 * ps + x0 y: 0.5091 * ps + y0 control2X: 0.2197 * ps + x0 y: 0.5309 * ps + y0;
		curveToX: 0.3268 * ps + x0 y: 0.5802 * ps + y0 control1X: 0.2565 * ps + x0 y: 0.5621 * ps + y0 control2X: 0.2861 * ps + x0 y: 0.5734 * ps + y0;
		moveToX: 0.3757 * ps + x0 y: 0.6740 * ps + y0;
		lineToX: 0.3757 * ps + x0 y: 0.8854 * ps + y0;
		curveToX: 0.4689 * ps + x0 y: 0.8502 * ps + y0 control1X: 0.4170 * ps + x0 y: 0.8798 * ps + y0 control2X: 0.4481 * ps + x0 y: 0.8681 * ps + y0;
		curveToX: 0.5007 * ps + x0 y: 0.7794 * ps + y0 control1X: 0.4901 * ps + x0 y: 0.8323 * ps + y0 control2X: 0.5007 * ps + x0 y: 0.8087 * ps + y0;
		curveToX: 0.4704 * ps + x0 y: 0.7111 * ps + y0 control1X: 0.5007 * ps + x0 y: 0.7508 * ps + y0 control2X: 0.4906 * ps + x0 y: 0.7280 * ps + y0;
		curveToX: 0.3757 * ps + x0 y: 0.6740 * ps + y0 control1X: 0.4505 * ps + x0 y: 0.6941 * ps + y0 control2X: 0.4190 * ps + x0 y: 0.6818 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans37pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.7648 * ps + x0 y: 0.6354 * ps + y0;
		curveToX: 0.6979 * ps + x0 y: 0.6715 * ps + y0 control1X: 0.7365 * ps + x0 y: 0.6354 * ps + y0 control2X: 0.7142 * ps + x0 y: 0.6474 * ps + y0;
		curveToX: 0.6740 * ps + x0 y: 0.7721 * ps + y0 control1X: 0.6820 * ps + x0 y: 0.6956 * ps + y0 control2X: 0.6740 * ps + x0 y: 0.7291 * ps + y0;
		curveToX: 0.6979 * ps + x0 y: 0.8722 * ps + y0 control1X: 0.6740 * ps + x0 y: 0.8144 * ps + y0 control2X: 0.6820 * ps + x0 y: 0.8478 * ps + y0;
		curveToX: 0.7648 * ps + x0 y: 0.9083 * ps + y0 control1X: 0.7142 * ps + x0 y: 0.8963 * ps + y0 control2X: 0.7365 * ps + x0 y: 0.9083 * ps + y0;
		curveToX: 0.8302 * ps + x0 y: 0.8722 * ps + y0 control1X: 0.7925 * ps + x0 y: 0.9083 * ps + y0 control2X: 0.8143 * ps + x0 y: 0.8963 * ps + y0;
		curveToX: 0.8547 * ps + x0 y: 0.7721 * ps + y0 control1X: 0.8465 * ps + x0 y: 0.8478 * ps + y0 control2X: 0.8547 * ps + x0 y: 0.8144 * ps + y0;
		curveToX: 0.8302 * ps + x0 y: 0.6720 * ps + y0 control1X: 0.8547 * ps + x0 y: 0.7295 * ps + y0 control2X: 0.8465 * ps + x0 y: 0.6961 * ps + y0;
		curveToX: 0.7648 * ps + x0 y: 0.6354 * ps + y0 control1X: 0.8143 * ps + x0 y: 0.6476 * ps + y0 control2X: 0.7925 * ps + x0 y: 0.6354 * ps + y0;
		moveToX: 0.7648 * ps + x0 y: 0.5734 * ps + y0;
		curveToX: 0.8874 * ps + x0 y: 0.6271 * ps + y0 control1X: 0.8162 * ps + x0 y: 0.5734 * ps + y0 control2X: 0.8571 * ps + x0 y: 0.5913 * ps + y0;
		curveToX: 0.9328 * ps + x0 y: 0.7721 * ps + y0 control1X: 0.9176 * ps + x0 y: 0.6629 * ps + y0 control2X: 0.9328 * ps + x0 y: 0.7112 * ps + y0;
		curveToX: 0.8869 * ps + x0 y: 0.9171 * ps + y0 control1X: 0.9328 * ps + x0 y: 0.8330 * ps + y0 control2X: 0.9175 * ps + x0 y: 0.8813 * ps + y0;
		curveToX: 0.7648 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.8566 * ps + x0 y: 0.9526 * ps + y0 control2X: 0.8159 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.6413 * ps + x0 y: 0.9171 * ps + y0 control1X: 0.7127 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.6716 * ps + x0 y: 0.9526 * ps + y0;
		curveToX: 0.5959 * ps + x0 y: 0.7721 * ps + y0 control1X: 0.6110 * ps + x0 y: 0.8813 * ps + y0 control2X: 0.5959 * ps + x0 y: 0.8330 * ps + y0;
		curveToX: 0.6413 * ps + x0 y: 0.6271 * ps + y0 control1X: 0.5959 * ps + x0 y: 0.7109 * ps + y0 control2X: 0.6110 * ps + x0 y: 0.6626 * ps + y0;
		curveToX: 0.7648 * ps + x0 y: 0.5734 * ps + y0 control1X: 0.6719 * ps + x0 y: 0.5913 * ps + y0 control2X: 0.7131 * ps + x0 y: 0.5734 * ps + y0;
		moveToX: 0.2609 * ps + x0 y: 0.2760 * ps + y0;
		curveToX: 0.1945 * ps + x0 y: 0.3126 * ps + y0 control1X: 0.2329 * ps + x0 y: 0.2760 * ps + y0 control2X: 0.2108 * ps + x0 y: 0.2882 * ps + y0;
		curveToX: 0.1706 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.1786 * ps + x0 y: 0.3367 * ps + y0 control2X: 0.1706 * ps + x0 y: 0.3699 * ps + y0;
		curveToX: 0.1945 * ps + x0 y: 0.5128 * ps + y0 control1X: 0.1706 * ps + x0 y: 0.4552 * ps + y0 control2X: 0.1786 * ps + x0 y: 0.4887 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.5490 * ps + y0 control1X: 0.2105 * ps + x0 y: 0.5369 * ps + y0 control2X: 0.2326 * ps + x0 y: 0.5490 * ps + y0;
		curveToX: 0.3273 * ps + x0 y: 0.5128 * ps + y0 control1X: 0.2892 * ps + x0 y: 0.5490 * ps + y0 control2X: 0.3114 * ps + x0 y: 0.5369 * ps + y0;
		curveToX: 0.3517 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.3436 * ps + x0 y: 0.4887 * ps + y0 control2X: 0.3517 * ps + x0 y: 0.4552 * ps + y0;
		curveToX: 0.3273 * ps + x0 y: 0.3126 * ps + y0 control1X: 0.3517 * ps + x0 y: 0.3702 * ps + y0 control2X: 0.3436 * ps + x0 y: 0.3370 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.2760 * ps + y0 control1X: 0.3110 * ps + x0 y: 0.2882 * ps + y0 control2X: 0.2889 * ps + x0 y: 0.2760 * ps + y0;
		moveToX: 0.7018 * ps + x0 y: 0.2140 * ps + y0;
		lineToX: 0.7800 * ps + x0 y: 0.2140 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.9703 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.9703 * ps + y0;
		lineToX: 0.7018 * ps + x0 y: 0.2140 * ps + y0;
		moveToX: 0.2609 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.3840 * ps + x0 y: 0.2677 * ps + y0 control1X: 0.3123 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.3534 * ps + x0 y: 0.2319 * ps + y0;
		curveToX: 0.4299 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.4146 * ps + x0 y: 0.3032 * ps + y0 control2X: 0.4299 * ps + x0 y: 0.3514 * ps + y0;
		curveToX: 0.3840 * ps + x0 y: 0.5577 * ps + y0 control1X: 0.4299 * ps + x0 y: 0.4738 * ps + y0 control2X: 0.4146 * ps + x0 y: 0.5223 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.3537 * ps + x0 y: 0.5932 * ps + y0 control2X: 0.3127 * ps + x0 y: 0.6110 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5577 * ps + y0 control1X: 0.2092 * ps + x0 y: 0.6110 * ps + y0 control2X: 0.1681 * ps + x0 y: 0.5932 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.1079 * ps + x0 y: 0.5219 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.4734 * ps + y0;
		curveToX: 0.1384 * ps + x0 y: 0.2677 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.3517 * ps + y0 control2X: 0.1081 * ps + x0 y: 0.3035 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.1686 * ps + x0 y: 0.2319 * ps + y0 control2X: 0.2095 * ps + x0 y: 0.2140 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans38pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2809 * ps + x0 y: 0.5641 * ps + y0;
		curveToX: 0.2155 * ps + x0 y: 0.6432 * ps + y0 control1X: 0.2513 * ps + x0 y: 0.5905 * ps + y0 control2X: 0.2295 * ps + x0 y: 0.6168 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.7252 * ps + y0 control1X: 0.2018 * ps + x0 y: 0.6692 * ps + y0 control2X: 0.1950 * ps + x0 y: 0.6966 * ps + y0;
		curveToX: 0.2468 * ps + x0 y: 0.8439 * ps + y0 control1X: 0.1950 * ps + x0 y: 0.7728 * ps + y0 control2X: 0.2122 * ps + x0 y: 0.8123 * ps + y0;
		curveToX: 0.3766 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.2813 * ps + x0 y: 0.8755 * ps + y0 control2X: 0.3246 * ps + x0 y: 0.8912 * ps + y0;
		curveToX: 0.4635 * ps + x0 y: 0.8761 * ps + y0 control1X: 0.4076 * ps + x0 y: 0.8912 * ps + y0 control2X: 0.4365 * ps + x0 y: 0.8862 * ps + y0;
		curveToX: 0.5397 * ps + x0 y: 0.8292 * ps + y0 control1X: 0.4906 * ps + x0 y: 0.8657 * ps + y0 control2X: 0.5160 * ps + x0 y: 0.8501 * ps + y0;
		lineToX: 0.2809 * ps + x0 y: 0.5641 * ps + y0;
		moveToX: 0.3498 * ps + x0 y: 0.5094 * ps + y0;
		lineToX: 0.5978 * ps + x0 y: 0.7633 * ps + y0;
		curveToX: 0.6427 * ps + x0 y: 0.6705 * ps + y0 control1X: 0.6170 * ps + x0 y: 0.7343 * ps + y0 control2X: 0.6320 * ps + x0 y: 0.7034 * ps + y0;
		curveToX: 0.6618 * ps + x0 y: 0.5651 * ps + y0 control1X: 0.6535 * ps + x0 y: 0.6373 * ps + y0 control2X: 0.6598 * ps + x0 y: 0.6022 * ps + y0;
		lineToX: 0.7526 * ps + x0 y: 0.5651 * ps + y0;
		curveToX: 0.7214 * ps + x0 y: 0.6925 * ps + y0 control1X: 0.7487 * ps + x0 y: 0.6080 * ps + y0 control2X: 0.7383 * ps + x0 y: 0.6505 * ps + y0;
		curveToX: 0.6506 * ps + x0 y: 0.8170 * ps + y0 control1X: 0.7044 * ps + x0 y: 0.7345 * ps + y0 control2X: 0.6808 * ps + x0 y: 0.7760 * ps + y0;
		lineToX: 0.7868 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6637 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5939 * ps + x0 y: 0.8844 * ps + y0;
		curveToX: 0.4875 * ps + x0 y: 0.9493 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.9134 * ps + y0 control2X: 0.5246 * ps + x0 y: 0.9350 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4504 * ps + x0 y: 0.9633 * ps + y0 control2X: 0.4105 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1755 * ps + x0 y: 0.9034 * ps + y0 control1X: 0.2894 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2253 * ps + x0 y: 0.9480 * ps + y0;
		curveToX: 0.1008 * ps + x0 y: 0.7311 * ps + y0 control1X: 0.1257 * ps + x0 y: 0.8585 * ps + y0 control2X: 0.1008 * ps + x0 y: 0.8011 * ps + y0;
		curveToX: 0.1335 * ps + x0 y: 0.6139 * ps + y0 control1X: 0.1008 * ps + x0 y: 0.6894 * ps + y0 control2X: 0.1117 * ps + x0 y: 0.6504 * ps + y0;
		curveToX: 0.2316 * ps + x0 y: 0.5104 * ps + y0 control1X: 0.1553 * ps + x0 y: 0.5771 * ps + y0 control2X: 0.1880 * ps + x0 y: 0.5426 * ps + y0;
		curveToX: 0.1960 * ps + x0 y: 0.4493 * ps + y0 control1X: 0.2160 * ps + x0 y: 0.4899 * ps + y0 control2X: 0.2041 * ps + x0 y: 0.4695 * ps + y0;
		curveToX: 0.1838 * ps + x0 y: 0.3893 * ps + y0 control1X: 0.1878 * ps + x0 y: 0.4288 * ps + y0 control2X: 0.1838 * ps + x0 y: 0.4088 * ps + y0;
		curveToX: 0.2380 * ps + x0 y: 0.2623 * ps + y0 control1X: 0.1838 * ps + x0 y: 0.3366 * ps + y0 control2X: 0.2018 * ps + x0 y: 0.2942 * ps + y0;
		curveToX: 0.3820 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2741 * ps + x0 y: 0.2301 * ps + y0 control2X: 0.3221 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4626 * ps + x0 y: 0.2228 * ps + y0 control1X: 0.4090 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4359 * ps + x0 y: 0.2169 * ps + y0;
		curveToX: 0.5446 * ps + x0 y: 0.2492 * ps + y0 control1X: 0.4896 * ps + x0 y: 0.2286 * ps + y0 control2X: 0.5169 * ps + x0 y: 0.2374 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.3380 * ps + y0;
		curveToX: 0.4635 * ps + x0 y: 0.3033 * ps + y0 control1X: 0.5163 * ps + x0 y: 0.3227 * ps + y0 control2X: 0.4893 * ps + x0 y: 0.3112 * ps + y0;
		curveToX: 0.3918 * ps + x0 y: 0.2911 * ps + y0 control1X: 0.4378 * ps + x0 y: 0.2952 * ps + y0 control2X: 0.4139 * ps + x0 y: 0.2911 * ps + y0;
		curveToX: 0.3083 * ps + x0 y: 0.3185 * ps + y0 control1X: 0.3576 * ps + x0 y: 0.2911 * ps + y0 control2X: 0.3298 * ps + x0 y: 0.3003 * ps + y0;
		curveToX: 0.2765 * ps + x0 y: 0.3883 * ps + y0 control1X: 0.2871 * ps + x0 y: 0.3364 * ps + y0 control2X: 0.2765 * ps + x0 y: 0.3597 * ps + y0;
		curveToX: 0.2907 * ps + x0 y: 0.4386 * ps + y0 control1X: 0.2765 * ps + x0 y: 0.4049 * ps + y0 control2X: 0.2813 * ps + x0 y: 0.4217 * ps + y0;
		curveToX: 0.3498 * ps + x0 y: 0.5094 * ps + y0 control1X: 0.3005 * ps + x0 y: 0.4552 * ps + y0 control2X: 0.3202 * ps + x0 y: 0.4788 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans39pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2170 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.1340 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.1340 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans40pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3478 * ps + x0 y: 0.1974 * ps + y0;
		curveToX: 0.2507 * ps + x0 y: 0.4196 * ps + y0 control1X: 0.3042 * ps + x0 y: 0.2723 * ps + y0 control2X: 0.2718 * ps + x0 y: 0.3463 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6422 * ps + y0 control1X: 0.2295 * ps + x0 y: 0.4928 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.5670 * ps + y0;
		curveToX: 0.2507 * ps + x0 y: 0.8658 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.7174 * ps + y0 control2X: 0.2295 * ps + x0 y: 0.7920 * ps + y0;
		curveToX: 0.3478 * ps + x0 y: 1.0880 * ps + y0 control1X: 0.2721 * ps + x0 y: 0.9394 * ps + y0 control2X: 0.3045 * ps + x0 y: 1.0135 * ps + y0;
		lineToX: 0.2697 * ps + x0 y: 1.0880 * ps + y0;
		curveToX: 0.1598 * ps + x0 y: 0.8624 * ps + y0 control1X: 0.2209 * ps + x0 y: 1.0115 * ps + y0 control2X: 0.1843 * ps + x0 y: 0.9363 * ps + y0;
		curveToX: 0.1237 * ps + x0 y: 0.6422 * ps + y0 control1X: 0.1357 * ps + x0 y: 0.7885 * ps + y0 control2X: 0.1237 * ps + x0 y: 0.7151 * ps + y0;
		curveToX: 0.1598 * ps + x0 y: 0.4230 * ps + y0 control1X: 0.1237 * ps + x0 y: 0.5696 * ps + y0 control2X: 0.1357 * ps + x0 y: 0.4965 * ps + y0;
		curveToX: 0.2697 * ps + x0 y: 0.1974 * ps + y0 control1X: 0.1839 * ps + x0 y: 0.3494 * ps + y0 control2X: 0.2205 * ps + x0 y: 0.2742 * ps + y0;
		lineToX: 0.3478 * ps + x0 y: 0.1974 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans41pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1178 * ps + x0 y: 0.1974 * ps + y0;
		lineToX: 0.1960 * ps + x0 y: 0.1974 * ps + y0;
		curveToX: 0.3053 * ps + x0 y: 0.4230 * ps + y0 control1X: 0.2448 * ps + x0 y: 0.2742 * ps + y0 control2X: 0.2813 * ps + x0 y: 0.3494 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.6422 * ps + y0 control1X: 0.3298 * ps + x0 y: 0.4965 * ps + y0 control2X: 0.3420 * ps + x0 y: 0.5696 * ps + y0;
		curveToX: 0.3053 * ps + x0 y: 0.8624 * ps + y0 control1X: 0.3420 * ps + x0 y: 0.7151 * ps + y0 control2X: 0.3298 * ps + x0 y: 0.7885 * ps + y0;
		curveToX: 0.1960 * ps + x0 y: 1.0880 * ps + y0 control1X: 0.2813 * ps + x0 y: 0.9363 * ps + y0 control2X: 0.2448 * ps + x0 y: 1.0115 * ps + y0;
		lineToX: 0.1178 * ps + x0 y: 1.0880 * ps + y0;
		curveToX: 0.2145 * ps + x0 y: 0.8658 * ps + y0 control1X: 0.1611 * ps + x0 y: 1.0135 * ps + y0 control2X: 0.1934 * ps + x0 y: 0.9394 * ps + y0;
		curveToX: 0.2468 * ps + x0 y: 0.6422 * ps + y0 control1X: 0.2360 * ps + x0 y: 0.7920 * ps + y0 control2X: 0.2468 * ps + x0 y: 0.7174 * ps + y0;
		curveToX: 0.2145 * ps + x0 y: 0.4196 * ps + y0 control1X: 0.2468 * ps + x0 y: 0.5670 * ps + y0 control2X: 0.2360 * ps + x0 y: 0.4928 * ps + y0;
		curveToX: 0.1178 * ps + x0 y: 0.1974 * ps + y0 control1X: 0.1934 * ps + x0 y: 0.3463 * ps + y0 control2X: 0.1611 * ps + x0 y: 0.2723 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans42pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5080 * ps + x0 y: 0.3473 * ps + y0;
		lineToX: 0.3327 * ps + x0 y: 0.4420 * ps + y0;
		lineToX: 0.5080 * ps + x0 y: 0.5372 * ps + y0;
		lineToX: 0.4797 * ps + x0 y: 0.5851 * ps + y0;
		lineToX: 0.3156 * ps + x0 y: 0.4860 * ps + y0;
		lineToX: 0.3156 * ps + x0 y: 0.6700 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.6700 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.4860 * ps + y0;
		lineToX: 0.0959 * ps + x0 y: 0.5851 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.5372 * ps + y0;
		lineToX: 0.2428 * ps + x0 y: 0.4420 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.3473 * ps + y0;
		lineToX: 0.0959 * ps + x0 y: 0.2990 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.3981 * ps + y0;
		lineToX: 0.2599 * ps + x0 y: 0.2140 * ps + y0;
		lineToX: 0.3156 * ps + x0 y: 0.2140 * ps + y0;
		lineToX: 0.3156 * ps + x0 y: 0.3981 * ps + y0;
		lineToX: 0.4797 * ps + x0 y: 0.2990 * ps + y0;
		lineToX: 0.5080 * ps + x0 y: 0.3473 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans43pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4977 * ps + x0 y: 0.3292 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.3292 * ps + y0;
		lineToX: 0.4977 * ps + x0 y: 0.3292 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans44pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.9161 * ps + y0;
		lineToX: 0.1779 * ps + x0 y: 1.0724 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 1.0724 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.9161 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans45pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0866 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.6422 * ps + y0;
		lineToX: 0.3498 * ps + x0 y: 0.7223 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.7223 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6422 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans46pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1447 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2477 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1447 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1447 * ps + x0 y: 0.8322 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans47pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2917 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3747 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1208 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.0378 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.2917 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans48pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3556 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.2409 * ps + x0 y: 0.3673 * ps + y0 control1X: 0.3049 * ps + x0 y: 0.2921 * ps + y0 control2X: 0.2666 * ps + x0 y: 0.3172 * ps + y0;
		curveToX: 0.2028 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2155 * ps + x0 y: 0.4171 * ps + y0 control2X: 0.2028 * ps + x0 y: 0.4922 * ps + y0;
		curveToX: 0.2409 * ps + x0 y: 0.8175 * ps + y0 control1X: 0.2028 * ps + x0 y: 0.6923 * ps + y0 control2X: 0.2155 * ps + x0 y: 0.7674 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2666 * ps + x0 y: 0.8673 * ps + y0 control2X: 0.3049 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4704 * ps + x0 y: 0.8175 * ps + y0 control1X: 0.4067 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4450 * ps + x0 y: 0.8673 * ps + y0;
		curveToX: 0.5090 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.4961 * ps + x0 y: 0.7674 * ps + y0 control2X: 0.5090 * ps + x0 y: 0.6923 * ps + y0;
		curveToX: 0.4704 * ps + x0 y: 0.3673 * ps + y0 control1X: 0.5090 * ps + x0 y: 0.4922 * ps + y0 control2X: 0.4961 * ps + x0 y: 0.4171 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2921 * ps + y0 control1X: 0.4450 * ps + x0 y: 0.3172 * ps + y0 control2X: 0.4067 * ps + x0 y: 0.2921 * ps + y0;
		moveToX: 0.3556 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5426 * ps + x0 y: 0.3112 * ps + y0 control1X: 0.4373 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4997 * ps + x0 y: 0.2464 * ps + y0;
		curveToX: 0.6076 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.5859 * ps + x0 y: 0.3756 * ps + y0 control2X: 0.6076 * ps + x0 y: 0.4694 * ps + y0;
		curveToX: 0.5426 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.6076 * ps + x0 y: 0.7151 * ps + y0 control2X: 0.5859 * ps + x0 y: 0.8089 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4997 * ps + x0 y: 0.9381 * ps + y0 control2X: 0.4373 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1681 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.2739 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2114 * ps + x0 y: 0.9381 * ps + y0;
		curveToX: 0.1037 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1252 * ps + x0 y: 0.8089 * ps + y0 control2X: 0.1037 * ps + x0 y: 0.7151 * ps + y0;
		curveToX: 0.1681 * ps + x0 y: 0.3112 * ps + y0 control1X: 0.1037 * ps + x0 y: 0.4694 * ps + y0 control2X: 0.1252 * ps + x0 y: 0.3756 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.2464 * ps + y0 control2X: 0.2739 * ps + x0 y: 0.2140 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans49pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1618 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.3229 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.3229 * ps + x0 y: 0.3170 * ps + y0;
		lineToX: 0.1476 * ps + x0 y: 0.3522 * ps + y0;
		lineToX: 0.1476 * ps + x0 y: 0.2623 * ps + y0;
		lineToX: 0.3219 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4206 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4206 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1618 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1618 * ps + x0 y: 0.8732 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans50pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2297 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5739 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5739 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1110 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.2638 * ps + x0 y: 0.7174 * ps + y0 control1X: 0.1484 * ps + x0 y: 0.8344 * ps + y0 control2X: 0.1994 * ps + x0 y: 0.7825 * ps + y0;
		curveToX: 0.3859 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.3286 * ps + x0 y: 0.6520 * ps + y0 control2X: 0.3693 * ps + x0 y: 0.6098 * ps + y0;
		curveToX: 0.4518 * ps + x0 y: 0.5011 * ps + y0 control1X: 0.4175 * ps + x0 y: 0.5555 * ps + y0 control2X: 0.4395 * ps + x0 y: 0.5255 * ps + y0;
		curveToX: 0.4709 * ps + x0 y: 0.4283 * ps + y0 control1X: 0.4645 * ps + x0 y: 0.4764 * ps + y0 control2X: 0.4709 * ps + x0 y: 0.4521 * ps + y0;
		curveToX: 0.4299 * ps + x0 y: 0.3336 * ps + y0 control1X: 0.4709 * ps + x0 y: 0.3896 * ps + y0 control2X: 0.4572 * ps + x0 y: 0.3580 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.2970 * ps + y0 control1X: 0.4028 * ps + x0 y: 0.3092 * ps + y0 control2X: 0.3675 * ps + x0 y: 0.2970 * ps + y0;
		curveToX: 0.2258 * ps + x0 y: 0.3131 * ps + y0 control1X: 0.2930 * ps + x0 y: 0.2970 * ps + y0 control2X: 0.2603 * ps + x0 y: 0.3024 * ps + y0;
		curveToX: 0.1159 * ps + x0 y: 0.3619 * ps + y0 control1X: 0.1916 * ps + x0 y: 0.3239 * ps + y0 control2X: 0.1550 * ps + x0 y: 0.3401 * ps + y0;
		lineToX: 0.1159 * ps + x0 y: 0.2623 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 0.2262 * ps + y0 control1X: 0.1556 * ps + x0 y: 0.2464 * ps + y0 control2X: 0.1927 * ps + x0 y: 0.2343 * ps + y0;
		curveToX: 0.3219 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2617 * ps + x0 y: 0.2181 * ps + y0 control2X: 0.2933 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5026 * ps + x0 y: 0.2706 * ps + y0 control1X: 0.3975 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4577 * ps + x0 y: 0.2329 * ps + y0;
		curveToX: 0.5700 * ps + x0 y: 0.4220 * ps + y0 control1X: 0.5475 * ps + x0 y: 0.3084 * ps + y0 control2X: 0.5700 * ps + x0 y: 0.3589 * ps + y0;
		curveToX: 0.5529 * ps + x0 y: 0.5075 * ps + y0 control1X: 0.5700 * ps + x0 y: 0.4520 * ps + y0 control2X: 0.5643 * ps + x0 y: 0.4804 * ps + y0;
		curveToX: 0.4919 * ps + x0 y: 0.6022 * ps + y0 control1X: 0.5418 * ps + x0 y: 0.5341 * ps + y0 control2X: 0.5215 * ps + x0 y: 0.5657 * ps + y0;
		curveToX: 0.4142 * ps + x0 y: 0.6842 * ps + y0 control1X: 0.4837 * ps + x0 y: 0.6116 * ps + y0 control2X: 0.4579 * ps + x0 y: 0.6390 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.8732 * ps + y0 control1X: 0.3706 * ps + x0 y: 0.7291 * ps + y0 control2X: 0.3091 * ps + x0 y: 0.7921 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans51pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4435 * ps + x0 y: 0.5631 * ps + y0;
		curveToX: 0.5539 * ps + x0 y: 0.6261 * ps + y0 control1X: 0.4907 * ps + x0 y: 0.5732 * ps + y0 control2X: 0.5275 * ps + x0 y: 0.5942 * ps + y0;
		curveToX: 0.5939 * ps + x0 y: 0.7443 * ps + y0 control1X: 0.5806 * ps + x0 y: 0.6580 * ps + y0 control2X: 0.5939 * ps + x0 y: 0.6974 * ps + y0;
		curveToX: 0.5197 * ps + x0 y: 0.9113 * ps + y0 control1X: 0.5939 * ps + x0 y: 0.8162 * ps + y0 control2X: 0.5692 * ps + x0 y: 0.8719 * ps + y0;
		curveToX: 0.3088 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4702 * ps + x0 y: 0.9506 * ps + y0 control2X: 0.3999 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2140 * ps + x0 y: 0.9611 * ps + y0 control1X: 0.2782 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2466 * ps + x0 y: 0.9672 * ps + y0;
		curveToX: 0.1139 * ps + x0 y: 0.9342 * ps + y0 control1X: 0.1818 * ps + x0 y: 0.9552 * ps + y0 control2X: 0.1484 * ps + x0 y: 0.9463 * ps + y0;
		lineToX: 0.1139 * ps + x0 y: 0.8390 * ps + y0;
		curveToX: 0.2038 * ps + x0 y: 0.8751 * ps + y0 control1X: 0.1413 * ps + x0 y: 0.8549 * ps + y0 control2X: 0.1712 * ps + x0 y: 0.8670 * ps + y0;
		curveToX: 0.3058 * ps + x0 y: 0.8873 * ps + y0 control1X: 0.2363 * ps + x0 y: 0.8833 * ps + y0 control2X: 0.2704 * ps + x0 y: 0.8873 * ps + y0;
		curveToX: 0.4469 * ps + x0 y: 0.8507 * ps + y0 control1X: 0.3677 * ps + x0 y: 0.8873 * ps + y0 control2X: 0.4147 * ps + x0 y: 0.8751 * ps + y0;
		curveToX: 0.4958 * ps + x0 y: 0.7443 * ps + y0 control1X: 0.4795 * ps + x0 y: 0.8263 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.7908 * ps + y0;
		curveToX: 0.4504 * ps + x0 y: 0.6437 * ps + y0 control1X: 0.4958 * ps + x0 y: 0.7013 * ps + y0 control2X: 0.4806 * ps + x0 y: 0.6678 * ps + y0;
		curveToX: 0.3249 * ps + x0 y: 0.6071 * ps + y0 control1X: 0.4204 * ps + x0 y: 0.6193 * ps + y0 control2X: 0.3786 * ps + x0 y: 0.6071 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.6071 * ps + y0;
		lineToX: 0.2399 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.3288 * ps + x0 y: 0.5260 * ps + y0;
		curveToX: 0.4401 * ps + x0 y: 0.4972 * ps + y0 control1X: 0.3773 * ps + x0 y: 0.5260 * ps + y0 control2X: 0.4144 * ps + x0 y: 0.5164 * ps + y0;
		curveToX: 0.4787 * ps + x0 y: 0.4132 * ps + y0 control1X: 0.4658 * ps + x0 y: 0.4777 * ps + y0 control2X: 0.4787 * ps + x0 y: 0.4497 * ps + y0;
		curveToX: 0.4386 * ps + x0 y: 0.3273 * ps + y0 control1X: 0.4787 * ps + x0 y: 0.3758 * ps + y0 control2X: 0.4653 * ps + x0 y: 0.3471 * ps + y0;
		curveToX: 0.3249 * ps + x0 y: 0.2970 * ps + y0 control1X: 0.4123 * ps + x0 y: 0.3071 * ps + y0 control2X: 0.3744 * ps + x0 y: 0.2970 * ps + y0;
		curveToX: 0.2380 * ps + x0 y: 0.3058 * ps + y0 control1X: 0.2979 * ps + x0 y: 0.2970 * ps + y0 control2X: 0.2689 * ps + x0 y: 0.2999 * ps + y0;
		curveToX: 0.1359 * ps + x0 y: 0.3331 * ps + y0 control1X: 0.2070 * ps + x0 y: 0.3117 * ps + y0 control2X: 0.1730 * ps + x0 y: 0.3208 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2452 * ps + y0;
		curveToX: 0.2409 * ps + x0 y: 0.2218 * ps + y0 control1X: 0.1733 * ps + x0 y: 0.2348 * ps + y0 control2X: 0.2083 * ps + x0 y: 0.2270 * ps + y0;
		curveToX: 0.3337 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2738 * ps + x0 y: 0.2166 * ps + y0 control2X: 0.3047 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5114 * ps + x0 y: 0.2653 * ps + y0 control1X: 0.4085 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4678 * ps + x0 y: 0.2311 * ps + y0;
		curveToX: 0.5768 * ps + x0 y: 0.4030 * ps + y0 control1X: 0.5550 * ps + x0 y: 0.2991 * ps + y0 control2X: 0.5768 * ps + x0 y: 0.3450 * ps + y0;
		curveToX: 0.5422 * ps + x0 y: 0.5055 * ps + y0 control1X: 0.5768 * ps + x0 y: 0.4433 * ps + y0 control2X: 0.5653 * ps + x0 y: 0.4775 * ps + y0;
		curveToX: 0.4435 * ps + x0 y: 0.5631 * ps + y0 control1X: 0.5190 * ps + x0 y: 0.5332 * ps + y0 control2X: 0.4862 * ps + x0 y: 0.5524 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans52pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4157 * ps + x0 y: 0.3131 * ps + y0;
		lineToX: 0.1667 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.3131 * ps + y0;
		moveToX: 0.3898 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.6178 * ps + x0 y: 0.7023 * ps + y0;
		lineToX: 0.6178 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4157 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6891 * ps + y0;
		lineToX: 0.3898 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans53pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1457 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5329 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5329 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2360 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2360 * ps + x0 y: 0.4889 * ps + y0;
		curveToX: 0.2790 * ps + x0 y: 0.4782 * ps + y0 control1X: 0.2503 * ps + x0 y: 0.4840 * ps + y0 control2X: 0.2647 * ps + x0 y: 0.4804 * ps + y0;
		curveToX: 0.3219 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.2933 * ps + x0 y: 0.4756 * ps + y0 control2X: 0.3076 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.5153 * ps + x0 y: 0.5411 * ps + y0 control1X: 0.4033 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.4678 * ps + x0 y: 0.4965 * ps + y0;
		curveToX: 0.5866 * ps + x0 y: 0.7223 * ps + y0 control1X: 0.5628 * ps + x0 y: 0.5857 * ps + y0 control2X: 0.5866 * ps + x0 y: 0.6461 * ps + y0;
		curveToX: 0.5134 * ps + x0 y: 0.9054 * ps + y0 control1X: 0.5866 * ps + x0 y: 0.8007 * ps + y0 control2X: 0.5622 * ps + x0 y: 0.8618 * ps + y0;
		curveToX: 0.3068 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4645 * ps + x0 y: 0.9487 * ps + y0 control2X: 0.3957 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2131 * ps + x0 y: 0.9625 * ps + y0 control1X: 0.2762 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2450 * ps + x0 y: 0.9677 * ps + y0;
		curveToX: 0.1149 * ps + x0 y: 0.9391 * ps + y0 control1X: 0.1815 * ps + x0 y: 0.9573 * ps + y0 control2X: 0.1488 * ps + x0 y: 0.9495 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 0.8400 * ps + y0;
		curveToX: 0.2057 * ps + x0 y: 0.8756 * ps + y0 control1X: 0.1442 * ps + x0 y: 0.8559 * ps + y0 control2X: 0.1745 * ps + x0 y: 0.8678 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8873 * ps + y0 control1X: 0.2370 * ps + x0 y: 0.8834 * ps + y0 control2X: 0.2700 * ps + x0 y: 0.8873 * ps + y0;
		curveToX: 0.4386 * ps + x0 y: 0.8429 * ps + y0 control1X: 0.3612 * ps + x0 y: 0.8873 * ps + y0 control2X: 0.4058 * ps + x0 y: 0.8725 * ps + y0;
		curveToX: 0.4880 * ps + x0 y: 0.7223 * ps + y0 control1X: 0.4715 * ps + x0 y: 0.8133 * ps + y0 control2X: 0.4880 * ps + x0 y: 0.7731 * ps + y0;
		curveToX: 0.4386 * ps + x0 y: 0.6017 * ps + y0 control1X: 0.4880 * ps + x0 y: 0.6715 * ps + y0 control2X: 0.4715 * ps + x0 y: 0.6313 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.5573 * ps + y0 control1X: 0.4058 * ps + x0 y: 0.5721 * ps + y0 control2X: 0.3612 * ps + x0 y: 0.5573 * ps + y0;
		curveToX: 0.2258 * ps + x0 y: 0.5660 * ps + y0 control1X: 0.2785 * ps + x0 y: 0.5573 * ps + y0 control2X: 0.2521 * ps + x0 y: 0.5602 * ps + y0;
		curveToX: 0.1457 * ps + x0 y: 0.5934 * ps + y0 control1X: 0.1997 * ps + x0 y: 0.5719 * ps + y0 control2X: 0.1730 * ps + x0 y: 0.5810 * ps + y0;
		lineToX: 0.1457 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans54pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3678 * ps + x0 y: 0.5524 * ps + y0;
		curveToX: 0.2624 * ps + x0 y: 0.5978 * ps + y0 control1X: 0.3236 * ps + x0 y: 0.5524 * ps + y0 control2X: 0.2884 * ps + x0 y: 0.5675 * ps + y0;
		curveToX: 0.2238 * ps + x0 y: 0.7223 * ps + y0 control1X: 0.2367 * ps + x0 y: 0.6281 * ps + y0 control2X: 0.2238 * ps + x0 y: 0.6696 * ps + y0;
		curveToX: 0.2624 * ps + x0 y: 0.8468 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7747 * ps + y0 control2X: 0.2367 * ps + x0 y: 0.8162 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2884 * ps + x0 y: 0.8771 * ps + y0 control2X: 0.3236 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4728 * ps + x0 y: 0.8468 * ps + y0 control1X: 0.4121 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4471 * ps + x0 y: 0.8771 * ps + y0;
		curveToX: 0.5119 * ps + x0 y: 0.7223 * ps + y0 control1X: 0.4989 * ps + x0 y: 0.8162 * ps + y0 control2X: 0.5119 * ps + x0 y: 0.7747 * ps + y0;
		curveToX: 0.4728 * ps + x0 y: 0.5978 * ps + y0 control1X: 0.5119 * ps + x0 y: 0.6696 * ps + y0 control2X: 0.4989 * ps + x0 y: 0.6281 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.5524 * ps + y0 control1X: 0.4471 * ps + x0 y: 0.5675 * ps + y0 control2X: 0.4121 * ps + x0 y: 0.5524 * ps + y0;
		moveToX: 0.5636 * ps + x0 y: 0.2433 * ps + y0;
		lineToX: 0.5636 * ps + x0 y: 0.3331 * ps + y0;
		curveToX: 0.4885 * ps + x0 y: 0.3063 * ps + y0 control1X: 0.5389 * ps + x0 y: 0.3214 * ps + y0 control2X: 0.5138 * ps + x0 y: 0.3125 * ps + y0;
		curveToX: 0.4137 * ps + x0 y: 0.2970 * ps + y0 control1X: 0.4634 * ps + x0 y: 0.3001 * ps + y0 control2X: 0.4385 * ps + x0 y: 0.2970 * ps + y0;
		curveToX: 0.2643 * ps + x0 y: 0.3629 * ps + y0 control1X: 0.3486 * ps + x0 y: 0.2970 * ps + y0 control2X: 0.2988 * ps + x0 y: 0.3190 * ps + y0;
		curveToX: 0.2057 * ps + x0 y: 0.5621 * ps + y0 control1X: 0.2301 * ps + x0 y: 0.4069 * ps + y0 control2X: 0.2106 * ps + x0 y: 0.4733 * ps + y0;
		curveToX: 0.2780 * ps + x0 y: 0.4972 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.5338 * ps + y0 control2X: 0.2490 * ps + x0 y: 0.5122 * ps + y0;
		curveToX: 0.3737 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.3070 * ps + x0 y: 0.4819 * ps + y0 control2X: 0.3389 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.5470 * ps + x0 y: 0.5411 * ps + y0 control1X: 0.4469 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.5047 * ps + x0 y: 0.4965 * ps + y0;
		curveToX: 0.6110 * ps + x0 y: 0.7223 * ps + y0 control1X: 0.5897 * ps + x0 y: 0.5854 * ps + y0 control2X: 0.6110 * ps + x0 y: 0.6458 * ps + y0;
		curveToX: 0.5446 * ps + x0 y: 0.9025 * ps + y0 control1X: 0.6110 * ps + x0 y: 0.7972 * ps + y0 control2X: 0.5889 * ps + x0 y: 0.8572 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5003 * ps + x0 y: 0.9477 * ps + y0 control2X: 0.4414 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1745 * ps + x0 y: 0.8737 * ps + y0 control1X: 0.2835 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2191 * ps + x0 y: 0.9381 * ps + y0;
		curveToX: 0.1076 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1299 * ps + x0 y: 0.8089 * ps + y0 control2X: 0.1076 * ps + x0 y: 0.7151 * ps + y0;
		curveToX: 0.1896 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.1076 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1349 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4098 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2443 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3177 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4845 * ps + x0 y: 0.2213 * ps + y0 control1X: 0.4346 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4595 * ps + x0 y: 0.2164 * ps + y0;
		curveToX: 0.5636 * ps + x0 y: 0.2433 * ps + y0 control1X: 0.5099 * ps + x0 y: 0.2262 * ps + y0 control2X: 0.5363 * ps + x0 y: 0.2335 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans55pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1198 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5885 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5885 * ps + x0 y: 0.2692 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2209 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1198 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.1198 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans56pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3556 * ps + x0 y: 0.6100 * ps + y0;
		curveToX: 0.2448 * ps + x0 y: 0.6476 * ps + y0 control1X: 0.3088 * ps + x0 y: 0.6100 * ps + y0 control2X: 0.2718 * ps + x0 y: 0.6225 * ps + y0;
		curveToX: 0.2048 * ps + x0 y: 0.7511 * ps + y0 control1X: 0.2181 * ps + x0 y: 0.6727 * ps + y0 control2X: 0.2048 * ps + x0 y: 0.7072 * ps + y0;
		curveToX: 0.2448 * ps + x0 y: 0.8546 * ps + y0 control1X: 0.2048 * ps + x0 y: 0.7950 * ps + y0 control2X: 0.2181 * ps + x0 y: 0.8296 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.8922 * ps + y0 control1X: 0.2718 * ps + x0 y: 0.8797 * ps + y0 control2X: 0.3088 * ps + x0 y: 0.8922 * ps + y0;
		curveToX: 0.4665 * ps + x0 y: 0.8546 * ps + y0 control1X: 0.4025 * ps + x0 y: 0.8922 * ps + y0 control2X: 0.4395 * ps + x0 y: 0.8797 * ps + y0;
		curveToX: 0.5070 * ps + x0 y: 0.7511 * ps + y0 control1X: 0.4935 * ps + x0 y: 0.8292 * ps + y0 control2X: 0.5070 * ps + x0 y: 0.7947 * ps + y0;
		curveToX: 0.4665 * ps + x0 y: 0.6476 * ps + y0 control1X: 0.5070 * ps + x0 y: 0.7072 * ps + y0 control2X: 0.4935 * ps + x0 y: 0.6727 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.6100 * ps + y0 control1X: 0.4398 * ps + x0 y: 0.6225 * ps + y0 control2X: 0.4028 * ps + x0 y: 0.6100 * ps + y0;
		moveToX: 0.2570 * ps + x0 y: 0.5680 * ps + y0;
		curveToX: 0.1579 * ps + x0 y: 0.5089 * ps + y0 control1X: 0.2147 * ps + x0 y: 0.5576 * ps + y0 control2X: 0.1816 * ps + x0 y: 0.5379 * ps + y0;
		curveToX: 0.1227 * ps + x0 y: 0.4030 * ps + y0 control1X: 0.1344 * ps + x0 y: 0.4799 * ps + y0 control2X: 0.1227 * ps + x0 y: 0.4446 * ps + y0;
		curveToX: 0.1847 * ps + x0 y: 0.2648 * ps + y0 control1X: 0.1227 * ps + x0 y: 0.3447 * ps + y0 control2X: 0.1434 * ps + x0 y: 0.2986 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2264 * ps + x0 y: 0.2309 * ps + y0 control2X: 0.2834 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5265 * ps + x0 y: 0.2648 * ps + y0 control1X: 0.4282 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4852 * ps + x0 y: 0.2309 * ps + y0;
		curveToX: 0.5885 * ps + x0 y: 0.4030 * ps + y0 control1X: 0.5679 * ps + x0 y: 0.2986 * ps + y0 control2X: 0.5885 * ps + x0 y: 0.3447 * ps + y0;
		curveToX: 0.5529 * ps + x0 y: 0.5089 * ps + y0 control1X: 0.5885 * ps + x0 y: 0.4446 * ps + y0 control2X: 0.5767 * ps + x0 y: 0.4799 * ps + y0;
		curveToX: 0.4548 * ps + x0 y: 0.5680 * ps + y0 control1X: 0.5295 * ps + x0 y: 0.5379 * ps + y0 control2X: 0.4968 * ps + x0 y: 0.5576 * ps + y0;
		curveToX: 0.5656 * ps + x0 y: 0.6329 * ps + y0 control1X: 0.5023 * ps + x0 y: 0.5791 * ps + y0 control2X: 0.5392 * ps + x0 y: 0.6007 * ps + y0;
		curveToX: 0.6056 * ps + x0 y: 0.7511 * ps + y0 control1X: 0.5923 * ps + x0 y: 0.6652 * ps + y0 control2X: 0.6056 * ps + x0 y: 0.7046 * ps + y0;
		curveToX: 0.5407 * ps + x0 y: 0.9137 * ps + y0 control1X: 0.6056 * ps + x0 y: 0.8217 * ps + y0 control2X: 0.5840 * ps + x0 y: 0.8759 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4977 * ps + x0 y: 0.9515 * ps + y0 control2X: 0.4360 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1701 * ps + x0 y: 0.9137 * ps + y0 control1X: 0.2752 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2134 * ps + x0 y: 0.9515 * ps + y0;
		curveToX: 0.1056 * ps + x0 y: 0.7511 * ps + y0 control1X: 0.1271 * ps + x0 y: 0.8759 * ps + y0 control2X: 0.1056 * ps + x0 y: 0.8217 * ps + y0;
		curveToX: 0.1457 * ps + x0 y: 0.6329 * ps + y0 control1X: 0.1056 * ps + x0 y: 0.7046 * ps + y0 control2X: 0.1190 * ps + x0 y: 0.6652 * ps + y0;
		curveToX: 0.2570 * ps + x0 y: 0.5680 * ps + y0 control1X: 0.1724 * ps + x0 y: 0.6007 * ps + y0 control2X: 0.2095 * ps + x0 y: 0.5791 * ps + y0;
		moveToX: 0.2209 * ps + x0 y: 0.4122 * ps + y0;
		curveToX: 0.2560 * ps + x0 y: 0.5006 * ps + y0 control1X: 0.2209 * ps + x0 y: 0.4500 * ps + y0 control2X: 0.2326 * ps + x0 y: 0.4795 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.5324 * ps + y0 control1X: 0.2798 * ps + x0 y: 0.5218 * ps + y0 control2X: 0.3130 * ps + x0 y: 0.5324 * ps + y0;
		curveToX: 0.4548 * ps + x0 y: 0.5006 * ps + y0 control1X: 0.3980 * ps + x0 y: 0.5324 * ps + y0 control2X: 0.4310 * ps + x0 y: 0.5218 * ps + y0;
		curveToX: 0.4909 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.4788 * ps + x0 y: 0.4795 * ps + y0 control2X: 0.4909 * ps + x0 y: 0.4500 * ps + y0;
		curveToX: 0.4548 * ps + x0 y: 0.3239 * ps + y0 control1X: 0.4909 * ps + x0 y: 0.3745 * ps + y0 control2X: 0.4788 * ps + x0 y: 0.3450 * ps + y0;
		curveToX: 0.3556 * ps + x0 y: 0.2921 * ps + y0 control1X: 0.4310 * ps + x0 y: 0.3027 * ps + y0 control2X: 0.3980 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.2560 * ps + x0 y: 0.3239 * ps + y0 control1X: 0.3130 * ps + x0 y: 0.2921 * ps + y0 control2X: 0.2798 * ps + x0 y: 0.3027 * ps + y0;
		curveToX: 0.2209 * ps + x0 y: 0.4122 * ps + y0 control1X: 0.2326 * ps + x0 y: 0.3450 * ps + y0 control2X: 0.2209 * ps + x0 y: 0.3745 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans57pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1476 * ps + x0 y: 0.9410 * ps + y0;
		lineToX: 0.1476 * ps + x0 y: 0.8512 * ps + y0;
		curveToX: 0.2228 * ps + x0 y: 0.8781 * ps + y0 control1X: 0.1724 * ps + x0 y: 0.8629 * ps + y0 control2X: 0.1974 * ps + x0 y: 0.8719 * ps + y0;
		curveToX: 0.2975 * ps + x0 y: 0.8873 * ps + y0 control1X: 0.2482 * ps + x0 y: 0.8842 * ps + y0 control2X: 0.2731 * ps + x0 y: 0.8873 * ps + y0;
		curveToX: 0.4465 * ps + x0 y: 0.8219 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.8873 * ps + y0 control2X: 0.4123 * ps + x0 y: 0.8655 * ps + y0;
		curveToX: 0.5055 * ps + x0 y: 0.6222 * ps + y0 control1X: 0.4810 * ps + x0 y: 0.7780 * ps + y0 control2X: 0.5007 * ps + x0 y: 0.7114 * ps + y0;
		curveToX: 0.4338 * ps + x0 y: 0.6867 * ps + y0 control1X: 0.4867 * ps + x0 y: 0.6502 * ps + y0 control2X: 0.4627 * ps + x0 y: 0.6717 * ps + y0;
		curveToX: 0.3376 * ps + x0 y: 0.7091 * ps + y0 control1X: 0.4048 * ps + x0 y: 0.7016 * ps + y0 control2X: 0.3727 * ps + x0 y: 0.7091 * ps + y0;
		curveToX: 0.1642 * ps + x0 y: 0.6432 * ps + y0 control1X: 0.2647 * ps + x0 y: 0.7091 * ps + y0 control2X: 0.2069 * ps + x0 y: 0.6871 * ps + y0;
		curveToX: 0.1008 * ps + x0 y: 0.4620 * ps + y0 control1X: 0.1219 * ps + x0 y: 0.5989 * ps + y0 control2X: 0.1008 * ps + x0 y: 0.5385 * ps + y0;
		curveToX: 0.1672 * ps + x0 y: 0.2819 * ps + y0 control1X: 0.1008 * ps + x0 y: 0.3872 * ps + y0 control2X: 0.1229 * ps + x0 y: 0.3271 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2114 * ps + x0 y: 0.2366 * ps + y0 control2X: 0.2704 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5368 * ps + x0 y: 0.3112 * ps + y0 control1X: 0.4282 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4925 * ps + x0 y: 0.2464 * ps + y0;
		curveToX: 0.6037 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.5814 * ps + x0 y: 0.3756 * ps + y0 control2X: 0.6037 * ps + x0 y: 0.4694 * ps + y0;
		curveToX: 0.5217 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.6037 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.5763 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.3019 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4673 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.3940 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2267 * ps + x0 y: 0.9630 * ps + y0 control1X: 0.2772 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2521 * ps + x0 y: 0.9679 * ps + y0;
		curveToX: 0.1476 * ps + x0 y: 0.9410 * ps + y0 control1X: 0.2013 * ps + x0 y: 0.9581 * ps + y0 control2X: 0.1750 * ps + x0 y: 0.9508 * ps + y0;
		moveToX: 0.3439 * ps + x0 y: 0.6320 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.5866 * ps + y0 control1X: 0.3882 * ps + x0 y: 0.6320 * ps + y0 control2X: 0.4232 * ps + x0 y: 0.6168 * ps + y0;
		curveToX: 0.4880 * ps + x0 y: 0.4620 * ps + y0 control1X: 0.4749 * ps + x0 y: 0.5563 * ps + y0 control2X: 0.4880 * ps + x0 y: 0.5148 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.4880 * ps + x0 y: 0.4096 * ps + y0 control2X: 0.4749 * ps + x0 y: 0.3683 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.2921 * ps + y0 control1X: 0.4232 * ps + x0 y: 0.3074 * ps + y0 control2X: 0.3882 * ps + x0 y: 0.2921 * ps + y0;
		curveToX: 0.2385 * ps + x0 y: 0.3380 * ps + y0 control1X: 0.2996 * ps + x0 y: 0.2921 * ps + y0 control2X: 0.2645 * ps + x0 y: 0.3074 * ps + y0;
		curveToX: 0.1999 * ps + x0 y: 0.4620 * ps + y0 control1X: 0.2127 * ps + x0 y: 0.3683 * ps + y0 control2X: 0.1999 * ps + x0 y: 0.4096 * ps + y0;
		curveToX: 0.2385 * ps + x0 y: 0.5866 * ps + y0 control1X: 0.1999 * ps + x0 y: 0.5148 * ps + y0 control2X: 0.2127 * ps + x0 y: 0.5563 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6320 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.6168 * ps + y0 control2X: 0.2996 * ps + x0 y: 0.6320 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans58pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0;
		moveToX: 0.1550 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.5631 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.5631 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.4391 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans59pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1550 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.4391 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.5631 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.5631 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.4391 * ps + y0;
		moveToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.2580 * ps + x0 y: 0.9161 * ps + y0;
		lineToX: 0.1779 * ps + x0 y: 1.0724 * ps + y0;
		lineToX: 0.1149 * ps + x0 y: 1.0724 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.9161 * ps + y0;
		lineToX: 0.1550 * ps + x0 y: 0.8322 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans60pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.7697 * ps + x0 y: 0.4640 * ps + y0;
		lineToX: 0.2658 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.8214 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.9103 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.6022 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.3751 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.4640 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans61pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1437 * ps + x0 y: 0.5021 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.5021 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.5841 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.5841 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.5021 * ps + y0;
		moveToX: 0.1437 * ps + x0 y: 0.7013 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.7013 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.7843 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.7013 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans62pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1437 * ps + x0 y: 0.4640 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.3751 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6022 * ps + y0;
		lineToX: 0.7697 * ps + x0 y: 0.6832 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.9103 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.8214 * ps + y0;
		lineToX: 0.6467 * ps + x0 y: 0.6432 * ps + y0;
		lineToX: 0.1437 * ps + x0 y: 0.4640 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans63pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2287 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.3278 * ps + x0 y: 0.8322 * ps + y0;
		lineToX: 0.3278 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2287 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2287 * ps + x0 y: 0.8322 * ps + y0;
		moveToX: 0.3249 * ps + x0 y: 0.7604 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.7604 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.6852 * ps + y0;
		curveToX: 0.2453 * ps + x0 y: 0.6041 * ps + y0 control1X: 0.2316 * ps + x0 y: 0.6523 * ps + y0 control2X: 0.2362 * ps + x0 y: 0.6253 * ps + y0;
		curveToX: 0.3029 * ps + x0 y: 0.5304 * ps + y0 control1X: 0.2544 * ps + x0 y: 0.5830 * ps + y0 control2X: 0.2736 * ps + x0 y: 0.5584 * ps + y0;
		lineToX: 0.3468 * ps + x0 y: 0.4869 * ps + y0;
		curveToX: 0.3869 * ps + x0 y: 0.4381 * ps + y0 control1X: 0.3654 * ps + x0 y: 0.4697 * ps + y0 control2X: 0.3787 * ps + x0 y: 0.4534 * ps + y0;
		curveToX: 0.3996 * ps + x0 y: 0.3912 * ps + y0 control1X: 0.3954 * ps + x0 y: 0.4228 * ps + y0 control2X: 0.3996 * ps + x0 y: 0.4072 * ps + y0;
		curveToX: 0.3674 * ps + x0 y: 0.3209 * ps + y0 control1X: 0.3996 * ps + x0 y: 0.3623 * ps + y0 control2X: 0.3888 * ps + x0 y: 0.3388 * ps + y0;
		curveToX: 0.2829 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.3462 * ps + x0 y: 0.3030 * ps + y0 control2X: 0.3180 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2004 * ps + x0 y: 0.3112 * ps + y0 control1X: 0.2572 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.2297 * ps + x0 y: 0.2998 * ps + y0;
		curveToX: 0.1095 * ps + x0 y: 0.3610 * ps + y0 control1X: 0.1714 * ps + x0 y: 0.3226 * ps + y0 control2X: 0.1411 * ps + x0 y: 0.3392 * ps + y0;
		lineToX: 0.1095 * ps + x0 y: 0.2692 * ps + y0;
		curveToX: 0.2023 * ps + x0 y: 0.2277 * ps + y0 control1X: 0.1401 * ps + x0 y: 0.2506 * ps + y0 control2X: 0.1711 * ps + x0 y: 0.2368 * ps + y0;
		curveToX: 0.3000 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2339 * ps + x0 y: 0.2186 * ps + y0 control2X: 0.2664 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4440 * ps + x0 y: 0.2614 * ps + y0 control1X: 0.3599 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4079 * ps + x0 y: 0.2298 * ps + y0;
		curveToX: 0.4987 * ps + x0 y: 0.3864 * ps + y0 control1X: 0.4805 * ps + x0 y: 0.2929 * ps + y0 control2X: 0.4987 * ps + x0 y: 0.3346 * ps + y0;
		curveToX: 0.4811 * ps + x0 y: 0.4572 * ps + y0 control1X: 0.4987 * ps + x0 y: 0.4111 * ps + y0 control2X: 0.4928 * ps + x0 y: 0.4347 * ps + y0;
		curveToX: 0.4196 * ps + x0 y: 0.5324 * ps + y0 control1X: 0.4694 * ps + x0 y: 0.4793 * ps + y0 control2X: 0.4489 * ps + x0 y: 0.5044 * ps + y0;
		lineToX: 0.3766 * ps + x0 y: 0.5743 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6105 * ps + y0 control1X: 0.3613 * ps + x0 y: 0.5896 * ps + y0 control2X: 0.3504 * ps + x0 y: 0.6017 * ps + y0;
		curveToX: 0.3307 * ps + x0 y: 0.6354 * ps + y0 control1X: 0.3377 * ps + x0 y: 0.6189 * ps + y0 control2X: 0.3333 * ps + x0 y: 0.6272 * ps + y0;
		curveToX: 0.3263 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.3288 * ps + x0 y: 0.6422 * ps + y0 control2X: 0.3273 * ps + x0 y: 0.6505 * ps + y0;
		curveToX: 0.3249 * ps + x0 y: 0.7003 * ps + y0 control1X: 0.3254 * ps + x0 y: 0.6700 * ps + y0 control2X: 0.3249 * ps + x0 y: 0.6834 * ps + y0;
		lineToX: 0.3249 * ps + x0 y: 0.7604 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans64pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4098 * ps + x0 y: 0.6940 * ps + y0;
		curveToX: 0.4445 * ps + x0 y: 0.8038 * ps + y0 control1X: 0.4098 * ps + x0 y: 0.7405 * ps + y0 control2X: 0.4214 * ps + x0 y: 0.7771 * ps + y0;
		curveToX: 0.5397 * ps + x0 y: 0.8434 * ps + y0 control1X: 0.4676 * ps + x0 y: 0.8302 * ps + y0 control2X: 0.4994 * ps + x0 y: 0.8434 * ps + y0;
		curveToX: 0.6340 * ps + x0 y: 0.8033 * ps + y0 control1X: 0.5798 * ps + x0 y: 0.8434 * ps + y0 control2X: 0.6112 * ps + x0 y: 0.8300 * ps + y0;
		curveToX: 0.6686 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.6571 * ps + x0 y: 0.7767 * ps + y0 control2X: 0.6686 * ps + x0 y: 0.7402 * ps + y0;
		curveToX: 0.6335 * ps + x0 y: 0.5856 * ps + y0 control1X: 0.6686 * ps + x0 y: 0.6484 * ps + y0 control2X: 0.6569 * ps + x0 y: 0.6123 * ps + y0;
		curveToX: 0.5387 * ps + x0 y: 0.5450 * ps + y0 control1X: 0.6100 * ps + x0 y: 0.5586 * ps + y0 control2X: 0.5785 * ps + x0 y: 0.5450 * ps + y0;
		curveToX: 0.4445 * ps + x0 y: 0.5851 * ps + y0 control1X: 0.4994 * ps + x0 y: 0.5450 * ps + y0 control2X: 0.4679 * ps + x0 y: 0.5584 * ps + y0;
		curveToX: 0.4098 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.4214 * ps + x0 y: 0.6118 * ps + y0 control2X: 0.4098 * ps + x0 y: 0.6481 * ps + y0;
		moveToX: 0.6760 * ps + x0 y: 0.8400 * ps + y0;
		curveToX: 0.6086 * ps + x0 y: 0.8956 * ps + y0 control1X: 0.6564 * ps + x0 y: 0.8650 * ps + y0 control2X: 0.6340 * ps + x0 y: 0.8836 * ps + y0;
		curveToX: 0.5207 * ps + x0 y: 0.9132 * ps + y0 control1X: 0.5835 * ps + x0 y: 0.9074 * ps + y0 control2X: 0.5542 * ps + x0 y: 0.9132 * ps + y0;
		curveToX: 0.3840 * ps + x0 y: 0.8527 * ps + y0 control1X: 0.4647 * ps + x0 y: 0.9132 * ps + y0 control2X: 0.4191 * ps + x0 y: 0.8930 * ps + y0;
		curveToX: 0.3317 * ps + x0 y: 0.6940 * ps + y0 control1X: 0.3491 * ps + x0 y: 0.8120 * ps + y0 control2X: 0.3317 * ps + x0 y: 0.7591 * ps + y0;
		curveToX: 0.3844 * ps + x0 y: 0.5353 * ps + y0 control1X: 0.3317 * ps + x0 y: 0.6289 * ps + y0 control2X: 0.3493 * ps + x0 y: 0.5760 * ps + y0;
		curveToX: 0.5207 * ps + x0 y: 0.4742 * ps + y0 control1X: 0.4196 * ps + x0 y: 0.4946 * ps + y0 control2X: 0.4650 * ps + x0 y: 0.4742 * ps + y0;
		curveToX: 0.6091 * ps + x0 y: 0.4928 * ps + y0 control1X: 0.5542 * ps + x0 y: 0.4742 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.4804 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.5480 * ps + y0 control1X: 0.6344 * ps + x0 y: 0.5048 * ps + y0 control2X: 0.6567 * ps + x0 y: 0.5232 * ps + y0;
		lineToX: 0.6760 * ps + x0 y: 0.4840 * ps + y0;
		lineToX: 0.7458 * ps + x0 y: 0.4840 * ps + y0;
		lineToX: 0.7458 * ps + x0 y: 0.8434 * ps + y0;
		curveToX: 0.8571 * ps + x0 y: 0.7784 * ps + y0 control1X: 0.7933 * ps + x0 y: 0.8362 * ps + y0 control2X: 0.8304 * ps + x0 y: 0.8146 * ps + y0;
		curveToX: 0.8976 * ps + x0 y: 0.6373 * ps + y0 control1X: 0.8841 * ps + x0 y: 0.7420 * ps + y0 control2X: 0.8976 * ps + x0 y: 0.6950 * ps + y0;
		curveToX: 0.8820 * ps + x0 y: 0.5392 * ps + y0 control1X: 0.8976 * ps + x0 y: 0.6025 * ps + y0 control2X: 0.8924 * ps + x0 y: 0.5698 * ps + y0;
		curveToX: 0.8356 * ps + x0 y: 0.4542 * ps + y0 control1X: 0.8719 * ps + x0 y: 0.5086 * ps + y0 control2X: 0.8565 * ps + x0 y: 0.4803 * ps + y0;
		curveToX: 0.7116 * ps + x0 y: 0.3566 * ps + y0 control1X: 0.8018 * ps + x0 y: 0.4116 * ps + y0 control2X: 0.7604 * ps + x0 y: 0.3790 * ps + y0;
		curveToX: 0.5529 * ps + x0 y: 0.3224 * ps + y0 control1X: 0.6631 * ps + x0 y: 0.3338 * ps + y0 control2X: 0.6102 * ps + x0 y: 0.3224 * ps + y0;
		curveToX: 0.4377 * ps + x0 y: 0.3385 * ps + y0 control1X: 0.5129 * ps + x0 y: 0.3224 * ps + y0 control2X: 0.4745 * ps + x0 y: 0.3278 * ps + y0;
		curveToX: 0.3356 * ps + x0 y: 0.3854 * ps + y0 control1X: 0.4009 * ps + x0 y: 0.3489 * ps + y0 control2X: 0.3669 * ps + x0 y: 0.3645 * ps + y0;
		curveToX: 0.2155 * ps + x0 y: 0.5162 * ps + y0 control1X: 0.2845 * ps + x0 y: 0.4186 * ps + y0 control2X: 0.2445 * ps + x0 y: 0.4622 * ps + y0;
		curveToX: 0.1725 * ps + x0 y: 0.6910 * ps + y0 control1X: 0.1869 * ps + x0 y: 0.5700 * ps + y0 control2X: 0.1725 * ps + x0 y: 0.6282 * ps + y0;
		curveToX: 0.2004 * ps + x0 y: 0.8366 * ps + y0 control1X: 0.1725 * ps + x0 y: 0.7428 * ps + y0 control2X: 0.1818 * ps + x0 y: 0.7913 * ps + y0;
		curveToX: 0.2819 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.2192 * ps + x0 y: 0.8818 * ps + y0 control2X: 0.2464 * ps + x0 y: 0.9217 * ps + y0;
		curveToX: 0.4006 * ps + x0 y: 1.0333 * ps + y0 control1X: 0.3161 * ps + x0 y: 0.9900 * ps + y0 control2X: 0.3556 * ps + x0 y: 1.0158 * ps + y0;
		curveToX: 0.5446 * ps + x0 y: 1.0602 * ps + y0 control1X: 0.4455 * ps + x0 y: 1.0512 * ps + y0 control2X: 0.4935 * ps + x0 y: 1.0602 * ps + y0;
		curveToX: 0.6681 * ps + x0 y: 1.0387 * ps + y0 control1X: 0.5866 * ps + x0 y: 1.0602 * ps + y0 control2X: 0.6278 * ps + x0 y: 1.0530 * ps + y0;
		curveToX: 0.7800 * ps + x0 y: 0.9782 * ps + y0 control1X: 0.7088 * ps + x0 y: 1.0247 * ps + y0 control2X: 0.7461 * ps + x0 y: 1.0045 * ps + y0;
		lineToX: 0.8239 * ps + x0 y: 1.0324 * ps + y0;
		curveToX: 0.6906 * ps + x0 y: 1.1046 * ps + y0 control1X: 0.7832 * ps + x0 y: 1.0639 * ps + y0 control2X: 0.7388 * ps + x0 y: 1.0880 * ps + y0;
		curveToX: 0.5446 * ps + x0 y: 1.1300 * ps + y0 control1X: 0.6427 * ps + x0 y: 1.1215 * ps + y0 control2X: 0.5941 * ps + x0 y: 1.1300 * ps + y0;
		curveToX: 0.3742 * ps + x0 y: 1.0978 * ps + y0 control1X: 0.4844 * ps + x0 y: 1.1300 * ps + y0 control2X: 0.4276 * ps + x0 y: 1.1193 * ps + y0;
		curveToX: 0.2316 * ps + x0 y: 1.0050 * ps + y0 control1X: 0.3208 * ps + x0 y: 1.0766 * ps + y0 control2X: 0.2733 * ps + x0 y: 1.0457 * ps + y0;
		curveToX: 0.1364 * ps + x0 y: 0.8639 * ps + y0 control1X: 0.1899 * ps + x0 y: 0.9643 * ps + y0 control2X: 0.1582 * ps + x0 y: 0.9173 * ps + y0;
		curveToX: 0.1037 * ps + x0 y: 0.6910 * ps + y0 control1X: 0.1146 * ps + x0 y: 0.8102 * ps + y0 control2X: 0.1037 * ps + x0 y: 0.7526 * ps + y0;
		curveToX: 0.1369 * ps + x0 y: 0.5216 * ps + y0 control1X: 0.1037 * ps + x0 y: 0.6318 * ps + y0 control2X: 0.1148 * ps + x0 y: 0.5753 * ps + y0;
		curveToX: 0.2316 * ps + x0 y: 0.3800 * ps + y0 control1X: 0.1590 * ps + x0 y: 0.4679 * ps + y0 control2X: 0.1906 * ps + x0 y: 0.4207 * ps + y0;
		curveToX: 0.3771 * ps + x0 y: 0.2853 * ps + y0 control1X: 0.2736 * ps + x0 y: 0.3387 * ps + y0 control2X: 0.3221 * ps + x0 y: 0.3071 * ps + y0;
		curveToX: 0.5519 * ps + x0 y: 0.2521 * ps + y0 control1X: 0.4321 * ps + x0 y: 0.2631 * ps + y0 control2X: 0.4904 * ps + x0 y: 0.2521 * ps + y0;
		curveToX: 0.7438 * ps + x0 y: 0.2946 * ps + y0 control1X: 0.6209 * ps + x0 y: 0.2521 * ps + y0 control2X: 0.6849 * ps + x0 y: 0.2662 * ps + y0;
		curveToX: 0.8927 * ps + x0 y: 0.4152 * ps + y0 control1X: 0.8031 * ps + x0 y: 0.3229 * ps + y0 control2X: 0.8527 * ps + x0 y: 0.3631 * ps + y0;
		curveToX: 0.9484 * ps + x0 y: 0.5192 * ps + y0 control1X: 0.9172 * ps + x0 y: 0.4471 * ps + y0 control2X: 0.9357 * ps + x0 y: 0.4817 * ps + y0;
		curveToX: 0.9679 * ps + x0 y: 0.6354 * ps + y0 control1X: 0.9614 * ps + x0 y: 0.5566 * ps + y0 control2X: 0.9679 * ps + x0 y: 0.5953 * ps + y0;
		curveToX: 0.8903 * ps + x0 y: 0.8380 * ps + y0 control1X: 0.9679 * ps + x0 y: 0.7210 * ps + y0 control2X: 0.9421 * ps + x0 y: 0.7885 * ps + y0;
		curveToX: 0.6760 * ps + x0 y: 0.9152 * ps + y0 control1X: 0.8385 * ps + x0 y: 0.8875 * ps + y0 control2X: 0.7671 * ps + x0 y: 0.9132 * ps + y0;
		lineToX: 0.6760 * ps + x0 y: 0.8400 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans65pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		lineToX: 0.2458 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.5138 * ps + x0 y: 0.6871 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.3243 * ps + y0;
		moveToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.2160 * ps + x0 y: 0.7692 * ps + y0;
		lineToX: 0.1496 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans66pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2345 * ps + x0 y: 0.6080 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8751 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.8751 * ps + y0;
		curveToX: 0.5104 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.4458 * ps + x0 y: 0.8751 * ps + y0 control2X: 0.4850 * ps + x0 y: 0.8642 * ps + y0;
		curveToX: 0.5490 * ps + x0 y: 0.7413 * ps + y0 control1X: 0.5361 * ps + x0 y: 0.8203 * ps + y0 control2X: 0.5490 * ps + x0 y: 0.7866 * ps + y0;
		curveToX: 0.5104 * ps + x0 y: 0.6408 * ps + y0 control1X: 0.5490 * ps + x0 y: 0.6958 * ps + y0 control2X: 0.5361 * ps + x0 y: 0.6622 * ps + y0;
		curveToX: 0.3927 * ps + x0 y: 0.6080 * ps + y0 control1X: 0.4850 * ps + x0 y: 0.6189 * ps + y0 control2X: 0.4458 * ps + x0 y: 0.6080 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6080 * ps + y0;
		moveToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5280 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.5280 * ps + y0;
		curveToX: 0.4880 * ps + x0 y: 0.5011 * ps + y0 control1X: 0.4287 * ps + x0 y: 0.5280 * ps + y0 control2X: 0.4645 * ps + x0 y: 0.5190 * ps + y0;
		curveToX: 0.5236 * ps + x0 y: 0.4181 * ps + y0 control1X: 0.5117 * ps + x0 y: 0.4829 * ps + y0 control2X: 0.5236 * ps + x0 y: 0.4552 * ps + y0;
		curveToX: 0.4880 * ps + x0 y: 0.3356 * ps + y0 control1X: 0.5236 * ps + x0 y: 0.3813 * ps + y0 control2X: 0.5117 * ps + x0 y: 0.3538 * ps + y0;
		curveToX: 0.3805 * ps + x0 y: 0.3082 * ps + y0 control1X: 0.4645 * ps + x0 y: 0.3173 * ps + y0 control2X: 0.4287 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3879 * ps + x0 y: 0.2272 * ps + y0;
		curveToX: 0.5617 * ps + x0 y: 0.2741 * ps + y0 control1X: 0.4631 * ps + x0 y: 0.2272 * ps + y0 control2X: 0.5210 * ps + x0 y: 0.2428 * ps + y0;
		curveToX: 0.6227 * ps + x0 y: 0.4074 * ps + y0 control1X: 0.6024 * ps + x0 y: 0.3053 * ps + y0 control2X: 0.6227 * ps + x0 y: 0.3497 * ps + y0;
		curveToX: 0.5915 * ps + x0 y: 0.5138 * ps + y0 control1X: 0.6227 * ps + x0 y: 0.4520 * ps + y0 control2X: 0.6123 * ps + x0 y: 0.4874 * ps + y0;
		curveToX: 0.4997 * ps + x0 y: 0.5631 * ps + y0 control1X: 0.5706 * ps + x0 y: 0.5402 * ps + y0 control2X: 0.5400 * ps + x0 y: 0.5566 * ps + y0;
		curveToX: 0.6125 * ps + x0 y: 0.6285 * ps + y0 control1X: 0.5482 * ps + x0 y: 0.5735 * ps + y0 control2X: 0.5858 * ps + x0 y: 0.5953 * ps + y0;
		curveToX: 0.6530 * ps + x0 y: 0.7521 * ps + y0 control1X: 0.6395 * ps + x0 y: 0.6614 * ps + y0 control2X: 0.6530 * ps + x0 y: 0.7026 * ps + y0;
		curveToX: 0.5866 * ps + x0 y: 0.9030 * ps + y0 control1X: 0.6530 * ps + x0 y: 0.8172 * ps + y0 control2X: 0.6309 * ps + x0 y: 0.8675 * ps + y0;
		curveToX: 0.3976 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.5423 * ps + x0 y: 0.9384 * ps + y0 control2X: 0.4793 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans67pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6818 * ps + x0 y: 0.2833 * ps + y0;
		lineToX: 0.6818 * ps + x0 y: 0.3873 * ps + y0;
		curveToX: 0.5754 * ps + x0 y: 0.3180 * ps + y0 control1X: 0.6486 * ps + x0 y: 0.3564 * ps + y0 control2X: 0.6131 * ps + x0 y: 0.3333 * ps + y0;
		curveToX: 0.4557 * ps + x0 y: 0.2950 * ps + y0 control1X: 0.5379 * ps + x0 y: 0.3027 * ps + y0 control2X: 0.4981 * ps + x0 y: 0.2950 * ps + y0;
		curveToX: 0.2643 * ps + x0 y: 0.3717 * ps + y0 control1X: 0.3724 * ps + x0 y: 0.2950 * ps + y0 control2X: 0.3086 * ps + x0 y: 0.3206 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2201 * ps + x0 y: 0.4225 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.4961 * ps + y0;
		curveToX: 0.2643 * ps + x0 y: 0.8131 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6884 * ps + y0 control2X: 0.2201 * ps + x0 y: 0.7620 * ps + y0;
		curveToX: 0.4557 * ps + x0 y: 0.8893 * ps + y0 control1X: 0.3086 * ps + x0 y: 0.8639 * ps + y0 control2X: 0.3724 * ps + x0 y: 0.8893 * ps + y0;
		curveToX: 0.5754 * ps + x0 y: 0.8663 * ps + y0 control1X: 0.4981 * ps + x0 y: 0.8893 * ps + y0 control2X: 0.5379 * ps + x0 y: 0.8816 * ps + y0;
		curveToX: 0.6818 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.6131 * ps + x0 y: 0.8510 * ps + y0 control2X: 0.6486 * ps + x0 y: 0.8279 * ps + y0;
		lineToX: 0.6818 * ps + x0 y: 0.9000 * ps + y0;
		curveToX: 0.5719 * ps + x0 y: 0.9528 * ps + y0 control1X: 0.6473 * ps + x0 y: 0.9235 * ps + y0 control2X: 0.6107 * ps + x0 y: 0.9410 * ps + y0;
		curveToX: 0.4499 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5335 * ps + x0 y: 0.9645 * ps + y0 control2X: 0.4928 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1891 * ps + x0 y: 0.8693 * ps + y0 control1X: 0.3395 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2526 * ps + x0 y: 0.9367 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1257 * ps + x0 y: 0.8016 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7093 * ps + y0;
		curveToX: 0.1891 * ps + x0 y: 0.3156 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4752 * ps + y0 control2X: 0.1257 * ps + x0 y: 0.3829 * ps + y0;
		curveToX: 0.4499 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2526 * ps + x0 y: 0.2478 * ps + y0 control2X: 0.3395 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5729 * ps + x0 y: 0.2316 * ps + y0 control1X: 0.4935 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.5345 * ps + x0 y: 0.2199 * ps + y0;
		curveToX: 0.6818 * ps + x0 y: 0.2833 * ps + y0 control1X: 0.6117 * ps + x0 y: 0.2430 * ps + y0 control2X: 0.6480 * ps + x0 y: 0.2602 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans68pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8751 * ps + y0;
		lineToX: 0.3537 * ps + x0 y: 0.8751 * ps + y0;
		curveToX: 0.5744 * ps + x0 y: 0.8068 * ps + y0 control1X: 0.4543 * ps + x0 y: 0.8751 * ps + y0 control2X: 0.5278 * ps + x0 y: 0.8523 * ps + y0;
		curveToX: 0.6447 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.6213 * ps + x0 y: 0.7612 * ps + y0 control2X: 0.6447 * ps + x0 y: 0.6893 * ps + y0;
		curveToX: 0.5744 * ps + x0 y: 0.3766 * ps + y0 control1X: 0.6447 * ps + x0 y: 0.4933 * ps + y0 control2X: 0.6213 * ps + x0 y: 0.4218 * ps + y0;
		curveToX: 0.3537 * ps + x0 y: 0.3082 * ps + y0 control1X: 0.5278 * ps + x0 y: 0.3310 * ps + y0 control2X: 0.4543 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3385 * ps + x0 y: 0.2272 * ps + y0;
		curveToX: 0.6496 * ps + x0 y: 0.3156 * ps + y0 control1X: 0.4798 * ps + x0 y: 0.2272 * ps + y0 control2X: 0.5835 * ps + x0 y: 0.2566 * ps + y0;
		curveToX: 0.7487 * ps + x0 y: 0.5909 * ps + y0 control1X: 0.7157 * ps + x0 y: 0.3742 * ps + y0 control2X: 0.7487 * ps + x0 y: 0.4659 * ps + y0;
		curveToX: 0.6491 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7487 * ps + x0 y: 0.7166 * ps + y0 control2X: 0.7155 * ps + x0 y: 0.8089 * ps + y0;
		curveToX: 0.3385 * ps + x0 y: 0.9562 * ps + y0 control1X: 0.5827 * ps + x0 y: 0.9267 * ps + y0 control2X: 0.4792 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans69pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5968 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5817 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6056 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans70pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5250 * ps + y0;
		lineToX: 0.5236 * ps + x0 y: 0.5250 * ps + y0;
		lineToX: 0.5236 * ps + x0 y: 0.6080 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6080 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans71pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.6330 * ps + x0 y: 0.8522 * ps + y0;
		lineToX: 0.6330 * ps + x0 y: 0.6564 * ps + y0;
		lineToX: 0.4718 * ps + x0 y: 0.6564 * ps + y0;
		lineToX: 0.4718 * ps + x0 y: 0.5753 * ps + y0;
		lineToX: 0.7306 * ps + x0 y: 0.5753 * ps + y0;
		lineToX: 0.7306 * ps + x0 y: 0.8883 * ps + y0;
		curveToX: 0.6047 * ps + x0 y: 0.9498 * ps + y0 control1X: 0.6926 * ps + x0 y: 0.9153 * ps + y0 control2X: 0.6506 * ps + x0 y: 0.9358 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5588 * ps + x0 y: 0.9635 * ps + y0 control2X: 0.5098 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1901 * ps + x0 y: 0.8707 * ps + y0 control1X: 0.3438 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2546 * ps + x0 y: 0.9371 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1260 * ps + x0 y: 0.8040 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7112 * ps + y0;
		curveToX: 0.1901 * ps + x0 y: 0.3141 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4733 * ps + y0 control2X: 0.1260 * ps + x0 y: 0.3805 * ps + y0;
		curveToX: 0.4577 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2546 * ps + x0 y: 0.2474 * ps + y0 control2X: 0.3438 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.5929 * ps + x0 y: 0.2316 * ps + y0 control1X: 0.5052 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.5503 * ps + x0 y: 0.2199 * ps + y0;
		curveToX: 0.7116 * ps + x0 y: 0.2833 * ps + y0 control1X: 0.6359 * ps + x0 y: 0.2433 * ps + y0 control2X: 0.6755 * ps + x0 y: 0.2605 * ps + y0;
		lineToX: 0.7116 * ps + x0 y: 0.3883 * ps + y0;
		curveToX: 0.5954 * ps + x0 y: 0.3185 * ps + y0 control1X: 0.6751 * ps + x0 y: 0.3574 * ps + y0 control2X: 0.6364 * ps + x0 y: 0.3341 * ps + y0;
		curveToX: 0.4660 * ps + x0 y: 0.2950 * ps + y0 control1X: 0.5544 * ps + x0 y: 0.3029 * ps + y0 control2X: 0.5112 * ps + x0 y: 0.2950 * ps + y0;
		curveToX: 0.2648 * ps + x0 y: 0.3698 * ps + y0 control1X: 0.3768 * ps + x0 y: 0.2950 * ps + y0 control2X: 0.3097 * ps + x0 y: 0.3200 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2202 * ps + x0 y: 0.4196 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.4938 * ps + y0;
		curveToX: 0.2648 * ps + x0 y: 0.8146 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6907 * ps + y0 control2X: 0.2202 * ps + x0 y: 0.7648 * ps + y0;
		curveToX: 0.4660 * ps + x0 y: 0.8893 * ps + y0 control1X: 0.3097 * ps + x0 y: 0.8644 * ps + y0 control2X: 0.3768 * ps + x0 y: 0.8893 * ps + y0;
		curveToX: 0.5593 * ps + x0 y: 0.8805 * ps + y0 control1X: 0.5008 * ps + x0 y: 0.8893 * ps + y0 control2X: 0.5319 * ps + x0 y: 0.8864 * ps + y0;
		curveToX: 0.6330 * ps + x0 y: 0.8522 * ps + y0 control1X: 0.5866 * ps + x0 y: 0.8743 * ps + y0 control2X: 0.6112 * ps + x0 y: 0.8649 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans72pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5929 * ps + x0 y: 0.5260 * ps + y0;
		lineToX: 0.5929 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6916 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6916 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5929 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5929 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans73pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans74pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9054 * ps + y0;
		curveToX: 0.1843 * ps + x0 y: 1.0968 * ps + y0 control1X: 0.2345 * ps + x0 y: 0.9933 * ps + y0 control2X: 0.2178 * ps + x0 y: 1.0571 * ps + y0;
		curveToX: 0.0236 * ps + x0 y: 1.1564 * ps + y0 control1X: 0.1510 * ps + x0 y: 1.1365 * ps + y0 control2X: 0.0975 * ps + x0 y: 1.1564 * ps + y0;
		lineToX: -0.0140 * ps + x0 y: 1.1564 * ps + y0;
		lineToX: -0.0140 * ps + x0 y: 1.0734 * ps + y0;
		lineToX: 0.0168 * ps + x0 y: 1.0734 * ps + y0;
		curveToX: 0.1091 * ps + x0 y: 1.0367 * ps + y0 control1X: 0.0604 * ps + x0 y: 1.0734 * ps + y0 control2X: 0.0912 * ps + x0 y: 1.0612 * ps + y0;
		curveToX: 0.1359 * ps + x0 y: 0.9054 * ps + y0 control1X: 0.1270 * ps + x0 y: 1.0123 * ps + y0 control2X: 0.1359 * ps + x0 y: 0.9686 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans75pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5353 * ps + y0;
		lineToX: 0.5617 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6886 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3268 * ps + x0 y: 0.5670 * ps + y0;
		lineToX: 0.7145 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5846 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6051 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans76pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5895 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.5895 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans77pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2829 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4689 * ps + x0 y: 0.7233 * ps + y0;
		lineToX: 0.6559 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.8029 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.8029 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.7067 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.7067 * ps + x0 y: 0.3160 * ps + y0;
		lineToX: 0.5187 * ps + x0 y: 0.8160 * ps + y0;
		lineToX: 0.4196 * ps + x0 y: 0.8160 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.3160 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans78pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2687 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5920 * ps + x0 y: 0.8370 * ps + y0;
		lineToX: 0.5920 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6877 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5549 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.3463 * ps + y0;
		lineToX: 0.2316 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans79pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.7073 * ps + y0 control2X: 0.7381 * ps + x0 y: 0.7991 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.6157 * ps + x0 y: 0.9362 * ps + y0 control2X: 0.5340 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7994 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7076 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans80pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5822 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.5822 * ps + y0;
		curveToX: 0.4650 * ps + x0 y: 0.5465 * ps + y0 control1X: 0.4045 * ps + x0 y: 0.5822 * ps + y0 control2X: 0.4399 * ps + x0 y: 0.5703 * ps + y0;
		curveToX: 0.5026 * ps + x0 y: 0.4450 * ps + y0 control1X: 0.4901 * ps + x0 y: 0.5228 * ps + y0 control2X: 0.5026 * ps + x0 y: 0.4889 * ps + y0;
		curveToX: 0.4650 * ps + x0 y: 0.3439 * ps + y0 control1X: 0.5026 * ps + x0 y: 0.4013 * ps + y0 control2X: 0.4901 * ps + x0 y: 0.3676 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.3082 * ps + y0 control1X: 0.4399 * ps + x0 y: 0.3201 * ps + y0 control2X: 0.4045 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		moveToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.2272 * ps + y0;
		curveToX: 0.5436 * ps + x0 y: 0.2828 * ps + y0 control1X: 0.4403 * ps + x0 y: 0.2272 * ps + y0 control2X: 0.5020 * ps + x0 y: 0.2457 * ps + y0;
		curveToX: 0.6066 * ps + x0 y: 0.4450 * ps + y0 control1X: 0.5856 * ps + x0 y: 0.3196 * ps + y0 control2X: 0.6066 * ps + x0 y: 0.3737 * ps + y0;
		curveToX: 0.5436 * ps + x0 y: 0.6080 * ps + y0 control1X: 0.6066 * ps + x0 y: 0.5169 * ps + y0 control2X: 0.5856 * ps + x0 y: 0.5713 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.6632 * ps + y0 control1X: 0.5020 * ps + x0 y: 0.6448 * ps + y0 control2X: 0.4403 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6632 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans81pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.3602 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.3032 * ps + x0 y: 0.3208 * ps + y0;
		curveToX: 0.1979 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.4275 * ps + y0 control2X: 0.1979 * ps + x0 y: 0.5003 * ps + y0;
		curveToX: 0.2609 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.1979 * ps + x0 y: 0.6842 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.7568 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.3032 * ps + x0 y: 0.8636 * ps + y0 control2X: 0.3602 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.8102 * ps + y0 control1X: 0.5034 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5601 * ps + x0 y: 0.8636 * ps + y0;
		curveToX: 0.6647 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.6437 * ps + x0 y: 0.7568 * ps + y0 control2X: 0.6647 * ps + x0 y: 0.6842 * ps + y0;
		curveToX: 0.6017 * ps + x0 y: 0.3742 * ps + y0 control1X: 0.6647 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.6437 * ps + x0 y: 0.4275 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.5601 * ps + x0 y: 0.3208 * ps + y0 control2X: 0.5034 * ps + x0 y: 0.2941 * ps + y0;
		moveToX: 0.5700 * ps + x0 y: 0.9430 * ps + y0;
		lineToX: 0.6999 * ps + x0 y: 1.0851 * ps + y0;
		lineToX: 0.5807 * ps + x0 y: 1.0851 * ps + y0;
		lineToX: 0.4728 * ps + x0 y: 0.9684 * ps + y0;
		curveToX: 0.4479 * ps + x0 y: 0.9699 * ps + y0 control1X: 0.4621 * ps + x0 y: 0.9690 * ps + y0 control2X: 0.4538 * ps + x0 y: 0.9695 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4424 * ps + x0 y: 0.9702 * ps + y0 control2X: 0.4370 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.8678 * ps + y0 control1X: 0.3293 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2472 * ps + x0 y: 0.9362 * ps + y0;
		curveToX: 0.0939 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.1245 * ps + x0 y: 0.7991 * ps + y0 control2X: 0.0939 * ps + x0 y: 0.7073 * ps + y0;
		curveToX: 0.1857 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.0939 * ps + x0 y: 0.4772 * ps + y0 control2X: 0.1245 * ps + x0 y: 0.3854 * ps + y0;
		curveToX: 0.4318 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2472 * ps + x0 y: 0.2483 * ps + y0 control2X: 0.3293 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.6769 * ps + x0 y: 0.3170 * ps + y0 control1X: 0.5340 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.6157 * ps + x0 y: 0.2483 * ps + y0;
		curveToX: 0.7687 * ps + x0 y: 0.5924 * ps + y0 control1X: 0.7381 * ps + x0 y: 0.3854 * ps + y0 control2X: 0.7687 * ps + x0 y: 0.4772 * ps + y0;
		curveToX: 0.7175 * ps + x0 y: 0.8097 * ps + y0 control1X: 0.7687 * ps + x0 y: 0.6770 * ps + y0 control2X: 0.7516 * ps + x0 y: 0.7495 * ps + y0;
		curveToX: 0.5700 * ps + x0 y: 0.9430 * ps + y0 control1X: 0.6836 * ps + x0 y: 0.8699 * ps + y0 control2X: 0.6344 * ps + x0 y: 0.9144 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans82pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4816 * ps + x0 y: 0.6144 * ps + y0;
		curveToX: 0.5431 * ps + x0 y: 0.6603 * ps + y0 control1X: 0.5028 * ps + x0 y: 0.6215 * ps + y0 control2X: 0.5233 * ps + x0 y: 0.6368 * ps + y0;
		curveToX: 0.6037 * ps + x0 y: 0.7570 * ps + y0 control1X: 0.5633 * ps + x0 y: 0.6837 * ps + y0 control2X: 0.5835 * ps + x0 y: 0.7159 * ps + y0;
		lineToX: 0.7038 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5978 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5046 * ps + x0 y: 0.7692 * ps + y0;
		curveToX: 0.4343 * ps + x0 y: 0.6720 * ps + y0 control1X: 0.4805 * ps + x0 y: 0.7203 * ps + y0 control2X: 0.4570 * ps + x0 y: 0.6880 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.6481 * ps + y0 control1X: 0.4118 * ps + x0 y: 0.6561 * ps + y0 control2X: 0.3810 * ps + x0 y: 0.6481 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.6481 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1359 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.2272 * ps + y0;
		curveToX: 0.5451 * ps + x0 y: 0.2794 * ps + y0 control1X: 0.4419 * ps + x0 y: 0.2272 * ps + y0 control2X: 0.5041 * ps + x0 y: 0.2446 * ps + y0;
		curveToX: 0.6066 * ps + x0 y: 0.4371 * ps + y0 control1X: 0.5861 * ps + x0 y: 0.3143 * ps + y0 control2X: 0.6066 * ps + x0 y: 0.3668 * ps + y0;
		curveToX: 0.5744 * ps + x0 y: 0.5514 * ps + y0 control1X: 0.6066 * ps + x0 y: 0.4830 * ps + y0 control2X: 0.5959 * ps + x0 y: 0.5211 * ps + y0;
		curveToX: 0.4816 * ps + x0 y: 0.6144 * ps + y0 control1X: 0.5532 * ps + x0 y: 0.5817 * ps + y0 control2X: 0.5223 * ps + x0 y: 0.6027 * ps + y0;
		moveToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.5670 * ps + y0;
		lineToX: 0.3586 * ps + x0 y: 0.5670 * ps + y0;
		curveToX: 0.4660 * ps + x0 y: 0.5343 * ps + y0 control1X: 0.4061 * ps + x0 y: 0.5670 * ps + y0 control2X: 0.4419 * ps + x0 y: 0.5561 * ps + y0;
		curveToX: 0.5026 * ps + x0 y: 0.4371 * ps + y0 control1X: 0.4904 * ps + x0 y: 0.5122 * ps + y0 control2X: 0.5026 * ps + x0 y: 0.4798 * ps + y0;
		curveToX: 0.4660 * ps + x0 y: 0.3409 * ps + y0 control1X: 0.5026 * ps + x0 y: 0.3945 * ps + y0 control2X: 0.4904 * ps + x0 y: 0.3624 * ps + y0;
		curveToX: 0.3586 * ps + x0 y: 0.3082 * ps + y0 control1X: 0.4419 * ps + x0 y: 0.3191 * ps + y0 control2X: 0.4061 * ps + x0 y: 0.3082 * ps + y0;
		lineToX: 0.2345 * ps + x0 y: 0.3082 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans83pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5729 * ps + x0 y: 0.2511 * ps + y0;
		lineToX: 0.5729 * ps + x0 y: 0.3473 * ps + y0;
		curveToX: 0.4670 * ps + x0 y: 0.3073 * ps + y0 control1X: 0.5355 * ps + x0 y: 0.3294 * ps + y0 control2X: 0.5002 * ps + x0 y: 0.3160 * ps + y0;
		curveToX: 0.3708 * ps + x0 y: 0.2941 * ps + y0 control1X: 0.4338 * ps + x0 y: 0.2985 * ps + y0 control2X: 0.4017 * ps + x0 y: 0.2941 * ps + y0;
		curveToX: 0.2463 * ps + x0 y: 0.3253 * ps + y0 control1X: 0.3171 * ps + x0 y: 0.2941 * ps + y0 control2X: 0.2756 * ps + x0 y: 0.3045 * ps + y0;
		curveToX: 0.2028 * ps + x0 y: 0.4142 * ps + y0 control1X: 0.2173 * ps + x0 y: 0.3462 * ps + y0 control2X: 0.2028 * ps + x0 y: 0.3758 * ps + y0;
		curveToX: 0.2316 * ps + x0 y: 0.4874 * ps + y0 control1X: 0.2028 * ps + x0 y: 0.4464 * ps + y0 control2X: 0.2124 * ps + x0 y: 0.4708 * ps + y0;
		curveToX: 0.3420 * ps + x0 y: 0.5270 * ps + y0 control1X: 0.2511 * ps + x0 y: 0.5037 * ps + y0 control2X: 0.2879 * ps + x0 y: 0.5169 * ps + y0;
		lineToX: 0.4015 * ps + x0 y: 0.5392 * ps + y0;
		curveToX: 0.5641 * ps + x0 y: 0.6134 * ps + y0 control1X: 0.4751 * ps + x0 y: 0.5532 * ps + y0 control2X: 0.5293 * ps + x0 y: 0.5779 * ps + y0;
		curveToX: 0.6169 * ps + x0 y: 0.7550 * ps + y0 control1X: 0.5993 * ps + x0 y: 0.6486 * ps + y0 control2X: 0.6169 * ps + x0 y: 0.6958 * ps + y0;
		curveToX: 0.5456 * ps + x0 y: 0.9157 * ps + y0 control1X: 0.6169 * ps + x0 y: 0.8256 * ps + y0 control2X: 0.5931 * ps + x0 y: 0.8792 * ps + y0;
		curveToX: 0.3376 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4984 * ps + x0 y: 0.9521 * ps + y0 control2X: 0.4290 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2272 * ps + x0 y: 0.9586 * ps + y0 control1X: 0.3031 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2663 * ps + x0 y: 0.9664 * ps + y0;
		curveToX: 0.1066 * ps + x0 y: 0.9240 * ps + y0 control1X: 0.1885 * ps + x0 y: 0.9508 * ps + y0 control2X: 0.1483 * ps + x0 y: 0.9393 * ps + y0;
		lineToX: 0.1066 * ps + x0 y: 0.8224 * ps + y0;
		curveToX: 0.2243 * ps + x0 y: 0.8732 * ps + y0 control1X: 0.1467 * ps + x0 y: 0.8449 * ps + y0 control2X: 0.1859 * ps + x0 y: 0.8618 * ps + y0;
		curveToX: 0.3376 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2627 * ps + x0 y: 0.8846 * ps + y0 control2X: 0.3005 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.4679 * ps + x0 y: 0.8571 * ps + y0 control1X: 0.3939 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.4373 * ps + x0 y: 0.8792 * ps + y0;
		curveToX: 0.5138 * ps + x0 y: 0.7623 * ps + y0 control1X: 0.4985 * ps + x0 y: 0.8349 * ps + y0 control2X: 0.5138 * ps + x0 y: 0.8033 * ps + y0;
		curveToX: 0.4806 * ps + x0 y: 0.6783 * ps + y0 control1X: 0.5138 * ps + x0 y: 0.7265 * ps + y0 control2X: 0.5028 * ps + x0 y: 0.6985 * ps + y0;
		curveToX: 0.3727 * ps + x0 y: 0.6329 * ps + y0 control1X: 0.4588 * ps + x0 y: 0.6582 * ps + y0 control2X: 0.4229 * ps + x0 y: 0.6430 * ps + y0;
		lineToX: 0.3127 * ps + x0 y: 0.6212 * ps + y0;
		curveToX: 0.1530 * ps + x0 y: 0.5524 * ps + y0 control1X: 0.2391 * ps + x0 y: 0.6066 * ps + y0 control2X: 0.1859 * ps + x0 y: 0.5836 * ps + y0;
		curveToX: 0.1037 * ps + x0 y: 0.4220 * ps + y0 control1X: 0.1201 * ps + x0 y: 0.5211 * ps + y0 control2X: 0.1037 * ps + x0 y: 0.4777 * ps + y0;
		curveToX: 0.1716 * ps + x0 y: 0.2697 * ps + y0 control1X: 0.1037 * ps + x0 y: 0.3575 * ps + y0 control2X: 0.1263 * ps + x0 y: 0.3068 * ps + y0;
		curveToX: 0.3595 * ps + x0 y: 0.2140 * ps + y0 control1X: 0.2171 * ps + x0 y: 0.2325 * ps + y0 control2X: 0.2798 * ps + x0 y: 0.2140 * ps + y0;
		curveToX: 0.4640 * ps + x0 y: 0.2233 * ps + y0 control1X: 0.3937 * ps + x0 y: 0.2140 * ps + y0 control2X: 0.4286 * ps + x0 y: 0.2171 * ps + y0;
		curveToX: 0.5729 * ps + x0 y: 0.2511 * ps + y0 control1X: 0.4995 * ps + x0 y: 0.2295 * ps + y0 control2X: 0.5358 * ps + x0 y: 0.2387 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans84pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0348 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6515 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6515 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0348 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0348 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans85pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2238 * ps + x0 y: 0.6700 * ps + y0;
		curveToX: 0.2663 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2238 * ps + x0 y: 0.7482 * ps + y0 control2X: 0.2380 * ps + x0 y: 0.8045 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.8903 * ps + y0 control1X: 0.2946 * ps + x0 y: 0.8732 * ps + y0 control2X: 0.3405 * ps + x0 y: 0.8903 * ps + y0;
		curveToX: 0.5412 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4671 * ps + x0 y: 0.8903 * ps + y0 control2X: 0.5129 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.5837 * ps + x0 y: 0.6700 * ps + y0 control1X: 0.5695 * ps + x0 y: 0.8045 * ps + y0 control2X: 0.5837 * ps + x0 y: 0.7482 * ps + y0;
		lineToX: 0.5837 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6828 * ps + x0 y: 0.6823 * ps + y0;
		curveToX: 0.6120 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.6828 * ps + x0 y: 0.7773 * ps + y0 control2X: 0.6592 * ps + x0 y: 0.8491 * ps + y0;
		curveToX: 0.4040 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5651 * ps + x0 y: 0.9461 * ps + y0 control2X: 0.4958 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1950 * ps + x0 y: 0.8976 * ps + y0 control1X: 0.3119 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2422 * ps + x0 y: 0.9461 * ps + y0;
		curveToX: 0.1247 * ps + x0 y: 0.6823 * ps + y0 control1X: 0.1481 * ps + x0 y: 0.8491 * ps + y0 control2X: 0.1247 * ps + x0 y: 0.7773 * ps + y0;
		lineToX: 0.1247 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans86pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3239 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0456 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1486 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3796 * ps + x0 y: 0.8409 * ps + y0;
		lineToX: 0.6110 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7135 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4357 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.9562 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans87pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0710 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1706 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3239 * ps + x0 y: 0.8434 * ps + y0;
		lineToX: 0.4767 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.5876 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.7409 * ps + x0 y: 0.8434 * ps + y0;
		lineToX: 0.8937 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.9938 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.8107 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.6867 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5329 * ps + x0 y: 0.3234 * ps + y0;
		lineToX: 0.3776 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2536 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0710 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:11'!
drawVeraSans88pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1008 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.2067 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3879 * ps + x0 y: 0.4982 * ps + y0;
		lineToX: 0.5700 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6760 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.4416 * ps + x0 y: 0.5773 * ps + y0;
		lineToX: 0.6916 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.5856 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6461 * ps + y0;
		lineToX: 0.1740 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0676 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.3278 * ps + x0 y: 0.5670 * ps + y0;
		lineToX: 0.1008 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans89pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1418 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3439 * ps + x0 y: 0.5270 * ps + y0;
		lineToX: 0.5446 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6506 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.3927 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.2936 * ps + x0 y: 0.6090 * ps + y0;
		lineToX: 0.0358 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans90pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0939 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6667 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.6667 * ps + x0 y: 0.3024 * ps + y0;
		lineToX: 0.2057 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6779 * ps + x0 y: 0.8732 * ps + y0;
		lineToX: 0.6779 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0827 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.0827 * ps + x0 y: 0.8810 * ps + y0;
		lineToX: 0.5436 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0939 * ps + x0 y: 0.3102 * ps + y0;
		lineToX: 0.0939 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans91pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1237 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.2135 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.2135 * ps + x0 y: 1.0182 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 1.0182 * ps + y0;
		lineToX: 0.3307 * ps + x0 y: 1.0880 * ps + y0;
		lineToX: 0.1237 * ps + x0 y: 1.0880 * ps + y0;
		lineToX: 0.1237 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans92pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.1208 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.3747 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.2917 * ps + x0 y: 1.0490 * ps + y0;
		lineToX: 0.0378 * ps + x0 y: 0.2272 * ps + y0;
		lineToX: 0.1208 * ps + x0 y: 0.2272 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans93pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3420 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3420 * ps + x0 y: 1.0880 * ps + y0;
		lineToX: 0.1349 * ps + x0 y: 1.0880 * ps + y0;
		lineToX: 0.1349 * ps + x0 y: 1.0182 * ps + y0;
		lineToX: 0.2516 * ps + x0 y: 1.0182 * ps + y0;
		lineToX: 0.2516 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.1349 * ps + x0 y: 0.2662 * ps + y0;
		lineToX: 0.1349 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.3420 * ps + x0 y: 0.1964 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans94pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.4347 * ps + x0 y: 0.2238 * ps + y0;
		lineToX: 0.4787 * ps + x0 y: 0.2238 * ps + y0;
		lineToX: 0.6701 * ps + x0 y: 0.4161 * ps + y0;
		lineToX: 0.6115 * ps + x0 y: 0.4747 * ps + y0;
		lineToX: 0.4982 * ps + x0 y: 0.3605 * ps + y0;
		lineToX: 0.4982 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4152 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4152 * ps + x0 y: 0.3605 * ps + y0;
		lineToX: 0.3010 * ps + x0 y: 0.4747 * ps + y0;
		lineToX: 0.2424 * ps + x0 y: 0.4161 * ps + y0;
		lineToX: 0.4347 * ps + x0 y: 0.2238 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans95pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.0866 * ps + x0 y: 0.6647 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6207 * ps + y0;
		lineToX: 0.2785 * ps + x0 y: 0.4288 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.4874 * ps + y0;
		lineToX: 0.2233 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.8185 * ps + x0 y: 0.6012 * ps + y0;
		lineToX: 0.8185 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.2233 * ps + x0 y: 0.6842 * ps + y0;
		lineToX: 0.3371 * ps + x0 y: 0.7980 * ps + y0;
		lineToX: 0.2785 * ps + x0 y: 0.8566 * ps + y0;
		lineToX: 0.0866 * ps + x0 y: 0.6647 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans96pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.2170 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.3547 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.2800 * ps + x0 y: 0.3390 * ps + y0;
		lineToX: 0.1208 * ps + x0 y: 0.1564 * ps + y0;
		lineToX: 0.2170 * ps + x0 y: 0.1564 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans97pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		curveToX: 0.2297 * ps + x0 y: 0.7062 * ps + y0 control1X: 0.3079 * ps + x0 y: 0.6813 * ps + y0 control2X: 0.2577 * ps + x0 y: 0.6896 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.7911 * ps + y0 control1X: 0.2017 * ps + x0 y: 0.7228 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.7511 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8673 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.8230 * ps + y0 control2X: 0.1981 * ps + x0 y: 0.8484 * ps + y0;
		curveToX: 0.3049 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2401 * ps + x0 y: 0.8859 * ps + y0 control2X: 0.2687 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4245 * ps + x0 y: 0.8424 * ps + y0 control1X: 0.3547 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.3945 * ps + x0 y: 0.8776 * ps + y0;
		curveToX: 0.4699 * ps + x0 y: 0.7013 * ps + y0 control1X: 0.4548 * ps + x0 y: 0.8069 * ps + y0 control2X: 0.4699 * ps + x0 y: 0.7599 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6813 * ps + y0;
		lineToX: 0.3805 * ps + x0 y: 0.6813 * ps + y0;
		moveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0;
		lineToX: 0.5597 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.8732 * ps + y0;
		curveToX: 0.3932 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.4494 * ps + x0 y: 0.9064 * ps + y0 control2X: 0.4238 * ps + x0 y: 0.9310 * ps + y0;
		curveToX: 0.2809 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.3626 * ps + x0 y: 0.9625 * ps + y0 control2X: 0.3252 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1471 * ps + x0 y: 0.9235 * ps + y0 control1X: 0.2249 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.1803 * ps + x0 y: 0.9547 * ps + y0;
		curveToX: 0.0978 * ps + x0 y: 0.7970 * ps + y0 control1X: 0.1143 * ps + x0 y: 0.8919 * ps + y0 control2X: 0.0978 * ps + x0 y: 0.8497 * ps + y0;
		curveToX: 0.1593 * ps + x0 y: 0.6578 * ps + y0 control1X: 0.0978 * ps + x0 y: 0.7355 * ps + y0 control2X: 0.1183 * ps + x0 y: 0.6891 * ps + y0;
		curveToX: 0.3439 * ps + x0 y: 0.6110 * ps + y0 control1X: 0.2007 * ps + x0 y: 0.6266 * ps + y0 control2X: 0.2622 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6110 * ps + y0;
		lineToX: 0.4699 * ps + x0 y: 0.6022 * ps + y0;
		curveToX: 0.4289 * ps + x0 y: 0.5065 * ps + y0 control1X: 0.4699 * ps + x0 y: 0.5608 * ps + y0 control2X: 0.4562 * ps + x0 y: 0.5289 * ps + y0;
		curveToX: 0.3146 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4019 * ps + x0 y: 0.4837 * ps + y0 control2X: 0.3638 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2233 * ps + x0 y: 0.4835 * ps + y0 control1X: 0.2834 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2529 * ps + x0 y: 0.4760 * ps + y0;
		curveToX: 0.1379 * ps + x0 y: 0.5172 * ps + y0 control1X: 0.1937 * ps + x0 y: 0.4910 * ps + y0 control2X: 0.1652 * ps + x0 y: 0.5022 * ps + y0;
		lineToX: 0.1379 * ps + x0 y: 0.4342 * ps + y0;
		curveToX: 0.2336 * ps + x0 y: 0.4059 * ps + y0 control1X: 0.1707 * ps + x0 y: 0.4215 * ps + y0 control2X: 0.2026 * ps + x0 y: 0.4121 * ps + y0;
		curveToX: 0.3239 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2645 * ps + x0 y: 0.3994 * ps + y0 control2X: 0.2946 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5011 * ps + x0 y: 0.4576 * ps + y0 control1X: 0.4030 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4621 * ps + x0 y: 0.4166 * ps + y0;
		curveToX: 0.5597 * ps + x0 y: 0.6442 * ps + y0 control1X: 0.5402 * ps + x0 y: 0.4987 * ps + y0 control2X: 0.5597 * ps + x0 y: 0.5608 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans98pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.5246 * ps + x0 y: 0.6172 * ps + y0 control2X: 0.5109 * ps + x0 y: 0.5654 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.4713 * ps + y0 control1X: 0.4565 * ps + x0 y: 0.4902 * ps + y0 control2X: 0.4193 * ps + x0 y: 0.4713 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.3242 * ps + x0 y: 0.4713 * ps + y0 control2X: 0.2868 * ps + x0 y: 0.4902 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2324 * ps + x0 y: 0.5654 * ps + y0 control2X: 0.2189 * ps + x0 y: 0.6172 * ps + y0;
		curveToX: 0.2594 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.2189 * ps + x0 y: 0.7493 * ps + y0 control2X: 0.2324 * ps + x0 y: 0.8012 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.8951 * ps + y0 control1X: 0.2868 * ps + x0 y: 0.8764 * ps + y0 control2X: 0.3242 * ps + x0 y: 0.8951 * ps + y0;
		curveToX: 0.4836 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.4193 * ps + x0 y: 0.8951 * ps + y0 control2X: 0.4565 * ps + x0 y: 0.8764 * ps + y0;
		curveToX: 0.5246 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5109 * ps + x0 y: 0.8012 * ps + y0 control2X: 0.5246 * ps + x0 y: 0.7493 * ps + y0;
		moveToX: 0.2189 * ps + x0 y: 0.4923 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.4200 * ps + y0 control1X: 0.2378 * ps + x0 y: 0.4598 * ps + y0 control2X: 0.2616 * ps + x0 y: 0.4357 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.3192 * ps + x0 y: 0.4041 * ps + y0 control2X: 0.3537 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.4752 * ps + y0 control1X: 0.4601 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.5140 * ps + x0 y: 0.4225 * ps + y0;
		curveToX: 0.6178 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.5970 * ps + x0 y: 0.5280 * ps + y0 control2X: 0.6178 * ps + x0 y: 0.5973 * ps + y0;
		curveToX: 0.5553 * ps + x0 y: 0.8912 * ps + y0 control1X: 0.6178 * ps + x0 y: 0.7692 * ps + y0 control2X: 0.5970 * ps + x0 y: 0.8385 * ps + y0;
		curveToX: 0.3937 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.5140 * ps + x0 y: 0.9440 * ps + y0 control2X: 0.4601 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.2902 * ps + x0 y: 0.9469 * ps + y0 control1X: 0.3537 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.3192 * ps + x0 y: 0.9625 * ps + y0;
		curveToX: 0.2189 * ps + x0 y: 0.8742 * ps + y0 control1X: 0.2616 * ps + x0 y: 0.9310 * ps + y0 control2X: 0.2378 * ps + x0 y: 0.9067 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.9562 * ps + y0;
		lineToX: 0.1286 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.1964 * ps + y0;
		lineToX: 0.2189 * ps + x0 y: 0.4923 * ps + y0! !

!Morphic3Canvas methodsFor: 'Bitstream Vera Sans glyphs' stamp: 'jmv 5/9/2011 16:12'!
drawVeraSans99pointSize: ps atX: x0 y: y0
	self
		moveToX: 0.5256 * ps + x0 y: 0.4303 * ps + y0;
		lineToX: 0.5256 * ps + x0 y: 0.5143 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.4830 * ps + y0 control1X: 0.5002 * ps + x0 y: 0.5003 * ps + y0 control2X: 0.4746 * ps + x0 y: 0.4899 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.4723 * ps + y0 control1X: 0.4235 * ps + x0 y: 0.4759 * ps + y0 control2X: 0.3978 * ps + x0 y: 0.4723 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.5280 * ps + y0 control1X: 0.3135 * ps + x0 y: 0.4723 * ps + y0 control2X: 0.2682 * ps + x0 y: 0.4908 * ps + y0;
		curveToX: 0.1877 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.2038 * ps + x0 y: 0.5647 * ps + y0 control2X: 0.1877 * ps + x0 y: 0.6165 * ps + y0;
		curveToX: 0.2360 * ps + x0 y: 0.8390 * ps + y0 control1X: 0.1877 * ps + x0 y: 0.7500 * ps + y0 control2X: 0.2038 * ps + x0 y: 0.8019 * ps + y0;
		curveToX: 0.3718 * ps + x0 y: 0.8942 * ps + y0 control1X: 0.2682 * ps + x0 y: 0.8758 * ps + y0 control2X: 0.3135 * ps + x0 y: 0.8942 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.8839 * ps + y0 control1X: 0.3978 * ps + x0 y: 0.8942 * ps + y0 control2X: 0.4235 * ps + x0 y: 0.8908 * ps + y0;
		curveToX: 0.5256 * ps + x0 y: 0.8522 * ps + y0 control1X: 0.4746 * ps + x0 y: 0.8768 * ps + y0 control2X: 0.5002 * ps + x0 y: 0.8662 * ps + y0;
		lineToX: 0.5256 * ps + x0 y: 0.9352 * ps + y0;
		curveToX: 0.4474 * ps + x0 y: 0.9616 * ps + y0 control1X: 0.5005 * ps + x0 y: 0.9469 * ps + y0 control2X: 0.4745 * ps + x0 y: 0.9557 * ps + y0;
		curveToX: 0.3620 * ps + x0 y: 0.9703 * ps + y0 control1X: 0.4207 * ps + x0 y: 0.9674 * ps + y0 control2X: 0.3923 * ps + x0 y: 0.9703 * ps + y0;
		curveToX: 0.1657 * ps + x0 y: 0.8927 * ps + y0 control1X: 0.2796 * ps + x0 y: 0.9703 * ps + y0 control2X: 0.2142 * ps + x0 y: 0.9445 * ps + y0;
		curveToX: 0.0929 * ps + x0 y: 0.6832 * ps + y0 control1X: 0.1172 * ps + x0 y: 0.8409 * ps + y0 control2X: 0.0929 * ps + x0 y: 0.7711 * ps + y0;
		curveToX: 0.1662 * ps + x0 y: 0.4728 * ps + y0 control1X: 0.0929 * ps + x0 y: 0.5940 * ps + y0 control2X: 0.1174 * ps + x0 y: 0.5239 * ps + y0;
		curveToX: 0.3678 * ps + x0 y: 0.3961 * ps + y0 control1X: 0.2153 * ps + x0 y: 0.4217 * ps + y0 control2X: 0.2826 * ps + x0 y: 0.3961 * ps + y0;
		curveToX: 0.4489 * ps + x0 y: 0.4049 * ps + y0 control1X: 0.3955 * ps + x0 y: 0.3961 * ps + y0 control2X: 0.4225 * ps + x0 y: 0.3991 * ps + y0;
		curveToX: 0.5256 * ps + x0 y: 0.4303 * ps + y0 control1X: 0.4753 * ps + x0 y: 0.4104 * ps + y0 control2X: 0.5008 * ps + x0 y: 0.4189 * ps + y0! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 2/15/2013 17:16'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc

	^self ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: nil strokeDashArrayOffset: nil! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 18:46'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao
	"
	DEJAR FINALMENTE unicamente metodos que tomen la currentTransformation. NADA en coordenadas de pantalla. NADA con una M2x3 argumento!!
	"
	| bw fc bc sda sdao dx dy rx ry phi center |
	self revisar.
	
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	center _ currentTransformation externalizePosition: mcx@mcy.
	dx _ currentTransformation externalizeDelta: mrx@0.
	dy _ currentTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	self displayCoordarcCenterX: center x centerY: center y radiusX: rx radiusY: ry rotationAngle: phi start: 0.0 sweep:  Float pi * 2.
	
	self closeTrajectory.

	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/6/2014 19:28'!
fillRectangle: aRectangle color: aColor

	self rectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom borderWidth: 0 borderColor: aColor fillColor: aColor strokeDashArray: nil strokeDashArrayOffset: nil! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/6/2014 19:28'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
"self halt"
	self rectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom borderWidth: borderWidth borderColor: baseColorForBorder fillColor: aColor strokeDashArray: nil strokeDashArrayOffset: nil! !

!Morphic3Canvas methodsFor: 'trajectories - aux' stamp: 'jmv 7/14/2014 21:31'!
finishTrajectory
	"Para la iteracion sobre todo el morph, poner un mensaje especial... no necesita el rectangulo, ya lo sabe!!"
	
	"Atencion. Si vamos a hacer computeFill, mas vale que hayamos hecho closeTrajectory primero!!!!!!!!!!"
	| pixelIndex |
	(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
		computeFill ifTrue: [
			"This is actually an error condition. If we are asked to fill, then the path should be closed.
			This is a workaround. It width > 0.0, we'll be adding a visible edge to the path."
			self displayCoordbasicLineToX: firstX y: firstY ].
		].
	
	"A discontinuity in the trajectory"
	self flag: #lastAndButLast.	"Remove if we just use current (and not last) in #addCurrentToTrajectory"
	"Warning: must be the last call to #addCurrentToTrajectory before the blend. That's why it is AFTER tht closing of the path if computeFill"
	currentX _ nil.
	currentY _ nil.
	self addCurrentToTrajectory.

	lastControl2Point _ nil.

	morphBoundsTop _ morphBoundsTop min: boundsTop.
	morphBoundsBottom _ morphBoundsBottom max: boundsBottom.
	morphBoundsLeft _ morphBoundsLeft min: boundsLeft.
	morphBoundsRight _ morphBoundsRight max: boundsRight.

	computeFill
		ifTrue: [
			^ self blendTrajectoryAndFill ].

"	self
		overTop: boundsTop bottom: boundsBottom left: boundsLeft right: boundsRight
		do: [ :displayX :displayY :pixelIndex |"
	boundsTop to: boundsBottom do: [ :displayY |
		pixelIndex _ displayY * formWidth +  boundsLeft.
		boundsLeft to: boundsRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			self
				blendAtX: displayX y: displayY pixelIndex: pixelIndex
				redIsInside: false greenIsInside: false blueIsInside: false.
			]].
	
	Display forceToScreen.

	auxDataBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1 height: boundsBottom-boundsTop+1.
	auxDataBitBlt destForm bits: auxAlphas.
	auxDataBitBlt copyBits! !

!Morphic3Canvas methodsFor: 'accessing' stamp: 'jmv 11/8/2012 23:52'!
fontForeColor: aColor
	fontForeColor _ aColor! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 16:45'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self halt! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 17:05'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor

	self rectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom borderWidth: borderWidth borderColor: borderColor fillColor: nil strokeDashArray: nil strokeDashArrayOffset: nil! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 17:10'!
image: aForm at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
"	self halt"
super image: aForm at: aPoint! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 17:10'!
image: aForm multipliedBy: aColor at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
"no!!!!!!!!"
"	self halt"
super image: aForm multipliedBy: aColor at: aPoint! !

!Morphic3Canvas methodsFor: 'initialization' stamp: 'jmv 4/7/2014 09:24'!
initialize
	super initialize.
	autoHintText _ false! !

!Morphic3Canvas methodsFor: 'trajectories - aux' stamp: 'jmv 6/1/2010 11:41'!
initializeTrajectory
	
	lastDisplayY _ nil.
	firstX _ nil.
	firstY _ nil.
	trajectoryLength _ 0.0! !

!Morphic3Canvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 21:37'!
into: aMorph
	super into: aMorph.
	morphBoundsTop _ formHeight - 1.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ formWidth - 1.
	morphBoundsRight _ 0.! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:43'!
l: dx

	| x2 y2 p2 displayY2 |
	x2 _ currentClientX - dx.
	y2 _ currentClientY.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	self displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x2! !

!Morphic3Canvas methodsFor: 'accessing' stamp: 'jmv 6/14/2010 10:07'!
lastControl2Point

	^lastControl2Point! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:26'!
li: x y: y
	"no confundir con l: !!!!!!!!"
	
	| p2 displayY2 |
	p2 _ currentTransformation externalizePosition: x@y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	self displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x.
	currentClientY _ y! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/7/2014 13:24'!
line: pt1 to: pt2 width: w color: c

	self setupBorderWidth: w borderColor: c fillColor: c strokeDashArray: nil strokeDashArrayOffset: nil.
	self initializeTrajectory.
	self moveToX: pt1 x y: pt1 y.
	self lineToX: pt2 x y: pt2 y.
	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'categorizar' stamp: 'jmv 11/8/2012 23:26'!
lineToX: x2 y: y2
	| p2 |
	p2 _ currentTransformation externalizePosition: x2@y2.
	self displayCoordlineToX: p2 x y: p2 y! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/7/2014 09:35'!
m: x y: y

	| p  displayY |
	p _ currentTransformation externalizePosition: x@y.
	displayY _ p y.
	autoHintText ifTrue: [
		displayY _ displayY rounded ].
	self displayCoordmoveToX: p x y: displayY.
	currentClientX _ x.
	currentClientY _ y! !

!Morphic3Canvas methodsFor: 'categorizar' stamp: 'jmv 11/8/2012 23:24'!
moveToX: x y: y
	| p |
	p _ currentTransformation externalizePosition: x@y.
	self displayCoordmoveToX: p x y: p y! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 2/15/2013 17:46'!
overTop: top bottom: bottom left: left right: right do: aBlock
	| pixelIndex |

	top to: bottom do: [ :displayY |
		pixelIndex _ displayY * formWidth +  left.
		left to: right do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			aBlock value: displayX value: displayY value: pixelIndex.
		].
	]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 16:55'!
pathArcCommand: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless

	| to rxp ryp xAxisRotationP p0 r |
	p0 _ coordinatesAreRelative ifTrue: [ self currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ]].
	
	1 to: toPoints size do: [ :i |
		coordinatesAreRelative
			ifTrue: [
				to _ p0 + (currentTransformation externalizeDelta: (toPoints at: i)).
				p0 _ to.
			]
			ifFalse: [
				to _ currentTransformation externalizePosition: (toPoints at: i).
			].
	
		xAxisRotationP _ (currentTransformation externalizeDelta: (Point r: 1 degrees: (xAxisRotations at: i) radiansToDegrees)) theta.

		"Wrong!!"
		"rxp _ aMorph externalizeScalarToWorld: rx.
		ryp _ aMorph externalizeScalarToWorld: ry."
		"Right!!"
		r _ rs at: i.
		rxp _ (currentTransformation externalizeDelta: r x@0) r.
		ryp _ (currentTransformation externalizeDelta: 0@r y) r.

		self
			displayCoordarcToX: to x y: to y
			radiusX: rxp radiusY: ryp
			angleOfXAxis: xAxisRotationP
			largeFlag: (doLargerArcs at: i)
			sweepFlag: (useIncreasingAngless at: i) ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 16:56'!
pathCloseCommand
	self closeTrajectory! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/9/2012 17:10'!
pathCommands: aCollection borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| bw fc bc sda sdao |
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	aCollection do: [ :command |
		command runOn: self ].

	self finishTrajectory! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:01'!
pathCurveToCommand: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points

	| p0 to c1 c2 |
	p0 _ coordinatesAreRelative ifTrue: [ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	1 to: toPoints size do: [ :i |
		coordinatesAreRelative
			ifTrue: [
				to _ p0 + (currentTransformation externalizeDelta: (toPoints at: i)).
				c1 _ p0 + (currentTransformation externalizeDelta: (control1Points at: i)).
				c2 _ p0 + (currentTransformation externalizeDelta: (control2Points at: i)).
				p0 _ to.
			]
			ifFalse: [
				to _ currentTransformation externalizePosition: (toPoints at: i).
				c1 _ currentTransformation externalizePosition: (control1Points at: i).
				c2 _ currentTransformation externalizePosition: (control2Points at: i).
			].
		self displayCoordcurveToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:00'!
pathCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints control2Points: control2Points

	| p0 to c1 c2 |
	p0 _ self currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ].
	c2 _ self lastControl2Point ifNil: [ p0].
	1 to: toPoints size do: [ :i |
		c1 _ p0 + p0 - c2.
		coordinatesAreRelative
			ifTrue: [
				to _ p0 + (currentTransformation externalizeDelta: (toPoints at: i)).
				c2 _ p0 + (currentTransformation externalizeDelta: (control2Points at: i)).
				p0 _ to.
			]
			ifFalse: [
				to _ currentTransformation externalizePosition: (toPoints at: i).
				c2 _ currentTransformation externalizePosition: (control2Points at: i).
			].
		self displayCoordcurveToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:02'!
pathLineToCommand: coordinatesAreRelative points: points
	| p |
	p _ coordinatesAreRelative ifTrue: [ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	points do: [ :each |
		p _ coordinatesAreRelative
			ifTrue: [ p + (currentTransformation externalizeDelta: each) ]
			ifFalse: [ currentTransformation externalizePosition: each ].
		self displayCoordlineToX: p x y: p y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:04'!
pathLineToHCommand: coordinatesAreRelative xs: xs

	| p internalP |
	p _ self currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ].
	coordinatesAreRelative ifFalse: [
		internalP _ currentTransformation internalizeDelta: p].
	xs do: [ :each |
		coordinatesAreRelative
			ifTrue: [ p _ p + (currentTransformation externalizeDelta: each@0) ]
			ifFalse: [
				internalP _ each @internalP y.
				p _  currentTransformation externalizePosition: internalP ].
		self displayCoordlineToX: p x y: p y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:05'!
pathLineToVCommand: coordinatesAreRelative ys: ys

	| p internalP |
	p _ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ].
	coordinatesAreRelative ifFalse: [
		internalP _ currentTransformation internalizeDelta: p].
	ys do: [ :each |
		coordinatesAreRelative
			ifTrue: [ p _ p + (currentTransformation externalizeDelta: 0@each) ]
			ifFalse: [
				internalP _ internalP x @ each.
				p _  currentTransformation externalizePosition: internalP ].
		self displayCoordlineToX: p x y: p y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:06'!
pathMoveToCommand: coordinatesAreRelative points: points

	| p isFirst |
	isFirst _ true.
	p _ coordinatesAreRelative ifTrue: [ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	points do: [ :each |
		p _ coordinatesAreRelative
			ifTrue: [ p + (currentTransformation externalizeDelta: each) ]
			ifFalse: [ currentTransformation externalizePosition: each ].
		isFirst
			ifTrue: [ self displayCoordmoveToX: p x y: p y ]
			ifFalse: [ self displayCoordlineToX: p x y: p y ].
		isFirst _ false ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:07'!
pathQuadraticCurveToCommand: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints

	| p0 to c |
	p0 _ coordinatesAreRelative ifTrue: [ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	1 to: toPoints size do: [ :i |
		coordinatesAreRelative
			ifTrue: [
				to _ p0 + (currentTransformation externalizeDelta: (toPoints at: i)).
				c _ p0 + (currentTransformation externalizeDelta: (controlPoints at: i)).
				p0 _ to.
			]
			ifFalse: [
				to _ currentTransformation externalizePosition: (toPoints at: i).
				c _ currentTransformation externalizePosition: (controlPoints at: i).
			].
		self displayCoordquadraticBezierCurveToX: to x y: to y controlX: c x y: c y ]! !

!Morphic3Canvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 11/9/2012 17:09'!
pathQuadraticCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints

	| p0 to c |
	p0 _ self currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ].
	c _ self lastControl2Point ifNil: [ p0].
	1 to: toPoints size do: [ :i |
		c _ p0 + p0 - c.
		coordinatesAreRelative
			ifTrue: [
				to _ p0 + (currentTransformation externalizeDelta: (toPoints at: i)).
				p0 _ to.
			]
			ifFalse: [
				to _ currentTransformation externalizePosition: (toPoints at: i).
			].
		self displayCoordquadraticBezierCurveToX: to x y: to y controlX: c x y: c y ]! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 18:39'!
polyLine: points borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| displayPoints bw fc bc sda sdao |
	displayPoints _ points collect: [ :p |
		currentTransformation externalizePosition: p ].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self displayCoordspolyLine: displayPoints borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 19:12'!
quadraticBezier: points borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	| displayPoints bw fc bc sda sdao |
	displayPoints _ points collect: [ :p |
		currentTransformation externalizePosition: p ].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self displayCoordquadraticBezier: displayPoints borderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:42'!
r: dx

	| x2 y2 p2 displayY2 |
	x2 _ currentClientX + dx.
	y2 _ currentClientY.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	self displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x2! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 23:22'!
rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| bw fc bc p1 p2 p3 p4 sda sdao |
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	p1 _ currentTransformation externalizePosition: xMin@yMin.
	p2 _ currentTransformation externalizePosition: xMax@yMin.
	p3 _ currentTransformation externalizePosition: xMax@yMax.
	p4 _ currentTransformation externalizePosition: xMin@yMax.
	
	self
		displayCoordmoveToX: p1 x y: p1 y;
		displayCoordlineToX: p2 x y: p2 y;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordlineToX: p4 x y: p4 y;
		closeTrajectory.

	self finishTrajectory
! !

!Morphic3Canvas methodsFor: 'private' stamp: 'jmv 2/15/2013 17:46'!
removeSpuriousTranslucencyFloatAndBlendOver: aColor
"Norma de prefijos para variables
f1XXX: 0.0..1.0 (float)
bXXX: 0..255 (entero o float)
b127XXX: 0..127 (entero o float)
bFFXXX: 0..255
bFFbFFXXX: 0..255*255
b16XXX: 0..65535
bFFFFXXXX0..65535
y asi siguiendo...
"
	| t b l r isTransparentClose word 
	f1rAlpha f1aaRAlpha f1componentRAlpha
	f1gAlpha f1aaGAlpha f1componentGAlpha
	b255bAlpha f1aaBAlpha f1componentBAlpha
	f1red f1green f1blue componentAlphasWord componentAlphasIfOnlyOpaqueColorsWord 
	f1backR f1backG f1backB pixelIndex pixelIndex2 bFFred bFFgreen bFFblue |
	f1backR _ aColor red.
	f1backG _ aColor green.
	f1backB _ aColor blue.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			componentAlphasWord _ alphas at: pixelIndex.
			"Si el resultado es opaco, no hay nada que hacer (nuestro form ya tiene el alpha puesto a 255)"
			componentAlphasWord = 16rFFFFFF ifFalse: [
				f1rAlpha _ ((componentAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
				f1gAlpha _ ((componentAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
				b255bAlpha _ (componentAlphasWord bitAnd: 16rFF) / 255.0.

				isTransparentClose _ false.
				"ver en #addCurrentToTrajectory. Deberia alcanzar con la mitad. Pero incluir deltaForSubPixP"
				t _ (displayY-AAWidth) truncated.
				t < 0 ifTrue: [ t _ 0 ].
				b _ (displayY+AAWidth) truncated+1.
				b > (formHeight-1) ifTrue: [ b _ formHeight-1 ].
				l _ (displayX-AAWidth) truncated.
				l < 0 ifTrue: [ l _ 0 ].
				r _ (displayX+AAWidth) truncated+1.
				r > (formWidth-1) ifTrue: [ r _ formWidth-1 ].
"				self
					overTop: t bottom: b left: l right: r
					do: [ :displayX2 :displayY2 :pixelIndex2 |"
				t to: b do: [ :displayY2 |
					pixelIndex2 _ displayY2 * formWidth +  l.
					l to: r do: [ :displayX2 |
						pixelIndex2 _ pixelIndex2 + 1.
						isTransparentClose _ isTransparentClose or: 	"chequeo solo el verde creo que esta bien para esto"
							[((alphasIfOpaqueColors at: pixelIndex2) bitAnd: 16rFF00) = 0]]].
				isTransparentClose
					ifFalse: [
						componentAlphasIfOnlyOpaqueColorsWord _ alphasIfOpaqueColors at: pixelIndex.
						componentAlphasIfOnlyOpaqueColorsWord = componentAlphasWord
							ifTrue: [
								f1rAlpha _ 255.
								f1gAlpha _ 255.
								b255bAlpha _ 255]
							ifFalse: [
								f1rAlpha > 0 ifTrue: [
									f1aaRAlpha _ ((componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF0000) >> 16) / 255.0.
									f1componentRAlpha _ f1rAlpha / f1aaRAlpha.
									f1rAlpha _ f1componentRAlpha].
								f1gAlpha > 0 ifTrue: [
									f1aaGAlpha _ ((componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF00) >> 8) / 255.0.
									f1componentGAlpha _ f1gAlpha / f1aaGAlpha.
									f1gAlpha _ f1componentGAlpha].
								b255bAlpha > 0 ifTrue: [
									f1aaBAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF) / 255.0.
									f1componentBAlpha _ b255bAlpha / f1aaBAlpha.
									b255bAlpha _ f1componentBAlpha]]].

				"Optimization. If we are away from transparent, and color was opaque, do nothing (i.e. don't blend)"
				(isTransparentClose or: [ componentAlphasIfOnlyOpaqueColorsWord ~= componentAlphasWord ]) ifTrue: [
					word _ form bits at: pixelIndex.
					f1red _ ((word bitAnd: 16rFF0000) >> 16) / 255.0.
					f1green _ ((word bitAnd: 16rFF00) >> 8) / 255.0.
					f1blue _ (word bitAnd: 16rFF) / 255.0.

					f1red _ f1rAlpha * f1red + (1.0-f1rAlpha * f1backR).
					f1green _ f1gAlpha * f1green + (1.0-f1gAlpha * f1backG).
					f1blue _ b255bAlpha * f1blue + (1.0-b255bAlpha * f1backB).

					bFFred _ (f1red * 255) rounded bitAnd: 16rFF.
					bFFgreen _ (f1green * 255) rounded bitAnd: 16rFF.
					bFFblue _ (f1blue * 255) rounded bitAnd: 16rFF.

					word _ (255 bitShift: 24) bitOr: (bFFred bitShift: 16) + (bFFgreen bitShift: 8) + bFFblue.

					form bits at: pixelIndex put: word ]]]]! !

!Morphic3Canvas methodsFor: 'private' stamp: 'jmv 2/15/2013 17:47'!
removeSpuriousTranslucencyIntAndBlendOver: aColor
"Norma de prefijos para variables
f1XXX: 0.0..1.0 (float)
bXXX: 0..255 (entero o float)
b127XXX: 0..127 (entero o float)
bFFXXX: 0..255
bFFbFFXXX: 0..255*255
b16XXX: 0..65535
bFFFFXXXX0..65535
y asi siguiendo...
"
	| t b l r isTransparentClose word 
	b255rAlpha b255aaRAlpha b255componentRAlpha
	b255gAlpha b255aaGAlpha b255componentGAlpha
	b255bAlpha b255aaBAlpha b255componentBAlpha
	 componentAlphasWord componentAlphasIfOnlyOpaqueColorsWord 
	bFFbackR bFFbackG bFFbackB pixelIndex pixelIndex2 bFFred bFFgreen bFFblue bFFbFFred bFFbFFgreen bFFbFFblue dilatedWidth |
	self flag: #revisarM3.	"NO tengo derecho a usar borderWidth. Puede haber 7 mil!!!!!!"
	dilatedWidth _ borderWidth + AAWidth.
	bFFbackR _ (aColor red * 255.0) truncated.
	bFFbackG _ (aColor green * 255.0) truncated.
	bFFbackB _ (aColor blue * 255.0) truncated.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			componentAlphasWord _ alphas at: pixelIndex.
			"Si el resultado es opaco, no hay nada que hacer (nuestro form ya tiene el alpha puesto a 255)"
			componentAlphasWord = 16rFFFFFF ifFalse: [
				b255rAlpha _ (componentAlphasWord bitShift: -16) bitAnd: 16rFF.
				b255gAlpha _ (componentAlphasWord bitShift: -8) bitAnd: 16rFF.
				b255bAlpha _ componentAlphasWord bitAnd: 16rFF.

				isTransparentClose _ false.
				t _ (displayY -dilatedWidth) truncated +1.
				t < 0 ifTrue: [ t _ 0 ].
				b _ (displayY +dilatedWidth) truncated.
				b > (formHeight - 1) ifTrue: [ b _ formHeight - 1 ].
				l _ (displayX -dilatedWidth -1) truncated +1.
				l < 0 ifTrue: [ l _ 0 ].
				r _ (displayX +dilatedWidth +1) truncated.
				r > (formWidth - 1) ifTrue: [ r _ formWidth - 1 ].
"				self
					overTop: t bottom: b left: l right: r
					do: [ :displayX2 :displayY2 :pixelIndex2 |"
				t to: b do: [ :displayY2 |
					pixelIndex2 _ displayY2 * formWidth +  l.
					l to: r do: [ :displayX2 |
						pixelIndex2 _ pixelIndex2 + 1.
						isTransparentClose _ isTransparentClose or: 	"chequeo solo el verde creo que esta bien para esto"
							[((alphasIfOpaqueColors at: pixelIndex2) bitAnd: 16rFF00) = 0]]].
				isTransparentClose
					ifFalse: [
						componentAlphasIfOnlyOpaqueColorsWord _ alphasIfOpaqueColors at: pixelIndex.
						componentAlphasIfOnlyOpaqueColorsWord = componentAlphasWord
							ifTrue: [
								b255rAlpha _ 255.
								b255gAlpha _ 255.
								b255bAlpha _ 255]
							ifFalse: [
								b255rAlpha > 0 ifTrue: [
									b255aaRAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitShift: -16) bitAnd: 16rFF.
									b255componentRAlpha _ 255.0 * b255rAlpha / b255aaRAlpha.
									b255rAlpha _ b255componentRAlpha truncated].
								b255gAlpha > 0 ifTrue: [
									b255aaGAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitShift: -8) bitAnd: 16rFF.
									b255componentGAlpha _ 255.0 * b255gAlpha / b255aaGAlpha.
									b255gAlpha _ b255componentGAlpha truncated].
								b255bAlpha > 0 ifTrue: [
									b255aaBAlpha _ componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF.
									b255componentBAlpha _ 255.0 * b255bAlpha / b255aaBAlpha.
									b255bAlpha _ b255componentBAlpha truncated]]].

				"Optimization. If we are away from transparent, and color was opaque, do nothing (i.e. don't blend)"
				(isTransparentClose or: [ componentAlphasIfOnlyOpaqueColorsWord ~= componentAlphasWord ]) ifTrue: [
					word _ form bits at: pixelIndex.
					bFFred _ (word bitShift: -16) bitAnd: 16rFF.
					bFFgreen _ (word bitShift: -8) bitAnd: 16rFF.
					bFFblue _ word bitAnd: 16rFF.

					bFFbFFred _ b255rAlpha * bFFred + (255-b255rAlpha * bFFbackR).
					bFFbFFgreen _ b255gAlpha * bFFgreen + (255-b255gAlpha * bFFbackG).
					bFFbFFblue _ b255bAlpha * bFFblue + (255-b255bAlpha * bFFbackB).

					"hacer la aritmetica correcta, *26 y el shift..."
					bFFred _ (bFFbFFred // 255) rounded bitAnd: 16rFF.
					bFFgreen _ (bFFbFFgreen // 255) rounded bitAnd: 16rFF.
					bFFblue _ (bFFbFFblue // 255) rounded bitAnd: 16rFF.

					word _ (255 bitShift: 24) bitOr: (bFFred bitShift: 16) + (bFFgreen bitShift: 8) + bFFblue.

					form bits at: pixelIndex put: word ]]]]! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 16:46'!
reverseRectangleBorder: r borderWidth: borderWidth
	self halt! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 7/7/2014 22:44'!
roundBottomRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| bw fc bc corner1 corner2 corner3 corner4 dx dy p5 p6 p7 p8 rx ry phi doLargerArc useIncreasingAngles sda sdao |
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax
			borderWidth: mbw borderColor: mbc fillColor: mfc
			strokeDashArray: sda strokeDashArrayOffset: sdao ].
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	corner1 _ currentTransformation externalizePosition: xMin@yMin.
	corner2 _ currentTransformation externalizePosition: xMax@yMin.
	corner3 _ currentTransformation externalizePosition: xMax@yMax.
	corner4 _ currentTransformation externalizePosition: xMin@yMax.
	dx _ currentTransformation externalizeDelta: mrx@0.
	dy _ currentTransformation externalizeDelta: 0@mry.
	p5 _ corner3 - dy.
	p6 _ corner3 - dx.
	p7 _ corner4 + dx.
	p8 _ corner4 - dy.
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	
	doLargerArc _ false.
	useIncreasingAngles _ true.
	self
		displayCoordmoveToX: corner1 x y: corner1 y ;
		displayCoordlineToX: corner2 x y: corner2 y;
		displayCoordlineToX: p5 x y: p5 y;
		displayCoordarcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p7 x y: p7 y;
		displayCoordarcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		closeTrajectory.

	self finishTrajectory
! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 17:02'!
roundRect: aRectangle color: aColor radius: r
self roundRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom rx: r ry: r borderWidth: 0 borderColor: aColor fillColor: aColor strokeDashArray: nil strokeDashArrayOffset: nil! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 16:44'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self halt! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 16:45'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self halt! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 11/8/2012 23:22'!
roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| bw fc bc corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 p5 p6 p7 p8 rx ry phi doLargerArc useIncreasingAngles sda sdao |
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax
			borderWidth: mbw borderColor: mbc fillColor: mfc
			strokeDashArray: sda strokeDashArrayOffset: sdao ].
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	corner1 _ currentTransformation externalizePosition: xMin@yMin.
	corner2 _ currentTransformation externalizePosition: xMax@yMin.
	corner3 _ currentTransformation externalizePosition: xMax@yMax.
	corner4 _ currentTransformation externalizePosition: xMin@yMax.
	dx _ currentTransformation externalizeDelta: mrx@0.
	dy _ currentTransformation externalizeDelta: 0@mry.
	p1 _ corner1 + dy.
	p2 _ corner1 + dx.
	p3 _ corner2 - dx.
	p4 _ corner2 + dy.
	p5 _ corner3 - dy.
	p6 _ corner3 - dx.
	p7 _ corner4 + dx.
	p8 _ corner4 - dy.
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	
	doLargerArc _ false.
	useIncreasingAngles _ true.
	self
		displayCoordmoveToX: p1 x y: p1 y ;
		displayCoordarcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordarcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p5 x y: p5 y;
		displayCoordarcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p7 x y: p7 y;
		displayCoordarcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		closeTrajectory.

	self finishTrajectory
! !

!Morphic3Canvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 7/7/2014 22:40'!
roundTopRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: mfc strokeDashArray: msda strokeDashArrayOffset: msdao

	| bw fc bc corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 rx ry phi doLargerArc useIncreasingAngles sda sdao |
	sda _ msda ifNotNil: [ msda collect: [ :each | currentTransformation externalizeScalar: each ]].
	sdao _ msdao ifNotNil: [ currentTransformation externalizeScalar: msdao ].
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax
			borderWidth: mbw borderColor: mbc fillColor: mfc
			strokeDashArray: sda strokeDashArrayOffset: sdao ].
	mbc ifNil: [ mfc ifNil: [ ^self ]].
	bw _ currentTransformation externalizeScalar: mbw.
	fc _ mfc ifNotNil: [ mfc externalizedWith: currentTransformation ].
	bc _ mbc ifNotNil: [ mbc externalizedWith: currentTransformation ].
	self setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao.
	self initializeTrajectory.

	corner1 _ currentTransformation externalizePosition: xMin@yMin.
	corner2 _ currentTransformation externalizePosition: xMax@yMin.
	corner3 _ currentTransformation externalizePosition: xMax@yMax.
	corner4 _ currentTransformation externalizePosition: xMin@yMax.
	dx _ currentTransformation externalizeDelta: mrx@0.
	dy _ currentTransformation externalizeDelta: 0@mry.
	p1 _ corner1 + dy.
	p2 _ corner1 + dx.
	p3 _ corner2 - dx.
	p4 _ corner2 + dy.
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	
	doLargerArc _ false.
	useIncreasingAngles _ true.
	self
		displayCoordmoveToX: p1 x y: p1 y ;
		displayCoordarcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordarcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: corner3 x y: corner3 y;
		displayCoordlineToX: corner4 x y: corner4 y;
		closeTrajectory.

	self finishTrajectory
! !

!Morphic3Canvas methodsFor: 'private' stamp: 'jmv 7/11/2014 13:49'!
setForm: aForm
	"
	World extent: World extent
	"
	| hack s |
	super setForm: aForm.
	formWidth _ aForm width.
	formHeight _ aForm height.
	s _ aForm bits size.
	(auxAlphas isNil or: [ auxAlphas size ~= s ])
		ifTrue: [
			auxAlphas _ WordArray new: s.
			auxEdgeCounts _ WordArray new: s.
			morphAtPixel _ Array new: s.
			alphas _ nil.
			alphasIfOpaqueColors _ nil.
			hack _ Form extent: form extent depth: 32 bits: auxAlphas.
			auxDataBitBlt _ BitBlt toForm: hack.
			auxDataBitBlt fillBitmap: (Bitmap with: 0).
			auxDataBitBlt combinationRule: Form over ]
		ifFalse: [
			"Not really needed, actually..."
			self flag: #revisarM3.
			"Perhaps this should be done in an ifCurtailed: block in case of Morphic drawing failure..."
			auxDataBitBlt destRect: form boundingBox.
			auxDataBitBlt destForm bits: auxAlphas.
			auxDataBitBlt copyBits.
			auxDataBitBlt destForm bits: auxEdgeCounts.
			auxDataBitBlt copyBits.
			morphAtPixel atAllPut: 0.
			alphas ifNotNil: [
				auxDataBitBlt destForm bits: alphas.
				auxDataBitBlt copyBits.
				auxDataBitBlt destForm bits: alphasIfOpaqueColors.
				auxDataBitBlt copyBits ]]! !

!Morphic3Canvas methodsFor: 'trajectories - aux' stamp: 'jmv 2/15/2013 17:47'!
setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao
	"Nib is round!!"
	"fb y fc seran no colores, sino quien me dara colores, p.ej. para gradientes
	
	Hay 4 casos aqui:
	
	1) Solo stroke.
		- fc isNil
		- bw > AAWidth
		
	2) Solo fill
		- bc = fc	//para facilitar el algoritmo
		- bw = 0.0	// O tan chiquito, que lo descarto

	3) Fill + stroke, ambos del mismo color, y con borde de ancho < que AAWidth
		Entra en el caso anterior sin hacer nada.

	4) Fill + stroke, de distinto color, o con ancho grande
		- bc ~= fc
		- bw >= AAWidth (Si viene ancho finito, pero fillColor es distinto de borderColor, ENSANCHAR el borde)
		- La trayectoria debe ser cerrada, o se cerrara con un trazo visible
	"
	| bcToUse wantsBorder |
	computeFill _ fc notNil and: [ (fc isKindOf: Color) not or: [ fc isTransparent not ]].
	bcToUse _ bc.
	wantsBorder _ bc notNil and: [ (bc isKindOf: Color) not or: [ bc isTransparent not ]] and: [ bw > 0.0 ].
	borderWidth _ wantsBorder ifTrue: [ bw ] ifFalse: [ 0.0 ].

	"Decision dificil. Aqui podriamos decir borderWidtn < 0.1 por ejemplo. Y asi evitariamos que bordes muy finos
	le roben el lugar y ocupen completamente al fill. Pero cualquier threshold introduce una discontinuidad.
	En realidad, si el color de fill es distinto del stroke y el borde es < que el aaw, ya estamos sonados.
	Seria mejor que los senders nunca pidan esa condicion 
			(o sea, que si los bordes son finos, o en todo caso, se dibuje fill y sobre ello stroke en 2 operaciones distintas,
			como hacen todos los renderers de SVG).
	Jugar con #experLinesAndShapes2, ver como las lineas de la fila de abajo, con w=0 parecen mas anchas.
	Ahora empiezo a creer que podemos decir que si el bw < AAW/3, entonces lo que los bordes le roban al fill (de area)
	seria mas que lo que los bordes pierden si ni los pongo, y ese es un threshold razonable...
	"
"	(computeFill and: [ borderWidth = 0.0 ])"
	(computeFill and: [ borderWidth < (AAWidth*0.33) ])
		ifTrue: [
			bcToUse _ fc.
			borderWidth _ 0.0]		"Bordes tan delgados que al transparentarlos se perderian, prefiero que no le roben lugar al fill"
		ifFalse: [
			fc ~= bc
				ifTrue: [		"Bordes que quiero mostrar. No son tan delgados, o no hay fill. Asegurar que tengan el ancho necesario para muestrearse"
					borderWidth _ borderWidth max: AAWidth ]].

	"#experLinesAndShapes y #experLinesAndShapes2 no dejan dudas. Cuando  estamos en el modo bw=0, necesitamos mas hops. No se aun por que.
	#experLinesAndShapes3"
	hopsFactor _ borderWidth < AAWidth
		ifTrue: [ borderWidth*10 < AAWidth 
			ifTrue: [ 14 ]	"#experLinesAndShapes"
			ifFalse: [ 8 ]]	"#experLinesAndShapes3"
		ifFalse: [ 3 ].		"#experLinesAndShapes4"

	boundsTop _ formHeight - 1.		"drawable bottom. Will later be refined."
	boundsBottom _ 0.				"drawable top. Will later be refined."
	boundsLeft _ formWidth - 1.		"drawable right. Will later be refined."
	boundsRight _ 0.					"drawable left. Will later be refined."
	
	fAlpha _ nil. fRed _ nil. fGreen _ nil. fBlue _ nil.
	bAlpha _ nil. bRed _ nil. bGreen _ nil. bBlue _ nil.
	(fc isKindOf: Color)
		ifTrue: [
			fAlpha _ (fc alpha * 255.0) rounded.
			fRed _ (fc red * 255.0) rounded.
			fGreen _ (fc green * 255.0) rounded.
			fBlue _ (fc blue * 255.0) rounded.
			fillColor _ nil ]
		ifFalse: [
			fillColor _ fc ].

	(bcToUse isKindOf: Color)
		ifTrue: [
			"ojo. ver def de bw mas arriba. Si hemos ensanchado el borde, contrarrestar bajando el alpha."
			borderWidth = bw | (borderWidth=0) ifTrue: [
				bAlpha _ (bcToUse alpha * 255.0) rounded ]
			ifFalse: [
				bAlpha _ (bcToUse alpha * 255.0 * bw / borderWidth) rounded ].
			bRed _ (bcToUse red * 255.0) rounded.
			bGreen _ (bcToUse green * 255.0) rounded.
			bBlue _ (bcToUse blue * 255.0) rounded.
			borderColor _ nil ]
		ifFalse: [
			borderColor _ bcToUse ].

	strokeDashArray _ nil.
	strokeDashArrayOffset _ nil.
	sda ifNotNil: [
		borderWidth > 0.0 ifTrue: [
			strokeDashArray _ sda withIndexCollect: [ :each :i |
				 i odd ifTrue: [ each - AAWidth ] ifFalse: [ each ]] .
			strokeDashArrayOffset _ sdao ]]! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 16:43'!
stencil: stencilForm at: aPoint color: aColor
	self halt! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/3/2014 17:03'!
textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc
"ojo que es un quad, o sea un rect en coordenadas del morph..."
"	self halt"
super textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc! !

!Morphic3Canvas methodsFor: 'stroke font elements' stamp: 'jmv 4/12/2014 18:43'!
u: dy

	| newY p2 displayY2 |
	newY _ currentClientY + dy.
	p2 _ currentTransformation externalizePosition: currentClientX@newY.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	self displayCoordlineToX: p2 x y: displayY2.
	currentClientY _ newY! !

!Morphic3Canvas methodsFor: 'private' stamp: 'jmv 6/8/2010 10:01'!
useComponentAlphas
	| s |
	s _ form bits size.
	alphas _ WordArray new: s.
	alphasIfOpaqueColors _ WordArray new: s! !

!Morphic3Canvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 7/7/2014 22:45'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
"	self halt"
"
	| hbw |
	self 
		roundRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom 
		rx: r ry: r borderWidth: bw borderColor: aColor fillColor: insideColor strokeDashArray: nil strokeDashArrayOffset: nil.
	hbw _ bw *0.5.
	self
		rectangleMinX: aRectangle left+hbw maxX: aRectangle right-hbw minY: aRectangle top+hbw maxY: aRectangle top + lh-hbw
		borderWidth: 0 borderColor: aColor 
		fillColor: aColor
		strokeDashArray: nil strokeDashArrayOffset: nil.
	"	
	
	self 
		roundTopRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle top+lh 
		rx: r ry: r borderWidth: bw borderColor: aColor fillColor: aColor
		strokeDashArray: nil strokeDashArrayOffset: nil.
	self
		roundBottomRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top+lh maxY: aRectangle bottom
		rx: r ry: r borderWidth: bw borderColor: aColor fillColor: insideColor
		strokeDashArray: nil strokeDashArrayOffset: nil.! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 09:46'!
zMark1blendFloatWithBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Experimental. Soportando fondos con alpha correctamente.
	Por ahora usando aritmetica Float.
	Si no es muy caro, es la posta!!
	(blend5 era la version integer, que parece no andar bien... CREO QUE LA ARREGLE!! Aunque increiblemente anda mas rapido esta
	Asegurarse que no pase a large integer, y usar trucos para dividir por casi-potencias-de-2)
	Tratar de hacer que si el fondo es opaco ande tan rapido como blendCurrent... (igual, no estamos lejos!!)
	
	Guau!!
	http://graphics.stanford.edu/papers/merging-sig81/
	Ahi muestra como si siempre tenes los colores premultiplicados por alpha, no necesitas dividir.
	El problema es que en Squeak los forms no estan premultiplicados por alpha... Y si alguna vez usamos Display con alpha, que solo la vm de linux soporta, seguramente sera sin premultiplicar.
	O sea, es posible que necesite dividir al final. Igual, serian muchas menos divisiones
	
	Optimizaciones posibles:
	1) Considerar que el fondo siempre va a ser opaco. Ahi, las formilas dan que va a quedar opaco tambien el resultado, y el formuleo se reduce bastante
	2) Medio tirada de los pelos, pero creo que rápida: Si estoy blendeando sobre fondo opaco, como 1). Si el fondo es traslúcido, descartarlo, y considerar a lo que pongo arriba como opaco.
	El problema de estas ideas es que hago pelota el antialiasing en el borde..."

	"Supporting a traslucent background and result is now correct. To test, use a morph with traslucent colors and do:
| f |
self drawOn: Display getCanvas.
Display forceToScreen.
f _ Form extent: 200@200 depth: 32.
f fillColor: (Color r: 1 g: 1 b: 0 alpha: 0.2).
f fillColor: (Color r: 1 g:  0 b: 1 alpha: 0.0).
self drawOn: f getCanvas.
(f colorAt: 120@120) print.
f colorAt: 120@120 put: (Color r: 1 g: 0 b: 0 alpha: 0.2).
f displayOn: Display at: 150@1 clippingBox: Display boundingBox rule: Form blend fillColor: nil
I believe both images should look the same, but the one with traslucent background is grayed out.
"
	"The 'extended' alphaX parameters also define which color to blend over which other color.
	rAlphaX, gAlphaX and bAlphaX mean:
		alphaX inside [0 .. 127]	-> alpha  = (alphaX/127.0) for blending borderColor over background (outer edge of border)
		alphaX inside [128 .. 255] -> alpha = (alphaX-128.0 / 127.0) is the alpha for blending fillColor over borderColor (inner edge of border)"
	| backgroundWord backgroundColorA backgroundColorR backgroundColorG backgroundColorB resultColorR resultColorG resultColorB resultColorA resultWord bits borderFillAbyBorderA borderFillUbyFillA borderBackgroundAbyBorderA foregroundA resultRA resultGA resultBA foregroundAByR resultR foregroundAByG resultG foregroundAByB resultB alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside fillComponent borderComponent borderAlpha fillAlpha subpixX |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ redAlpha _ 127-redAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ 127-greenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ 127-blueAlpha. isBlueInside _ false ]].
	
	
	form depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			greenAlpha > 63 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"
				form pixelValueAt: (pixelIndex \\ formWidth) @(pixelIndex // formWidth) put: 1
				"form
					pixelValueAt: pixelIndex >> 5
					put: 1"	"Color black pixelValueForDepth: 1" ].
			^ self].

	bits _ form bits.
	backgroundWord _ bits at: pixelIndex.
	backgroundColorA _ ((backgroundWord bitShift: -24) bitAnd: 16rFF) / 255.0.
	backgroundColorR _ ((backgroundWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	backgroundColorG _ ((backgroundWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	backgroundColorB _ (backgroundWord bitAnd: 16rFF) / 255.0.
	"Cual es el alpha a aplicar a los alphas? No esta bien definido, porque es el los forms usan el mismo alpha para los 3 colores (nosotros somos mas detallados!!)
	A falta de algo mejor, tomar el verde"
	"PEro para calcular cada color estoy calculando su alpha... No se si es necesario!! quizas meter el verde tambien adentro, y de esa manera ignorar el alpha que me dan para blendear en los subpixels... quizas sea mejor como esta"
	subpixX _ x-SubPixDelta.
	borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isRedInside
		ifTrue: [
			fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			borderFillAbyBorderA _ (redAlpha/127.0) * borderAlpha.
			borderFillUbyFillA _ (127- redAlpha/127.0) * fillAlpha.
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA ]
		ifFalse: [
			borderBackgroundAbyBorderA _ redAlpha/127.0  * borderAlpha.
			foregroundA _ borderBackgroundAbyBorderA ].
	resultRA _ 1.0 - ((1.0 - foregroundA) * (1.0  - backgroundColorA)).
	resultRA = 0.0
		ifTrue: [ resultR _ 0.0 ]
		ifFalse: [
			borderComponent _ (bRed ifNil: [borderColor redAtX: subpixX y: y])/ 255.0.
			isRedInside
				ifTrue: [
					fillComponent _ (fRed ifNil: [fillColor redAtX: subpixX y: y]) / 255.0.
					foregroundAByR _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]
				ifFalse: [							"Esto tambien ya lo pregunte mas arriba"
					foregroundAByR _ borderBackgroundAbyBorderA * borderComponent ].
			resultR _( foregroundAByR  + (backgroundColorA * backgroundColorR * (1.0 - foregroundA))) / resultRA ].

	borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: x y: y]) / 255.0.
	isGreenInside
		ifTrue: [
			fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: x y: y]) / 255.0.
			borderFillAbyBorderA _ (greenAlpha/127.0) * borderAlpha.
			borderFillUbyFillA _ (127- greenAlpha/127.0) * fillAlpha.
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA ]
		ifFalse: [
			borderBackgroundAbyBorderA _ greenAlpha/127.0  * borderAlpha.
			foregroundA _ borderBackgroundAbyBorderA ].
	resultGA _ 1.0 - ((1.0 - foregroundA) * (1.0  - backgroundColorA)).
	resultGA = 0.0
		ifTrue: [ resultG _ 0.0 ]
		ifFalse: [
			borderComponent _ (bGreen ifNil: [borderColor greenAtX: x y: y])/ 255.0.
			isGreenInside
				ifTrue: [
					fillComponent _ (fGreen ifNil: [fillColor greenAtX: x y: y]) / 255.0.
					foregroundAByG _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]
				ifFalse: [
					foregroundAByG _ borderBackgroundAbyBorderA * borderComponent ].
			resultG _ (foregroundAByG + (backgroundColorA * backgroundColorG * (1.0 - foregroundA))) / resultGA ].

	subpixX _ x+SubPixDelta.
	borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isBlueInside
		ifTrue: [
			fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			borderFillAbyBorderA _ (blueAlpha/127.0) * borderAlpha.
			borderFillUbyFillA _ (127 - blueAlpha/127.0) * fillAlpha.
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA ]
		ifFalse: [
			borderBackgroundAbyBorderA _ blueAlpha/127.0  * borderAlpha.
			foregroundA _ borderBackgroundAbyBorderA ].
	resultBA _ 1.0 - ((1.0 - foregroundA) * (1.0  - backgroundColorA)).
	resultBA = 0.0
		ifTrue: [ resultB _ 0.0 ]
		ifFalse: [
			borderComponent _ (bBlue ifNil: [borderColor blueAtX: subpixX y: y])/ 255.0.
			isBlueInside
				ifTrue: [
					fillComponent _ (fBlue ifNil: [fillColor blueAtX: subpixX y: y]) / 255.0.
					foregroundAByB _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]
				ifFalse: [
					foregroundAByB _ borderBackgroundAbyBorderA * borderComponent ].
			resultB _ (foregroundAByB + (backgroundColorA * backgroundColorB * (1.0 - foregroundA))) / resultBA ].

	resultColorA _ (resultGA * 255.0) rounded bitAnd: 16rFF.
	resultColorR _ (resultR * 255.0) rounded bitAnd: 16rFF.
	resultColorG _ (resultG * 255.0) rounded bitAnd: 16rFF.
	resultColorB _ (resultB * 255.0) rounded bitAnd: 16rFF.
	resultWord _ (((resultColorA bitShift: 24)
			bitOr: (resultColorR bitShift: 16))
			bitOr: (resultColorG bitShift: 8))
			bitOr: resultColorB.
	bits at: pixelIndex put: resultWord.! !

!Morphic3Canvas methodsFor: 'trajectories - private' stamp: 'jmv 11/4/2012 17:03'!
zMark1blendNoBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"The 'extended' alphaX parameters also define which color to blend over which other color.
	rAlphaX, gAlphaX and bAlphaX mean:
		alphaX inside [0 .. 127]	-> alpha  = (alphaX/127.0) for blending borderColor over background (outer edge of border)
		alphaX inside [128 .. 255] -> alpha = (alphaX-128.0 / 127.0) is the alpha for blending fillColor over borderColor (inner edge of border)"
	"
	Interesante!!
	http://www.stereopsis.com/doubleblend.html
	Muestra una aproximacion a la division, que NO usa una multiplicacion extra, sino una suma:
	a * b // 255	'teorica'
	a * b * 259 >> 16 'la que uso yo, aunque no divido por 255, sino por 127*255'
	(a + 1) * b >> 8	Es mas rapido. Pero es peor (al 255 solo manda 255*255, porque 255*254 y 254*255 van a 254.
	Tambien hay un par de trucos para calcular cosas con 2 componentes a la vez... aunque mmx es mejor de ultima.
	"
	| backgroundWord backgroundColorR backgroundColorG backgroundColorB resultColorR resultColorG resultColorB resultWord bits borderFillAbyBorderA borderFillUbyFillA borderBackgroundAbyBorderA foregroundA foregroundAByR resultR foregroundAByG resultG foregroundAByB resultB alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside borderAlpha borderComponent fillAlpha fillComponent subpixX |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ redAlpha _ 127-redAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ 127-greenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ 127-blueAlpha. isBlueInside _ false ]].
	
	
	form depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			greenAlpha > 63 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"	 "Color black pixelValueForDepth: 1"
				"Atencion: Falla si el form no es potencia de 2 (anda bien 512x512). Ademas hardcodea que somos little endian. MEJORAR (y poner en el otro blend)"
				"bits _ form bits.
				backgroundWord _ bits at: pixelIndex >> 5.
				backgroundWord _ backgroundWord bitOr: 1 << (31-(pixelIndex bitAnd: 31)).
				bits at: pixelIndex >> 5 put: backgroundWord."
				"Esto es super ineficiente pero parece andar bien. Mejorar..."
				form pixelValueAt: (pixelIndex \\ formWidth) @(pixelIndex // formWidth) put: 1
			].
			^ self].
	
	bits _ form bits.
	backgroundWord _ bits at: pixelIndex.
	backgroundColorR _ (backgroundWord bitShift: -16) bitAnd: 16rFF.
	backgroundColorG _ (backgroundWord bitShift: -8) bitAnd: 16rFF.
	backgroundColorB _ backgroundWord bitAnd: 16rFF.
	"alphaX inside [0 .. 127]	-> alpha  = (alphaX/127.0) for blending borderColor over background (outer edge of border)
	alphaX inside [128 .. 255] -> alpha = (alphaX-128.0 / 127.0) is the alpha for blending fillColor over borderColor (inner edge of border)"
	"
	renombrar
	fillBorderA -> borderFillU
	fillBorderU -> borderFillA
	Arreglar los comentarios...
	"
	subpixX _ x-SubPixDelta.
	borderAlpha _ bAlpha ifNil: [borderColor alphaAtX: subpixX y: y].
	borderComponent _ bRed ifNil: [borderColor redAtX: subpixX y: y].
	isRedInside
		ifTrue: [
			fillAlpha _ fAlpha ifNil: [fillColor alphaAtX: subpixX y: y].
			fillComponent _ fRed ifNil: [fillColor redAtX: subpixX y: y].
			borderFillAbyBorderA _ redAlpha * borderAlpha.						"0 .. 127*255"
			borderFillUbyFillA _ (127 - redAlpha) * fillAlpha.						"0 .. 127*255"
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA.			"0 .. 127*255"
			foregroundAByR _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]		"0 .. 127*255*255"
		ifFalse: [
			borderBackgroundAbyBorderA _ redAlpha * borderAlpha.			"0 .. 127 * 255"
			foregroundA _ borderBackgroundAbyBorderA.						"0 .. 127 * 255"
			foregroundAByR _ borderBackgroundAbyBorderA * borderComponent ].	"0 .. 127*255*255"
	resultR _ foregroundAByR  + (backgroundColorR * (32385 "=127*255" - foregroundA)).		"0 .. 127*255*255"
	borderAlpha _ bAlpha ifNil: [borderColor alphaAtX: x y: y].
	borderComponent _ bGreen ifNil: [borderColor greenAtX: x y: y].
	isGreenInside
		ifTrue: [
			fillAlpha _ fAlpha ifNil: [fillColor alphaAtX: x y: y].
			fillComponent _ fGreen ifNil: [fillColor greenAtX: x y: y].
			borderFillAbyBorderA _ greenAlpha * borderAlpha.					"0 .. 127*255"
			borderFillUbyFillA _ (127 - greenAlpha) * fillAlpha.					"0 .. 127*255"
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA.			"0 .. 127*255"
			foregroundAByG _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]		"0 .. 127*255*255"
		ifFalse: [
			borderBackgroundAbyBorderA _ greenAlpha  * borderAlpha.			"0 .. 127 * 255"
			foregroundA _ borderBackgroundAbyBorderA.						"0 .. 127 * 255"
			foregroundAByG _ borderBackgroundAbyBorderA * borderComponent ].	"0 .. 127*255*255"
	resultG _ foregroundAByG + (backgroundColorG * (32385 "=127*255" - foregroundA)).		"0 .. 127*255*255"
	subpixX _ x+SubPixDelta.
	borderAlpha _ bAlpha ifNil: [borderColor alphaAtX: subpixX y: y].
	borderComponent _ bBlue ifNil: [borderColor blueAtX: subpixX y: y].
	isBlueInside
		ifTrue: [
			fillAlpha _ fAlpha ifNil: [fillColor alphaAtX: subpixX y: y].
			fillComponent _ fBlue ifNil: [fillColor blueAtX: subpixX y: y].
			borderFillAbyBorderA _ blueAlpha * borderAlpha.					"0 .. 127*255"
			borderFillUbyFillA _ (127 - blueAlpha) * fillAlpha.						"0 .. 127*255"
			foregroundA _ borderFillAbyBorderA + borderFillUbyFillA.			"0 .. 127*255"
			foregroundAByB _ (borderFillAbyBorderA * borderComponent) + (borderFillUbyFillA * fillComponent) ]		"0 .. 127*255*255"
		ifFalse: [
			borderBackgroundAbyBorderA _ blueAlpha  * borderAlpha.			"0 .. 127 * 255"
			foregroundA _ borderBackgroundAbyBorderA.						"0 .. 127 * 255"
			foregroundAByB _ borderBackgroundAbyBorderA * borderComponent ].	"0 .. 127*255*255"
	resultB _ foregroundAByB + (backgroundColorB * (32385 "=127*255" - foregroundA)).		"0 .. 127*255*255"

	"Various alternatives for this calculation. Only one of these is used below:"
	"This is the exact float. However, it sends to slots 0 and 255 half the values it sends to any other slot:
	(See code snippet below to test)
		resultColorR _  (resultR / (1.0 * 127*255)) rounded."
	"These are integer-only versions of that float calculation:
		resultColorR _ (resultR + 16192) //  32385.
		resultColorR _ (resultR + (resultR bitShift: -7) + (resultR bitShift: -8) + 16192) bitShift: -15.
		resultColorR _ (resultR * 259) +  4145280 bitShift: -23."
	"These are much better. Not only they are really fast, but they also sends almost the same quantity of values to any slot:
	They are equivalent, you can choose between a mul or an add and a shift
		resultColorR _ (resultR * 260) bitShift: -23.
		resultColorR _ (resultR + (resultR bitShift: -6) bitShift: -15).
	To test, inspect:
				| b |
				b _ Bag new.
				0 to: 255*255*127 do: [ :i |
				b add: (i * 260 bitShift: -23) ].
				b"				
"	resultColorR _ ((resultR * 260) bitShift: -7) bitAnd: 16rFF0000.
	resultColorG _ ((resultG * 260) bitShift: -15) bitAnd: 16rFF00.
	resultColorB _ (resultB * 260) bitShift: -23."
	resultColorR _ ((resultR * 65) bitShift: -5) bitAnd: 16rFF0000.	"avoid LargeIntegers!!"
	resultColorG _ ((resultG * 65) bitShift: -13) bitAnd: 16rFF00.
	resultColorB _ (resultB * 65) bitShift: -21.
	resultWord _ (( "4278190080" " = (255 bitShift: 24)" 0	"I don't really care, and don't want largeIntegers"
			bitOr: resultColorR)
			bitOr: resultColorG)
			bitOr: resultColorB.
	bits at: pixelIndex put: resultWord.! !

!Morphic3Canvas class methodsFor: 'accessing' stamp: 'jmv 5/27/2010 16:55'!
aAWidth
	^AAWidth! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 19:00'!
experLinesAndShapes
	"
	Transcript clear. [self experLinesAndShapes] forkAt: 30
	"
	| canvas y1 yy2 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.
	canvas setupBorderWidth: 0.001 borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 20 y: 10;
		displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
		displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.
	canvas setupBorderWidth: AAWidth borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 220 y: 10;
		displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
		displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ AAWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		yy2 _ y2 >= 1 ifTrue: [ AAWidth * 0.5 ] ifFalse: [0].
		canvas setupBorderWidth: AAWidth borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 431 y: y1+yy2; displayCoordlineToX: 530 y: y1+yy2+dy;
			displayCoordlineToX: 530 y: y1+y2-yy2+dy; displayCoordlineToX: 431 y: y1+y2-yy2;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX:  y1+yy2 y: 431; displayCoordlineToX: y1+yy2+dy y: 530;
			displayCoordlineToX: y1+y2-yy2+dy y: 530; displayCoordlineToX: y1+y2-yy2 y: 431;
			closeTrajectory; finishTrajectory.
		"Solo fill. w casi=0"
		canvas setupBorderWidth: 0.001 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
			displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
			displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
			closeTrajectory; finishTrajectory.
		"Solo fill. w =0"
		canvas setupBorderWidth: 0.0 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
			displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
			closeTrajectory; finishTrajectory.
		"solo stroke, no fill"
		canvas setupBorderWidth: y2 borderColor: Color black fillColor: Color transparent 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: y2*0.5+731 y: y2*0.5 + y1+dy+dy+dy; displayCoordlineToX: 830 y: y2 * 0.5 + y1  + dy + dy+dy+dy;
			finishTrajectory.
		canvas
			displayCoordmoveToX: y2*0.5 + y1+dy+dy+dy y: y2*0.5+731; displayCoordlineToX: y2 * 0.5 + y1  + dy + dy+dy+dy y: 830;
			finishTrajectory.
		y1 _ y1 + y2 + AAWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 19:00'!
experLinesAndShapes2
	"
	Transcript clear. [self experLinesAndShapes2] forkAt: 30
	"
	| canvas y1 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.

	y1 _ 10.
	dy _ 3.
	y2 _ AAWidth*4*2.
	15 timesRepeat: [
		"Solo fill. w chico pero aun visible"
		canvas setupBorderWidth: 1.0 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 131 y: y1; displayCoordlineToX: 230 y: y1+dy;
			displayCoordlineToX: 230 y: y1+y2+dy; displayCoordlineToX: 131 y: y1+y2;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1 y: 131; displayCoordlineToX: y1+dy y: 230;
			displayCoordlineToX: y1+y2+dy y: 230;displayCoordlineToX: y1+y2 y: 131;
			closeTrajectory; finishTrajectory.
		"Solo fill. w chico pero aun visible"
		canvas setupBorderWidth: 0.8 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 231 y: y1+dy; displayCoordlineToX: 330 y: y1+dy+dy;
			displayCoordlineToX: 330 y: y1+y2+dy+dy; displayCoordlineToX: 231 y: y1+y2+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy y: 231; displayCoordlineToX: y1+dy+dy y: 330;
			displayCoordlineToX: y1+y2+dy+dy y: 330; displayCoordlineToX: y1+y2+dy y: 231;
			closeTrajectory; finishTrajectory.
		"Solo fill. w chico pero aun visible"
		canvas setupBorderWidth: 0.6 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 331 y: y1+dy+dy; displayCoordlineToX: 430 y: y1+dy+dy+dy;
			displayCoordlineToX: 430 y: y1+y2+dy+dy+dy; displayCoordlineToX: 331 y: y1+y2+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy y: 331; displayCoordlineToX: y1+dy+dy+dy y: 430;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 430; displayCoordlineToX: y1+y2+dy+dy y: 331;
			closeTrajectory; finishTrajectory.
		"Solo fill. w chico pero aun visible"
		canvas setupBorderWidth: 0.4 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 431 y: y1+dy+dy+dy; displayCoordlineToX: 530 y: y1+dy+dy+dy+dy;
			displayCoordlineToX: 530 y: y1+y2+dy+dy+dy+dy; displayCoordlineToX: 431 y: y1+y2+dy+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy+dy y: 431; displayCoordlineToX: y1+dy+dy+dy+dy y: 530;
			displayCoordlineToX: y1+y2+dy+dy+dy+dy y: 530; displayCoordlineToX: y1+y2+dy+dy+dy y: 431;
			closeTrajectory; finishTrajectory.
		"Solo fill. w chico pero aun visible"
		canvas setupBorderWidth: 0.2 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 531 y: y1+dy+dy+dy+dy; displayCoordlineToX: 630 y: y1+dy+dy+dy+dy+dy;
			displayCoordlineToX: 630 y: y1+y2+dy+dy+dy+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy+dy+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy+dy+dy y: 531; displayCoordlineToX: y1+dy+dy+dy+dy+dy y: 630;
			displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy y: 630; displayCoordlineToX: y1+y2+dy+dy+dy+dy y: 531;
			closeTrajectory; finishTrajectory.
		"Solo fill. w chico. Preferimos ya usarlo para definir mejor el shape (se transparentaria demasiado)"
		canvas setupBorderWidth: 0.0 borderColor: Color green fillColor: Color red 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 631 y: y1+dy+dy+dy+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy+dy+dy+dy;
			displayCoordlineToX: 730 y: y1+y2+dy+dy+dy+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy+dy+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy+dy+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy+dy+dy+dy y: 730;
			displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy y: 631;
			closeTrajectory; finishTrajectory.
		y1 _ y1 + y2 + AAWidth + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 19:00'!
experLinesAndShapes3
	"
	Transcript clear. [self experLinesAndShapes3] forkAt: 30
	"
	| canvas y1 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.
	canvas setupBorderWidth: 0.001 borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 20 y: 10;
		displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
		displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.
	canvas setupBorderWidth: AAWidth borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 220 y: 10;
		displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
		displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ AAWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		canvas setupBorderWidth: AAWidth*0.4 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 431 y: y1; displayCoordlineToX: 530 y: y1+dy;
			displayCoordlineToX: 530 y: y1+y2+dy; displayCoordlineToX: 431 y: y1+y2;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX:  y1 y: 431; displayCoordlineToX: y1+dy y: 530;
			displayCoordlineToX: y1+y2+dy y: 530; displayCoordlineToX: y1+y2 y: 431;
			closeTrajectory; finishTrajectory.
		"Solo fill. w casi=0"
		canvas setupBorderWidth: AAWidth*0.2 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
			displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
			displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
			closeTrajectory; finishTrajectory.
		"Solo fill. w =0"
		canvas setupBorderWidth: AAWidth*0.0 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
			displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
			closeTrajectory; finishTrajectory.
		y1 _ y1 + y2 + AAWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 18:59'!
experLinesAndShapes4
	"
	Transcript clear. [self experLinesAndShapes4] forkAt: 30
	"
	| canvas y1 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.
	canvas setupBorderWidth: 0.001 borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 20 y: 10;
		displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
		displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.
	canvas setupBorderWidth: AAWidth borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 220 y: 10;
		displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
		displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ AAWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		canvas setupBorderWidth: AAWidth*2 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 431 y: y1; displayCoordlineToX: 530 y: y1+dy;
			displayCoordlineToX: 530 y: y1+y2+dy; displayCoordlineToX: 431 y: y1+y2;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX:  y1 y: 431; displayCoordlineToX: y1+dy y: 530;
			displayCoordlineToX: y1+y2+dy y: 530; displayCoordlineToX: y1+y2 y: 431;
			closeTrajectory; finishTrajectory.
		"Solo fill. w casi=0"
		canvas setupBorderWidth: AAWidth*1.5 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
			displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
			displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
			closeTrajectory; finishTrajectory.
		"Solo fill. w =0"
		canvas setupBorderWidth: AAWidth borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
			displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
			closeTrajectory; finishTrajectory.
		y1 _ y1 + y2 + AAWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 3/3/2014 15:27'!
experLinesAndShapesx
	"
	Transcript clear. [self experLinesAndShapesx] forkAt: 30
	"
	| canvas y1 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.
	canvas setupBorderWidth: 0.001 borderColor: Color black fillColor: Color black strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 20 y: 10;
		displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
		displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ AAWidth * 2.
	15 timesRepeat: [
		y2 print.
		"Solo fill. w =0"
		canvas setupBorderWidth: 0.0 borderColor: Color black fillColor: Color black 
			strokeDashArray: nil strokeDashArrayOffset: nil.
		canvas
			displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
			displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
			closeTrajectory; finishTrajectory.
		canvas
			displayCoordmoveToX: y1+dy+dy y: 431; displayCoordlineToX: y1+dy+dy+dy y: 530;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 530; displayCoordlineToX: y1+y2+dy+dy y: 431;
			closeTrajectory; finishTrajectory.
		y1 _ y1 + y2 + AAWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 3/3/2014 16:26'!
experLinesAndShapesx2
	"
	self experLinesAndShapesx2
	"
	| canvas y1 dy y2 |
	canvas _ Morphic3Canvas onForm: Display.
	y1 _ 10.
	dy _ 3.
	y2 _ 0.1.
	y2 print.
	"Solo fill. w =0"
	canvas setupBorderWidth: 0.5 borderColor: Color black fillColor: Color black 
		strokeDashArray: nil strokeDashArrayOffset: nil.
	canvas
		displayCoordmoveToX: 10 y: y1+dy+dy; displayCoordlineToX: 200 y: y1+dy+dy+dy;
		displayCoordlineToX: 200 y: y1+y2+dy+dy+dy; displayCoordlineToX: 10 y: y1+y2+dy+dy;
		closeTrajectory; finishTrajectory.
	canvas
		displayCoordmoveToX: y1+dy+dy y: 10; displayCoordlineToX: y1+dy+dy+dy y: 200;
		displayCoordlineToX: y1+y2+dy+dy+dy y: 200; displayCoordlineToX: y1+y2+dy+dy y: 10;
		closeTrajectory; finishTrajectory.
	Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 18:59'!
experiment1
	"
	self experiment1
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
canvas _ Morphic3Canvas onForm: Display.
h _ 300.
h _ 600.
bottom _ 200.
centerX _ 400.
w0 _ 60.0.
hf _ 1.0.
y _ nil.
delta _ nil.
1 to: 400 do: [ :i |
	i print.
	prevDelta _ delta.
	delta _ h / (w0 * i + h).
	prevY _ y.
	y _ bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-6 // delta to: 6 // delta do: [ :ii |
			x1p _ centerX - (prevDelta * w0 * (ii-1)).
			x2p _ centerX - (prevDelta * w0 * ii).
			x1 _ centerX - (delta * w0 * (ii-1)).
			x2 _ centerX - (delta * w0 * ii).
			color _ i + ii \\ 2 = 0 ifTrue: [ Color white ] ifFalse: [ Color black ].
			color _ color alpha: (((prevY-y) min: (x1-x2)) "* 0.3" min: 1).
"			color _ color alpha: 0.5."
			canvas setupBorderWidth: 0.0 borderColor: color fillColor: color strokeDashArray: nil strokeDashArrayOffset: nil.
			canvas
				displayCoordmoveToX: x1 y: y;
				displayCoordlineToX: x2 y: y;
				displayCoordlineToX: x2p y: prevY;
				displayCoordlineToX: x1p y: prevY;
				closeTrajectory;
				finishTrajectory
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 19:00'!
experiment2
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	self experiment2
	Ver senders de #truchada
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
canvas _ Morphic3Canvas onForm: Display.
h _ 300.
h _ 490.
h _ 280.
bottom _ 200.
bottom _ 0.
centerX _ 400.
centerX _ 250.
w0 _ 120.0.
hf _ 1.0.
hf _ 1.73.
hf _ 1.71.
y _ nil.
delta _ nil.
	e _ 500@500.
	f _ Form extent: e depth: 32.
	f fillColor: Color transparent..
"	canvas _  Morphic3Canvas on: f.
	canvas useComponentAlphas."
1 to: "400" 150 do: [ :i |
	i print.
	prevDelta _ delta.
	delta _ h / (w0 * i + h).
	prevY _ y.
	y _ bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-3 // delta to: 3 // delta do: [ :ii |
			x1p _ centerX - (prevDelta * w0 * (ii-1)).
			x2p _ centerX - (prevDelta * w0 * ii).
			x1 _ centerX - (delta * w0 * (ii-1)).
			x2 _ centerX - (delta * w0 * ii).
			color _ i + ii \\ 2 = 0 ifTrue: [ Color black ] ifFalse: [ Color white ].
"			color _ color alpha: (((prevY-y) min: (x1-x2)) min: 1)."
"			color _ color alpha: 0.5."
			canvas setupBorderWidth: 0.0 borderColor: color fillColor: color strokeDashArray: nil strokeDashArrayOffset: nil.
			canvas
				displayCoordmoveToX: x1 y: y;
				displayCoordlineToX: x2 y: y;
				displayCoordlineToX: x2p y: prevY;
				displayCoordlineToX: x1p y: prevY;
				closeTrajectory;
				finishTrajectory
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
"
	canvas removeSpuriousTranslucencyFloatAndBlendOver: Color red.
	f writePNGfileNamed: 'pp17.png'
"! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 23:18'!
experiment3
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	self experiment3
	Ver senders de #truchada
	"
	| c w r |
	c _ Morphic3Canvas onForm: Display.
	w _ 10.
	r _ 15.
	100 timesRepeat: [
		c ellipseCenterX: 300 y: 300 rx: r ry: r borderWidth: w borderColor: Color black fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
		r _ r + w.
		w _ w * 0.8.
		r _ r + w.

	Display forceToScreen.
	].
Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'experiments' stamp: 'jmv 11/8/2012 23:18'!
experiment4
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	self experiment4
	Ver senders de #truchada
	"
	| c w r |
	c _ Morphic3Canvas onForm: Display.
	w _ 10.
	r _ 15.
	1 to: 100 do: [ :i |
		r _ r + w.
		w _ 20.0 / i.
		r _ r + w.
		c ellipseCenterX: 300 y: 300 rx: r ry: r borderWidth: w borderColor: Color black fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
	Display forceToScreen.
	].
Display forceToScreen.
! !

!Morphic3Canvas class methodsFor: 'class initialization' stamp: 'jmv 3/25/2013 21:04'!
initialize
	"
	Morphic3Canvas initialize
	0.33: r - b
	0.1: b - r
	"
	AAWidth _ 1.3.		"Perhaps smaller for very narrow lines?"
	AAWidth _ 1.1.		"Crisper. Very slightly pixellated. Great for text!!"
	SubPixDelta _ 0.25.		"Values closer to 0.33 produce a bit of coloring, used to be 0.28"
	MatrixTransform2x3 doIntegerTransformations: false.! !

!Morphic3Canvas class methodsFor: 'text experiments' stamp: 'jmv 7/2/2014 09:10'!
textex1
"Jugando con Paragraph"
"
self textex1
De cualquier manera, estos experimentos pavos deben desaparecer.
Ejemplos como e de displayString:from:to:at:font: son mas realistas y mejores
"
	| t para frame |
	t _ Text
		initialFont: (AbstractFont familyName: 'DejaVu' pointSize: 9)
		stringOrText: 'Acá vamos a escribir un poquito de texto. Esto es una prueba del renderer de texto de Morphic 3. La idea es poder reemplazar al actual en todos los casos en que sea posible. Quizás para algún pointsize especial, como 9, sigamos usando StrikeFont si se alinean los planetas. Ñandú, agüita, çå½´®¥ïøßƒ©Ý²Þ¬­ç¦µ'.
	frame _ 10@10 extent: 320@400.
	para _ TextComposition new.
	para
		setModel: (TextModel withText: t);
		extentForComposing: (320 @ 9999);
		composeAll.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	frame _ 334@10 extent: 320@400.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	Display getCanvas
		textComposition: para 
		bounds: (324@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	(Morphic3Canvas onForm: Display)
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange! !

!Morphic3Canvas class methodsFor: 'text experiments' stamp: 'jmv 3/30/2014 17:14'!
textex2
"Jugando con Paragraph"
"
self textex2
"
	| t para frame |
	
	t _ Text
		initialFont: (AbstractFont familyName: 'DejaVu' pointSize: 22)
		stringOrText: (String streamContents: [ :strm | (32 to: 255) do: [ :i | strm nextPut: (Character value: i)]]).
	frame _ 10@10 extent: 320@400.
	para _ TextComposition new.
	para
		setModel: (TextModel withText: t);
		extentForComposing: (320 @ 9999);
		composeAll.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	frame _ 334@10 extent: 320@400.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	Display getCanvas
		textComposition: para 
		bounds: (324@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	(Morphic3Canvas onForm: Display)
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.! !

!Morphic3Canvas class methodsFor: 'text experiments' stamp: 'jmv 3/2/2014 22:58'!
textex3
"Jugando con Paragraph"
"
self textex3
"
	| t para frame |
	t _ Text
		initialFont: (AbstractFont familyName: 'DejaVu' pointSize: 9)
		stringOrText: 'Acá vamos a escribir un poquito de texto. Esto es una prueba del renderer de texto de Morphic 3. La idea es poder reemplazar al actual en todos los casos en que sea posible. Quizás para algún pointsize especial, como 9, sigamos usando StrikeFont si se alinean los planetas. Ñandú, agüita, çå½´®¥ïøßƒ©Ý²Þ¬­ç¦µ'.
	frame _ 10@10 extent: 320@400.
	para _ TextComposition new.
	para
		setModel: (TextModel withText: t);
		extentForComposing: (320 @ 9999);
		composeAll.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	frame _ 334@10 extent: 320@400.
	Display fill: (frame insetBy: -2) fillColor: Color black;
			fill: frame fillColor: Color white.
	Display getCanvas
		textComposition: para 
		bounds: (324@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	(Morphic3Canvas onForm: Display)
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange! !

!BoundsFinderCanvas methodsFor: 'trajectories - aux' stamp: 'jmv 4/28/2011 17:22'!
addCurrentToTrajectory

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| dilatedHalfWidth t b l r erodedHalfWidth constant1 |
	dilatedHalfWidth _ borderWidth + AAWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
	erodedHalfWidth _ borderWidth - AAWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
lastX ifNotNil: [
	constant1 _ 127.0 / AAWidth.

	"ok agregar estos bounds? Probar con trayectorias que se arriman a los extremos del espacio"
	t _ (lastY -dilatedHalfWidth) truncated +1.
	b _ (lastY +dilatedHalfWidth) truncated.
	l _ (lastX -dilatedHalfWidth -SubPixDelta) truncated +1.
	r _ (lastX +dilatedHalfWidth +SubPixDelta) truncated.

	l < boundsLeft ifTrue: [ boundsLeft _ l ].
	r > boundsRight ifTrue: [ boundsRight _ r ].
	t < boundsTop ifTrue: [ boundsTop _ t ].
	b > boundsBottom ifTrue: [ boundsBottom _ b ].
].
butLastX _ lastX.
butLastY _ lastY.
lastX _ currentX.
lastY _ currentY! !

!BoundsFinderCanvas methodsFor: 'trajectories - aux' stamp: 'jmv 4/28/2011 17:42'!
finishTrajectory
"all the points are actually painted, to use Dan's convention, add 1 to corner"
"	(boundsLeft@boundsTop corner: boundsRight@boundsBottom) print"! !

!BoundsFinderCanvas methodsFor: 'trajectories - aux' stamp: 'jmv 4/28/2011 17:45'!
r
"all the points are actually painted, to use Dan's convention, add 1 to corner"
	^(boundsLeft@boundsTop corner: boundsRight@boundsBottom + 1)! !

!BoundsFinderCanvas methodsFor: 'trajectories - aux' stamp: 'jmv 4/29/2011 21:47'!
setupBorderWidth: bw borderColor: bc fillColor: fc strokeDashArray: sda strokeDashArrayOffset: sdao

	| wantsBorder |
	computeFill _ fc notNil and: [ (fc isKindOf: Color) not or: [ fc isTransparent not ]].
	wantsBorder _ bc notNil and: [ (bc isKindOf: Color) not or: [ bc isTransparent not ]] and: [ bw > 0.0 ].
	borderWidth _ wantsBorder ifTrue: [ bw ] ifFalse: [ 0.0 ].


	(computeFill and: [ borderWidth < (AAWidth*0.33) ])
		ifTrue: [
			borderWidth _ 0.0]		"Bordes tan delgados que al transparentarlos se perderian, prefiero que no le roben lugar al fill"
		ifFalse: [
			fc ~= bc
				ifTrue: [		"Bordes que quiero mostrar. No son tan delgados, o no hay fill. Asegurar que tengan el ancho necesario para muestrearse"
					borderWidth _ borderWidth max: AAWidth ]].

	hopsFactor _ 1.

self flag: #revisarM3.		"poner infinito o nil???"
	boundsTop _ 10000000000.	"	drawable bottom. Will later be refined."
	boundsBottom _ -10000000000.				"drawable top. Will later be refined."
	boundsLeft _ 10000000000.		"drawable right. Will later be refined."
	boundsRight _ -10000000000.					"drawable left. Will later be refined."
	
	! !

!Morphic3Canvas2 methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:28'!
blendAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
"En esta subclase calculamos component alpha, e incluso component alpha suponiendo colores opacos"

self zMark1blendIntWithBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
self zMark1blendIntWithBackgroundAndOpaqueColorsAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.


"
self blendFloatWithBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
self blendFloatWithBackgroundAndOpaqueColorsAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
"! !

!Morphic3Canvas2 methodsFor: 'private' stamp: 'jmv 2/15/2013 17:48'!
blendOver: aColor

	| gAlpha componentAlphasWord word red green blue rAlpha blueAlpha backR backG backB pixelIndex |
	backR _ aColor red.
	backG _ aColor green.
	backB _ aColor blue.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			componentAlphasWord _ componentAlphas at: pixelIndex.
			rAlpha _ ((componentAlphasWord bitAnd: 16rFF0000) >> 16) / 255.0.
			gAlpha _ ((componentAlphasWord bitAnd: 16rFF00) >> 8) / 255.0.
			blueAlpha _ (componentAlphasWord bitAnd: 16rFF) / 255.0.

			word _ form bits at: pixelIndex.
			red _ ((word bitAnd: 16rFF0000) >> 16) / 255.0.
			green _ ((word bitAnd: 16rFF00) >> 8) / 255.0.
			blue _ (word bitAnd: 16rFF) / 255.0.
					
			red _ rAlpha * red + (1.0-rAlpha * backR).
			green _ gAlpha * green + (1.0-gAlpha * backG).
			blue _ blueAlpha * blue + (1.0-blueAlpha * backB).

			red _ (red * 255) rounded bitAnd: 16rFF.
			green _ (green * 255) rounded bitAnd: 16rFF.
			blue _ (blue * 255) rounded bitAnd: 16rFF.

			word _ (((255 bitShift: 24) bitOr: (red bitShift: 16)) bitOr: (green bitShift: 8)) bitOr: blue.
					
			form bits at: pixelIndex put: word ]]! !

!Morphic3Canvas2 methodsFor: 'private' stamp: 'jmv 2/15/2013 17:48'!
removeSpuriousTranslucencyFloatAndBlendOver: aColor
"Norma de prefijos para variables
f1XXX: 0.0..1.0 (float)
bXXX: 0..255 (entero o float)
b127XXX: 0..127 (entero o float)
bFFXXX: 0..255
bFFbFFXXX: 0..255*255
b16XXX: 0..65535
bFFFFXXXX0..65535
y asi siguiendo...
"
	| t b l r isTransparentClose word 
	f1rAlpha f1aaRAlpha f1componentRAlpha
	f1gAlpha f1aaGAlpha f1componentGAlpha
	b255bAlpha f1aaBAlpha f1componentBAlpha
	f1red f1green f1blue componentAlphasWord componentAlphasIfOnlyOpaqueColorsWord 
	f1backR f1backG f1backB pixelIndex pixelIndex2 bFFred bFFgreen bFFblue |
	f1backR _ aColor red.
	f1backG _ aColor green.
	f1backB _ aColor blue.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			componentAlphasWord _ componentAlphas at: pixelIndex.
			"Si el resultado es opaco, no hay nada que hacer (nuestro form ya tiene el alpha puesto a 255)"
			componentAlphasWord = 16rFFFFFF ifFalse: [
				f1rAlpha _ ((componentAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
				f1gAlpha _ ((componentAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
				b255bAlpha _ (componentAlphasWord bitAnd: 16rFF) / 255.0.

				isTransparentClose _ false.
				"ver en #addCurrentToTrajectory. Deberia alcanzar con la mitad. Pero incluir deltaForSubPixP"
				t _ (displayY-AAWidth) truncated.
				t < 0 ifTrue: [ t _ 0 ].
				b _ (displayY+AAWidth) truncated+1.
				b > (formHeight-1) ifTrue: [ b _ formHeight-1 ].
				l _ (displayX-AAWidth) truncated.
				l < 0 ifTrue: [ l _ 0 ].
				r _ (displayX+AAWidth) truncated+1.
				r > (formWidth-1) ifTrue: [ r _ formWidth-1 ].
"				self
					overTop: t bottom: b left: l right: r
					do: [ :displayX2 :displayY2 :pixelIndex2 |"
				t to: b do: [ :displayY2 |
					pixelIndex2 _ displayY2 * formWidth +  l.
					l to: r do: [ :displayX2 |
						pixelIndex2 _ pixelIndex2 + 1.
						isTransparentClose _ isTransparentClose or: 	"chequeo solo el verde creo que esta bien para esto"
							[((componentAlphasIfOnlyOpaqueColors at: pixelIndex2) bitAnd: 16rFF00) = 0]]].
				isTransparentClose
					ifFalse: [
						componentAlphasIfOnlyOpaqueColorsWord _ componentAlphasIfOnlyOpaqueColors at: pixelIndex.
						componentAlphasIfOnlyOpaqueColorsWord = componentAlphasWord
							ifTrue: [
								f1rAlpha _ 255.
								f1gAlpha _ 255.
								b255bAlpha _ 255]
							ifFalse: [
								f1rAlpha > 0 ifTrue: [
									f1aaRAlpha _ ((componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF0000) >> 16) / 255.0.
									f1componentRAlpha _ f1rAlpha / f1aaRAlpha.
									f1rAlpha _ f1componentRAlpha].
								f1gAlpha > 0 ifTrue: [
									f1aaGAlpha _ ((componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF00) >> 8) / 255.0.
									f1componentGAlpha _ f1gAlpha / f1aaGAlpha.
									f1gAlpha _ f1componentGAlpha].
								b255bAlpha > 0 ifTrue: [
									f1aaBAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF) / 255.0.
									f1componentBAlpha _ b255bAlpha / f1aaBAlpha.
									b255bAlpha _ f1componentBAlpha]]].

				"Optimization. If we are away from transparent, and color was opaque, do nothing (i.e. don't blend)"
				(isTransparentClose or: [ componentAlphasIfOnlyOpaqueColorsWord ~= componentAlphasWord ]) ifTrue: [
					word _ form bits at: pixelIndex.
					f1red _ ((word bitAnd: 16rFF0000) >> 16) / 255.0.
					f1green _ ((word bitAnd: 16rFF00) >> 8) / 255.0.
					f1blue _ (word bitAnd: 16rFF) / 255.0.

					f1red _ f1rAlpha * f1red + (1.0-f1rAlpha * f1backR).
					f1green _ f1gAlpha * f1green + (1.0-f1gAlpha * f1backG).
					f1blue _ b255bAlpha * f1blue + (1.0-b255bAlpha * f1backB).

					bFFred _ (f1red * 255) rounded bitAnd: 16rFF.
					bFFgreen _ (f1green * 255) rounded bitAnd: 16rFF.
					bFFblue _ (f1blue * 255) rounded bitAnd: 16rFF.

					word _ (255 bitShift: 24) bitOr: (bFFred bitShift: 16) + (bFFgreen bitShift: 8) + bFFblue.

					form bits at: pixelIndex put: word ]]]]! !

!Morphic3Canvas2 methodsFor: 'private' stamp: 'jmv 2/15/2013 17:49'!
removeSpuriousTranslucencyIntAndBlendOver: aColor
"Norma de prefijos para variables
f1XXX: 0.0..1.0 (float)
bXXX: 0..255 (entero o float)
b127XXX: 0..127 (entero o float)
bFFXXX: 0..255
bFFbFFXXX: 0..255*255
b16XXX: 0..65535
bFFFFXXXX0..65535
y asi siguiendo...
"
	| t b l r isTransparentClose word 
	b255rAlpha b255aaRAlpha b255componentRAlpha
	b255gAlpha b255aaGAlpha b255componentGAlpha
	b255bAlpha b255aaBAlpha b255componentBAlpha
	 componentAlphasWord componentAlphasIfOnlyOpaqueColorsWord 
	bFFbackR bFFbackG bFFbackB pixelIndex pixelIndex2 bFFred bFFgreen bFFblue bFFbFFred bFFbFFgreen bFFbFFblue dilatedWidth |
	self flag: #revisarM3.	"NO tengo derecho a usar borderWidth. Puede haber 7 mil!!!!!!"
	dilatedWidth _ borderWidth + AAWidth.
	bFFbackR _ (aColor red * 255.0) truncated.
	bFFbackG _ (aColor green * 255.0) truncated.
	bFFbackB _ (aColor blue * 255.0) truncated.

"	self
		overTop: 0 bottom: formHeight-1 left: 0 right: formWidth-1
		do: [ :displayX :displayY :pixelIndex |"
	0 to: formHeight-1 do: [ :displayY |
		pixelIndex _ displayY * formWidth.
		0 to: formWidth-1 do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			componentAlphasWord _ componentAlphas at: pixelIndex.
			"Si el resultado es opaco, no hay nada que hacer (nuestro form ya tiene el alpha puesto a 255)"
			componentAlphasWord = 16rFFFFFF ifFalse: [
				b255rAlpha _ (componentAlphasWord bitShift: -16) bitAnd: 16rFF.
				b255gAlpha _ (componentAlphasWord bitShift: -8) bitAnd: 16rFF.
				b255bAlpha _ componentAlphasWord bitAnd: 16rFF.

				isTransparentClose _ false.
				t _ (displayY -dilatedWidth) truncated +1.
				t < 0 ifTrue: [ t _ 0 ].
				b _ (displayY +dilatedWidth) truncated.
				b > (formHeight - 1) ifTrue: [ b _ formHeight - 1 ].
				l _ (displayX -dilatedWidth -1) truncated +1.
				l < 0 ifTrue: [ l _ 0 ].
				r _ (displayX +dilatedWidth +1) truncated.
				r > (formWidth - 1) ifTrue: [ r _ formWidth - 1 ].
"				self
					overTop: t bottom: b left: l right: r
					do: [ :displayX2 :displayY2 :pixelIndex2 |"
				t to: b do: [ :displayY2 |
					pixelIndex2 _ displayY2 * formWidth +  l.
					l to: r do: [ :displayX2 |
						pixelIndex2 _ pixelIndex2 + 1.
						isTransparentClose _ isTransparentClose or: 	"chequeo solo el verde creo que esta bien para esto"
							[((componentAlphasIfOnlyOpaqueColors at: pixelIndex2) bitAnd: 16rFF00) = 0]]].
				isTransparentClose
					ifFalse: [
						componentAlphasIfOnlyOpaqueColorsWord _ componentAlphasIfOnlyOpaqueColors at: pixelIndex.
						componentAlphasIfOnlyOpaqueColorsWord = componentAlphasWord
							ifTrue: [
								b255rAlpha _ 255.
								b255gAlpha _ 255.
								b255bAlpha _ 255]
							ifFalse: [
								b255rAlpha > 0 ifTrue: [
									b255aaRAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitShift: -16) bitAnd: 16rFF.
									b255componentRAlpha _ 255.0 * b255rAlpha / b255aaRAlpha.
									b255rAlpha _ b255componentRAlpha truncated].
								b255gAlpha > 0 ifTrue: [
									b255aaGAlpha _ (componentAlphasIfOnlyOpaqueColorsWord bitShift: -8) bitAnd: 16rFF.
									b255componentGAlpha _ 255.0 * b255gAlpha / b255aaGAlpha.
									b255gAlpha _ b255componentGAlpha truncated].
								b255bAlpha > 0 ifTrue: [
									b255aaBAlpha _ componentAlphasIfOnlyOpaqueColorsWord bitAnd: 16rFF.
									b255componentBAlpha _ 255.0 * b255bAlpha / b255aaBAlpha.
									b255bAlpha _ b255componentBAlpha truncated]]].

				"Optimization. If we are away from transparent, and color was opaque, do nothing (i.e. don't blend)"
				(isTransparentClose or: [ componentAlphasIfOnlyOpaqueColorsWord ~= componentAlphasWord ]) ifTrue: [
					word _ form bits at: pixelIndex.
					bFFred _ (word bitShift: -16) bitAnd: 16rFF.
					bFFgreen _ (word bitShift: -8) bitAnd: 16rFF.
					bFFblue _ word bitAnd: 16rFF.

					bFFbFFred _ b255rAlpha * bFFred + (255-b255rAlpha * bFFbackR).
					bFFbFFgreen _ b255gAlpha * bFFgreen + (255-b255gAlpha * bFFbackG).
					bFFbFFblue _ b255bAlpha * bFFblue + (255-b255bAlpha * bFFbackB).

					"hacer la aritmetica correcta, *26 y el shift..."
					bFFred _ (bFFbFFred // 255) rounded bitAnd: 16rFF.
					bFFgreen _ (bFFbFFgreen // 255) rounded bitAnd: 16rFF.
					bFFblue _ (bFFbFFblue // 255) rounded bitAnd: 16rFF.

					word _ (255 bitShift: 24) bitOr: (bFFred bitShift: 16) + (bFFgreen bitShift: 8) + bFFblue.

					form bits at: pixelIndex put: word ]]]]! !

!Morphic3Canvas2 methodsFor: 'private' stamp: 'jmv 5/19/2010 17:40'!
setForm: aForm
	"
	World extent: World extent
	"
	| s |
	super setForm: aForm.
	s _ aForm bits size.
	
	"Ojo, no los limpiamos nunca!!!!!!!!"
	(componentAlphas isNil or: [ componentAlphas size ~= s ])
		ifTrue: [
			componentAlphas _ WordArray new: s.
			componentAlphasIfOnlyOpaqueColors _ WordArray new: s.]
		ifFalse: [
			"Not really needed, actually..."
			self flag: #revisarM3.
			"Perhaps this should be done in an ifCurtailed: block in case of Morphic drawing failure..."
			auxDataBitBlt destRect: form boundingBox.
			auxDataBitBlt destForm bits: componentAlphas.
			auxDataBitBlt copyBits.
			auxDataBitBlt destForm bits: componentAlphasIfOnlyOpaqueColors.
			auxDataBitBlt copyBits ]! !

!Morphic3Canvas2 methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:27'!
zMark1blendFloatWithBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Diferencia con la implementacion en super:
	tenemos alpha channel para cada componente de color, en componentAlphas"
	| backgroundWord f1backgroundColorR f1backgroundColorG f1backgroundColorB resultColorR resultColorG resultColorB resultWord bits f1borderFillAbyBorderA f1borderFillUbyFillA f1borderBackgroundAbyBorderA f1foregroundA f1resultRA f1resultGA f1resultBA f1foregroundAByR f1resultR f1foregroundAByG f1resultG f1foregroundAByB f1resultB alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside f1fillComponent f1borderComponent f1borderAlpha f1fillAlpha backgroundAlphasWord f1backgroundColorRA f1backgroundColorGA f1backgroundColorBA resultColorRA resultColorGA resultColorBA subpixX |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ redAlpha _ 127-redAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ 127-greenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ 127-blueAlpha. isBlueInside _ false ]].

	backgroundAlphasWord _ componentAlphas at: pixelIndex.
	f1backgroundColorRA _ ((backgroundAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	f1backgroundColorGA _ ((backgroundAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	f1backgroundColorBA _ (backgroundAlphasWord bitAnd: 16rFF) / 255.0.
	bits _ form bits.
	backgroundWord _ bits at: pixelIndex.
	f1backgroundColorR _ ((backgroundWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	f1backgroundColorG _ ((backgroundWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	f1backgroundColorB _ (backgroundWord bitAnd: 16rFF) / 255.0.

	subpixX _ x-SubPixDelta.
	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isRedInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			f1borderFillAbyBorderA _ (redAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- redAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ redAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultRA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorRA)).
	f1resultRA = 0.0
		ifTrue: [ f1resultR _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bRed ifNil: [borderColor redAtX: subpixX y: y]) / 255.0.
			isRedInside
				ifTrue: [
					f1fillComponent _ (fRed ifNil: [fillColor redAtX: subpixX y: y]) / 255.0.
					f1foregroundAByR _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByR _ f1borderBackgroundAbyBorderA * f1borderComponent ].
			f1resultR _( f1foregroundAByR  + (f1backgroundColorRA * f1backgroundColorR * (1.0 - f1foregroundA))) / f1resultRA ].

	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: x y: y]) / 255.0.
	isGreenInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: x y: y]) / 255.0.
			f1borderFillAbyBorderA _ (greenAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- greenAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ greenAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultGA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorGA)).
	f1resultGA = 0.0
		ifTrue: [ f1resultG _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bGreen ifNil: [borderColor greenAtX: x y: y])/ 255.0.
			isGreenInside
				ifTrue: [
					f1fillComponent _ (fGreen ifNil: [fillColor greenAtX: x y: y]) / 255.0.
					f1foregroundAByG _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByG _ f1borderBackgroundAbyBorderA * f1borderComponent ].
		f1resultG _ (f1foregroundAByG + (f1backgroundColorGA * f1backgroundColorG * (1.0 - f1foregroundA))) / f1resultGA ].

	subpixX _ x+SubPixDelta.
	f1borderAlpha _ (bAlpha ifNil: [borderColor alphaAtX: subpixX y: y]) / 255.0.
	isBlueInside
		ifTrue: [
			f1fillAlpha _ (fAlpha ifNil: [fillColor alphaAtX: subpixX y: y]) / 255.0.
			f1borderFillAbyBorderA _ (blueAlpha/127.0) * f1borderAlpha.
			f1borderFillUbyFillA _ (127- blueAlpha/127.0) * f1fillAlpha.
			f1foregroundA _ f1borderFillAbyBorderA + f1borderFillUbyFillA ]
		ifFalse: [
			f1borderBackgroundAbyBorderA _ blueAlpha/127.0  * f1borderAlpha.
			f1foregroundA _ f1borderBackgroundAbyBorderA ].
	f1resultBA _ 1.0 - ((1.0 - f1foregroundA) * (1.0  - f1backgroundColorBA)).
	f1resultBA = 0.0
		ifTrue: [ f1resultB _ 0.0 ]
		ifFalse: [
			f1borderComponent _ (bBlue ifNil: [borderColor blueAtX: subpixX y: y])/ 255.0.
			isBlueInside
				ifTrue: [
					f1fillComponent _ (fBlue ifNil: [fillColor blueAtX: subpixX y: y]) / 255.0.
					f1foregroundAByB _ (f1borderFillAbyBorderA * f1borderComponent) + (f1borderFillUbyFillA * f1fillComponent) ]
				ifFalse: [
					f1foregroundAByB _ f1borderBackgroundAbyBorderA * f1borderComponent ].
			f1resultB _ (f1foregroundAByB + (f1backgroundColorBA * f1backgroundColorB * (1.0 - f1foregroundA))) / f1resultBA ].

	resultColorR _ (f1resultR *255.0) rounded bitAnd: 16rFF.
	resultColorG _ (f1resultG *255.0) rounded bitAnd: 16rFF.
	resultColorB _ (f1resultB *255.0) rounded bitAnd: 16rFF.
	resultWord _ (255 bitShift: 24) bitOr: (resultColorR bitShift: 16) + (resultColorG bitShift: 8) +resultColorB.
	bits at: pixelIndex put: resultWord.

	resultColorRA _ (f1resultRA *255.0) rounded bitAnd: 16rFF.
	resultColorGA _ (f1resultGA *255.0) rounded bitAnd: 16rFF.
	resultColorBA _ (f1resultBA *255.0) rounded bitAnd: 16rFF.
	resultWord _ (resultColorRA bitShift: 16) +(resultColorGA bitShift: 8) + resultColorBA.
	componentAlphas at: pixelIndex put: resultWord.! !

!Morphic3Canvas2 methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:27'!
zMark1blendFloatWithBackgroundAndOpaqueColorsAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Este metod NO calcula colores, sino unicamente compoent alphas, suponiendo colores opacos para pintar"
	| resultWord alphaWord redAlpha greenAlpha blueAlpha isRedInside isGreenInside isBlueInside backgroundAlphasWord backgroundColorRA backgroundColorGA backgroundColorBA resultRA resultGA resultBA resultColorRA resultColorGA resultColorBA |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	redAlpha _ (alphaWord bitShift: -16) bitAnd: 16rFF.
	greenAlpha _ (alphaWord bitShift: -8) bitAnd: 16rFF.
	blueAlpha _ alphaWord bitAnd: 16rFF.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ redAlpha _ 127-redAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ greenAlpha _ 127-greenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ blueAlpha _ 127-blueAlpha. isBlueInside _ false ]].

	backgroundAlphasWord _ componentAlphasIfOnlyOpaqueColors at: pixelIndex.
	backgroundColorRA _ ((backgroundAlphasWord bitShift: -16) bitAnd: 16rFF) / 255.0.
	backgroundColorGA _ ((backgroundAlphasWord bitShift: -8) bitAnd: 16rFF) / 255.0.
	backgroundColorBA _ (backgroundAlphasWord bitAnd: 16rFF) / 255.0.

	isRedInside
		ifTrue: [ resultRA _ 1.0 ]
		ifFalse: [ resultRA _ 1.0 - ((1.0 - (redAlpha/127.0)) * (1.0  - backgroundColorRA)) ].

	isGreenInside
		ifTrue: [ resultGA _ 1.0 ]
		ifFalse: [ resultGA _ 1.0 - ((1.0 - (greenAlpha/127.0)) * (1.0  - backgroundColorGA)) ].

	isBlueInside
		ifTrue: [ resultBA _ 1.0 ]
		ifFalse: [ resultBA _ 1.0 - ((1.0 - (blueAlpha/127.0)) * (1.0  - backgroundColorBA)) ].

	resultColorRA _ (resultRA * 255.0) rounded bitAnd: 16rFF.
	resultColorGA _ (resultGA * 255.0) rounded bitAnd: 16rFF.
	resultColorBA _ (resultBA * 255.0) rounded bitAnd: 16rFF.
	resultWord _ ((resultColorRA bitShift: 16)
			bitOr: (resultColorGA bitShift: 8))
			bitOr: resultColorBA.
	componentAlphasIfOnlyOpaqueColors at: pixelIndex put: resultWord.! !

!Morphic3Canvas2 methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:27'!
zMark1blendIntWithBackgroundAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Diferencia con la implementacion en super:
	tenemos alpha channel para cada componente de color, en componentAlphas"
	| backgroundWord bFFbackgroundColorR bFFbackgroundColorG bFFbackgroundColorB resultWord bits b7FbFFborderFillAbyBorderA b7FbFFborderFillUbyFillA b7FbFFborderBackgroundAbyBorderA b7FbFFforegroundA b7FbFFbFFresultRA b7FbFFbFFresultGA b7FbFFbFFresultBA b7FbFFbFFforegroundAByC bFFresultR bFFresultG bFFresultB alphaWord b7FredAlpha b7FgreenAlpha b7FblueAlpha isRedInside isGreenInside isBlueInside bFFfillComponent bFFborderComponent bFFborderAlpha bFFfillAlpha backgroundAlphasWord bFFbackgroundColorRA bFFbackgroundColorGA bFFbackgroundColorBA resultColorRA resultColorGA resultColorBA subpixX |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	b7FredAlpha _ (alphaWord bitShift: -16) bitAnd: 16r7F.
	b7FgreenAlpha _ (alphaWord bitShift: -8) bitAnd: 16r7F.
	b7FblueAlpha _ alphaWord bitAnd: 16r7F.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ b7FredAlpha _ 127-b7FredAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ b7FgreenAlpha _ 127-b7FgreenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ b7FblueAlpha _ 127-b7FblueAlpha. isBlueInside _ false ]].

	backgroundAlphasWord _ componentAlphas at: pixelIndex.
	bFFbackgroundColorRA _ (backgroundAlphasWord bitShift: -16) bitAnd: 16rFF.
	bFFbackgroundColorGA _ (backgroundAlphasWord bitShift: -8) bitAnd: 16rFF.
	bFFbackgroundColorBA _ backgroundAlphasWord bitAnd: 16rFF.
	bits _ form bits.
	backgroundWord _ bits at: pixelIndex.
	bFFbackgroundColorR _ (backgroundWord bitShift: -16) bitAnd: 16rFF.
	bFFbackgroundColorG _ (backgroundWord bitShift: -8) bitAnd: 16rFF.
	bFFbackgroundColorB _ backgroundWord bitAnd: 16rFF.

	subpixX _ x-SubPixDelta.
	bFFborderAlpha _ bAlpha ifNil: [borderColor alphaAtX: subpixX y: y].
	isRedInside
		ifTrue: [
			bFFfillAlpha _ fAlpha ifNil: [fillColor alphaAtX: subpixX y: y].
			b7FbFFborderFillAbyBorderA _ b7FredAlpha * bFFborderAlpha.
			b7FbFFborderFillUbyFillA _ (127- b7FredAlpha) * bFFfillAlpha.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FredAlpha  * bFFborderAlpha.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultRA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorRA)).
	b7FbFFbFFresultRA = 0
		ifTrue: [ bFFresultR _ 0 ]
		ifFalse: [
			bFFborderComponent _ bRed ifNil: [borderColor redAtX: subpixX y: y].
			isRedInside
				ifTrue: [
					bFFfillComponent _ fRed ifNil: [fillColor redAtX: subpixX y: y].
					b7FbFFbFFforegroundAByC _ (b7FbFFborderFillAbyBorderA * bFFborderComponent) + (b7FbFFborderFillUbyFillA * bFFfillComponent) ]
				ifFalse: [
					b7FbFFbFFforegroundAByC _ b7FbFFborderBackgroundAbyBorderA * bFFborderComponent ].
			"Atencion. Esta cuenta la estoy haciendo en float"
			bFFresultR _ ((b7FbFFbFFforegroundAByC * 255.0) + (bFFbackgroundColorRA * bFFbackgroundColorR * ( 32385 - b7FbFFforegroundA))) / b7FbFFbFFresultRA.
			bFFresultR _ bFFresultR truncated bitAnd: 16rFF ].

	bFFborderAlpha _ bAlpha ifNil: [borderColor alphaAtX: x y: y].
	isGreenInside
		ifTrue: [
			bFFfillAlpha _ fAlpha ifNil: [fillColor alphaAtX: x y: y].
			b7FbFFborderFillAbyBorderA _ b7FgreenAlpha * bFFborderAlpha.
			b7FbFFborderFillUbyFillA _ (127- b7FgreenAlpha) * bFFfillAlpha.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FgreenAlpha  * bFFborderAlpha.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultGA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorGA)).
	b7FbFFbFFresultGA = 0
		ifTrue: [ bFFresultG _ 0 ]
		ifFalse: [
			bFFborderComponent _ bGreen ifNil: [borderColor greenAtX: x y: y].
			isGreenInside
				ifTrue: [
					bFFfillComponent _ fGreen ifNil: [fillColor greenAtX: x y: y].
					b7FbFFbFFforegroundAByC _ (b7FbFFborderFillAbyBorderA * bFFborderComponent) + (b7FbFFborderFillUbyFillA * bFFfillComponent) ]
				ifFalse: [
					b7FbFFbFFforegroundAByC _ b7FbFFborderBackgroundAbyBorderA * bFFborderComponent ].
			"Atencion. Esta cuenta la estoy haciendo en float"
			bFFresultG _ ((b7FbFFbFFforegroundAByC * 255.0) + (bFFbackgroundColorGA * bFFbackgroundColorG * ( 32385 - b7FbFFforegroundA))) / b7FbFFbFFresultGA.
			bFFresultG _ bFFresultG truncated bitAnd: 16rFF ].

	subpixX _ x+SubPixDelta.
	bFFborderAlpha _ bAlpha ifNil: [borderColor alphaAtX: subpixX y: y].
	isBlueInside
		ifTrue: [
			bFFfillAlpha _ fAlpha ifNil: [fillColor alphaAtX: subpixX y: y].
			b7FbFFborderFillAbyBorderA _ b7FblueAlpha * bFFborderAlpha.
			b7FbFFborderFillUbyFillA _ (127- b7FblueAlpha) * bFFfillAlpha.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FblueAlpha  * bFFborderAlpha.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultBA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorBA)).
	b7FbFFbFFresultBA = 0
		ifTrue: [ bFFresultB _ 0 ]
		ifFalse: [
			bFFborderComponent _ bBlue ifNil: [borderColor blueAtX: subpixX y: y].
			isBlueInside
				ifTrue: [
					bFFfillComponent _ fBlue ifNil: [fillColor blueAtX: subpixX y: y].
					b7FbFFbFFforegroundAByC _ (b7FbFFborderFillAbyBorderA * bFFborderComponent) + (b7FbFFborderFillUbyFillA * bFFfillComponent) ]
				ifFalse: [
					b7FbFFbFFforegroundAByC _ b7FbFFborderBackgroundAbyBorderA * bFFborderComponent ].
			"Atencion. Esta cuenta la estoy haciendo en float"
			bFFresultB _ ((b7FbFFbFFforegroundAByC * 255.0) + (bFFbackgroundColorBA * bFFbackgroundColorB * ( 32385 - b7FbFFforegroundA))) / b7FbFFbFFresultBA.
			bFFresultB _ bFFresultB truncated bitAnd: 16rFF ].

	resultWord _ (255 bitShift: 24) bitOr: (bFFresultR bitShift: 16) + (bFFresultG bitShift: 8) + bFFresultB.
	bits at: pixelIndex put: resultWord.

	"usar la division aproximada rapida"
	resultColorRA _ (b7FbFFbFFresultRA // 32385) truncated bitAnd: 16rFF.
	resultColorGA _ (b7FbFFbFFresultGA // 32385) truncated bitAnd: 16rFF.
	resultColorBA _ (b7FbFFbFFresultBA // 32385) truncated bitAnd: 16rFF.
	resultWord _ (resultColorRA bitShift: 16) + (resultColorGA bitShift: 8) + resultColorBA.
	componentAlphas at: pixelIndex put: resultWord! !

!Morphic3Canvas2 methodsFor: 'trajectories - private' stamp: 'jmv 6/8/2010 10:27'!
zMark1blendIntWithBackgroundAndOpaqueColorsAlphaAtX: x y: y pixelIndex: pixelIndex redIsInside: isxRedInside greenIsInside: isxGreenInside blueIsInside: isxBlueInside

	"Derivado del otro. Solo se le cambia para que calcule alpha y no color, y que considere opacos a los colores, y que labure con el array correcto
	Se puede optimizar bastante, a costo de diferencias en el redondeo con el otro, que causan artefactos visibles.
	Esto NO pasaba en la version float, por supuesto"
	| resultWord b7FbFFborderFillAbyBorderA b7FbFFborderFillUbyFillA b7FbFFborderBackgroundAbyBorderA b7FbFFforegroundA b7FbFFbFFresultRA b7FbFFbFFresultGA b7FbFFbFFresultBA alphaWord b7FredAlpha b7FgreenAlpha b7FblueAlpha isRedInside isGreenInside isBlueInside backgroundAlphasWord bFFbackgroundColorRA bFFbackgroundColorGA bFFbackgroundColorBA resultColorRA resultColorGA resultColorBA |
	alphaWord _ auxAlphas at: pixelIndex.
	alphaWord = 0 ifTrue: [
		(isxRedInside or: [  isxGreenInside or: [ isxBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."
	b7FredAlpha _ (alphaWord bitShift: -16) bitAnd: 16r7F.
	b7FgreenAlpha _ (alphaWord bitShift: -8) bitAnd: 16r7F.
	b7FblueAlpha _ alphaWord bitAnd: 16r7F.
	isRedInside _ isxRedInside.
	isGreenInside _ isxGreenInside.
	isBlueInside _ isxBlueInside.
	
	"Esto no me gusta nada... ver si hay forma de hacer mejor"
	borderWidth = 0.0 ifTrue: [
		isxRedInside ifTrue: [ b7FredAlpha _ 127-b7FredAlpha. isRedInside _ false ].
		isxGreenInside ifTrue: [ b7FgreenAlpha _ 127-b7FgreenAlpha. isGreenInside _ false ].
		isxBlueInside ifTrue: [ b7FblueAlpha _ 127-b7FblueAlpha. isBlueInside _ false ]].

	backgroundAlphasWord _ componentAlphasIfOnlyOpaqueColors at: pixelIndex.
	bFFbackgroundColorRA _ (backgroundAlphasWord bitShift: -16) bitAnd: 16rFF.
	bFFbackgroundColorGA _ (backgroundAlphasWord bitShift: -8) bitAnd: 16rFF.
	bFFbackgroundColorBA _ backgroundAlphasWord bitAnd: 16rFF.

	isRedInside
		ifTrue: [
			b7FbFFborderFillAbyBorderA _ b7FredAlpha * 255.
			b7FbFFborderFillUbyFillA _ (127- b7FredAlpha) * 255.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FredAlpha  * 255.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultRA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorRA)).

	isGreenInside
		ifTrue: [
			b7FbFFborderFillAbyBorderA _ b7FgreenAlpha * 255.
			b7FbFFborderFillUbyFillA _ (127- b7FgreenAlpha) * 255.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FgreenAlpha  * 255.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultGA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorGA)).

	isBlueInside
		ifTrue: [
			b7FbFFborderFillAbyBorderA _ b7FblueAlpha * 255.
			b7FbFFborderFillUbyFillA _ (127- b7FblueAlpha) * 255.
			b7FbFFforegroundA _ b7FbFFborderFillAbyBorderA + b7FbFFborderFillUbyFillA ]
		ifFalse: [
			b7FbFFborderBackgroundAbyBorderA _ b7FblueAlpha  * 255.
			b7FbFFforegroundA _ b7FbFFborderBackgroundAbyBorderA ].
	b7FbFFbFFresultBA _ 8258175 - ((32385 - b7FbFFforegroundA) * (255 - bFFbackgroundColorBA)).

	"usar la division aproximada rapida"
	resultColorRA _ (b7FbFFbFFresultRA // 32385) truncated bitAnd: 16rFF.
	resultColorGA _ (b7FbFFbFFresultGA // 32385) truncated bitAnd: 16rFF.
	resultColorBA _ (b7FbFFbFFresultBA // 32385) truncated bitAnd: 16rFF.
	resultWord _ (resultColorRA bitShift: 16) + (resultColorGA bitShift: 8) + resultColorBA.
	componentAlphasIfOnlyOpaqueColors at: pixelIndex put: resultWord! !

!M3Exp01Morph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 14:34'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"
| m |
m _ M3Exp01Morph new.
m privateOwner: self runningWorld.
m location setPosition: 50@30.
(Morphic3Canvas onForm: Display) fullDraw: m.
Display forceToScreen
	"

aCanvas
		ellipseCenterX: 46 y: 46 rx: "24"16 ry: "12"16  borderWidth: 4 borderColor: (Color black "green alpha: 0.6") fillColor: (Color yellow alpha: 0.4) strokeDashArray: nil strokeDashArrayOffset: nil.
aCanvas
		ellipseCenterX: 46 y: 46 rx: "24"36 ry: "12"36  borderWidth: 18 borderColor: (Color black "green alpha: 0.6") fillColor: (Color yellow alpha: 0.0) strokeDashArray: nil strokeDashArrayOffset: nil.


	aCanvas
		polyLine: {15@32. 30@50. 45@20. 15@32}
		borderWidth: 0.0
		borderColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		fillColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		strokeDashArray: nil
		strokeDashArrayOffset: nil.
	aCanvas
		polyLine: {60@32. 30@50. 45@20. 60@32}
		borderWidth: 0.0
		borderColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		fillColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		strokeDashArray: nil
		strokeDashArrayOffset: nil.


	aCanvas cubicBezier: {10@5. 15@30. 60@40. 60@10} borderWidth: 2 borderColor: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) strokeDashArray: nil strokeDashArrayOffset: nil.

	aCanvas cubicBezier: {10@5. 15@30. 60@60. 60@10} borderWidth: 2 borderColor: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) strokeDashArray: nil strokeDashArrayOffset: nil.

	aCanvas quadraticBezier: { 10@5 . 15@30 . 60@10 }borderWidth: 1.5 borderColor: (Color red alpha: 0.7+0.3) fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
	aCanvas quadraticBezier: { 10@5 . 60@60 . 60@10 } borderWidth: 1 borderColor: (Color green alpha: 0.7+0.3) fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
	

aCanvas roundRectangleMinX: -10 maxX: 80 minY: -10 maxY: 60 rx: 40 ry: 20 borderWidth: 5 borderColor: (Color green alpha: 0.6) fillColor: (Color red alpha: 0.3) strokeDashArray: nil strokeDashArrayOffset: nil! !

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 7/14/2014 14:28'!
clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ true! !

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 7/3/2014 11:08'!
clipsSubmorphs

	^true! !

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 14:34'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"
| m |
m _ M3Exp02Morph new.
m privateOwner: self runningWorld.
m location setPosition: 50@30.
(Morphic3Canvas onForm: Display) fullDraw: m.
Display forceToScreen
	"
	
	aCanvas
		roundRectangleMinX: -10 maxX: 80 minY: -10 maxY: 60 
		rx: 40 ry: 20 
		borderWidth: 15 
		borderColor: (Color green alpha: 0.8) 
		fillColor: (Color red alpha: 0.6) 
		strokeDashArray: nil 
		strokeDashArrayOffset: nil! !

!M3Exp03Morph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 14:34'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"
| m |
m _ M3Exp03Morph new.
m privateOwner: self runningWorld.
m location setPosition: 50@30.
(Morphic3Canvas onForm: Display) fullDraw: m.
Display forceToScreen
	"

true ifTrue: [
	aCanvas
		polyLine: {-10@32. 120@-30. 
"		60@20. 120@20. 120@10. 150@50. 120@70. 120@30. 80@30."
"		60@90. -50@32"
		60@90. -60@80. -60@20. -50@70. -10@32
		}
		borderWidth: 0.0
		borderColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		fillColor: "(Color r: 0.961 g: 0.772 b: 0.8)" (Color black alpha: 0.5)
		strokeDashArray: nil
		strokeDashArrayOffset: nil.
].
false ifTrue: [
	aCanvas
		polyLine: {60@32. 30@50. 45@20. 60@32}
		borderWidth: 0.0
		borderColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		fillColor: "(Color r: 0.961 g: 0.772 b: 0.8)" Color black
		strokeDashArray: nil
		strokeDashArrayOffset: nil.
].

false ifTrue: [
	aCanvas cubicBezier: {10@5. 15@30. 60@40. 60@10} borderWidth: 2 borderColor: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) strokeDashArray: nil strokeDashArrayOffset: nil.
].

false ifTrue: [
	aCanvas cubicBezier: {10@5. 15@30. 60@60. 60@10} borderWidth: 2 borderColor: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) strokeDashArray: nil strokeDashArrayOffset: nil.
].

false ifTrue: [
	aCanvas quadraticBezier: { 10@5 . 15@30 . 60@10 }borderWidth: 1.5 borderColor: (Color red alpha: 0.7+0.3) fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
].

false ifTrue: [
	aCanvas quadraticBezier: { 10@5 . 60@60 . 60@10 } borderWidth: 1 borderColor: (Color green alpha: 0.7+0.3) fillColor: Color transparent strokeDashArray: nil strokeDashArrayOffset: nil.
]! !

!SVGElementMorph methodsFor: 'building' stamp: 'jmv 11/8/2012 20:09'!
buildFromXml: anXMLElement

	| usedAttributeNames styleAttributes strokeDashArrayData strokeDashArrayOffsetData transformData svgTransform |
	styleAttributes _ self class styleAttributesIn: anXMLElement.
	
	"These are always nested. No need to inherit."
	svgTransform _ SVGTransform new.
	transformData _ self class attributeAt: 'transform' in: anXMLElement orIn: styleAttributes.
	transformData ifNotNil: [
		svgTransform buildFrom: transformData.
		location _ svgTransform asMatrixTransform2x3 ].

	strokeWidth _ self class numberAttributeAt: 'stroke-width' in: anXMLElement orIn: styleAttributes.
	strokeColorOrGradient _ self class colorAttributeAt: 'stroke' in: anXMLElement orIn: styleAttributes.
	fillColorOrGradient _ self class colorAttributeAt: 'fill' in: anXMLElement orIn: styleAttributes.
	strokeOpacity _ self class numberAttributeAt: 'stroke-opacity' in: anXMLElement orIn: styleAttributes.
	fillOpacity _ self class numberAttributeAt: 'fill-opacity' in: anXMLElement orIn: styleAttributes.
	opacity _ self class numberAttributeAt: 'opacity' in: anXMLElement orIn: styleAttributes.

	strokeDashArrayData _ self class attributeAt: 'stroke-dasharray' in: anXMLElement orIn: styleAttributes.
	strokeDashArrayOffsetData _ self class attributeAt: 'stroke-dashoffset' in: anXMLElement orIn: styleAttributes.
	(strokeDashArrayData notNil and: [ strokeDashArrayData ~= 'none' ]) ifTrue: [
		strokeDashArray _ (self class numbersIn: strokeDashArrayData).

	strokeDashArray size even ifFalse: [
		strokeDashArray _ strokeDashArray, strokeDashArray ].
	strokeDashArrayOffsetData ifNotNil: [
		strokeDashArrayOffset _ strokeDashArrayOffsetData asNumber ]].

	defs _ nil.
	anXMLElement elementsDo: [ :subElement |
		"Process def elements first"
		(subElement name = SVGDefs svgName)
			ifTrue: [
				defs ifNil: [ defs _ SVGDefs new ].
				defs buildFromXml: subElement]].


	usedAttributeNames _ self usedAttributeNames asSet.
	anXMLElement attributes keysAndValuesDo: [ :attrName :value |
		((usedAttributeNames includes: attrName) or: [
			(IgnoredAttributes includes: attrName) or: [
				(IgnoredPrefixes anySatisfy: [ :prefix | attrName beginsWith: prefix]) or: [
					(DefaultedAtributes at: attrName ifAbsent: []) = value]]])  ifFalse: [
			SVGWarning signal: 'Unhandled attribute: ', attrName printString, ' in: ', self class name, ' value: ', value printString ]].

	styleAttributes ifNotNil: [
		styleAttributes  keysAndValuesDo: [ :attrName :value |
			((usedAttributeNames includes: attrName) or: [
				(IgnoredAttributes includes: attrName) or: [
					(IgnoredPrefixes anySatisfy: [ :prefix | attrName beginsWith: prefix]) or: [
						(DefaultedAtributes at: attrName ifAbsent: []) = value]]])  ifFalse: [
				SVGWarning signal: 'Unhandled style token: ', attrName printString, ' in: ', self class name, ' value: ', value printString ]]].

	anXMLElement elementsDo: [ :subElement |
		(self isKindOf: SVGGroupMorph) ifFalse: [
			SVGWarning signal: 'Unexpected element: ', subElement name printString, ' in: ', self class name ]
		]! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 14:17'!
defAt: aString

	| answer |
	answer _ defs ifNotNil: [ defs defAt: aString ].
	^answer ifNil: [
		(owner isKindOf: SVGElementMorph)
			ifTrue: [ owner defAt: aString ] ]! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 21:53'!
fillColorOrGradient

	| answer |
	answer _ fillColorOrGradient.
	
	fillColorOrGradient == #inherit
		ifTrue: [
			answer _ (owner isKindOf: SVGElementMorph)
				ifTrue: [ owner fillColorOrGradient ]
				ifFalse: [ Color black ]]
		ifFalse: [
			fillColorOrGradient isString ifTrue: [
				answer _ self defAt: fillColorOrGradient ]].
	answer ifNil: [ ^nil ].

	self flag: #revisarM3.
	"http://www.w3.org/TR/SVG/masking.html#ObjectAndGroupOpacityProperties
	En realidad la opacity hay que aplicarla al grupo..."

	(answer isKindOf: Morphic3Gradient)
		ifTrue: [ ^answer ].
	(answer isKindOf: SVGGradient) ifTrue: [
		^ answer asMorphic3GradientIn: self ].
	^answer alpha: self fillOpacity * self opacity! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 15:54'!
fillOpacity
	^fillOpacity == #inherit
		ifFalse: [ fillOpacity ]
		ifTrue: [
			(owner isKindOf: SVGElementMorph)
				ifTrue: [ owner fillOpacity ]
				ifFalse: [ 1.0 ]]! !

!SVGElementMorph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:30'!
morphBoundsInWorld
self revisar. "por supuesto que no..."
"Intentar eliminar, directamente"
	^Display boundingBox! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 15:54'!
opacity
	^opacity == #inherit
		ifFalse: [ opacity ]
		ifTrue: [
			(owner isKindOf: SVGElementMorph)
				ifTrue: [ owner opacity ]
				ifFalse: [ 1.0 ]]! !

!SVGElementMorph methodsFor: 'private' stamp: 'jmv 6/10/2010 21:50'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	submorphs do: [ :each | each privatePointsDo: aBlock ]! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/12/2010 23:17'!
strokeColorOrGradient
"
Este metodo deberia recibir el rectangulo de pantalla. Y dejar claro que es adaptado a ese rectangulo, y que no debe llamarse en cualquier momento
Si llamamos al owner, pasar el rectangulo tambien.
Reescribir el #asMorphic3GradientIn:

Duda terrible: Donde se aplica la traduccion si hay transform? No se aplica?
Oh, es en el #toWorldFrom:
	Eso quiere decir que armo todo en coordenadas del morph y traduzco despues.

Creo que en el nuevo esquema nada de eso es necesario. Directamente aplico la spec (en fracciones 0..1) al rectangulo de display que conoce el canvas. Se simplifica bastante!!
Claro, los vectores  (las fracciones 0..1) hay que traducirlos del espacio del morph al world, obviamente

Lo que no tengo claro es como hacer lo de los angulos correctos, con toda esa cosa rara...
Quizas obtener perpendiculares, traudir hacia afuera, y perpendiculares de nuevo...?
Ver
'Ejemplos/LinearGradientTest.svg'
'Ejemplos/LinearGradientTest2.svg'
'Ejemplos/LinearGradientTest3.svg'
'Ejemplos/LinearGradientTest4.svg' (inkscape y firefox coinciden, safari distinto la rotacion)
	objectBoundingBox con % (en este caso, si no es cuadrado, hace la cosa rara seudo-perpendicular)
	pareciera que en este caso, aplicar al box calculado al renderizar estaria bien
'Ejemplos/LinearGradientTest5.svg' (inkscape y safari coinciden, firefox cambia el gradiente al retam ventana!!)
	es userSpaceOnUse con %
	En este caso no se. Cual corno es? los bounds del morph mas externo?
'Ejemplos/LinearGradientTest6.svg' (coinciden inkscape, safari, firefox)
	userSpaceOnUse  con coordenadas absolutas en ese espacio
	Ojo. Las coordenadas estan en el morph que lo referencia directamente (no en quien lo herede)
	Eso pareciera querer decir que el gradiente conoce un 'owner'

Es probable que todo esto finalmente SI simplifique cosas...

Creo que me conviene un chage set nuevo, y quizas clases nuevas para esto... me voy a marear...
gradientTransform too... http://www.w3.org/TR/SVG/pservers.html 

Clases nuevas. Una para gradiente en % del objectBoundingBox (se termina de armar despues de la trayectoria y antes del blend), y otra para coordenadas absolutas (aplicar % si es lo que vino) del sistCoord del owner del gradiente.  Se podria quizas armar en el drawOn: del morph en cuestion... Aunque para simplificar, tambien dan ganas de armarlo despues de la trayectoria y antes del blend...
"
	| answer |
	answer _ strokeColorOrGradient.
	
	strokeColorOrGradient == #inherit
		ifTrue: [
			answer _ (owner isKindOf: SVGElementMorph)
				ifTrue: [ owner strokeColorOrGradient ]
				ifFalse: [ nil ]]
		ifFalse: [
			strokeColorOrGradient isString ifTrue: [
				answer _ self defAt: strokeColorOrGradient ]].
	answer ifNil: [ ^nil ].

	self flag: #revisarM3.
	"http://www.w3.org/TR/SVG/masking.html#ObjectAndGroupOpacityProperties
	En realidad la opacity hay que aplicarla al grupo..."

	(answer isKindOf: Morphic3Gradient)
		ifTrue: [ ^answer ].
	(answer isKindOf: SVGGradient) ifTrue: [
		^ answer asMorphic3GradientIn: self ].
	^answer alpha: self strokeOpacity * self opacity! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 4/29/2011 21:55'!
strokeDashArray

	| s |
	^strokeDashArray ifNotNil: [
		s _ 0.0.
		strokeDashArray collect: [ :d |
			s _ s + d. 
			s]]! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 4/29/2011 21:54'!
strokeDashArrayOffset
	^ strokeDashArrayOffset! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 15:54'!
strokeOpacity
	^strokeOpacity == #inherit
		ifFalse: [ strokeOpacity ]
		ifTrue: [
			(owner isKindOf: SVGElementMorph)
				ifTrue: [ owner strokeOpacity ]
				ifFalse: [ 1.0 ]]! !

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 15:16'!
strokeWidth

	strokeWidth == #inherit ifTrue: [
		^ (owner isKindOf: SVGElementMorph)
			ifTrue: [ owner strokeWidth ]
			ifFalse: [ 1 ]].

	"nil"
	^strokeWidth! !

!SVGElementMorph methodsFor: 'building' stamp: 'jmv 6/16/2010 10:01'!
usedAttributeNames
	^#('style' 'fill' 'fill-opacity' 'opacity' 'stroke' 'stroke-opacity' 'stroke-width' 'transform' 'stroke-dasharray' 'stroke-dashoffset')! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:16'!
attributeAt: attributeName in: anXMLElement orIn: styleDictionary

	^anXMLElement attributeAt: attributeName ifAbsent: [
		styleDictionary ifNotNil: [ styleDictionary at: attributeName ifAbsent: nil ]]! !

!SVGElementMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:25'!
classFor: anSVGName

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGName
			ifTrue: [ ^cls ] ].
	^nil! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:17'!
colorAttributeAt: attributeName in: anXMLElement orIn: styleDictionary

	| data |
	data _ self attributeAt: attributeName in: anXMLElement orIn: styleDictionary.
	^data
		ifNil: [ #inherit ]
		ifNotNil: [ self svgColorOrGradientNameFor: data ]! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 7/10/2014 14:10'!
examples
"
SVGElementMorph examples
"
| m base |
base _ 'svgSamples', FileDirectory slash.
#('lion' 'lionx' 'tiger1' 'magician' 'beer' 'butterfly' 'picasso' 'Phone' 'baby' 'longhorn' 'cowboy' 'dancer' 'b8' 'clinton' 'pixelCoverageSucks') do: [ :fn |
Transcript clear.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, fn, '.svg'.
"		'lionx.svg'"
"		'tiger1.svg'"
"		'magician.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/Phone.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/pixelCoverageSucks.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/svg/lion.svg'"
"		'svgSamples\b8.svg'"
"		'svgSamples/baby.svg'"
"		 'svgSamples/beer.svg'"
"		'svgSamples/clinton.svg'"
"		'svgSamples/dancer.svg'"
"		'svgSamples/longhorn.svg'"
"		'svgSamples/butterfly.svg'"
"		'svgSamples/picasso.svg'"
"gradients"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/adam_lowe/adam_lowe_Branches_Flowers_Birds.svg'"	
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/adam_lowe/adam_lowe_Owl.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/addon/addon_the_artist.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/addon/addon_the_photographer.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/addon/addon_sandcastle_2.svg'"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/adam_lowe/flower1.svg'"
"zeroDivide!!!!!!"
"		'/Users/juanvuletich/Desktop/Morphic 3/svgSamples/http-openclipart.org/people/adam_lowe/adam_lowe_Anime_Schoolgirl.svg'"	
		.
	m privateOwner: self runningWorld.
	m morphPosition: 100@100.
	(Morphic3Canvas onForm: Display) fullDraw: m.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 7/10/2014 14:12'!
examplesMagician
"
SVGElementMorph examplesMagician
"
| m base |
Transcript clear.
base _ 'svgSamples', FileDirectory slash.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'magician.svg'
		.
	m privateOwner: self runningWorld.
	m morphPosition: 100@100.
	(Morphic3Canvas onForm: Display) fullDraw: m.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'instance creation' stamp: 'jmv 6/16/2010 10:32'!
fromXml: anXMLElement
	| cls n |
	n _  anXMLElement name.
	cls _ SVGElementMorph classFor: n.
	cls ifNil: [
		((IgnoredElements includes: n) or: [
			IgnoredPrefixes anySatisfy: [ :prefix | n beginsWith: prefix ]]) ifFalse: [
			SVGWarning signal: ('Unhandled element: ', n printString) ].
		^nil ].
	^ cls new buildFromXml: anXMLElement! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 09:50'!
ignoredAttributes

	^IgnoredAttributes! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 10:44'!
ignoredElements

	^IgnoredElements! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 10:08'!
ignoredPrefixes

	^IgnoredPrefixes! !

!SVGElementMorph class methodsFor: 'class initialization' stamp: 'jmv 6/16/2010 10:42'!
initialize
	"
	self initialize
	"
	IgnoredPrefixes _ #('sodipodi:' 'inkscape:' 'xmlns:' 'i:') asSet.
	IgnoredElements _ #('title' 'metadata' 'foreignObject') asSet.
	IgnoredAttributes _ #('id' 'stroke-miterlimit') asSet.
	DefaultedAtributes _ Dictionary new
		at: 'fill-rule' put: 'nonzero';
		at: 'stroke-linecap' put: 'round';
		yourself! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:17'!
numberAttributeAt: attributeName in: anXMLElement orIn: styleDictionary

	| data |
	data _ self attributeAt: attributeName in: anXMLElement orIn: styleDictionary.
	^data
		ifNil: [ #inherit ]
		ifNotNil: [ data asNumber ]! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:18'!
numbersIn: aString

	| rs |
	rs _ aString readStream.
	^Array streamContents: [ :strm |
		[ rs atEnd ] whileFalse: [
			rs nextNumber ifNotNil: [ :n |
				strm nextPut: n ]]]! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 7/10/2014 14:11'!
phoneExample
"
SVGElementMorph phoneExample
"
| m base |
base _ 'svgSamples', FileDirectory slash.
Transcript clear.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'Phone.svg'.
	m privateOwner: self runningWorld.
	m location setRadians: 0 / 180 * Float pi scale: 0.5; setPosition: 120.0@100.0.
	(Morphic3Canvas onForm: Display) fullDraw: m.
	m location setRadians:  -15 / 180 * Float pi scale: 0.75; setPosition:  500.0@110.0.
	(Morphic3Canvas onForm: Display) fullDraw: m.
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 7/30/2012 18:00'!
styleAttributesIn: anXMLElement
	| styleDict styleData pair |
	styleData _ anXMLElement attributeAt: 'style'.
	styleData ifNil: [ ^nil ].
	
	styleDict _ Dictionary new.
	(styleData findTokens: #($;)) do: [ :each |	"For example, 'fill:#653300'"
		pair _ each findTokens: #($:).
		pair size > 1 ifTrue: [
			styleDict at: pair first withBlanksTrimmed put: pair second withBlanksTrimmed ]].
	^styleDict! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 2/15/2013 16:53'!
svgColorOrGradientNameFor: aString

	| dataStream |
	aString = 'none' ifTrue: [^nil].
self revisar.
	(aString beginsWith: 'url')
		ifFalse: [ ^Color "fromCSS2String: just hex? Or real CSS2?" fromString: aString ].
	" 'url'  means reference to a def object. Just keep the name, for later binding"
	dataStream _ aString readStream.
	dataStream upTo: $#.
	^(dataStream upTo: $) ) withBlanksTrimmed! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:20'!
svgHeightFractionIn: anXMLElement

	| string |
	string _ anXMLElement attributeAt: 'height' ifAbsent: [ ^ nil ].
	^self svgLengthFractionFor: string! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 7/30/2012 18:00'!
svgLengthFractionFor: aString

	| string data |
	string _ aString withBlanksTrimmed.
	data _ string asNumber.
	(string last = $%) ifTrue: [
		data _ data / 100.0 ].
	^data! !

!SVGElementMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:28'!
svgName
	"Answer the name of the svg elements we can represent"

	^self subclassResponsibility! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:21'!
svgPixelHeightIn: anXMLElement

	| string |
	string _ anXMLElement attributeAt: 'height' ifAbsent: [ ^ nil ].
	^self svgPixelLengthFor: string! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 7/30/2012 18:00'!
svgPixelLengthFor: aString

	| string data |
	string _ aString withBlanksTrimmed.
	(string last = $%) ifTrue: [ ^nil ].
	data _ string asNumber.
	(string endsWith: 'in')
		ifTrue: [ ^(data * 90) rounded ].
	(string endsWith: 'mm')
		ifTrue: [ ^(data * 3.571429) rounded ].
	(string endsWith: 'cm')
		ifTrue: [ ^(data * 35.71429) rounded ].
	(string endsWith: 'pt')
		ifTrue: [ ^(data * 1.25) rounded ].
	(string endsWith: 'pc')
		ifTrue: [ ^(data * 15) rounded ].
	^data rounded! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:21'!
svgPixelWidthIn: anXMLElement

	| string |
	string _ anXMLElement attributeAt: 'width' ifAbsent: [ ^ nil ].
	^self svgPixelLengthFor: string! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:21'!
svgViewBoxIn: anXMLElement

	| string data |
	string _ anXMLElement attributeAt: 'viewBox' ifAbsent: [ ^ nil ].
	data _ string substrings collect: [ :str | str asNumber].
	^Rectangle origin: (data first @ data second) extent: (data third @ data fourth)! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:21'!
svgWidthFractionIn: anXMLElement

	| string |
	string _ anXMLElement attributeAt: 'width' ifAbsent: [ ^ nil ].
	^self svgLengthFractionFor: string! !

!SVGCircle methodsFor: 'building' stamp: 'jmv 6/10/2010 09:21'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	cx _ (anXMLElement attributeAt: 'cx') asNumber.
	cy _ (anXMLElement attributeAt: 'cy') asNumber.
	r _ (anXMLElement attributeAt: 'r') asNumber! !

!SVGCircle methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:28'!
drawOn: aCanvas
	aCanvas
		ellipseCenterX: cx y: cy rx: r ry: r
		borderWidth: self strokeWidth
		borderColor: self strokeColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: self strokeDashArray
		strokeDashArrayOffset: self strokeDashArrayOffset! !

!SVGCircle methodsFor: 'private' stamp: 'jmv 6/10/2010 21:50'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	super privatePointsDo: aBlock.
	aBlock
		value: (cx-r) @ cy;
		value: cx @ (cy-r);
		value: (cx+r) @ cy;
		value: cx @ (cy+r)! !

!SVGCircle methodsFor: 'building' stamp: 'jmv 5/14/2010 22:21'!
usedAttributeNames

	^super usedAttributeNames,
		#('cx' 'cy' 'r')! !

!SVGCircle class methodsFor: 'instance creation' stamp: 'jmv 5/14/2010 22:25'!
svgName
	"Answer the name of the svg elements we can represent"

	^#circle! !

!SVGEllipse methodsFor: 'building' stamp: 'jmv 6/10/2010 09:21'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	cx _ (anXMLElement attributeAt: 'cx') asNumber.
	cy _ (anXMLElement attributeAt: 'cy') asNumber.
	rx _ (anXMLElement attributeAt: 'rx') asNumber.
	ry _ (anXMLElement attributeAt: 'ry') asNumber! !

!SVGEllipse methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:28'!
drawOn: aCanvas

	aCanvas
		ellipseCenterX: cx y: cy rx: rx ry: ry
		borderWidth: self strokeWidth
		borderColor: self strokeColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: self strokeDashArray
		strokeDashArrayOffset: self strokeDashArrayOffset! !

!SVGEllipse methodsFor: 'private' stamp: 'jmv 6/10/2010 21:50'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	super privatePointsDo: aBlock.
	aBlock
		value: (cx-rx) @ cy;
		value: cx @ (cy-ry);
		value: (cx+rx) @ cy;
		value: cx @ (cy+ry)! !

!SVGEllipse methodsFor: 'building' stamp: 'jmv 5/14/2010 22:21'!
usedAttributeNames

	^super usedAttributeNames,
		#('cx' 'cy' 'rx' 'ry')! !

!SVGEllipse class methodsFor: 'instance creation' stamp: 'jmv 5/14/2010 22:25'!
svgName
	"Answer the name of the svg elements we can represent"

	^#ellipse! !

!SVGGroupMorph methodsFor: 'building' stamp: 'jmv 6/10/2010 10:17'!
buildFromXml: anXMLElement
	| m |

	super buildFromXml: anXMLElement.

	anXMLElement elementsDo: [ :subElement |
		"Skip de def elements"
		(subElement name = SVGDefs svgName)
			ifFalse: [
				m _ SVGElementMorph fromXml: subElement.
				m ifNotNil: [
					self addMorph: m ]]].! !

!SVGGroupMorph methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:28'!
drawOn: aCanvas
	"Nothing to do here..."! !

!SVGGroupMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:35'!
svgName
	"Answer the name of the svg elements we can represent"

	^#g! !

!SVGMainMorph methodsFor: 'building' stamp: 'jmv 11/8/2012 19:25'!
buildFromXml: anXMLElement

	
	| viewBox w h xtraViewBoxH wFraction hFraction |

	super buildFromXml: anXMLElement.
	wFraction _ (self class svgWidthFractionIn: anXMLElement) ifNil: [1.0].
	hFraction _ (self class svgHeightFractionIn: anXMLElement) ifNil: [ 1.0 ].
	w _ self class svgPixelWidthIn: anXMLElement.
	h _ self class svgPixelHeightIn: anXMLElement.
	"For some unknown (to me) reason, Inkscape uses 745@1053 if the svg doesn't specify sizes"
	viewBox _ (self class svgViewBoxIn: anXMLElement) ifNil: [ 0@0 extent: (w ifNil: [(745 / hFraction) rounded])@(h ifNil: [(1053 / hFraction) rounded]) ].
	xtraViewBoxH _ 0.
	(w notNil and: [h notNil]) ifTrue: [
		xtraViewBoxH _ (h / w * viewBox width - viewBox height / 2.0) rounded max: 0 ].
	w ifNil: [ w _ (viewBox width * wFraction) rounded ].
	h ifNil: [ h _ (viewBox height * hFraction) rounded ].

	self flag: #revisarM3.
	"space
		positionInOwner: (w@h // 2);"
"		locationExtent: (w@h);"
		"rotationAngle: 0."
"		xMin: viewBox left xMax: viewBox right yMin: viewBox top - xtraViewBoxH yMax: viewBox bottom + xtraViewBoxH"
	location setPosition: (w@h // 2)! !

!SVGMainMorph methodsFor: 'building' stamp: 'jmv 6/10/2010 08:56'!
usedAttributeNames
	^super usedAttributeNames, 
		#('version' 'xmlns' 'xmlns:xlink' 'viewBox' 'width' 'height')! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 6/9/2010 22:26'!
attributeAt: attributeName in: anXMLElement orIn: styleDictionary

	^anXMLElement attributeAt: attributeName ifAbsent: [
		styleDictionary ifNotNil: [ styleDictionary at: attributeName ifAbsent: nil ]]! !

!SVGMainMorph class methodsFor: 'instance creation' stamp: 'jmv 6/10/2010 09:31'!
fromFileNamed: fileName
	"
	| m |
	m _ SVGMorph fromFileNamed: 'pru04.svg'.
	m privateOwner: World; inspect.
	m drawOn: Display getCanvas.
	Display forceToScreen
	"
	| xmlDocument |
	xmlDocument _ XMLDOMParser parseDocumentFromFileNamed: fileName readIntoMemory: true.
	^ self fromXml: xmlDocument elements first! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 6/9/2010 22:26'!
numbersIn: aString

	| rs |
	rs _ aString readStream.
	^Array streamContents: [ :strm |
		[ rs atEnd ] whileFalse: [
			strm nextPut: rs nextNumber ]]! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 7/30/2012 18:00'!
styleAttributesIn: anXMLElement
	| styleDict styleData pair |
	styleData _ anXMLElement attributeAt: 'style'.
	styleData ifNil: [ ^nil ].
	
	styleDict _ Dictionary new.
	(styleData findTokens: #($;)) do: [ :each |	"For example, 'fill:#653300'"
		pair _ each findTokens: #($:).
		pair size > 1 ifTrue: [
			styleDict at: pair first withBlanksTrimmed put: pair second withBlanksTrimmed ]].
	^styleDict! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 2/15/2013 16:53'!
svgColorOrGradientNameFor: aString

	| dataStream |
	aString = 'none' ifTrue: [^nil].
self revisar.
	(aString beginsWith: 'url')
		ifFalse: [ ^Color "fromCSS2String: just hex? Or real CSS2?" fromString:  aString ].

	" 'url'  means reference to a def object. Just keep the name, for later binding"
	dataStream _ aString readStream.
	dataStream upTo: $#.
	^(dataStream upTo: $) ) withBlanksTrimmed! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 2/15/2013 16:53'!
svgColorOrGradientNameFor: aString opacity: opacityString

	| dataStream c |
	aString = 'none' ifTrue: [^nil].
self revisar.
	(aString beginsWith: 'url')
		ifFalse: [ 
			c _ Color "fromCSS2String: just hex? Or real CSS2?" fromString:  aString.
			opacityString ifNotNil: [
				c _ c alpha: opacityString asNumber ].
			^c ].

	" 'url'  means reference to a def object. Just keep the name, for later binding"
	dataStream _ aString readStream.
	dataStream upTo: $#.
	^(dataStream upTo: $) ) withBlanksTrimmed! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 7/30/2012 18:00'!
svgLengthFractionFor: aString

	| string data |
	string _ aString withBlanksTrimmed.
	data _ string asNumber.
	(string last = $%) ifTrue: [
		data _ data / 100.0 ].
	^data! !

!SVGMainMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:25'!
svgName
	"Answer the name of the svg elements we can represent"

	^#svg! !

!SVGMainMorph class methodsFor: 'services' stamp: 'jmv 7/30/2012 18:00'!
svgPixelLengthFor: aString

	| string data |
	string _ aString withBlanksTrimmed.
	(string last = $%) ifTrue: [ ^nil ].
	data _ string asNumber.
	(string endsWith: 'in')
		ifTrue: [ ^(data * 90) rounded ].
	(string endsWith: 'mm')
		ifTrue: [ ^(data * 3.571429) rounded ].
	(string endsWith: 'cm')
		ifTrue: [ ^(data * 35.71429) rounded ].
	(string endsWith: 'pt')
		ifTrue: [ ^(data * 1.25) rounded ].
	(string endsWith: 'pc')
		ifTrue: [ ^(data * 15) rounded ].
	^data rounded! !

!SVGLine methodsFor: 'building' stamp: 'jmv 6/10/2010 09:21'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	x1 _ (anXMLElement attributeAt: 'x1') asNumber.
	y1 _ (anXMLElement attributeAt: 'y1') asNumber.
	x2 _ (anXMLElement attributeAt: 'x2') asNumber.
	y2 _ (anXMLElement attributeAt: 'y2') asNumber! !

!SVGLine methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:28'!
drawOn: aCanvas
	"Se podria hacer alternativamente basado en 
		line: point1 to: point2 width: width color: c in: aMorph
		Pensar!!"

	self flag: #revisarM3.
"	aCanvas line: x1@y1 to: x2@y2 width: borderWidth color: borderColorOrGradient in: self	"
		
		"Puedo pasar in owner..."! !

!SVGLine methodsFor: 'private' stamp: 'jmv 6/10/2010 21:51'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	super privatePointsDo: aBlock.
	aBlock
		value: x1 @ y1;
		value: x2 @ y2! !

!SVGLine methodsFor: 'building' stamp: 'jmv 5/14/2010 22:17'!
usedAttributeNames

	^super usedAttributeNames,
		#('x1' 'y1' 'x2' 'y2')! !

!SVGLine class methodsFor: 'instance creation' stamp: 'jmv 5/14/2010 22:17'!
svgName
	"Answer the name of the svg elements we can represent"

	^#line! !

!SVGPath methodsFor: 'building' stamp: 'jmv 6/11/2010 14:12'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	| commandsData commandsDataStream command commandData commandChar |
	super buildFromXml: anXMLElement.
	commands _ OrderedCollection new.
	commandsData _ anXMLElement attributeAt: 'd'.
	commandsDataStream _ commandsData readStream.
	commandsDataStream upToAny: 'MLHVCSQTAZmlhvcsqtaz'.
	[ commandsDataStream atEnd ] whileFalse: [
		commandChar _ commandsDataStream next.
		commandData _ commandsDataStream upToAny: 'MLHVCSQTAZmlhvcsqtaz'.
		command _ SVGPathCommand from: commandChar stream: commandData readStream.
		commands add: command. ]! !

!SVGPath methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:29'!
drawOn: aCanvas
"Atencion!! hacer lo dash en todos!!!!!!!!!!"

"Esto hay que hacerlo solo si el path no tiene z... O hacerlo siempre asi, como deciamos..."
| bw |
self flag: #revisarM3.
false ifTrue: [
	"Atencion!! esta tecnica se lleva mal con el ensanchamiento / grisado de los bordes.
	En Tiger.svg, los bigotes tienen borde muy angosto.
	Aca, se lo engrosa y hace trasluciro, y eso le roba espacio al relleno blanco opaco del bigote.
	La opcion de abajo es mejor: Primero rellenamos de blanco opaco todo el ancho que corresponde al relleno del bigote,
	y despues le pintamos el borde por encima.
	
	Tomar la opcion de abajo en todos lados, y simplificar el blend. Ya encontre el argumento para hacerlo
	"
	aCanvas
		pathCommands: commands
		borderWidth: self strokeWidth
		borderColor: self strokeColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: self strokeDashArray
		strokeDashArrayOffset: self strokeDashArrayOffset.
] ifFalse: [
	aCanvas
		pathCommands: (commands copyWith: SVGPathCloseCommand new)
		borderWidth: 0
		borderColor: self fillColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: nil
		strokeDashArrayOffset: nil.
	bw _ self strokeWidth.
	bw > 0 ifTrue: [
		aCanvas
			pathCommands: commands
			borderWidth: bw
			borderColor: self strokeColorOrGradient
			fillColor: nil
			strokeDashArray: self strokeDashArray
			strokeDashArrayOffset: self strokeDashArrayOffset ]
]! !

!SVGPath methodsFor: 'private' stamp: 'jmv 6/10/2010 21:51'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	super privatePointsDo: aBlock.
	commands do:[ :each |
		each privatePointsDo: aBlock ]! !

!SVGPath methodsFor: 'building' stamp: 'jmv 5/4/2010 05:26'!
usedAttributeNames

	^super usedAttributeNames,
		#('d')! !

!SVGPath class methodsFor: 'instance creation' stamp: 'jmv 5/3/2010 22:25'!
svgName
	"Answer the name of the svg elements we can represent"

	^#path! !

!SVGPolyLine methodsFor: 'building' stamp: 'jmv 6/10/2010 22:04'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	| pointsData pointsDataStream x y |
	super buildFromXml: anXMLElement.
	
	points _ OrderedCollection new.
	pointsData _ anXMLElement attributeAt: 'points'.
	pointsDataStream _ pointsData readStream.
	[ pointsDataStream atEnd ] whileFalse: [
		x _ pointsDataStream nextNumber.
		y _ pointsDataStream nextNumber.
		y ifNotNil: [
			points add: x@y ]]! !

!SVGPolyLine methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:29'!
drawOn: aCanvas

	aCanvas
		polyLine: points
		borderWidth: self strokeWidth
		borderColor: self strokeColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: self strokeDashArray
		strokeDashArrayOffset: self strokeDashArrayOffset! !

!SVGPolyLine methodsFor: 'private' stamp: 'jmv 6/10/2010 21:51'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	super privatePointsDo: aBlock.
	points do: aBlock! !

!SVGPolyLine methodsFor: 'building' stamp: 'jmv 4/27/2010 15:59'!
usedAttributeNames

	^super usedAttributeNames,
		#('points')! !

!SVGPolyLine class methodsFor: 'instance creation' stamp: 'jmv 4/27/2010 15:53'!
svgName
	"Answer the name of the svg elements we can represent"

	^#polyline! !

!SVGPolygon methodsFor: 'building' stamp: 'jmv 6/10/2010 09:22'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	points last = points first ifFalse: [
		points add: points first ]! !

!SVGPolygon class methodsFor: 'instance creation' stamp: 'jmv 4/27/2010 15:54'!
svgName
	"Answer the name of the svg elements we can represent"

	^#polygon! !

!SVGRect methodsFor: 'building' stamp: 'jmv 6/10/2010 09:22'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	| w h |
	super buildFromXml: anXMLElement.
	xMin _ (anXMLElement attributeAt: 'x') asNumber.
	yMin _ (anXMLElement attributeAt: 'y') asNumber.
	w _ (anXMLElement attributeAt: 'width') asNumber.
	h _ (anXMLElement attributeAt: 'height') asNumber.
	xMax _  xMin + w.
	yMax _ yMin + h.
	rx _ (anXMLElement attributeAt: 'rx' ifAbsent: '0') asNumber min: w * 0.5.
	ry _ (anXMLElement attributeAt: 'ry' ifAbsent: '0') asNumber min: h * 0.5.! !

!SVGRect methodsFor: 'drawing' stamp: 'jmv 11/8/2012 19:30'!
drawOn: aCanvas

	aCanvas
		roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: rx ry: ry
		borderWidth: self strokeWidth
		borderColor: self strokeColorOrGradient
		fillColor: self fillColorOrGradient
		strokeDashArray: self strokeDashArray
		strokeDashArrayOffset: self strokeDashArrayOffset! !

!SVGRect methodsFor: 'private' stamp: 'jmv 6/14/2010 10:47'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	"tratar de eliminar"
	self flag: #revisarM3.

	super privatePointsDo: aBlock.
	aBlock
		value: xMin @ yMin;
		value: xMax @ yMax! !

!SVGRect methodsFor: 'building' stamp: 'jmv 5/14/2010 22:17'!
usedAttributeNames

	^super usedAttributeNames,
		#('x' 'y' 'width' 'height' 'rx' 'ry')! !

!SVGRect class methodsFor: 'instance creation' stamp: 'jmv 5/14/2010 22:17'!
svgName
	"Answer the name of the svg elements we can represent"

	^#rect! !

!Morphic3Gradient methodsFor: 'services' stamp: 'jmv 5/13/2010 21:25'!
alphaAtX: x y: y
	| f |
	f _ self vectorFractionAtX: x y: y.
	^self at: f in: stopAlphas! !

!Morphic3Gradient methodsFor: 'services' stamp: 'jmv 5/13/2010 21:23'!
at: aFloat in: anArray
	| f f2 stop1 stop2 |
	f2 _ aFloat max: stopFractions first.
	f2 _ f2 min: stopFractions last.
	^stopFractions
		findBinaryIndex: [ :each | f2 - each ]
		do: [ :stopIndex | anArray at: stopIndex ]
		ifNone: [ :stopIndex1 :stopIndex2 |
			stop1 _ stopFractions at: stopIndex1.
			stop2 _ stopFractions at: stopIndex2.
			f _ 1.0 / (stop2 - stop1).
			(((anArray at: stopIndex1) * (stop2 - f2) * f ) +
			((anArray at: stopIndex2) * (f2 - stop1) * f)) rounded]! !

!Morphic3Gradient methodsFor: 'services' stamp: 'jmv 5/13/2010 21:25'!
blueAtX: x y: y
	| f |
	f _ self vectorFractionAtX: x y: y.
	^self at: f in: stopBlues! !

!Morphic3Gradient methodsFor: 'services' stamp: 'jmv 5/13/2010 21:24'!
greenAtX: x y: y
	| f |
	f _ self vectorFractionAtX: x y: y.
	^self at: f in: stopGreens! !

!Morphic3Gradient methodsFor: 'services' stamp: 'jmv 5/13/2010 21:24'!
redAtX: x y: y
	| f |
	f _ self vectorFractionAtX: x y: y.
	^self at: f in: stopReds! !

!Morphic3Gradient methodsFor: 'construction' stamp: 'jmv 5/13/2010 09:58'!
stopFractions: aCollection stopColors: anotherCollection

	stopFractions _ aCollection.
	stopReds _ anotherCollection collect: [ :each | (each red * 255) rounded ].
	stopGreens _ anotherCollection collect: [ :each | (each green * 255) rounded ].
	stopBlues _ anotherCollection collect: [ :each | (each blue * 255) rounded ].
	stopAlphas _ anotherCollection collect: [ :each | (each alpha * 255) rounded ].! !

!Morphic3LinearGradient methodsFor: 'construction' stamp: 'jmv 4/29/2011 18:21'!
externalizedWith: aMatrixTransform2x3
	| p1 p2 |
	p1 _ aMatrixTransform2x3 externalizePosition: vectorX1@vectorY1.
	p2 _ aMatrixTransform2x3 externalizePosition: vectorX2@vectorY2.
	^self copy vector: p1 to: p2! !

!Morphic3LinearGradient methodsFor: 'construction' stamp: 'jmv 5/11/2010 10:15'!
vector: aPoint to: anotherPoint

	vectorX1 _ aPoint x.
	vectorY1 _ aPoint y.
	vectorX2 _ anotherPoint x.
	vectorY2 _ anotherPoint y! !

!Morphic3LinearGradient methodsFor: 'services' stamp: 'jmv 5/13/2010 10:13'!
vectorFractionAtX: x y: y
	| dx dy ddx ddy f |

	"ver que precalcular, etc.
	yo en realidad queria que en el over me vayan calculando la fraccion...
	Sera quizas una optimizacion. Creo que precisaria eliminar esta clase y mergearla en el canvas... 
	(para border y fill, 2 veces)"
	dx _ vectorX2 - vectorX1.
	dy _ vectorY2 - vectorY1.
	"ddx es cuanto hay que moverse en x (y solo en x) para ir del 0 al 1 en el vector"
	ddx  _ dx / (dx squared + dy squared).
	"Lo mismo para ddy"
	ddy _ dy / (dx squared + dy squared).

	"Ahora que tengo todo traducido hacia adentro, calculo en que parte del vector caigo"
	f _ (x-vectorX1 * ddx) + (y-vectorY1 * ddy).
	f _ (f max: stopFractions first) min: stopFractions last.	"esto ultimo no deberia hacer falta"
	^f! !

!Morphic3RadialGradient methodsFor: 'construction' stamp: 'jmv 5/13/2010 16:02'!
angle: theta
	angleCos _ theta cos.
	angleSin _ theta sin! !

!Morphic3RadialGradient methodsFor: 'construction' stamp: 'jmv 5/13/2010 15:44'!
center: c radiusX: radiusX radiusY: radiusY

	cx _ c x.
	cy _ c y.
	rx _ radiusX.
	ry _ radiusY! !

!Morphic3RadialGradient methodsFor: 'construction' stamp: 'jmv 11/4/2012 21:26'!
externalizedWith: aMatrixTransform2x3

	| p1 r a |
	p1 _ aMatrixTransform2x3 externalizePosition: cx@cy.
	r _ aMatrixTransform2x3 externalizeDelta: rx@ ry.
	r _ rx@ ry.
	a _ (aMatrixTransform2x3 externalizeDelta: 1@0) theta.
	"entender!!!!!!!!!!"
	^self copy center: p1 radiusX: r x radiusY: r y; angle: 0-a! !

!Morphic3RadialGradient methodsFor: 'services' stamp: 'jmv 5/13/2010 16:07'!
vectorFractionAtX: x y: y
	| f xx yy xxx yyy |

	"ver que precalcular, etc."
	xxx _ x - cx.
	yyy _ y-cy.
	xx _ xxx * angleCos - (yyy*angleSin).
	yy _ xxx * angleSin + (yyy*angleCos).
	f _ ((xx  / rx) @ (yy / ry)) r.
	f _ (f max: stopFractions first) min: stopFractions last.	"esto ultimo no deberia hacer falta (creo)"
	^f! !

!SVGDefElement methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:40'!
def: anSVGDef
	def _ anSVGDef! !

!SVGDefElement class methodsFor: 'instance creation' stamp: 'jmv 5/7/2010 14:50'!
classFor: anSVGName

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGName
			ifTrue: [ ^cls ] ].
	^nil! !

!SVGDefElement class methodsFor: 'instance creation' stamp: 'jmv 6/16/2010 10:44'!
fromXml: anXMLElement
	| cls n |
	n _ anXMLElement name.
	cls _ self classFor: n.
	cls ifNil: [
		((SVGElementMorph ignoredElements includes: n) or: [
			SVGElementMorph ignoredPrefixes anySatisfy: [ :prefix | n beginsWith: prefix ]]) ifFalse: [
			SVGWarning signal: ('Unhandled def element: ', n printString) ].
		^nil ].
	^ cls new buildFromXml: anXMLElement! !

!SVGGradient methodsFor: 'building' stamp: 'jmv 7/30/2012 18:00'!
buildFromXml: anXMLElement
	| unsorted stop dataStream usedAttributeNames |
	name _ anXMLElement attributeAt: 'id'.
	unsorted _ OrderedCollection new.
	anXMLElement elementsDo: [ :subElement |
		stop _ SVGDefElement fromXml: subElement.
		unsorted add: stop].
	"Esto deberia ser un array de entrada, arreglar (codigo de levente) sortBy: para que ande en array sin crear sorted collection ni nada"
	unsorted isEmpty
		ifFalse: [
			stops _ unsorted sorted: [ :a :b | a offset < b offset ]]
		ifTrue: [ stops _ nil ].

	inheritFrom _ nil.
	(anXMLElement attributeAt: 'xlink:href') ifNotNil: [ :linkData |
		dataStream _ linkData readStream.
		dataStream upTo: $#.
		inheritFrom _ (dataStream upTo: $) ) withBlanksTrimmed ].

	usedAttributeNames _ self usedAttributeNames asSet.
	anXMLElement attributes keysAndValuesDo: [ :attrName :value |
		((usedAttributeNames includes: attrName) or: [
			(SVGElementMorph ignoredAttributes includes: attrName) or: [
				SVGElementMorph ignoredPrefixes anySatisfy: [ :prefix | attrName beginsWith: prefix ]]])  ifFalse: [
			SVGWarning signal: 'Unhandled attribute: ', attrName printString, ' in: ', self class name, ' value: ', value printString ]]! !

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 5/13/2010 09:56'!
name
	^name! !

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
stops
	^stops
		ifNotNil: [ stops ]
		ifNil: [ (def defAt: inheritFrom) stops ]! !

!SVGGradient methodsFor: 'building' stamp: 'jmv 6/16/2010 09:52'!
usedAttributeNames
	^#('xlink:href' 'id')! !

!SVGGradient class methodsFor: 'instance creation' stamp: 'jmv 5/13/2010 10:02'!
svgName
	^nil! !

!SVGLinearGradient methodsFor: 'converting' stamp: 'jmv 6/16/2010 09:29'!
asMorphic3GradientIn: anSVGShape
"Ojo. El estandar dice que el default es userSpaceOnUse, pero safari (y otros?) usan objectBoundingBox,
que tiene esa extra~na particularidad de perpendicular en un espacio inventado para eso..."
	| answer minX minY maxX maxY x1 y1 x2 y2 u centerProjectionX centerProjectionY deltaX1 deltaY1 deltaX2 deltaY2 centerX centerY w h d1 d2 s somePerpendicular dd1 dd2 dd3 a x y st vectorX1z vectorX2z vectorY1z vectorY2z |
	
	minX _ nil.
	minY _ nil.
	maxX _ nil.
	maxY _ nil.
	anSVGShape privatePointsDo: [ :p |
		x _ p x. y _ p y.
		minX
			ifNil: [ minX _ x. minY _ y. maxX _ x. maxY _ y ]
			ifNotNil: [
				x < minX ifTrue: [ minX _ x ].
				y < minY ifTrue: [ minY _ y ].
				x > maxX ifTrue: [ maxX _ x ].
				y > maxY ifTrue: [ maxY _ y ]]].

	centerX _ minX + maxX * 0.5.
	centerY _ minY + maxY * 0.5.
	w _ maxX - minX.
	h _ maxY - minY.

	"Compute a parallel vector to the given one, but crossing 0.5@0.5"
	vectorX1z _ self vectorX1.
	vectorX2z _ self vectorX2.
	vectorY1z _ self vectorY1.
	vectorY2z _ self vectorY2.
	u _ ((0.5-vectorX1z) * (vectorX2z-vectorX1z)) + ((0.5-vectorY1z) * (vectorY2z-vectorY1z)) / 
		((vectorX2z-vectorX1z) squared + (vectorY2z-vectorY1z) squared).
	centerProjectionX _ vectorX2z-vectorX1z * u + vectorX1z.
	centerProjectionY _ vectorY2z-vectorY1z * u + vectorY1z.
	deltaX1 _ vectorX1z - centerProjectionX.
	deltaY1 _ vectorY1z - centerProjectionY.
	deltaX2 _ vectorX2z - centerProjectionX.
	deltaY2 _ vectorY2z - centerProjectionY.
	
	dd1 _ (deltaX1 )@(0 - deltaY1 ).		"invert y, to use standard angles"
	somePerpendicular _ dd1 rotateBy: 0-Float pi / 2.
	"multiply by w and h twice: once to restore previous angle, another to twist even more"
	dd2 _ (somePerpendicular x * w * w)@(somePerpendicular y * h * h).
	dd3 _ dd2 rotateBy: Float pi/2.

	a _ (dd1 theta -  dd3 theta).
	d1 _ deltaX1@deltaY1 rotateBy: a.
	d2 _ deltaX2@deltaY2 rotateBy: a.

	s _ 1 / ((d1 theta-(deltaX1 @deltaY1 ) theta)cos).
	d1 _ d1 * s.
	d2 _ d2 * s.
	
	x1 _ d1 x * w + centerX.
	y1 _ d1 y * h + centerY.
	x2 _ d2 x * w + centerX.
	y2 _ d2 y * h + centerY.

	st _ self stops.
	answer _ Morphic3LinearGradient new.
	answer
		vector: x1@y1 to: x2@y2;
		stopFractions: (st collect: [ :each | each offset]) stopColors: (st collect: [ :each | each color]).
	^answer! !

!SVGLinearGradient methodsFor: 'building' stamp: 'jmv 6/14/2010 17:23'!
buildFromXml: anXMLElement

	super buildFromXml: anXMLElement.
	vectorX1 _ (anXMLElement attributeAt: 'x1') ifNotNil: [ :data | SVGElementMorph svgLengthFractionFor: data ] ifNil: [ 0.0 ].
	vectorY1 _ (anXMLElement attributeAt: 'y1') ifNotNil: [ :data | SVGElementMorph svgLengthFractionFor: data ] ifNil: [ 0.0 ].
	vectorX2 _ (anXMLElement attributeAt: 'x2') ifNotNil: [ :data | SVGElementMorph svgLengthFractionFor: data ] ifNil: [ 1.0 ].
	vectorY2 _ (anXMLElement attributeAt: 'y2') ifNotNil: [ :data | SVGElementMorph svgLengthFractionFor: data ] ifNil: [ 0.0 ]! !

!SVGLinearGradient methodsFor: 'building' stamp: 'jmv 6/16/2010 09:53'!
usedAttributeNames
	^super usedAttributeNames,
		#('x1' 'y1' 'x2' 'y2')! !

!SVGLinearGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
vectorX1
	^vectorX1
		ifNotNil: [ vectorX1 ]
		ifNil: [ (def defAt: inheritFrom) vectorX1 ]! !

!SVGLinearGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
vectorX2
	^vectorX2
		ifNotNil: [ vectorX2 ]
		ifNil: [ (def defAt: inheritFrom) vectorX2 ]! !

!SVGLinearGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
vectorY1
	^vectorY1
		ifNotNil: [ vectorY1 ]
		ifNil: [ (def defAt: inheritFrom) vectorY1 ]! !

!SVGLinearGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
vectorY2
	^vectorY2
		ifNotNil: [ vectorY2 ]
		ifNil: [ (def defAt: inheritFrom) vectorY2 ]! !

!SVGLinearGradient class methodsFor: 'instance creation' stamp: 'jmv 5/7/2010 14:51'!
svgName
	^#linearGradient! !

!SVGRadialGradient methodsFor: 'converting' stamp: 'jmv 6/15/2010 15:32'!
asMorphic3GradientIn: anSVGShape
"Ojo. El estandar dice que el default es userSpaceOnUse, pero safary (y otros?) usan objectBoundingBox,
que tiene esa extra~na particularidad de perpendicular en un espacio inventado para eso..."
	| answer minX minY maxX maxY centerX centerY w h wh cxz cyz x y st rz |
	
	minX _ nil.
	minY _ nil.
	maxX _ nil.
	maxY _ nil.
	anSVGShape privatePointsDo: [ :p |
		x _ p x.
		y _ p y.
		minX
			ifNil: [ minX _ x. minY _ y. maxX _ x. maxY _ y ]
			ifNotNil: [
				x < minX ifTrue: [ minX _ x ].
				y < minY ifTrue: [ minY _ y ].
				x > maxX ifTrue: [ maxX _ x ].
				y > maxY ifTrue: [ maxY _ y ]]].

	centerX _ minX + maxX * 0.5.
	centerY _ minY + maxY * 0.5.
	w _ maxX - minX.
	h _ maxY - minY.
	
	"Simple gradient. just circular, not elliptic."
	wh _ w min: h.
	cxz _ wh * self cx + centerX - (wh*0.5).
	cyz _ wh * self cy + centerY - (wh*0.5).

	st _ self stops.
	rz _ self r.
	answer _ Morphic3RadialGradient new.
	answer
		center: cxz@cyz radiusX: w* rz radiusY: h*rz;
		stopFractions: (st collect: [ :each | each offset]) stopColors: (st collect: [ :each | each color]).
	^answer! !

!SVGRadialGradient methodsFor: 'building' stamp: 'jmv 6/14/2010 17:23'!
buildFromXml: anXMLElement

	| data |
	super buildFromXml: anXMLElement.
	data _ anXMLElement attributeAt: 'cx'.
	cx _ SVGElementMorph svgLengthFractionFor: (data ifNil: ['50%']).
	data _ anXMLElement attributeAt: 'cy'.
	cy _ SVGElementMorph svgLengthFractionFor: (data ifNil: ['50%']).
	data _ anXMLElement attributeAt: 'r'.
	r _ SVGElementMorph svgLengthFractionFor:(data ifNil: ['50%'])! !

!SVGRadialGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 09:04'!
cx
	^cx
		ifNotNil: [ cx ]
		ifNil: [ (def defAt: inheritFrom) cx ]! !

!SVGRadialGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 09:04'!
cy
	^cy
		ifNotNil: [ cy ]
		ifNil: [ (def defAt: inheritFrom) cy ]! !

!SVGRadialGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 09:04'!
r
	^r
		ifNotNil: [ r ]
		ifNil: [ (def defAt: inheritFrom) r ]! !

!SVGRadialGradient methodsFor: 'building' stamp: 'jmv 6/16/2010 09:53'!
usedAttributeNames
	^super usedAttributeNames,
		#('cx' 'cy' 'r')! !

!SVGRadialGradient class methodsFor: 'instance creation' stamp: 'jmv 5/13/2010 14:46'!
svgName
	^#radialGradient! !

!SVGGradientStop methodsFor: 'building' stamp: 'jmv 2/15/2013 16:53'!
buildFromXml: anXMLElement
	| styleAttributes offsetData stopColorData stopOpacityData usedAttributeNames |
	styleAttributes _ SVGElementMorph styleAttributesIn: anXMLElement.
	offsetData _ SVGElementMorph attributeAt: 'offset' in: anXMLElement orIn: styleAttributes.
	stopColorData _ SVGElementMorph attributeAt: 'stop-color' in: anXMLElement orIn: styleAttributes.
	stopOpacityData _ SVGElementMorph attributeAt: 'stop-opacity' in: anXMLElement orIn: styleAttributes.
	offset _ SVGElementMorph svgLengthFractionFor: offsetData.
self revisar.
	color _ Color "fromCSS2String: just hex? Or real CSS2?" fromString:  stopColorData.
	stopOpacityData ifNotNil: [
		color _ color alpha: stopOpacityData asNumber ].
	

	usedAttributeNames _ self usedAttributeNames asSet.
	anXMLElement attributes keysAndValuesDo: [ :attrName :value |
		((usedAttributeNames includes: attrName) or: [
			(SVGElementMorph ignoredAttributes includes: attrName) or: [
				SVGElementMorph ignoredPrefixes anySatisfy: [ :prefix | attrName beginsWith: prefix ]]])  ifFalse: [
			SVGWarning signal: 'Unhandled attribute: ', attrName printString, ' in: ', self class name, ' value: ', value printString ]].

	styleAttributes ifNotNil: [
		styleAttributes  keysAndValuesDo: [ :attrName :value |
			((usedAttributeNames includes: attrName) or: [
				(SVGElementMorph ignoredAttributes includes: attrName) or: [
					SVGElementMorph ignoredPrefixes anySatisfy: [ :prefix | attrName beginsWith: prefix ]]])  ifFalse: [
				SVGWarning signal: 'Unhandled style token: ', attrName printString, ' in: ', self class name, ' value: ', value printString ]]].! !

!SVGGradientStop methodsFor: 'accessing' stamp: 'jmv 5/10/2010 11:07'!
color
	^color! !

!SVGGradientStop methodsFor: 'accessing' stamp: 'jmv 5/7/2010 17:27'!
offset
	^offset! !

!SVGGradientStop methodsFor: 'building' stamp: 'jmv 6/16/2010 10:02'!
usedAttributeNames
	^#('style' 'offset' 'stop-color' 'stop-opacity')! !

!SVGGradientStop class methodsFor: 'instance creation' stamp: 'jmv 5/7/2010 15:14'!
svgName
	^#stop! !

!SVGDefs methodsFor: 'building' stamp: 'jmv 6/14/2010 09:00'!
buildFromXml: anXMLElement
	anXMLElement elementsDo: [ :subElement |
		(SVGDefElement fromXml: subElement) ifNotNil: [ :defElement |
			defElement def: self.
			defs at: defElement name put: defElement]]! !

!SVGDefs methodsFor: 'accessing' stamp: 'jmv 5/10/2010 10:16'!
defAt: aString

	^defs at: aString ifAbsent: nil! !

!SVGDefs methodsFor: 'initialize-release' stamp: 'jmv 5/10/2010 09:42'!
initialize
	defs _ Dictionary new! !

!SVGDefs class methodsFor: 'instance creation' stamp: 'jmv 5/7/2010 14:42'!
svgName
	"Answer the name of the svg elements we can represent"

	^#defs! !

!SVGPathCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:15'!
buildFrom: commandChar stream: aStream

	coordinatesAreRelative _ commandChar isLowercase! !

!SVGPathCommand methodsFor: 'building' stamp: 'jmv 5/4/2010 06:16'!
buildFromStream: aStream in: parentToBe

	coordinatesAreRelative _ aStream next isLowercase! !

!SVGPathCommand methodsFor: 'as yet unclassified' stamp: 'jmv 5/6/2011 22:18'!
coordinatesAreRelative
^coordinatesAreRelative! !

!SVGPathCommand methodsFor: 'private' stamp: 'jmv 5/11/2010 10:05'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	self subclassResponsibility! !

!SVGPathCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	self subclassResponsibility! !

!SVGPathCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:03'!
classFor: anSVGPathCommandName

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGPathCommandName asUppercase
			ifTrue: [ ^cls ] ].
	^nil! !

!SVGPathCommand class methodsFor: 'instance creation' stamp: 'jmv 6/16/2010 09:06'!
from: commandChar stream: aStream
	| cls|
	cls _ self classFor: commandChar.
	cls ifNil: [
		SVGError signal: ('Unhandled path command: 'copyWith: commandChar).
		^nil ].
	^cls new buildFrom: commandChar stream: aStream
	! !

!SVGPathCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:03'!
svgName
	"Answer the name of the svg elements we can represent"

	^self subclassResponsibility! !

!SVGPathArcCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:41'!
buildFrom: commandChar stream: aStream

	| rx ry xAxisRotation doLargerArc useIncreasingAngles x y |
	super buildFrom: commandChar stream: aStream.
	rs _ OrderedCollection new.
	xAxisRotations _ OrderedCollection new.
	toPoints _ OrderedCollection new.
	doLargerArcs _ OrderedCollection new.
	useIncreasingAngless _ OrderedCollection new.
	[
		rx _ aStream nextNumber.
		ry _ aStream nextNumber.
		xAxisRotation _ aStream nextNumber.
		doLargerArc _ (aStream nextNumber = 0) not.
		useIncreasingAngles _ (aStream nextNumber = 0) not.
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			rs add: rx@ry.
			xAxisRotations add: xAxisRotation degreesToRadians.
			doLargerArcs add: doLargerArc.
			useIncreasingAngless add: useIncreasingAngles.
			toPoints add: x@y ]! !

!SVGPathArcCommand methodsFor: 'private' stamp: 'jmv 6/15/2010 15:30'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

"si los puntos son relativos, tirar una excepcion...
Y completar los que falten!!"
self flag: #revisarM3.
	toPoints do: aBlock! !

!SVGPathArcCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	aCanvas pathArcCommand: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless! !

!SVGPathArcCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$A! !

!SVGPathCloseCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:10'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream! !

!SVGPathCloseCommand methodsFor: 'private' stamp: 'jmv 5/11/2010 10:05'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."! !

!SVGPathCloseCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	aCanvas pathCloseCommand! !

!SVGPathCloseCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$Z! !

!SVGPathCurveToCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 09:56'!
buildFrom: commandChar stream: aStream

	| x1 y1 x2 y2 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints _ OrderedCollection new.
	control1Points _ OrderedCollection new.
	control2Points _ OrderedCollection new.
	[
		x1 _ aStream nextNumber.
		y1 _ aStream nextNumber.
		x2 _ aStream nextNumber.
		y2 _ aStream nextNumber.
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			control1Points add: x1@y1.
			control2Points add: x2@y2 ]! !

!SVGPathCurveToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 09:57'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!SVGPathCurveToCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	aCanvas pathCurveToCommand: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points! !

!SVGPathCurveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$C! !

!SVGPathCurveToSmoothCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 09:59'!
buildFrom: commandChar stream: aStream

	| x2 y2 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints _ OrderedCollection new.
	control2Points _ OrderedCollection new.
	[
		x2 _ aStream nextNumber.
		y2 _ aStream nextNumber.
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			control2Points add: x2@y2 ]! !

!SVGPathCurveToSmoothCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:01'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!SVGPathCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	aCanvas pathCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints control2Points: control2Points! !

!SVGPathCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$S! !

!SVGPathLineToCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:33'!
buildFrom: commandChar stream: aStream

	| px py |
	super buildFrom: commandChar stream: aStream.
	points _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !

!SVGPathLineToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 09:32'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."
	"Atencion: Si son relativas todo esto esta mal... Igual, lo que quiero es reimplementar todo"

	points do: aBlock! !

!SVGPathLineToCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathLineToCommand: coordinatesAreRelative points: points! !

!SVGPathLineToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$L! !

!SVGPathLineToHCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:45'!
buildFrom: commandChar stream: aStream

	| px |
	super buildFrom: commandChar stream: aStream.
	xs _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		px notNil ] whileTrue:
			[ strm nextPut: px ]]! !

!SVGPathLineToHCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathLineToHCommand: coordinatesAreRelative xs: xs! !

!SVGPathLineToHCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$H! !

!SVGPathLineToVCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:45'!
buildFrom: commandChar stream: aStream

	| py |
	super buildFrom: commandChar stream: aStream.
	ys _ Array streamContents: [ :strm |
		[py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: py ]]! !

!SVGPathLineToVCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathLineToVCommand: coordinatesAreRelative ys: ys! !

!SVGPathLineToVCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$V! !

!SVGPathMoveToCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:34'!
buildFrom: commandChar stream: aStream

	| px py |
	super buildFrom: commandChar stream: aStream.
	points _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !

!SVGPathMoveToCommand methodsFor: 'private' stamp: 'jmv 6/11/2010 14:34'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	points do: aBlock! !

!SVGPathMoveToCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathMoveToCommand: coordinatesAreRelative points: points! !

!SVGPathMoveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$M! !

!SVGPathQuadraticCurveToCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:12'!
buildFrom: commandChar stream: aStream

	| x1 y1 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints _ OrderedCollection new.
	controlPoints _ OrderedCollection new.
	[
		x1 _ aStream nextNumber.
		y1 _ aStream nextNumber.
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			controlPoints add: x1@y1 ]! !

!SVGPathQuadraticCurveToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:14'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!SVGPathQuadraticCurveToCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathQuadraticCurveToCommand: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints! !

!SVGPathQuadraticCurveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$Q! !

!SVGPathQuadraticCurveToSmoothCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:15'!
buildFrom: commandChar stream: aStream

	| x y |
	super buildFrom: commandChar stream: aStream.
	toPoints _ OrderedCollection new.
	[
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y]! !

!SVGPathQuadraticCurveToSmoothCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:17'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!SVGPathQuadraticCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:10'!
runOn: aCanvas

	aCanvas pathQuadraticCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints! !

!SVGPathQuadraticCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:09'!
svgName

	^$T! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
applyMatrixWith: dataStream

	| b |
	b _ FloatMatrix identity: 3.
	b
		i: 1 j: 1 put: dataStream nextNumber;
		i: 2 j: 1 put: dataStream nextNumber;
		i: 1 j: 2 put: dataStream nextNumber;
		i: 2 j: 2 put: dataStream nextNumber;
		i: 1 j: 3 put: dataStream nextNumber;
		i: 2 j: 3 put: dataStream nextNumber.
	a _ a
		ifNil: [b]
		ifNotNil: [a * b]! !

!SVGTransform methodsFor: 'conversion' stamp: 'jmv 4/29/2011 18:59'!
asMatrixTransform2x3
	| answer |
	a ifNil: [ ^nil ].
	answer _ MatrixTransform2x3 new.
	answer a11: (a i: 1 j: 1).
	answer a12: (a i: 1 j: 2).
	answer a13: (a i: 1 j: 3).
	answer a21: (a i: 2 j: 1).
	answer a22: (a i: 2 j: 2).
	answer a23: (a i: 2 j: 3).
	^answer! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 7/30/2012 18:00'!
buildFrom: transformData

	| dataStream type |
	dataStream _ transformData readStream.
	[ dataStream atEnd ] whileFalse: [
		dataStream upToAny: 'mtsr'. "find the next transform in the list"
		type _ (dataStream upTo: $() withBlanksTrimmed.
		(type = 'matrix') ifTrue: [
			self applyMatrixWith: dataStream ].
		(type = 'translate') ifTrue: [
			self translateWith: dataStream ].
		(type = 'scale') ifTrue: [
			self scaleWith: dataStream ].
		(type = 'rotate') ifTrue: [
			self rotateWith: dataStream ].
		(type = 'skewX') ifTrue: [
			self skewXWith: dataStream ].
		(type = 'skewY') ifTrue: [
			self skewYWith: dataStream ]
	].! !

!SVGTransform methodsFor: 'initialize-release' stamp: 'jmv 4/29/2011 22:03'!
initialize
"	a _ FloatMatrix identity: 3."
	"nil means identity, i.e. no transformation"
	a _ nil.! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
rotateWith: dataStream

	| b b1 b2 angle angleCos angleSin |
	b _ FloatMatrix identity: 3.
	b1 _ FloatMatrix identity: 3.
	b2 _ FloatMatrix identity: 3.
	angle _ dataStream nextNumber degreesToRadians.
	angleCos _ angle cos.
	angleSin _ angle sin.
	b
		i: 1 j: 1 put: angleCos;
		i: 1 j: 2 put: 0.0 - angleSin;
		i: 2 j: 1 put: angleSin;
		i: 2 j: 2 put: angleCos.
	dataStream nextNumber ifNotNil: [ :cx |
		b1 i: 1 j: 3 put: cx.
		b2 i: 1 j: 3 put: 0.0-cx.
		dataStream nextNumber ifNotNil: [ :cy |
			b1 i: 2 j: 3 put: cy.
			b2 i: 2 j: 3 put: 0.0-cy ]].

	a _ a
		ifNil: [ b1 * b * b2 ]
		ifNotNil: [ a * b1 * b * b2]! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
scaleWith: dataStream

	| b sx sy |
	b _ FloatMatrix identity: 3.
	sx _ dataStream nextNumber.
	sy _ dataStream nextNumber ifNil: [ sx ].
	b i: 1 j: 1 put: sx.
	b i: 2 j: 2 put: sy.
	a _ a
		ifNil: [b]
		ifNotNil: [a * b]! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
skewXWith: dataStream

	| b skewAngle |
	b _ FloatMatrix identity: 3.
	skewAngle _ dataStream nextNumber degreesToRadians.
	b i: 1 j: 2 put: skewAngle tan.
	a _ a
		ifNil: [b]
		ifNotNil:[a * b]! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
skewYWith: dataStream

	| b skewAngle |
	b _ FloatMatrix identity: 3.
	skewAngle _ dataStream nextNumber degreesToRadians.
	b i: 2 j: 1 put: skewAngle tan.
	a _ a
		ifNil: [b]
		ifNotNil: [a * b]! !

!SVGTransform methodsFor: 'building' stamp: 'jmv 4/29/2011 22:03'!
translateWith: dataStream

	| b |
	b _ FloatMatrix identity: 3.
	b i: 1 j: 3 put: dataStream nextNumber.
	dataStream nextNumber ifNotNil: [ :dy |
		b i: 2 j: 3 put: dy ].
	a _ a
		ifNil: [b]
		ifNotNil: [a * b]! !
Morphic3Canvas initialize!
SVGElementMorph initialize!
