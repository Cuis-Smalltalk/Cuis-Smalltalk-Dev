'From Cuis 5.0 of 7 November 2016 [latest update: #3230] on 17 December 2017 at 6:12:10 pm'!
'Description Please enter a description for this package '!
!provides: 'Morphic3' 1 31!
!requires: 'LinearAlgebra' 1 nil nil!
!requires: 'OpenCL' 1 nil nil!
!requires: 'YAXO' 1 nil nil!
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

!classDefinition: #StrokeFont category: #'Morphic3-Text'!
AbstractFont subclass: #StrokeFont
	instanceVariableNames: 'name emphasis baseFont derivativeFonts pointSize characterToGlyphMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Text'!
!classDefinition: 'StrokeFont class' category: #'Morphic3-Text'!
StrokeFont class
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

!classDefinition: #VectorCanvas category: #'Morphic3-Kernel'!
MorphicCanvas subclass: #VectorCanvas
	instanceVariableNames: 'fontForeColor currentClientX currentClientY autoHintText engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'VectorCanvas class' category: #'Morphic3-Kernel'!
VectorCanvas class
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
	instanceVariableNames: 'stops name inheritFrom averageColor'
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

!classDefinition: #SVGTransform category: #'Morphic3-SVG'!
Object subclass: #SVGTransform
	instanceVariableNames: 'a'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-SVG'!
!classDefinition: 'SVGTransform class' category: #'Morphic3-SVG'!
SVGTransform class
	instanceVariableNames: ''!

!classDefinition: #VectorEngine category: #'Morphic3-Kernel'!
Object subclass: #VectorEngine
	instanceVariableNames: 'targetForm targetWidth targetHeight antiAliasingWidth subPixelDelta targetSubpixelAlphas targetSubpixelAlphasIfStrokesAndFillsWereOpaque strokeWidth strokeAlpha strokeRed strokeGreen strokeBlue fillAlpha fillRed fillGreen fillBlue doStroke doFill clipLeft clipTop clipRight clipBottom currentMorph currentOwnerIfClips boundsLeft boundsTop boundsRight boundsBottom morphAtPixel morphBoundsLeft morphBoundsRight morphBoundsTop morphBoundsBottom penAlpha edgeCountPerPixel hopsPerPixel auxBitBlt firstX firstY currentX currentY lastControl2Point trajectoryPoints fragmentStartIndexes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'VectorEngine class' category: #'Morphic3-Kernel'!
VectorEngine class
	instanceVariableNames: ''!

!classDefinition: #VectorFontRenderer category: #'Morphic3-Kernel'!
Object subclass: #VectorFontRenderer
	instanceVariableNames: 'vectorGraphicsCanvas vectorGraphicsEngine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'VectorFontRenderer class' category: #'Morphic3-Kernel'!
VectorFontRenderer class
	instanceVariableNames: ''!

!classDefinition: #BitstreamVeraRenderer category: #'Morphic3-Kernel'!
VectorFontRenderer subclass: #BitstreamVeraRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'BitstreamVeraRenderer class' category: #'Morphic3-Kernel'!
BitstreamVeraRenderer class
	instanceVariableNames: ''!

!classDefinition: #TechnicalFontRenderer category: #'Morphic3-Kernel'!
VectorFontRenderer subclass: #TechnicalFontRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'TechnicalFontRenderer class' category: #'Morphic3-Kernel'!
TechnicalFontRenderer class
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

!M3Exp01Morph commentStamp: 'jmv 12/11/2017 21:33:29' prior: 0!
| m |
m _ M3Exp01Morph new.
m morphPosition: 15@30.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.!

!M3Exp02Morph commentStamp: 'jmv 12/11/2017 21:37:40' prior: 0!
| m |
m _ M3Exp02Morph new.
m morphPosition: 50@30.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen

| m |
m _ M3Exp02Morph new.
m morphPosition: 50@30.
m addMorph: M3Exp03Morph new.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.

| m |
m _ M3Exp02Morph new.
m addMorph: M3Exp03Morph new.
m morphPosition: 50@30.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.!

!M3Exp03Morph commentStamp: '<historical>' prior: 0!
| m |
m _ M3Exp03Morph new.
m morphPosition: 50@20.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.

| m |
m _ M3Exp03Morph new.
m morphPosition: 53@20+40.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.

| m |
m _ M3Exp03Morph new.
m morphPosition: 53@20+40.
(VectorCanvas onForm: Display)
setClipRect: (40@40 extent: 100@100);
fullDraw: m.
Display forceToScreen.!

!VectorCanvas commentStamp: '<historical>' prior: 0!
A canvas specially built for Morphic 3.

(VectorCanvas onForm: Display) fullDraw: self runningWorld. Display writePNGfileNamed: 'm3z.png'

On any Morph:
| c |
c _ VectorCanvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self.
Display forceToScreen!

!VectorEngine commentStamp: 'jmv 12/16/2017 18:27:28' prior: 0!
A Vector Graphics rasterizer for Morphic 3.
Supports most SVG primitives.
Gives exceptional quality.

(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine displayCoordarcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine displayCoordarcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngine onForm: Display)
	resetMorphBounds;
	fillColor: Color red do: [ :engine |
		engine displayCoordarcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen!

!Color methodsFor: '*Morphic3' stamp: 'jmv 1/21/2015 12:48'!
externalizedWith: aMorphicLocation

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

!StrokeFont methodsFor: 'accessing' stamp: 'jmv 9/14/2014 21:38'!
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
	10 6 6 6 6 6 6 6 6 5 5 6 6 7 6 7 6 7 6 6 6 6 6 9 6 6 6 3 5 3 5 9 	"@, A - Z - _"
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

!SVGElementMorph methodsFor: 'building' stamp: 'jmv 1/21/2015 09:03'!
buildFromXml: anXMLElement

	| usedAttributeNames styleAttributes strokeDashArrayData strokeDashArrayOffsetData transformData svgTransform |
	styleAttributes _ self class styleAttributesIn: anXMLElement.
	
	"These are always nested. No need to inherit."
	svgTransform _ SVGTransform new.
	transformData _ self class attributeAt: 'transform' in: anXMLElement orIn: styleAttributes.
	transformData ifNotNil: [
		svgTransform buildFrom: transformData.
		location _ svgTransform asAffineTransformation ].

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

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:13:52'!
fillColor
	"Answer a Color, even if a gradient was specified. See also #fillColorOrGradient"
	fillColorOrGradient == #inherit
		ifTrue: [
			fillColorOrGradient _ (owner isKindOf: SVGElementMorph)
				ifTrue: [ owner fillColor ]
				ifFalse: [ Color black ]]
		ifFalse: [
			fillColorOrGradient isString ifTrue: [
				fillColorOrGradient _ self defAt: fillColorOrGradient ]].
	fillColorOrGradient ifNil: [ ^nil ].

	(fillColorOrGradient isKindOf: SVGGradient)
		ifTrue: [ fillColorOrGradient _ fillColorOrGradient averageColor ].
	^fillColorOrGradient alpha: self fillOpacity * self opacity! !

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

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:13:57'!
strokeColor
	"Answer a Color, even if a gradient was specified. See also #strokeColorOrGradient"
	strokeColorOrGradient == #inherit
		ifTrue: [
			strokeColorOrGradient _ (owner isKindOf: SVGElementMorph)
				ifTrue: [ owner strokeColor ]
				ifFalse: [ nil ]]
		ifFalse: [
			strokeColorOrGradient isString ifTrue: [
				strokeColorOrGradient _ self defAt: strokeColorOrGradient ]].
	strokeColorOrGradient ifNil: [ ^nil ].

	(strokeColorOrGradient isKindOf: SVGGradient) 
		ifTrue: [ strokeColorOrGradient _ strokeColorOrGradient averageColor].
	^strokeColorOrGradient alpha: self strokeOpacity * self opacity! !

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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:27:23'!
examples
"
SVGElementMorph examples
"
| m base |
base _ 'Cuis-Smalltalk-Dev/M3/SVGSamples/'.
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
	(VectorCanvas onForm: Display) fullDraw: m.
	Display forceToScreen.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	Display copy inspect.
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:24:25'!
examplesBeer
"
SVGElementMorph examplesBeer
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Experimental/SVGSamples/'.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'beer.svg'
		.
	m privateOwner: self runningWorld.
	m morphPosition: 800@300.
	(VectorCanvas onForm: Display) fullDraw: m.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	Display copy inspect.
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:24:33'!
examplesLion
"
SVGElementMorph examplesLion
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Experimental/SVGSamples/'.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'lionx.svg'
		.
	m privateOwner: self runningWorld.
	m morphPosition: 10@10.
	(VectorCanvas onForm: Display) fullDraw: m.
	Display forceToScreen.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	Display copy inspect.
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:27:26'!
examplesMagician
"
SVGElementMorph examplesMagician
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/M3/SVGSamples/'.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'magician.svg'
		.
	m privateOwner: self runningWorld.
	m morphPosition: 100@100.
	(VectorCanvas onForm: Display) fullDraw: m.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	Display copy inspect.
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:24:39'!
examplesTiger
"
SVGElementMorph examplesTiger
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Experimental/SVGSamples/'.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'tiger1.svg'
		.
	m privateOwner: self runningWorld.
	m morphPosition: 400@60.
	(VectorCanvas onForm: Display) fullDraw: m.
	m scaleBy: 0.6.
	m morphPosition: 200@00.
	(VectorCanvas onForm: Display) fullDraw: m.
	m scaleBy: 0.6.
	m morphPosition: 700@500.
	(VectorCanvas onForm: Display) fullDraw: m.
	m scaleBy: 0.6.
	m morphPosition: 800@680.
	(VectorCanvas onForm: Display) fullDraw: m.
	m scaleBy: 0.6.
	m morphPosition: 650@700.
	(VectorCanvas onForm: Display) fullDraw: m.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	Display copy inspect.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 5/30/2017 15:24:29'!
phoneExample
"
SVGElementMorph phoneExample
"
| m base |
base _ 'Cuis-Smalltalk-Dev/Experimental/SVGSamples/'.
Transcript clear.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, 'Phone.svg'.
	m privateOwner: self runningWorld.
	m rotation: 0 / 180 * Float pi scale: 0.5; morphPosition: 120.0@100.0.
	(VectorCanvas onForm: Display) fullDraw: m.
	m rotation:  -15 / 180 * Float pi scale: 0.75; morphPosition:  500.0@110.0.
	(VectorCanvas onForm: Display) fullDraw: m.
	Display copy inspect.
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

!SVGMainMorph methodsFor: 'building' stamp: 'jmv 1/12/2015 13:15'!
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
	self privatePosition: (w@h // 2)! !

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

!SVGCircle methodsFor: 'building' stamp: 'jmv 6/10/2010 09:21'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	cx _ (anXMLElement attributeAt: 'cx') asNumber.
	cy _ (anXMLElement attributeAt: 'cy') asNumber.
	r _ (anXMLElement attributeAt: 'r') asNumber! !

!SVGCircle methodsFor: 'drawing' stamp: 'jmv 12/12/2017 21:37:40'!
drawOn: aCanvas

	aCanvas strokeWidth: self strokeWidth color: self strokeColor fillColor: self fillColor do: [
		aCanvas
			ellipseCenterX: cx y: cy rx: r ry: r ]! !

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

!SVGEllipse methodsFor: 'drawing' stamp: 'jmv 12/12/2017 21:37:55'!
drawOn: aCanvas


	aCanvas strokeWidth: self strokeWidth color: self strokeColor fillColor: self fillColor do: [
		aCanvas
			ellipseCenterX: cx y: cy rx: rx ry: ry ]! !

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

!SVGPath methodsFor: 'drawing' stamp: 'jmv 12/12/2017 21:38:03'!
drawOn: aCanvas

	aCanvas strokeWidth: self strokeWidth color: self strokeColor fillColor: self fillColor do: [
		commands do: [ :command |
			command runOn: aCanvas ]]! !

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

!SVGPolyLine methodsFor: 'drawing' stamp: 'jmv 12/12/2017 21:38:14'!
drawOn: aCanvas

	aCanvas strokeWidth: self strokeWidth color: self strokeColor fillColor: self fillColor do: [
		aCanvas
			polyLine: points ]! !

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

!SVGRect methodsFor: 'drawing' stamp: 'jmv 12/12/2017 21:38:22'!
drawOn: aCanvas

	aCanvas strokeWidth: self strokeWidth color: self strokeColor fillColor: self fillColor do: [
		aCanvas
			roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: rx ry: ry ]! !

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

!M3Exp01Morph methodsFor: 'drawing' stamp: 'jmv 12/11/2017 21:34:45'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."


aCanvas strokeWidth: 4 color: (Color black "green alpha: 0.6") fillColor: (Color yellow alpha: 0.4) do: [
	aCanvas
		ellipseCenterX: 46 y: 46 rx: "24"16 ry: "12"16 ].

aCanvas strokeWidth: 18 color: (Color black "green alpha: 0.6") fillColor: (Color yellow alpha: 0.4) do: [
	aCanvas
		ellipseCenterX: 46 y: 46 rx: "24"36 ry: "12"36 ].

aCanvas fillColor: Color black do: [
	aCanvas
		polyLine: {15@32. 30@50. 45@20. 15@32} * 2.
	aCanvas
		polyLine: {60@32. 30@50. 45@20. 60@32} * 2 ].

aCanvas strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [
	aCanvas cubicBezier: {10@5. 15@30. 60@40. 60@10} ].

aCanvas strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [
	aCanvas cubicBezier: {10@5. 15@30. 60@60. 60@10} ].

aCanvas strokeWidth: 1.5 color: (Color red alpha: 0.7+0.3) do: [
	aCanvas quadraticBezier: { 10@5 . 15@30 . 60@10 } ].

aCanvas strokeWidth: 1 color: (Color green alpha: 0.7+0.3) do: [
	aCanvas quadraticBezier: { 10@5 . 60@60 . 60@10 } ].

aCanvas strokeWidth: 5 color: (Color green alpha: 0.6) fillColor: (Color red alpha: 0.3) do: [
	aCanvas roundRectangleMinX: -10 maxX: 80 minY: -10 maxY: 60 rx: 40 ry: 20 ]
! !

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 7/14/2014 14:28'!
clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ true! !

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 12/11/2017 21:08:39'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas strokeWidth: 15 color: (Color green alpha: 0.8) fillColor: (Color red alpha: 0.6) do: [
		aCanvas
			roundRectangleMinX: -10 maxX: 80 minY: -10 maxY: 60  rx: 40 ry: 20 ]! !

!M3Exp03Morph methodsFor: 'drawing' stamp: 'jmv 12/11/2017 21:25:49'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

true ifTrue: [
	aCanvas fillColor: (Color black alpha: 0.5) do: [
	aCanvas
		polyLine: {-10@32. 120@-30. 
"		60@20. 120@20. 120@10. 150@50. 120@70. 120@30. 80@30."
"		60@90. -50@32"
		60@90. -60@80. -60@20. -50@70. -10@32
		}
 ]].
false ifTrue: [
	aCanvas fillColor: Color black do: [
	aCanvas
		polyLine: {60@32. 30@50. 45@20. 60@32}
]].

false ifTrue: [
	aCanvas strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [
	aCanvas cubicBezier: {10@5. 15@30. 60@40. 60@10} ].
].

false ifTrue: [
	aCanvas strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [
	aCanvas cubicBezier: {10@5. 15@30. 60@60. 60@10} ].
].

false ifTrue: [
	aCanvas strokeWidth: 1.5 color: (Color red alpha: 0.7+0.3) do: [
	aCanvas quadraticBezier: { 10@5 . 15@30 . 60@10 } ].
].

false ifTrue: [
	aCanvas strokeWidth: 1 color: (Color red alpha: 0.7+0.3) do: [
	aCanvas quadraticBezier: { 10@5 . 60@60 . 60@10 } ].
]! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 9/30/2014 20:02'!
cDx: dx dy: dy
	"Curve comes from an horizontal segment, and is followed by a vertical segment"
	"Does not autoHint y"

	self
		curveToX: currentClientX+dx y: currentClientY+dy
		control1X: currentClientX+(dx*0.5) y: currentClientY 
		control2X: currentClientX+dx y: currentClientY+(dy*0.5).
	currentClientX _ currentClientX+dx.
	currentClientY _ currentClientY+dy! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 9/30/2014 20:02'!
cDy: dy dx: dx
	"Curve comes from a vertical segment, and is followed by an horizontal segment"
	"Does not autoHint y"
	self
		curveToX: currentClientX+dx y: currentClientY+dy
		control1X: currentClientX y: currentClientY+(dy*0.5)
		control2X: currentClientX+(dx*0.5) y: currentClientY+dy.
	currentClientX _ currentClientX+dx.
	currentClientY _ currentClientY+dy! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:02'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:03'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.	
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:07'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ currentClientX + dr! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:08'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:10'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY - dr! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 09:00'!
clippingRectForCurrentMorph
	"In form coordinates"

	^engine clippingRectForCurrentMorph! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:12'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:14'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY + dr! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:16'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:18'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientY _ currentClientY - dr! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:19'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 17:58:26'!
cubicBezier: points
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	| start control1 control2 end |
	start _ currentTransformation externalizePosition: points first.
	control1 _ currentTransformation externalizePosition: points second.
	control2 _ currentTransformation externalizePosition: points third.
	end _ currentTransformation externalizePosition: points fourth.
	engine displayCoordmoveToX: start x y: start y.
	engine
		displayCoordcubicBezierToX: end x y: end y
		control1X: control1 x y: control1 y
		control2X: control2 x y: control2 y! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:43'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:45'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.	
	currentClientX _ x3.
	currentClientY _ y3! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 12/17/2017 17:58:47'!
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
	engine displayCoordcubicBezierToX: p3 x y: displayY3 control1X: p1 x y: displayY1 control2X: p2 x y: displayY2.
	currentClientX _ currentClientX + dr! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 16:17'!
currentOwnerIfClips: currentMorphOwnerOrNil
	engine currentOwnerIfClips: currentMorphOwnerOrNil! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 17:58:54'!
curveToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	| p3 p1 p2 |
	p3 _ currentTransformation externalizePosition: x3@y3.
	p1 _ currentTransformation externalizePosition: x1@y1.
	p2 _ currentTransformation externalizePosition: x2@y2.
	engine displayCoordcubicBezierToX: p3 x y: p3 y control1X: p1 x y: p1 y control2X: p2 x y: p2 y! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:25'!
d: dy

	| newY p2 displayY2 |
	newY _ currentClientY - dy.
	p2 _ currentTransformation externalizePosition: currentClientX@newY.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	engine displayCoordlineToX: p2 x y: displayY2.
	currentClientY _ newY! !

!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/11/2017 22:15:07'!
displayString: aString from: startIndex to: stopIndex at: aPoint strokeFont: font
	| ascii destX destY displayWidth ps isBold scaledPS wheight isItalic char renderer |

	engine antiAliasingWidth: self class antiAliasingWidthForStrokeFonts.

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
	"NECESITAMOS pasar el strokeWidth en coordenadas nuestras!!!!!!!!!!!!"
	renderer _ TechnicalFontRenderer new vectorGraphicsCanvas: self.
	engine strokeWidth: wheight color: fontForeColor do: [
		startIndex to: stopIndex do: [ :charIndex |
			char _ aString at: charIndex.
			ascii _ char asciiValue.
			displayWidth _ font widthOf: char.
			renderer drawCharCode: ascii atX: destX y: destY pointSize: ps italics: isItalic.
			destX _ destX + displayWidth.
		].
	].
	autoHintText _ false.
	engine antiAliasingWidth: self class defaultAntiAliasingWidth! !

!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/11/2017 22:15:38'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: font
	| ascii destX destY displayWidth ps isBold isItalic char renderer |

	engine antiAliasingWidth: self class antiAliasingWidthForOutlineFonts.

	destY _ aPoint y.
	destX _ aPoint x.
	ps _ font pointSize.
	isBold _ font isBold.
	isItalic _ font isItalic.

	renderer _ BitstreamVeraRenderer new vectorGraphicsCanvas: self.
	engine strokeWidth: (isBold ifTrue: [0.7] ifFalse: [0]) color: fontForeColor fillColor: fontForeColor do: [
		startIndex to: stopIndex do: [ :charIndex |
			char _ aString at: charIndex.
			ascii _ char asciiValue.
			displayWidth _ font widthOf: char.
			renderer drawCharCode: ascii atX: destX y: destY pointSize: ps * 1.3 italics: isItalic.
			destX _ destX + displayWidth.
		]
	].
	engine antiAliasingWidth: self class defaultAntiAliasingWidth! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 9/30/2014 20:02'!
drawButtonIconFromCurrentMorph

	currentMorph iconName ifNotNil: [ :selector |
		self perform: selector.
		^true ].
	^false! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:12'!
drawCloseIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:22'!
drawCollapseIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:26'!
drawDownIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:31'!
drawExpandIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:35'!
drawLeftIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:40'!
drawMenuIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:45'!
drawRightIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/30/2014 20:02'!
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
	"do it better!!"
	font class = StrokeFont ifTrue: [
		self
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			strokeFont: font ]
	ifFalse: [
		self
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			trueTypeFont: font ]! !

!VectorCanvas methodsFor: 'drawing-icons' stamp: 'jmv 12/11/2017 22:07:49'!
drawUpIcon

self strokeWidth: 1.0 color: Color black do: [
	self
		m: 12 y: 12;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9.
]! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 18:01:45'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry
	"
	DEJAR FINALMENTE unicamente metodos que tomen la currentTransformation. NADA en coordenadas de pantalla. NADA con una M2x3 argumento!!
	"
	| dx dy rx ry phi center |
	self revisar.
	
	center _ currentTransformation externalizePosition: mcx@mcy.
	dx _ currentTransformation externalizeDelta: mrx@0.
	dy _ currentTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	engine
		displayCoordarcCenterX: center x centerY: center y radiusX: rx radiusY: ry rotationAngle: phi start: 0.0 sweep:  Float pi * 2;
		closePath! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/11/2017 21:15:33'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: morphStrokeWidth borderColor: strokeColorOrGradient fillColor: fillColorOrGradient

	
	self strokeWidth: morphStrokeWidth color: strokeColorOrGradient fillColor: fillColorOrGradient do: [
		self ellipseCenterX: mcx y: mcy rx: mrx ry: mry ]! !

!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 10/14/2014 11:02'!
engine
	^engine! !

!VectorCanvas methodsFor: 'trajectories - init' stamp: 'jmv 12/10/2017 22:53:52'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	engine fillColor: aFillColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/10/2017 22:59:51'!
fillRectangle: aRectangle color: aColor
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"

	self fillColor: aColor do: [
		self rectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 ]! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/10/2017 23:17:50'!
fillRectangle: aRectangle color: aColor borderWidth: morphStrokeWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"

	| brColor tlColor d1 d2 |
	d1 _ morphStrokeWidth*0.5-0.5.
	d2 _ morphStrokeWidth*0.5+0.5.
	tlColor _ baseColorForBorder.
	aSymbol == #raised ifTrue: [
		tlColor _ baseColorForBorder quiteWhiter.
		brColor _ baseColorForBorder quiteBlacker ].

	aSymbol == #inset ifTrue: [
		tlColor _ baseColorForBorder quiteBlacker.
		brColor _ baseColorForBorder quiteWhiter ].

	self strokeWidth: morphStrokeWidth color: tlColor fillColor: aColor do: [
		self rectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+d1 maxY: aRectangle bottom-d2 ].

	brColor ifNotNil: [
		self strokeWidth: morphStrokeWidth color: brColor do: [
			self line: aRectangle bottomLeft+(d1@(0-d2)) to: aRectangle bottomRight-d2.
			self line: aRectangle bottomRight-d2 to: aRectangle topRight+(0-d2@d1) ]
	]! !

!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 9/30/2014 20:02'!
fontForeColor: aColor
	fontForeColor _ aColor! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self halt! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/11/2017 20:57:09'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	| d1 d2 |
	d1 _ borderWidth*0.5-0.5.
	d2 _ borderWidth*0.5+0.5.

	self strokeWidth: borderWidth color: borderColor do: [
		self rectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+d1 maxY: aRectangle bottom-d2 ]! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 10/9/2014 09:51'!
image: aForm at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
self flag: #jmvVer4.
" 	self halt"
"super image: aForm at: aPoint"! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
image: aForm multipliedBy: aColor at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
"no!!!!!!!!"
"	self halt"
super image: aForm multipliedBy: aColor at: aPoint! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 9/30/2014 20:03'!
initialize
	super initialize.
	autoHintText _ false! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 16:14'!
into: aMorph
	super into: aMorph.
	engine currentMorph: currentMorph.
	engine resetMorphBounds! !

!VectorCanvas methodsFor: 'testing' stamp: 'jmv 5/30/2017 15:28:19'!
isCurrentMorphVisible
	self flag: #jmvVer2. "Also #morphBoundsInWorld"
self revisar. "por supuesto que no..."
	^ true! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:25'!
l: dx

	| x2 y2 p2 displayY2 |
	x2 _ currentClientX - dx.
	y2 _ currentClientY.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	engine displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x2! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:25'!
li: x y: y
	"no confundir con l: !!!!!!!!"
	
	| p2 displayY2 |
	p2 _ currentTransformation externalizePosition: x@y.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	engine displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x.
	currentClientY _ y! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/10/2017 23:16:18'!
line: pt1 to: pt2

	| p p2 |
	p _ currentTransformation externalizePosition: pt1.
	engine displayCoordmoveToX: p x y: p y.
	p2 _ currentTransformation externalizePosition: pt2.
	engine displayCoordlineToX: p2 x y: p2 y! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/11/2017 20:53:48'!
line: pt1 to: pt2 width: morphStrokeWidth color: aStrokeColor

	self strokeWidth: morphStrokeWidth color: aStrokeColor do: [
		self line: pt1 to: pt2 ]! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/11/2017 20:45:03'!
lineToX: x2 y: y2
	| p2 |
	p2 _ currentTransformation externalizePosition: x2@y2.
	engine displayCoordlineToX: p2 x y: p2 y! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:27'!
m: x y: y

	| p  displayY |
	p _ currentTransformation externalizePosition: x@y.
	displayY _ p y.
	autoHintText ifTrue: [
		displayY _ displayY rounded ].
	engine displayCoordmoveToX: p x y: displayY.
	currentClientX _ x.
	currentClientY _ y! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/11/2017 20:45:07'!
moveToX: x y: y
	| p |
	p _ currentTransformation externalizePosition: x@y.
	engine displayCoordmoveToX: p x y: p y! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/14/2014 16:14'!
outOfMorph
	super outOfMorph.
	engine currentMorph: currentMorph.! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathArcCommand: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless

	| to rxp ryp xAxisRotationP p0 r |
	p0 _ coordinatesAreRelative ifTrue: [ engine currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ]].
	
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

		engine
			displayCoordarcToX: to x y: to y
			radiusX: rxp radiusY: ryp
			angleOfXAxis: xAxisRotationP
			largeFlag: (doLargerArcs at: i)
			sweepFlag: (useIncreasingAngless at: i) ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 12/17/2017 18:01:49'!
pathCloseCommand

	engine closePath! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 12/17/2017 17:59:04'!
pathCurveToCommand: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points

	| p0 to c1 c2 |
	p0 _ coordinatesAreRelative ifTrue: [ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
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
		engine displayCoordcubicBezierToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 12/17/2017 17:59:06'!
pathCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints control2Points: control2Points

	| p0 to c1 c2 |
	p0 _ engine currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ].
	c2 _ engine lastControl2Point ifNil: [ p0].
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
		engine displayCoordcubicBezierToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathLineToCommand: coordinatesAreRelative points: points
	| p |
	p _ coordinatesAreRelative ifTrue: [ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	points do: [ :each |
		p _ coordinatesAreRelative
			ifTrue: [ p + (currentTransformation externalizeDelta: each) ]
			ifFalse: [ currentTransformation externalizePosition: each ].
		engine displayCoordlineToX: p x y: p y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathLineToHCommand: coordinatesAreRelative xs: xs

	| p internalP |
	p _ engine currentPoint ifNil: [  currentTransformation externalizePosition: 0.0@0.0 ].
	coordinatesAreRelative ifFalse: [
		internalP _ currentTransformation internalizeDelta: p].
	xs do: [ :each |
		coordinatesAreRelative
			ifTrue: [ p _ p + (currentTransformation externalizeDelta: each@0) ]
			ifFalse: [
				internalP _ each @internalP y.
				p _  currentTransformation externalizePosition: internalP ].
		engine displayCoordlineToX: p x y: p y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathLineToVCommand: coordinatesAreRelative ys: ys

	| p internalP |
	p _ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ].
	coordinatesAreRelative ifFalse: [
		internalP _ currentTransformation internalizeDelta: p].
	ys do: [ :each |
		coordinatesAreRelative
			ifTrue: [ p _ p + (currentTransformation externalizeDelta: 0@each) ]
			ifFalse: [
				internalP _ internalP x @ each.
				p _  currentTransformation externalizePosition: internalP ].
		engine displayCoordlineToX: p x y: p y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathMoveToCommand: coordinatesAreRelative points: points

	| p isFirst |
	isFirst _ true.
	p _ coordinatesAreRelative ifTrue: [ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
	points do: [ :each |
		p _ coordinatesAreRelative
			ifTrue: [ p + (currentTransformation externalizeDelta: each) ]
			ifFalse: [ currentTransformation externalizePosition: each ].
		isFirst
			ifTrue: [ engine displayCoordmoveToX: p x y: p y ]
			ifFalse: [ engine displayCoordlineToX: p x y: p y ].
		isFirst _ false ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 10:57'!
pathQuadraticCurveToCommand: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints

	| p0 to c |
	p0 _ coordinatesAreRelative ifTrue: [ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ]].
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
		engine displayCoordquadraticBezierCurveToX: to x y: to y controlX: c x y: c y ]! !

!VectorCanvas methodsFor: 'trajectories - path commands - morph coordinates' stamp: 'jmv 10/14/2014 11:00'!
pathQuadraticCurveToSmoothCommand: coordinatesAreRelative toPoints: toPoints

	| p0 to c |
	p0 _ engine currentPoint ifNil: [ currentTransformation externalizePosition: 0.0@0.0 ].
	c _ engine lastControl2Point ifNil: [ p0].
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
		engine displayCoordquadraticBezierCurveToX: to x y: to y controlX: c x y: c y ]! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 17:26:25'!
polyLine: points

	| p dp |
	dp _ currentTransformation externalizePosition: points first.
	engine displayCoordmoveToX: dp x y: dp y.
	2 to: points size do: [ :i |
		p _ points at: i.	
		dp _ currentTransformation externalizePosition: p.
		engine displayCoordlineToX: dp x y: dp y ]! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 17:34:24'!
quadraticBezier: points
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	| start control end |
	start _ currentTransformation externalizePosition: points first.
	control _ currentTransformation externalizePosition: points second.
	end _ currentTransformation externalizePosition: points third.
	engine displayCoordmoveToX: start x y: start y.
	engine
		displayCoordquadraticBezierCurveToX: end x y: end y
		controlX: control x y: control y! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:25'!
r: dx

	| x2 y2 p2 displayY2 |
	x2 _ currentClientX + dx.
	y2 _ currentClientY.
	p2 _ currentTransformation externalizePosition: x2@y2.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	engine displayCoordlineToX: p2 x y: displayY2.
	currentClientX _ x2! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 18:01:51'!
rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax

	| p1 p2 p3 p4 |

	p1 _ currentTransformation externalizePosition: xMin@yMin.
	p2 _ currentTransformation externalizePosition: xMax@yMin.
	p3 _ currentTransformation externalizePosition: xMax@yMax.
	p4 _ currentTransformation externalizePosition: xMin@yMax.
	
	engine
		displayCoordmoveToX: p1 x y: p1 y;
		displayCoordlineToX: p2 x y: p2 y;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordlineToX: p4 x y: p4 y;
		closePath! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
reverseRectangleBorder: r borderWidth: borderWidth
	self halt! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 18:01:53'!
roundBottomRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p5 p6 p7 p8 rx ry phi doLargerArc useIncreasingAngles |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

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
	engine
		displayCoordmoveToX: corner1 x y: corner1 y ;
		displayCoordlineToX: corner2 x y: corner2 y;
		displayCoordlineToX: p5 x y: p5 y;
		displayCoordarcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p7 x y: p7 y;
		displayCoordarcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		closePath! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/11/2017 21:10:33'!
roundRect: aRectangle color: aColor radius: r
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 rx: r ry: r ]! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self halt! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self halt! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 18:01:55'!
roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 p5 p6 p7 p8 rx ry phi doLargerArc useIncreasingAngles |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

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
	engine
		displayCoordmoveToX: p1 x y: p1 y ;
		displayCoordarcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordarcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p5 x y: p5 y;
		displayCoordarcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p7 x y: p7 y;
		displayCoordarcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		closePath! !

!VectorCanvas methodsFor: 'trajectories - morph coordinates' stamp: 'jmv 12/17/2017 18:01:58'!
roundTopRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 rx ry phi doLargerArc useIncreasingAngles |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

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
	engine
		displayCoordmoveToX: p1 x y: p1 y ;
		displayCoordarcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: p3 x y: p3 y;
		displayCoordarcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;
		displayCoordlineToX: corner3 x y: corner3 y;
		displayCoordlineToX: corner4 x y: corner4 y;
		closePath! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 10/13/2014 16:17'!
setClipRect: aRectangle
	"In form coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 12/16/2017 18:18:00'!
setForm: aForm
	"
	World extent: World extent
	"
	form _ aForm.
	engine _ VectorEngine onForm: form.
	self newClipRect: nil.
	shadowColor _ nil! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
stencil: stencilForm at: aPoint color: aColor
	self halt! !

!VectorCanvas methodsFor: 'trajectories - init' stamp: 'jmv 12/10/2017 23:10:14'!
strokeWidth: morphStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"
	| strokeWidth |

	strokeWidth _ currentTransformation externalizeScalar: morphStrokeWidth.
	engine strokeWidth: strokeWidth color: aStrokeColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'trajectories - init' stamp: 'jmv 12/10/2017 22:56:14'!
strokeWidth: morphStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"
	| strokeWidth |

	strokeWidth _ currentTransformation externalizeScalar: morphStrokeWidth.
	engine strokeWidth: strokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc
"ojo que es un quad, o sea un rect en coordenadas del morph..."
"	self halt"
super textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc! !

!VectorCanvas methodsFor: 'stroke font elements' stamp: 'jmv 10/14/2014 09:25'!
u: dy

	| newY p2 displayY2 |
	newY _ currentClientY + dy.
	p2 _ currentTransformation externalizePosition: currentClientX@newY.
	displayY2 _ p2 y.
	autoHintText ifTrue: [
		displayY2 _ displayY2 rounded ].
	engine displayCoordlineToX: p2 x y: displayY2.
	currentClientY _ newY! !

!VectorCanvas methodsFor: 'FormCanvas protocol' stamp: 'jmv 12/16/2017 18:32:06'!
windowFrame: aRectangle color: aColor radius: r border: morphStrokeWidth labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	VectorCanvas onForm: Display :: into: self runningWorld; windowFrame: (210@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"
	"
	| hbw |
	self strokeWidth: morphStrokeWidth color: aColor fillColor: insideColor do: [
		self roundRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom rx: r ry: r ].
	hbw _ morphStrokeWidth *0.5.
	self fillColor: aColor do: [
		self rectangleMinX: aRectangle left+hbw maxX: aRectangle right-hbw minY: aRectangle top+hbw maxY: aRectangle top + lh-hbw ].
	"

	| d1 d2 |
	d1 _ morphStrokeWidth*0.5-0.5.
	d2 _ morphStrokeWidth*0.5+0.5.

	self strokeWidth: morphStrokeWidth color: aColor fillColor: aColor do: [
		self 
			roundTopRectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+d1 maxY: aRectangle top+lh-d2 rx: r ry: r ].
	self strokeWidth: morphStrokeWidth color: aColor fillColor: insideColor do: [
		self
			roundBottomRectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+lh-d2 maxY: aRectangle bottom-d2 rx: r ry: r ]! !

!VectorCanvas class methodsFor: 'accessing' stamp: 'jmv 9/30/2014 20:03'!
antiAliasingWidthForOutlineFonts
	^1.4! !

!VectorCanvas class methodsFor: 'accessing' stamp: 'jmv 9/30/2014 20:03'!
antiAliasingWidthForStrokeFonts
	^1.1! !

!VectorCanvas class methodsFor: 'BitBltCanvas protocol' stamp: 'jmv 10/9/2014 23:04'!
arrowOfDirection: aSymbol size: finalSizeInteger
	self flag: #jmvVer2.! !

!VectorCanvas class methodsFor: 'BitBltCanvas protocol' stamp: 'jmv 10/9/2014 23:04'!
arrowWithGradientOfDirection: aSymbol
	"
	(VectorCanvas arrowWithGradientOfDirection: #up) display.
	(VectorCanvas arrowWithGradientOfDirection: #down) display.
	(VectorCanvas arrowWithGradientOfDirection: #left) display.
	(VectorCanvas arrowWithGradientOfDirection: #right) display.
	"
	self flag: #jmvVer2.! !

!VectorCanvas class methodsFor: 'BitBltCanvas protocol' stamp: 'jmv 10/9/2014 23:04'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"
	self flag: #jmvVer2.! !

!VectorCanvas class methodsFor: 'accessing' stamp: 'jmv 9/30/2014 20:03'!
defaultAntiAliasingWidth
	^1.3. "default for graphics"! !

!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 1/21/2015 08:41'!
initialize
	"
	VectorCanvas initialize
	0.33: r - b
	0.1: b - r
	"
	" AffineTransformation doIntegerTransformations: false"! !

!VectorCanvas class methodsFor: 'BitBltCanvas protocol' stamp: 'jmv 10/9/2014 23:04'!
steButtonForm: extent
	self flag: #jmvVer2.! !

!VectorCanvas class methodsFor: 'text experiments' stamp: 'jmv 10/14/2014 22:57'!
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
	(self onForm: Display)
			into: self runningWorld;
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	Display forceToScreen! !

!VectorCanvas class methodsFor: 'text experiments' stamp: 'jmv 12/11/2017 23:57:28'!
textex2
"Jugando con Paragraph"
"
self textex2
"
	| t para frame |
	
	t _ Text
		initialFont: (AbstractFont familyName: 'DejaVu' pointSize: 22)
		stringOrText: (String streamContents: [ :strm | (32 to: 255) do: [ :i | strm nextPut: (Character numericValue: i)]]).
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
	(self onForm: Display)
			into: self runningWorld;
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	Display forceToScreen! !

!VectorCanvas class methodsFor: 'text experiments' stamp: 'jmv 10/9/2014 09:42'!
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
	(self onForm: Display)
		into: self runningWorld;
		textComposition: para 
		bounds: (0@0 extent: Display extent) 
		color: Color black
		selectionColor: Color orange.
	Display forceToScreen! !

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

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 12/12/2017 21:40:16'!
averageColor
	| s |
	averageColor ifNotNil: [ ^ averageColor ].
	s _ self stops.
	averageColor _ (s collect: [ :c | c color / s size ]) sum.
	^ averageColor! !

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

!SVGTransform methodsFor: 'conversion' stamp: 'jmv 1/21/2015 09:03'!
asAffineTransformation
	| answer |
	a ifNil: [ ^nil ].
	answer _ AffineTransformation new.
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

!VectorEngine methodsFor: 'trajectories - aux' stamp: 'jmv 12/17/2017 18:08:09'!
addPenPositionX: x y: y
	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"

	trajectoryPoints add: x@y! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
antiAliasingWidth: aNumber

	antiAliasingWidth _ aNumber! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendFillOnly

	| redEdgesTotal greenEdgesTotal blueEdgesTotal redEdges greenEdges blueEdges isRedInside isGreenInside isBlueInside edgesPerPixelWord pixelIndex m |

	boundsTop to: boundsBottom do: [ :displayY |
		redEdgesTotal _ 0.
		greenEdgesTotal _ 0.
		blueEdgesTotal _ 0.
		pixelIndex _ displayY * targetWidth +  boundsLeft.
		boundsLeft to: boundsRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesPerPixelWord _ edgeCountPerPixel bytesAt: pixelIndex.
			redEdges _ edgesPerPixelWord at: 3.
			greenEdges _ edgesPerPixelWord at: 2.
			blueEdges _ edgesPerPixelWord at: 1.

			"Not needed in C or asm, use signed char, i.e. a type where 16rFF means -1 and not 255"
			redEdges > 127 ifTrue: [ redEdges _ redEdges - 256 ].
			greenEdges > 127 ifTrue: [ greenEdges _ greenEdges - 256 ].
			blueEdges > 127 ifTrue: [ blueEdges _ blueEdges - 256 ].

			redEdgesTotal _ redEdgesTotal + redEdges.
			greenEdgesTotal _ greenEdgesTotal + greenEdges.
			blueEdgesTotal _ blueEdgesTotal + blueEdges.
			isRedInside _ redEdgesTotal ~= 0.
			isGreenInside _ greenEdgesTotal ~= 0.
			isBlueInside _ blueEdgesTotal ~= 0.

			(currentOwnerIfClips isNil or: [
				m _ morphAtPixel at: pixelIndex.
				m = currentOwnerIfClips or: [m = currentMorph]]) ifTrue: [
				self
					blendFillOnlyAtX: displayX y: displayY pixelIndex: pixelIndex 
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
				"-----------------------"
				"y registrar el morph actual a este pixel!!, si es que lo pintamos... Mejorar. ver alpha."
				"en realidad, si el alpha usado para el verde es relevante, mayor a 0.2, p.ej...
				Ojo. Esto exige!! que los que contesten clipsSubmorphs, sean opacotes, sino...
				Uh, usar isGreenInside hace que se clippe a la mitad interna del borde... Además, no se podrá agarrar un morph de la mitad externa del borde...
				Se podria hacer que el blend conteste si estabamos adentro o no... Y simplemente usar eso...
				"
				"-----------------------"
				isGreenInside ifTrue: [
				"(auxAlphas at: pixelIndex) = 0 ifFalse: ["
					morphAtPixel at: pixelIndex put: currentMorph ]].
			]].
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: penAlpha.
	auxBitBlt copyBits.
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendFillOnlyAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends fill over background. Despite method selector, there might be a very thin stroke of the same color as the fill. Handle this by adjusting antiAliasAlphas.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha fillAlphaToUse |

	antiAliasAlphasWord _ penAlpha bytesAt: pixelIndex.
	antiAliasAlphasWord = "16rFF000000" #[16r00 16r00 16r00 16rFF] ifTrue: [
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	antiAliasRedAlpha _ (antiAliasAlphasWord at: 3) / 127.0.
	isRedInside ifTrue: [ antiAliasRedAlpha _ 1.0 - antiAliasRedAlpha ].
	antiAliasGreenAlpha _ (antiAliasAlphasWord at: 2) / 127.0.
	isGreenInside ifTrue: [ antiAliasGreenAlpha _ 1.0 - antiAliasGreenAlpha ].
	antiAliasBlueAlpha _ (antiAliasAlphasWord at: 1) / 127.0.
	isBlueInside ifTrue: [ antiAliasBlueAlpha _ 1.0 - antiAliasBlueAlpha ].
	
	targetForm depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			antiAliasGreenAlpha > 0.5 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"	 "Color black pixelValueForDepth: 1"
				"Atencion: Falla si el form no es potencia de 2 (anda bien 512x512). Ademas hardcodea que somos little endian. MEJORAR (y poner en el otro blend)"
				"bits _ targetForm bits.
				backgroundWord _ bits at: pixelIndex >> 5.
				backgroundWord _ backgroundWord bitOr: 1 << (31-(pixelIndex bitAnd: 31)).
				bits at: pixelIndex >> 5 put: backgroundWord."
				"Esto es super ineficiente pero parece andar bien. Mejorar..."
				targetForm pixelValueAt: (pixelIndex \\ targetWidth) @(pixelIndex // targetWidth) put: 1
			].
			^ self].

	fillAlphaToUse _ fillAlpha / 255.0.
	bits _ targetForm bits.
	targetWord _ bits bytesAt: pixelIndex.

	alpha _ antiAliasRedAlpha * fillAlphaToUse.
	resultR _ alpha * fillRed + ((1.0 - alpha) * (targetWord at: 3)).
	targetWord at: 3 put: resultR rounded.

	alpha _ antiAliasGreenAlpha * fillAlphaToUse.
	resultG _ alpha * fillGreen + ((1.0 - alpha) * (targetWord at: 2)).
	targetWord at: 2 put: resultG rounded.

	alpha _ antiAliasBlueAlpha * fillAlphaToUse.
	resultB _ alpha * fillBlue + ((1.0 - alpha) * (targetWord at: 1)).
	targetWord at: 1 put: resultB rounded.

	bits bytesAt: pixelIndex put: targetWord.! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendStrokeAndFill

	| redEdgesTotal greenEdgesTotal blueEdgesTotal redEdges greenEdges blueEdges isRedInside isGreenInside isBlueInside edgesPerPixelWord pixelIndex m |

	boundsTop to: boundsBottom do: [ :displayY |
		redEdgesTotal _ 0.
		greenEdgesTotal _ 0.
		blueEdgesTotal _ 0.
		pixelIndex _ displayY * targetWidth +  boundsLeft.
		boundsLeft to: boundsRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesPerPixelWord _ edgeCountPerPixel bytesAt: pixelIndex.
			redEdges _ edgesPerPixelWord at: 3.
			greenEdges _ edgesPerPixelWord at: 2.
			blueEdges _ edgesPerPixelWord at: 1.

			"Not needed in C or asm, use signed char, i.e. a type where 16rFF means -1 and not 255"
			redEdges > 127 ifTrue: [ redEdges _ redEdges - 256 ].
			greenEdges > 127 ifTrue: [ greenEdges _ greenEdges - 256 ].
			blueEdges > 127 ifTrue: [ blueEdges _ blueEdges - 256 ].

			redEdgesTotal _ redEdgesTotal + redEdges.
			greenEdgesTotal _ greenEdgesTotal + greenEdges.
			blueEdgesTotal _ blueEdgesTotal + blueEdges.
			isRedInside _ redEdgesTotal ~= 0.
			isGreenInside _ greenEdgesTotal ~= 0.
			isBlueInside _ blueEdgesTotal ~= 0.
			(currentOwnerIfClips isNil or: [
				m _ morphAtPixel at: pixelIndex.
				m = currentOwnerIfClips or: [m = currentMorph]]) ifTrue: [
				self
					blendStrokeAndFillAtX: displayX y: displayY pixelIndex: pixelIndex 
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.
				"-----------------------"
				"y registrar el morph actual a este pixel!!, si es que lo pintamos... Mejorar. ver alpha."
				"en realidad, si el alpha usado para el verde es relevante, mayor a 0.2, p.ej...
				Ojo. Esto exige!! que los que contesten clipsSubmorphs, sean opacotes, sino...
				Uh, usar isGreenInside hace que se clippe a la mitad interna del borde... Además, no se podrá agarrar un morph de la mitad externa del borde...
				Se podria hacer que el blend conteste si estabamos adentro o no... Y simplemente usar eso...
				"
				"-----------------------"
				isGreenInside ifTrue: [
				"(auxAlphas at: pixelIndex) = 0 ifFalse: ["
					morphAtPixel at: pixelIndex put: currentMorph ]].
			]].
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: penAlpha.
	auxBitBlt copyBits.
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendStrokeAndFillAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Do an appropriate (anti aliased) mix between stoke and fill (or pick just stroke or just fill). Blend this over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha foreComponent strokeAlphaToUse fillAlphaToUse |

	antiAliasAlphasWord _ penAlpha bytesAt: pixelIndex.
	antiAliasAlphasWord = "16rFF000000" #[16r00 16r00 16r00 16rFF] ifTrue: [
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasRedAlpha _ (antiAliasAlphasWord at: 3) / 127.0.
	antiAliasGreenAlpha _ (antiAliasAlphasWord at: 2) / 127.0.
	antiAliasBlueAlpha _ (antiAliasAlphasWord at: 1) / 127.0.
	
	targetForm depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			antiAliasGreenAlpha > 0.5 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"	 "Color black pixelValueForDepth: 1"
				"Atencion: Falla si el form no es potencia de 2 (anda bien 512x512). Ademas hardcodea que somos little endian. MEJORAR (y poner en el otro blend)"
				"bits _ targetForm bits.
				backgroundWord _ bits at: pixelIndex >> 5.
				backgroundWord _ backgroundWord bitOr: 1 << (31-(pixelIndex bitAnd: 31)).
				bits at: pixelIndex >> 5 put: backgroundWord."
				"Esto es super ineficiente pero parece andar bien. Mejorar..."
				targetForm pixelValueAt: (pixelIndex \\ targetWidth) @(pixelIndex // targetWidth) put: 1
			].
			^ self].

	strokeAlphaToUse _ strokeAlpha / 255.0.
	fillAlphaToUse _ fillAlpha / 255.0.
	bits _ targetForm bits.
	targetWord _ bits bytesAt: pixelIndex.

	isRedInside
		ifFalse: [							"Blend stroke over background"
			alpha _ antiAliasRedAlpha * strokeAlphaToUse.
			foreComponent _ strokeRed ]
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha _ antiAliasRedAlpha * strokeAlphaToUse + ((1-antiAliasRedAlpha) * fillAlphaToUse).
			foreComponent _ antiAliasRedAlpha * strokeRed + ((1-antiAliasRedAlpha) * fillRed) ].
	resultR _ alpha * foreComponent + ((1.0 - alpha) * (targetWord at: 3)).
	targetWord at: 3 put: resultR rounded.

	isGreenInside
		ifFalse: [							"Blend stroke over background"
			alpha _ antiAliasGreenAlpha * strokeAlphaToUse.
			foreComponent _ strokeGreen ]
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha _ antiAliasGreenAlpha * strokeAlphaToUse + ((1-antiAliasGreenAlpha) * fillAlphaToUse).
			foreComponent _ antiAliasGreenAlpha * strokeGreen + ((1-antiAliasGreenAlpha) * fillGreen) ].
	resultG _ alpha * foreComponent + ((1.0 - alpha) * (targetWord at: 2)).
	targetWord at: 2 put: resultG rounded.

	isBlueInside
		ifFalse: [							"Blend stroke over background"
			alpha _ antiAliasBlueAlpha * strokeAlphaToUse.
			foreComponent _ strokeBlue ]
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha _ antiAliasBlueAlpha * strokeAlphaToUse + ((1-antiAliasBlueAlpha) * fillAlphaToUse).
			foreComponent _ antiAliasBlueAlpha * strokeBlue + ((1-antiAliasBlueAlpha) * fillBlue) ].
	resultB _ alpha * foreComponent + ((1.0 - alpha) * (targetWord at: 1)).
	targetWord at: 1 put: resultB rounded.

	bits bytesAt: pixelIndex put: targetWord.! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendStrokeOnly

	| pixelIndex m |
	boundsTop to: boundsBottom do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  boundsLeft.
		boundsLeft to: boundsRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
				(currentOwnerIfClips isNil or: [
				m _ morphAtPixel at: pixelIndex.
				m = currentOwnerIfClips or: [m = currentMorph]]) ifTrue: [
					self blendStrokeOnlyAtX: displayX y: displayY pixelIndex: pixelIndex ]]].

	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: penAlpha.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'trajectories - private' stamp: 'jmv 12/16/2017 18:17:50'!
blendStrokeOnlyAtX: x y: y pixelIndex: pixelIndex

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha strokeAlphaToUse |

	antiAliasAlphasWord _ penAlpha bytesAt: pixelIndex.
	antiAliasAlphasWord = "16rFF000000" #[16r00 16r00 16r00 16rFF] ifTrue: [
		^self].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasRedAlpha _ (antiAliasAlphasWord at: 3) / 127.0.
	antiAliasGreenAlpha _ (antiAliasAlphasWord at: 2) / 127.0.
	antiAliasBlueAlpha _ (antiAliasAlphasWord at: 1) / 127.0.

	targetForm depth = 1  "We only support depths 32 and 1, right now"
		ifTrue: [
			antiAliasGreenAlpha > 0.5 ifTrue: [	"Take somewhat opaque pixels. Consider only green alpha"	"Ojo, habria que tomar el alpha del fill o border!!!!"	 "Color black pixelValueForDepth: 1"
				"Atencion: Falla si el form no es potencia de 2 (anda bien 512x512). Ademas hardcodea que somos little endian. MEJORAR (y poner en el otro blend)"
				"bits _ targetForm bits.
				backgroundWord _ bits at: pixelIndex >> 5.
				backgroundWord _ backgroundWord bitOr: 1 << (31-(pixelIndex bitAnd: 31)).
				bits at: pixelIndex >> 5 put: backgroundWord."
				"Esto es super ineficiente pero parece andar bien. Mejorar..."
				targetForm pixelValueAt: (pixelIndex \\ targetWidth) @(pixelIndex // targetWidth) put: 1
			].
			^ self].

	strokeAlphaToUse _ strokeAlpha / 255.0.
	bits _ targetForm bits.
	targetWord _ bits bytesAt: pixelIndex.

	alpha _ antiAliasRedAlpha * strokeAlphaToUse.
	resultR _ alpha * strokeRed + ((1.0 - alpha) * (targetWord at: 3)).
	targetWord at: 3 put: resultR rounded.

	alpha _ antiAliasGreenAlpha * strokeAlphaToUse.
	resultG _ alpha * strokeGreen + ((1.0 - alpha) * (targetWord at: 2)).
	targetWord at: 2 put: resultG rounded.

	alpha _ antiAliasBlueAlpha * strokeAlphaToUse.
	resultB _ alpha * strokeBlue + ((1.0 - alpha) * (targetWord at: 1)).
	targetWord at: 1 put: resultB rounded.

	bits bytesAt: pixelIndex put: targetWord.! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
clipRect: aRectangle 
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In form coordinates"
	clipLeft _ aRectangle left.
	clipTop _ aRectangle top.
	clipRight _ aRectangle right - 1.
	clipBottom _ aRectangle bottom - 1! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
clippingRectForCurrentMorph
	"In form coordinates"

	^morphBoundsLeft @ morphBoundsTop corner: morphBoundsRight @ morphBoundsBottom! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 18:04:20'!
closePath
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordarcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			closePath ].
Display forceToScreen
"
	self displayCoordlineToX: firstX y: firstY! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
currentMorph: aMorph
	currentMorph _ aMorph! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
currentOwnerIfClips: currentMorphOwnerOrNil
	currentOwnerIfClips _ currentMorphOwnerOrNil! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
currentPoint

	currentX ifNil: [ ^nil ].
	^currentX@currentY! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 18:08:20'!
displayCoordarcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine displayCoordarcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.
"
	| d hops angle thetaCos thetaSin xp yp x0 y0 x y |
	
	"Por supuesto probar tambien rx no igual a ry, y uno (o ambos) menor que el ancho de trazo..."
	thetaCos _ theta cos.
	thetaSin _ theta sin.
	angle _ startAngle.
	xp _ angle cos * rx.
	yp _ angle sin * ry.
	x0 _ thetaCos * xp - (thetaSin * yp) + cx.
	y0 _ thetaSin * xp + (thetaCos * yp) + cy.
	firstX
		ifNotNil: [ self displayCoordlineToX: x0 y: y0 ]	"Draw a line. There are cases where we want to join an arc. For example, window frames"
		ifNil: [
			"trajectory starts here. Eventually, #closePath should draw a line to here"
			fragmentStartIndexes add: trajectoryPoints size + 1.	"Next point will be first in a new trajectory fragment"
			firstX _ x0.
			firstY _ y0.
			self addPenPositionX: x0 y: y0 ].
	hops _ ((rx+ry * 4 * sweepAngle abs / (2*Float pi)) * hopsPerPixel) truncated + 1.
	d _ sweepAngle / hops.
	hops timesRepeat: [
		angle _ angle + d.
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		x _ thetaCos * xp - (thetaSin * yp) + cx.
		y _ thetaSin * xp + (thetaCos * yp) + cy.
		self addPenPositionX: x y: y ].

	currentX _ x.
	currentY _ y.
	lastControl2Point _ nil! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 17:56:46'!
displayCoordarcToX: x2 y: y2 radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordarcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			displayCoordmoveToX: 100 y: 100;
			displayCoordarcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"
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
	

	self displayCoordarcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: phi start: theta1 sweep: deltaTheta! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 18:08:27'!
displayCoordcubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordcubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine displayCoordmoveToX: 100 y: 50.
		engine
			displayCoordcubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ].
Display forceToScreen
	"

	| dx dy hops increment t x0 y0 oneLessT f2 f3 f1 f23 f4 x y |

	dx _ (x1-currentX) abs + (x2-x1) abs + (x3-x2) abs.
	dy _ (y1-currentY) abs + (y2-y1) abs + (y3-y2) abs.
	hops _ ((dx max: dy) * hopsPerPixel)truncated + 1.
	increment _ 1.0 / hops.
	t _ 0.0.
	"en muchos casos, secuencias de estos,  repetimos puntos, el ultimo de la curva anterior... pensar, etc"
	x0 _ currentX.
	y0 _ currentY.
	self addPenPositionX: x0 y: y0.
	hops-1 timesRepeat: [
		t _ t + increment.
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * x0) + (f2 * x1) + (f3 * x2) + (f4 * x3).
		y _ (f1 * y0) + (f2 * y1) + (f3 * y2) + (f4 * y3).
		self addPenPositionX: x y: y ].
	self addPenPositionX: x3 y: y3.

	currentX _ x3.
	currentY _ y3.
	lastControl2Point _ x2@y2! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 18:08:24'!
displayCoordlineToX: x2 y: y2
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordlineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine displayCoordmoveToX: 10 y: 5.
		engine displayCoordlineToX: 15 y: 30.
		engine displayCoordlineToX: 60 y: 10 ].
Display forceToScreen
"
	| dx dy hops incrementX incrementY x y |
	x _ currentX.
	y _ currentY.
	dx _ x2 - x.
	dy _ y2 - y.
	hops _ ((dx abs max: dy abs) * hopsPerPixel)truncated + 1.
	incrementX _ dx *1.0 / hops.
	incrementY _ dy *1.0 / hops.
	hops timesRepeat: [
		self addPenPositionX: x y: y.
		x _ x + incrementX.
		y _ y + incrementY ].
	self addPenPositionX: x2 y: y2.

	currentX _ x2.
	currentY _ y2.
	lastControl2Point _ nil! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 17:56:29'!
displayCoordmoveToX: x y: y
"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordlineToX: 200 y: 150;
			displayCoordmoveToX: 210 y: 150;
			displayCoordlineToX: 300 y: 200 ].
Display forceToScreen
"

	"A discontinuity in the trajectory"

	"We are starting a new trajectory, or at a discontinuity in trajectory. Eventually #closeTrajectory should draw a line to here"
	fragmentStartIndexes add: trajectoryPoints size + 1.	"Next point will be first in a new trajectory fragment"
	firstX _ x.
	firstY _ y.
	currentX _ x.
	currentY _ y.

	lastControl2Point _ nil! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 12/17/2017 18:08:29'!
displayCoordquadraticBezierCurveToX: x2 y: y2 controlX: x1 y: y1
	"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			displayCoordmoveToX: 100 y: 100;
			displayCoordquadraticBezierCurveToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	resetMorphBounds;
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine displayCoordmoveToX: 10 y: 5.
		engine displayCoordquadraticBezierCurveToX: 60 y: 10 controlX: 15 y: 30 ].
Display forceToScreen
	"
	| dx dy hops increment t x0 y0 oneLessT f1 f3 f2 x y |
	dx _ (x1-currentX) abs + (x2-x1) abs.
	dy _ (y1-currentY) abs + (y2-y1) abs.
	hops _ ((dx max: dy) * hopsPerPixel)truncated + 1.
	increment _ 1.0 / hops.
	t _ 0.0.
	x0 _ currentX.
	y0 _ currentY.
	self addPenPositionX: x0 y: y0.
	hops-1 timesRepeat: [
		t _ t + increment.
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2* oneLessT * t.
		f3 _ t * t.
		x _ (f1 * x0) + (f2 * x1) + (f3 * x2).
		y _ (f1 * y0) + (f2 * y1) + (f3 * y2).
		self addPenPositionX: x y: y ].
	self addPenPositionX: x2 y: y2.

	currentX _ x2.
	currentY _ y2.
	lastControl2Point _ x1@y1! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 12/17/2017 18:06:35'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	doStroke _ false.
	doFill _ true.
	strokeWidth _ 0.0.
	hopsPerPixel _ 8.										"See #experimentHopsPerPixel3"

	"Done with stroke width. Now prepare colors."
	fillAlpha _ (aFillColor alpha * 255.0) rounded.
	fillRed _ (aFillColor red * 255.0) rounded.
	fillGreen _ (aFillColor green * 255.0) rounded.
	fillBlue _ (aFillColor blue * 255.0) rounded.
	strokeAlpha _ nil. strokeRed _ nil. strokeGreen _ nil. strokeBlue _ nil.

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/17/2017 18:11:09'!
finishPath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| prevTruncatedY dilatedHalfWidth fragmentEndIndex pp |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	doFill ifTrue: [
		(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
			"This is actually an error condition. If we are asked to fill, then the path should be closed.
			This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
			self closePath ]].

	"Current trajectory finished"

	"Compute edges intersecting with this horizontal line, for fills."
	doFill ifTrue: [
		fragmentStartIndexes withNextDo: [ :fragmentStartIndex :nextFragmentStartOrNil |
			fragmentEndIndex _ nextFragmentStartOrNil ifNotNil: [ :ns | ns-1 ] ifNil: [ trajectoryPoints size ].
			prevTruncatedY _ nil.
			fragmentStartIndex to: fragmentEndIndex do: [ :index |
				pp _ trajectoryPoints at: index.
				prevTruncatedY _ self updateEdgeCountAtX: pp x y: pp y prevTruncatedY: prevTruncatedY ]]].

	"calcular bounds"
	boundsTop _ targetHeight.			"drawable bottom. Will later be refined."
	boundsBottom _ 0.						"drawable top. Will later be refined."
	boundsLeft _ targetWidth.				"drawable right. Will later be refined."
	boundsRight _ 0.						"drawable left. Will later be refined."
	trajectoryPoints do: [ :p |
		boundsTop _ boundsTop min: p y.
		boundsBottom _ boundsBottom max: p y.
		boundsLeft _ boundsLeft min: p x.
		boundsRight _ boundsRight max: p x.
		].
	"Compute affected rect. Honor clipRect"
	dilatedHalfWidth _ strokeWidth + antiAliasingWidth * 0.5.
	boundsTop _ (boundsTop -dilatedHalfWidth) truncated +1.
	boundsTop < clipTop ifTrue: [ boundsTop _ clipTop ].
	boundsBottom _ (boundsBottom +dilatedHalfWidth) truncated.
	boundsBottom > clipBottom ifTrue: [ boundsBottom _ clipBottom ].
	boundsLeft _ (boundsLeft -dilatedHalfWidth -subPixelDelta) truncated +1.
	boundsLeft< clipLeft ifTrue: [ boundsLeft _ clipLeft ].
	boundsRight _ (boundsRight +dilatedHalfWidth +subPixelDelta) truncated.
	boundsRight > clipRight ifTrue: [ boundsRight _ clipRight ].

	trajectoryPoints do: [ :p | self updateAlphaX: p x y: p y ].

	firstX _ nil.
	firstY _ nil.
	currentX _ nil.
	currentY _ nil.
	trajectoryPoints _ OrderedCollection new.
	lastControl2Point _ nil.
	
	morphBoundsTop _ morphBoundsTop min: boundsTop.
	morphBoundsBottom _ morphBoundsBottom max: boundsBottom.
	morphBoundsLeft _ morphBoundsLeft min: boundsLeft.
	morphBoundsRight _ morphBoundsRight max: boundsRight.

	doFill ifTrue: [
		^doStroke
			ifTrue: [ self blendStrokeAndFill. Display forceToScreen ]
			ifFalse: [ self blendFillOnly. Display forceToScreen ]].

	self blendStrokeOnly.
	Display forceToScreen! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/17/2017 18:06:04'!
initializePath

	firstX _ nil.
	firstY _ nil.
	currentX _ nil.
	currentY _ nil.
	trajectoryPoints _ OrderedCollection new.
	fragmentStartIndexes _ OrderedCollection new.
	lastControl2Point _ nil! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
lastControl2Point

	^lastControl2Point! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/16/2017 18:17:50'!
resetMorphBounds

	morphBoundsTop _ targetHeight - 1.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ targetWidth - 1.
	morphBoundsRight _ 0! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/16/2017 18:17:50'!
setForm: aForm
	"
	World extent: World extent
	"
	| hack s |
	targetForm _ aForm.
	targetWidth _ aForm width.
	targetHeight _ aForm height.
	 self clipRect: (0@0 corner: targetWidth@targetHeight).
	antiAliasingWidth _ VectorCanvas defaultAntiAliasingWidth.
	subPixelDelta _ 0.25. "Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"
	s _ aForm bits size.
	(penAlpha isNil or: [ penAlpha size ~= s ])
		ifTrue: [
			penAlpha _ WordArray new: s.
			edgeCountPerPixel _ WordArray new: s.
			morphAtPixel _ Array new: s.
			targetSubpixelAlphas _ nil.
			targetSubpixelAlphasIfStrokesAndFillsWereOpaque _ nil.
			hack _ Form extent: targetWidth@targetHeight depth: 32 bits: penAlpha.
			auxBitBlt _ BitBlt toForm: hack.
			auxBitBlt fillBitmap: (Bitmap with: 16rFF000000).
			auxBitBlt combinationRule: Form over ].

	"Perhaps this should be done in an ifCurtailed: block in case of Morphic drawing failure..."
	auxBitBlt destRect: targetForm boundingBox.
	auxBitBlt destForm bits: penAlpha.
	auxBitBlt copyBits.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	morphAtPixel atAllPut: 0.
	targetSubpixelAlphas ifNotNil: [
		auxBitBlt destForm bits: targetSubpixelAlphas.
		auxBitBlt copyBits.
		auxBitBlt destForm bits: targetSubpixelAlphasIfStrokesAndFillsWereOpaque.
		auxBitBlt copyBits ]! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 12/17/2017 18:06:37'!
strokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	| strokeAlphaFactor |
	doStroke _ true.
	doFill _ false.

	"Strokes we want to show. They are not too thin, or maybe they are, but there's no fill. Make them wide enough to be properly sampled."
	strokeWidth _ aStrokeWidth max: antiAliasingWidth.
	strokeAlphaFactor _ aStrokeWidth / strokeWidth.
	hopsPerPixel _ 10.0 / strokeWidth + 1.		"See #experimentHopsPerPixel2"

	"Done with stroke width. Now prepare colors."
	fillAlpha _ nil. fillRed _ nil. fillGreen _ nil. fillBlue _ nil.
	strokeAlpha _ (aStrokeColor alpha * 255.0 * strokeAlphaFactor) rounded.
	strokeRed _ (aStrokeColor red * 255.0) rounded.
	strokeGreen _ (aStrokeColor green * 255.0) rounded.
	strokeBlue _ (aStrokeColor blue * 255.0) rounded.

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath! !

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 12/17/2017 18:06:39'!
strokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	aStrokeColor ifNil: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aFillColor ifNil: [ ^ self strokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		self strokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock.
		^ self ].

	doStroke _ true.
	doFill _ true.
	strokeWidth _ aStrokeWidth.
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"Strokes are really thin, but strokeColor is same as fillColor, so there's no problem sampling them."
			hopsPerPixel _ 8 ]								"See #experimentHopsPerPixel1"
		ifFalse: [
			"Strokes are not too thin. Make them wide enough to be properly sampled."
			hopsPerPixel _ 10.0 / strokeWidth + 1 ].		"See #experimentHopsPerPixel2"

	"Done with stroke width. Now prepare colors."
	fillAlpha _ (aFillColor alpha * 255.0) rounded.
	fillRed _ (aFillColor red * 255.0) rounded.
	fillGreen _ (aFillColor green * 255.0) rounded.
	fillBlue _ (aFillColor blue * 255.0) rounded.
	strokeAlpha _ (aStrokeColor alpha * 255.0) rounded.
	strokeRed _ (aStrokeColor red * 255.0) rounded.
	strokeGreen _ (aStrokeColor green * 255.0) rounded.
	strokeBlue _ (aStrokeColor blue * 255.0) rounded.

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath! !

!VectorEngine methodsFor: 'trajectories - aux' stamp: 'jmv 12/17/2017 18:11:01'!
updateAlphaX: x y: y

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| dilatedHalfWidth erodedHalfWidth distanceToAxis alpha t b l r constant1 alphaWord doUpdate dySquared dx dxSquared dy pixelIndex dxp |

	dilatedHalfWidth _ strokeWidth + antiAliasingWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
	erodedHalfWidth _ strokeWidth - antiAliasingWidth * 0.5.			"Podria ir como ivar... o al menos como arg"
	constant1 _ 127.0 / antiAliasingWidth.

	"Compute affected rect. Honor clipRect"
	t _ (y -dilatedHalfWidth) truncated +1.
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y +dilatedHalfWidth) truncated.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x -dilatedHalfWidth -subPixelDelta) truncated +1.
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x +dilatedHalfWidth +subPixelDelta) truncated.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  l.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaWord _ penAlpha bytesAt: pixelIndex.
			alphaWord = "16rFF7F7F7F" #[16r7F 16r7F 16r7F 16rFF] ifFalse: [
				doUpdate _ false.
				dy _ displayY - y.
				dySquared _ dy * dy.
				dx _ displayX - x.
				"Red"
				dxp _ dx - subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxis _ (dxSquared + dySquared) sqrt.
				distanceToAxis < dilatedHalfWidth
					ifTrue: [
						alpha _ distanceToAxis <= erodedHalfWidth
							ifTrue: [ 16r7F ]
							ifFalse: [ (dilatedHalfWidth - distanceToAxis * constant1) rounded ].
						alpha > (alphaWord at: 3) ifTrue: [
							alphaWord at: 3 put: alpha.
							doUpdate _ true ]].
				"Green"
				dxSquared _ dx * dx.
				distanceToAxis _ (dxSquared + dySquared) sqrt.
				distanceToAxis < dilatedHalfWidth
					ifTrue: [
						alpha _ distanceToAxis <= erodedHalfWidth
							ifTrue: [ 16r7F ]
							ifFalse: [ (dilatedHalfWidth - distanceToAxis * constant1) rounded ].
						alpha > (alphaWord at: 2) ifTrue: [
							alphaWord at: 2 put: alpha.
							doUpdate _ true ]].
				"Blue"
				dxp _ dx + subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxis _ (dxSquared + dySquared) sqrt.
				distanceToAxis < dilatedHalfWidth
					ifTrue: [
						alpha _ distanceToAxis <= erodedHalfWidth
							ifTrue: [ 16r7F ]
							ifFalse: [ (dilatedHalfWidth - distanceToAxis * constant1) rounded ].
						alpha > (alphaWord at: 1) ifTrue: [
							alphaWord at: 1 put: alpha.
							doUpdate _ true ]].
				doUpdate ifTrue: [
					penAlpha bytesAt: pixelIndex put: alphaWord ]]]]! !

!VectorEngine methodsFor: 'trajectories - aux' stamp: 'jmv 12/17/2017 18:09:07'!
updateEdgeCountAtX: x y: y prevTruncatedY: prevTruncatedY
	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex increment countWord redCount greenCount blueCount redOffset greenOffest blueOffset |

	thisYTruncated _ (y truncated max: clipTop-1) min: clipBottom-1.
	prevTruncatedY ifNil: [ 
		^ thisYTruncated ].
	thisYTruncated = prevTruncatedY ifTrue: [
		^ thisYTruncated ].

	thisYTruncated > prevTruncatedY
		ifTrue: [
			pixelY _ thisYTruncated.
			increment _ 1 ]
		ifFalse: [
			pixelY _ prevTruncatedY.
			increment _ -1 ].

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase _ pixelY * targetWidth + 1.
	redOffset _ (x+subPixelDelta) truncated + 1 max: clipLeft.
	greenOffest _ x truncated + 1 max: clipLeft.
	blueOffset _ (x-subPixelDelta) truncated + 1 max: clipLeft.
	redPixelIndex _ pixelIndexBase + redOffset.
	greenPixelIndex _ pixelIndexBase + greenOffest.
	bluePixelIndex _ pixelIndexBase + blueOffset.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord _ edgeCountPerPixel bytesAt: redPixelIndex.
				redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
				countWord at: 3 put: redCount.
				greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
				countWord at: 2 put: greenCount.
				blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
				countWord at: 1 put: blueCount.
				edgeCountPerPixel bytesAt: redPixelIndex put: countWord ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in following"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCountPerPixel bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						edgeCountPerPixel bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCountPerPixel bytesAt: bluePixelIndex.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCountPerPixel bytesAt: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the following"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCountPerPixel bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						edgeCountPerPixel bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCountPerPixel bytesAt: bluePixelIndex.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCountPerPixel bytesAt: bluePixelIndex put: countWord ].
				]
		].
	^thisYTruncated! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:01'!
experLinesAndShapes
	"
	self experLinesAndShapes
	"
	| engine y1 yy2 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	Display forceToScreen.
	engine strokeWidth: VectorCanvas defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 220 y: 10;
			displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
			displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
			closePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorCanvas defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		yy2 _ y2 >= 1 ifTrue: [ VectorCanvas defaultAntiAliasingWidth * 0.5 ] ifFalse: [0].
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 431 y: y1+yy2; displayCoordlineToX: 530 y: y1+yy2+dy;
				displayCoordlineToX: 530 y: y1+y2-yy2+dy; displayCoordlineToX: 431 y: y1+y2-yy2;
				closePath.
			engine
				displayCoordmoveToX:  y1+yy2 y: 431; displayCoordlineToX: y1+yy2+dy y: 530;
				displayCoordlineToX: y1+y2-yy2+dy y: 530; displayCoordlineToX: y1+y2-yy2 y: 431;
				closePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
				displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
				displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
				closePath ].
		"Solo fill."
		engine fillColor: Color black do: [
			engine
				displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
				displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
				displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
				closePath ].
		"solo stroke, no fill"
		engine strokeWidth: y2 color: Color black do: [
			engine
				displayCoordmoveToX: y2*0.5+731 y: y2*0.5 + y1+dy+dy+dy; displayCoordlineToX: 830 y: y2 * 0.5 + y1  + dy + dy+dy+dy.
			engine
				displayCoordmoveToX: y2*0.5 + y1+dy+dy+dy y: y2*0.5+731; displayCoordlineToX: y2 * 0.5 + y1  + dy + dy+dy+dy y: 830 ].
		y1 _ y1 + y2 + VectorCanvas defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:52'!
experLinesAndShapes2
	"
	self experLinesAndShapes2
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorCanvas defaultAntiAliasingWidth*4*2.
	15 timesRepeat: [
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 1.0 color: Color green fillColor: Color red do: [
			engine
				displayCoordmoveToX: 131 y: y1; displayCoordlineToX: 230 y: y1+dy;
				displayCoordlineToX: 230 y: y1+y2+dy; displayCoordlineToX: 131 y: y1+y2;
				closePath.
			engine
				displayCoordmoveToX: y1 y: 131; displayCoordlineToX: y1+dy y: 230;
				displayCoordlineToX: y1+y2+dy y: 230;displayCoordlineToX: y1+y2 y: 131;
				closePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.8 color: Color green fillColor: Color red do: [
			engine
				displayCoordmoveToX: 231 y: y1+dy; displayCoordlineToX: 330 y: y1+dy+dy;
				displayCoordlineToX: 330 y: y1+y2+dy+dy; displayCoordlineToX: 231 y: y1+y2+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy y: 231; displayCoordlineToX: y1+dy+dy y: 330;
				displayCoordlineToX: y1+y2+dy+dy y: 330; displayCoordlineToX: y1+y2+dy y: 231;
				closePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.6 color: Color green fillColor: Color red do: [.
			engine
				displayCoordmoveToX: 331 y: y1+dy+dy; displayCoordlineToX: 430 y: y1+dy+dy+dy;
				displayCoordlineToX: 430 y: y1+y2+dy+dy+dy; displayCoordlineToX: 331 y: y1+y2+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy y: 331; displayCoordlineToX: y1+dy+dy+dy y: 430;
				displayCoordlineToX: y1+y2+dy+dy+dy y: 430; displayCoordlineToX: y1+y2+dy+dy y: 331;
				closePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.4 color: Color green fillColor: Color red do: [
			engine
				displayCoordmoveToX: 431 y: y1+dy+dy+dy; displayCoordlineToX: 530 y: y1+dy+dy+dy+dy;
				displayCoordlineToX: 530 y: y1+y2+dy+dy+dy+dy; displayCoordlineToX: 431 y: y1+y2+dy+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy+dy y: 431; displayCoordlineToX: y1+dy+dy+dy+dy y: 530;
				displayCoordlineToX: y1+y2+dy+dy+dy+dy y: 530; displayCoordlineToX: y1+y2+dy+dy+dy y: 431;
				closePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.2 color: Color green fillColor: Color red do: [
			engine
				displayCoordmoveToX: 531 y: y1+dy+dy+dy+dy; displayCoordlineToX: 630 y: y1+dy+dy+dy+dy+dy;
				displayCoordlineToX: 630 y: y1+y2+dy+dy+dy+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy+dy+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy+dy+dy y: 531; displayCoordlineToX: y1+dy+dy+dy+dy+dy y: 630;
				displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy y: 630; displayCoordlineToX: y1+y2+dy+dy+dy+dy y: 531;
				closePath ].
		"Solo fill. w chico. Preferimos ya usarlo para definir mejor el shape (se transparentaria demasiado)"
		engine strokeWidth: 0.0 color: Color green fillColor: Color red do: [
			engine
				displayCoordmoveToX: 631 y: y1+dy+dy+dy+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy+dy+dy+dy;
				displayCoordlineToX: 730 y: y1+y2+dy+dy+dy+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy+dy+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy+dy+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy+dy+dy+dy y: 730;
				displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy+dy+dy+dy y: 631;
				closePath ].
		y1 _ y1 + y2 + VectorCanvas defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:54'!
experLinesAndShapes3
	"
	self experLinesAndShapes3
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	Display forceToScreen.
	engine strokeWidth: VectorCanvas defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 220 y: 10;
			displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
			displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
			closePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorCanvas defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth*0.4 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 431 y: y1; displayCoordlineToX: 530 y: y1+dy;
				displayCoordlineToX: 530 y: y1+y2+dy; displayCoordlineToX: 431 y: y1+y2;
				closePath.
			engine
				displayCoordmoveToX:  y1 y: 431; displayCoordlineToX: y1+dy y: 530;
				displayCoordlineToX: y1+y2+dy y: 530; displayCoordlineToX: y1+y2 y: 431;
				closePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth*0.2 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
				displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
				displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
				closePath ].
		"Solo fill. w =0"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth*0.0 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
				displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
				displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
				closePath ].
		y1 _ y1 + y2 + VectorCanvas defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:56'!
experLinesAndShapes4
	"
	self experLinesAndShapes4
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	Display forceToScreen.
	engine strokeWidth: VectorCanvas defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 220 y: 10;
			displayCoordlineToX: 410 y: 15; displayCoordlineToX: 220 y: 20;
			displayCoordlineToX: 215 y: 410; displayCoordlineToX: 210 y: 20;
			closePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorCanvas defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth*2 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 431 y: y1; displayCoordlineToX: 530 y: y1+dy;
				displayCoordlineToX: 530 y: y1+y2+dy; displayCoordlineToX: 431 y: y1+y2;
				closePath.
			engine
				displayCoordmoveToX:  y1 y: 431; displayCoordlineToX: y1+dy y: 530;
				displayCoordlineToX: y1+y2+dy y: 530; displayCoordlineToX: y1+y2 y: 431;
				closePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth*1.5 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 531 y: y1+dy; displayCoordlineToX: 630 y: y1+dy+dy;
				displayCoordlineToX: 630 y: y1+y2+dy+dy; displayCoordlineToX: 531 y: y1+y2+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy y: 531; displayCoordlineToX: y1+dy+dy y: 630;
				displayCoordlineToX: y1+y2+dy+dy y: 630; displayCoordlineToX: y1+y2+dy y: 531;
				closePath ].
		"Solo fill. w =0"
		engine strokeWidth: VectorCanvas defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
				displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy y: 631; displayCoordlineToX: y1+dy+dy+dy y: 730;
				displayCoordlineToX: y1+y2+dy+dy+dy y: 730; displayCoordlineToX: y1+y2+dy+dy y: 631;
				closePath ].
		y1 _ y1 + y2 + VectorCanvas defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:35'!
experLinesAndShapesx
	"
	self experLinesAndShapesx
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorCanvas defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		y2 print.
		"Solo fill. w =0"
		engine strokeWidth: 0.0 color: Color black fillColor: Color black do: [
			engine
				displayCoordmoveToX: 631 y: y1+dy+dy; displayCoordlineToX: 730 y: y1+dy+dy+dy;
				displayCoordlineToX: 730 y: y1+y2+dy+dy+dy; displayCoordlineToX: 631 y: y1+y2+dy+dy;
				closePath.
			engine
				displayCoordmoveToX: y1+dy+dy y: 431; displayCoordlineToX: y1+dy+dy+dy y: 530;
				displayCoordlineToX: y1+y2+dy+dy+dy y: 530; displayCoordlineToX: y1+y2+dy+dy y: 431;
				closePath ].
		y1 _ y1 + y2 + VectorCanvas defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:37'!
experLinesAndShapesx2
	"
	self experLinesAndShapesx2
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	y1 _ 10.
	dy _ 3.
	y2 _ 0.1.
	y2 print.
	"Solo fill. w =0"
	engine strokeWidth: 0.5 color: Color black fillColor: Color black do: [
		engine
			displayCoordmoveToX: 10 y: y1+dy+dy; displayCoordlineToX: 200 y: y1+dy+dy+dy;
			displayCoordlineToX: 200 y: y1+y2+dy+dy+dy; displayCoordlineToX: 10 y: y1+y2+dy+dy;
			closePath.
		engine
			displayCoordmoveToX: y1+dy+dy y: 10; displayCoordlineToX: y1+dy+dy+dy y: 200;
			displayCoordlineToX: y1+y2+dy+dy+dy y: 200; displayCoordlineToX: y1+y2+dy+dy y: 10;
			closePath ].
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 12/17/2017 18:02:20'!
experiment1
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	self experiment1
	"
| engine h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
engine _ self onForm: Display.
engine resetMorphBounds.
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
"	canvas _  self on: f.
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
			engine fillColor: color do: [
				engine
					displayCoordmoveToX: x1 y: y;
					displayCoordlineToX: x2 y: y;
					displayCoordlineToX: x2p y: prevY;
					displayCoordlineToX: x1p y: prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
"
	canvas removeSpuriousTranslucencyFloatAndBlendOver: Color red.
	f writePNGfileNamed: 'pp17.png'
"! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 12/17/2017 18:02:22'!
experiment2
	"
	self experiment2
	"
| engine h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
engine _ self onForm: Display.
engine resetMorphBounds.
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
			engine fillColor: color do: [
				engine
					displayCoordmoveToX: x1 y: y;
					displayCoordlineToX: x2 y: y;
					displayCoordlineToX: x2p y: prevY;
					displayCoordlineToX: x1p y: prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 12/17/2017 18:02:24'!
experiment3
	"
	self experiment3
	"
	| engine w r |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	w _ 10.
	r _ 15.
	100 timesRepeat: [
		engine strokeWidth: w color: Color black do: [
			engine
				displayCoordarcCenterX: 300 centerY: 300 radiusX: r radiusY: r rotationAngle: 0.0 start: 0.0 sweep:  Float pi * 2;
				closePath ].
		r _ r + w.
		w _ w * 0.8.
		r _ r + w.

	Display forceToScreen.
	].
Display forceToScreen.
! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 12/17/2017 18:02:26'!
experiment4
	"
	self experiment4
	"
	| engine w r |
	engine _ self onForm: Display.
	engine resetMorphBounds.
	w _ 10.
	r _ 15.
	1 to: 100 do: [ :i |
		r _ r + w.
		w _ 20.0 / i.
		r _ r + w.
		engine strokeWidth: w color: Color black do: [
		engine
			displayCoordarcCenterX: 300 centerY: 300 radiusX: r radiusY: r rotationAngle: 0.0 start: 0.0 sweep:  Float pi * 2;
			closePath ].
	Display forceToScreen.
	].
Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:39'!
experimentHopsPerPixel1
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 1: Very thin stroke, the same color as fill
	VectorEngine experimentHopsPerPixel1
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 8.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ VectorEngine onForm: f1.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine resetMorphBounds.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) *10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:41'!
experimentHopsPerPixel2
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 2: Stroke of various widths, no fill
	VectorEngine experimentHopsPerPixel2
	"
	| d engine f1 f2 hops1 hops2 strokeWidth |
	hops1 _ 200.
	hops1 _ 20.
	"
	strokeWidth -> hops2
	1.3 -> 8
	1.5 -> 6
	2 -> 4
	4 -> 3
	6 -> 2
	16 -> 1
	Maybe 10 / strokeWidth + 1 is ok?
	"
	strokeWidth _ 16*2.
	hops2 _ 8*0+1.
	hops2 _ 10.0 / strokeWidth + 1.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ VectorEngine onForm: f1.
	engine resetMorphBounds.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			displayCoordmoveToX: 40 y: 30;
			displayCoordlineToX: 190 y: 135; displayCoordlineToX: 40 y: 40;
			displayCoordlineToX: 135 y: 390; displayCoordlineToX: 30 y: 40;
			closePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine resetMorphBounds.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			displayCoordmoveToX: 40 y: 30;
			displayCoordlineToX: 190 y: 135; displayCoordlineToX: 40 y: 40;
			displayCoordlineToX: 135 y: 390; displayCoordlineToX: 30 y: 40;
			closePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) * 10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 12/17/2017 18:03:42'!
experimentHopsPerPixel3
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 3: No stroke, just fill
	VectorEngine experimentHopsPerPixel3
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 8.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ VectorEngine onForm: f1.
	engine resetMorphBounds.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine resetMorphBounds.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			displayCoordmoveToX: 20 y: 10;
			displayCoordlineToX: 210 y: 15; displayCoordlineToX: 20 y: 20;
			displayCoordlineToX: 15 y: 410; displayCoordlineToX: 10 y: 20;
			closePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) * 10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 12/16/2017 18:17:50'!
onForm: aForm

	^ self new setForm: aForm! !

!VectorFontRenderer methodsFor: 'accessing' stamp: 'jmv 10/14/2014 11:05'!
vectorGraphicsCanvas: aVectorGraphicsCanvas

	vectorGraphicsCanvas _ aVectorGraphicsCanvas.
	vectorGraphicsEngine _ aVectorGraphicsCanvas engine! !

!BitstreamVeraRenderer methodsFor: 'services' stamp: 'jmv 12/10/2017 21:10:43'!
drawCharCode: charCode atX: x0 y: y0 pointSize: ps italics: doItalics
"TrueType en M3. Bitstream Vera Sans, importado asi: TTF -> PDF -> SVG -> Codigo Smalltalk.
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 1 color: Color black fillColor: Color blue do: [
		(BitstreamVeraRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 167 atX: 150 y: 200 pointSize: 200 italics: false;
			drawCharCode: 167 atX: 220 y: 200 pointSize: 200 italics: true ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 3.25 color: Color black fillColor: Color blue do: [
		(BitstreamVeraRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 120 italics: false ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	fillColor: Color black do: [
		(BitstreamVeraRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 158 y: 200.25 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 166 y: 200.5 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 174 y: 200.75 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 182 y: 201 pointSize: 12 italics: false ].
Display forceToScreen
"
	| symbol loc |
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].

	"Recibir la location y modificarla, no?"
	loc _ AffineTransformation withRadians: 0.0 scale: ps position: x0@y0.
	doItalics ifTrue: [ loc _ loc italizing2 ].
	vectorGraphicsCanvas intoLocation: loc.

	symbol _ ('drawVeraSans', charCode printString) asSymbol.
	self perform: symbol.

	vectorGraphicsCanvas outOfLocation! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans100
	vectorGraphicsCanvas
		moveToX: 0.4919 y: 0.4923;
		lineToX: 0.4919 y: 0.1964;
		lineToX: 0.5817 y: 0.1964;
		lineToX: 0.5817 y: 0.9562;
		lineToX: 0.4919 y: 0.9562;
		lineToX: 0.4919 y: 0.8742;
		curveToX: 0.4201 y: 0.9469 control1X: 0.4730 y: 0.9067 control2X: 0.4491 y: 0.9310;
		curveToX: 0.3166 y: 0.9703 control1X: 0.3914 y: 0.9625 control2X: 0.3569 y: 0.9703;
		curveToX: 0.1550 y: 0.8912 control1X: 0.2505 y: 0.9703 control2X: 0.1966 y: 0.9440;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1136 y: 0.8385 control2X: 0.0929 y: 0.7692;
		curveToX: 0.1550 y: 0.4752 control1X: 0.0929 y: 0.5973 control2X: 0.1136 y: 0.5280;
		curveToX: 0.3166 y: 0.3961 control1X: 0.1966 y: 0.4225 control2X: 0.2505 y: 0.3961;
		curveToX: 0.4201 y: 0.4200 control1X: 0.3569 y: 0.3961 control2X: 0.3914 y: 0.4041;
		curveToX: 0.4919 y: 0.4923 control1X: 0.4491 y: 0.4357 control2X: 0.4730 y: 0.4598;
		moveToX: 0.1857 y: 0.6832;
		curveToX: 0.2262 y: 0.8390 control1X: 0.1857 y: 0.7493 control2X: 0.1992 y: 0.8012;
		curveToX: 0.3385 y: 0.8951 control1X: 0.2536 y: 0.8764 control2X: 0.2910 y: 0.8951;
		curveToX: 0.4509 y: 0.8390 control1X: 0.3861 y: 0.8951 control2X: 0.4235 y: 0.8764;
		curveToX: 0.4919 y: 0.6832 control1X: 0.4782 y: 0.8012 control2X: 0.4919 y: 0.7493;
		curveToX: 0.4509 y: 0.5280 control1X: 0.4919 y: 0.6172 control2X: 0.4782 y: 0.5654;
		curveToX: 0.3385 y: 0.4713 control1X: 0.4235 y: 0.4902 control2X: 0.3861 y: 0.4713;
		curveToX: 0.2262 y: 0.5280 control1X: 0.2910 y: 0.4713 control2X: 0.2536 y: 0.4902;
		curveToX: 0.1857 y: 0.6832 control1X: 0.1992 y: 0.5654 control2X: 0.1857 y: 0.6172! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans101
	vectorGraphicsCanvas
		moveToX: 0.5998 y: 0.6603;
		lineToX: 0.5998 y: 0.7042;
		lineToX: 0.1867 y: 0.7042;
		curveToX: 0.2424 y: 0.8458 control1X: 0.1906 y: 0.7661 control2X: 0.2092 y: 0.8133;
		curveToX: 0.3820 y: 0.8942 control1X: 0.2759 y: 0.8781 control2X: 0.3224 y: 0.8942;
		curveToX: 0.4821 y: 0.8815 control1X: 0.4165 y: 0.8942 control2X: 0.4499 y: 0.8899;
		curveToX: 0.5788 y: 0.8434 control1X: 0.5147 y: 0.8730 control2X: 0.5469 y: 0.8603;
		lineToX: 0.5788 y: 0.9283;
		curveToX: 0.4797 y: 0.9596 control1X: 0.5466 y: 0.9420 control2X: 0.5135 y: 0.9524;
		curveToX: 0.3766 y: 0.9703 control1X: 0.4458 y: 0.9668 control2X: 0.4115 y: 0.9703;
		curveToX: 0.1691 y: 0.8942 control1X: 0.2894 y: 0.9703 control2X: 0.2202 y: 0.9450;
		curveToX: 0.0929 y: 0.6881 control1X: 0.1183 y: 0.8434 control2X: 0.0929 y: 0.7747;
		curveToX: 0.1652 y: 0.4752 control1X: 0.0929 y: 0.5986 control2X: 0.1170 y: 0.5276;
		curveToX: 0.3610 y: 0.3961 control1X: 0.2137 y: 0.4225 control2X: 0.2790 y: 0.3961;
		curveToX: 0.5353 y: 0.4674 control1X: 0.4346 y: 0.3961 control2X: 0.4927 y: 0.4199;
		curveToX: 0.5998 y: 0.6603 control1X: 0.5783 y: 0.5146 control2X: 0.5998 y: 0.5789;
		moveToX: 0.5099 y: 0.6339;
		curveToX: 0.4684 y: 0.5162 control1X: 0.5093 y: 0.5848 control2X: 0.4954 y: 0.5455;
		curveToX: 0.3620 y: 0.4723 control1X: 0.4417 y: 0.4869 control2X: 0.4063 y: 0.4723;
		curveToX: 0.2414 y: 0.5148 control1X: 0.3119 y: 0.4723 control2X: 0.2717 y: 0.4865;
		curveToX: 0.1896 y: 0.6344 control1X: 0.2114 y: 0.5431 control2X: 0.1942 y: 0.5830;
		lineToX: 0.5099 y: 0.6339! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans102
	vectorGraphicsCanvas
		moveToX: 0.4089 y: 0.1964;
		lineToX: 0.4089 y: 0.2711;
		lineToX: 0.3229 y: 0.2711;
		curveToX: 0.2555 y: 0.2907 control1X: 0.2907 y: 0.2711 control2X: 0.2682 y: 0.2776;
		curveToX: 0.2370 y: 0.3610 control1X: 0.2432 y: 0.3037 control2X: 0.2370 y: 0.3271;
		lineToX: 0.2370 y: 0.4093;
		lineToX: 0.3849 y: 0.4093;
		lineToX: 0.3849 y: 0.4791;
		lineToX: 0.2370 y: 0.4791;
		lineToX: 0.2370 y: 0.9562;
		lineToX: 0.1467 y: 0.9562;
		lineToX: 0.1467 y: 0.4791;
		lineToX: 0.0607 y: 0.4791;
		lineToX: 0.0607 y: 0.4093;
		lineToX: 0.1467 y: 0.4093;
		lineToX: 0.1467 y: 0.3712;
		curveToX: 0.1891 y: 0.2384 control1X: 0.1467 y: 0.3103 control2X: 0.1608 y: 0.2661;
		curveToX: 0.3239 y: 0.1964 control1X: 0.2175 y: 0.2104 control2X: 0.2624 y: 0.1964;
		lineToX: 0.4089 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans103
	vectorGraphicsCanvas
		moveToX: 0.4919 y: 0.6764;
		curveToX: 0.4513 y: 0.5250 control1X: 0.4919 y: 0.6113 control2X: 0.4784 y: 0.5608;
		curveToX: 0.3385 y: 0.4713 control1X: 0.4246 y: 0.4892 control2X: 0.3871 y: 0.4713;
		curveToX: 0.2258 y: 0.5250 control1X: 0.2904 y: 0.4713 control2X: 0.2528 y: 0.4892;
		curveToX: 0.1857 y: 0.6764 control1X: 0.1991 y: 0.5608 control2X: 0.1857 y: 0.6113;
		curveToX: 0.2258 y: 0.8273 control1X: 0.1857 y: 0.7412 control2X: 0.1991 y: 0.7915;
		curveToX: 0.3385 y: 0.8810 control1X: 0.2528 y: 0.8631 control2X: 0.2904 y: 0.8810;
		curveToX: 0.4513 y: 0.8273 control1X: 0.3871 y: 0.8810 control2X: 0.4246 y: 0.8631;
		curveToX: 0.4919 y: 0.6764 control1X: 0.4784 y: 0.7915 control2X: 0.4919 y: 0.7412;
		moveToX: 0.5817 y: 0.8883;
		curveToX: 0.5197 y: 1.0958 control1X: 0.5817 y: 0.9814 control2X: 0.5610 y: 1.0506;
		curveToX: 0.3298 y: 1.1642 control1X: 0.4784 y: 1.1414 control2X: 0.4150 y: 1.1642;
		curveToX: 0.2404 y: 1.1569 control1X: 0.2982 y: 1.1642 control2X: 0.2684 y: 1.1617;
		curveToX: 0.1589 y: 1.1354 control1X: 0.2124 y: 1.1523 control2X: 0.1852 y: 1.1451;
		lineToX: 0.1589 y: 1.0480;
		curveToX: 0.2370 y: 1.0797 control1X: 0.1852 y: 1.0623 control2X: 0.2113 y: 1.0729;
		curveToX: 0.3156 y: 1.0900 control1X: 0.2627 y: 1.0866 control2X: 0.2889 y: 1.0900;
		curveToX: 0.4479 y: 1.0436 control1X: 0.3745 y: 1.0900 control2X: 0.4186 y: 1.0745;
		curveToX: 0.4919 y: 0.9044 control1X: 0.4772 y: 1.0130 control2X: 0.4919 y: 0.9666;
		lineToX: 0.4919 y: 0.8600;
		curveToX: 0.4206 y: 0.9323 control1X: 0.4733 y: 0.8922 control2X: 0.4496 y: 0.9163;
		curveToX: 0.3166 y: 0.9562 control1X: 0.3916 y: 0.9482 control2X: 0.3569 y: 0.9562;
		curveToX: 0.1545 y: 0.8795 control1X: 0.2495 y: 0.9562 control2X: 0.1955 y: 0.9306;
		curveToX: 0.0929 y: 0.6764 control1X: 0.1135 y: 0.8284 control2X: 0.0929 y: 0.7607;
		curveToX: 0.1545 y: 0.4728 control1X: 0.0929 y: 0.5918 control2X: 0.1135 y: 0.5239;
		curveToX: 0.3166 y: 0.3961 control1X: 0.1955 y: 0.4217 control2X: 0.2495 y: 0.3961;
		curveToX: 0.4206 y: 0.4200 control1X: 0.3569 y: 0.3961 control2X: 0.3916 y: 0.4041;
		curveToX: 0.4919 y: 0.4923 control1X: 0.4496 y: 0.4360 control2X: 0.4733 y: 0.4601;
		lineToX: 0.4919 y: 0.4093;
		lineToX: 0.5817 y: 0.4093;
		lineToX: 0.5817 y: 0.8883! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans104
	vectorGraphicsCanvas
		moveToX: 0.5866 y: 0.6261;
		lineToX: 0.5866 y: 0.9562;
		lineToX: 0.4968 y: 0.9562;
		lineToX: 0.4968 y: 0.6290;
		curveToX: 0.4665 y: 0.5128 control1X: 0.4968 y: 0.5773 control2X: 0.4867 y: 0.5385;
		curveToX: 0.3757 y: 0.4742 control1X: 0.4463 y: 0.4871 control2X: 0.4160 y: 0.4742;
		curveToX: 0.2609 y: 0.5206 control1X: 0.3272 y: 0.4742 control2X: 0.2889 y: 0.4897;
		curveToX: 0.2189 y: 0.6471 control1X: 0.2329 y: 0.5516 control2X: 0.2189 y: 0.5937;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.1964;
		lineToX: 0.2189 y: 0.1964;
		lineToX: 0.2189 y: 0.4943;
		curveToX: 0.2946 y: 0.4205 control1X: 0.2404 y: 0.4614 control2X: 0.2656 y: 0.4368;
		curveToX: 0.3957 y: 0.3961 control1X: 0.3239 y: 0.4043 control2X: 0.3576 y: 0.3961;
		curveToX: 0.5383 y: 0.4547 control1X: 0.4585 y: 0.3961 control2X: 0.5060 y: 0.4157;
		curveToX: 0.5866 y: 0.6261 control1X: 0.5705 y: 0.4935 control2X: 0.5866 y: 0.5506! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans105
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.1320 y: 0.1964;
		lineToX: 0.2218 y: 0.1964;
		lineToX: 0.2218 y: 0.3102;
		lineToX: 0.1320 y: 0.3102;
		lineToX: 0.1320 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans106
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9659;
		curveToX: 0.1818 y: 1.1173 control1X: 0.2218 y: 1.0356 control2X: 0.2085 y: 1.0861;
		curveToX: 0.0539 y: 1.1642 control1X: 0.1554 y: 1.1486 control2X: 0.1128 y: 1.1642;
		lineToX: 0.0197 y: 1.1642;
		lineToX: 0.0197 y: 1.0880;
		lineToX: 0.0436 y: 1.0880;
		curveToX: 0.1135 y: 1.0641 control1X: 0.0778 y: 1.0880 control2X: 0.1011 y: 1.0800;
		curveToX: 0.1320 y: 0.9659 control1X: 0.1258 y: 1.0485 control2X: 0.1320 y: 1.0158;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.1320 y: 0.1964;
		lineToX: 0.2218 y: 0.1964;
		lineToX: 0.2218 y: 0.3102;
		lineToX: 0.1320 y: 0.3102;
		lineToX: 0.1320 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans107
	vectorGraphicsCanvas
		moveToX: 0.1286 y: 0.1964;
		lineToX: 0.2189 y: 0.1964;
		lineToX: 0.2189 y: 0.6451;
		lineToX: 0.4870 y: 0.4093;
		lineToX: 0.6017 y: 0.4093;
		lineToX: 0.3117 y: 0.6652;
		lineToX: 0.6139 y: 0.9562;
		lineToX: 0.4968 y: 0.9562;
		lineToX: 0.2189 y: 0.6891;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans108
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.1964;
		lineToX: 0.2218 y: 0.1964;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans109
	vectorGraphicsCanvas
		moveToX: 0.5578 y: 0.5143;
		curveToX: 0.6384 y: 0.4249 control1X: 0.5802 y: 0.4739 control2X: 0.6071 y: 0.4441;
		curveToX: 0.7487 y: 0.3961 control1X: 0.6696 y: 0.4057 control2X: 0.7064 y: 0.3961;
		curveToX: 0.8805 y: 0.4562 control1X: 0.8057 y: 0.3961 control2X: 0.8496 y: 0.4161;
		curveToX: 0.9269 y: 0.6261 control1X: 0.9115 y: 0.4959 control2X: 0.9269 y: 0.5525;
		lineToX: 0.9269 y: 0.9562;
		lineToX: 0.8366 y: 0.9562;
		lineToX: 0.8366 y: 0.6290;
		curveToX: 0.8088 y: 0.5123 control1X: 0.8366 y: 0.5766 control2X: 0.8273 y: 0.5377;
		curveToX: 0.7238 y: 0.4742 control1X: 0.7902 y: 0.4869 control2X: 0.7619 y: 0.4742;
		curveToX: 0.6135 y: 0.5206 control1X: 0.6773 y: 0.4742 control2X: 0.6405 y: 0.4897;
		curveToX: 0.5729 y: 0.6471 control1X: 0.5864 y: 0.5516 control2X: 0.5729 y: 0.5937;
		lineToX: 0.5729 y: 0.9562;
		lineToX: 0.4826 y: 0.9562;
		lineToX: 0.4826 y: 0.6290;
		curveToX: 0.4548 y: 0.5123 control1X: 0.4826 y: 0.5763 control2X: 0.4733 y: 0.5374;
		curveToX: 0.3688 y: 0.4742 control1X: 0.4362 y: 0.4869 control2X: 0.4076 y: 0.4742;
		curveToX: 0.2594 y: 0.5211 control1X: 0.3229 y: 0.4742 control2X: 0.2865 y: 0.4899;
		curveToX: 0.2189 y: 0.6471 control1X: 0.2324 y: 0.5520 control2X: 0.2189 y: 0.5940;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.4093;
		lineToX: 0.2189 y: 0.4093;
		lineToX: 0.2189 y: 0.4943;
		curveToX: 0.2926 y: 0.4200 control1X: 0.2394 y: 0.4607 control2X: 0.2640 y: 0.4360;
		curveToX: 0.3947 y: 0.3961 control1X: 0.3213 y: 0.4041 control2X: 0.3553 y: 0.3961;
		curveToX: 0.4958 y: 0.4264 control1X: 0.4344 y: 0.3961 control2X: 0.4681 y: 0.4062;
		curveToX: 0.5578 y: 0.5143 control1X: 0.5238 y: 0.4466 control2X: 0.5444 y: 0.4759! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans110
	vectorGraphicsCanvas
		moveToX: 0.5866 y: 0.6261;
		lineToX: 0.5866 y: 0.9562;
		lineToX: 0.4968 y: 0.9562;
		lineToX: 0.4968 y: 0.6290;
		curveToX: 0.4665 y: 0.5128 control1X: 0.4968 y: 0.5773 control2X: 0.4867 y: 0.5385;
		curveToX: 0.3757 y: 0.4742 control1X: 0.4463 y: 0.4871 control2X: 0.4160 y: 0.4742;
		curveToX: 0.2609 y: 0.5206 control1X: 0.3272 y: 0.4742 control2X: 0.2889 y: 0.4897;
		curveToX: 0.2189 y: 0.6471 control1X: 0.2329 y: 0.5516 control2X: 0.2189 y: 0.5937;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.4093;
		lineToX: 0.2189 y: 0.4093;
		lineToX: 0.2189 y: 0.4943;
		curveToX: 0.2946 y: 0.4205 control1X: 0.2404 y: 0.4614 control2X: 0.2656 y: 0.4368;
		curveToX: 0.3957 y: 0.3961 control1X: 0.3239 y: 0.4043 control2X: 0.3576 y: 0.3961;
		curveToX: 0.5383 y: 0.4547 control1X: 0.4585 y: 0.3961 control2X: 0.5060 y: 0.4157;
		curveToX: 0.5866 y: 0.6261 control1X: 0.5705 y: 0.4935 control2X: 0.5866 y: 0.5506! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans111
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans112
	vectorGraphicsCanvas
		moveToX: 0.2189 y: 0.8742;
		lineToX: 0.2189 y: 1.1642;
		lineToX: 0.1286 y: 1.1642;
		lineToX: 0.1286 y: 0.4093;
		lineToX: 0.2189 y: 0.4093;
		lineToX: 0.2189 y: 0.4923;
		curveToX: 0.2902 y: 0.4200 control1X: 0.2378 y: 0.4598 control2X: 0.2616 y: 0.4357;
		curveToX: 0.3937 y: 0.3961 control1X: 0.3192 y: 0.4041 control2X: 0.3537 y: 0.3961;
		curveToX: 0.5553 y: 0.4752 control1X: 0.4601 y: 0.3961 control2X: 0.5140 y: 0.4225;
		curveToX: 0.6178 y: 0.6832 control1X: 0.5970 y: 0.5280 control2X: 0.6178 y: 0.5973;
		curveToX: 0.5553 y: 0.8912 control1X: 0.6178 y: 0.7692 control2X: 0.5970 y: 0.8385;
		curveToX: 0.3937 y: 0.9703 control1X: 0.5140 y: 0.9440 control2X: 0.4601 y: 0.9703;
		curveToX: 0.2902 y: 0.9469 control1X: 0.3537 y: 0.9703 control2X: 0.3192 y: 0.9625;
		curveToX: 0.2189 y: 0.8742 control1X: 0.2616 y: 0.9310 control2X: 0.2378 y: 0.9067;
		moveToX: 0.5246 y: 0.6832;
		curveToX: 0.4836 y: 0.5280 control1X: 0.5246 y: 0.6172 control2X: 0.5109 y: 0.5654;
		curveToX: 0.3718 y: 0.4713 control1X: 0.4565 y: 0.4902 control2X: 0.4193 y: 0.4713;
		curveToX: 0.2594 y: 0.5280 control1X: 0.3242 y: 0.4713 control2X: 0.2868 y: 0.4902;
		curveToX: 0.2189 y: 0.6832 control1X: 0.2324 y: 0.5654 control2X: 0.2189 y: 0.6172;
		curveToX: 0.2594 y: 0.8390 control1X: 0.2189 y: 0.7493 control2X: 0.2324 y: 0.8012;
		curveToX: 0.3718 y: 0.8951 control1X: 0.2868 y: 0.8764 control2X: 0.3242 y: 0.8951;
		curveToX: 0.4836 y: 0.8390 control1X: 0.4193 y: 0.8951 control2X: 0.4565 y: 0.8764;
		curveToX: 0.5246 y: 0.6832 control1X: 0.5109 y: 0.8012 control2X: 0.5246 y: 0.7493! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans113
	vectorGraphicsCanvas
		moveToX: 0.1857 y: 0.6832;
		curveToX: 0.2262 y: 0.8390 control1X: 0.1857 y: 0.7493 control2X: 0.1992 y: 0.8012;
		curveToX: 0.3385 y: 0.8951 control1X: 0.2536 y: 0.8764 control2X: 0.2910 y: 0.8951;
		curveToX: 0.4509 y: 0.8390 control1X: 0.3861 y: 0.8951 control2X: 0.4235 y: 0.8764;
		curveToX: 0.4919 y: 0.6832 control1X: 0.4782 y: 0.8012 control2X: 0.4919 y: 0.7493;
		curveToX: 0.4509 y: 0.5280 control1X: 0.4919 y: 0.6172 control2X: 0.4782 y: 0.5654;
		curveToX: 0.3385 y: 0.4713 control1X: 0.4235 y: 0.4902 control2X: 0.3861 y: 0.4713;
		curveToX: 0.2262 y: 0.5280 control1X: 0.2910 y: 0.4713 control2X: 0.2536 y: 0.4902;
		curveToX: 0.1857 y: 0.6832 control1X: 0.1992 y: 0.5654 control2X: 0.1857 y: 0.6172;
		moveToX: 0.4919 y: 0.8742;
		curveToX: 0.4201 y: 0.9469 control1X: 0.4730 y: 0.9067 control2X: 0.4491 y: 0.9310;
		curveToX: 0.3166 y: 0.9703 control1X: 0.3914 y: 0.9625 control2X: 0.3569 y: 0.9703;
		curveToX: 0.1550 y: 0.8912 control1X: 0.2505 y: 0.9703 control2X: 0.1966 y: 0.9440;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1136 y: 0.8385 control2X: 0.0929 y: 0.7692;
		curveToX: 0.1550 y: 0.4752 control1X: 0.0929 y: 0.5973 control2X: 0.1136 y: 0.5280;
		curveToX: 0.3166 y: 0.3961 control1X: 0.1966 y: 0.4225 control2X: 0.2505 y: 0.3961;
		curveToX: 0.4201 y: 0.4200 control1X: 0.3569 y: 0.3961 control2X: 0.3914 y: 0.4041;
		curveToX: 0.4919 y: 0.4923 control1X: 0.4491 y: 0.4357 control2X: 0.4730 y: 0.4598;
		lineToX: 0.4919 y: 0.4093;
		lineToX: 0.5817 y: 0.4093;
		lineToX: 0.5817 y: 1.1642;
		lineToX: 0.4919 y: 1.1642;
		lineToX: 0.4919 y: 0.8742! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans114
	vectorGraphicsCanvas
		moveToX: 0.4489 y: 0.4933;
		curveToX: 0.4157 y: 0.4806 control1X: 0.4388 y: 0.4874 control2X: 0.4277 y: 0.4832;
		curveToX: 0.3766 y: 0.4762 control1X: 0.4040 y: 0.4777 control2X: 0.3910 y: 0.4762;
		curveToX: 0.2594 y: 0.5260 control1X: 0.3259 y: 0.4762 control2X: 0.2868 y: 0.4928;
		curveToX: 0.2189 y: 0.6681 control1X: 0.2324 y: 0.5589 control2X: 0.2189 y: 0.6062;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.4093;
		lineToX: 0.2189 y: 0.4093;
		lineToX: 0.2189 y: 0.4943;
		curveToX: 0.2926 y: 0.4205 control1X: 0.2378 y: 0.4611 control2X: 0.2624 y: 0.4365;
		curveToX: 0.4030 y: 0.3961 control1X: 0.3229 y: 0.4043 control2X: 0.3597 y: 0.3961;
		curveToX: 0.4235 y: 0.3976 control1X: 0.4092 y: 0.3961 control2X: 0.4160 y: 0.3966;
		curveToX: 0.4484 y: 0.4010 control1X: 0.4310 y: 0.3982 control2X: 0.4393 y: 0.3994;
		lineToX: 0.4489 y: 0.4933! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans115
	vectorGraphicsCanvas
		moveToX: 0.4806 y: 0.4254;
		lineToX: 0.4806 y: 0.5104;
		curveToX: 0.4015 y: 0.4811 control1X: 0.4552 y: 0.4974 control2X: 0.4289 y: 0.4876;
		curveToX: 0.3166 y: 0.4713 control1X: 0.3742 y: 0.4746 control2X: 0.3459 y: 0.4713;
		curveToX: 0.2160 y: 0.4918 control1X: 0.2720 y: 0.4713 control2X: 0.2385 y: 0.4782;
		curveToX: 0.1828 y: 0.5533 control1X: 0.1939 y: 0.5055 control2X: 0.1828 y: 0.5260;
		curveToX: 0.2067 y: 0.6027 control1X: 0.1828 y: 0.5742 control2X: 0.1908 y: 0.5906;
		curveToX: 0.3029 y: 0.6364 control1X: 0.2227 y: 0.6144 control2X: 0.2547 y: 0.6256;
		lineToX: 0.3337 y: 0.6432;
		curveToX: 0.4694 y: 0.7013 control1X: 0.3975 y: 0.6569 control2X: 0.4427 y: 0.6762;
		curveToX: 0.5099 y: 0.8053 control1X: 0.4964 y: 0.7260 control2X: 0.5099 y: 0.7607;
		curveToX: 0.4494 y: 0.9259 control1X: 0.5099 y: 0.8561 control2X: 0.4898 y: 0.8963;
		curveToX: 0.2839 y: 0.9703 control1X: 0.4093 y: 0.9555 control2X: 0.3542 y: 0.9703;
		curveToX: 0.1921 y: 0.9616 control1X: 0.2546 y: 0.9703 control2X: 0.2240 y: 0.9674;
		curveToX: 0.0920 y: 0.9362 control1X: 0.1605 y: 0.9560 control2X: 0.1271 y: 0.9476;
		lineToX: 0.0920 y: 0.8434;
		curveToX: 0.1901 y: 0.8825 control1X: 0.1252 y: 0.8606 control2X: 0.1579 y: 0.8737;
		curveToX: 0.2858 y: 0.8951 control1X: 0.2223 y: 0.8909 control2X: 0.2542 y: 0.8951;
		curveToX: 0.3835 y: 0.8737 control1X: 0.3281 y: 0.8951 control2X: 0.3607 y: 0.8880;
		curveToX: 0.4176 y: 0.8121 control1X: 0.4063 y: 0.8590 control2X: 0.4176 y: 0.8385;
		curveToX: 0.3927 y: 0.7560 control1X: 0.4176 y: 0.7877 control2X: 0.4093 y: 0.7690;
		curveToX: 0.2848 y: 0.7184 control1X: 0.3765 y: 0.7430 control2X: 0.3405 y: 0.7304;
		lineToX: 0.2536 y: 0.7111;
		curveToX: 0.1330 y: 0.6574 control1X: 0.1979 y: 0.6993 control2X: 0.1577 y: 0.6814;
		curveToX: 0.0959 y: 0.5573 control1X: 0.1082 y: 0.6329 control2X: 0.0959 y: 0.5996;
		curveToX: 0.1506 y: 0.4381 control1X: 0.0959 y: 0.5058 control2X: 0.1141 y: 0.4661;
		curveToX: 0.3058 y: 0.3961 control1X: 0.1870 y: 0.4101 control2X: 0.2388 y: 0.3961;
		curveToX: 0.3996 y: 0.4034 control1X: 0.3390 y: 0.3961 control2X: 0.3703 y: 0.3986;
		curveToX: 0.4806 y: 0.4254 control1X: 0.4289 y: 0.4083 control2X: 0.4559 y: 0.4157! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans116
	vectorGraphicsCanvas
		moveToX: 0.2209 y: 0.2540;
		lineToX: 0.2209 y: 0.4093;
		lineToX: 0.4059 y: 0.4093;
		lineToX: 0.4059 y: 0.4791;
		lineToX: 0.2209 y: 0.4791;
		lineToX: 0.2209 y: 0.7760;
		curveToX: 0.2389 y: 0.8619 control1X: 0.2209 y: 0.8206 control2X: 0.2269 y: 0.8492;
		curveToX: 0.3136 y: 0.8810 control1X: 0.2513 y: 0.8746 control2X: 0.2762 y: 0.8810;
		lineToX: 0.4059 y: 0.8810;
		lineToX: 0.4059 y: 0.9562;
		lineToX: 0.3136 y: 0.9562;
		curveToX: 0.1701 y: 0.9176 control1X: 0.2443 y: 0.9562 control2X: 0.1965 y: 0.9433;
		curveToX: 0.1305 y: 0.7760 control1X: 0.1437 y: 0.8916 control2X: 0.1305 y: 0.8444;
		lineToX: 0.1305 y: 0.4791;
		lineToX: 0.0646 y: 0.4791;
		lineToX: 0.0646 y: 0.4093;
		lineToX: 0.1305 y: 0.4093;
		lineToX: 0.1305 y: 0.2540;
		lineToX: 0.2209 y: 0.2540! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans117
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 0.7404;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7369;
		curveToX: 0.2428 y: 0.8536 control1X: 0.2126 y: 0.7887 control2X: 0.2227 y: 0.8276;
		curveToX: 0.3337 y: 0.8922 control1X: 0.2630 y: 0.8794 control2X: 0.2933 y: 0.8922;
		curveToX: 0.4484 y: 0.8458 control1X: 0.3822 y: 0.8922 control2X: 0.4204 y: 0.8768;
		curveToX: 0.4909 y: 0.7194 control1X: 0.4767 y: 0.8149 control2X: 0.4909 y: 0.7728;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.4909 y: 0.8722;
		curveToX: 0.4147 y: 0.9464 control1X: 0.4691 y: 0.9054 control2X: 0.4437 y: 0.9301;
		curveToX: 0.3146 y: 0.9703 control1X: 0.3861 y: 0.9624 control2X: 0.3527 y: 0.9703;
		curveToX: 0.1716 y: 0.9117 control1X: 0.2518 y: 0.9703 control2X: 0.2041 y: 0.9508;
		curveToX: 0.1227 y: 0.7404 control1X: 0.1390 y: 0.8727 control2X: 0.1227 y: 0.8156! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans118
	vectorGraphicsCanvas
		moveToX: 0.0676 y: 0.4093;
		lineToX: 0.1628 y: 0.4093;
		lineToX: 0.3337 y: 0.8683;
		lineToX: 0.5046 y: 0.4093;
		lineToX: 0.5998 y: 0.4093;
		lineToX: 0.3947 y: 0.9562;
		lineToX: 0.2726 y: 0.9562;
		lineToX: 0.0676 y: 0.4093! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans119
	vectorGraphicsCanvas
		moveToX: 0.0798 y: 0.4093;
		lineToX: 0.1696 y: 0.4093;
		lineToX: 0.2819 y: 0.8361;
		lineToX: 0.3937 y: 0.4093;
		lineToX: 0.4997 y: 0.4093;
		lineToX: 0.6120 y: 0.8361;
		lineToX: 0.7238 y: 0.4093;
		lineToX: 0.8136 y: 0.4093;
		lineToX: 0.6706 y: 0.9562;
		lineToX: 0.5646 y: 0.9562;
		lineToX: 0.4469 y: 0.5079;
		lineToX: 0.3288 y: 0.9562;
		lineToX: 0.2228 y: 0.9562;
		lineToX: 0.0798 y: 0.4093! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans120
	vectorGraphicsCanvas
		moveToX: 0.5866 y: 0.4093;
		lineToX: 0.3888 y: 0.6754;
		lineToX: 0.5968 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.3317 y: 0.7413;
		lineToX: 0.1725 y: 0.9562;
		lineToX: 0.0666 y: 0.9562;
		lineToX: 0.2790 y: 0.6700;
		lineToX: 0.0846 y: 0.4093;
		lineToX: 0.1906 y: 0.4093;
		lineToX: 0.3356 y: 0.6041;
		lineToX: 0.4806 y: 0.4093;
		lineToX: 0.5866 y: 0.4093! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans121
	vectorGraphicsCanvas
		moveToX: 0.3595 y: 1.0070;
		curveToX: 0.2853 y: 1.1344 control1X: 0.3342 y: 1.0721 control2X: 0.3094 y: 1.1145;
		curveToX: 0.1886 y: 1.1642 control1X: 0.2612 y: 1.1543 control2X: 0.2290 y: 1.1642;
		lineToX: 0.1169 y: 1.1642;
		lineToX: 0.1169 y: 1.0890;
		lineToX: 0.1696 y: 1.0890;
		curveToX: 0.2272 y: 1.0714 control1X: 0.1943 y: 1.0890 control2X: 0.2135 y: 1.0831;
		curveToX: 0.2726 y: 0.9884 control1X: 0.2409 y: 1.0597 control2X: 0.2560 y: 1.0320;
		lineToX: 0.2887 y: 0.9474;
		lineToX: 0.0676 y: 0.4093;
		lineToX: 0.1628 y: 0.4093;
		lineToX: 0.3337 y: 0.8370;
		lineToX: 0.5046 y: 0.4093;
		lineToX: 0.5998 y: 0.4093;
		lineToX: 0.3595 y: 1.0070! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans122
	vectorGraphicsCanvas
		moveToX: 0.0929 y: 0.4093;
		lineToX: 0.5197 y: 0.4093;
		lineToX: 0.5197 y: 0.4913;
		lineToX: 0.1818 y: 0.8844;
		lineToX: 0.5197 y: 0.8844;
		lineToX: 0.5197 y: 0.9562;
		lineToX: 0.0807 y: 0.9562;
		lineToX: 0.0807 y: 0.8742;
		lineToX: 0.4186 y: 0.4811;
		lineToX: 0.0929 y: 0.4811;
		lineToX: 0.0929 y: 0.4093! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans123
	vectorGraphicsCanvas
		moveToX: 0.5490 y: 1.0490;
		lineToX: 0.5490 y: 1.1193;
		lineToX: 0.5187 y: 1.1193;
		curveToX: 0.3556 y: 1.0831 control1X: 0.4377 y: 1.1193 control2X: 0.3833 y: 1.1072;
		curveToX: 0.3146 y: 0.9391 control1X: 0.3283 y: 1.0590 control2X: 0.3146 y: 1.0110;
		lineToX: 0.3146 y: 0.8224;
		curveToX: 0.2883 y: 0.7203 control1X: 0.3146 y: 0.7732 control2X: 0.3058 y: 0.7392;
		curveToX: 0.1926 y: 0.6920 control1X: 0.2707 y: 0.7015 control2X: 0.2388 y: 0.6920;
		lineToX: 0.1628 y: 0.6920;
		lineToX: 0.1628 y: 0.6222;
		lineToX: 0.1926 y: 0.6222;
		curveToX: 0.2883 y: 0.5944 control1X: 0.2391 y: 0.6222 control2X: 0.2710 y: 0.6129;
		curveToX: 0.3146 y: 0.4933 control1X: 0.3058 y: 0.5755 control2X: 0.3146 y: 0.5418;
		lineToX: 0.3146 y: 0.3761;
		curveToX: 0.3556 y: 0.2325 control1X: 0.3146 y: 0.3042 control2X: 0.3283 y: 0.2563;
		curveToX: 0.5187 y: 0.1964 control1X: 0.3833 y: 0.2085 control2X: 0.4377 y: 0.1964;
		lineToX: 0.5490 y: 0.1964;
		lineToX: 0.5490 y: 0.2662;
		lineToX: 0.5158 y: 0.2662;
		curveToX: 0.4260 y: 0.2877 control1X: 0.4699 y: 0.2662 control2X: 0.4399 y: 0.2734;
		curveToX: 0.4050 y: 0.3781 control1X: 0.4120 y: 0.3020 control2X: 0.4050 y: 0.3322;
		lineToX: 0.4050 y: 0.4992;
		curveToX: 0.3825 y: 0.6105 control1X: 0.4050 y: 0.5503 control2X: 0.3975 y: 0.5874;
		curveToX: 0.3068 y: 0.6574 control1X: 0.3678 y: 0.6336 control2X: 0.3426 y: 0.6492;
		curveToX: 0.3830 y: 0.7052 control1X: 0.3429 y: 0.6661 control2X: 0.3683 y: 0.6821;
		curveToX: 0.4050 y: 0.8160 control1X: 0.3976 y: 0.7283 control2X: 0.4050 y: 0.7653;
		lineToX: 0.4050 y: 0.9371;
		curveToX: 0.4260 y: 1.0275 control1X: 0.4050 y: 0.9830 control2X: 0.4120 y: 1.0131;
		curveToX: 0.5158 y: 1.0490 control1X: 0.4399 y: 1.0418 control2X: 0.4699 y: 1.0490;
		lineToX: 0.5490 y: 1.0490! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans124
	vectorGraphicsCanvas
		moveToX: 0.2477 y: 0.1920;
		lineToX: 0.2477 y: 1.1920;
		lineToX: 0.1647 y: 1.1920;
		lineToX: 0.1647 y: 0.1920;
		lineToX: 0.2477 y: 0.1920! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans125
	vectorGraphicsCanvas
		moveToX: 0.1628 y: 1.0490;
		lineToX: 0.1969 y: 1.0490;
		curveToX: 0.2858 y: 1.0280 control1X: 0.2425 y: 1.0490 control2X: 0.2721 y: 1.0420;
		curveToX: 0.3068 y: 0.9371 control1X: 0.2998 y: 1.0140 control2X: 0.3068 y: 0.9837;
		lineToX: 0.3068 y: 0.8160;
		curveToX: 0.3288 y: 0.7052 control1X: 0.3068 y: 0.7653 control2X: 0.3141 y: 0.7283;
		curveToX: 0.4050 y: 0.6574 control1X: 0.3434 y: 0.6821 control2X: 0.3688 y: 0.6661;
		curveToX: 0.3288 y: 0.6105 control1X: 0.3688 y: 0.6492 control2X: 0.3434 y: 0.6336;
		curveToX: 0.3068 y: 0.4992 control1X: 0.3141 y: 0.5874 control2X: 0.3068 y: 0.5503;
		lineToX: 0.3068 y: 0.3781;
		curveToX: 0.2858 y: 0.2877 control1X: 0.3068 y: 0.3318 control2X: 0.2998 y: 0.3017;
		curveToX: 0.1969 y: 0.2662 control1X: 0.2721 y: 0.2734 control2X: 0.2425 y: 0.2662;
		lineToX: 0.1628 y: 0.2662;
		lineToX: 0.1628 y: 0.1964;
		lineToX: 0.1935 y: 0.1964;
		curveToX: 0.3556 y: 0.2325 control1X: 0.2746 y: 0.1964 control2X: 0.3286 y: 0.2085;
		curveToX: 0.3967 y: 0.3761 control1X: 0.3830 y: 0.2563 control2X: 0.3967 y: 0.3042;
		lineToX: 0.3967 y: 0.4933;
		curveToX: 0.4230 y: 0.5944 control1X: 0.3967 y: 0.5418 control2X: 0.4054 y: 0.5755;
		curveToX: 0.5187 y: 0.6222 control1X: 0.4406 y: 0.6129 control2X: 0.4725 y: 0.6222;
		lineToX: 0.5490 y: 0.6222;
		lineToX: 0.5490 y: 0.6920;
		lineToX: 0.5187 y: 0.6920;
		curveToX: 0.4230 y: 0.7203 control1X: 0.4725 y: 0.6920 control2X: 0.4406 y: 0.7015;
		curveToX: 0.3967 y: 0.8224 control1X: 0.4054 y: 0.7392 control2X: 0.3967 y: 0.7732;
		lineToX: 0.3967 y: 0.9391;
		curveToX: 0.3556 y: 1.0831 control1X: 0.3967 y: 1.0110 control2X: 0.3830 y: 1.0590;
		curveToX: 0.1935 y: 1.1193 control1X: 0.3286 y: 1.1072 control2X: 0.2746 y: 1.1193;
		lineToX: 0.1628 y: 1.1193;
		lineToX: 0.1628 y: 1.0490! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans126
	vectorGraphicsCanvas
		moveToX: 0.7697 y: 0.5573;
		lineToX: 0.7697 y: 0.6442;
		curveToX: 0.6745 y: 0.6993 control1X: 0.7355 y: 0.6699 control2X: 0.7038 y: 0.6883;
		curveToX: 0.5837 y: 0.7159 control1X: 0.6455 y: 0.7104 control2X: 0.6152 y: 0.7159;
		curveToX: 0.4587 y: 0.6871 control1X: 0.5479 y: 0.7159 control2X: 0.5062 y: 0.7063;
		curveToX: 0.4509 y: 0.6842 control1X: 0.4551 y: 0.6858 control2X: 0.4525 y: 0.6849;
		curveToX: 0.4401 y: 0.6803 control1X: 0.4486 y: 0.6832 control2X: 0.4450 y: 0.6819;
		curveToX: 0.3185 y: 0.6500 control1X: 0.3897 y: 0.6601 control2X: 0.3491 y: 0.6500;
		curveToX: 0.2336 y: 0.6691 control1X: 0.2899 y: 0.6500 control2X: 0.2616 y: 0.6564;
		curveToX: 0.1437 y: 0.7282 control1X: 0.2056 y: 0.6814 control2X: 0.1756 y: 0.7011;
		lineToX: 0.1437 y: 0.6412;
		curveToX: 0.2385 y: 0.5861 control1X: 0.1779 y: 0.6155 control2X: 0.2095 y: 0.5971;
		curveToX: 0.3298 y: 0.5690 control1X: 0.2677 y: 0.5747 control2X: 0.2982 y: 0.5690;
		curveToX: 0.4552 y: 0.5983 control1X: 0.3656 y: 0.5690 control2X: 0.4074 y: 0.5787;
		curveToX: 0.4626 y: 0.6012 control1X: 0.4585 y: 0.5996 control2X: 0.4609 y: 0.6006;
		curveToX: 0.4733 y: 0.6051 control1X: 0.4652 y: 0.6022 control2X: 0.4688 y: 0.6035;
		curveToX: 0.5949 y: 0.6354 control1X: 0.5238 y: 0.6253 control2X: 0.5643 y: 0.6354;
		curveToX: 0.6779 y: 0.6168 control1X: 0.6229 y: 0.6354 control2X: 0.6506 y: 0.6292;
		curveToX: 0.7697 y: 0.5573 control1X: 0.7056 y: 0.6045 control2X: 0.7362 y: 0.5846! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans128
	vectorGraphicsCanvas
		moveToX: 0.5046 y: 0.2272;
		lineToX: 0.7697 y: 0.4992;
		lineToX: 0.6716 y: 0.4992;
		lineToX: 0.4567 y: 0.3063;
		lineToX: 0.2419 y: 0.4992;
		lineToX: 0.1437 y: 0.4992;
		lineToX: 0.4089 y: 0.2272;
		lineToX: 0.5046 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans129
	vectorGraphicsCanvas
		moveToX: 0.5475 y: 1.1222;
		lineToX: 0.5475 y: 1.1920;
		lineToX: 0.0280 y: 1.1920;
		lineToX: 0.0280 y: 1.1222;
		lineToX: 0.5475 y: 1.1222! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans130
	vectorGraphicsCanvas
		moveToX: 0.4347 y: 0.2238;
		lineToX: 0.4787 y: 0.2238;
		lineToX: 0.6701 y: 0.4161;
		lineToX: 0.6115 y: 0.4747;
		lineToX: 0.4982 y: 0.3605;
		lineToX: 0.4982 y: 0.9562;
		lineToX: 0.4152 y: 0.9562;
		lineToX: 0.4152 y: 0.3605;
		lineToX: 0.3010 y: 0.4747;
		lineToX: 0.2424 y: 0.4161;
		lineToX: 0.4347 y: 0.2238! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans131
	vectorGraphicsCanvas
		moveToX: 0.0866 y: 0.6647;
		lineToX: 0.0866 y: 0.6207;
		lineToX: 0.2785 y: 0.4288;
		lineToX: 0.3371 y: 0.4874;
		lineToX: 0.2233 y: 0.6012;
		lineToX: 0.8185 y: 0.6012;
		lineToX: 0.8185 y: 0.6842;
		lineToX: 0.2233 y: 0.6842;
		lineToX: 0.3371 y: 0.7980;
		lineToX: 0.2785 y: 0.8566;
		lineToX: 0.0866 y: 0.6647! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans161
	vectorGraphicsCanvas
		moveToX: 0.2878 y: 0.3512;
		lineToX: 0.1886 y: 0.3512;
		lineToX: 0.1886 y: 0.2272;
		lineToX: 0.2878 y: 0.2272;
		lineToX: 0.2878 y: 0.3512;
		moveToX: 0.2878 y: 0.9562;
		lineToX: 0.1886 y: 0.9562;
		lineToX: 0.1886 y: 0.6364;
		lineToX: 0.1989 y: 0.4620;
		lineToX: 0.2780 y: 0.4620;
		lineToX: 0.2878 y: 0.6364;
		lineToX: 0.2878 y: 0.9562! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans162
	vectorGraphicsCanvas
		moveToX: 0.3688 y: 0.8922;
		lineToX: 0.3688 y: 0.4742;
		curveToX: 0.2570 y: 0.5382 control1X: 0.3210 y: 0.4801 control2X: 0.2837 y: 0.5014;
		curveToX: 0.2170 y: 0.6832 control1X: 0.2303 y: 0.5750 control2X: 0.2170 y: 0.6233;
		curveToX: 0.2570 y: 0.8287 control1X: 0.2170 y: 0.7435 control2X: 0.2303 y: 0.7920;
		curveToX: 0.3688 y: 0.8922 control1X: 0.2837 y: 0.8655 control2X: 0.3210 y: 0.8867;
		moveToX: 0.5549 y: 0.4303;
		lineToX: 0.5549 y: 0.5143;
		curveToX: 0.4855 y: 0.4840 control1X: 0.5308 y: 0.5009 control2X: 0.5077 y: 0.4908;
		curveToX: 0.4191 y: 0.4723 control1X: 0.4634 y: 0.4772 control2X: 0.4412 y: 0.4733;
		lineToX: 0.4186 y: 0.8942;
		curveToX: 0.4860 y: 0.8815 control1X: 0.4414 y: 0.8925 control2X: 0.4639 y: 0.8883;
		curveToX: 0.5549 y: 0.8522 control1X: 0.5085 y: 0.8746 control2X: 0.5314 y: 0.8649;
		lineToX: 0.5549 y: 0.9352;
		curveToX: 0.4894 y: 0.9581 control1X: 0.5337 y: 0.9450 control2X: 0.5119 y: 0.9526;
		curveToX: 0.4186 y: 0.9703 control1X: 0.4673 y: 0.9640 control2X: 0.4437 y: 0.9681;
		lineToX: 0.4186 y: 1.1090;
		lineToX: 0.3688 y: 1.1090;
		lineToX: 0.3688 y: 0.9684;
		curveToX: 0.1862 y: 0.8839 control1X: 0.2904 y: 0.9619 control2X: 0.2295 y: 0.9337;
		curveToX: 0.1218 y: 0.6832 control1X: 0.1432 y: 0.8338 control2X: 0.1218 y: 0.7669;
		curveToX: 0.1862 y: 0.4825 control1X: 0.1218 y: 0.5992 control2X: 0.1432 y: 0.5324;
		curveToX: 0.3688 y: 0.3971 control1X: 0.2295 y: 0.4327 control2X: 0.2904 y: 0.4043;
		lineToX: 0.3688 y: 0.2570;
		lineToX: 0.4186 y: 0.2570;
		lineToX: 0.4191 y: 0.3971;
		curveToX: 0.4880 y: 0.4074 control1X: 0.4429 y: 0.3984 control2X: 0.4658 y: 0.4018;
		curveToX: 0.5549 y: 0.4303 control1X: 0.5104 y: 0.4126 control2X: 0.5327 y: 0.4202! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans163
	vectorGraphicsCanvas
		moveToX: 0.5759 y: 0.2433;
		lineToX: 0.5759 y: 0.3322;
		curveToX: 0.5055 y: 0.3024 control1X: 0.5511 y: 0.3188 control2X: 0.5277 y: 0.3089;
		curveToX: 0.4426 y: 0.2921 control1X: 0.4834 y: 0.2955 control2X: 0.4624 y: 0.2921;
		curveToX: 0.3420 y: 0.3302 control1X: 0.3944 y: 0.2921 control2X: 0.3608 y: 0.3048;
		curveToX: 0.3136 y: 0.4713 control1X: 0.3231 y: 0.3553 control2X: 0.3136 y: 0.4023;
		lineToX: 0.3136 y: 0.5763;
		lineToX: 0.5046 y: 0.5763;
		lineToX: 0.5046 y: 0.6461;
		lineToX: 0.3136 y: 0.6461;
		lineToX: 0.3136 y: 0.8732;
		lineToX: 0.5856 y: 0.8732;
		lineToX: 0.5856 y: 0.9562;
		lineToX: 0.1008 y: 0.9562;
		lineToX: 0.1008 y: 0.8732;
		lineToX: 0.2160 y: 0.8732;
		lineToX: 0.2160 y: 0.6461;
		lineToX: 0.1188 y: 0.6461;
		lineToX: 0.1188 y: 0.5763;
		lineToX: 0.2160 y: 0.5763;
		lineToX: 0.2160 y: 0.4601;
		curveToX: 0.2682 y: 0.2736 control1X: 0.2160 y: 0.3751 control2X: 0.2334 y: 0.3130;
		curveToX: 0.4338 y: 0.2140 control1X: 0.3031 y: 0.2339 control2X: 0.3582 y: 0.2140;
		curveToX: 0.5002 y: 0.2218 control1X: 0.4536 y: 0.2140 control2X: 0.4758 y: 0.2166;
		curveToX: 0.5759 y: 0.2433 control1X: 0.5249 y: 0.2267 control2X: 0.5501 y: 0.2339! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans164
	vectorGraphicsCanvas
		moveToX: 0.6076 y: 0.2833;
		lineToX: 0.6076 y: 0.3873;
		curveToX: 0.5217 y: 0.3141 control1X: 0.5780 y: 0.3532 control2X: 0.5493 y: 0.3287;
		curveToX: 0.4308 y: 0.2921 control1X: 0.4943 y: 0.2994 control2X: 0.4640 y: 0.2921;
		curveToX: 0.3049 y: 0.3409 control1X: 0.3797 y: 0.2921 control2X: 0.3377 y: 0.3084;
		curveToX: 0.2399 y: 0.4811 control1X: 0.2720 y: 0.3735 control2X: 0.2503 y: 0.4202;
		lineToX: 0.5217 y: 0.4811;
		lineToX: 0.4948 y: 0.5411;
		lineToX: 0.2321 y: 0.5411;
		curveToX: 0.2306 y: 0.5641 control1X: 0.2315 y: 0.5490 control2X: 0.2310 y: 0.5566;
		curveToX: 0.2306 y: 0.5924 control1X: 0.2306 y: 0.5716 control2X: 0.2306 y: 0.5810;
		curveToX: 0.2306 y: 0.6193 control1X: 0.2306 y: 0.6028 control2X: 0.2306 y: 0.6118;
		curveToX: 0.2321 y: 0.6422 control1X: 0.2310 y: 0.6268 control2X: 0.2315 y: 0.6344;
		lineToX: 0.4499 y: 0.6422;
		lineToX: 0.4225 y: 0.7023;
		lineToX: 0.2399 y: 0.7023;
		curveToX: 0.3049 y: 0.8429 control1X: 0.2503 y: 0.7631 control2X: 0.2720 y: 0.8100;
		curveToX: 0.4308 y: 0.8922 control1X: 0.3377 y: 0.8758 control2X: 0.3797 y: 0.8922;
		curveToX: 0.5217 y: 0.8702 control1X: 0.4640 y: 0.8922 control2X: 0.4943 y: 0.8849;
		curveToX: 0.6076 y: 0.7970 control1X: 0.5493 y: 0.8556 control2X: 0.5780 y: 0.8312;
		lineToX: 0.6076 y: 0.9000;
		curveToX: 0.5187 y: 0.9528 control1X: 0.5786 y: 0.9235 control2X: 0.5490 y: 0.9410;
		curveToX: 0.4269 y: 0.9703 control1X: 0.4888 y: 0.9645 control2X: 0.4582 y: 0.9703;
		curveToX: 0.2355 y: 0.8995 control1X: 0.3498 y: 0.9703 control2X: 0.2860 y: 0.9467;
		curveToX: 0.1408 y: 0.7023 control1X: 0.1854 y: 0.8523 control2X: 0.1538 y: 0.7866;
		lineToX: 0.0378 y: 0.7023;
		lineToX: 0.0646 y: 0.6422;
		lineToX: 0.1325 y: 0.6422;
		curveToX: 0.1320 y: 0.6198 control1X: 0.1325 y: 0.6347 control2X: 0.1323 y: 0.6272;
		curveToX: 0.1320 y: 0.5924 control1X: 0.1320 y: 0.6119 control2X: 0.1320 y: 0.6028;
		curveToX: 0.1320 y: 0.5641 control1X: 0.1320 y: 0.5810 control2X: 0.1320 y: 0.5716;
		curveToX: 0.1325 y: 0.5411 control1X: 0.1323 y: 0.5563 control2X: 0.1325 y: 0.5486;
		lineToX: 0.0378 y: 0.5411;
		lineToX: 0.0646 y: 0.4811;
		lineToX: 0.1408 y: 0.4811;
		curveToX: 0.2360 y: 0.2848 control1X: 0.1538 y: 0.3974 control2X: 0.1856 y: 0.3320;
		curveToX: 0.4269 y: 0.2140 control1X: 0.2865 y: 0.2376 control2X: 0.3501 y: 0.2140;
		curveToX: 0.5197 y: 0.2316 control1X: 0.4588 y: 0.2140 control2X: 0.4898 y: 0.2199;
		curveToX: 0.6076 y: 0.2833 control1X: 0.5500 y: 0.2430 control2X: 0.5793 y: 0.2602! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans165
	vectorGraphicsCanvas
		moveToX: 0.6066 y: 0.7340;
		lineToX: 0.4050 y: 0.7340;
		lineToX: 0.4050 y: 0.9562;
		lineToX: 0.3068 y: 0.9562;
		lineToX: 0.3068 y: 0.7340;
		lineToX: 0.1037 y: 0.7340;
		lineToX: 0.1037 y: 0.6740;
		lineToX: 0.3068 y: 0.6740;
		lineToX: 0.3068 y: 0.6491;
		lineToX: 0.2658 y: 0.5734;
		lineToX: 0.1037 y: 0.5734;
		lineToX: 0.1037 y: 0.5133;
		lineToX: 0.2326 y: 0.5133;
		lineToX: 0.0778 y: 0.2272;
		lineToX: 0.1706 y: 0.2272;
		lineToX: 0.3556 y: 0.5690;
		lineToX: 0.5397 y: 0.2272;
		lineToX: 0.6330 y: 0.2272;
		lineToX: 0.4777 y: 0.5133;
		lineToX: 0.6066 y: 0.5133;
		lineToX: 0.6066 y: 0.5734;
		lineToX: 0.4450 y: 0.5734;
		lineToX: 0.4040 y: 0.6491;
		lineToX: 0.4040 y: 0.6740;
		lineToX: 0.6066 y: 0.6740;
		lineToX: 0.6066 y: 0.7340! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans166
	vectorGraphicsCanvas
		moveToX: 0.5729 y: 0.2511;
		lineToX: 0.5729 y: 0.3473;
		curveToX: 0.4670 y: 0.3073 control1X: 0.5355 y: 0.3294 control2X: 0.5002 y: 0.3160;
		curveToX: 0.3708 y: 0.2941 control1X: 0.4338 y: 0.2985 control2X: 0.4017 y: 0.2941;
		curveToX: 0.2463 y: 0.3253 control1X: 0.3171 y: 0.2941 control2X: 0.2756 y: 0.3045;
		curveToX: 0.2028 y: 0.4142 control1X: 0.2173 y: 0.3462 control2X: 0.2028 y: 0.3758;
		curveToX: 0.2316 y: 0.4874 control1X: 0.2028 y: 0.4464 control2X: 0.2124 y: 0.4708;
		curveToX: 0.3420 y: 0.5270 control1X: 0.2511 y: 0.5037 control2X: 0.2879 y: 0.5169;
		lineToX: 0.4015 y: 0.5392;
		curveToX: 0.5641 y: 0.6134 control1X: 0.4751 y: 0.5532 control2X: 0.5293 y: 0.5779;
		curveToX: 0.6169 y: 0.7550 control1X: 0.5993 y: 0.6486 control2X: 0.6169 y: 0.6958;
		curveToX: 0.5456 y: 0.9157 control1X: 0.6169 y: 0.8256 control2X: 0.5931 y: 0.8792;
		curveToX: 0.3376 y: 0.9703 control1X: 0.4984 y: 0.9521 control2X: 0.4290 y: 0.9703;
		curveToX: 0.2272 y: 0.9586 control1X: 0.3031 y: 0.9703 control2X: 0.2663 y: 0.9664;
		curveToX: 0.1066 y: 0.9240 control1X: 0.1885 y: 0.9508 control2X: 0.1483 y: 0.9393;
		lineToX: 0.1066 y: 0.8224;
		curveToX: 0.2243 y: 0.8732 control1X: 0.1467 y: 0.8449 control2X: 0.1859 y: 0.8618;
		curveToX: 0.3376 y: 0.8903 control1X: 0.2627 y: 0.8846 control2X: 0.3005 y: 0.8903;
		curveToX: 0.4679 y: 0.8571 control1X: 0.3939 y: 0.8903 control2X: 0.4373 y: 0.8792;
		curveToX: 0.5138 y: 0.7623 control1X: 0.4985 y: 0.8349 control2X: 0.5138 y: 0.8033;
		curveToX: 0.4806 y: 0.6783 control1X: 0.5138 y: 0.7265 control2X: 0.5028 y: 0.6985;
		curveToX: 0.3727 y: 0.6329 control1X: 0.4588 y: 0.6582 control2X: 0.4229 y: 0.6430;
		lineToX: 0.3127 y: 0.6212;
		curveToX: 0.1530 y: 0.5524 control1X: 0.2391 y: 0.6066 control2X: 0.1859 y: 0.5836;
		curveToX: 0.1037 y: 0.4220 control1X: 0.1201 y: 0.5211 control2X: 0.1037 y: 0.4777;
		curveToX: 0.1716 y: 0.2697 control1X: 0.1037 y: 0.3575 control2X: 0.1263 y: 0.3068;
		curveToX: 0.3595 y: 0.2140 control1X: 0.2171 y: 0.2325 control2X: 0.2798 y: 0.2140;
		curveToX: 0.4640 y: 0.2233 control1X: 0.3937 y: 0.2140 control2X: 0.4286 y: 0.2171;
		curveToX: 0.5729 y: 0.2511 control1X: 0.4995 y: 0.2295 control2X: 0.5358 y: 0.2387;
		moveToX: 0.3097 y: 0.1578;
		lineToX: 0.2067 y: 0.0280;
		lineToX: 0.2746 y: 0.0280;
		lineToX: 0.3556 y: 0.1149;
		lineToX: 0.4367 y: 0.0280;
		lineToX: 0.5046 y: 0.0280;
		lineToX: 0.4015 y: 0.1578;
		lineToX: 0.3097 y: 0.1578! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans167
	vectorGraphicsCanvas
		moveToX: 0.2228 y: 0.4992;
		curveToX: 0.1769 y: 0.5436 control1X: 0.2023 y: 0.5141 control2X: 0.1870 y: 0.5289;
		curveToX: 0.1618 y: 0.5880 control1X: 0.1668 y: 0.5582 control2X: 0.1618 y: 0.5730;
		curveToX: 0.1955 y: 0.6578 control1X: 0.1618 y: 0.6128 control2X: 0.1730 y: 0.6360;
		curveToX: 0.3517 y: 0.7560 control1X: 0.2183 y: 0.6793 control2X: 0.2704 y: 0.7120;
		curveToX: 0.3976 y: 0.7120 control1X: 0.3722 y: 0.7413 control2X: 0.3875 y: 0.7267;
		curveToX: 0.4128 y: 0.6671 control1X: 0.4077 y: 0.6971 control2X: 0.4128 y: 0.6821;
		curveToX: 0.3776 y: 0.5973 control1X: 0.4128 y: 0.6427 control2X: 0.4010 y: 0.6194;
		curveToX: 0.2228 y: 0.4992 control1X: 0.3545 y: 0.5752 control2X: 0.3029 y: 0.5424;
		moveToX: 0.4426 y: 0.2423;
		lineToX: 0.4426 y: 0.3224;
		curveToX: 0.3669 y: 0.2941 control1X: 0.4155 y: 0.3097 control2X: 0.3903 y: 0.3003;
		curveToX: 0.3049 y: 0.2843 control1X: 0.3438 y: 0.2876 control2X: 0.3231 y: 0.2843;
		curveToX: 0.2311 y: 0.3038 control1X: 0.2733 y: 0.2843 control2X: 0.2487 y: 0.2908;
		curveToX: 0.2048 y: 0.3580 control1X: 0.2135 y: 0.3169 control2X: 0.2048 y: 0.3349;
		curveToX: 0.3053 y: 0.4581 control1X: 0.2048 y: 0.3873 control2X: 0.2383 y: 0.4207;
		curveToX: 0.3249 y: 0.4694 control1X: 0.3138 y: 0.4630 control2X: 0.3203 y: 0.4668;
		curveToX: 0.4596 y: 0.5651 control1X: 0.3936 y: 0.5081 control2X: 0.4385 y: 0.5400;
		curveToX: 0.4919 y: 0.6520 control1X: 0.4811 y: 0.5901 control2X: 0.4919 y: 0.6191;
		curveToX: 0.4694 y: 0.7301 control1X: 0.4919 y: 0.6813 control2X: 0.4844 y: 0.7073;
		curveToX: 0.4015 y: 0.7902 control1X: 0.4544 y: 0.7529 control2X: 0.4318 y: 0.7729;
		curveToX: 0.4455 y: 0.8424 control1X: 0.4217 y: 0.8071 control2X: 0.4364 y: 0.8245;
		curveToX: 0.4596 y: 0.9000 control1X: 0.4549 y: 0.8600 control2X: 0.4596 y: 0.8792;
		curveToX: 0.4098 y: 1.0104 control1X: 0.4596 y: 0.9463 control2X: 0.4430 y: 0.9830;
		curveToX: 0.2756 y: 1.0514 control1X: 0.3766 y: 1.0377 control2X: 0.3319 y: 1.0514;
		curveToX: 0.2023 y: 1.0441 control1X: 0.2518 y: 1.0514 control2X: 0.2274 y: 1.0490;
		curveToX: 0.1218 y: 1.0231 control1X: 0.1773 y: 1.0395 control2X: 0.1504 y: 1.0325;
		lineToX: 0.1218 y: 0.9430;
		curveToX: 0.2004 y: 0.9713 control1X: 0.1501 y: 0.9557 control2X: 0.1763 y: 0.9651;
		curveToX: 0.2648 y: 0.9811 control1X: 0.2245 y: 0.9778 control2X: 0.2459 y: 0.9811;
		curveToX: 0.3420 y: 0.9606 control1X: 0.2980 y: 0.9811 control2X: 0.3237 y: 0.9742;
		curveToX: 0.3698 y: 0.9030 control1X: 0.3605 y: 0.9469 control2X: 0.3698 y: 0.9277;
		curveToX: 0.2619 y: 0.7931 control1X: 0.3698 y: 0.8698 control2X: 0.3338 y: 0.8331;
		lineToX: 0.2497 y: 0.7863;
		curveToX: 0.1139 y: 0.6906 control1X: 0.1800 y: 0.7472 control2X: 0.1348 y: 0.7153;
		curveToX: 0.0827 y: 0.6032 control1X: 0.0931 y: 0.6655 control2X: 0.0827 y: 0.6364;
		curveToX: 0.1051 y: 0.5241 control1X: 0.0827 y: 0.5735 control2X: 0.0902 y: 0.5472;
		curveToX: 0.1725 y: 0.4650 control1X: 0.1204 y: 0.5006 control2X: 0.1429 y: 0.4809;
		curveToX: 0.1291 y: 0.4161 control1X: 0.1533 y: 0.4506 control2X: 0.1388 y: 0.4344;
		curveToX: 0.1149 y: 0.3551 control1X: 0.1196 y: 0.3979 control2X: 0.1149 y: 0.3776;
		curveToX: 0.1637 y: 0.2531 control1X: 0.1149 y: 0.3128 control2X: 0.1312 y: 0.2788;
		curveToX: 0.2936 y: 0.2140 control1X: 0.1963 y: 0.2270 control2X: 0.2396 y: 0.2140;
		curveToX: 0.3664 y: 0.2213 control1X: 0.3174 y: 0.2140 control2X: 0.3416 y: 0.2164;
		curveToX: 0.4426 y: 0.2423 control1X: 0.3914 y: 0.2259 control2X: 0.4168 y: 0.2329! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans168
	vectorGraphicsCanvas
		moveToX: 0.4806 y: 0.4254;
		lineToX: 0.4806 y: 0.5104;
		curveToX: 0.4015 y: 0.4811 control1X: 0.4552 y: 0.4974 control2X: 0.4289 y: 0.4876;
		curveToX: 0.3166 y: 0.4713 control1X: 0.3742 y: 0.4746 control2X: 0.3459 y: 0.4713;
		curveToX: 0.2160 y: 0.4918 control1X: 0.2720 y: 0.4713 control2X: 0.2385 y: 0.4782;
		curveToX: 0.1828 y: 0.5533 control1X: 0.1939 y: 0.5055 control2X: 0.1828 y: 0.5260;
		curveToX: 0.2067 y: 0.6027 control1X: 0.1828 y: 0.5742 control2X: 0.1908 y: 0.5906;
		curveToX: 0.3029 y: 0.6364 control1X: 0.2227 y: 0.6144 control2X: 0.2547 y: 0.6256;
		lineToX: 0.3337 y: 0.6432;
		curveToX: 0.4694 y: 0.7013 control1X: 0.3975 y: 0.6569 control2X: 0.4427 y: 0.6762;
		curveToX: 0.5099 y: 0.8053 control1X: 0.4964 y: 0.7260 control2X: 0.5099 y: 0.7607;
		curveToX: 0.4494 y: 0.9259 control1X: 0.5099 y: 0.8561 control2X: 0.4898 y: 0.8963;
		curveToX: 0.2839 y: 0.9703 control1X: 0.4093 y: 0.9555 control2X: 0.3542 y: 0.9703;
		curveToX: 0.1921 y: 0.9616 control1X: 0.2546 y: 0.9703 control2X: 0.2240 y: 0.9674;
		curveToX: 0.0920 y: 0.9362 control1X: 0.1605 y: 0.9560 control2X: 0.1271 y: 0.9476;
		lineToX: 0.0920 y: 0.8434;
		curveToX: 0.1901 y: 0.8825 control1X: 0.1252 y: 0.8606 control2X: 0.1579 y: 0.8737;
		curveToX: 0.2858 y: 0.8951 control1X: 0.2223 y: 0.8909 control2X: 0.2542 y: 0.8951;
		curveToX: 0.3835 y: 0.8737 control1X: 0.3281 y: 0.8951 control2X: 0.3607 y: 0.8880;
		curveToX: 0.4176 y: 0.8121 control1X: 0.4063 y: 0.8590 control2X: 0.4176 y: 0.8385;
		curveToX: 0.3927 y: 0.7560 control1X: 0.4176 y: 0.7877 control2X: 0.4093 y: 0.7690;
		curveToX: 0.2848 y: 0.7184 control1X: 0.3765 y: 0.7430 control2X: 0.3405 y: 0.7304;
		lineToX: 0.2536 y: 0.7111;
		curveToX: 0.1330 y: 0.6574 control1X: 0.1979 y: 0.6993 control2X: 0.1577 y: 0.6814;
		curveToX: 0.0959 y: 0.5573 control1X: 0.1082 y: 0.6329 control2X: 0.0959 y: 0.5996;
		curveToX: 0.1506 y: 0.4381 control1X: 0.0959 y: 0.5058 control2X: 0.1141 y: 0.4661;
		curveToX: 0.3058 y: 0.3961 control1X: 0.1870 y: 0.4101 control2X: 0.2388 y: 0.3961;
		curveToX: 0.3996 y: 0.4034 control1X: 0.3390 y: 0.3961 control2X: 0.3703 y: 0.3986;
		curveToX: 0.4806 y: 0.4254 control1X: 0.4289 y: 0.4083 control2X: 0.4559 y: 0.4157;
		moveToX: 0.2629 y: 0.3400;
		lineToX: 0.1432 y: 0.1564;
		lineToX: 0.2111 y: 0.1564;
		lineToX: 0.2990 y: 0.2760;
		lineToX: 0.3869 y: 0.1564;
		lineToX: 0.4548 y: 0.1564;
		lineToX: 0.3351 y: 0.3400;
		lineToX: 0.2629 y: 0.3400! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans169
	vectorGraphicsCanvas
		moveToX: 0.5378 y: 0.2311;
		curveToX: 0.6760 y: 0.2579 control1X: 0.5872 y: 0.2311 control2X: 0.6333 y: 0.2400;
		curveToX: 0.7936 y: 0.3380 control1X: 0.7189 y: 0.2758 control2X: 0.7581 y: 0.3025;
		curveToX: 0.8732 y: 0.4552 control1X: 0.8291 y: 0.3735 control2X: 0.8556 y: 0.4126;
		curveToX: 0.8996 y: 0.5944 control1X: 0.8908 y: 0.4978 control2X: 0.8996 y: 0.5442;
		curveToX: 0.8732 y: 0.7325 control1X: 0.8996 y: 0.6438 control2X: 0.8908 y: 0.6899;
		curveToX: 0.7936 y: 0.8492 control1X: 0.8556 y: 0.7749 control2X: 0.8291 y: 0.8138;
		curveToX: 0.6760 y: 0.9293 control1X: 0.7581 y: 0.8847 control2X: 0.7189 y: 0.9114;
		curveToX: 0.5378 y: 0.9562 control1X: 0.6333 y: 0.9472 control2X: 0.5872 y: 0.9562;
		curveToX: 0.3991 y: 0.9293 control1X: 0.4883 y: 0.9562 control2X: 0.4421 y: 0.9472;
		curveToX: 0.2819 y: 0.8492 control1X: 0.3565 y: 0.9114 control2X: 0.3174 y: 0.8847;
		curveToX: 0.2023 y: 0.7325 control1X: 0.2464 y: 0.8138 control2X: 0.2199 y: 0.7749;
		curveToX: 0.1760 y: 0.5944 control1X: 0.1847 y: 0.6899 control2X: 0.1760 y: 0.6438;
		curveToX: 0.2023 y: 0.4552 control1X: 0.1760 y: 0.5442 control2X: 0.1847 y: 0.4978;
		curveToX: 0.2819 y: 0.3380 control1X: 0.2199 y: 0.4126 control2X: 0.2464 y: 0.3735;
		curveToX: 0.3991 y: 0.2579 control1X: 0.3174 y: 0.3025 control2X: 0.3565 y: 0.2758;
		curveToX: 0.5378 y: 0.2311 control1X: 0.4421 y: 0.2400 control2X: 0.4883 y: 0.2311;
		moveToX: 0.5378 y: 0.2814;
		curveToX: 0.4186 y: 0.3043 control1X: 0.4951 y: 0.2814 control2X: 0.4554 y: 0.2890;
		curveToX: 0.3176 y: 0.3732 control1X: 0.3818 y: 0.3196 control2X: 0.3482 y: 0.3426;
		curveToX: 0.2482 y: 0.4752 control1X: 0.2870 y: 0.4038 control2X: 0.2638 y: 0.4378;
		curveToX: 0.2248 y: 0.5944 control1X: 0.2326 y: 0.5123 control2X: 0.2248 y: 0.5520;
		curveToX: 0.2482 y: 0.7130 control1X: 0.2248 y: 0.6364 control2X: 0.2326 y: 0.6759;
		curveToX: 0.3176 y: 0.8141 control1X: 0.2638 y: 0.7498 control2X: 0.2870 y: 0.7835;
		curveToX: 0.4186 y: 0.8834 control1X: 0.3482 y: 0.8447 control2X: 0.3818 y: 0.8678;
		curveToX: 0.5378 y: 0.9064 control1X: 0.4554 y: 0.8987 control2X: 0.4951 y: 0.9064;
		curveToX: 0.6574 y: 0.8834 control1X: 0.5807 y: 0.9064 control2X: 0.6206 y: 0.8987;
		curveToX: 0.7590 y: 0.8141 control1X: 0.6945 y: 0.8678 control2X: 0.7284 y: 0.8447;
		curveToX: 0.8268 y: 0.7135 control1X: 0.7892 y: 0.7838 control2X: 0.8119 y: 0.7503;
		curveToX: 0.8498 y: 0.5944 control1X: 0.8421 y: 0.6767 control2X: 0.8498 y: 0.6370;
		curveToX: 0.8268 y: 0.4742 control1X: 0.8498 y: 0.5511 control2X: 0.8421 y: 0.5110;
		curveToX: 0.7590 y: 0.3732 control1X: 0.8115 y: 0.4371 control2X: 0.7889 y: 0.4034;
		curveToX: 0.6574 y: 0.3043 control1X: 0.7284 y: 0.3426 control2X: 0.6945 y: 0.3196;
		curveToX: 0.5378 y: 0.2814 control1X: 0.6206 y: 0.2890 control2X: 0.5807 y: 0.2814;
		moveToX: 0.6838 y: 0.4010;
		lineToX: 0.6838 y: 0.4640;
		curveToX: 0.6198 y: 0.4401 control1X: 0.6623 y: 0.4533 control2X: 0.6410 y: 0.4453;
		curveToX: 0.5558 y: 0.4323 control1X: 0.5986 y: 0.4349 control2X: 0.5773 y: 0.4323;
		curveToX: 0.4421 y: 0.4752 control1X: 0.5073 y: 0.4323 control2X: 0.4694 y: 0.4466;
		curveToX: 0.4015 y: 0.5944 control1X: 0.4150 y: 0.5035 control2X: 0.4015 y: 0.5433;
		curveToX: 0.4430 y: 0.7145 control1X: 0.4015 y: 0.6461 control2X: 0.4154 y: 0.6862;
		curveToX: 0.5607 y: 0.7570 control1X: 0.4710 y: 0.7428 control2X: 0.5103 y: 0.7570;
		curveToX: 0.6218 y: 0.7496 control1X: 0.5815 y: 0.7570 control2X: 0.6019 y: 0.7545;
		curveToX: 0.6838 y: 0.7252 control1X: 0.6416 y: 0.7444 control2X: 0.6623 y: 0.7363;
		lineToX: 0.6838 y: 0.7872;
		curveToX: 0.6174 y: 0.8082 control1X: 0.6620 y: 0.7967 control2X: 0.6398 y: 0.8037;
		curveToX: 0.5500 y: 0.8151 control1X: 0.5952 y: 0.8128 control2X: 0.5728 y: 0.8151;
		curveToX: 0.3830 y: 0.7555 control1X: 0.4797 y: 0.8151 control2X: 0.4240 y: 0.7952;
		curveToX: 0.3219 y: 0.5944 control1X: 0.3423 y: 0.7158 control2X: 0.3219 y: 0.6621;
		curveToX: 0.3830 y: 0.4332 control1X: 0.3219 y: 0.5263 control2X: 0.3423 y: 0.4726;
		curveToX: 0.5500 y: 0.3742 control1X: 0.4240 y: 0.3938 control2X: 0.4797 y: 0.3742;
		curveToX: 0.6188 y: 0.3810 control1X: 0.5737 y: 0.3742 control2X: 0.5967 y: 0.3764;
		curveToX: 0.6838 y: 0.4010 control1X: 0.6410 y: 0.3855 control2X: 0.6626 y: 0.3922! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans170
	vectorGraphicsCanvas
		moveToX: 0.1056 y: 0.6671;
		lineToX: 0.4416 y: 0.6671;
		lineToX: 0.4416 y: 0.7272;
		lineToX: 0.1056 y: 0.7272;
		lineToX: 0.1056 y: 0.6671;
		moveToX: 0.4406 y: 0.3873;
		lineToX: 0.4406 y: 0.6061;
		lineToX: 0.3678 y: 0.6061;
		lineToX: 0.3678 y: 0.5514;
		curveToX: 0.3112 y: 0.5988 control1X: 0.3535 y: 0.5719 control2X: 0.3346 y: 0.5877;
		curveToX: 0.2306 y: 0.6154 control1X: 0.2878 y: 0.6098 control2X: 0.2609 y: 0.6154;
		curveToX: 0.1310 y: 0.5826 control1X: 0.1890 y: 0.6154 control2X: 0.1558 y: 0.6045;
		curveToX: 0.0939 y: 0.4943 control1X: 0.1063 y: 0.5605 control2X: 0.0939 y: 0.5311;
		curveToX: 0.1403 y: 0.3947 control1X: 0.0939 y: 0.4503 control2X: 0.1094 y: 0.4171;
		curveToX: 0.2790 y: 0.3610 control1X: 0.1716 y: 0.3722 control2X: 0.2178 y: 0.3610;
		lineToX: 0.3678 y: 0.3610;
		lineToX: 0.3678 y: 0.3590;
		curveToX: 0.3390 y: 0.2926 control1X: 0.3678 y: 0.3294 control2X: 0.3582 y: 0.3073;
		curveToX: 0.2536 y: 0.2701 control1X: 0.3202 y: 0.2776 control2X: 0.2917 y: 0.2701;
		curveToX: 0.1901 y: 0.2784 control1X: 0.2334 y: 0.2701 control2X: 0.2122 y: 0.2729;
		curveToX: 0.1237 y: 0.3033 control1X: 0.1680 y: 0.2840 control2X: 0.1458 y: 0.2923;
		lineToX: 0.1237 y: 0.2413;
		curveToX: 0.1945 y: 0.2208 control1X: 0.1475 y: 0.2322 control2X: 0.1711 y: 0.2254;
		curveToX: 0.2638 y: 0.2140 control1X: 0.2183 y: 0.2163 control2X: 0.2414 y: 0.2140;
		curveToX: 0.3967 y: 0.2570 control1X: 0.3234 y: 0.2140 control2X: 0.3677 y: 0.2283;
		curveToX: 0.4406 y: 0.3873 control1X: 0.4260 y: 0.2856 control2X: 0.4406 y: 0.3291;
		moveToX: 0.3058 y: 0.4171;
		curveToX: 0.1965 y: 0.4337 control1X: 0.2534 y: 0.4171 control2X: 0.2170 y: 0.4227;
		curveToX: 0.1657 y: 0.4884 control1X: 0.1760 y: 0.4445 control2X: 0.1657 y: 0.4627;
		curveToX: 0.1896 y: 0.5387 control1X: 0.1657 y: 0.5092 control2X: 0.1737 y: 0.5260;
		curveToX: 0.2536 y: 0.5573 control1X: 0.2056 y: 0.5511 control2X: 0.2269 y: 0.5573;
		curveToX: 0.3361 y: 0.5221 control1X: 0.2874 y: 0.5573 control2X: 0.3149 y: 0.5455;
		curveToX: 0.3678 y: 0.4313 control1X: 0.3573 y: 0.4987 control2X: 0.3678 y: 0.4684;
		lineToX: 0.3678 y: 0.4171;
		lineToX: 0.3058 y: 0.4171! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans171
	vectorGraphicsCanvas
		moveToX: 0.5558 y: 0.4391;
		lineToX: 0.5558 y: 0.5324;
		lineToX: 0.4089 y: 0.6632;
		lineToX: 0.5558 y: 0.7941;
		lineToX: 0.5558 y: 0.8873;
		lineToX: 0.3268 y: 0.6832;
		lineToX: 0.3268 y: 0.6432;
		lineToX: 0.5558 y: 0.4391;
		moveToX: 0.3439 y: 0.4391;
		lineToX: 0.3439 y: 0.5324;
		lineToX: 0.1969 y: 0.6632;
		lineToX: 0.3439 y: 0.7941;
		lineToX: 0.3439 y: 0.8873;
		lineToX: 0.1149 y: 0.6832;
		lineToX: 0.1149 y: 0.6432;
		lineToX: 0.3439 y: 0.4391! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans172
	vectorGraphicsCanvas
		moveToX: 0.1437 y: 0.5353;
		lineToX: 0.7697 y: 0.5353;
		lineToX: 0.7697 y: 0.8160;
		lineToX: 0.6877 y: 0.8160;
		lineToX: 0.6877 y: 0.6183;
		lineToX: 0.1437 y: 0.6183;
		lineToX: 0.1437 y: 0.5353! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans173
	vectorGraphicsCanvas
		moveToX: 0.0866 y: 0.6422;
		lineToX: 0.3498 y: 0.6422;
		lineToX: 0.3498 y: 0.7223;
		lineToX: 0.0866 y: 0.7223;
		lineToX: 0.0866 y: 0.6422! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans174
	vectorGraphicsCanvas
		moveToX: 0.5378 y: 0.2814;
		curveToX: 0.4186 y: 0.3043 control1X: 0.4951 y: 0.2814 control2X: 0.4554 y: 0.2890;
		curveToX: 0.3176 y: 0.3732 control1X: 0.3818 y: 0.3196 control2X: 0.3482 y: 0.3426;
		curveToX: 0.2482 y: 0.4752 control1X: 0.2870 y: 0.4038 control2X: 0.2638 y: 0.4378;
		curveToX: 0.2248 y: 0.5944 control1X: 0.2326 y: 0.5123 control2X: 0.2248 y: 0.5520;
		curveToX: 0.2482 y: 0.7130 control1X: 0.2248 y: 0.6364 control2X: 0.2326 y: 0.6759;
		curveToX: 0.3176 y: 0.8141 control1X: 0.2638 y: 0.7498 control2X: 0.2870 y: 0.7835;
		curveToX: 0.4186 y: 0.8834 control1X: 0.3482 y: 0.8447 control2X: 0.3818 y: 0.8678;
		curveToX: 0.5378 y: 0.9064 control1X: 0.4554 y: 0.8987 control2X: 0.4951 y: 0.9064;
		curveToX: 0.6574 y: 0.8834 control1X: 0.5807 y: 0.9064 control2X: 0.6206 y: 0.8987;
		curveToX: 0.7590 y: 0.8141 control1X: 0.6945 y: 0.8678 control2X: 0.7284 y: 0.8447;
		curveToX: 0.8268 y: 0.7135 control1X: 0.7892 y: 0.7838 control2X: 0.8119 y: 0.7503;
		curveToX: 0.8498 y: 0.5944 control1X: 0.8421 y: 0.6767 control2X: 0.8498 y: 0.6370;
		curveToX: 0.8268 y: 0.4742 control1X: 0.8498 y: 0.5511 control2X: 0.8421 y: 0.5110;
		curveToX: 0.7590 y: 0.3732 control1X: 0.8115 y: 0.4371 control2X: 0.7889 y: 0.4034;
		curveToX: 0.6574 y: 0.3043 control1X: 0.7284 y: 0.3426 control2X: 0.6945 y: 0.3196;
		curveToX: 0.5378 y: 0.2814 control1X: 0.6206 y: 0.2890 control2X: 0.5807 y: 0.2814;
		moveToX: 0.5378 y: 0.2311;
		curveToX: 0.6760 y: 0.2579 control1X: 0.5872 y: 0.2311 control2X: 0.6333 y: 0.2400;
		curveToX: 0.7936 y: 0.3380 control1X: 0.7189 y: 0.2758 control2X: 0.7581 y: 0.3025;
		curveToX: 0.8732 y: 0.4552 control1X: 0.8291 y: 0.3735 control2X: 0.8556 y: 0.4126;
		curveToX: 0.8996 y: 0.5944 control1X: 0.8908 y: 0.4978 control2X: 0.8996 y: 0.5442;
		curveToX: 0.8732 y: 0.7325 control1X: 0.8996 y: 0.6438 control2X: 0.8908 y: 0.6899;
		curveToX: 0.7936 y: 0.8492 control1X: 0.8556 y: 0.7749 control2X: 0.8291 y: 0.8138;
		curveToX: 0.6760 y: 0.9293 control1X: 0.7581 y: 0.8847 control2X: 0.7189 y: 0.9114;
		curveToX: 0.5378 y: 0.9562 control1X: 0.6333 y: 0.9472 control2X: 0.5872 y: 0.9562;
		curveToX: 0.3991 y: 0.9293 control1X: 0.4883 y: 0.9562 control2X: 0.4421 y: 0.9472;
		curveToX: 0.2819 y: 0.8492 control1X: 0.3565 y: 0.9114 control2X: 0.3174 y: 0.8847;
		curveToX: 0.2023 y: 0.7325 control1X: 0.2464 y: 0.8138 control2X: 0.2199 y: 0.7749;
		curveToX: 0.1760 y: 0.5944 control1X: 0.1847 y: 0.6899 control2X: 0.1760 y: 0.6438;
		curveToX: 0.2023 y: 0.4552 control1X: 0.1760 y: 0.5442 control2X: 0.1847 y: 0.4978;
		curveToX: 0.2819 y: 0.3380 control1X: 0.2199 y: 0.4126 control2X: 0.2464 y: 0.3735;
		curveToX: 0.3991 y: 0.2579 control1X: 0.3174 y: 0.3025 control2X: 0.3565 y: 0.2758;
		curveToX: 0.5378 y: 0.2311 control1X: 0.4421 y: 0.2400 control2X: 0.4883 y: 0.2311;
		moveToX: 0.5246 y: 0.4332;
		lineToX: 0.4645 y: 0.4332;
		lineToX: 0.4645 y: 0.5680;
		lineToX: 0.5246 y: 0.5680;
		curveToX: 0.5993 y: 0.5529 control1X: 0.5604 y: 0.5680 control2X: 0.5853 y: 0.5630;
		curveToX: 0.6208 y: 0.5011 control1X: 0.6136 y: 0.5428 control2X: 0.6208 y: 0.5255;
		curveToX: 0.5993 y: 0.4489 control1X: 0.6208 y: 0.4764 control2X: 0.6136 y: 0.4589;
		curveToX: 0.5246 y: 0.4332 control1X: 0.5850 y: 0.4384 control2X: 0.5601 y: 0.4332;
		moveToX: 0.5280 y: 0.3829;
		curveToX: 0.6564 y: 0.4122 control1X: 0.5853 y: 0.3829 control2X: 0.6281 y: 0.3927;
		curveToX: 0.6989 y: 0.5001 control1X: 0.6847 y: 0.4314 control2X: 0.6989 y: 0.4607;
		curveToX: 0.6730 y: 0.5695 control1X: 0.6989 y: 0.5281 control2X: 0.6903 y: 0.5512;
		curveToX: 0.6008 y: 0.6051 control1X: 0.6561 y: 0.5877 control2X: 0.6320 y: 0.5996;
		curveToX: 0.6286 y: 0.6281 control1X: 0.6086 y: 0.6077 control2X: 0.6178 y: 0.6154;
		curveToX: 0.6676 y: 0.6823 control1X: 0.6397 y: 0.6404 control2X: 0.6527 y: 0.6585;
		lineToX: 0.7345 y: 0.7911;
		lineToX: 0.6506 y: 0.7911;
		lineToX: 0.5876 y: 0.6891;
		curveToX: 0.5407 y: 0.6300 control1X: 0.5684 y: 0.6578 control2X: 0.5527 y: 0.6381;
		curveToX: 0.4968 y: 0.6173 control1X: 0.5290 y: 0.6215 control2X: 0.5143 y: 0.6173;
		lineToX: 0.4645 y: 0.6173;
		lineToX: 0.4645 y: 0.7911;
		lineToX: 0.3888 y: 0.7911;
		lineToX: 0.3888 y: 0.3829;
		lineToX: 0.5280 y: 0.3829! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans175
	vectorGraphicsCanvas
		moveToX: 0.1418 y: 0.2111;
		lineToX: 0.4338 y: 0.2111;
		lineToX: 0.4338 y: 0.2833;
		lineToX: 0.1418 y: 0.2833;
		lineToX: 0.1418 y: 0.2111! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans176
	vectorGraphicsCanvas
		moveToX: 0.2878 y: 0.2770;
		curveToX: 0.2218 y: 0.3043 control1X: 0.2617 y: 0.2770 control2X: 0.2398 y: 0.2861;
		curveToX: 0.1950 y: 0.3702 control1X: 0.2039 y: 0.3222 control2X: 0.1950 y: 0.3442;
		curveToX: 0.2218 y: 0.4357 control1X: 0.1950 y: 0.3960 control2X: 0.2039 y: 0.4178;
		curveToX: 0.2878 y: 0.4620 control1X: 0.2398 y: 0.4533 control2X: 0.2617 y: 0.4620;
		curveToX: 0.3537 y: 0.4357 control1X: 0.3138 y: 0.4620 control2X: 0.3358 y: 0.4533;
		curveToX: 0.3805 y: 0.3702 control1X: 0.3716 y: 0.4178 control2X: 0.3805 y: 0.3960;
		curveToX: 0.3532 y: 0.3043 control1X: 0.3805 y: 0.3445 control2X: 0.3714 y: 0.3226;
		curveToX: 0.2878 y: 0.2770 control1X: 0.3353 y: 0.2861 control2X: 0.3135 y: 0.2770;
		moveToX: 0.2878 y: 0.2140;
		curveToX: 0.3478 y: 0.2262 control1X: 0.3086 y: 0.2140 control2X: 0.3286 y: 0.2181;
		curveToX: 0.3976 y: 0.2604 control1X: 0.3670 y: 0.2340 control2X: 0.3836 y: 0.2454;
		curveToX: 0.4313 y: 0.3102 control1X: 0.4126 y: 0.2750 control2X: 0.4238 y: 0.2916;
		curveToX: 0.4426 y: 0.3702 control1X: 0.4388 y: 0.3287 control2X: 0.4426 y: 0.3488;
		curveToX: 0.3971 y: 0.4796 control1X: 0.4426 y: 0.4132 control2X: 0.4274 y: 0.4497;
		curveToX: 0.2868 y: 0.5241 control1X: 0.3672 y: 0.5092 control2X: 0.3304 y: 0.5241;
		curveToX: 0.1769 y: 0.4801 control1X: 0.2428 y: 0.5241 control2X: 0.2062 y: 0.5094;
		curveToX: 0.1330 y: 0.3702 control1X: 0.1476 y: 0.4508 control2X: 0.1330 y: 0.4142;
		curveToX: 0.1779 y: 0.2594 control1X: 0.1330 y: 0.3266 control2X: 0.1480 y: 0.2897;
		curveToX: 0.2878 y: 0.2140 control1X: 0.2079 y: 0.2291 control2X: 0.2445 y: 0.2140! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans177
	vectorGraphicsCanvas
		moveToX: 0.4977 y: 0.3292;
		lineToX: 0.4977 y: 0.5182;
		lineToX: 0.7697 y: 0.5182;
		lineToX: 0.7697 y: 0.6012;
		lineToX: 0.4977 y: 0.6012;
		lineToX: 0.4977 y: 0.7902;
		lineToX: 0.4157 y: 0.7902;
		lineToX: 0.4157 y: 0.6012;
		lineToX: 0.1437 y: 0.6012;
		lineToX: 0.1437 y: 0.5182;
		lineToX: 0.4157 y: 0.5182;
		lineToX: 0.4157 y: 0.3292;
		lineToX: 0.4977 y: 0.3292;
		moveToX: 0.1437 y: 0.8732;
		lineToX: 0.7697 y: 0.8732;
		lineToX: 0.7697 y: 0.9562;
		lineToX: 0.1437 y: 0.9562;
		lineToX: 0.1437 y: 0.8732! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans178
	vectorGraphicsCanvas
		moveToX: 0.1686 y: 0.5743;
		lineToX: 0.3757 y: 0.5743;
		lineToX: 0.3757 y: 0.6300;
		lineToX: 0.0837 y: 0.6300;
		lineToX: 0.0837 y: 0.5763;
		curveToX: 0.1310 y: 0.5338 control1X: 0.0947 y: 0.5662 control2X: 0.1105 y: 0.5520;
		curveToX: 0.2990 y: 0.3390 control1X: 0.2430 y: 0.4345 control2X: 0.2990 y: 0.3696;
		curveToX: 0.2736 y: 0.2872 control1X: 0.2990 y: 0.3175 control2X: 0.2905 y: 0.3003;
		curveToX: 0.2067 y: 0.2672 control1X: 0.2567 y: 0.2739 control2X: 0.2344 y: 0.2672;
		curveToX: 0.1515 y: 0.2760 control1X: 0.1898 y: 0.2672 control2X: 0.1714 y: 0.2701;
		curveToX: 0.0866 y: 0.3014 control1X: 0.1317 y: 0.2815 control2X: 0.1100 y: 0.2900;
		lineToX: 0.0866 y: 0.2413;
		curveToX: 0.1564 y: 0.2208 control1X: 0.1117 y: 0.2322 control2X: 0.1349 y: 0.2254;
		curveToX: 0.2170 y: 0.2140 control1X: 0.1782 y: 0.2163 control2X: 0.1984 y: 0.2140;
		curveToX: 0.3302 y: 0.2462 control1X: 0.2642 y: 0.2140 control2X: 0.3019 y: 0.2247;
		curveToX: 0.3727 y: 0.3312 control1X: 0.3586 y: 0.2677 control2X: 0.3727 y: 0.2960;
		curveToX: 0.2111 y: 0.5377 control1X: 0.3727 y: 0.3764 control2X: 0.3189 y: 0.4453;
		curveToX: 0.1686 y: 0.5743 control1X: 0.1929 y: 0.5533 control2X: 0.1787 y: 0.5656! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans179
	vectorGraphicsCanvas
		moveToX: 0.2936 y: 0.4093;
		curveToX: 0.3630 y: 0.4450 control1X: 0.3236 y: 0.4152 control2X: 0.3467 y: 0.4270;
		curveToX: 0.3879 y: 0.5114 control1X: 0.3796 y: 0.4625 control2X: 0.3879 y: 0.4847;
		curveToX: 0.3415 y: 0.6046 control1X: 0.3879 y: 0.5517 control2X: 0.3724 y: 0.5828;
		curveToX: 0.2087 y: 0.6373 control1X: 0.3106 y: 0.6264 control2X: 0.2663 y: 0.6373;
		curveToX: 0.1501 y: 0.6325 control1X: 0.1901 y: 0.6373 control2X: 0.1706 y: 0.6357;
		curveToX: 0.0856 y: 0.6173 control1X: 0.1299 y: 0.6289 control2X: 0.1084 y: 0.6238;
		lineToX: 0.0856 y: 0.5582;
		curveToX: 0.1398 y: 0.5778 control1X: 0.1025 y: 0.5670 control2X: 0.1206 y: 0.5735;
		curveToX: 0.2018 y: 0.5841 control1X: 0.1593 y: 0.5820 control2X: 0.1800 y: 0.5841;
		curveToX: 0.2843 y: 0.5651 control1X: 0.2373 y: 0.5841 control2X: 0.2648 y: 0.5778;
		curveToX: 0.3136 y: 0.5114 control1X: 0.3039 y: 0.5520 control2X: 0.3136 y: 0.5341;
		curveToX: 0.2863 y: 0.4567 control1X: 0.3136 y: 0.4873 control2X: 0.3045 y: 0.4690;
		curveToX: 0.2067 y: 0.4381 control1X: 0.2684 y: 0.4443 control2X: 0.2419 y: 0.4381;
		lineToX: 0.1647 y: 0.4381;
		lineToX: 0.1647 y: 0.3854;
		lineToX: 0.2106 y: 0.3854;
		curveToX: 0.2800 y: 0.3702 control1X: 0.2412 y: 0.3854 control2X: 0.2643 y: 0.3803;
		curveToX: 0.3039 y: 0.3253 control1X: 0.2959 y: 0.3598 control2X: 0.3039 y: 0.3449;
		curveToX: 0.2795 y: 0.2824 control1X: 0.3039 y: 0.3064 control2X: 0.2957 y: 0.2921;
		curveToX: 0.2087 y: 0.2672 control1X: 0.2632 y: 0.2723 control2X: 0.2396 y: 0.2672;
		curveToX: 0.1642 y: 0.2716 control1X: 0.1956 y: 0.2672 control2X: 0.1808 y: 0.2687;
		curveToX: 0.0998 y: 0.2872 control1X: 0.1476 y: 0.2745 control2X: 0.1261 y: 0.2797;
		lineToX: 0.0998 y: 0.2311;
		curveToX: 0.1667 y: 0.2184 control1X: 0.1235 y: 0.2256 control2X: 0.1458 y: 0.2213;
		curveToX: 0.2248 y: 0.2140 control1X: 0.1875 y: 0.2155 control2X: 0.2069 y: 0.2140;
		curveToX: 0.3361 y: 0.2428 control1X: 0.2717 y: 0.2140 control2X: 0.3088 y: 0.2236;
		curveToX: 0.3776 y: 0.3200 control1X: 0.3638 y: 0.2620 control2X: 0.3776 y: 0.2877;
		curveToX: 0.3556 y: 0.3771 control1X: 0.3776 y: 0.3424 control2X: 0.3703 y: 0.3615;
		curveToX: 0.2936 y: 0.4093 control1X: 0.3410 y: 0.3927 control2X: 0.3203 y: 0.4034! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans180
	vectorGraphicsCanvas
		moveToX: 0.0939 y: 0.2272;
		lineToX: 0.6667 y: 0.2272;
		lineToX: 0.6667 y: 0.3024;
		lineToX: 0.2057 y: 0.8732;
		lineToX: 0.6779 y: 0.8732;
		lineToX: 0.6779 y: 0.9562;
		lineToX: 0.0827 y: 0.9562;
		lineToX: 0.0827 y: 0.8810;
		lineToX: 0.5436 y: 0.3102;
		lineToX: 0.0939 y: 0.3102;
		lineToX: 0.0939 y: 0.2272;
		moveToX: 0.3346 y: 0.1578;
		lineToX: 0.2316 y: 0.0280;
		lineToX: 0.2995 y: 0.0280;
		lineToX: 0.3805 y: 0.1149;
		lineToX: 0.4616 y: 0.0280;
		lineToX: 0.5295 y: 0.0280;
		lineToX: 0.4264 y: 0.1578;
		lineToX: 0.3346 y: 0.1578! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans181
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 1.1642;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7492;
		curveToX: 0.2463 y: 0.8561 control1X: 0.2126 y: 0.7964 control2X: 0.2238 y: 0.8320;
		curveToX: 0.3459 y: 0.8922 control1X: 0.2687 y: 0.8802 control2X: 0.3019 y: 0.8922;
		curveToX: 0.4543 y: 0.8512 control1X: 0.3940 y: 0.8922 control2X: 0.4302 y: 0.8785;
		curveToX: 0.4909 y: 0.7282 control1X: 0.4787 y: 0.8239 control2X: 0.4909 y: 0.7828;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.8302;
		curveToX: 0.5890 y: 0.8737 control1X: 0.5807 y: 0.8497 control2X: 0.5835 y: 0.8642;
		curveToX: 0.6159 y: 0.8873 control1X: 0.5949 y: 0.8828 control2X: 0.6038 y: 0.8873;
		curveToX: 0.6281 y: 0.8849 control1X: 0.6188 y: 0.8873 control2X: 0.6229 y: 0.8865;
		curveToX: 0.6496 y: 0.8761 control1X: 0.6333 y: 0.8829 control2X: 0.6405 y: 0.8800;
		lineToX: 0.6496 y: 0.9484;
		curveToX: 0.6115 y: 0.9650 control1X: 0.6362 y: 0.9559 control2X: 0.6235 y: 0.9614;
		curveToX: 0.5768 y: 0.9703 control1X: 0.5998 y: 0.9686 control2X: 0.5882 y: 0.9703;
		curveToX: 0.5231 y: 0.9513 control1X: 0.5544 y: 0.9703 control2X: 0.5365 y: 0.9640;
		curveToX: 0.4958 y: 0.8932 control1X: 0.5098 y: 0.9386 control2X: 0.5007 y: 0.9192;
		curveToX: 0.4357 y: 0.9513 control1X: 0.4795 y: 0.9189 control2X: 0.4595 y: 0.9383;
		curveToX: 0.3527 y: 0.9703 control1X: 0.4123 y: 0.9640 control2X: 0.3846 y: 0.9703;
		curveToX: 0.2677 y: 0.9513 control1X: 0.3195 y: 0.9703 control2X: 0.2912 y: 0.9640;
		curveToX: 0.2126 y: 0.8942 control1X: 0.2446 y: 0.9386 control2X: 0.2262 y: 0.9196;
		lineToX: 0.2126 y: 1.1642;
		lineToX: 0.1227 y: 1.1642! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans182
	vectorGraphicsCanvas
		moveToX: 0.3468 y: 0.2272;
		lineToX: 0.5656 y: 0.2272;
		lineToX: 0.5656 y: 1.0524;
		lineToX: 0.4968 y: 1.0524;
		lineToX: 0.4968 y: 0.2872;
		lineToX: 0.4040 y: 0.2872;
		lineToX: 0.4040 y: 1.0524;
		lineToX: 0.3346 y: 1.0524;
		lineToX: 0.3346 y: 0.6393;
		curveToX: 0.1720 y: 0.5773 control1X: 0.2647 y: 0.6338 control2X: 0.2105 y: 0.6131;
		curveToX: 0.1149 y: 0.4332 control1X: 0.1340 y: 0.5411 control2X: 0.1149 y: 0.4931;
		curveToX: 0.1784 y: 0.2838 control1X: 0.1149 y: 0.3714 control2X: 0.1361 y: 0.3216;
		curveToX: 0.3468 y: 0.2272 control1X: 0.2207 y: 0.2461 control2X: 0.2769 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans183
	vectorGraphicsCanvas
		moveToX: 0.1447 y: 0.5470;
		lineToX: 0.2477 y: 0.5470;
		lineToX: 0.2477 y: 0.6710;
		lineToX: 0.1447 y: 0.6710;
		lineToX: 0.1447 y: 0.5470! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans184
	vectorGraphicsCanvas
		moveToX: 0.0929 y: 0.4093;
		lineToX: 0.5197 y: 0.4093;
		lineToX: 0.5197 y: 0.4913;
		lineToX: 0.1818 y: 0.8844;
		lineToX: 0.5197 y: 0.8844;
		lineToX: 0.5197 y: 0.9562;
		lineToX: 0.0807 y: 0.9562;
		lineToX: 0.0807 y: 0.8742;
		lineToX: 0.4186 y: 0.4811;
		lineToX: 0.0929 y: 0.4811;
		lineToX: 0.0929 y: 0.4093;
		moveToX: 0.2648 y: 0.3400;
		lineToX: 0.1452 y: 0.1564;
		lineToX: 0.2131 y: 0.1564;
		lineToX: 0.3010 y: 0.2760;
		lineToX: 0.3888 y: 0.1564;
		lineToX: 0.4567 y: 0.1564;
		lineToX: 0.3371 y: 0.3400;
		lineToX: 0.2648 y: 0.3400! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans185
	vectorGraphicsCanvas
		moveToX: 0.1139 y: 0.5763;
		lineToX: 0.2135 y: 0.5763;
		lineToX: 0.2135 y: 0.2780;
		lineToX: 0.1047 y: 0.2980;
		lineToX: 0.1047 y: 0.2413;
		lineToX: 0.2170 y: 0.2223;
		lineToX: 0.2839 y: 0.2223;
		lineToX: 0.2839 y: 0.5763;
		lineToX: 0.3840 y: 0.5763;
		lineToX: 0.3840 y: 0.6300;
		lineToX: 0.1139 y: 0.6300;
		lineToX: 0.1139 y: 0.5763! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans186
	vectorGraphicsCanvas
		moveToX: 0.1056 y: 0.6671;
		lineToX: 0.4416 y: 0.6671;
		lineToX: 0.4416 y: 0.7272;
		lineToX: 0.1056 y: 0.7272;
		lineToX: 0.1056 y: 0.6671;
		moveToX: 0.2736 y: 0.2140;
		curveToX: 0.4113 y: 0.2682 control1X: 0.3319 y: 0.2140 control2X: 0.3778 y: 0.2321;
		curveToX: 0.4616 y: 0.4152 control1X: 0.4448 y: 0.3040 control2X: 0.4616 y: 0.3530;
		curveToX: 0.4113 y: 0.5621 control1X: 0.4616 y: 0.4773 control2X: 0.4448 y: 0.5263;
		curveToX: 0.2736 y: 0.6154 control1X: 0.3778 y: 0.5976 control2X: 0.3319 y: 0.6154;
		curveToX: 0.1354 y: 0.5617 control1X: 0.2153 y: 0.6154 control2X: 0.1693 y: 0.5975;
		curveToX: 0.0846 y: 0.4152 control1X: 0.1016 y: 0.5258 control2X: 0.0846 y: 0.4770;
		curveToX: 0.1354 y: 0.2682 control1X: 0.0846 y: 0.3530 control2X: 0.1016 y: 0.3040;
		curveToX: 0.2736 y: 0.2140 control1X: 0.1693 y: 0.2321 control2X: 0.2153 y: 0.2140;
		moveToX: 0.2736 y: 0.2701;
		curveToX: 0.1916 y: 0.3097 control1X: 0.2394 y: 0.2701 control2X: 0.2121 y: 0.2833;
		curveToX: 0.1608 y: 0.4152 control1X: 0.1711 y: 0.3357 control2X: 0.1608 y: 0.3709;
		curveToX: 0.1916 y: 0.5192 control1X: 0.1608 y: 0.4585 control2X: 0.1711 y: 0.4931;
		curveToX: 0.2736 y: 0.5582 control1X: 0.2124 y: 0.5452 control2X: 0.2398 y: 0.5582;
		curveToX: 0.3551 y: 0.5192 control1X: 0.3078 y: 0.5582 control2X: 0.3350 y: 0.5452;
		curveToX: 0.3859 y: 0.4152 control1X: 0.3757 y: 0.4931 control2X: 0.3859 y: 0.4585;
		curveToX: 0.3556 y: 0.3092 control1X: 0.3859 y: 0.3706 control2X: 0.3758 y: 0.3353;
		curveToX: 0.2736 y: 0.2701 control1X: 0.3355 y: 0.2832 control2X: 0.3081 y: 0.2701! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans187
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4391;
		lineToX: 0.3610 y: 0.6432;
		lineToX: 0.3610 y: 0.6832;
		lineToX: 0.1320 y: 0.8873;
		lineToX: 0.1320 y: 0.7941;
		lineToX: 0.2790 y: 0.6632;
		lineToX: 0.1320 y: 0.5324;
		lineToX: 0.1320 y: 0.4391;
		moveToX: 0.3439 y: 0.4391;
		lineToX: 0.5729 y: 0.6432;
		lineToX: 0.5729 y: 0.6832;
		lineToX: 0.3439 y: 0.8873;
		lineToX: 0.3439 y: 0.7941;
		lineToX: 0.4909 y: 0.6632;
		lineToX: 0.3439 y: 0.5324;
		lineToX: 0.3439 y: 0.4391! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans188
	vectorGraphicsCanvas
		moveToX: 1.0348 y: 0.2272;
		lineToX: 1.0348 y: 0.3102;
		lineToX: 0.6725 y: 0.3102;
		lineToX: 0.6725 y: 0.5260;
		lineToX: 1.0197 y: 0.5260;
		lineToX: 1.0197 y: 0.6090;
		lineToX: 0.6725 y: 0.6090;
		lineToX: 0.6725 y: 0.8732;
		lineToX: 1.0436 y: 0.8732;
		lineToX: 1.0436 y: 0.9562;
		lineToX: 0.5236 y: 0.9562;
		curveToX: 0.2028 y: 0.8634 control1X: 0.3827 y: 0.9562 control2X: 0.2757 y: 0.9253;
		curveToX: 0.0939 y: 0.5909 control1X: 0.1302 y: 0.8016 control2X: 0.0939 y: 0.7107;
		curveToX: 0.2028 y: 0.3200 control1X: 0.0939 y: 0.4721 control2X: 0.1302 y: 0.3818;
		curveToX: 0.5236 y: 0.2272 control1X: 0.2757 y: 0.2581 control2X: 0.3827 y: 0.2272;
		lineToX: 1.0348 y: 0.2272;
		moveToX: 0.5739 y: 0.3102;
		lineToX: 0.5109 y: 0.3102;
		curveToX: 0.2760 y: 0.3805 control1X: 0.4064 y: 0.3102 control2X: 0.3281 y: 0.3336;
		curveToX: 0.1979 y: 0.5909 control1X: 0.2240 y: 0.4270 control2X: 0.1979 y: 0.4972;
		curveToX: 0.2760 y: 0.8029 control1X: 0.1979 y: 0.6850 control2X: 0.2240 y: 0.7557;
		curveToX: 0.5109 y: 0.8732 control1X: 0.3281 y: 0.8497 control2X: 0.4064 y: 0.8732;
		lineToX: 0.5739 y: 0.8732;
		lineToX: 0.5739 y: 0.3102! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans189
	vectorGraphicsCanvas
		moveToX: 0.9176 y: 0.6339;
		curveToX: 0.8766 y: 0.5162 control1X: 0.9170 y: 0.5844 control2X: 0.9033 y: 0.5452;
		curveToX: 0.7697 y: 0.4723 control1X: 0.8499 y: 0.4869 control2X: 0.8143 y: 0.4723;
		curveToX: 0.6496 y: 0.5148 control1X: 0.7199 y: 0.4723 control2X: 0.6799 y: 0.4865;
		curveToX: 0.5978 y: 0.6344 control1X: 0.6196 y: 0.5431 control2X: 0.6024 y: 0.5830;
		lineToX: 0.9176 y: 0.6339;
		moveToX: 1.0080 y: 0.6603;
		lineToX: 1.0080 y: 0.7042;
		lineToX: 0.5949 y: 0.7042;
		curveToX: 0.6506 y: 0.8458 control1X: 0.5988 y: 0.7661 control2X: 0.6174 y: 0.8133;
		curveToX: 0.7897 y: 0.8942 control1X: 0.6838 y: 0.8781 control2X: 0.7301 y: 0.8942;
		curveToX: 0.8903 y: 0.8815 control1X: 0.8242 y: 0.8942 control2X: 0.8578 y: 0.8899;
		curveToX: 0.9870 y: 0.8434 control1X: 0.9229 y: 0.8730 control2X: 0.9551 y: 0.8603;
		lineToX: 0.9870 y: 0.9283;
		curveToX: 0.8874 y: 0.9596 control1X: 0.9544 y: 0.9420 control2X: 0.9212 y: 0.9524;
		curveToX: 0.7848 y: 0.9703 control1X: 0.8535 y: 0.9668 control2X: 0.8193 y: 0.9703;
		curveToX: 0.6476 y: 0.9435 control1X: 0.7328 y: 0.9703 control2X: 0.6870 y: 0.9614;
		curveToX: 0.5490 y: 0.8634 control1X: 0.6082 y: 0.9256 control2X: 0.5754 y: 0.8989;
		curveToX: 0.4631 y: 0.9440 control1X: 0.5259 y: 0.8992 control2X: 0.4972 y: 0.9261;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4292 y: 0.9616 control2X: 0.3895 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		curveToX: 0.4635 y: 0.4235 control1X: 0.3895 y: 0.3961 control2X: 0.4294 y: 0.4052;
		curveToX: 0.5475 y: 0.5031 control1X: 0.4981 y: 0.4414 control2X: 0.5260 y: 0.4679;
		curveToX: 0.6423 y: 0.4235 control1X: 0.5729 y: 0.4682 control2X: 0.6045 y: 0.4417;
		curveToX: 0.7687 y: 0.3961 control1X: 0.6800 y: 0.4052 control2X: 0.7222 y: 0.3961;
		curveToX: 0.9435 y: 0.4674 control1X: 0.8423 y: 0.3961 control2X: 0.9006 y: 0.4199;
		curveToX: 1.0080 y: 0.6603 control1X: 0.9865 y: 0.5146 control2X: 1.0080 y: 0.5789;
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans190
	vectorGraphicsCanvas
		moveToX: 0.0358 y: 0.2272;
		lineToX: 0.1418 y: 0.2272;
		lineToX: 0.3439 y: 0.5270;
		lineToX: 0.5446 y: 0.2272;
		lineToX: 0.6506 y: 0.2272;
		lineToX: 0.3927 y: 0.6090;
		lineToX: 0.3927 y: 0.9562;
		lineToX: 0.2936 y: 0.9562;
		lineToX: 0.2936 y: 0.6090;
		lineToX: 0.0358 y: 0.2272;
		moveToX: 0.3898 y: 0.0431;
		lineToX: 0.4889 y: 0.0431;
		lineToX: 0.4889 y: 0.1422;
		lineToX: 0.3898 y: 0.1422;
		lineToX: 0.3898 y: 0.0431;
		moveToX: 0.1989 y: 0.0431;
		lineToX: 0.2980 y: 0.0431;
		lineToX: 0.2980 y: 0.1422;
		lineToX: 0.1989 y: 0.1422;
		lineToX: 0.1989 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans191
	vectorGraphicsCanvas
		moveToX: 0.2819 y: 0.4230;
		lineToX: 0.3747 y: 0.4230;
		lineToX: 0.3747 y: 0.4992;
		curveToX: 0.3610 y: 0.5802 control1X: 0.3747 y: 0.5320 control2X: 0.3701 y: 0.5590;
		curveToX: 0.3039 y: 0.6544 control1X: 0.3522 y: 0.6014 control2X: 0.3332 y: 0.6261;
		lineToX: 0.2599 y: 0.6974;
		curveToX: 0.2189 y: 0.7462 control1X: 0.2411 y: 0.7146 control2X: 0.2274 y: 0.7309;
		curveToX: 0.2067 y: 0.7931 control1X: 0.2108 y: 0.7615 control2X: 0.2067 y: 0.7771;
		curveToX: 0.2385 y: 0.8634 control1X: 0.2067 y: 0.8221 control2X: 0.2173 y: 0.8455;
		curveToX: 0.3239 y: 0.8903 control1X: 0.2599 y: 0.8813 control2X: 0.2884 y: 0.8903;
		curveToX: 0.4059 y: 0.8732 control1X: 0.3493 y: 0.8903 control2X: 0.3766 y: 0.8846;
		curveToX: 0.4968 y: 0.8234 control1X: 0.4352 y: 0.8618 control2X: 0.4655 y: 0.8452;
		lineToX: 0.4968 y: 0.9152;
		curveToX: 0.4040 y: 0.9567 control1X: 0.4662 y: 0.9337 control2X: 0.4352 y: 0.9476;
		curveToX: 0.3068 y: 0.9703 control1X: 0.3727 y: 0.9658 control2X: 0.3403 y: 0.9703;
		curveToX: 0.1623 y: 0.9230 control1X: 0.2469 y: 0.9703 control2X: 0.1987 y: 0.9546;
		curveToX: 0.1076 y: 0.7980 control1X: 0.1258 y: 0.8914 control2X: 0.1076 y: 0.8497;
		curveToX: 0.1252 y: 0.7277 control1X: 0.1076 y: 0.7732 control2X: 0.1135 y: 0.7498;
		curveToX: 0.1867 y: 0.6520 control1X: 0.1372 y: 0.7052 control2X: 0.1577 y: 0.6800;
		lineToX: 0.2297 y: 0.6100;
		curveToX: 0.2624 y: 0.5743 control1X: 0.2453 y: 0.5947 control2X: 0.2562 y: 0.5828;
		curveToX: 0.2756 y: 0.5490 control1X: 0.2686 y: 0.5656 control2X: 0.2730 y: 0.5571;
		curveToX: 0.2804 y: 0.5241 control1X: 0.2778 y: 0.5421 control2X: 0.2795 y: 0.5338;
		curveToX: 0.2819 y: 0.4830 control1X: 0.2814 y: 0.5143 control2X: 0.2819 y: 0.5006;
		lineToX: 0.2819 y: 0.4230;
		moveToX: 0.3776 y: 0.3512;
		lineToX: 0.2790 y: 0.3512;
		lineToX: 0.2790 y: 0.2272;
		lineToX: 0.3776 y: 0.2272;
		lineToX: 0.3776 y: 0.3512! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans192
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272;
		moveToX: 0.3517 y: 0.0289;
		lineToX: 0.4474 y: 0.1578;
		lineToX: 0.3727 y: 0.1578;
		lineToX: 0.2604 y: 0.0289;
		lineToX: 0.3517 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans193
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272;
		moveToX: 0.4064 y: 0.0289;
		lineToX: 0.4968 y: 0.0289;
		lineToX: 0.3854 y: 0.1578;
		lineToX: 0.3107 y: 0.1578;
		lineToX: 0.4064 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans194
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272;
		moveToX: 0.3337 y: 0.0280;
		lineToX: 0.4255 y: 0.0280;
		lineToX: 0.5285 y: 0.1578;
		lineToX: 0.4606 y: 0.1578;
		lineToX: 0.3796 y: 0.0709;
		lineToX: 0.2985 y: 0.1578;
		lineToX: 0.2306 y: 0.1578;
		lineToX: 0.3337 y: 0.0280! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans195
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272;
		moveToX: 0.3776 y: 0.1212;
		lineToX: 0.3498 y: 0.1051;
		curveToX: 0.3298 y: 0.0958 control1X: 0.3416 y: 0.1006 control2X: 0.3350 y: 0.0975;
		curveToX: 0.3166 y: 0.0929 control1X: 0.3249 y: 0.0939 control2X: 0.3205 y: 0.0929;
		curveToX: 0.2892 y: 0.1051 control1X: 0.3049 y: 0.0929 control2X: 0.2957 y: 0.0970;
		curveToX: 0.2795 y: 0.1393 control1X: 0.2827 y: 0.1132 control2X: 0.2795 y: 0.1246;
		lineToX: 0.2795 y: 0.1422;
		lineToX: 0.2184 y: 0.1422;
		curveToX: 0.2433 y: 0.0641 control1X: 0.2184 y: 0.1093 control2X: 0.2267 y: 0.0833;
		curveToX: 0.3107 y: 0.0353 control1X: 0.2603 y: 0.0449 control2X: 0.2827 y: 0.0353;
		curveToX: 0.3429 y: 0.0392 control1X: 0.3224 y: 0.0353 control2X: 0.3332 y: 0.0366;
		curveToX: 0.3815 y: 0.0563 control1X: 0.3530 y: 0.0418 control2X: 0.3659 y: 0.0475;
		lineToX: 0.4093 y: 0.0709;
		curveToX: 0.4289 y: 0.0802 control1X: 0.4168 y: 0.0752 control2X: 0.4233 y: 0.0783;
		curveToX: 0.4445 y: 0.0831 control1X: 0.4344 y: 0.0822 control2X: 0.4396 y: 0.0831;
		curveToX: 0.4699 y: 0.0709 control1X: 0.4549 y: 0.0831 control2X: 0.4634 y: 0.0791;
		curveToX: 0.4797 y: 0.0382 control1X: 0.4764 y: 0.0625 control2X: 0.4797 y: 0.0516;
		lineToX: 0.4797 y: 0.0353;
		lineToX: 0.5407 y: 0.0353;
		curveToX: 0.5143 y: 0.1134 control1X: 0.5400 y: 0.0678 control2X: 0.5313 y: 0.0939;
		curveToX: 0.4484 y: 0.1422 control1X: 0.4977 y: 0.1326 control2X: 0.4758 y: 0.1422;
		curveToX: 0.4172 y: 0.1383 control1X: 0.4373 y: 0.1422 control2X: 0.4269 y: 0.1409;
		curveToX: 0.3776 y: 0.1212 control1X: 0.4077 y: 0.1357 control2X: 0.3945 y: 0.1300! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans196
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272;
		moveToX: 0.4255 y: 0.0431;
		lineToX: 0.5246 y: 0.0431;
		lineToX: 0.5246 y: 0.1422;
		lineToX: 0.4255 y: 0.1422;
		lineToX: 0.4255 y: 0.0431;
		moveToX: 0.2345 y: 0.0431;
		lineToX: 0.3337 y: 0.0431;
		lineToX: 0.3337 y: 0.1422;
		lineToX: 0.2345 y: 0.1422;
		lineToX: 0.2345 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans197
	vectorGraphicsCanvas
		moveToX: 0.4538 y: 0.1622;
		curveToX: 0.4318 y: 0.1100 control1X: 0.4538 y: 0.1417 control2X: 0.4465 y: 0.1243;
		curveToX: 0.3796 y: 0.0880 control1X: 0.4175 y: 0.0953 control2X: 0.4001 y: 0.0880;
		curveToX: 0.3268 y: 0.1095 control1X: 0.3587 y: 0.0880 control2X: 0.3412 y: 0.0952;
		curveToX: 0.3058 y: 0.1622 control1X: 0.3128 y: 0.1235 control2X: 0.3058 y: 0.1411;
		curveToX: 0.3273 y: 0.2145 control1X: 0.3058 y: 0.1827 control2X: 0.3130 y: 0.2002;
		curveToX: 0.3796 y: 0.2360 control1X: 0.3416 y: 0.2288 control2X: 0.3591 y: 0.2360;
		curveToX: 0.4318 y: 0.2145 control1X: 0.4001 y: 0.2360 control2X: 0.4175 y: 0.2288;
		curveToX: 0.4538 y: 0.1622 control1X: 0.4465 y: 0.2002 control2X: 0.4538 y: 0.1827;
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2468 y: 0.6871;
		lineToX: 0.5129 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3058 y: 0.2741;
		curveToX: 0.2609 y: 0.2262 control1X: 0.2860 y: 0.2607 control2X: 0.2710 y: 0.2448;
		curveToX: 0.2458 y: 0.1622 control1X: 0.2508 y: 0.2073 control2X: 0.2458 y: 0.1860;
		curveToX: 0.2843 y: 0.0670 control1X: 0.2458 y: 0.1248 control2X: 0.2586 y: 0.0931;
		curveToX: 0.3796 y: 0.0280 control1X: 0.3104 y: 0.0410 control2X: 0.3421 y: 0.0280;
		curveToX: 0.4743 y: 0.0675 control1X: 0.4167 y: 0.0280 control2X: 0.4482 y: 0.0411;
		curveToX: 0.5138 y: 0.1622 control1X: 0.5007 y: 0.0936 control2X: 0.5138 y: 0.1251;
		curveToX: 0.4982 y: 0.2252 control1X: 0.5138 y: 0.1850 control2X: 0.5086 y: 0.2060;
		curveToX: 0.4538 y: 0.2741 control1X: 0.4881 y: 0.2444 control2X: 0.4733 y: 0.2607;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3058 y: 0.2741! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans198
	vectorGraphicsCanvas
		moveToX: 0.9386 y: 0.2272;
		lineToX: 0.9386 y: 0.3102;
		lineToX: 0.5768 y: 0.3102;
		lineToX: 0.5768 y: 0.5260;
		lineToX: 0.9240 y: 0.5260;
		lineToX: 0.9240 y: 0.6090;
		lineToX: 0.5768 y: 0.6090;
		lineToX: 0.5768 y: 0.8732;
		lineToX: 0.9479 y: 0.8732;
		lineToX: 0.9479 y: 0.9562;
		lineToX: 0.4777 y: 0.9562;
		lineToX: 0.4777 y: 0.7692;
		lineToX: 0.2199 y: 0.7692;
		lineToX: 0.1418 y: 0.9562;
		lineToX: 0.0417 y: 0.9562;
		lineToX: 0.3468 y: 0.2272;
		lineToX: 0.9386 y: 0.2272;
		moveToX: 0.4147 y: 0.3043;
		lineToX: 0.2536 y: 0.6871;
		lineToX: 0.4777 y: 0.6871;
		lineToX: 0.4777 y: 0.3043;
		lineToX: 0.4147 y: 0.3043! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans199
	vectorGraphicsCanvas
		moveToX: 0.6818 y: 0.2833;
		lineToX: 0.6818 y: 0.3873;
		curveToX: 0.5754 y: 0.3180 control1X: 0.6486 y: 0.3564 control2X: 0.6131 y: 0.3333;
		curveToX: 0.4557 y: 0.2950 control1X: 0.5379 y: 0.3027 control2X: 0.4981 y: 0.2950;
		curveToX: 0.2643 y: 0.3717 control1X: 0.3724 y: 0.2950 control2X: 0.3086 y: 0.3206;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2201 y: 0.4225 control2X: 0.1979 y: 0.4961;
		curveToX: 0.2643 y: 0.8131 control1X: 0.1979 y: 0.6884 control2X: 0.2201 y: 0.7620;
		curveToX: 0.4557 y: 0.8893 control1X: 0.3086 y: 0.8639 control2X: 0.3724 y: 0.8893;
		curveToX: 0.5754 y: 0.8663 control1X: 0.4981 y: 0.8893 control2X: 0.5379 y: 0.8816;
		curveToX: 0.6818 y: 0.7970 control1X: 0.6131 y: 0.8510 control2X: 0.6486 y: 0.8279;
		lineToX: 0.6818 y: 0.9000;
		curveToX: 0.5719 y: 0.9528 control1X: 0.6473 y: 0.9235 control2X: 0.6107 y: 0.9410;
		curveToX: 0.4499 y: 0.9703 control1X: 0.5335 y: 0.9645 control2X: 0.4928 y: 0.9703;
		curveToX: 0.1891 y: 0.8693 control1X: 0.3395 y: 0.9703 control2X: 0.2526 y: 0.9367;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1257 y: 0.8016 control2X: 0.0939 y: 0.7093;
		curveToX: 0.1891 y: 0.3156 control1X: 0.0939 y: 0.4752 control2X: 0.1257 y: 0.3829;
		curveToX: 0.4499 y: 0.2140 control1X: 0.2526 y: 0.2478 control2X: 0.3395 y: 0.2140;
		curveToX: 0.5729 y: 0.2316 control1X: 0.4935 y: 0.2140 control2X: 0.5345 y: 0.2199;
		curveToX: 0.6818 y: 0.2833 control1X: 0.6117 y: 0.2430 control2X: 0.6480 y: 0.2602;
		moveToX: 0.4758 y: 0.9562;
		curveToX: 0.5158 y: 1.0118 control1X: 0.4937 y: 0.9764 control2X: 0.5070 y: 0.9949;
		curveToX: 0.5290 y: 1.0612 control1X: 0.5246 y: 1.0291 control2X: 0.5290 y: 1.0455;
		curveToX: 0.4997 y: 1.1266 control1X: 0.5290 y: 1.0901 control2X: 0.5192 y: 1.1119;
		curveToX: 0.4128 y: 1.1491 control1X: 0.4801 y: 1.1416 control2X: 0.4512 y: 1.1491;
		curveToX: 0.3688 y: 1.1461 control1X: 0.3978 y: 1.1491 control2X: 0.3831 y: 1.1481;
		curveToX: 0.3268 y: 1.1373 control1X: 0.3548 y: 1.1442 control2X: 0.3408 y: 1.1412;
		lineToX: 0.3268 y: 1.0734;
		curveToX: 0.3615 y: 1.0851 control1X: 0.3379 y: 1.0789 control2X: 0.3495 y: 1.0828;
		curveToX: 0.4025 y: 1.0890 control1X: 0.3735 y: 1.0877 control2X: 0.3872 y: 1.0890;
		curveToX: 0.4460 y: 1.0773 control1X: 0.4217 y: 1.0890 control2X: 0.4362 y: 1.0851;
		curveToX: 0.4606 y: 1.0431 control1X: 0.4557 y: 1.0695 control2X: 0.4606 y: 1.0581;
		curveToX: 0.4499 y: 1.0070 control1X: 0.4606 y: 1.0333 control2X: 0.4570 y: 1.0213;
		curveToX: 0.4176 y: 0.9562 control1X: 0.4430 y: 0.9930 control2X: 0.4323 y: 0.9760;
		lineToX: 0.4758 y: 0.9562! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans200
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5968 y: 0.2272;
		lineToX: 0.5968 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5817 y: 0.5260;
		lineToX: 0.5817 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.6056 y: 0.8732;
		lineToX: 0.6056 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.3371 y: 0.0289;
		lineToX: 0.4328 y: 0.1578;
		lineToX: 0.3581 y: 0.1578;
		lineToX: 0.2458 y: 0.0289;
		lineToX: 0.3371 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans201
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5968 y: 0.2272;
		lineToX: 0.5968 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5817 y: 0.5260;
		lineToX: 0.5817 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.6056 y: 0.8732;
		lineToX: 0.6056 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.3918 y: 0.0289;
		lineToX: 0.4821 y: 0.0289;
		lineToX: 0.3708 y: 0.1578;
		lineToX: 0.2961 y: 0.1578;
		lineToX: 0.3918 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans202
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5968 y: 0.2272;
		lineToX: 0.5968 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5817 y: 0.5260;
		lineToX: 0.5817 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.6056 y: 0.8732;
		lineToX: 0.6056 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.3190 y: 0.0280;
		lineToX: 0.4108 y: 0.0280;
		lineToX: 0.5138 y: 0.1578;
		lineToX: 0.4460 y: 0.1578;
		lineToX: 0.3649 y: 0.0709;
		lineToX: 0.2839 y: 0.1578;
		lineToX: 0.2160 y: 0.1578;
		lineToX: 0.3190 y: 0.0280! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans203
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5968 y: 0.2272;
		lineToX: 0.5968 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5817 y: 0.5260;
		lineToX: 0.5817 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.6056 y: 0.8732;
		lineToX: 0.6056 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.4108 y: 0.0431;
		lineToX: 0.5099 y: 0.0431;
		lineToX: 0.5099 y: 0.1422;
		lineToX: 0.4108 y: 0.1422;
		lineToX: 0.4108 y: 0.0431;
		moveToX: 0.2199 y: 0.0431;
		lineToX: 0.3190 y: 0.0431;
		lineToX: 0.3190 y: 0.1422;
		lineToX: 0.2199 y: 0.1422;
		lineToX: 0.2199 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans204
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.1579 y: 0.0289;
		lineToX: 0.2536 y: 0.1578;
		lineToX: 0.1789 y: 0.1578;
		lineToX: 0.0666 y: 0.0289;
		lineToX: 0.1579 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans205
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.2126 y: 0.0289;
		lineToX: 0.3029 y: 0.0289;
		lineToX: 0.1916 y: 0.1578;
		lineToX: 0.1169 y: 0.1578;
		lineToX: 0.2126 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans206
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.1398 y: 0.0280;
		lineToX: 0.2316 y: 0.0280;
		lineToX: 0.3346 y: 0.1578;
		lineToX: 0.2668 y: 0.1578;
		lineToX: 0.1857 y: 0.0709;
		lineToX: 0.1047 y: 0.1578;
		lineToX: 0.0368 y: 0.1578;
		lineToX: 0.1398 y: 0.0280! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans207
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.2316 y: 0.0431;
		lineToX: 0.3307 y: 0.0431;
		lineToX: 0.3307 y: 0.1422;
		lineToX: 0.2316 y: 0.1422;
		lineToX: 0.2316 y: 0.0431;
		moveToX: 0.0407 y: 0.0431;
		lineToX: 0.1398 y: 0.0431;
		lineToX: 0.1398 y: 0.1422;
		lineToX: 0.0407 y: 0.1422;
		lineToX: 0.0407 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans208
	vectorGraphicsCanvas
		moveToX: 0.1408 y: 0.2272;
		lineToX: 0.3439 y: 0.2272;
		curveToX: 0.6545 y: 0.3156 control1X: 0.4849 y: 0.2272 control2X: 0.5884 y: 0.2566;
		curveToX: 0.7536 y: 0.5909 control1X: 0.7205 y: 0.3742 control2X: 0.7536 y: 0.4659;
		curveToX: 0.6540 y: 0.8678 control1X: 0.7536 y: 0.7166 control2X: 0.7204 y: 0.8089;
		curveToX: 0.3439 y: 0.9562 control1X: 0.5879 y: 0.9267 control2X: 0.4845 y: 0.9562;
		lineToX: 0.1408 y: 0.9562;
		lineToX: 0.1408 y: 0.6144;
		lineToX: 0.0426 y: 0.6144;
		lineToX: 0.0426 y: 0.5441;
		lineToX: 0.1408 y: 0.5441;
		lineToX: 0.1408 y: 0.2272;
		moveToX: 0.2399 y: 0.3082;
		lineToX: 0.2399 y: 0.5441;
		lineToX: 0.4040 y: 0.5441;
		lineToX: 0.4040 y: 0.6144;
		lineToX: 0.2399 y: 0.6144;
		lineToX: 0.2399 y: 0.8751;
		lineToX: 0.3586 y: 0.8751;
		curveToX: 0.5793 y: 0.8068 control1X: 0.4592 y: 0.8751 control2X: 0.5327 y: 0.8523;
		curveToX: 0.6496 y: 0.5909 control1X: 0.6261 y: 0.7612 control2X: 0.6496 y: 0.6893;
		curveToX: 0.5793 y: 0.3766 control1X: 0.6496 y: 0.4933 control2X: 0.6261 y: 0.4218;
		curveToX: 0.3586 y: 0.3082 control1X: 0.5327 y: 0.3310 control2X: 0.4592 y: 0.3082;
		lineToX: 0.2399 y: 0.3082! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans209
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2687 y: 0.2272;
		lineToX: 0.5920 y: 0.8370;
		lineToX: 0.5920 y: 0.2272;
		lineToX: 0.6877 y: 0.2272;
		lineToX: 0.6877 y: 0.9562;
		lineToX: 0.5549 y: 0.9562;
		lineToX: 0.2316 y: 0.3463;
		lineToX: 0.2316 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.4098 y: 0.1212;
		lineToX: 0.3820 y: 0.1051;
		curveToX: 0.3620 y: 0.0958 control1X: 0.3739 y: 0.1006 control2X: 0.3672 y: 0.0975;
		curveToX: 0.3488 y: 0.0929 control1X: 0.3571 y: 0.0939 control2X: 0.3527 y: 0.0929;
		curveToX: 0.3215 y: 0.1051 control1X: 0.3371 y: 0.0929 control2X: 0.3280 y: 0.0970;
		curveToX: 0.3117 y: 0.1393 control1X: 0.3149 y: 0.1132 control2X: 0.3117 y: 0.1246;
		lineToX: 0.3117 y: 0.1422;
		lineToX: 0.2507 y: 0.1422;
		curveToX: 0.2756 y: 0.0641 control1X: 0.2507 y: 0.1093 control2X: 0.2590 y: 0.0833;
		curveToX: 0.3429 y: 0.0353 control1X: 0.2925 y: 0.0449 control2X: 0.3149 y: 0.0353;
		curveToX: 0.3752 y: 0.0392 control1X: 0.3547 y: 0.0353 control2X: 0.3654 y: 0.0366;
		curveToX: 0.4137 y: 0.0563 control1X: 0.3853 y: 0.0418 control2X: 0.3981 y: 0.0475;
		lineToX: 0.4416 y: 0.0709;
		curveToX: 0.4611 y: 0.0802 control1X: 0.4491 y: 0.0752 control2X: 0.4556 y: 0.0783;
		curveToX: 0.4767 y: 0.0831 control1X: 0.4666 y: 0.0822 control2X: 0.4718 y: 0.0831;
		curveToX: 0.5021 y: 0.0709 control1X: 0.4871 y: 0.0831 control2X: 0.4956 y: 0.0791;
		curveToX: 0.5119 y: 0.0382 control1X: 0.5086 y: 0.0625 control2X: 0.5119 y: 0.0516;
		lineToX: 0.5119 y: 0.0353;
		lineToX: 0.5729 y: 0.0353;
		curveToX: 0.5466 y: 0.1134 control1X: 0.5723 y: 0.0678 control2X: 0.5635 y: 0.0939;
		curveToX: 0.4806 y: 0.1422 control1X: 0.5300 y: 0.1326 control2X: 0.5080 y: 0.1422;
		curveToX: 0.4494 y: 0.1383 control1X: 0.4696 y: 0.1422 control2X: 0.4592 y: 0.1409;
		curveToX: 0.4098 y: 0.1212 control1X: 0.4399 y: 0.1357 control2X: 0.4268 y: 0.1300! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans210
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		moveToX: 0.4040 y: 0.0289;
		lineToX: 0.4997 y: 0.1578;
		lineToX: 0.4250 y: 0.1578;
		lineToX: 0.3127 y: 0.0289;
		lineToX: 0.4040 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans211
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		moveToX: 0.4587 y: 0.0289;
		lineToX: 0.5490 y: 0.0289;
		lineToX: 0.4377 y: 0.1578;
		lineToX: 0.3630 y: 0.1578;
		lineToX: 0.4587 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans212
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		moveToX: 0.3859 y: 0.0280;
		lineToX: 0.4777 y: 0.0280;
		lineToX: 0.5807 y: 0.1578;
		lineToX: 0.5129 y: 0.1578;
		lineToX: 0.4318 y: 0.0709;
		lineToX: 0.3508 y: 0.1578;
		lineToX: 0.2829 y: 0.1578;
		lineToX: 0.3859 y: 0.0280! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans213
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		moveToX: 0.4299 y: 0.1212;
		lineToX: 0.4020 y: 0.1051;
		curveToX: 0.3820 y: 0.0958 control1X: 0.3939 y: 0.1006 control2X: 0.3872 y: 0.0975;
		curveToX: 0.3688 y: 0.0929 control1X: 0.3771 y: 0.0939 control2X: 0.3727 y: 0.0929;
		curveToX: 0.3415 y: 0.1051 control1X: 0.3571 y: 0.0929 control2X: 0.3480 y: 0.0970;
		curveToX: 0.3317 y: 0.1393 control1X: 0.3350 y: 0.1132 control2X: 0.3317 y: 0.1246;
		lineToX: 0.3317 y: 0.1422;
		lineToX: 0.2707 y: 0.1422;
		curveToX: 0.2956 y: 0.0641 control1X: 0.2707 y: 0.1093 control2X: 0.2790 y: 0.0833;
		curveToX: 0.3630 y: 0.0353 control1X: 0.3125 y: 0.0449 control2X: 0.3350 y: 0.0353;
		curveToX: 0.3952 y: 0.0392 control1X: 0.3747 y: 0.0353 control2X: 0.3854 y: 0.0366;
		curveToX: 0.4338 y: 0.0563 control1X: 0.4053 y: 0.0418 control2X: 0.4181 y: 0.0475;
		lineToX: 0.4616 y: 0.0709;
		curveToX: 0.4811 y: 0.0802 control1X: 0.4691 y: 0.0752 control2X: 0.4756 y: 0.0783;
		curveToX: 0.4968 y: 0.0831 control1X: 0.4867 y: 0.0822 control2X: 0.4919 y: 0.0831;
		curveToX: 0.5221 y: 0.0709 control1X: 0.5072 y: 0.0831 control2X: 0.5156 y: 0.0791;
		curveToX: 0.5319 y: 0.0382 control1X: 0.5287 y: 0.0625 control2X: 0.5319 y: 0.0516;
		lineToX: 0.5319 y: 0.0353;
		lineToX: 0.5929 y: 0.0353;
		curveToX: 0.5666 y: 0.1134 control1X: 0.5923 y: 0.0678 control2X: 0.5835 y: 0.0939;
		curveToX: 0.5007 y: 0.1422 control1X: 0.5500 y: 0.1326 control2X: 0.5280 y: 0.1422;
		curveToX: 0.4694 y: 0.1383 control1X: 0.4896 y: 0.1422 control2X: 0.4792 y: 0.1409;
		curveToX: 0.4299 y: 0.1212 control1X: 0.4600 y: 0.1357 control2X: 0.4468 y: 0.1300! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans214
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		moveToX: 0.4777 y: 0.0431;
		lineToX: 0.5768 y: 0.0431;
		lineToX: 0.5768 y: 0.1422;
		lineToX: 0.4777 y: 0.1422;
		lineToX: 0.4777 y: 0.0431;
		moveToX: 0.2868 y: 0.0431;
		lineToX: 0.3859 y: 0.0431;
		lineToX: 0.3859 y: 0.1422;
		lineToX: 0.2868 y: 0.1422;
		lineToX: 0.2868 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans215
	vectorGraphicsCanvas
		moveToX: 0.7389 y: 0.4191;
		lineToX: 0.5158 y: 0.6432;
		lineToX: 0.7389 y: 0.8663;
		lineToX: 0.6808 y: 0.9254;
		lineToX: 0.4567 y: 0.7013;
		lineToX: 0.2326 y: 0.9254;
		lineToX: 0.1750 y: 0.8663;
		lineToX: 0.3976 y: 0.6432;
		lineToX: 0.1750 y: 0.4191;
		lineToX: 0.2326 y: 0.3600;
		lineToX: 0.4567 y: 0.5841;
		lineToX: 0.6808 y: 0.3600;
		lineToX: 0.7389 y: 0.4191! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans216
	vectorGraphicsCanvas
		moveToX: 0.6266 y: 0.4132;
		lineToX: 0.2765 y: 0.8263;
		curveToX: 0.3459 y: 0.8742 control1X: 0.2967 y: 0.8471 control2X: 0.3198 y: 0.8631;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3722 y: 0.8849 control2X: 0.4009 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6550 y: 0.4928 control1X: 0.6647 y: 0.5560 control2X: 0.6615 y: 0.5228;
		curveToX: 0.6266 y: 0.4132 control1X: 0.6488 y: 0.4629 control2X: 0.6393 y: 0.4363;
		moveToX: 0.5866 y: 0.3580;
		curveToX: 0.5173 y: 0.3107 control1X: 0.5667 y: 0.3372 control2X: 0.5436 y: 0.3214;
		curveToX: 0.4318 y: 0.2941 control1X: 0.4912 y: 0.2996 control2X: 0.4627 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2072 y: 0.6930 control1X: 0.1979 y: 0.6289 control2X: 0.2010 y: 0.6624;
		curveToX: 0.2360 y: 0.7721 control1X: 0.2137 y: 0.7236 control2X: 0.2233 y: 0.7500;
		lineToX: 0.5866 y: 0.3580;
		moveToX: 0.1706 y: 0.8492;
		curveToX: 0.1130 y: 0.7350 control1X: 0.1452 y: 0.8160 control2X: 0.1260 y: 0.7780;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1003 y: 0.6920 control2X: 0.0939 y: 0.6445;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		curveToX: 0.5490 y: 0.2330 control1X: 0.4741 y: 0.2140 control2X: 0.5132 y: 0.2203;
		curveToX: 0.6457 y: 0.2882 control1X: 0.5851 y: 0.2454 control2X: 0.6174 y: 0.2638;
		lineToX: 0.7248 y: 0.1950;
		lineToX: 0.7746 y: 0.2384;
		lineToX: 0.6916 y: 0.3351;
		curveToX: 0.7492 y: 0.4503 control1X: 0.7170 y: 0.3686 control2X: 0.7362 y: 0.4070;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7622 y: 0.4936 control2X: 0.7687 y: 0.5410;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.3151 y: 0.9523 control1X: 0.3901 y: 0.9703 control2X: 0.3512 y: 0.9643;
		curveToX: 0.2170 y: 0.8971 control1X: 0.2793 y: 0.9399 control2X: 0.2466 y: 0.9215;
		lineToX: 0.1379 y: 0.9904;
		lineToX: 0.0876 y: 0.9474;
		lineToX: 0.1706 y: 0.8492! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans217
	vectorGraphicsCanvas
		moveToX: 0.1247 y: 0.2272;
		lineToX: 0.2238 y: 0.2272;
		lineToX: 0.2238 y: 0.6700;
		curveToX: 0.2663 y: 0.8390 control1X: 0.2238 y: 0.7482 control2X: 0.2380 y: 0.8045;
		curveToX: 0.4040 y: 0.8903 control1X: 0.2946 y: 0.8732 control2X: 0.3405 y: 0.8903;
		curveToX: 0.5412 y: 0.8390 control1X: 0.4671 y: 0.8903 control2X: 0.5129 y: 0.8732;
		curveToX: 0.5837 y: 0.6700 control1X: 0.5695 y: 0.8045 control2X: 0.5837 y: 0.7482;
		lineToX: 0.5837 y: 0.2272;
		lineToX: 0.6828 y: 0.2272;
		lineToX: 0.6828 y: 0.6823;
		curveToX: 0.6120 y: 0.8976 control1X: 0.6828 y: 0.7773 control2X: 0.6592 y: 0.8491;
		curveToX: 0.4040 y: 0.9703 control1X: 0.5651 y: 0.9461 control2X: 0.4958 y: 0.9703;
		curveToX: 0.1950 y: 0.8976 control1X: 0.3119 y: 0.9703 control2X: 0.2422 y: 0.9461;
		curveToX: 0.1247 y: 0.6823 control1X: 0.1481 y: 0.8491 control2X: 0.1247 y: 0.7773;
		lineToX: 0.1247 y: 0.2272;
		moveToX: 0.3761 y: 0.0289;
		lineToX: 0.4718 y: 0.1578;
		lineToX: 0.3971 y: 0.1578;
		lineToX: 0.2848 y: 0.0289;
		lineToX: 0.3761 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans218
	vectorGraphicsCanvas
		moveToX: 0.1247 y: 0.2272;
		lineToX: 0.2238 y: 0.2272;
		lineToX: 0.2238 y: 0.6700;
		curveToX: 0.2663 y: 0.8390 control1X: 0.2238 y: 0.7482 control2X: 0.2380 y: 0.8045;
		curveToX: 0.4040 y: 0.8903 control1X: 0.2946 y: 0.8732 control2X: 0.3405 y: 0.8903;
		curveToX: 0.5412 y: 0.8390 control1X: 0.4671 y: 0.8903 control2X: 0.5129 y: 0.8732;
		curveToX: 0.5837 y: 0.6700 control1X: 0.5695 y: 0.8045 control2X: 0.5837 y: 0.7482;
		lineToX: 0.5837 y: 0.2272;
		lineToX: 0.6828 y: 0.2272;
		lineToX: 0.6828 y: 0.6823;
		curveToX: 0.6120 y: 0.8976 control1X: 0.6828 y: 0.7773 control2X: 0.6592 y: 0.8491;
		curveToX: 0.4040 y: 0.9703 control1X: 0.5651 y: 0.9461 control2X: 0.4958 y: 0.9703;
		curveToX: 0.1950 y: 0.8976 control1X: 0.3119 y: 0.9703 control2X: 0.2422 y: 0.9461;
		curveToX: 0.1247 y: 0.6823 control1X: 0.1481 y: 0.8491 control2X: 0.1247 y: 0.7773;
		lineToX: 0.1247 y: 0.2272;
		moveToX: 0.4308 y: 0.0289;
		lineToX: 0.5212 y: 0.0289;
		lineToX: 0.4098 y: 0.1578;
		lineToX: 0.3351 y: 0.1578;
		lineToX: 0.4308 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans219
	vectorGraphicsCanvas
		moveToX: 0.1247 y: 0.2272;
		lineToX: 0.2238 y: 0.2272;
		lineToX: 0.2238 y: 0.6700;
		curveToX: 0.2663 y: 0.8390 control1X: 0.2238 y: 0.7482 control2X: 0.2380 y: 0.8045;
		curveToX: 0.4040 y: 0.8903 control1X: 0.2946 y: 0.8732 control2X: 0.3405 y: 0.8903;
		curveToX: 0.5412 y: 0.8390 control1X: 0.4671 y: 0.8903 control2X: 0.5129 y: 0.8732;
		curveToX: 0.5837 y: 0.6700 control1X: 0.5695 y: 0.8045 control2X: 0.5837 y: 0.7482;
		lineToX: 0.5837 y: 0.2272;
		lineToX: 0.6828 y: 0.2272;
		lineToX: 0.6828 y: 0.6823;
		curveToX: 0.6120 y: 0.8976 control1X: 0.6828 y: 0.7773 control2X: 0.6592 y: 0.8491;
		curveToX: 0.4040 y: 0.9703 control1X: 0.5651 y: 0.9461 control2X: 0.4958 y: 0.9703;
		curveToX: 0.1950 y: 0.8976 control1X: 0.3119 y: 0.9703 control2X: 0.2422 y: 0.9461;
		curveToX: 0.1247 y: 0.6823 control1X: 0.1481 y: 0.8491 control2X: 0.1247 y: 0.7773;
		lineToX: 0.1247 y: 0.2272;
		moveToX: 0.3581 y: 0.0280;
		lineToX: 0.4499 y: 0.0280;
		lineToX: 0.5529 y: 0.1578;
		lineToX: 0.4850 y: 0.1578;
		lineToX: 0.4040 y: 0.0709;
		lineToX: 0.3229 y: 0.1578;
		lineToX: 0.2551 y: 0.1578;
		lineToX: 0.3581 y: 0.0280! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans220
	vectorGraphicsCanvas
		moveToX: 0.1247 y: 0.2272;
		lineToX: 0.2238 y: 0.2272;
		lineToX: 0.2238 y: 0.6700;
		curveToX: 0.2663 y: 0.8390 control1X: 0.2238 y: 0.7482 control2X: 0.2380 y: 0.8045;
		curveToX: 0.4040 y: 0.8903 control1X: 0.2946 y: 0.8732 control2X: 0.3405 y: 0.8903;
		curveToX: 0.5412 y: 0.8390 control1X: 0.4671 y: 0.8903 control2X: 0.5129 y: 0.8732;
		curveToX: 0.5837 y: 0.6700 control1X: 0.5695 y: 0.8045 control2X: 0.5837 y: 0.7482;
		lineToX: 0.5837 y: 0.2272;
		lineToX: 0.6828 y: 0.2272;
		lineToX: 0.6828 y: 0.6823;
		curveToX: 0.6120 y: 0.8976 control1X: 0.6828 y: 0.7773 control2X: 0.6592 y: 0.8491;
		curveToX: 0.4040 y: 0.9703 control1X: 0.5651 y: 0.9461 control2X: 0.4958 y: 0.9703;
		curveToX: 0.1950 y: 0.8976 control1X: 0.3119 y: 0.9703 control2X: 0.2422 y: 0.9461;
		curveToX: 0.1247 y: 0.6823 control1X: 0.1481 y: 0.8491 control2X: 0.1247 y: 0.7773;
		lineToX: 0.1247 y: 0.2272;
		moveToX: 0.4499 y: 0.0431;
		lineToX: 0.5490 y: 0.0431;
		lineToX: 0.5490 y: 0.1422;
		lineToX: 0.4499 y: 0.1422;
		lineToX: 0.4499 y: 0.0431;
		moveToX: 0.2590 y: 0.0431;
		lineToX: 0.3581 y: 0.0431;
		lineToX: 0.3581 y: 0.1422;
		lineToX: 0.2590 y: 0.1422;
		lineToX: 0.2590 y: 0.0431! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans221
	vectorGraphicsCanvas
		moveToX: 0.0358 y: 0.2272;
		lineToX: 0.1418 y: 0.2272;
		lineToX: 0.3439 y: 0.5270;
		lineToX: 0.5446 y: 0.2272;
		lineToX: 0.6506 y: 0.2272;
		lineToX: 0.3927 y: 0.6090;
		lineToX: 0.3927 y: 0.9562;
		lineToX: 0.2936 y: 0.9562;
		lineToX: 0.2936 y: 0.6090;
		lineToX: 0.0358 y: 0.2272;
		moveToX: 0.3708 y: 0.0289;
		lineToX: 0.4611 y: 0.0289;
		lineToX: 0.3498 y: 0.1578;
		lineToX: 0.2751 y: 0.1578;
		lineToX: 0.3708 y: 0.0289! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans222
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.3561;
		lineToX: 0.3586 y: 0.3561;
		curveToX: 0.5436 y: 0.4113 control1X: 0.4403 y: 0.3561 control2X: 0.5020 y: 0.3745;
		curveToX: 0.6066 y: 0.5734 control1X: 0.5856 y: 0.4477 control2X: 0.6066 y: 0.5018;
		curveToX: 0.5436 y: 0.7360 control1X: 0.6066 y: 0.6450 control2X: 0.5856 y: 0.6992;
		curveToX: 0.3586 y: 0.7911 control1X: 0.5020 y: 0.7728 control2X: 0.4403 y: 0.7911;
		lineToX: 0.2345 y: 0.7911;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		moveToX: 0.2345 y: 0.4371;
		lineToX: 0.2345 y: 0.7101;
		lineToX: 0.3586 y: 0.7101;
		curveToX: 0.4650 y: 0.6744 control1X: 0.4045 y: 0.7101 control2X: 0.4399 y: 0.6982;
		curveToX: 0.5026 y: 0.5734 control1X: 0.4901 y: 0.6507 control2X: 0.5026 y: 0.6170;
		curveToX: 0.4650 y: 0.4728 control1X: 0.5026 y: 0.5297 control2X: 0.4901 y: 0.4962;
		curveToX: 0.3586 y: 0.4371 control1X: 0.4403 y: 0.4490 control2X: 0.4048 y: 0.4371;
		lineToX: 0.2345 y: 0.4371! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans223
	vectorGraphicsCanvas
		moveToX: 0.1286 y: 0.4010;
		curveToX: 0.1867 y: 0.2501 control1X: 0.1286 y: 0.3359 control2X: 0.1480 y: 0.2856;
		curveToX: 0.3517 y: 0.1964 control1X: 0.2258 y: 0.2143 control2X: 0.2808 y: 0.1964;
		curveToX: 0.5065 y: 0.2531 control1X: 0.4194 y: 0.1964 control2X: 0.4710 y: 0.2153;
		curveToX: 0.5617 y: 0.4191 control1X: 0.5423 y: 0.2908 control2X: 0.5607 y: 0.3462;
		curveToX: 0.4469 y: 0.4513 control1X: 0.5125 y: 0.4217 control2X: 0.4743 y: 0.4324;
		curveToX: 0.4059 y: 0.5260 control1X: 0.4196 y: 0.4699 control2X: 0.4059 y: 0.4948;
		curveToX: 0.4201 y: 0.5690 control1X: 0.4059 y: 0.5413 control2X: 0.4107 y: 0.5556;
		curveToX: 0.4660 y: 0.6090 control1X: 0.4295 y: 0.5820 control2X: 0.4448 y: 0.5953;
		lineToX: 0.4938 y: 0.6271;
		curveToX: 0.5983 y: 0.7135 control1X: 0.5479 y: 0.6616 control2X: 0.5827 y: 0.6904;
		curveToX: 0.6218 y: 0.7970 control1X: 0.6139 y: 0.7366 control2X: 0.6218 y: 0.7644;
		curveToX: 0.5666 y: 0.9259 control1X: 0.6218 y: 0.8530 control2X: 0.6034 y: 0.8960;
		curveToX: 0.4089 y: 0.9703 control1X: 0.5301 y: 0.9555 control2X: 0.4775 y: 0.9703;
		curveToX: 0.3444 y: 0.9640 control1X: 0.3880 y: 0.9703 control2X: 0.3665 y: 0.9682;
		curveToX: 0.2756 y: 0.9464 control1X: 0.3223 y: 0.9601 control2X: 0.2993 y: 0.9542;
		lineToX: 0.2756 y: 0.8663;
		curveToX: 0.3488 y: 0.8883 control1X: 0.3016 y: 0.8761 control2X: 0.3260 y: 0.8834;
		curveToX: 0.4147 y: 0.8951 control1X: 0.3716 y: 0.8929 control2X: 0.3936 y: 0.8951;
		curveToX: 0.5007 y: 0.8722 control1X: 0.4525 y: 0.8951 control2X: 0.4811 y: 0.8875;
		curveToX: 0.5300 y: 0.8043 control1X: 0.5202 y: 0.8566 control2X: 0.5300 y: 0.8339;
		curveToX: 0.5153 y: 0.7531 control1X: 0.5300 y: 0.7838 control2X: 0.5251 y: 0.7667;
		curveToX: 0.4518 y: 0.7023 control1X: 0.5059 y: 0.7394 control2X: 0.4847 y: 0.7225;
		lineToX: 0.4069 y: 0.6754;
		curveToX: 0.3385 y: 0.6139 control1X: 0.3757 y: 0.6562 control2X: 0.3529 y: 0.6357;
		curveToX: 0.3176 y: 0.5363 control1X: 0.3246 y: 0.5918 control2X: 0.3176 y: 0.5659;
		curveToX: 0.3581 y: 0.4323 control1X: 0.3176 y: 0.4949 control2X: 0.3311 y: 0.4603;
		curveToX: 0.4728 y: 0.3761 control1X: 0.3854 y: 0.4043 control2X: 0.4237 y: 0.3855;
		curveToX: 0.4367 y: 0.2985 control1X: 0.4702 y: 0.3426 control2X: 0.4582 y: 0.3167;
		curveToX: 0.3498 y: 0.2711 control1X: 0.4155 y: 0.2802 control2X: 0.3866 y: 0.2711;
		curveToX: 0.2531 y: 0.3053 control1X: 0.3075 y: 0.2711 control2X: 0.2752 y: 0.2825;
		curveToX: 0.2199 y: 0.4030 control1X: 0.2310 y: 0.3278 control2X: 0.2199 y: 0.3603;
		lineToX: 0.2199 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.4010! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans224
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.2570 y: 0.1564;
		lineToX: 0.3947 y: 0.3390;
		lineToX: 0.3200 y: 0.3390;
		lineToX: 0.1608 y: 0.1564;
		lineToX: 0.2570 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans225
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.3957 y: 0.1564;
		lineToX: 0.4928 y: 0.1564;
		lineToX: 0.3337 y: 0.3400;
		lineToX: 0.2590 y: 0.3400;
		lineToX: 0.3957 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans226
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.2917 y: 0.1564;
		lineToX: 0.3639 y: 0.1564;
		lineToX: 0.4836 y: 0.3400;
		lineToX: 0.4157 y: 0.3400;
		lineToX: 0.3278 y: 0.2203;
		lineToX: 0.2399 y: 0.3400;
		lineToX: 0.1720 y: 0.3400;
		lineToX: 0.2917 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans227
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.3259 y: 0.2872;
		lineToX: 0.2980 y: 0.2604;
		curveToX: 0.2790 y: 0.2462 control1X: 0.2909 y: 0.2539 control2X: 0.2845 y: 0.2492;
		curveToX: 0.2648 y: 0.2413 control1X: 0.2738 y: 0.2430 control2X: 0.2690 y: 0.2413;
		curveToX: 0.2375 y: 0.2594 control1X: 0.2524 y: 0.2413 control2X: 0.2433 y: 0.2474;
		curveToX: 0.2277 y: 0.3170 control1X: 0.2316 y: 0.2711 control2X: 0.2284 y: 0.2903;
		lineToX: 0.1667 y: 0.3170;
		curveToX: 0.1926 y: 0.2155 control1X: 0.1673 y: 0.2731 control2X: 0.1760 y: 0.2392;
		curveToX: 0.2619 y: 0.1793 control1X: 0.2092 y: 0.1914 control2X: 0.2323 y: 0.1793;
		curveToX: 0.2961 y: 0.1862 control1X: 0.2743 y: 0.1793 control2X: 0.2857 y: 0.1816;
		curveToX: 0.3298 y: 0.2091 control1X: 0.3065 y: 0.1907 control2X: 0.3177 y: 0.1984;
		lineToX: 0.3576 y: 0.2360;
		curveToX: 0.3761 y: 0.2506 control1X: 0.3648 y: 0.2425 control2X: 0.3709 y: 0.2474;
		curveToX: 0.3908 y: 0.2550 control1X: 0.3817 y: 0.2535 control2X: 0.3866 y: 0.2550;
		curveToX: 0.4181 y: 0.2374 control1X: 0.4032 y: 0.2550 control2X: 0.4123 y: 0.2492;
		curveToX: 0.4279 y: 0.1793 control1X: 0.4240 y: 0.2254 control2X: 0.4273 y: 0.2060;
		lineToX: 0.4889 y: 0.1793;
		curveToX: 0.4631 y: 0.2814 control1X: 0.4883 y: 0.2233 control2X: 0.4797 y: 0.2573;
		curveToX: 0.3937 y: 0.3170 control1X: 0.4465 y: 0.3051 control2X: 0.4233 y: 0.3170;
		curveToX: 0.3595 y: 0.3102 control1X: 0.3814 y: 0.3170 control2X: 0.3700 y: 0.3147;
		curveToX: 0.3259 y: 0.2872 control1X: 0.3491 y: 0.3056 control2X: 0.3379 y: 0.2980! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans228
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.3737 y: 0.1984;
		lineToX: 0.4728 y: 0.1984;
		lineToX: 0.4728 y: 0.2970;
		lineToX: 0.3737 y: 0.2970;
		lineToX: 0.3737 y: 0.1984;
		moveToX: 0.1828 y: 0.1984;
		lineToX: 0.2819 y: 0.1984;
		lineToX: 0.2819 y: 0.2970;
		lineToX: 0.1828 y: 0.2970;
		lineToX: 0.1828 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans229
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608;
		moveToX: 0.4020 y: 0.2120;
		curveToX: 0.3805 y: 0.1598 control1X: 0.4020 y: 0.1915 control2X: 0.3949 y: 0.1741;
		curveToX: 0.3278 y: 0.1383 control1X: 0.3662 y: 0.1455 control2X: 0.3486 y: 0.1383;
		curveToX: 0.2746 y: 0.1598 control1X: 0.3066 y: 0.1383 control2X: 0.2889 y: 0.1455;
		curveToX: 0.2536 y: 0.2120 control1X: 0.2606 y: 0.1738 control2X: 0.2536 y: 0.1912;
		curveToX: 0.2746 y: 0.2653 control1X: 0.2536 y: 0.2332 control2X: 0.2606 y: 0.2509;
		curveToX: 0.3278 y: 0.2863 control1X: 0.2889 y: 0.2793 control2X: 0.3066 y: 0.2863;
		curveToX: 0.3805 y: 0.2648 control1X: 0.3486 y: 0.2863 control2X: 0.3662 y: 0.2791;
		curveToX: 0.4020 y: 0.2120 control1X: 0.3949 y: 0.2505 control2X: 0.4020 y: 0.2329;
		moveToX: 0.4616 y: 0.2120;
		curveToX: 0.4225 y: 0.3073 control1X: 0.4616 y: 0.2495 control2X: 0.4486 y: 0.2812;
		curveToX: 0.3278 y: 0.3463 control1X: 0.3968 y: 0.3333 control2X: 0.3652 y: 0.3463;
		curveToX: 0.2326 y: 0.3073 control1X: 0.2904 y: 0.3463 control2X: 0.2586 y: 0.3333;
		curveToX: 0.1940 y: 0.2120 control1X: 0.2069 y: 0.2812 control2X: 0.1940 y: 0.2495;
		curveToX: 0.2326 y: 0.1173 control1X: 0.1940 y: 0.1746 control2X: 0.2069 y: 0.1430;
		curveToX: 0.3278 y: 0.0783 control1X: 0.2586 y: 0.0913 control2X: 0.2904 y: 0.0783;
		curveToX: 0.4225 y: 0.1173 control1X: 0.3652 y: 0.0783 control2X: 0.3968 y: 0.0913;
		curveToX: 0.4616 y: 0.2120 control1X: 0.4486 y: 0.1430 control2X: 0.4616 y: 0.1746! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans230
	vectorGraphicsCanvas
		moveToX: 0.8766 y: 0.6339;
		curveToX: 0.8356 y: 0.5162 control1X: 0.8763 y: 0.5848 control2X: 0.8626 y: 0.5455;
		curveToX: 0.7287 y: 0.4723 control1X: 0.8089 y: 0.4869 control2X: 0.7733 y: 0.4723;
		curveToX: 0.6086 y: 0.5148 control1X: 0.6789 y: 0.4723 control2X: 0.6388 y: 0.4865;
		curveToX: 0.5568 y: 0.6344 control1X: 0.5786 y: 0.5431 control2X: 0.5614 y: 0.5830;
		lineToX: 0.8766 y: 0.6339;
		moveToX: 0.5236 y: 0.4860;
		curveToX: 0.6115 y: 0.4191 control1X: 0.5477 y: 0.4567 control2X: 0.5770 y: 0.4344;
		curveToX: 0.7277 y: 0.3961 control1X: 0.6460 y: 0.4038 control2X: 0.6847 y: 0.3961;
		curveToX: 0.9025 y: 0.4674 control1X: 0.8013 y: 0.3961 control2X: 0.8595 y: 0.4199;
		curveToX: 0.9670 y: 0.6603 control1X: 0.9455 y: 0.5146 control2X: 0.9670 y: 0.5789;
		lineToX: 0.9670 y: 0.7042;
		lineToX: 0.5539 y: 0.7042;
		curveToX: 0.6095 y: 0.8463 control1X: 0.5578 y: 0.7664 control2X: 0.5763 y: 0.8138;
		curveToX: 0.7487 y: 0.8951 control1X: 0.6427 y: 0.8789 control2X: 0.6891 y: 0.8951;
		curveToX: 0.8483 y: 0.8825 control1X: 0.7826 y: 0.8951 control2X: 0.8158 y: 0.8909;
		curveToX: 0.9460 y: 0.8434 control1X: 0.8809 y: 0.8737 control2X: 0.9134 y: 0.8606;
		lineToX: 0.9460 y: 0.9283;
		curveToX: 0.8464 y: 0.9596 control1X: 0.9134 y: 0.9420 control2X: 0.8802 y: 0.9524;
		curveToX: 0.7438 y: 0.9703 control1X: 0.8125 y: 0.9668 control2X: 0.7783 y: 0.9703;
		curveToX: 0.6017 y: 0.9410 control1X: 0.6895 y: 0.9703 control2X: 0.6421 y: 0.9606;
		curveToX: 0.5036 y: 0.8532 control1X: 0.5614 y: 0.9212 control2X: 0.5287 y: 0.8919;
		curveToX: 0.4152 y: 0.9410 control1X: 0.4798 y: 0.8922 control2X: 0.4504 y: 0.9215;
		curveToX: 0.2926 y: 0.9703 control1X: 0.3801 y: 0.9606 control2X: 0.3392 y: 0.9703;
		curveToX: 0.1491 y: 0.9249 control1X: 0.2311 y: 0.9703 control2X: 0.1833 y: 0.9552;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1149 y: 0.8943 control2X: 0.0978 y: 0.8517;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.4450 y: 0.4196 control1X: 0.3701 y: 0.3961 control2X: 0.4105 y: 0.4039;
		curveToX: 0.5236 y: 0.4860 control1X: 0.4798 y: 0.4352 control2X: 0.5060 y: 0.4573;
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans231
	vectorGraphicsCanvas
		moveToX: 0.5256 y: 0.4303;
		lineToX: 0.5256 y: 0.5143;
		curveToX: 0.4489 y: 0.4830 control1X: 0.5002 y: 0.5003 control2X: 0.4746 y: 0.4899;
		curveToX: 0.3718 y: 0.4723 control1X: 0.4235 y: 0.4759 control2X: 0.3978 y: 0.4723;
		curveToX: 0.2360 y: 0.5280 control1X: 0.3135 y: 0.4723 control2X: 0.2682 y: 0.4908;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2038 y: 0.5647 control2X: 0.1877 y: 0.6165;
		curveToX: 0.2360 y: 0.8390 control1X: 0.1877 y: 0.7500 control2X: 0.2038 y: 0.8019;
		curveToX: 0.3718 y: 0.8942 control1X: 0.2682 y: 0.8758 control2X: 0.3135 y: 0.8942;
		curveToX: 0.4489 y: 0.8839 control1X: 0.3978 y: 0.8942 control2X: 0.4235 y: 0.8908;
		curveToX: 0.5256 y: 0.8522 control1X: 0.4746 y: 0.8768 control2X: 0.5002 y: 0.8662;
		lineToX: 0.5256 y: 0.9352;
		curveToX: 0.4474 y: 0.9616 control1X: 0.5005 y: 0.9469 control2X: 0.4745 y: 0.9557;
		curveToX: 0.3620 y: 0.9703 control1X: 0.4207 y: 0.9674 control2X: 0.3923 y: 0.9703;
		curveToX: 0.1657 y: 0.8927 control1X: 0.2796 y: 0.9703 control2X: 0.2142 y: 0.9445;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1172 y: 0.8409 control2X: 0.0929 y: 0.7711;
		curveToX: 0.1662 y: 0.4728 control1X: 0.0929 y: 0.5940 control2X: 0.1174 y: 0.5239;
		curveToX: 0.3678 y: 0.3961 control1X: 0.2153 y: 0.4217 control2X: 0.2826 y: 0.3961;
		curveToX: 0.4489 y: 0.4049 control1X: 0.3955 y: 0.3961 control2X: 0.4225 y: 0.3991;
		curveToX: 0.5256 y: 0.4303 control1X: 0.4753 y: 0.4104 control2X: 0.5008 y: 0.4189;
		moveToX: 0.3986 y: 0.9562;
		curveToX: 0.4386 y: 1.0118 control1X: 0.4165 y: 0.9764 control2X: 0.4299 y: 0.9949;
		curveToX: 0.4518 y: 1.0612 control1X: 0.4474 y: 1.0291 control2X: 0.4518 y: 1.0455;
		curveToX: 0.4225 y: 1.1266 control1X: 0.4518 y: 1.0901 control2X: 0.4421 y: 1.1119;
		curveToX: 0.3356 y: 1.1491 control1X: 0.4030 y: 1.1416 control2X: 0.3740 y: 1.1491;
		curveToX: 0.2917 y: 1.1461 control1X: 0.3206 y: 1.1491 control2X: 0.3060 y: 1.1481;
		curveToX: 0.2497 y: 1.1373 control1X: 0.2777 y: 1.1442 control2X: 0.2637 y: 1.1412;
		lineToX: 0.2497 y: 1.0734;
		curveToX: 0.2843 y: 1.0851 control1X: 0.2607 y: 1.0789 control2X: 0.2723 y: 1.0828;
		curveToX: 0.3254 y: 1.0890 control1X: 0.2964 y: 1.0877 control2X: 0.3101 y: 1.0890;
		curveToX: 0.3688 y: 1.0773 control1X: 0.3446 y: 1.0890 control2X: 0.3591 y: 1.0851;
		curveToX: 0.3835 y: 1.0431 control1X: 0.3786 y: 1.0695 control2X: 0.3835 y: 1.0581;
		curveToX: 0.3727 y: 1.0070 control1X: 0.3835 y: 1.0333 control2X: 0.3799 y: 1.0213;
		curveToX: 0.3405 y: 0.9562 control1X: 0.3659 y: 0.9930 control2X: 0.3551 y: 0.9760;
		lineToX: 0.3986 y: 0.9562! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans232
	vectorGraphicsCanvas
		moveToX: 0.5998 y: 0.6603;
		lineToX: 0.5998 y: 0.7042;
		lineToX: 0.1867 y: 0.7042;
		curveToX: 0.2424 y: 0.8458 control1X: 0.1906 y: 0.7661 control2X: 0.2092 y: 0.8133;
		curveToX: 0.3820 y: 0.8942 control1X: 0.2759 y: 0.8781 control2X: 0.3224 y: 0.8942;
		curveToX: 0.4821 y: 0.8815 control1X: 0.4165 y: 0.8942 control2X: 0.4499 y: 0.8899;
		curveToX: 0.5788 y: 0.8434 control1X: 0.5147 y: 0.8730 control2X: 0.5469 y: 0.8603;
		lineToX: 0.5788 y: 0.9283;
		curveToX: 0.4797 y: 0.9596 control1X: 0.5466 y: 0.9420 control2X: 0.5135 y: 0.9524;
		curveToX: 0.3766 y: 0.9703 control1X: 0.4458 y: 0.9668 control2X: 0.4115 y: 0.9703;
		curveToX: 0.1691 y: 0.8942 control1X: 0.2894 y: 0.9703 control2X: 0.2202 y: 0.9450;
		curveToX: 0.0929 y: 0.6881 control1X: 0.1183 y: 0.8434 control2X: 0.0929 y: 0.7747;
		curveToX: 0.1652 y: 0.4752 control1X: 0.0929 y: 0.5986 control2X: 0.1170 y: 0.5276;
		curveToX: 0.3610 y: 0.3961 control1X: 0.2137 y: 0.4225 control2X: 0.2790 y: 0.3961;
		curveToX: 0.5353 y: 0.4674 control1X: 0.4346 y: 0.3961 control2X: 0.4927 y: 0.4199;
		curveToX: 0.5998 y: 0.6603 control1X: 0.5783 y: 0.5146 control2X: 0.5998 y: 0.5789;
		moveToX: 0.5099 y: 0.6339;
		curveToX: 0.4684 y: 0.5162 control1X: 0.5093 y: 0.5848 control2X: 0.4954 y: 0.5455;
		curveToX: 0.3620 y: 0.4723 control1X: 0.4417 y: 0.4869 control2X: 0.4063 y: 0.4723;
		curveToX: 0.2414 y: 0.5148 control1X: 0.3119 y: 0.4723 control2X: 0.2717 y: 0.4865;
		curveToX: 0.1896 y: 0.6344 control1X: 0.2114 y: 0.5431 control2X: 0.1942 y: 0.5830;
		lineToX: 0.5099 y: 0.6339;
		moveToX: 0.2848 y: 0.1564;
		lineToX: 0.4225 y: 0.3390;
		lineToX: 0.3478 y: 0.3390;
		lineToX: 0.1886 y: 0.1564;
		lineToX: 0.2848 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans233
	vectorGraphicsCanvas
		moveToX: 0.5998 y: 0.6603;
		lineToX: 0.5998 y: 0.7042;
		lineToX: 0.1867 y: 0.7042;
		curveToX: 0.2424 y: 0.8458 control1X: 0.1906 y: 0.7661 control2X: 0.2092 y: 0.8133;
		curveToX: 0.3820 y: 0.8942 control1X: 0.2759 y: 0.8781 control2X: 0.3224 y: 0.8942;
		curveToX: 0.4821 y: 0.8815 control1X: 0.4165 y: 0.8942 control2X: 0.4499 y: 0.8899;
		curveToX: 0.5788 y: 0.8434 control1X: 0.5147 y: 0.8730 control2X: 0.5469 y: 0.8603;
		lineToX: 0.5788 y: 0.9283;
		curveToX: 0.4797 y: 0.9596 control1X: 0.5466 y: 0.9420 control2X: 0.5135 y: 0.9524;
		curveToX: 0.3766 y: 0.9703 control1X: 0.4458 y: 0.9668 control2X: 0.4115 y: 0.9703;
		curveToX: 0.1691 y: 0.8942 control1X: 0.2894 y: 0.9703 control2X: 0.2202 y: 0.9450;
		curveToX: 0.0929 y: 0.6881 control1X: 0.1183 y: 0.8434 control2X: 0.0929 y: 0.7747;
		curveToX: 0.1652 y: 0.4752 control1X: 0.0929 y: 0.5986 control2X: 0.1170 y: 0.5276;
		curveToX: 0.3610 y: 0.3961 control1X: 0.2137 y: 0.4225 control2X: 0.2790 y: 0.3961;
		curveToX: 0.5353 y: 0.4674 control1X: 0.4346 y: 0.3961 control2X: 0.4927 y: 0.4199;
		curveToX: 0.5998 y: 0.6603 control1X: 0.5783 y: 0.5146 control2X: 0.5998 y: 0.5789;
		moveToX: 0.5099 y: 0.6339;
		curveToX: 0.4684 y: 0.5162 control1X: 0.5093 y: 0.5848 control2X: 0.4954 y: 0.5455;
		curveToX: 0.3620 y: 0.4723 control1X: 0.4417 y: 0.4869 control2X: 0.4063 y: 0.4723;
		curveToX: 0.2414 y: 0.5148 control1X: 0.3119 y: 0.4723 control2X: 0.2717 y: 0.4865;
		curveToX: 0.1896 y: 0.6344 control1X: 0.2114 y: 0.5431 control2X: 0.1942 y: 0.5830;
		lineToX: 0.5099 y: 0.6339;
		moveToX: 0.4235 y: 0.1564;
		lineToX: 0.5207 y: 0.1564;
		lineToX: 0.3615 y: 0.3400;
		lineToX: 0.2868 y: 0.3400;
		lineToX: 0.4235 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans234
	vectorGraphicsCanvas
		moveToX: 0.5998 y: 0.6603;
		lineToX: 0.5998 y: 0.7042;
		lineToX: 0.1867 y: 0.7042;
		curveToX: 0.2424 y: 0.8458 control1X: 0.1906 y: 0.7661 control2X: 0.2092 y: 0.8133;
		curveToX: 0.3820 y: 0.8942 control1X: 0.2759 y: 0.8781 control2X: 0.3224 y: 0.8942;
		curveToX: 0.4821 y: 0.8815 control1X: 0.4165 y: 0.8942 control2X: 0.4499 y: 0.8899;
		curveToX: 0.5788 y: 0.8434 control1X: 0.5147 y: 0.8730 control2X: 0.5469 y: 0.8603;
		lineToX: 0.5788 y: 0.9283;
		curveToX: 0.4797 y: 0.9596 control1X: 0.5466 y: 0.9420 control2X: 0.5135 y: 0.9524;
		curveToX: 0.3766 y: 0.9703 control1X: 0.4458 y: 0.9668 control2X: 0.4115 y: 0.9703;
		curveToX: 0.1691 y: 0.8942 control1X: 0.2894 y: 0.9703 control2X: 0.2202 y: 0.9450;
		curveToX: 0.0929 y: 0.6881 control1X: 0.1183 y: 0.8434 control2X: 0.0929 y: 0.7747;
		curveToX: 0.1652 y: 0.4752 control1X: 0.0929 y: 0.5986 control2X: 0.1170 y: 0.5276;
		curveToX: 0.3610 y: 0.3961 control1X: 0.2137 y: 0.4225 control2X: 0.2790 y: 0.3961;
		curveToX: 0.5353 y: 0.4674 control1X: 0.4346 y: 0.3961 control2X: 0.4927 y: 0.4199;
		curveToX: 0.5998 y: 0.6603 control1X: 0.5783 y: 0.5146 control2X: 0.5998 y: 0.5789;
		moveToX: 0.5099 y: 0.6339;
		curveToX: 0.4684 y: 0.5162 control1X: 0.5093 y: 0.5848 control2X: 0.4954 y: 0.5455;
		curveToX: 0.3620 y: 0.4723 control1X: 0.4417 y: 0.4869 control2X: 0.4063 y: 0.4723;
		curveToX: 0.2414 y: 0.5148 control1X: 0.3119 y: 0.4723 control2X: 0.2717 y: 0.4865;
		curveToX: 0.1896 y: 0.6344 control1X: 0.2114 y: 0.5431 control2X: 0.1942 y: 0.5830;
		lineToX: 0.5099 y: 0.6339;
		moveToX: 0.3195 y: 0.1564;
		lineToX: 0.3918 y: 0.1564;
		lineToX: 0.5114 y: 0.3400;
		lineToX: 0.4435 y: 0.3400;
		lineToX: 0.3556 y: 0.2203;
		lineToX: 0.2677 y: 0.3400;
		lineToX: 0.1999 y: 0.3400;
		lineToX: 0.3195 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans235
	vectorGraphicsCanvas
		moveToX: 0.5998 y: 0.6603;
		lineToX: 0.5998 y: 0.7042;
		lineToX: 0.1867 y: 0.7042;
		curveToX: 0.2424 y: 0.8458 control1X: 0.1906 y: 0.7661 control2X: 0.2092 y: 0.8133;
		curveToX: 0.3820 y: 0.8942 control1X: 0.2759 y: 0.8781 control2X: 0.3224 y: 0.8942;
		curveToX: 0.4821 y: 0.8815 control1X: 0.4165 y: 0.8942 control2X: 0.4499 y: 0.8899;
		curveToX: 0.5788 y: 0.8434 control1X: 0.5147 y: 0.8730 control2X: 0.5469 y: 0.8603;
		lineToX: 0.5788 y: 0.9283;
		curveToX: 0.4797 y: 0.9596 control1X: 0.5466 y: 0.9420 control2X: 0.5135 y: 0.9524;
		curveToX: 0.3766 y: 0.9703 control1X: 0.4458 y: 0.9668 control2X: 0.4115 y: 0.9703;
		curveToX: 0.1691 y: 0.8942 control1X: 0.2894 y: 0.9703 control2X: 0.2202 y: 0.9450;
		curveToX: 0.0929 y: 0.6881 control1X: 0.1183 y: 0.8434 control2X: 0.0929 y: 0.7747;
		curveToX: 0.1652 y: 0.4752 control1X: 0.0929 y: 0.5986 control2X: 0.1170 y: 0.5276;
		curveToX: 0.3610 y: 0.3961 control1X: 0.2137 y: 0.4225 control2X: 0.2790 y: 0.3961;
		curveToX: 0.5353 y: 0.4674 control1X: 0.4346 y: 0.3961 control2X: 0.4927 y: 0.4199;
		curveToX: 0.5998 y: 0.6603 control1X: 0.5783 y: 0.5146 control2X: 0.5998 y: 0.5789;
		moveToX: 0.5099 y: 0.6339;
		curveToX: 0.4684 y: 0.5162 control1X: 0.5093 y: 0.5848 control2X: 0.4954 y: 0.5455;
		curveToX: 0.3620 y: 0.4723 control1X: 0.4417 y: 0.4869 control2X: 0.4063 y: 0.4723;
		curveToX: 0.2414 y: 0.5148 control1X: 0.3119 y: 0.4723 control2X: 0.2717 y: 0.4865;
		curveToX: 0.1896 y: 0.6344 control1X: 0.2114 y: 0.5431 control2X: 0.1942 y: 0.5830;
		lineToX: 0.5099 y: 0.6339;
		moveToX: 0.4015 y: 0.1984;
		lineToX: 0.5007 y: 0.1984;
		lineToX: 0.5007 y: 0.2970;
		lineToX: 0.4015 y: 0.2970;
		lineToX: 0.4015 y: 0.1984;
		moveToX: 0.2106 y: 0.1984;
		lineToX: 0.3097 y: 0.1984;
		lineToX: 0.3097 y: 0.2970;
		lineToX: 0.2106 y: 0.2970;
		lineToX: 0.2106 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans236
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.1061 y: 0.1564;
		lineToX: 0.2438 y: 0.3390;
		lineToX: 0.1691 y: 0.3390;
		lineToX: 0.0099 y: 0.1564;
		lineToX: 0.1061 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans237
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.2448 y: 0.1564;
		lineToX: 0.3420 y: 0.1564;
		lineToX: 0.1828 y: 0.3400;
		lineToX: 0.1081 y: 0.3400;
		lineToX: 0.2448 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans238
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.1408 y: 0.1564;
		lineToX: 0.2131 y: 0.1564;
		lineToX: 0.3327 y: 0.3400;
		lineToX: 0.2648 y: 0.3400;
		lineToX: 0.1769 y: 0.2203;
		lineToX: 0.0890 y: 0.3400;
		lineToX: 0.0212 y: 0.3400;
		lineToX: 0.1408 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans239
	vectorGraphicsCanvas
		moveToX: 0.1320 y: 0.4093;
		lineToX: 0.2218 y: 0.4093;
		lineToX: 0.2218 y: 0.9562;
		lineToX: 0.1320 y: 0.9562;
		lineToX: 0.1320 y: 0.4093;
		moveToX: 0.2228 y: 0.1984;
		lineToX: 0.3219 y: 0.1984;
		lineToX: 0.3219 y: 0.2970;
		lineToX: 0.2228 y: 0.2970;
		lineToX: 0.2228 y: 0.1984;
		moveToX: 0.0319 y: 0.1984;
		lineToX: 0.1310 y: 0.1984;
		lineToX: 0.1310 y: 0.2970;
		lineToX: 0.0319 y: 0.2970;
		lineToX: 0.0319 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans240
	vectorGraphicsCanvas
		moveToX: 0.4469 y: 0.5094;
		curveToX: 0.4010 y: 0.4972 control1X: 0.4307 y: 0.5039 control2X: 0.4154 y: 0.4998;
		curveToX: 0.3595 y: 0.4933 control1X: 0.3867 y: 0.4946 control2X: 0.3729 y: 0.4933;
		curveToX: 0.2326 y: 0.5460 control1X: 0.3052 y: 0.4933 control2X: 0.2629 y: 0.5109;
		curveToX: 0.1877 y: 0.6940 control1X: 0.2026 y: 0.5812 control2X: 0.1877 y: 0.6305;
		curveToX: 0.2301 y: 0.8400 control1X: 0.1877 y: 0.7552 control2X: 0.2018 y: 0.8038;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2585 y: 0.8761 control2X: 0.2964 y: 0.8942;
		curveToX: 0.4572 y: 0.8400 control1X: 0.3911 y: 0.8942 control2X: 0.4289 y: 0.8761;
		curveToX: 0.4997 y: 0.6940 control1X: 0.4855 y: 0.8038 control2X: 0.4997 y: 0.7552;
		curveToX: 0.4865 y: 0.5885 control1X: 0.4997 y: 0.6543 control2X: 0.4953 y: 0.6191;
		curveToX: 0.4469 y: 0.5094 control1X: 0.4777 y: 0.5579 control2X: 0.4645 y: 0.5315;
		moveToX: 0.4777 y: 0.3991;
		curveToX: 0.5671 y: 0.5392 control1X: 0.5187 y: 0.4456 control2X: 0.5485 y: 0.4923;
		curveToX: 0.5949 y: 0.6940 control1X: 0.5856 y: 0.5861 control2X: 0.5949 y: 0.6377;
		curveToX: 0.5256 y: 0.8947 control1X: 0.5949 y: 0.7770 control2X: 0.5718 y: 0.8439;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4793 y: 0.9451 control2X: 0.4188 y: 0.9703;
		curveToX: 0.1618 y: 0.8947 control1X: 0.2687 y: 0.9703 control2X: 0.2080 y: 0.9451;
		curveToX: 0.0929 y: 0.6940 control1X: 0.1159 y: 0.8439 control2X: 0.0929 y: 0.7770;
		curveToX: 0.1603 y: 0.4962 control1X: 0.0929 y: 0.6126 control2X: 0.1154 y: 0.5467;
		curveToX: 0.3356 y: 0.4200 control1X: 0.2052 y: 0.4454 control2X: 0.2637 y: 0.4200;
		curveToX: 0.3571 y: 0.4215 control1X: 0.3415 y: 0.4200 control2X: 0.3486 y: 0.4205;
		curveToX: 0.3903 y: 0.4249 control1X: 0.3656 y: 0.4222 control2X: 0.3766 y: 0.4233;
		lineToX: 0.3127 y: 0.3370;
		lineToX: 0.1569 y: 0.3893;
		lineToX: 0.1408 y: 0.3409;
		lineToX: 0.2780 y: 0.2960;
		lineToX: 0.1896 y: 0.1964;
		lineToX: 0.3010 y: 0.1964;
		lineToX: 0.3630 y: 0.2672;
		lineToX: 0.5256 y: 0.2130;
		lineToX: 0.5417 y: 0.2604;
		lineToX: 0.3976 y: 0.3082;
		lineToX: 0.4777 y: 0.3991! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans241
	vectorGraphicsCanvas
		moveToX: 0.5866 y: 0.6261;
		lineToX: 0.5866 y: 0.9562;
		lineToX: 0.4968 y: 0.9562;
		lineToX: 0.4968 y: 0.6290;
		curveToX: 0.4665 y: 0.5128 control1X: 0.4968 y: 0.5773 control2X: 0.4867 y: 0.5385;
		curveToX: 0.3757 y: 0.4742 control1X: 0.4463 y: 0.4871 control2X: 0.4160 y: 0.4742;
		curveToX: 0.2609 y: 0.5206 control1X: 0.3272 y: 0.4742 control2X: 0.2889 y: 0.4897;
		curveToX: 0.2189 y: 0.6471 control1X: 0.2329 y: 0.5516 control2X: 0.2189 y: 0.5937;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.4093;
		lineToX: 0.2189 y: 0.4093;
		lineToX: 0.2189 y: 0.4943;
		curveToX: 0.2946 y: 0.4205 control1X: 0.2404 y: 0.4614 control2X: 0.2656 y: 0.4368;
		curveToX: 0.3957 y: 0.3961 control1X: 0.3239 y: 0.4043 control2X: 0.3576 y: 0.3961;
		curveToX: 0.5383 y: 0.4547 control1X: 0.4585 y: 0.3961 control2X: 0.5060 y: 0.4157;
		curveToX: 0.5866 y: 0.6261 control1X: 0.5705 y: 0.4935 control2X: 0.5866 y: 0.5506;
		moveToX: 0.3600 y: 0.2872;
		lineToX: 0.3322 y: 0.2604;
		curveToX: 0.3132 y: 0.2462 control1X: 0.3250 y: 0.2539 control2X: 0.3187 y: 0.2492;
		curveToX: 0.2990 y: 0.2413 control1X: 0.3079 y: 0.2430 control2X: 0.3032 y: 0.2413;
		curveToX: 0.2717 y: 0.2594 control1X: 0.2866 y: 0.2413 control2X: 0.2775 y: 0.2474;
		curveToX: 0.2619 y: 0.3170 control1X: 0.2658 y: 0.2711 control2X: 0.2625 y: 0.2903;
		lineToX: 0.2009 y: 0.3170;
		curveToX: 0.2267 y: 0.2155 control1X: 0.2015 y: 0.2731 control2X: 0.2101 y: 0.2392;
		curveToX: 0.2961 y: 0.1793 control1X: 0.2433 y: 0.1914 control2X: 0.2664 y: 0.1793;
		curveToX: 0.3302 y: 0.1862 control1X: 0.3084 y: 0.1793 control2X: 0.3198 y: 0.1816;
		curveToX: 0.3639 y: 0.2091 control1X: 0.3407 y: 0.1907 control2X: 0.3519 y: 0.1984;
		lineToX: 0.3918 y: 0.2360;
		curveToX: 0.4103 y: 0.2506 control1X: 0.3989 y: 0.2425 control2X: 0.4051 y: 0.2474;
		curveToX: 0.4250 y: 0.2550 control1X: 0.4159 y: 0.2535 control2X: 0.4207 y: 0.2550;
		curveToX: 0.4523 y: 0.2374 control1X: 0.4373 y: 0.2550 control2X: 0.4465 y: 0.2492;
		curveToX: 0.4621 y: 0.1793 control1X: 0.4582 y: 0.2254 control2X: 0.4614 y: 0.2060;
		lineToX: 0.5231 y: 0.1793;
		curveToX: 0.4972 y: 0.2814 control1X: 0.5225 y: 0.2233 control2X: 0.5138 y: 0.2573;
		curveToX: 0.4279 y: 0.3170 control1X: 0.4806 y: 0.3051 control2X: 0.4575 y: 0.3170;
		curveToX: 0.3937 y: 0.3102 control1X: 0.4155 y: 0.3170 control2X: 0.4041 y: 0.3147;
		curveToX: 0.3600 y: 0.2872 control1X: 0.3833 y: 0.3056 control2X: 0.3721 y: 0.2980! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans242
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		moveToX: 0.2731 y: 0.1564;
		lineToX: 0.4108 y: 0.3390;
		lineToX: 0.3361 y: 0.3390;
		lineToX: 0.1769 y: 0.1564;
		lineToX: 0.2731 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans243
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		moveToX: 0.4118 y: 0.1564;
		lineToX: 0.5090 y: 0.1564;
		lineToX: 0.3498 y: 0.3400;
		lineToX: 0.2751 y: 0.3400;
		lineToX: 0.4118 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans244
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		moveToX: 0.3078 y: 0.1564;
		lineToX: 0.3801 y: 0.1564;
		lineToX: 0.4997 y: 0.3400;
		lineToX: 0.4318 y: 0.3400;
		lineToX: 0.3439 y: 0.2203;
		lineToX: 0.2560 y: 0.3400;
		lineToX: 0.1882 y: 0.3400;
		lineToX: 0.3078 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans245
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		moveToX: 0.3420 y: 0.2872;
		lineToX: 0.3141 y: 0.2604;
		curveToX: 0.2951 y: 0.2462 control1X: 0.3070 y: 0.2539 control2X: 0.3006 y: 0.2492;
		curveToX: 0.2809 y: 0.2413 control1X: 0.2899 y: 0.2430 control2X: 0.2852 y: 0.2413;
		curveToX: 0.2536 y: 0.2594 control1X: 0.2686 y: 0.2413 control2X: 0.2594 y: 0.2474;
		curveToX: 0.2438 y: 0.3170 control1X: 0.2477 y: 0.2711 control2X: 0.2445 y: 0.2903;
		lineToX: 0.1828 y: 0.3170;
		curveToX: 0.2087 y: 0.2155 control1X: 0.1834 y: 0.2731 control2X: 0.1921 y: 0.2392;
		curveToX: 0.2780 y: 0.1793 control1X: 0.2253 y: 0.1914 control2X: 0.2484 y: 0.1793;
		curveToX: 0.3122 y: 0.1862 control1X: 0.2904 y: 0.1793 control2X: 0.3018 y: 0.1816;
		curveToX: 0.3459 y: 0.2091 control1X: 0.3226 y: 0.1907 control2X: 0.3338 y: 0.1984;
		lineToX: 0.3737 y: 0.2360;
		curveToX: 0.3923 y: 0.2506 control1X: 0.3809 y: 0.2425 control2X: 0.3871 y: 0.2474;
		curveToX: 0.4069 y: 0.2550 control1X: 0.3978 y: 0.2535 control2X: 0.4027 y: 0.2550;
		curveToX: 0.4343 y: 0.2374 control1X: 0.4193 y: 0.2550 control2X: 0.4284 y: 0.2492;
		curveToX: 0.4440 y: 0.1793 control1X: 0.4401 y: 0.2254 control2X: 0.4434 y: 0.2060;
		lineToX: 0.5051 y: 0.1793;
		curveToX: 0.4792 y: 0.2814 control1X: 0.5044 y: 0.2233 control2X: 0.4958 y: 0.2573;
		curveToX: 0.4098 y: 0.3170 control1X: 0.4626 y: 0.3051 control2X: 0.4395 y: 0.3170;
		curveToX: 0.3757 y: 0.3102 control1X: 0.3975 y: 0.3170 control2X: 0.3861 y: 0.3147;
		curveToX: 0.3420 y: 0.2872 control1X: 0.3652 y: 0.3056 control2X: 0.3540 y: 0.2980! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans246
	vectorGraphicsCanvas
		moveToX: 0.3439 y: 0.4723;
		curveToX: 0.2297 y: 0.5289 control1X: 0.2957 y: 0.4723 control2X: 0.2577 y: 0.4912;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2017 y: 0.5664 control2X: 0.1877 y: 0.6178;
		curveToX: 0.2292 y: 0.8380 control1X: 0.1877 y: 0.7487 control2X: 0.2015 y: 0.8003;
		curveToX: 0.3439 y: 0.8942 control1X: 0.2572 y: 0.8755 control2X: 0.2954 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4577 y: 0.5294 control1X: 0.4997 y: 0.6185 control2X: 0.4857 y: 0.5672;
		curveToX: 0.3439 y: 0.4723 control1X: 0.4297 y: 0.4913 control2X: 0.3918 y: 0.4723;
		moveToX: 0.3439 y: 0.3961;
		curveToX: 0.5280 y: 0.4723 control1X: 0.4220 y: 0.3961 control2X: 0.4834 y: 0.4215;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5726 y: 0.5231 control2X: 0.5949 y: 0.5934;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.1593 y: 0.8942 control1X: 0.2655 y: 0.9703 control2X: 0.2039 y: 0.9450;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1151 y: 0.8431 control2X: 0.0929 y: 0.7728;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		moveToX: 0.3898 y: 0.1984;
		lineToX: 0.4889 y: 0.1984;
		lineToX: 0.4889 y: 0.2970;
		lineToX: 0.3898 y: 0.2970;
		lineToX: 0.3898 y: 0.1984;
		moveToX: 0.1989 y: 0.1984;
		lineToX: 0.2980 y: 0.1984;
		lineToX: 0.2980 y: 0.2970;
		lineToX: 0.1989 y: 0.2970;
		lineToX: 0.1989 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans247
	vectorGraphicsCanvas
		moveToX: 0.3967 y: 0.4020;
		lineToX: 0.5168 y: 0.4020;
		lineToX: 0.5168 y: 0.5221;
		lineToX: 0.3967 y: 0.5221;
		lineToX: 0.3967 y: 0.4020;
		moveToX: 0.3967 y: 0.7633;
		lineToX: 0.5168 y: 0.7633;
		lineToX: 0.5168 y: 0.8829;
		lineToX: 0.3967 y: 0.8829;
		lineToX: 0.3967 y: 0.7633;
		moveToX: 0.1437 y: 0.6012;
		lineToX: 0.7697 y: 0.6012;
		lineToX: 0.7697 y: 0.6842;
		lineToX: 0.1437 y: 0.6842;
		lineToX: 0.1437 y: 0.6012! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans248
	vectorGraphicsCanvas
		moveToX: 0.4797 y: 0.5651;
		lineToX: 0.2419 y: 0.8541;
		curveToX: 0.2868 y: 0.8844 control1X: 0.2552 y: 0.8678 control2X: 0.2702 y: 0.8779;
		curveToX: 0.3439 y: 0.8942 control1X: 0.3037 y: 0.8909 control2X: 0.3228 y: 0.8942;
		curveToX: 0.4577 y: 0.8375 control1X: 0.3918 y: 0.8942 control2X: 0.4297 y: 0.8753;
		curveToX: 0.4997 y: 0.6832 control1X: 0.4857 y: 0.7998 control2X: 0.4997 y: 0.7483;
		curveToX: 0.4948 y: 0.6163 control1X: 0.4997 y: 0.6575 control2X: 0.4981 y: 0.6352;
		curveToX: 0.4797 y: 0.5651 control1X: 0.4915 y: 0.5971 control2X: 0.4865 y: 0.5800;
		moveToX: 0.4450 y: 0.5123;
		curveToX: 0.3991 y: 0.4825 control1X: 0.4313 y: 0.4990 control2X: 0.4160 y: 0.4891;
		curveToX: 0.3439 y: 0.4723 control1X: 0.3825 y: 0.4757 control2X: 0.3641 y: 0.4723;
		curveToX: 0.2287 y: 0.5299 control1X: 0.2948 y: 0.4723 control2X: 0.2564 y: 0.4915;
		curveToX: 0.1877 y: 0.6901 control1X: 0.2013 y: 0.5680 control2X: 0.1877 y: 0.6214;
		curveToX: 0.1921 y: 0.7521 control1X: 0.1877 y: 0.7135 control2X: 0.1891 y: 0.7342;
		curveToX: 0.2067 y: 0.8014 control1X: 0.1953 y: 0.7700 control2X: 0.2002 y: 0.7864;
		lineToX: 0.4450 y: 0.5123;
		moveToX: 0.1457 y: 0.8751;
		curveToX: 0.1061 y: 0.7897 control1X: 0.1281 y: 0.8501 control2X: 0.1149 y: 0.8216;
		curveToX: 0.0929 y: 0.6832 control1X: 0.0973 y: 0.7575 control2X: 0.0929 y: 0.7220;
		curveToX: 0.1593 y: 0.4723 control1X: 0.0929 y: 0.5934 control2X: 0.1151 y: 0.5231;
		curveToX: 0.3439 y: 0.3961 control1X: 0.2039 y: 0.4215 control2X: 0.2655 y: 0.3961;
		curveToX: 0.4279 y: 0.4088 control1X: 0.3742 y: 0.3961 control2X: 0.4022 y: 0.4004;
		curveToX: 0.4997 y: 0.4464 control1X: 0.4539 y: 0.4170 control2X: 0.4779 y: 0.4295;
		lineToX: 0.5676 y: 0.3644;
		lineToX: 0.6139 y: 0.4030;
		lineToX: 0.5426 y: 0.4904;
		curveToX: 0.5817 y: 0.5763 control1X: 0.5599 y: 0.5154 control2X: 0.5729 y: 0.5441;
		curveToX: 0.5949 y: 0.6832 control1X: 0.5905 y: 0.6085 control2X: 0.5949 y: 0.6442;
		curveToX: 0.5280 y: 0.8942 control1X: 0.5949 y: 0.7728 control2X: 0.5726 y: 0.8431;
		curveToX: 0.3439 y: 0.9703 control1X: 0.4834 y: 0.9450 control2X: 0.4220 y: 0.9703;
		curveToX: 0.2575 y: 0.9576 control1X: 0.3127 y: 0.9703 control2X: 0.2839 y: 0.9661;
		curveToX: 0.1877 y: 0.9200 control1X: 0.2315 y: 0.9492 control2X: 0.2082 y: 0.9367;
		lineToX: 0.1198 y: 1.0021;
		lineToX: 0.0729 y: 0.9640;
		lineToX: 0.1457 y: 0.8751! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans249
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 0.7404;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7369;
		curveToX: 0.2428 y: 0.8536 control1X: 0.2126 y: 0.7887 control2X: 0.2227 y: 0.8276;
		curveToX: 0.3337 y: 0.8922 control1X: 0.2630 y: 0.8794 control2X: 0.2933 y: 0.8922;
		curveToX: 0.4484 y: 0.8458 control1X: 0.3822 y: 0.8922 control2X: 0.4204 y: 0.8768;
		curveToX: 0.4909 y: 0.7194 control1X: 0.4767 y: 0.8149 control2X: 0.4909 y: 0.7728;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.4909 y: 0.8722;
		curveToX: 0.4147 y: 0.9464 control1X: 0.4691 y: 0.9054 control2X: 0.4437 y: 0.9301;
		curveToX: 0.3146 y: 0.9703 control1X: 0.3861 y: 0.9624 control2X: 0.3527 y: 0.9703;
		curveToX: 0.1716 y: 0.9117 control1X: 0.2518 y: 0.9703 control2X: 0.2041 y: 0.9508;
		curveToX: 0.1227 y: 0.7404 control1X: 0.1390 y: 0.8727 control2X: 0.1227 y: 0.8156;
		moveToX: 0.2770 y: 0.1564;
		lineToX: 0.4147 y: 0.3390;
		lineToX: 0.3400 y: 0.3390;
		lineToX: 0.1808 y: 0.1564;
		lineToX: 0.2770 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans250
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 0.7404;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7369;
		curveToX: 0.2428 y: 0.8536 control1X: 0.2126 y: 0.7887 control2X: 0.2227 y: 0.8276;
		curveToX: 0.3337 y: 0.8922 control1X: 0.2630 y: 0.8794 control2X: 0.2933 y: 0.8922;
		curveToX: 0.4484 y: 0.8458 control1X: 0.3822 y: 0.8922 control2X: 0.4204 y: 0.8768;
		curveToX: 0.4909 y: 0.7194 control1X: 0.4767 y: 0.8149 control2X: 0.4909 y: 0.7728;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.4909 y: 0.8722;
		curveToX: 0.4147 y: 0.9464 control1X: 0.4691 y: 0.9054 control2X: 0.4437 y: 0.9301;
		curveToX: 0.3146 y: 0.9703 control1X: 0.3861 y: 0.9624 control2X: 0.3527 y: 0.9703;
		curveToX: 0.1716 y: 0.9117 control1X: 0.2518 y: 0.9703 control2X: 0.2041 y: 0.9508;
		curveToX: 0.1227 y: 0.7404 control1X: 0.1390 y: 0.8727 control2X: 0.1227 y: 0.8156;
		moveToX: 0.4157 y: 0.1564;
		lineToX: 0.5129 y: 0.1564;
		lineToX: 0.3537 y: 0.3400;
		lineToX: 0.2790 y: 0.3400;
		lineToX: 0.4157 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans251
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 0.7404;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7369;
		curveToX: 0.2428 y: 0.8536 control1X: 0.2126 y: 0.7887 control2X: 0.2227 y: 0.8276;
		curveToX: 0.3337 y: 0.8922 control1X: 0.2630 y: 0.8794 control2X: 0.2933 y: 0.8922;
		curveToX: 0.4484 y: 0.8458 control1X: 0.3822 y: 0.8922 control2X: 0.4204 y: 0.8768;
		curveToX: 0.4909 y: 0.7194 control1X: 0.4767 y: 0.8149 control2X: 0.4909 y: 0.7728;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.4909 y: 0.8722;
		curveToX: 0.4147 y: 0.9464 control1X: 0.4691 y: 0.9054 control2X: 0.4437 y: 0.9301;
		curveToX: 0.3146 y: 0.9703 control1X: 0.3861 y: 0.9624 control2X: 0.3527 y: 0.9703;
		curveToX: 0.1716 y: 0.9117 control1X: 0.2518 y: 0.9703 control2X: 0.2041 y: 0.9508;
		curveToX: 0.1227 y: 0.7404 control1X: 0.1390 y: 0.8727 control2X: 0.1227 y: 0.8156;
		moveToX: 0.3117 y: 0.1564;
		lineToX: 0.3840 y: 0.1564;
		lineToX: 0.5036 y: 0.3400;
		lineToX: 0.4357 y: 0.3400;
		lineToX: 0.3478 y: 0.2203;
		lineToX: 0.2599 y: 0.3400;
		lineToX: 0.1921 y: 0.3400;
		lineToX: 0.3117 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans252
	vectorGraphicsCanvas
		moveToX: 0.1227 y: 0.7404;
		lineToX: 0.1227 y: 0.4093;
		lineToX: 0.2126 y: 0.4093;
		lineToX: 0.2126 y: 0.7369;
		curveToX: 0.2428 y: 0.8536 control1X: 0.2126 y: 0.7887 control2X: 0.2227 y: 0.8276;
		curveToX: 0.3337 y: 0.8922 control1X: 0.2630 y: 0.8794 control2X: 0.2933 y: 0.8922;
		curveToX: 0.4484 y: 0.8458 control1X: 0.3822 y: 0.8922 control2X: 0.4204 y: 0.8768;
		curveToX: 0.4909 y: 0.7194 control1X: 0.4767 y: 0.8149 control2X: 0.4909 y: 0.7728;
		lineToX: 0.4909 y: 0.4093;
		lineToX: 0.5807 y: 0.4093;
		lineToX: 0.5807 y: 0.9562;
		lineToX: 0.4909 y: 0.9562;
		lineToX: 0.4909 y: 0.8722;
		curveToX: 0.4147 y: 0.9464 control1X: 0.4691 y: 0.9054 control2X: 0.4437 y: 0.9301;
		curveToX: 0.3146 y: 0.9703 control1X: 0.3861 y: 0.9624 control2X: 0.3527 y: 0.9703;
		curveToX: 0.1716 y: 0.9117 control1X: 0.2518 y: 0.9703 control2X: 0.2041 y: 0.9508;
		curveToX: 0.1227 y: 0.7404 control1X: 0.1390 y: 0.8727 control2X: 0.1227 y: 0.8156;
		moveToX: 0.3937 y: 0.1984;
		lineToX: 0.4928 y: 0.1984;
		lineToX: 0.4928 y: 0.2970;
		lineToX: 0.3937 y: 0.2970;
		lineToX: 0.3937 y: 0.1984;
		moveToX: 0.2028 y: 0.1984;
		lineToX: 0.3019 y: 0.1984;
		lineToX: 0.3019 y: 0.2970;
		lineToX: 0.2028 y: 0.2970;
		lineToX: 0.2028 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans253
	vectorGraphicsCanvas
		moveToX: 0.3595 y: 1.0070;
		curveToX: 0.2853 y: 1.1344 control1X: 0.3342 y: 1.0721 control2X: 0.3094 y: 1.1145;
		curveToX: 0.1886 y: 1.1642 control1X: 0.2612 y: 1.1543 control2X: 0.2290 y: 1.1642;
		lineToX: 0.1169 y: 1.1642;
		lineToX: 0.1169 y: 1.0890;
		lineToX: 0.1696 y: 1.0890;
		curveToX: 0.2272 y: 1.0714 control1X: 0.1943 y: 1.0890 control2X: 0.2135 y: 1.0831;
		curveToX: 0.2726 y: 0.9884 control1X: 0.2409 y: 1.0597 control2X: 0.2560 y: 1.0320;
		lineToX: 0.2887 y: 0.9474;
		lineToX: 0.0676 y: 0.4093;
		lineToX: 0.1628 y: 0.4093;
		lineToX: 0.3337 y: 0.8370;
		lineToX: 0.5046 y: 0.4093;
		lineToX: 0.5998 y: 0.4093;
		lineToX: 0.3595 y: 1.0070;
		moveToX: 0.4015 y: 0.1564;
		lineToX: 0.4987 y: 0.1564;
		lineToX: 0.3395 y: 0.3400;
		lineToX: 0.2648 y: 0.3400;
		lineToX: 0.4015 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans254
	vectorGraphicsCanvas
		moveToX: 0.2189 y: 0.8742;
		lineToX: 0.2189 y: 1.1642;
		lineToX: 0.1286 y: 1.1642;
		lineToX: 0.1286 y: 0.1964;
		lineToX: 0.2189 y: 0.1964;
		lineToX: 0.2189 y: 0.4923;
		curveToX: 0.2902 y: 0.4200 control1X: 0.2378 y: 0.4598 control2X: 0.2616 y: 0.4357;
		curveToX: 0.3937 y: 0.3961 control1X: 0.3192 y: 0.4041 control2X: 0.3537 y: 0.3961;
		curveToX: 0.5553 y: 0.4752 control1X: 0.4601 y: 0.3961 control2X: 0.5140 y: 0.4225;
		curveToX: 0.6178 y: 0.6832 control1X: 0.5970 y: 0.5280 control2X: 0.6178 y: 0.5973;
		curveToX: 0.5553 y: 0.8912 control1X: 0.6178 y: 0.7692 control2X: 0.5970 y: 0.8385;
		curveToX: 0.3937 y: 0.9703 control1X: 0.5140 y: 0.9440 control2X: 0.4601 y: 0.9703;
		curveToX: 0.2902 y: 0.9469 control1X: 0.3537 y: 0.9703 control2X: 0.3192 y: 0.9625;
		curveToX: 0.2189 y: 0.8742 control1X: 0.2616 y: 0.9310 control2X: 0.2378 y: 0.9067;
		moveToX: 0.5246 y: 0.6832;
		curveToX: 0.4836 y: 0.5280 control1X: 0.5246 y: 0.6172 control2X: 0.5109 y: 0.5654;
		curveToX: 0.3718 y: 0.4713 control1X: 0.4565 y: 0.4902 control2X: 0.4193 y: 0.4713;
		curveToX: 0.2594 y: 0.5280 control1X: 0.3242 y: 0.4713 control2X: 0.2868 y: 0.4902;
		curveToX: 0.2189 y: 0.6832 control1X: 0.2324 y: 0.5654 control2X: 0.2189 y: 0.6172;
		curveToX: 0.2594 y: 0.8390 control1X: 0.2189 y: 0.7493 control2X: 0.2324 y: 0.8012;
		curveToX: 0.3718 y: 0.8951 control1X: 0.2868 y: 0.8764 control2X: 0.3242 y: 0.8951;
		curveToX: 0.4836 y: 0.8390 control1X: 0.4193 y: 0.8951 control2X: 0.4565 y: 0.8764;
		curveToX: 0.5246 y: 0.6832 control1X: 0.5109 y: 0.8012 control2X: 0.5246 y: 0.7493! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans255
	vectorGraphicsCanvas
		moveToX: 0.3595 y: 1.0070;
		curveToX: 0.2853 y: 1.1344 control1X: 0.3342 y: 1.0721 control2X: 0.3094 y: 1.1145;
		curveToX: 0.1886 y: 1.1642 control1X: 0.2612 y: 1.1543 control2X: 0.2290 y: 1.1642;
		lineToX: 0.1169 y: 1.1642;
		lineToX: 0.1169 y: 1.0890;
		lineToX: 0.1696 y: 1.0890;
		curveToX: 0.2272 y: 1.0714 control1X: 0.1943 y: 1.0890 control2X: 0.2135 y: 1.0831;
		curveToX: 0.2726 y: 0.9884 control1X: 0.2409 y: 1.0597 control2X: 0.2560 y: 1.0320;
		lineToX: 0.2887 y: 0.9474;
		lineToX: 0.0676 y: 0.4093;
		lineToX: 0.1628 y: 0.4093;
		lineToX: 0.3337 y: 0.8370;
		lineToX: 0.5046 y: 0.4093;
		lineToX: 0.5998 y: 0.4093;
		lineToX: 0.3595 y: 1.0070;
		moveToX: 0.3796 y: 0.1984;
		lineToX: 0.4787 y: 0.1984;
		lineToX: 0.4787 y: 0.2970;
		lineToX: 0.3796 y: 0.2970;
		lineToX: 0.3796 y: 0.1984;
		moveToX: 0.1886 y: 0.1984;
		lineToX: 0.2878 y: 0.1984;
		lineToX: 0.2878 y: 0.2970;
		lineToX: 0.1886 y: 0.2970;
		lineToX: 0.1886 y: 0.1984! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans33
	vectorGraphicsCanvas
		moveToX: 0.1886 y: 0.8322;
		lineToX: 0.2878 y: 0.8322;
		lineToX: 0.2878 y: 0.9562;
		lineToX: 0.1886 y: 0.9562;
		lineToX: 0.1886 y: 0.8322;
		moveToX: 0.1886 y: 0.2272;
		lineToX: 0.2878 y: 0.2272;
		lineToX: 0.2878 y: 0.5470;
		lineToX: 0.2780 y: 0.7213;
		lineToX: 0.1989 y: 0.7213;
		lineToX: 0.1886 y: 0.5470;
		lineToX: 0.1886 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans34
	vectorGraphicsCanvas
		moveToX: 0.2170 y: 0.2272;
		lineToX: 0.2170 y: 0.4982;
		lineToX: 0.1340 y: 0.4982;
		lineToX: 0.1340 y: 0.2272;
		lineToX: 0.2170 y: 0.2272;
		moveToX: 0.4015 y: 0.2272;
		lineToX: 0.4015 y: 0.4982;
		lineToX: 0.3185 y: 0.4982;
		lineToX: 0.3185 y: 0.2272;
		lineToX: 0.4015 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans35
	vectorGraphicsCanvas
		moveToX: 0.5490 y: 0.5162;
		lineToX: 0.4069 y: 0.5162;
		lineToX: 0.3659 y: 0.6793;
		lineToX: 0.5090 y: 0.6793;
		lineToX: 0.5490 y: 0.5162;
		moveToX: 0.4758 y: 0.2384;
		lineToX: 0.4250 y: 0.4410;
		lineToX: 0.5676 y: 0.4410;
		lineToX: 0.6188 y: 0.2384;
		lineToX: 0.6969 y: 0.2384;
		lineToX: 0.6467 y: 0.4410;
		lineToX: 0.7990 y: 0.4410;
		lineToX: 0.7990 y: 0.5162;
		lineToX: 0.6276 y: 0.5162;
		lineToX: 0.5876 y: 0.6793;
		lineToX: 0.7428 y: 0.6793;
		lineToX: 0.7428 y: 0.7540;
		lineToX: 0.5685 y: 0.7540;
		lineToX: 0.5177 y: 0.9562;
		lineToX: 0.4396 y: 0.9562;
		lineToX: 0.4899 y: 0.7540;
		lineToX: 0.3468 y: 0.7540;
		lineToX: 0.2966 y: 0.9562;
		lineToX: 0.2179 y: 0.9562;
		lineToX: 0.2687 y: 0.7540;
		lineToX: 0.1149 y: 0.7540;
		lineToX: 0.1149 y: 0.6793;
		lineToX: 0.2868 y: 0.6793;
		lineToX: 0.3278 y: 0.5162;
		lineToX: 0.1706 y: 0.5162;
		lineToX: 0.1706 y: 0.4410;
		lineToX: 0.3468 y: 0.4410;
		lineToX: 0.3967 y: 0.2384;
		lineToX: 0.4758 y: 0.2384! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans36
	vectorGraphicsCanvas
		moveToX: 0.3757 y: 1.1032;
		lineToX: 0.3268 y: 1.1032;
		lineToX: 0.3263 y: 0.9562;
		curveToX: 0.2238 y: 0.9445 control1X: 0.2922 y: 0.9555 control2X: 0.2580 y: 0.9516;
		curveToX: 0.1208 y: 0.9113 control1X: 0.1896 y: 0.9370 control2X: 0.1553 y: 0.9259;
		lineToX: 0.1208 y: 0.8234;
		curveToX: 0.2214 y: 0.8707 control1X: 0.1540 y: 0.8442 control2X: 0.1875 y: 0.8600;
		curveToX: 0.3268 y: 0.8868 control1X: 0.2555 y: 0.8811 control2X: 0.2907 y: 0.8865;
		lineToX: 0.3268 y: 0.6642;
		curveToX: 0.1696 y: 0.6046 control1X: 0.2549 y: 0.6525 control2X: 0.2025 y: 0.6326;
		curveToX: 0.1208 y: 0.4894 control1X: 0.1371 y: 0.5766 control2X: 0.1208 y: 0.5382;
		curveToX: 0.1740 y: 0.3639 control1X: 0.1208 y: 0.4363 control2X: 0.1385 y: 0.3945;
		curveToX: 0.3268 y: 0.3112 control1X: 0.2095 y: 0.3333 control2X: 0.2604 y: 0.3157;
		lineToX: 0.3268 y: 0.1964;
		lineToX: 0.3757 y: 0.1964;
		lineToX: 0.3757 y: 0.3097;
		curveToX: 0.4635 y: 0.3195 control1X: 0.4059 y: 0.3110 control2X: 0.4352 y: 0.3143;
		curveToX: 0.5466 y: 0.3400 control1X: 0.4919 y: 0.3243 control2X: 0.5195 y: 0.3312;
		lineToX: 0.5466 y: 0.4254;
		curveToX: 0.4631 y: 0.3937 control1X: 0.5195 y: 0.4117 control2X: 0.4917 y: 0.4012;
		curveToX: 0.3757 y: 0.3805 control1X: 0.4347 y: 0.3862 control2X: 0.4056 y: 0.3818;
		lineToX: 0.3757 y: 0.5890;
		curveToX: 0.5387 y: 0.6500 control1X: 0.4496 y: 0.6004 control2X: 0.5039 y: 0.6207;
		curveToX: 0.5910 y: 0.7701 control1X: 0.5736 y: 0.6793 control2X: 0.5910 y: 0.7194;
		curveToX: 0.5353 y: 0.9005 control1X: 0.5910 y: 0.8252 control2X: 0.5724 y: 0.8686;
		curveToX: 0.3757 y: 0.9552 control1X: 0.4985 y: 0.9321 control2X: 0.4453 y: 0.9503;
		lineToX: 0.3757 y: 1.1032;
		moveToX: 0.3268 y: 0.5802;
		lineToX: 0.3268 y: 0.3800;
		curveToX: 0.2404 y: 0.4122 control1X: 0.2891 y: 0.3842 control2X: 0.2603 y: 0.3950;
		curveToX: 0.2106 y: 0.4811 control1X: 0.2205 y: 0.4295 control2X: 0.2106 y: 0.4524;
		curveToX: 0.2380 y: 0.5465 control1X: 0.2106 y: 0.5091 control2X: 0.2197 y: 0.5309;
		curveToX: 0.3268 y: 0.5802 control1X: 0.2565 y: 0.5621 control2X: 0.2861 y: 0.5734;
		moveToX: 0.3757 y: 0.6740;
		lineToX: 0.3757 y: 0.8854;
		curveToX: 0.4689 y: 0.8502 control1X: 0.4170 y: 0.8798 control2X: 0.4481 y: 0.8681;
		curveToX: 0.5007 y: 0.7794 control1X: 0.4901 y: 0.8323 control2X: 0.5007 y: 0.8087;
		curveToX: 0.4704 y: 0.7111 control1X: 0.5007 y: 0.7508 control2X: 0.4906 y: 0.7280;
		curveToX: 0.3757 y: 0.6740 control1X: 0.4505 y: 0.6941 control2X: 0.4190 y: 0.6818! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans37
	vectorGraphicsCanvas
		moveToX: 0.7648 y: 0.6354;
		curveToX: 0.6979 y: 0.6715 control1X: 0.7365 y: 0.6354 control2X: 0.7142 y: 0.6474;
		curveToX: 0.6740 y: 0.7721 control1X: 0.6820 y: 0.6956 control2X: 0.6740 y: 0.7291;
		curveToX: 0.6979 y: 0.8722 control1X: 0.6740 y: 0.8144 control2X: 0.6820 y: 0.8478;
		curveToX: 0.7648 y: 0.9083 control1X: 0.7142 y: 0.8963 control2X: 0.7365 y: 0.9083;
		curveToX: 0.8302 y: 0.8722 control1X: 0.7925 y: 0.9083 control2X: 0.8143 y: 0.8963;
		curveToX: 0.8547 y: 0.7721 control1X: 0.8465 y: 0.8478 control2X: 0.8547 y: 0.8144;
		curveToX: 0.8302 y: 0.6720 control1X: 0.8547 y: 0.7295 control2X: 0.8465 y: 0.6961;
		curveToX: 0.7648 y: 0.6354 control1X: 0.8143 y: 0.6476 control2X: 0.7925 y: 0.6354;
		moveToX: 0.7648 y: 0.5734;
		curveToX: 0.8874 y: 0.6271 control1X: 0.8162 y: 0.5734 control2X: 0.8571 y: 0.5913;
		curveToX: 0.9328 y: 0.7721 control1X: 0.9176 y: 0.6629 control2X: 0.9328 y: 0.7112;
		curveToX: 0.8869 y: 0.9171 control1X: 0.9328 y: 0.8330 control2X: 0.9175 y: 0.8813;
		curveToX: 0.7648 y: 0.9703 control1X: 0.8566 y: 0.9526 control2X: 0.8159 y: 0.9703;
		curveToX: 0.6413 y: 0.9171 control1X: 0.7127 y: 0.9703 control2X: 0.6716 y: 0.9526;
		curveToX: 0.5959 y: 0.7721 control1X: 0.6110 y: 0.8813 control2X: 0.5959 y: 0.8330;
		curveToX: 0.6413 y: 0.6271 control1X: 0.5959 y: 0.7109 control2X: 0.6110 y: 0.6626;
		curveToX: 0.7648 y: 0.5734 control1X: 0.6719 y: 0.5913 control2X: 0.7131 y: 0.5734;
		moveToX: 0.2609 y: 0.2760;
		curveToX: 0.1945 y: 0.3126 control1X: 0.2329 y: 0.2760 control2X: 0.2108 y: 0.2882;
		curveToX: 0.1706 y: 0.4122 control1X: 0.1786 y: 0.3367 control2X: 0.1706 y: 0.3699;
		curveToX: 0.1945 y: 0.5128 control1X: 0.1706 y: 0.4552 control2X: 0.1786 y: 0.4887;
		curveToX: 0.2609 y: 0.5490 control1X: 0.2105 y: 0.5369 control2X: 0.2326 y: 0.5490;
		curveToX: 0.3273 y: 0.5128 control1X: 0.2892 y: 0.5490 control2X: 0.3114 y: 0.5369;
		curveToX: 0.3517 y: 0.4122 control1X: 0.3436 y: 0.4887 control2X: 0.3517 y: 0.4552;
		curveToX: 0.3273 y: 0.3126 control1X: 0.3517 y: 0.3702 control2X: 0.3436 y: 0.3370;
		curveToX: 0.2609 y: 0.2760 control1X: 0.3110 y: 0.2882 control2X: 0.2889 y: 0.2760;
		moveToX: 0.7018 y: 0.2140;
		lineToX: 0.7800 y: 0.2140;
		lineToX: 0.3239 y: 0.9703;
		lineToX: 0.2458 y: 0.9703;
		lineToX: 0.7018 y: 0.2140;
		moveToX: 0.2609 y: 0.2140;
		curveToX: 0.3840 y: 0.2677 control1X: 0.3123 y: 0.2140 control2X: 0.3534 y: 0.2319;
		curveToX: 0.4299 y: 0.4122 control1X: 0.4146 y: 0.3032 control2X: 0.4299 y: 0.3514;
		curveToX: 0.3840 y: 0.5577 control1X: 0.4299 y: 0.4738 control2X: 0.4146 y: 0.5223;
		curveToX: 0.2609 y: 0.6110 control1X: 0.3537 y: 0.5932 control2X: 0.3127 y: 0.6110;
		curveToX: 0.1379 y: 0.5577 control1X: 0.2092 y: 0.6110 control2X: 0.1681 y: 0.5932;
		curveToX: 0.0929 y: 0.4122 control1X: 0.1079 y: 0.5219 control2X: 0.0929 y: 0.4734;
		curveToX: 0.1384 y: 0.2677 control1X: 0.0929 y: 0.3517 control2X: 0.1081 y: 0.3035;
		curveToX: 0.2609 y: 0.2140 control1X: 0.1686 y: 0.2319 control2X: 0.2095 y: 0.2140! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans38
	vectorGraphicsCanvas
		moveToX: 0.2809 y: 0.5641;
		curveToX: 0.2155 y: 0.6432 control1X: 0.2513 y: 0.5905 control2X: 0.2295 y: 0.6168;
		curveToX: 0.1950 y: 0.7252 control1X: 0.2018 y: 0.6692 control2X: 0.1950 y: 0.6966;
		curveToX: 0.2468 y: 0.8439 control1X: 0.1950 y: 0.7728 control2X: 0.2122 y: 0.8123;
		curveToX: 0.3766 y: 0.8912 control1X: 0.2813 y: 0.8755 control2X: 0.3246 y: 0.8912;
		curveToX: 0.4635 y: 0.8761 control1X: 0.4076 y: 0.8912 control2X: 0.4365 y: 0.8862;
		curveToX: 0.5397 y: 0.8292 control1X: 0.4906 y: 0.8657 control2X: 0.5160 y: 0.8501;
		lineToX: 0.2809 y: 0.5641;
		moveToX: 0.3498 y: 0.5094;
		lineToX: 0.5978 y: 0.7633;
		curveToX: 0.6427 y: 0.6705 control1X: 0.6170 y: 0.7343 control2X: 0.6320 y: 0.7034;
		curveToX: 0.6618 y: 0.5651 control1X: 0.6535 y: 0.6373 control2X: 0.6598 y: 0.6022;
		lineToX: 0.7526 y: 0.5651;
		curveToX: 0.7214 y: 0.6925 control1X: 0.7487 y: 0.6080 control2X: 0.7383 y: 0.6505;
		curveToX: 0.6506 y: 0.8170 control1X: 0.7044 y: 0.7345 control2X: 0.6808 y: 0.7760;
		lineToX: 0.7868 y: 0.9562;
		lineToX: 0.6637 y: 0.9562;
		lineToX: 0.5939 y: 0.8844;
		curveToX: 0.4875 y: 0.9493 control1X: 0.5601 y: 0.9134 control2X: 0.5246 y: 0.9350;
		curveToX: 0.3678 y: 0.9703 control1X: 0.4504 y: 0.9633 control2X: 0.4105 y: 0.9703;
		curveToX: 0.1755 y: 0.9034 control1X: 0.2894 y: 0.9703 control2X: 0.2253 y: 0.9480;
		curveToX: 0.1008 y: 0.7311 control1X: 0.1257 y: 0.8585 control2X: 0.1008 y: 0.8011;
		curveToX: 0.1335 y: 0.6139 control1X: 0.1008 y: 0.6894 control2X: 0.1117 y: 0.6504;
		curveToX: 0.2316 y: 0.5104 control1X: 0.1553 y: 0.5771 control2X: 0.1880 y: 0.5426;
		curveToX: 0.1960 y: 0.4493 control1X: 0.2160 y: 0.4899 control2X: 0.2041 y: 0.4695;
		curveToX: 0.1838 y: 0.3893 control1X: 0.1878 y: 0.4288 control2X: 0.1838 y: 0.4088;
		curveToX: 0.2380 y: 0.2623 control1X: 0.1838 y: 0.3366 control2X: 0.2018 y: 0.2942;
		curveToX: 0.3820 y: 0.2140 control1X: 0.2741 y: 0.2301 control2X: 0.3221 y: 0.2140;
		curveToX: 0.4626 y: 0.2228 control1X: 0.4090 y: 0.2140 control2X: 0.4359 y: 0.2169;
		curveToX: 0.5446 y: 0.2492 control1X: 0.4896 y: 0.2286 control2X: 0.5169 y: 0.2374;
		lineToX: 0.5446 y: 0.3380;
		curveToX: 0.4635 y: 0.3033 control1X: 0.5163 y: 0.3227 control2X: 0.4893 y: 0.3112;
		curveToX: 0.3918 y: 0.2911 control1X: 0.4378 y: 0.2952 control2X: 0.4139 y: 0.2911;
		curveToX: 0.3083 y: 0.3185 control1X: 0.3576 y: 0.2911 control2X: 0.3298 y: 0.3003;
		curveToX: 0.2765 y: 0.3883 control1X: 0.2871 y: 0.3364 control2X: 0.2765 y: 0.3597;
		curveToX: 0.2907 y: 0.4386 control1X: 0.2765 y: 0.4049 control2X: 0.2813 y: 0.4217;
		curveToX: 0.3498 y: 0.5094 control1X: 0.3005 y: 0.4552 control2X: 0.3202 y: 0.4788! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans39
	vectorGraphicsCanvas
		moveToX: 0.2170 y: 0.2272;
		lineToX: 0.2170 y: 0.4982;
		lineToX: 0.1340 y: 0.4982;
		lineToX: 0.1340 y: 0.2272;
		lineToX: 0.2170 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans40
	vectorGraphicsCanvas
		moveToX: 0.3478 y: 0.1974;
		curveToX: 0.2507 y: 0.4196 control1X: 0.3042 y: 0.2723 control2X: 0.2718 y: 0.3463;
		curveToX: 0.2189 y: 0.6422 control1X: 0.2295 y: 0.4928 control2X: 0.2189 y: 0.5670;
		curveToX: 0.2507 y: 0.8658 control1X: 0.2189 y: 0.7174 control2X: 0.2295 y: 0.7920;
		curveToX: 0.3478 y: 1.0880 control1X: 0.2721 y: 0.9394 control2X: 0.3045 y: 1.0135;
		lineToX: 0.2697 y: 1.0880;
		curveToX: 0.1598 y: 0.8624 control1X: 0.2209 y: 1.0115 control2X: 0.1843 y: 0.9363;
		curveToX: 0.1237 y: 0.6422 control1X: 0.1357 y: 0.7885 control2X: 0.1237 y: 0.7151;
		curveToX: 0.1598 y: 0.4230 control1X: 0.1237 y: 0.5696 control2X: 0.1357 y: 0.4965;
		curveToX: 0.2697 y: 0.1974 control1X: 0.1839 y: 0.3494 control2X: 0.2205 y: 0.2742;
		lineToX: 0.3478 y: 0.1974! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans41
	vectorGraphicsCanvas
		moveToX: 0.1178 y: 0.1974;
		lineToX: 0.1960 y: 0.1974;
		curveToX: 0.3053 y: 0.4230 control1X: 0.2448 y: 0.2742 control2X: 0.2813 y: 0.3494;
		curveToX: 0.3420 y: 0.6422 control1X: 0.3298 y: 0.4965 control2X: 0.3420 y: 0.5696;
		curveToX: 0.3053 y: 0.8624 control1X: 0.3420 y: 0.7151 control2X: 0.3298 y: 0.7885;
		curveToX: 0.1960 y: 1.0880 control1X: 0.2813 y: 0.9363 control2X: 0.2448 y: 1.0115;
		lineToX: 0.1178 y: 1.0880;
		curveToX: 0.2145 y: 0.8658 control1X: 0.1611 y: 1.0135 control2X: 0.1934 y: 0.9394;
		curveToX: 0.2468 y: 0.6422 control1X: 0.2360 y: 0.7920 control2X: 0.2468 y: 0.7174;
		curveToX: 0.2145 y: 0.4196 control1X: 0.2468 y: 0.5670 control2X: 0.2360 y: 0.4928;
		curveToX: 0.1178 y: 0.1974 control1X: 0.1934 y: 0.3463 control2X: 0.1611 y: 0.2723! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans42
	vectorGraphicsCanvas
		moveToX: 0.5080 y: 0.3473;
		lineToX: 0.3327 y: 0.4420;
		lineToX: 0.5080 y: 0.5372;
		lineToX: 0.4797 y: 0.5851;
		lineToX: 0.3156 y: 0.4860;
		lineToX: 0.3156 y: 0.6700;
		lineToX: 0.2599 y: 0.6700;
		lineToX: 0.2599 y: 0.4860;
		lineToX: 0.0959 y: 0.5851;
		lineToX: 0.0676 y: 0.5372;
		lineToX: 0.2428 y: 0.4420;
		lineToX: 0.0676 y: 0.3473;
		lineToX: 0.0959 y: 0.2990;
		lineToX: 0.2599 y: 0.3981;
		lineToX: 0.2599 y: 0.2140;
		lineToX: 0.3156 y: 0.2140;
		lineToX: 0.3156 y: 0.3981;
		lineToX: 0.4797 y: 0.2990;
		lineToX: 0.5080 y: 0.3473! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans43
	vectorGraphicsCanvas
		moveToX: 0.4977 y: 0.3292;
		lineToX: 0.4977 y: 0.6012;
		lineToX: 0.7697 y: 0.6012;
		lineToX: 0.7697 y: 0.6842;
		lineToX: 0.4977 y: 0.6842;
		lineToX: 0.4977 y: 0.9562;
		lineToX: 0.4157 y: 0.9562;
		lineToX: 0.4157 y: 0.6842;
		lineToX: 0.1437 y: 0.6842;
		lineToX: 0.1437 y: 0.6012;
		lineToX: 0.4157 y: 0.6012;
		lineToX: 0.4157 y: 0.3292;
		lineToX: 0.4977 y: 0.3292! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans44
	vectorGraphicsCanvas
		moveToX: 0.1550 y: 0.8322;
		lineToX: 0.2580 y: 0.8322;
		lineToX: 0.2580 y: 0.9161;
		lineToX: 0.1779 y: 1.0724;
		lineToX: 0.1149 y: 1.0724;
		lineToX: 0.1550 y: 0.9161;
		lineToX: 0.1550 y: 0.8322! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans45
	vectorGraphicsCanvas
		moveToX: 0.0866 y: 0.6422;
		lineToX: 0.3498 y: 0.6422;
		lineToX: 0.3498 y: 0.7223;
		lineToX: 0.0866 y: 0.7223;
		lineToX: 0.0866 y: 0.6422! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans46
	vectorGraphicsCanvas
		moveToX: 0.1447 y: 0.8322;
		lineToX: 0.2477 y: 0.8322;
		lineToX: 0.2477 y: 0.9562;
		lineToX: 0.1447 y: 0.9562;
		lineToX: 0.1447 y: 0.8322! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans47
	vectorGraphicsCanvas
		moveToX: 0.2917 y: 0.2272;
		lineToX: 0.3747 y: 0.2272;
		lineToX: 0.1208 y: 1.0490;
		lineToX: 0.0378 y: 1.0490;
		lineToX: 0.2917 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans48
	vectorGraphicsCanvas
		moveToX: 0.3556 y: 0.2921;
		curveToX: 0.2409 y: 0.3673 control1X: 0.3049 y: 0.2921 control2X: 0.2666 y: 0.3172;
		curveToX: 0.2028 y: 0.5924 control1X: 0.2155 y: 0.4171 control2X: 0.2028 y: 0.4922;
		curveToX: 0.2409 y: 0.8175 control1X: 0.2028 y: 0.6923 control2X: 0.2155 y: 0.7674;
		curveToX: 0.3556 y: 0.8922 control1X: 0.2666 y: 0.8673 control2X: 0.3049 y: 0.8922;
		curveToX: 0.4704 y: 0.8175 control1X: 0.4067 y: 0.8922 control2X: 0.4450 y: 0.8673;
		curveToX: 0.5090 y: 0.5924 control1X: 0.4961 y: 0.7674 control2X: 0.5090 y: 0.6923;
		curveToX: 0.4704 y: 0.3673 control1X: 0.5090 y: 0.4922 control2X: 0.4961 y: 0.4171;
		curveToX: 0.3556 y: 0.2921 control1X: 0.4450 y: 0.3172 control2X: 0.4067 y: 0.2921;
		moveToX: 0.3556 y: 0.2140;
		curveToX: 0.5426 y: 0.3112 control1X: 0.4373 y: 0.2140 control2X: 0.4997 y: 0.2464;
		curveToX: 0.6076 y: 0.5924 control1X: 0.5859 y: 0.3756 control2X: 0.6076 y: 0.4694;
		curveToX: 0.5426 y: 0.8737 control1X: 0.6076 y: 0.7151 control2X: 0.5859 y: 0.8089;
		curveToX: 0.3556 y: 0.9703 control1X: 0.4997 y: 0.9381 control2X: 0.4373 y: 0.9703;
		curveToX: 0.1681 y: 0.8737 control1X: 0.2739 y: 0.9703 control2X: 0.2114 y: 0.9381;
		curveToX: 0.1037 y: 0.5924 control1X: 0.1252 y: 0.8089 control2X: 0.1037 y: 0.7151;
		curveToX: 0.1681 y: 0.3112 control1X: 0.1037 y: 0.4694 control2X: 0.1252 y: 0.3756;
		curveToX: 0.3556 y: 0.2140 control1X: 0.2114 y: 0.2464 control2X: 0.2739 y: 0.2140! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans49
	vectorGraphicsCanvas
		moveToX: 0.1618 y: 0.8732;
		lineToX: 0.3229 y: 0.8732;
		lineToX: 0.3229 y: 0.3170;
		lineToX: 0.1476 y: 0.3522;
		lineToX: 0.1476 y: 0.2623;
		lineToX: 0.3219 y: 0.2272;
		lineToX: 0.4206 y: 0.2272;
		lineToX: 0.4206 y: 0.8732;
		lineToX: 0.5817 y: 0.8732;
		lineToX: 0.5817 y: 0.9562;
		lineToX: 0.1618 y: 0.9562;
		lineToX: 0.1618 y: 0.8732! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans50
	vectorGraphicsCanvas
		moveToX: 0.2297 y: 0.8732;
		lineToX: 0.5739 y: 0.8732;
		lineToX: 0.5739 y: 0.9562;
		lineToX: 0.1110 y: 0.9562;
		lineToX: 0.1110 y: 0.8732;
		curveToX: 0.2638 y: 0.7174 control1X: 0.1484 y: 0.8344 control2X: 0.1994 y: 0.7825;
		curveToX: 0.3859 y: 0.5909 control1X: 0.3286 y: 0.6520 control2X: 0.3693 y: 0.6098;
		curveToX: 0.4518 y: 0.5011 control1X: 0.4175 y: 0.5555 control2X: 0.4395 y: 0.5255;
		curveToX: 0.4709 y: 0.4283 control1X: 0.4645 y: 0.4764 control2X: 0.4709 y: 0.4521;
		curveToX: 0.4299 y: 0.3336 control1X: 0.4709 y: 0.3896 control2X: 0.4572 y: 0.3580;
		curveToX: 0.3239 y: 0.2970 control1X: 0.4028 y: 0.3092 control2X: 0.3675 y: 0.2970;
		curveToX: 0.2258 y: 0.3131 control1X: 0.2930 y: 0.2970 control2X: 0.2603 y: 0.3024;
		curveToX: 0.1159 y: 0.3619 control1X: 0.1916 y: 0.3239 control2X: 0.1550 y: 0.3401;
		lineToX: 0.1159 y: 0.2623;
		curveToX: 0.2272 y: 0.2262 control1X: 0.1556 y: 0.2464 control2X: 0.1927 y: 0.2343;
		curveToX: 0.3219 y: 0.2140 control1X: 0.2617 y: 0.2181 control2X: 0.2933 y: 0.2140;
		curveToX: 0.5026 y: 0.2706 control1X: 0.3975 y: 0.2140 control2X: 0.4577 y: 0.2329;
		curveToX: 0.5700 y: 0.4220 control1X: 0.5475 y: 0.3084 control2X: 0.5700 y: 0.3589;
		curveToX: 0.5529 y: 0.5075 control1X: 0.5700 y: 0.4520 control2X: 0.5643 y: 0.4804;
		curveToX: 0.4919 y: 0.6022 control1X: 0.5418 y: 0.5341 control2X: 0.5215 y: 0.5657;
		curveToX: 0.4142 y: 0.6842 control1X: 0.4837 y: 0.6116 control2X: 0.4579 y: 0.6390;
		curveToX: 0.2297 y: 0.8732 control1X: 0.3706 y: 0.7291 control2X: 0.3091 y: 0.7921! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans51
	vectorGraphicsCanvas
		moveToX: 0.4435 y: 0.5631;
		curveToX: 0.5539 y: 0.6261 control1X: 0.4907 y: 0.5732 control2X: 0.5275 y: 0.5942;
		curveToX: 0.5939 y: 0.7443 control1X: 0.5806 y: 0.6580 control2X: 0.5939 y: 0.6974;
		curveToX: 0.5197 y: 0.9113 control1X: 0.5939 y: 0.8162 control2X: 0.5692 y: 0.8719;
		curveToX: 0.3088 y: 0.9703 control1X: 0.4702 y: 0.9506 control2X: 0.3999 y: 0.9703;
		curveToX: 0.2140 y: 0.9611 control1X: 0.2782 y: 0.9703 control2X: 0.2466 y: 0.9672;
		curveToX: 0.1139 y: 0.9342 control1X: 0.1818 y: 0.9552 control2X: 0.1484 y: 0.9463;
		lineToX: 0.1139 y: 0.8390;
		curveToX: 0.2038 y: 0.8751 control1X: 0.1413 y: 0.8549 control2X: 0.1712 y: 0.8670;
		curveToX: 0.3058 y: 0.8873 control1X: 0.2363 y: 0.8833 control2X: 0.2704 y: 0.8873;
		curveToX: 0.4469 y: 0.8507 control1X: 0.3677 y: 0.8873 control2X: 0.4147 y: 0.8751;
		curveToX: 0.4958 y: 0.7443 control1X: 0.4795 y: 0.8263 control2X: 0.4958 y: 0.7908;
		curveToX: 0.4504 y: 0.6437 control1X: 0.4958 y: 0.7013 control2X: 0.4806 y: 0.6678;
		curveToX: 0.3249 y: 0.6071 control1X: 0.4204 y: 0.6193 control2X: 0.3786 y: 0.6071;
		lineToX: 0.2399 y: 0.6071;
		lineToX: 0.2399 y: 0.5260;
		lineToX: 0.3288 y: 0.5260;
		curveToX: 0.4401 y: 0.4972 control1X: 0.3773 y: 0.5260 control2X: 0.4144 y: 0.5164;
		curveToX: 0.4787 y: 0.4132 control1X: 0.4658 y: 0.4777 control2X: 0.4787 y: 0.4497;
		curveToX: 0.4386 y: 0.3273 control1X: 0.4787 y: 0.3758 control2X: 0.4653 y: 0.3471;
		curveToX: 0.3249 y: 0.2970 control1X: 0.4123 y: 0.3071 control2X: 0.3744 y: 0.2970;
		curveToX: 0.2380 y: 0.3058 control1X: 0.2979 y: 0.2970 control2X: 0.2689 y: 0.2999;
		curveToX: 0.1359 y: 0.3331 control1X: 0.2070 y: 0.3117 control2X: 0.1730 y: 0.3208;
		lineToX: 0.1359 y: 0.2452;
		curveToX: 0.2409 y: 0.2218 control1X: 0.1733 y: 0.2348 control2X: 0.2083 y: 0.2270;
		curveToX: 0.3337 y: 0.2140 control1X: 0.2738 y: 0.2166 control2X: 0.3047 y: 0.2140;
		curveToX: 0.5114 y: 0.2653 control1X: 0.4085 y: 0.2140 control2X: 0.4678 y: 0.2311;
		curveToX: 0.5768 y: 0.4030 control1X: 0.5550 y: 0.2991 control2X: 0.5768 y: 0.3450;
		curveToX: 0.5422 y: 0.5055 control1X: 0.5768 y: 0.4433 control2X: 0.5653 y: 0.4775;
		curveToX: 0.4435 y: 0.5631 control1X: 0.5190 y: 0.5332 control2X: 0.4862 y: 0.5524! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans52
	vectorGraphicsCanvas
		moveToX: 0.4157 y: 0.3131;
		lineToX: 0.1667 y: 0.7023;
		lineToX: 0.4157 y: 0.7023;
		lineToX: 0.4157 y: 0.3131;
		moveToX: 0.3898 y: 0.2272;
		lineToX: 0.5138 y: 0.2272;
		lineToX: 0.5138 y: 0.7023;
		lineToX: 0.6178 y: 0.7023;
		lineToX: 0.6178 y: 0.7843;
		lineToX: 0.5138 y: 0.7843;
		lineToX: 0.5138 y: 0.9562;
		lineToX: 0.4157 y: 0.9562;
		lineToX: 0.4157 y: 0.7843;
		lineToX: 0.0866 y: 0.7843;
		lineToX: 0.0866 y: 0.6891;
		lineToX: 0.3898 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans53
	vectorGraphicsCanvas
		moveToX: 0.1457 y: 0.2272;
		lineToX: 0.5329 y: 0.2272;
		lineToX: 0.5329 y: 0.3102;
		lineToX: 0.2360 y: 0.3102;
		lineToX: 0.2360 y: 0.4889;
		curveToX: 0.2790 y: 0.4782 control1X: 0.2503 y: 0.4840 control2X: 0.2647 y: 0.4804;
		curveToX: 0.3219 y: 0.4742 control1X: 0.2933 y: 0.4756 control2X: 0.3076 y: 0.4742;
		curveToX: 0.5153 y: 0.5411 control1X: 0.4033 y: 0.4742 control2X: 0.4678 y: 0.4965;
		curveToX: 0.5866 y: 0.7223 control1X: 0.5628 y: 0.5857 control2X: 0.5866 y: 0.6461;
		curveToX: 0.5134 y: 0.9054 control1X: 0.5866 y: 0.8007 control2X: 0.5622 y: 0.8618;
		curveToX: 0.3068 y: 0.9703 control1X: 0.4645 y: 0.9487 control2X: 0.3957 y: 0.9703;
		curveToX: 0.2131 y: 0.9625 control1X: 0.2762 y: 0.9703 control2X: 0.2450 y: 0.9677;
		curveToX: 0.1149 y: 0.9391 control1X: 0.1815 y: 0.9573 control2X: 0.1488 y: 0.9495;
		lineToX: 0.1149 y: 0.8400;
		curveToX: 0.2057 y: 0.8756 control1X: 0.1442 y: 0.8559 control2X: 0.1745 y: 0.8678;
		curveToX: 0.3049 y: 0.8873 control1X: 0.2370 y: 0.8834 control2X: 0.2700 y: 0.8873;
		curveToX: 0.4386 y: 0.8429 control1X: 0.3612 y: 0.8873 control2X: 0.4058 y: 0.8725;
		curveToX: 0.4880 y: 0.7223 control1X: 0.4715 y: 0.8133 control2X: 0.4880 y: 0.7731;
		curveToX: 0.4386 y: 0.6017 control1X: 0.4880 y: 0.6715 control2X: 0.4715 y: 0.6313;
		curveToX: 0.3049 y: 0.5573 control1X: 0.4058 y: 0.5721 control2X: 0.3612 y: 0.5573;
		curveToX: 0.2258 y: 0.5660 control1X: 0.2785 y: 0.5573 control2X: 0.2521 y: 0.5602;
		curveToX: 0.1457 y: 0.5934 control1X: 0.1997 y: 0.5719 control2X: 0.1730 y: 0.5810;
		lineToX: 0.1457 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans54
	vectorGraphicsCanvas
		moveToX: 0.3678 y: 0.5524;
		curveToX: 0.2624 y: 0.5978 control1X: 0.3236 y: 0.5524 control2X: 0.2884 y: 0.5675;
		curveToX: 0.2238 y: 0.7223 control1X: 0.2367 y: 0.6281 control2X: 0.2238 y: 0.6696;
		curveToX: 0.2624 y: 0.8468 control1X: 0.2238 y: 0.7747 control2X: 0.2367 y: 0.8162;
		curveToX: 0.3678 y: 0.8922 control1X: 0.2884 y: 0.8771 control2X: 0.3236 y: 0.8922;
		curveToX: 0.4728 y: 0.8468 control1X: 0.4121 y: 0.8922 control2X: 0.4471 y: 0.8771;
		curveToX: 0.5119 y: 0.7223 control1X: 0.4989 y: 0.8162 control2X: 0.5119 y: 0.7747;
		curveToX: 0.4728 y: 0.5978 control1X: 0.5119 y: 0.6696 control2X: 0.4989 y: 0.6281;
		curveToX: 0.3678 y: 0.5524 control1X: 0.4471 y: 0.5675 control2X: 0.4121 y: 0.5524;
		moveToX: 0.5636 y: 0.2433;
		lineToX: 0.5636 y: 0.3331;
		curveToX: 0.4885 y: 0.3063 control1X: 0.5389 y: 0.3214 control2X: 0.5138 y: 0.3125;
		curveToX: 0.4137 y: 0.2970 control1X: 0.4634 y: 0.3001 control2X: 0.4385 y: 0.2970;
		curveToX: 0.2643 y: 0.3629 control1X: 0.3486 y: 0.2970 control2X: 0.2988 y: 0.3190;
		curveToX: 0.2057 y: 0.5621 control1X: 0.2301 y: 0.4069 control2X: 0.2106 y: 0.4733;
		curveToX: 0.2780 y: 0.4972 control1X: 0.2249 y: 0.5338 control2X: 0.2490 y: 0.5122;
		curveToX: 0.3737 y: 0.4742 control1X: 0.3070 y: 0.4819 control2X: 0.3389 y: 0.4742;
		curveToX: 0.5470 y: 0.5411 control1X: 0.4469 y: 0.4742 control2X: 0.5047 y: 0.4965;
		curveToX: 0.6110 y: 0.7223 control1X: 0.5897 y: 0.5854 control2X: 0.6110 y: 0.6458;
		curveToX: 0.5446 y: 0.9025 control1X: 0.6110 y: 0.7972 control2X: 0.5889 y: 0.8572;
		curveToX: 0.3678 y: 0.9703 control1X: 0.5003 y: 0.9477 control2X: 0.4414 y: 0.9703;
		curveToX: 0.1745 y: 0.8737 control1X: 0.2835 y: 0.9703 control2X: 0.2191 y: 0.9381;
		curveToX: 0.1076 y: 0.5924 control1X: 0.1299 y: 0.8089 control2X: 0.1076 y: 0.7151;
		curveToX: 0.1896 y: 0.3170 control1X: 0.1076 y: 0.4772 control2X: 0.1349 y: 0.3854;
		curveToX: 0.4098 y: 0.2140 control1X: 0.2443 y: 0.2483 control2X: 0.3177 y: 0.2140;
		curveToX: 0.4845 y: 0.2213 control1X: 0.4346 y: 0.2140 control2X: 0.4595 y: 0.2164;
		curveToX: 0.5636 y: 0.2433 control1X: 0.5099 y: 0.2262 control2X: 0.5363 y: 0.2335! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans55
	vectorGraphicsCanvas
		moveToX: 0.1198 y: 0.2272;
		lineToX: 0.5885 y: 0.2272;
		lineToX: 0.5885 y: 0.2692;
		lineToX: 0.3239 y: 0.9562;
		lineToX: 0.2209 y: 0.9562;
		lineToX: 0.4699 y: 0.3102;
		lineToX: 0.1198 y: 0.3102;
		lineToX: 0.1198 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans56
	vectorGraphicsCanvas
		moveToX: 0.3556 y: 0.6100;
		curveToX: 0.2448 y: 0.6476 control1X: 0.3088 y: 0.6100 control2X: 0.2718 y: 0.6225;
		curveToX: 0.2048 y: 0.7511 control1X: 0.2181 y: 0.6727 control2X: 0.2048 y: 0.7072;
		curveToX: 0.2448 y: 0.8546 control1X: 0.2048 y: 0.7950 control2X: 0.2181 y: 0.8296;
		curveToX: 0.3556 y: 0.8922 control1X: 0.2718 y: 0.8797 control2X: 0.3088 y: 0.8922;
		curveToX: 0.4665 y: 0.8546 control1X: 0.4025 y: 0.8922 control2X: 0.4395 y: 0.8797;
		curveToX: 0.5070 y: 0.7511 control1X: 0.4935 y: 0.8292 control2X: 0.5070 y: 0.7947;
		curveToX: 0.4665 y: 0.6476 control1X: 0.5070 y: 0.7072 control2X: 0.4935 y: 0.6727;
		curveToX: 0.3556 y: 0.6100 control1X: 0.4398 y: 0.6225 control2X: 0.4028 y: 0.6100;
		moveToX: 0.2570 y: 0.5680;
		curveToX: 0.1579 y: 0.5089 control1X: 0.2147 y: 0.5576 control2X: 0.1816 y: 0.5379;
		curveToX: 0.1227 y: 0.4030 control1X: 0.1344 y: 0.4799 control2X: 0.1227 y: 0.4446;
		curveToX: 0.1847 y: 0.2648 control1X: 0.1227 y: 0.3447 control2X: 0.1434 y: 0.2986;
		curveToX: 0.3556 y: 0.2140 control1X: 0.2264 y: 0.2309 control2X: 0.2834 y: 0.2140;
		curveToX: 0.5265 y: 0.2648 control1X: 0.4282 y: 0.2140 control2X: 0.4852 y: 0.2309;
		curveToX: 0.5885 y: 0.4030 control1X: 0.5679 y: 0.2986 control2X: 0.5885 y: 0.3447;
		curveToX: 0.5529 y: 0.5089 control1X: 0.5885 y: 0.4446 control2X: 0.5767 y: 0.4799;
		curveToX: 0.4548 y: 0.5680 control1X: 0.5295 y: 0.5379 control2X: 0.4968 y: 0.5576;
		curveToX: 0.5656 y: 0.6329 control1X: 0.5023 y: 0.5791 control2X: 0.5392 y: 0.6007;
		curveToX: 0.6056 y: 0.7511 control1X: 0.5923 y: 0.6652 control2X: 0.6056 y: 0.7046;
		curveToX: 0.5407 y: 0.9137 control1X: 0.6056 y: 0.8217 control2X: 0.5840 y: 0.8759;
		curveToX: 0.3556 y: 0.9703 control1X: 0.4977 y: 0.9515 control2X: 0.4360 y: 0.9703;
		curveToX: 0.1701 y: 0.9137 control1X: 0.2752 y: 0.9703 control2X: 0.2134 y: 0.9515;
		curveToX: 0.1056 y: 0.7511 control1X: 0.1271 y: 0.8759 control2X: 0.1056 y: 0.8217;
		curveToX: 0.1457 y: 0.6329 control1X: 0.1056 y: 0.7046 control2X: 0.1190 y: 0.6652;
		curveToX: 0.2570 y: 0.5680 control1X: 0.1724 y: 0.6007 control2X: 0.2095 y: 0.5791;
		moveToX: 0.2209 y: 0.4122;
		curveToX: 0.2560 y: 0.5006 control1X: 0.2209 y: 0.4500 control2X: 0.2326 y: 0.4795;
		curveToX: 0.3556 y: 0.5324 control1X: 0.2798 y: 0.5218 control2X: 0.3130 y: 0.5324;
		curveToX: 0.4548 y: 0.5006 control1X: 0.3980 y: 0.5324 control2X: 0.4310 y: 0.5218;
		curveToX: 0.4909 y: 0.4122 control1X: 0.4788 y: 0.4795 control2X: 0.4909 y: 0.4500;
		curveToX: 0.4548 y: 0.3239 control1X: 0.4909 y: 0.3745 control2X: 0.4788 y: 0.3450;
		curveToX: 0.3556 y: 0.2921 control1X: 0.4310 y: 0.3027 control2X: 0.3980 y: 0.2921;
		curveToX: 0.2560 y: 0.3239 control1X: 0.3130 y: 0.2921 control2X: 0.2798 y: 0.3027;
		curveToX: 0.2209 y: 0.4122 control1X: 0.2326 y: 0.3450 control2X: 0.2209 y: 0.3745! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans57
	vectorGraphicsCanvas
		moveToX: 0.1476 y: 0.9410;
		lineToX: 0.1476 y: 0.8512;
		curveToX: 0.2228 y: 0.8781 control1X: 0.1724 y: 0.8629 control2X: 0.1974 y: 0.8719;
		curveToX: 0.2975 y: 0.8873 control1X: 0.2482 y: 0.8842 control2X: 0.2731 y: 0.8873;
		curveToX: 0.4465 y: 0.8219 control1X: 0.3626 y: 0.8873 control2X: 0.4123 y: 0.8655;
		curveToX: 0.5055 y: 0.6222 control1X: 0.4810 y: 0.7780 control2X: 0.5007 y: 0.7114;
		curveToX: 0.4338 y: 0.6867 control1X: 0.4867 y: 0.6502 control2X: 0.4627 y: 0.6717;
		curveToX: 0.3376 y: 0.7091 control1X: 0.4048 y: 0.7016 control2X: 0.3727 y: 0.7091;
		curveToX: 0.1642 y: 0.6432 control1X: 0.2647 y: 0.7091 control2X: 0.2069 y: 0.6871;
		curveToX: 0.1008 y: 0.4620 control1X: 0.1219 y: 0.5989 control2X: 0.1008 y: 0.5385;
		curveToX: 0.1672 y: 0.2819 control1X: 0.1008 y: 0.3872 control2X: 0.1229 y: 0.3271;
		curveToX: 0.3439 y: 0.2140 control1X: 0.2114 y: 0.2366 control2X: 0.2704 y: 0.2140;
		curveToX: 0.5368 y: 0.3112 control1X: 0.4282 y: 0.2140 control2X: 0.4925 y: 0.2464;
		curveToX: 0.6037 y: 0.5924 control1X: 0.5814 y: 0.3756 control2X: 0.6037 y: 0.4694;
		curveToX: 0.5217 y: 0.8678 control1X: 0.6037 y: 0.7073 control2X: 0.5763 y: 0.7991;
		curveToX: 0.3019 y: 0.9703 control1X: 0.4673 y: 0.9362 control2X: 0.3940 y: 0.9703;
		curveToX: 0.2267 y: 0.9630 control1X: 0.2772 y: 0.9703 control2X: 0.2521 y: 0.9679;
		curveToX: 0.1476 y: 0.9410 control1X: 0.2013 y: 0.9581 control2X: 0.1750 y: 0.9508;
		moveToX: 0.3439 y: 0.6320;
		curveToX: 0.4489 y: 0.5866 control1X: 0.3882 y: 0.6320 control2X: 0.4232 y: 0.6168;
		curveToX: 0.4880 y: 0.4620 control1X: 0.4749 y: 0.5563 control2X: 0.4880 y: 0.5148;
		curveToX: 0.4489 y: 0.3380 control1X: 0.4880 y: 0.4096 control2X: 0.4749 y: 0.3683;
		curveToX: 0.3439 y: 0.2921 control1X: 0.4232 y: 0.3074 control2X: 0.3882 y: 0.2921;
		curveToX: 0.2385 y: 0.3380 control1X: 0.2996 y: 0.2921 control2X: 0.2645 y: 0.3074;
		curveToX: 0.1999 y: 0.4620 control1X: 0.2127 y: 0.3683 control2X: 0.1999 y: 0.4096;
		curveToX: 0.2385 y: 0.5866 control1X: 0.1999 y: 0.5148 control2X: 0.2127 y: 0.5563;
		curveToX: 0.3439 y: 0.6320 control1X: 0.2645 y: 0.6168 control2X: 0.2996 y: 0.6320! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans58
	vectorGraphicsCanvas
		moveToX: 0.1550 y: 0.8322;
		lineToX: 0.2580 y: 0.8322;
		lineToX: 0.2580 y: 0.9562;
		lineToX: 0.1550 y: 0.9562;
		lineToX: 0.1550 y: 0.8322;
		moveToX: 0.1550 y: 0.4391;
		lineToX: 0.2580 y: 0.4391;
		lineToX: 0.2580 y: 0.5631;
		lineToX: 0.1550 y: 0.5631;
		lineToX: 0.1550 y: 0.4391! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans59
	vectorGraphicsCanvas
		moveToX: 0.1550 y: 0.4391;
		lineToX: 0.2580 y: 0.4391;
		lineToX: 0.2580 y: 0.5631;
		lineToX: 0.1550 y: 0.5631;
		lineToX: 0.1550 y: 0.4391;
		moveToX: 0.1550 y: 0.8322;
		lineToX: 0.2580 y: 0.8322;
		lineToX: 0.2580 y: 0.9161;
		lineToX: 0.1779 y: 1.0724;
		lineToX: 0.1149 y: 1.0724;
		lineToX: 0.1550 y: 0.9161;
		lineToX: 0.1550 y: 0.8322! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans60
	vectorGraphicsCanvas
		moveToX: 0.7697 y: 0.4640;
		lineToX: 0.2658 y: 0.6432;
		lineToX: 0.7697 y: 0.8214;
		lineToX: 0.7697 y: 0.9103;
		lineToX: 0.1437 y: 0.6832;
		lineToX: 0.1437 y: 0.6022;
		lineToX: 0.7697 y: 0.3751;
		lineToX: 0.7697 y: 0.4640! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans61
	vectorGraphicsCanvas
		moveToX: 0.1437 y: 0.5021;
		lineToX: 0.7697 y: 0.5021;
		lineToX: 0.7697 y: 0.5841;
		lineToX: 0.1437 y: 0.5841;
		lineToX: 0.1437 y: 0.5021;
		moveToX: 0.1437 y: 0.7013;
		lineToX: 0.7697 y: 0.7013;
		lineToX: 0.7697 y: 0.7843;
		lineToX: 0.1437 y: 0.7843;
		lineToX: 0.1437 y: 0.7013! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans62
	vectorGraphicsCanvas
		moveToX: 0.1437 y: 0.4640;
		lineToX: 0.1437 y: 0.3751;
		lineToX: 0.7697 y: 0.6022;
		lineToX: 0.7697 y: 0.6832;
		lineToX: 0.1437 y: 0.9103;
		lineToX: 0.1437 y: 0.8214;
		lineToX: 0.6467 y: 0.6432;
		lineToX: 0.1437 y: 0.4640! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans63
	vectorGraphicsCanvas
		moveToX: 0.2287 y: 0.8322;
		lineToX: 0.3278 y: 0.8322;
		lineToX: 0.3278 y: 0.9562;
		lineToX: 0.2287 y: 0.9562;
		lineToX: 0.2287 y: 0.8322;
		moveToX: 0.3249 y: 0.7604;
		lineToX: 0.2316 y: 0.7604;
		lineToX: 0.2316 y: 0.6852;
		curveToX: 0.2453 y: 0.6041 control1X: 0.2316 y: 0.6523 control2X: 0.2362 y: 0.6253;
		curveToX: 0.3029 y: 0.5304 control1X: 0.2544 y: 0.5830 control2X: 0.2736 y: 0.5584;
		lineToX: 0.3468 y: 0.4869;
		curveToX: 0.3869 y: 0.4381 control1X: 0.3654 y: 0.4697 control2X: 0.3787 y: 0.4534;
		curveToX: 0.3996 y: 0.3912 control1X: 0.3954 y: 0.4228 control2X: 0.3996 y: 0.4072;
		curveToX: 0.3674 y: 0.3209 control1X: 0.3996 y: 0.3623 control2X: 0.3888 y: 0.3388;
		curveToX: 0.2829 y: 0.2941 control1X: 0.3462 y: 0.3030 control2X: 0.3180 y: 0.2941;
		curveToX: 0.2004 y: 0.3112 control1X: 0.2572 y: 0.2941 control2X: 0.2297 y: 0.2998;
		curveToX: 0.1095 y: 0.3610 control1X: 0.1714 y: 0.3226 control2X: 0.1411 y: 0.3392;
		lineToX: 0.1095 y: 0.2692;
		curveToX: 0.2023 y: 0.2277 control1X: 0.1401 y: 0.2506 control2X: 0.1711 y: 0.2368;
		curveToX: 0.3000 y: 0.2140 control1X: 0.2339 y: 0.2186 control2X: 0.2664 y: 0.2140;
		curveToX: 0.4440 y: 0.2614 control1X: 0.3599 y: 0.2140 control2X: 0.4079 y: 0.2298;
		curveToX: 0.4987 y: 0.3864 control1X: 0.4805 y: 0.2929 control2X: 0.4987 y: 0.3346;
		curveToX: 0.4811 y: 0.4572 control1X: 0.4987 y: 0.4111 control2X: 0.4928 y: 0.4347;
		curveToX: 0.4196 y: 0.5324 control1X: 0.4694 y: 0.4793 control2X: 0.4489 y: 0.5044;
		lineToX: 0.3766 y: 0.5743;
		curveToX: 0.3439 y: 0.6105 control1X: 0.3613 y: 0.5896 control2X: 0.3504 y: 0.6017;
		curveToX: 0.3307 y: 0.6354 control1X: 0.3377 y: 0.6189 control2X: 0.3333 y: 0.6272;
		curveToX: 0.3263 y: 0.6603 control1X: 0.3288 y: 0.6422 control2X: 0.3273 y: 0.6505;
		curveToX: 0.3249 y: 0.7003 control1X: 0.3254 y: 0.6700 control2X: 0.3249 y: 0.6834;
		lineToX: 0.3249 y: 0.7604! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans64
	vectorGraphicsCanvas
		moveToX: 0.4098 y: 0.6940;
		curveToX: 0.4445 y: 0.8038 control1X: 0.4098 y: 0.7405 control2X: 0.4214 y: 0.7771;
		curveToX: 0.5397 y: 0.8434 control1X: 0.4676 y: 0.8302 control2X: 0.4994 y: 0.8434;
		curveToX: 0.6340 y: 0.8033 control1X: 0.5798 y: 0.8434 control2X: 0.6112 y: 0.8300;
		curveToX: 0.6686 y: 0.6940 control1X: 0.6571 y: 0.7767 control2X: 0.6686 y: 0.7402;
		curveToX: 0.6335 y: 0.5856 control1X: 0.6686 y: 0.6484 control2X: 0.6569 y: 0.6123;
		curveToX: 0.5387 y: 0.5450 control1X: 0.6100 y: 0.5586 control2X: 0.5785 y: 0.5450;
		curveToX: 0.4445 y: 0.5851 control1X: 0.4994 y: 0.5450 control2X: 0.4679 y: 0.5584;
		curveToX: 0.4098 y: 0.6940 control1X: 0.4214 y: 0.6118 control2X: 0.4098 y: 0.6481;
		moveToX: 0.6760 y: 0.8400;
		curveToX: 0.6086 y: 0.8956 control1X: 0.6564 y: 0.8650 control2X: 0.6340 y: 0.8836;
		curveToX: 0.5207 y: 0.9132 control1X: 0.5835 y: 0.9074 control2X: 0.5542 y: 0.9132;
		curveToX: 0.3840 y: 0.8527 control1X: 0.4647 y: 0.9132 control2X: 0.4191 y: 0.8930;
		curveToX: 0.3317 y: 0.6940 control1X: 0.3491 y: 0.8120 control2X: 0.3317 y: 0.7591;
		curveToX: 0.3844 y: 0.5353 control1X: 0.3317 y: 0.6289 control2X: 0.3493 y: 0.5760;
		curveToX: 0.5207 y: 0.4742 control1X: 0.4196 y: 0.4946 control2X: 0.4650 y: 0.4742;
		curveToX: 0.6091 y: 0.4928 control1X: 0.5542 y: 0.4742 control2X: 0.5837 y: 0.4804;
		curveToX: 0.6760 y: 0.5480 control1X: 0.6344 y: 0.5048 control2X: 0.6567 y: 0.5232;
		lineToX: 0.6760 y: 0.4840;
		lineToX: 0.7458 y: 0.4840;
		lineToX: 0.7458 y: 0.8434;
		curveToX: 0.8571 y: 0.7784 control1X: 0.7933 y: 0.8362 control2X: 0.8304 y: 0.8146;
		curveToX: 0.8976 y: 0.6373 control1X: 0.8841 y: 0.7420 control2X: 0.8976 y: 0.6950;
		curveToX: 0.8820 y: 0.5392 control1X: 0.8976 y: 0.6025 control2X: 0.8924 y: 0.5698;
		curveToX: 0.8356 y: 0.4542 control1X: 0.8719 y: 0.5086 control2X: 0.8565 y: 0.4803;
		curveToX: 0.7116 y: 0.3566 control1X: 0.8018 y: 0.4116 control2X: 0.7604 y: 0.3790;
		curveToX: 0.5529 y: 0.3224 control1X: 0.6631 y: 0.3338 control2X: 0.6102 y: 0.3224;
		curveToX: 0.4377 y: 0.3385 control1X: 0.5129 y: 0.3224 control2X: 0.4745 y: 0.3278;
		curveToX: 0.3356 y: 0.3854 control1X: 0.4009 y: 0.3489 control2X: 0.3669 y: 0.3645;
		curveToX: 0.2155 y: 0.5162 control1X: 0.2845 y: 0.4186 control2X: 0.2445 y: 0.4622;
		curveToX: 0.1725 y: 0.6910 control1X: 0.1869 y: 0.5700 control2X: 0.1725 y: 0.6282;
		curveToX: 0.2004 y: 0.8366 control1X: 0.1725 y: 0.7428 control2X: 0.1818 y: 0.7913;
		curveToX: 0.2819 y: 0.9562 control1X: 0.2192 y: 0.8818 control2X: 0.2464 y: 0.9217;
		curveToX: 0.4006 y: 1.0333 control1X: 0.3161 y: 0.9900 control2X: 0.3556 y: 1.0158;
		curveToX: 0.5446 y: 1.0602 control1X: 0.4455 y: 1.0512 control2X: 0.4935 y: 1.0602;
		curveToX: 0.6681 y: 1.0387 control1X: 0.5866 y: 1.0602 control2X: 0.6278 y: 1.0530;
		curveToX: 0.7800 y: 0.9782 control1X: 0.7088 y: 1.0247 control2X: 0.7461 y: 1.0045;
		lineToX: 0.8239 y: 1.0324;
		curveToX: 0.6906 y: 1.1046 control1X: 0.7832 y: 1.0639 control2X: 0.7388 y: 1.0880;
		curveToX: 0.5446 y: 1.1300 control1X: 0.6427 y: 1.1215 control2X: 0.5941 y: 1.1300;
		curveToX: 0.3742 y: 1.0978 control1X: 0.4844 y: 1.1300 control2X: 0.4276 y: 1.1193;
		curveToX: 0.2316 y: 1.0050 control1X: 0.3208 y: 1.0766 control2X: 0.2733 y: 1.0457;
		curveToX: 0.1364 y: 0.8639 control1X: 0.1899 y: 0.9643 control2X: 0.1582 y: 0.9173;
		curveToX: 0.1037 y: 0.6910 control1X: 0.1146 y: 0.8102 control2X: 0.1037 y: 0.7526;
		curveToX: 0.1369 y: 0.5216 control1X: 0.1037 y: 0.6318 control2X: 0.1148 y: 0.5753;
		curveToX: 0.2316 y: 0.3800 control1X: 0.1590 y: 0.4679 control2X: 0.1906 y: 0.4207;
		curveToX: 0.3771 y: 0.2853 control1X: 0.2736 y: 0.3387 control2X: 0.3221 y: 0.3071;
		curveToX: 0.5519 y: 0.2521 control1X: 0.4321 y: 0.2631 control2X: 0.4904 y: 0.2521;
		curveToX: 0.7438 y: 0.2946 control1X: 0.6209 y: 0.2521 control2X: 0.6849 y: 0.2662;
		curveToX: 0.8927 y: 0.4152 control1X: 0.8031 y: 0.3229 control2X: 0.8527 y: 0.3631;
		curveToX: 0.9484 y: 0.5192 control1X: 0.9172 y: 0.4471 control2X: 0.9357 y: 0.4817;
		curveToX: 0.9679 y: 0.6354 control1X: 0.9614 y: 0.5566 control2X: 0.9679 y: 0.5953;
		curveToX: 0.8903 y: 0.8380 control1X: 0.9679 y: 0.7210 control2X: 0.9421 y: 0.7885;
		curveToX: 0.6760 y: 0.9152 control1X: 0.8385 y: 0.8875 control2X: 0.7671 y: 0.9132;
		lineToX: 0.6760 y: 0.8400! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans65
	vectorGraphicsCanvas
		moveToX: 0.3796 y: 0.3243;
		lineToX: 0.2458 y: 0.6871;
		lineToX: 0.5138 y: 0.6871;
		lineToX: 0.3796 y: 0.3243;
		moveToX: 0.3239 y: 0.2272;
		lineToX: 0.4357 y: 0.2272;
		lineToX: 0.7135 y: 0.9562;
		lineToX: 0.6110 y: 0.9562;
		lineToX: 0.5446 y: 0.7692;
		lineToX: 0.2160 y: 0.7692;
		lineToX: 0.1496 y: 0.9562;
		lineToX: 0.0456 y: 0.9562;
		lineToX: 0.3239 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans66
	vectorGraphicsCanvas
		moveToX: 0.2345 y: 0.6080;
		lineToX: 0.2345 y: 0.8751;
		lineToX: 0.3927 y: 0.8751;
		curveToX: 0.5104 y: 0.8424 control1X: 0.4458 y: 0.8751 control2X: 0.4850 y: 0.8642;
		curveToX: 0.5490 y: 0.7413 control1X: 0.5361 y: 0.8203 control2X: 0.5490 y: 0.7866;
		curveToX: 0.5104 y: 0.6408 control1X: 0.5490 y: 0.6958 control2X: 0.5361 y: 0.6622;
		curveToX: 0.3927 y: 0.6080 control1X: 0.4850 y: 0.6189 control2X: 0.4458 y: 0.6080;
		lineToX: 0.2345 y: 0.6080;
		moveToX: 0.2345 y: 0.3082;
		lineToX: 0.2345 y: 0.5280;
		lineToX: 0.3805 y: 0.5280;
		curveToX: 0.4880 y: 0.5011 control1X: 0.4287 y: 0.5280 control2X: 0.4645 y: 0.5190;
		curveToX: 0.5236 y: 0.4181 control1X: 0.5117 y: 0.4829 control2X: 0.5236 y: 0.4552;
		curveToX: 0.4880 y: 0.3356 control1X: 0.5236 y: 0.3813 control2X: 0.5117 y: 0.3538;
		curveToX: 0.3805 y: 0.3082 control1X: 0.4645 y: 0.3173 control2X: 0.4287 y: 0.3082;
		lineToX: 0.2345 y: 0.3082;
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.3879 y: 0.2272;
		curveToX: 0.5617 y: 0.2741 control1X: 0.4631 y: 0.2272 control2X: 0.5210 y: 0.2428;
		curveToX: 0.6227 y: 0.4074 control1X: 0.6024 y: 0.3053 control2X: 0.6227 y: 0.3497;
		curveToX: 0.5915 y: 0.5138 control1X: 0.6227 y: 0.4520 control2X: 0.6123 y: 0.4874;
		curveToX: 0.4997 y: 0.5631 control1X: 0.5706 y: 0.5402 control2X: 0.5400 y: 0.5566;
		curveToX: 0.6125 y: 0.6285 control1X: 0.5482 y: 0.5735 control2X: 0.5858 y: 0.5953;
		curveToX: 0.6530 y: 0.7521 control1X: 0.6395 y: 0.6614 control2X: 0.6530 y: 0.7026;
		curveToX: 0.5866 y: 0.9030 control1X: 0.6530 y: 0.8172 control2X: 0.6309 y: 0.8675;
		curveToX: 0.3976 y: 0.9562 control1X: 0.5423 y: 0.9384 control2X: 0.4793 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans67
	vectorGraphicsCanvas
		moveToX: 0.6818 y: 0.2833;
		lineToX: 0.6818 y: 0.3873;
		curveToX: 0.5754 y: 0.3180 control1X: 0.6486 y: 0.3564 control2X: 0.6131 y: 0.3333;
		curveToX: 0.4557 y: 0.2950 control1X: 0.5379 y: 0.3027 control2X: 0.4981 y: 0.2950;
		curveToX: 0.2643 y: 0.3717 control1X: 0.3724 y: 0.2950 control2X: 0.3086 y: 0.3206;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2201 y: 0.4225 control2X: 0.1979 y: 0.4961;
		curveToX: 0.2643 y: 0.8131 control1X: 0.1979 y: 0.6884 control2X: 0.2201 y: 0.7620;
		curveToX: 0.4557 y: 0.8893 control1X: 0.3086 y: 0.8639 control2X: 0.3724 y: 0.8893;
		curveToX: 0.5754 y: 0.8663 control1X: 0.4981 y: 0.8893 control2X: 0.5379 y: 0.8816;
		curveToX: 0.6818 y: 0.7970 control1X: 0.6131 y: 0.8510 control2X: 0.6486 y: 0.8279;
		lineToX: 0.6818 y: 0.9000;
		curveToX: 0.5719 y: 0.9528 control1X: 0.6473 y: 0.9235 control2X: 0.6107 y: 0.9410;
		curveToX: 0.4499 y: 0.9703 control1X: 0.5335 y: 0.9645 control2X: 0.4928 y: 0.9703;
		curveToX: 0.1891 y: 0.8693 control1X: 0.3395 y: 0.9703 control2X: 0.2526 y: 0.9367;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1257 y: 0.8016 control2X: 0.0939 y: 0.7093;
		curveToX: 0.1891 y: 0.3156 control1X: 0.0939 y: 0.4752 control2X: 0.1257 y: 0.3829;
		curveToX: 0.4499 y: 0.2140 control1X: 0.2526 y: 0.2478 control2X: 0.3395 y: 0.2140;
		curveToX: 0.5729 y: 0.2316 control1X: 0.4935 y: 0.2140 control2X: 0.5345 y: 0.2199;
		curveToX: 0.6818 y: 0.2833 control1X: 0.6117 y: 0.2430 control2X: 0.6480 y: 0.2602! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans68
	vectorGraphicsCanvas
		moveToX: 0.2345 y: 0.3082;
		lineToX: 0.2345 y: 0.8751;
		lineToX: 0.3537 y: 0.8751;
		curveToX: 0.5744 y: 0.8068 control1X: 0.4543 y: 0.8751 control2X: 0.5278 y: 0.8523;
		curveToX: 0.6447 y: 0.5909 control1X: 0.6213 y: 0.7612 control2X: 0.6447 y: 0.6893;
		curveToX: 0.5744 y: 0.3766 control1X: 0.6447 y: 0.4933 control2X: 0.6213 y: 0.4218;
		curveToX: 0.3537 y: 0.3082 control1X: 0.5278 y: 0.3310 control2X: 0.4543 y: 0.3082;
		lineToX: 0.2345 y: 0.3082;
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.3385 y: 0.2272;
		curveToX: 0.6496 y: 0.3156 control1X: 0.4798 y: 0.2272 control2X: 0.5835 y: 0.2566;
		curveToX: 0.7487 y: 0.5909 control1X: 0.7157 y: 0.3742 control2X: 0.7487 y: 0.4659;
		curveToX: 0.6491 y: 0.8678 control1X: 0.7487 y: 0.7166 control2X: 0.7155 y: 0.8089;
		curveToX: 0.3385 y: 0.9562 control1X: 0.5827 y: 0.9267 control2X: 0.4792 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans69
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5968 y: 0.2272;
		lineToX: 0.5968 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5817 y: 0.5260;
		lineToX: 0.5817 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.6056 y: 0.8732;
		lineToX: 0.6056 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans70
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.5549 y: 0.2272;
		lineToX: 0.5549 y: 0.3102;
		lineToX: 0.2345 y: 0.3102;
		lineToX: 0.2345 y: 0.5250;
		lineToX: 0.5236 y: 0.5250;
		lineToX: 0.5236 y: 0.6080;
		lineToX: 0.2345 y: 0.6080;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans71
	vectorGraphicsCanvas
		moveToX: 0.6330 y: 0.8522;
		lineToX: 0.6330 y: 0.6564;
		lineToX: 0.4718 y: 0.6564;
		lineToX: 0.4718 y: 0.5753;
		lineToX: 0.7306 y: 0.5753;
		lineToX: 0.7306 y: 0.8883;
		curveToX: 0.6047 y: 0.9498 control1X: 0.6926 y: 0.9153 control2X: 0.6506 y: 0.9358;
		curveToX: 0.4577 y: 0.9703 control1X: 0.5588 y: 0.9635 control2X: 0.5098 y: 0.9703;
		curveToX: 0.1901 y: 0.8707 control1X: 0.3438 y: 0.9703 control2X: 0.2546 y: 0.9371;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1260 y: 0.8040 control2X: 0.0939 y: 0.7112;
		curveToX: 0.1901 y: 0.3141 control1X: 0.0939 y: 0.4733 control2X: 0.1260 y: 0.3805;
		curveToX: 0.4577 y: 0.2140 control1X: 0.2546 y: 0.2474 control2X: 0.3438 y: 0.2140;
		curveToX: 0.5929 y: 0.2316 control1X: 0.5052 y: 0.2140 control2X: 0.5503 y: 0.2199;
		curveToX: 0.7116 y: 0.2833 control1X: 0.6359 y: 0.2433 control2X: 0.6755 y: 0.2605;
		lineToX: 0.7116 y: 0.3883;
		curveToX: 0.5954 y: 0.3185 control1X: 0.6751 y: 0.3574 control2X: 0.6364 y: 0.3341;
		curveToX: 0.4660 y: 0.2950 control1X: 0.5544 y: 0.3029 control2X: 0.5112 y: 0.2950;
		curveToX: 0.2648 y: 0.3698 control1X: 0.3768 y: 0.2950 control2X: 0.3097 y: 0.3200;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2202 y: 0.4196 control2X: 0.1979 y: 0.4938;
		curveToX: 0.2648 y: 0.8146 control1X: 0.1979 y: 0.6907 control2X: 0.2202 y: 0.7648;
		curveToX: 0.4660 y: 0.8893 control1X: 0.3097 y: 0.8644 control2X: 0.3768 y: 0.8893;
		curveToX: 0.5593 y: 0.8805 control1X: 0.5008 y: 0.8893 control2X: 0.5319 y: 0.8864;
		curveToX: 0.6330 y: 0.8522 control1X: 0.5866 y: 0.8743 control2X: 0.6112 y: 0.8649! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans72
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.5260;
		lineToX: 0.5929 y: 0.5260;
		lineToX: 0.5929 y: 0.2272;
		lineToX: 0.6916 y: 0.2272;
		lineToX: 0.6916 y: 0.9562;
		lineToX: 0.5929 y: 0.9562;
		lineToX: 0.5929 y: 0.6090;
		lineToX: 0.2345 y: 0.6090;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans73
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans74
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.9054;
		curveToX: 0.1843 y: 1.0968 control1X: 0.2345 y: 0.9933 control2X: 0.2178 y: 1.0571;
		curveToX: 0.0236 y: 1.1564 control1X: 0.1510 y: 1.1365 control2X: 0.0975 y: 1.1564;
		lineToX: -0.0140 y: 1.1564;
		lineToX: -0.0140 y: 1.0734;
		lineToX: 0.0168 y: 1.0734;
		curveToX: 0.1091 y: 1.0367 control1X: 0.0604 y: 1.0734 control2X: 0.0912 y: 1.0612;
		curveToX: 0.1359 y: 0.9054 control1X: 0.1270 y: 1.0123 control2X: 0.1359 y: 0.9686;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans75
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.5353;
		lineToX: 0.5617 y: 0.2272;
		lineToX: 0.6886 y: 0.2272;
		lineToX: 0.3268 y: 0.5670;
		lineToX: 0.7145 y: 0.9562;
		lineToX: 0.5846 y: 0.9562;
		lineToX: 0.2345 y: 0.6051;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans76
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2345 y: 0.2272;
		lineToX: 0.2345 y: 0.8732;
		lineToX: 0.5895 y: 0.8732;
		lineToX: 0.5895 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans77
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2829 y: 0.2272;
		lineToX: 0.4689 y: 0.7233;
		lineToX: 0.6559 y: 0.2272;
		lineToX: 0.8029 y: 0.2272;
		lineToX: 0.8029 y: 0.9562;
		lineToX: 0.7067 y: 0.9562;
		lineToX: 0.7067 y: 0.3160;
		lineToX: 0.5187 y: 0.8160;
		lineToX: 0.4196 y: 0.8160;
		lineToX: 0.2316 y: 0.3160;
		lineToX: 0.2316 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans78
	vectorGraphicsCanvas
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.2687 y: 0.2272;
		lineToX: 0.5920 y: 0.8370;
		lineToX: 0.5920 y: 0.2272;
		lineToX: 0.6877 y: 0.2272;
		lineToX: 0.6877 y: 0.9562;
		lineToX: 0.5549 y: 0.9562;
		lineToX: 0.2316 y: 0.3463;
		lineToX: 0.2316 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans79
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.4318 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.6769 y: 0.8678 control1X: 0.7687 y: 0.7073 control2X: 0.7381 y: 0.7991;
		curveToX: 0.4318 y: 0.9703 control1X: 0.6157 y: 0.9362 control2X: 0.5340 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7994 control2X: 0.0939 y: 0.7076;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans80
	vectorGraphicsCanvas
		moveToX: 0.2345 y: 0.3082;
		lineToX: 0.2345 y: 0.5822;
		lineToX: 0.3586 y: 0.5822;
		curveToX: 0.4650 y: 0.5465 control1X: 0.4045 y: 0.5822 control2X: 0.4399 y: 0.5703;
		curveToX: 0.5026 y: 0.4450 control1X: 0.4901 y: 0.5228 control2X: 0.5026 y: 0.4889;
		curveToX: 0.4650 y: 0.3439 control1X: 0.5026 y: 0.4013 control2X: 0.4901 y: 0.3676;
		curveToX: 0.3586 y: 0.3082 control1X: 0.4399 y: 0.3201 control2X: 0.4045 y: 0.3082;
		lineToX: 0.2345 y: 0.3082;
		moveToX: 0.1359 y: 0.2272;
		lineToX: 0.3586 y: 0.2272;
		curveToX: 0.5436 y: 0.2828 control1X: 0.4403 y: 0.2272 control2X: 0.5020 y: 0.2457;
		curveToX: 0.6066 y: 0.4450 control1X: 0.5856 y: 0.3196 control2X: 0.6066 y: 0.3737;
		curveToX: 0.5436 y: 0.6080 control1X: 0.6066 y: 0.5169 control2X: 0.5856 y: 0.5713;
		curveToX: 0.3586 y: 0.6632 control1X: 0.5020 y: 0.6448 control2X: 0.4403 y: 0.6632;
		lineToX: 0.2345 y: 0.6632;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans81
	vectorGraphicsCanvas
		moveToX: 0.4318 y: 0.2941;
		curveToX: 0.2609 y: 0.3742 control1X: 0.3602 y: 0.2941 control2X: 0.3032 y: 0.3208;
		curveToX: 0.1979 y: 0.5924 control1X: 0.2189 y: 0.4275 control2X: 0.1979 y: 0.5003;
		curveToX: 0.2609 y: 0.8102 control1X: 0.1979 y: 0.6842 control2X: 0.2189 y: 0.7568;
		curveToX: 0.4318 y: 0.8903 control1X: 0.3032 y: 0.8636 control2X: 0.3602 y: 0.8903;
		curveToX: 0.6017 y: 0.8102 control1X: 0.5034 y: 0.8903 control2X: 0.5601 y: 0.8636;
		curveToX: 0.6647 y: 0.5924 control1X: 0.6437 y: 0.7568 control2X: 0.6647 y: 0.6842;
		curveToX: 0.6017 y: 0.3742 control1X: 0.6647 y: 0.5003 control2X: 0.6437 y: 0.4275;
		curveToX: 0.4318 y: 0.2941 control1X: 0.5601 y: 0.3208 control2X: 0.5034 y: 0.2941;
		moveToX: 0.5700 y: 0.9430;
		lineToX: 0.6999 y: 1.0851;
		lineToX: 0.5807 y: 1.0851;
		lineToX: 0.4728 y: 0.9684;
		curveToX: 0.4479 y: 0.9699 control1X: 0.4621 y: 0.9690 control2X: 0.4538 y: 0.9695;
		curveToX: 0.4318 y: 0.9703 control1X: 0.4424 y: 0.9702 control2X: 0.4370 y: 0.9703;
		curveToX: 0.1857 y: 0.8678 control1X: 0.3293 y: 0.9703 control2X: 0.2472 y: 0.9362;
		curveToX: 0.0939 y: 0.5924 control1X: 0.1245 y: 0.7991 control2X: 0.0939 y: 0.7073;
		curveToX: 0.1857 y: 0.3170 control1X: 0.0939 y: 0.4772 control2X: 0.1245 y: 0.3854;
		curveToX: 0.4318 y: 0.2140 control1X: 0.2472 y: 0.2483 control2X: 0.3293 y: 0.2140;
		curveToX: 0.6769 y: 0.3170 control1X: 0.5340 y: 0.2140 control2X: 0.6157 y: 0.2483;
		curveToX: 0.7687 y: 0.5924 control1X: 0.7381 y: 0.3854 control2X: 0.7687 y: 0.4772;
		curveToX: 0.7175 y: 0.8097 control1X: 0.7687 y: 0.6770 control2X: 0.7516 y: 0.7495;
		curveToX: 0.5700 y: 0.9430 control1X: 0.6836 y: 0.8699 control2X: 0.6344 y: 0.9144! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans82
	vectorGraphicsCanvas
		moveToX: 0.4816 y: 0.6144;
		curveToX: 0.5431 y: 0.6603 control1X: 0.5028 y: 0.6215 control2X: 0.5233 y: 0.6368;
		curveToX: 0.6037 y: 0.7570 control1X: 0.5633 y: 0.6837 control2X: 0.5835 y: 0.7159;
		lineToX: 0.7038 y: 0.9562;
		lineToX: 0.5978 y: 0.9562;
		lineToX: 0.5046 y: 0.7692;
		curveToX: 0.4343 y: 0.6720 control1X: 0.4805 y: 0.7203 control2X: 0.4570 y: 0.6880;
		curveToX: 0.3420 y: 0.6481 control1X: 0.4118 y: 0.6561 control2X: 0.3810 y: 0.6481;
		lineToX: 0.2345 y: 0.6481;
		lineToX: 0.2345 y: 0.9562;
		lineToX: 0.1359 y: 0.9562;
		lineToX: 0.1359 y: 0.2272;
		lineToX: 0.3586 y: 0.2272;
		curveToX: 0.5451 y: 0.2794 control1X: 0.4419 y: 0.2272 control2X: 0.5041 y: 0.2446;
		curveToX: 0.6066 y: 0.4371 control1X: 0.5861 y: 0.3143 control2X: 0.6066 y: 0.3668;
		curveToX: 0.5744 y: 0.5514 control1X: 0.6066 y: 0.4830 control2X: 0.5959 y: 0.5211;
		curveToX: 0.4816 y: 0.6144 control1X: 0.5532 y: 0.5817 control2X: 0.5223 y: 0.6027;
		moveToX: 0.2345 y: 0.3082;
		lineToX: 0.2345 y: 0.5670;
		lineToX: 0.3586 y: 0.5670;
		curveToX: 0.4660 y: 0.5343 control1X: 0.4061 y: 0.5670 control2X: 0.4419 y: 0.5561;
		curveToX: 0.5026 y: 0.4371 control1X: 0.4904 y: 0.5122 control2X: 0.5026 y: 0.4798;
		curveToX: 0.4660 y: 0.3409 control1X: 0.5026 y: 0.3945 control2X: 0.4904 y: 0.3624;
		curveToX: 0.3586 y: 0.3082 control1X: 0.4419 y: 0.3191 control2X: 0.4061 y: 0.3082;
		lineToX: 0.2345 y: 0.3082! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans83
	vectorGraphicsCanvas
		moveToX: 0.5729 y: 0.2511;
		lineToX: 0.5729 y: 0.3473;
		curveToX: 0.4670 y: 0.3073 control1X: 0.5355 y: 0.3294 control2X: 0.5002 y: 0.3160;
		curveToX: 0.3708 y: 0.2941 control1X: 0.4338 y: 0.2985 control2X: 0.4017 y: 0.2941;
		curveToX: 0.2463 y: 0.3253 control1X: 0.3171 y: 0.2941 control2X: 0.2756 y: 0.3045;
		curveToX: 0.2028 y: 0.4142 control1X: 0.2173 y: 0.3462 control2X: 0.2028 y: 0.3758;
		curveToX: 0.2316 y: 0.4874 control1X: 0.2028 y: 0.4464 control2X: 0.2124 y: 0.4708;
		curveToX: 0.3420 y: 0.5270 control1X: 0.2511 y: 0.5037 control2X: 0.2879 y: 0.5169;
		lineToX: 0.4015 y: 0.5392;
		curveToX: 0.5641 y: 0.6134 control1X: 0.4751 y: 0.5532 control2X: 0.5293 y: 0.5779;
		curveToX: 0.6169 y: 0.7550 control1X: 0.5993 y: 0.6486 control2X: 0.6169 y: 0.6958;
		curveToX: 0.5456 y: 0.9157 control1X: 0.6169 y: 0.8256 control2X: 0.5931 y: 0.8792;
		curveToX: 0.3376 y: 0.9703 control1X: 0.4984 y: 0.9521 control2X: 0.4290 y: 0.9703;
		curveToX: 0.2272 y: 0.9586 control1X: 0.3031 y: 0.9703 control2X: 0.2663 y: 0.9664;
		curveToX: 0.1066 y: 0.9240 control1X: 0.1885 y: 0.9508 control2X: 0.1483 y: 0.9393;
		lineToX: 0.1066 y: 0.8224;
		curveToX: 0.2243 y: 0.8732 control1X: 0.1467 y: 0.8449 control2X: 0.1859 y: 0.8618;
		curveToX: 0.3376 y: 0.8903 control1X: 0.2627 y: 0.8846 control2X: 0.3005 y: 0.8903;
		curveToX: 0.4679 y: 0.8571 control1X: 0.3939 y: 0.8903 control2X: 0.4373 y: 0.8792;
		curveToX: 0.5138 y: 0.7623 control1X: 0.4985 y: 0.8349 control2X: 0.5138 y: 0.8033;
		curveToX: 0.4806 y: 0.6783 control1X: 0.5138 y: 0.7265 control2X: 0.5028 y: 0.6985;
		curveToX: 0.3727 y: 0.6329 control1X: 0.4588 y: 0.6582 control2X: 0.4229 y: 0.6430;
		lineToX: 0.3127 y: 0.6212;
		curveToX: 0.1530 y: 0.5524 control1X: 0.2391 y: 0.6066 control2X: 0.1859 y: 0.5836;
		curveToX: 0.1037 y: 0.4220 control1X: 0.1201 y: 0.5211 control2X: 0.1037 y: 0.4777;
		curveToX: 0.1716 y: 0.2697 control1X: 0.1037 y: 0.3575 control2X: 0.1263 y: 0.3068;
		curveToX: 0.3595 y: 0.2140 control1X: 0.2171 y: 0.2325 control2X: 0.2798 y: 0.2140;
		curveToX: 0.4640 y: 0.2233 control1X: 0.3937 y: 0.2140 control2X: 0.4286 y: 0.2171;
		curveToX: 0.5729 y: 0.2511 control1X: 0.4995 y: 0.2295 control2X: 0.5358 y: 0.2387! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans84
	vectorGraphicsCanvas
		moveToX: 0.0348 y: 0.2272;
		lineToX: 0.6515 y: 0.2272;
		lineToX: 0.6515 y: 0.3102;
		lineToX: 0.3927 y: 0.3102;
		lineToX: 0.3927 y: 0.9562;
		lineToX: 0.2936 y: 0.9562;
		lineToX: 0.2936 y: 0.3102;
		lineToX: 0.0348 y: 0.3102;
		lineToX: 0.0348 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans85
	vectorGraphicsCanvas
		moveToX: 0.1247 y: 0.2272;
		lineToX: 0.2238 y: 0.2272;
		lineToX: 0.2238 y: 0.6700;
		curveToX: 0.2663 y: 0.8390 control1X: 0.2238 y: 0.7482 control2X: 0.2380 y: 0.8045;
		curveToX: 0.4040 y: 0.8903 control1X: 0.2946 y: 0.8732 control2X: 0.3405 y: 0.8903;
		curveToX: 0.5412 y: 0.8390 control1X: 0.4671 y: 0.8903 control2X: 0.5129 y: 0.8732;
		curveToX: 0.5837 y: 0.6700 control1X: 0.5695 y: 0.8045 control2X: 0.5837 y: 0.7482;
		lineToX: 0.5837 y: 0.2272;
		lineToX: 0.6828 y: 0.2272;
		lineToX: 0.6828 y: 0.6823;
		curveToX: 0.6120 y: 0.8976 control1X: 0.6828 y: 0.7773 control2X: 0.6592 y: 0.8491;
		curveToX: 0.4040 y: 0.9703 control1X: 0.5651 y: 0.9461 control2X: 0.4958 y: 0.9703;
		curveToX: 0.1950 y: 0.8976 control1X: 0.3119 y: 0.9703 control2X: 0.2422 y: 0.9461;
		curveToX: 0.1247 y: 0.6823 control1X: 0.1481 y: 0.8491 control2X: 0.1247 y: 0.7773;
		lineToX: 0.1247 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans86
	vectorGraphicsCanvas
		moveToX: 0.3239 y: 0.9562;
		lineToX: 0.0456 y: 0.2272;
		lineToX: 0.1486 y: 0.2272;
		lineToX: 0.3796 y: 0.8409;
		lineToX: 0.6110 y: 0.2272;
		lineToX: 0.7135 y: 0.2272;
		lineToX: 0.4357 y: 0.9562;
		lineToX: 0.3239 y: 0.9562! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans87
	vectorGraphicsCanvas
		moveToX: 0.0710 y: 0.2272;
		lineToX: 0.1706 y: 0.2272;
		lineToX: 0.3239 y: 0.8434;
		lineToX: 0.4767 y: 0.2272;
		lineToX: 0.5876 y: 0.2272;
		lineToX: 0.7409 y: 0.8434;
		lineToX: 0.8937 y: 0.2272;
		lineToX: 0.9938 y: 0.2272;
		lineToX: 0.8107 y: 0.9562;
		lineToX: 0.6867 y: 0.9562;
		lineToX: 0.5329 y: 0.3234;
		lineToX: 0.3776 y: 0.9562;
		lineToX: 0.2536 y: 0.9562;
		lineToX: 0.0710 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans88
	vectorGraphicsCanvas
		moveToX: 0.1008 y: 0.2272;
		lineToX: 0.2067 y: 0.2272;
		lineToX: 0.3879 y: 0.4982;
		lineToX: 0.5700 y: 0.2272;
		lineToX: 0.6760 y: 0.2272;
		lineToX: 0.4416 y: 0.5773;
		lineToX: 0.6916 y: 0.9562;
		lineToX: 0.5856 y: 0.9562;
		lineToX: 0.3805 y: 0.6461;
		lineToX: 0.1740 y: 0.9562;
		lineToX: 0.0676 y: 0.9562;
		lineToX: 0.3278 y: 0.5670;
		lineToX: 0.1008 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans89
	vectorGraphicsCanvas
		moveToX: 0.0358 y: 0.2272;
		lineToX: 0.1418 y: 0.2272;
		lineToX: 0.3439 y: 0.5270;
		lineToX: 0.5446 y: 0.2272;
		lineToX: 0.6506 y: 0.2272;
		lineToX: 0.3927 y: 0.6090;
		lineToX: 0.3927 y: 0.9562;
		lineToX: 0.2936 y: 0.9562;
		lineToX: 0.2936 y: 0.6090;
		lineToX: 0.0358 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans90
	vectorGraphicsCanvas
		moveToX: 0.0939 y: 0.2272;
		lineToX: 0.6667 y: 0.2272;
		lineToX: 0.6667 y: 0.3024;
		lineToX: 0.2057 y: 0.8732;
		lineToX: 0.6779 y: 0.8732;
		lineToX: 0.6779 y: 0.9562;
		lineToX: 0.0827 y: 0.9562;
		lineToX: 0.0827 y: 0.8810;
		lineToX: 0.5436 y: 0.3102;
		lineToX: 0.0939 y: 0.3102;
		lineToX: 0.0939 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans91
	vectorGraphicsCanvas
		moveToX: 0.1237 y: 0.1964;
		lineToX: 0.3307 y: 0.1964;
		lineToX: 0.3307 y: 0.2662;
		lineToX: 0.2135 y: 0.2662;
		lineToX: 0.2135 y: 1.0182;
		lineToX: 0.3307 y: 1.0182;
		lineToX: 0.3307 y: 1.0880;
		lineToX: 0.1237 y: 1.0880;
		lineToX: 0.1237 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans92
	vectorGraphicsCanvas
		moveToX: 0.1208 y: 0.2272;
		lineToX: 0.3747 y: 1.0490;
		lineToX: 0.2917 y: 1.0490;
		lineToX: 0.0378 y: 0.2272;
		lineToX: 0.1208 y: 0.2272! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans93
	vectorGraphicsCanvas
		moveToX: 0.3420 y: 0.1964;
		lineToX: 0.3420 y: 1.0880;
		lineToX: 0.1349 y: 1.0880;
		lineToX: 0.1349 y: 1.0182;
		lineToX: 0.2516 y: 1.0182;
		lineToX: 0.2516 y: 0.2662;
		lineToX: 0.1349 y: 0.2662;
		lineToX: 0.1349 y: 0.1964;
		lineToX: 0.3420 y: 0.1964! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans94
	vectorGraphicsCanvas
		moveToX: 0.4347 y: 0.2238;
		lineToX: 0.4787 y: 0.2238;
		lineToX: 0.6701 y: 0.4161;
		lineToX: 0.6115 y: 0.4747;
		lineToX: 0.4982 y: 0.3605;
		lineToX: 0.4982 y: 0.9562;
		lineToX: 0.4152 y: 0.9562;
		lineToX: 0.4152 y: 0.3605;
		lineToX: 0.3010 y: 0.4747;
		lineToX: 0.2424 y: 0.4161;
		lineToX: 0.4347 y: 0.2238! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans95
	vectorGraphicsCanvas
		moveToX: 0.0866 y: 0.6647;
		lineToX: 0.0866 y: 0.6207;
		lineToX: 0.2785 y: 0.4288;
		lineToX: 0.3371 y: 0.4874;
		lineToX: 0.2233 y: 0.6012;
		lineToX: 0.8185 y: 0.6012;
		lineToX: 0.8185 y: 0.6842;
		lineToX: 0.2233 y: 0.6842;
		lineToX: 0.3371 y: 0.7980;
		lineToX: 0.2785 y: 0.8566;
		lineToX: 0.0866 y: 0.6647! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans96
	vectorGraphicsCanvas
		moveToX: 0.2170 y: 0.1564;
		lineToX: 0.3547 y: 0.3390;
		lineToX: 0.2800 y: 0.3390;
		lineToX: 0.1208 y: 0.1564;
		lineToX: 0.2170 y: 0.1564! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans97
	vectorGraphicsCanvas
		moveToX: 0.3805 y: 0.6813;
		curveToX: 0.2297 y: 0.7062 control1X: 0.3079 y: 0.6813 control2X: 0.2577 y: 0.6896;
		curveToX: 0.1877 y: 0.7911 control1X: 0.2017 y: 0.7228 control2X: 0.1877 y: 0.7511;
		curveToX: 0.2189 y: 0.8673 control1X: 0.1877 y: 0.8230 control2X: 0.1981 y: 0.8484;
		curveToX: 0.3049 y: 0.8951 control1X: 0.2401 y: 0.8859 control2X: 0.2687 y: 0.8951;
		curveToX: 0.4245 y: 0.8424 control1X: 0.3547 y: 0.8951 control2X: 0.3945 y: 0.8776;
		curveToX: 0.4699 y: 0.7013 control1X: 0.4548 y: 0.8069 control2X: 0.4699 y: 0.7599;
		lineToX: 0.4699 y: 0.6813;
		lineToX: 0.3805 y: 0.6813;
		moveToX: 0.5597 y: 0.6442;
		lineToX: 0.5597 y: 0.9562;
		lineToX: 0.4699 y: 0.9562;
		lineToX: 0.4699 y: 0.8732;
		curveToX: 0.3932 y: 0.9469 control1X: 0.4494 y: 0.9064 control2X: 0.4238 y: 0.9310;
		curveToX: 0.2809 y: 0.9703 control1X: 0.3626 y: 0.9625 control2X: 0.3252 y: 0.9703;
		curveToX: 0.1471 y: 0.9235 control1X: 0.2249 y: 0.9703 control2X: 0.1803 y: 0.9547;
		curveToX: 0.0978 y: 0.7970 control1X: 0.1143 y: 0.8919 control2X: 0.0978 y: 0.8497;
		curveToX: 0.1593 y: 0.6578 control1X: 0.0978 y: 0.7355 control2X: 0.1183 y: 0.6891;
		curveToX: 0.3439 y: 0.6110 control1X: 0.2007 y: 0.6266 control2X: 0.2622 y: 0.6110;
		lineToX: 0.4699 y: 0.6110;
		lineToX: 0.4699 y: 0.6022;
		curveToX: 0.4289 y: 0.5065 control1X: 0.4699 y: 0.5608 control2X: 0.4562 y: 0.5289;
		curveToX: 0.3146 y: 0.4723 control1X: 0.4019 y: 0.4837 control2X: 0.3638 y: 0.4723;
		curveToX: 0.2233 y: 0.4835 control1X: 0.2834 y: 0.4723 control2X: 0.2529 y: 0.4760;
		curveToX: 0.1379 y: 0.5172 control1X: 0.1937 y: 0.4910 control2X: 0.1652 y: 0.5022;
		lineToX: 0.1379 y: 0.4342;
		curveToX: 0.2336 y: 0.4059 control1X: 0.1707 y: 0.4215 control2X: 0.2026 y: 0.4121;
		curveToX: 0.3239 y: 0.3961 control1X: 0.2645 y: 0.3994 control2X: 0.2946 y: 0.3961;
		curveToX: 0.5011 y: 0.4576 control1X: 0.4030 y: 0.3961 control2X: 0.4621 y: 0.4166;
		curveToX: 0.5597 y: 0.6442 control1X: 0.5402 y: 0.4987 control2X: 0.5597 y: 0.5608! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans98
	vectorGraphicsCanvas
		moveToX: 0.5246 y: 0.6832;
		curveToX: 0.4836 y: 0.5280 control1X: 0.5246 y: 0.6172 control2X: 0.5109 y: 0.5654;
		curveToX: 0.3718 y: 0.4713 control1X: 0.4565 y: 0.4902 control2X: 0.4193 y: 0.4713;
		curveToX: 0.2594 y: 0.5280 control1X: 0.3242 y: 0.4713 control2X: 0.2868 y: 0.4902;
		curveToX: 0.2189 y: 0.6832 control1X: 0.2324 y: 0.5654 control2X: 0.2189 y: 0.6172;
		curveToX: 0.2594 y: 0.8390 control1X: 0.2189 y: 0.7493 control2X: 0.2324 y: 0.8012;
		curveToX: 0.3718 y: 0.8951 control1X: 0.2868 y: 0.8764 control2X: 0.3242 y: 0.8951;
		curveToX: 0.4836 y: 0.8390 control1X: 0.4193 y: 0.8951 control2X: 0.4565 y: 0.8764;
		curveToX: 0.5246 y: 0.6832 control1X: 0.5109 y: 0.8012 control2X: 0.5246 y: 0.7493;
		moveToX: 0.2189 y: 0.4923;
		curveToX: 0.2902 y: 0.4200 control1X: 0.2378 y: 0.4598 control2X: 0.2616 y: 0.4357;
		curveToX: 0.3937 y: 0.3961 control1X: 0.3192 y: 0.4041 control2X: 0.3537 y: 0.3961;
		curveToX: 0.5553 y: 0.4752 control1X: 0.4601 y: 0.3961 control2X: 0.5140 y: 0.4225;
		curveToX: 0.6178 y: 0.6832 control1X: 0.5970 y: 0.5280 control2X: 0.6178 y: 0.5973;
		curveToX: 0.5553 y: 0.8912 control1X: 0.6178 y: 0.7692 control2X: 0.5970 y: 0.8385;
		curveToX: 0.3937 y: 0.9703 control1X: 0.5140 y: 0.9440 control2X: 0.4601 y: 0.9703;
		curveToX: 0.2902 y: 0.9469 control1X: 0.3537 y: 0.9703 control2X: 0.3192 y: 0.9625;
		curveToX: 0.2189 y: 0.8742 control1X: 0.2616 y: 0.9310 control2X: 0.2378 y: 0.9067;
		lineToX: 0.2189 y: 0.9562;
		lineToX: 0.1286 y: 0.9562;
		lineToX: 0.1286 y: 0.1964;
		lineToX: 0.2189 y: 0.1964;
		lineToX: 0.2189 y: 0.4923! !

!BitstreamVeraRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:03'!
drawVeraSans99
	vectorGraphicsCanvas
		moveToX: 0.5256 y: 0.4303;
		lineToX: 0.5256 y: 0.5143;
		curveToX: 0.4489 y: 0.4830 control1X: 0.5002 y: 0.5003 control2X: 0.4746 y: 0.4899;
		curveToX: 0.3718 y: 0.4723 control1X: 0.4235 y: 0.4759 control2X: 0.3978 y: 0.4723;
		curveToX: 0.2360 y: 0.5280 control1X: 0.3135 y: 0.4723 control2X: 0.2682 y: 0.4908;
		curveToX: 0.1877 y: 0.6832 control1X: 0.2038 y: 0.5647 control2X: 0.1877 y: 0.6165;
		curveToX: 0.2360 y: 0.8390 control1X: 0.1877 y: 0.7500 control2X: 0.2038 y: 0.8019;
		curveToX: 0.3718 y: 0.8942 control1X: 0.2682 y: 0.8758 control2X: 0.3135 y: 0.8942;
		curveToX: 0.4489 y: 0.8839 control1X: 0.3978 y: 0.8942 control2X: 0.4235 y: 0.8908;
		curveToX: 0.5256 y: 0.8522 control1X: 0.4746 y: 0.8768 control2X: 0.5002 y: 0.8662;
		lineToX: 0.5256 y: 0.9352;
		curveToX: 0.4474 y: 0.9616 control1X: 0.5005 y: 0.9469 control2X: 0.4745 y: 0.9557;
		curveToX: 0.3620 y: 0.9703 control1X: 0.4207 y: 0.9674 control2X: 0.3923 y: 0.9703;
		curveToX: 0.1657 y: 0.8927 control1X: 0.2796 y: 0.9703 control2X: 0.2142 y: 0.9445;
		curveToX: 0.0929 y: 0.6832 control1X: 0.1172 y: 0.8409 control2X: 0.0929 y: 0.7711;
		curveToX: 0.1662 y: 0.4728 control1X: 0.0929 y: 0.5940 control2X: 0.1174 y: 0.5239;
		curveToX: 0.3678 y: 0.3961 control1X: 0.2153 y: 0.4217 control2X: 0.2826 y: 0.3961;
		curveToX: 0.4489 y: 0.4049 control1X: 0.3955 y: 0.3961 control2X: 0.4225 y: 0.3991;
		curveToX: 0.5256 y: 0.4303 control1X: 0.4753 y: 0.4104 control2X: 0.5008 y: 0.4189! !

!BitstreamVeraRenderer class methodsFor: 'testing' stamp: 'jmv 12/10/2017 21:16:44'!
testCharacter: code
	"
	BitstreamVeraRenderer testCharacter: 100
	"
	| c r  |
	c _ (VectorCanvas onForm: Display) into: self runningWorld.
	r _ BitstreamVeraRenderer new vectorGraphicsCanvas: c.
	c engine fillColor: Color green do: [
		r drawCharCode: code atX: 38 y: 22 pointSize: 160 italics: false ].
	Display forceToScreen! !

!TechnicalFontRenderer methodsFor: 'services' stamp: 'jmv 12/10/2017 21:12:35'!
drawCharCode: charCode atX: x0 y: y0 pointSize: ps italics: doItalics
	| symbol loc |
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 20 color: Color black do: [
		(TechnicalFontRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 200 italics: false;
			drawCharCode: 97 atX: 250 y: 200 pointSize: 200 italics: true ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 1.25 color: Color black do: [
		(TechnicalFontRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 10 italics: false ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 1.25 color: Color black do: [
		(TechnicalFontRenderer new vectorGraphicsCanvas: c)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 158 y: 200.25 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 166 y: 200.5 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 174 y: 200.75 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 182 y: 201 pointSize: 12 italics: false ].
Display forceToScreen
"
	"revisar todo este metodo"
	self revisar.
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].
	"Implement them!!"
	charCode > 127 ifTrue: [ ^self ].
	
	"Recibir la location y modificarla, no?"
	loc _ (AffineTransformation withRadians: 0.0 scale: ps/10 position: x0@(y0+ps)) withYAxisNegated.
	doItalics ifTrue: [ loc _ loc italizing ].
	vectorGraphicsCanvas intoLocation: loc.

	symbol _ ('drawTechnical', charCode printString) asSymbol.
	self perform: symbol.
	
	vectorGraphicsCanvas outOfLocation! !

!TechnicalFontRenderer methodsFor: 'other' stamp: 'jmv 12/10/2017 21:14:43'!
drawM3BoxAtX: x0 y: y0 pointSize: ps italics: doItalics

	vectorGraphicsCanvas intoLocation: (AffineTransformation withRadians: 0 scale: ps//10 position: x0@y0).
	vectorGraphicsEngine strokeWidth: 2 color: Color red do: [
		-0.5 to: 9.5 do: [ :x |
			vectorGraphicsCanvas
				moveToX: x y: 0.5;
				lineToX: x y: 10.5 ].
		0.5 to: 13.5 do: [ :y |
			vectorGraphicsCanvas
				moveToX: -0.5 y: y;
				lineToX: 5.5 y: y ]].
	vectorGraphicsCanvas outOfLocation! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:11'!
drawTechnical100
"
TechnicalFontRenderer testCharacter: 100.
"
	vectorGraphicsCanvas
		m: 4 y: 6;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical101
"
TechnicalFontRenderer testCharacter: 101.
"
	vectorGraphicsCanvas
		m: 0 y: 3;
		r: 4;
		u: 1.5;
		cul: 1.5;
		l: 1;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical102
"
TechnicalFontRenderer testCharacter: 102.
"
	vectorGraphicsCanvas
		m: 1 y: 0;
		u: 7.5;
		cur: 1.5;
		r: 0.5;
		m: 0 y: 6;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical103
"
TechnicalFontRenderer testCharacter: 103.
"
	vectorGraphicsCanvas
		m: 4 y: 0;
		l: 2.5;
		clu: 1.5;
		u: 3;
		cur: 1.5;
		r: 2.5;
		d: 7.5;
		cdl: 1.5;
		l: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical104
"
TechnicalFontRenderer testCharacter: 104.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		m: 0 y: 6;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical105
"
TechnicalFontRenderer testCharacter: 105.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 6;
		m: 0 y: 8.5;
		l: 0.15;
		u: 0.5;
		r: 0.3;
		d: 0.5;
		l: 0.15! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:12'!
drawTechnical106
"
TechnicalFontRenderer testCharacter: 106.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical107
"
TechnicalFontRenderer testCharacter: 107.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3;
		li: 4 y: 6;
		m: 1.4 y: 4.05;
		li: 4 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical108
"
TechnicalFontRenderer testCharacter: 108.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical109
"
TechnicalFontRenderer testCharacter: 109.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 6;
		r: 4.5;
		crd: 1.5;
		d: 4.5;
		m: 3 y: 6;
		d: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical110
"
TechnicalFontRenderer testCharacter: 110.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 6;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical111
"
TechnicalFontRenderer testCharacter: 111.
"
	vectorGraphicsCanvas
		m: 1.5 y: 0;
		clu: 1.5;
		u: 3;
		cur: 1.5;
		r: 1;
		crd: 1.5;
		d: 3;
		cdl: 1.5;
		l: 1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:13'!
drawTechnical112
"
TechnicalFontRenderer testCharacter: 112.
"
	vectorGraphicsCanvas
		m: 0 y: -3;
		u: 9;
		r: 2.5;
		crd: 1.5;
		d: 3;
		cdl: 1.5;
		l: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:14'!
drawTechnical113
"
TechnicalFontRenderer testCharacter: 113.
"
	vectorGraphicsCanvas
		m: 4 y: -3;
		u: 9;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:14'!
drawTechnical114
"
TechnicalFontRenderer testCharacter: 114.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 6;
		r: 1.5;
		cDx: 1.5 dy: -0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:14'!
drawTechnical115
"
TechnicalFontRenderer testCharacter: 115.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:14'!
drawTechnical116
"
TechnicalFontRenderer testCharacter: 116.
"
	vectorGraphicsCanvas
		m: 1 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5;
		m: 0 y: 6;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:14'!
drawTechnical117
"
TechnicalFontRenderer testCharacter: 117.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		d: 4.5;
		cdr: 1.5;
		r: 2.5;
		u: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:15'!
drawTechnical118
"
TechnicalFontRenderer testCharacter: 118.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		li: 2 y: 0;
		li: 4 y: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:15'!
drawTechnical119
"
TechnicalFontRenderer testCharacter: 119.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		li: 1.5 y: 0;
		li: 3 y: 4;
		li: 4.5 y: 0;
		li: 6 y: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:15'!
drawTechnical120
"
TechnicalFontRenderer testCharacter: 120.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		li: 4 y: 0;
		m: 4 y: 6;
		li: 0 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:15'!
drawTechnical121
"
TechnicalFontRenderer testCharacter: 121.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		li: 2 y: 0;
		m: 4 y: 6;
		li: 1 y: -3;
		l: 1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:15'!
drawTechnical122
"
TechnicalFontRenderer testCharacter: 122.
"
	vectorGraphicsCanvas
		m: 0 y: 6;
		r: 4;
		li: 0 y: 0;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:16'!
drawTechnical123
"
TechnicalFontRenderer testCharacter: 123.
"
	vectorGraphicsCanvas
		m: 0 y: 4;
		cru: 1.5;
		u: 3;
		cur: 1.5;
		m: 0 y: 4;
		crd: 1.5;
		d: 3;
		cdr: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:16'!
drawTechnical124
"
TechnicalFontRenderer testCharacter: 124.
"
	vectorGraphicsCanvas
		m: 0 y: 10;
		d: 11! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:16'!
drawTechnical125
"
TechnicalFontRenderer testCharacter: 125.
"
	vectorGraphicsCanvas
		m: 3 y: 4;
		clu: 1.5;
		u: 3;
		cul: 1.5;
		m: 3 y: 4;
		cld: 1.5;
		d: 3;
		cdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:16'!
drawTechnical126
"
TechnicalFontRenderer testCharacter: 126.
"
	vectorGraphicsCanvas
		m: 0 y: 3;
		curd: 1;
		cdru: 1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:21'!
drawTechnical127
"
TechnicalFontRenderer testCharacter: 127.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 6;
		r: 4;
		d: 6;
		l: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:02'!
drawTechnical32! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:21'!
drawTechnical33
"
TechnicalFontRenderer testCharacter: 33.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 6;
		m: 0 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:22'!
drawTechnical34
"
TechnicalFontRenderer testCharacter: 34.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 2;
		m: 2 y: 9;
		d: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:22'!
drawTechnical35
"
TechnicalFontRenderer testCharacter: 35.
"
	vectorGraphicsCanvas
		m: 0.5 y: 0;
		li: 2 y: 9;
		m: 3 y: 0;
		li: 4.5 y: 9;
		m: 0 y: 3;
		r: 5;
		m: 0 y: 6;
		r: 5
		! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:22'!
drawTechnical36
"
TechnicalFontRenderer testCharacter: 36.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:22'!
drawTechnical37
"
TechnicalFontRenderer testCharacter: 37.
"
	vectorGraphicsCanvas
		m: 1.5 y: 0;
		li: 6.5 y: 9;
		m: 1.5 y: 5.7;
		clur: 1.5;
		crdl: 1.5;
		m: 6.5 y: 0.3;
		clur: 1.5;
		crdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:22'!
drawTechnical38
"
TechnicalFontRenderer testCharacter: 38.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:23'!
drawTechnical39
"
TechnicalFontRenderer testCharacter: 39.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:23'!
drawTechnical40
"
TechnicalFontRenderer testCharacter: 40.
"
	vectorGraphicsCanvas
		m: 2 y: 10;
		cld: 1.5;
		li: 0 y: 6;
		d: 4;
		li: 0.5 y: -0.5;
		cdr: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:23'!
drawTechnical41
"
TechnicalFontRenderer testCharacter: 41.
"
	vectorGraphicsCanvas
		m: 0 y: 10;
		crd: 1.5;
		li: 2 y: 6;
		d: 4;
		li: 1.5 y: -0.5;
		cdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:23'!
drawTechnical42
"
TechnicalFontRenderer testCharacter: 42.
"
	vectorGraphicsCanvas
		m: 0 y: 7.5;
		li: 5 y: 4.5;
		m: 0 y: 4.5;
		li: 5 y: 7.5;
		m: 2.5 y: 9;
		d: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:23'!
drawTechnical43
"
TechnicalFontRenderer testCharacter: 43.
"
	vectorGraphicsCanvas
		m: 2.5 y: 0;
		u: 6;
		m: 0 y: 3;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical44
"
TechnicalFontRenderer testCharacter: 44.
"
	vectorGraphicsCanvas
		m: 1.25 y: 0;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		li: 0 y: -2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical45
"
TechnicalFontRenderer testCharacter: 45.
"
	vectorGraphicsCanvas
		m: 0 y: 3;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical46
"
TechnicalFontRenderer testCharacter: 46.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical47
"
TechnicalFontRenderer testCharacter: 47.
"
	vectorGraphicsCanvas
		m: 0 y: -1;
		li: 4 y: 10! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical48
"
TechnicalFontRenderer testCharacter: 48.
"
	vectorGraphicsCanvas
		m: 0 y: 7;
		cur: 2;
		r: 1;
		crd: 2;
		d: 5;
		cdl: 2;
		l: 1;
		clu: 2;
		u: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:24'!
drawTechnical49
"
TechnicalFontRenderer testCharacter: 49.
"
	vectorGraphicsCanvas
		m: 1 y: 7;
		li: 3 y: 9;
		d: 9;
		m: 1 y: 0;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical50
"
TechnicalFontRenderer testCharacter: 50.
"
	vectorGraphicsCanvas
		m: 0 y: 7;
		cur: 2;
		r: 1;
		crd: 2;
		d: 1;
		li: 0 y: 0;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical51
"
TechnicalFontRenderer testCharacter: 51.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical52
"
TechnicalFontRenderer testCharacter: 52.
"
	vectorGraphicsCanvas
		m: 2 y: 9;
		li: 0 y: 2;
		r: 5;
		m: 4 y: 0;
		u: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical53
"
TechnicalFontRenderer testCharacter: 53.
"
	vectorGraphicsCanvas
		m: 4.3 y: 9;
		l: 4.3;
		d: 4;
		r: 3;
		crd: 2;
		d: 1;
		cdl: 2;
		l: 1.5;
		cDx: -1.5 dy: 0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical54
"
TechnicalFontRenderer testCharacter: 54.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:25'!
drawTechnical55
"
TechnicalFontRenderer testCharacter: 55.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		r: 5;
		li: 1 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical56
"
TechnicalFontRenderer testCharacter: 56.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical57
"
TechnicalFontRenderer testCharacter: 57.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical58
"
TechnicalFontRenderer testCharacter: 58.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical59
"
TechnicalFontRenderer testCharacter: 59.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical60
"
TechnicalFontRenderer testCharacter: 60.
"
	vectorGraphicsCanvas
		m: 4 y: 5;
		li: 0 y: 2.5;
		li: 4 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical61
"
TechnicalFontRenderer testCharacter: 61.
"
	vectorGraphicsCanvas
		m: 0 y: 4;
		r: 4;
		m: 0 y: 1;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:26'!
drawTechnical62
"
TechnicalFontRenderer testCharacter: 62.
"
	vectorGraphicsCanvas
		m: 0 y: 5;
		li: 4 y: 2.5;
		li: 0 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical63
"
TechnicalFontRenderer testCharacter: 63.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical64
"
TechnicalFontRenderer testCharacter: 64.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical65
"
TechnicalFontRenderer testCharacter: 65.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		li: 2.5 y: 9;
		li: 5 y: 0;
		m: 0.56 y: 2;
		r: 3.88! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical66
"
TechnicalFontRenderer testCharacter: 66.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical67
"
TechnicalFontRenderer testCharacter: 67.
"
	vectorGraphicsCanvas
		m: 5 y: 9;
		l: 3;
		cld: 2;
		d: 5;
		cdr: 2;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:27'!
drawTechnical68
"
TechnicalFontRenderer testCharacter: 68.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		r: 2;
		crd: 3;
		d: 3;
		cdl: 3;
		l: 2;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:28'!
drawTechnical69
"
TechnicalFontRenderer testCharacter: 69.
"
	vectorGraphicsCanvas
		m: 5 y: 0;
		l: 5;
		u: 9;
		r: 5;
		m: 0 y: 5;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:28'!
drawTechnical70
"
TechnicalFontRenderer testCharacter: 70.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		r: 5;
		m: 0 y: 5;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:28'!
drawTechnical71
"
TechnicalFontRenderer testCharacter: 71.
"
	vectorGraphicsCanvas
		m: 5 y: 9;
		l: 3;
		cld: 2;
		d: 5;
		cdr: 2;
		r: 3;
		u: 5;
		l: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:28'!
drawTechnical72
"
TechnicalFontRenderer testCharacter: 72.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		m: 5 y: 0;
		u: 9;
		m: 0 y: 5;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:28'!
drawTechnical73
"
TechnicalFontRenderer testCharacter: 73.
"
	vectorGraphicsCanvas
		m: 0.5 y: 0;
		r: 3;
		m: 2 y: 0;
		u: 9;
		m: 0.5 y: 9;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical74
"
TechnicalFontRenderer testCharacter: 74.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		r: 2;
		cru: 2;
		u: 7! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical75
"
TechnicalFontRenderer testCharacter: 75.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3.5;
		li: 5 y: 9;
		m: 1.93 y: 5.6;
		li: 5 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical76
"
TechnicalFontRenderer testCharacter: 76.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 9;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical77
"
TechnicalFontRenderer testCharacter: 77.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		li: 3 y: 4;
		li: 6 y: 9;
		d: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical78
"
TechnicalFontRenderer testCharacter: 78.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		li: 5 y: 0;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:29'!
drawTechnical79
"
TechnicalFontRenderer testCharacter: 79.
"
	vectorGraphicsCanvas
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:30'!
drawTechnical80
"
TechnicalFontRenderer testCharacter: 80.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:30'!
drawTechnical81
"
TechnicalFontRenderer testCharacter: 81.
"
	vectorGraphicsCanvas
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3;
		m: 3 y: 2;
		li: 6 y: -1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:30'!
drawTechnical82
"
TechnicalFontRenderer testCharacter: 82.
"
	vectorGraphicsCanvas
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5;
		m: 3 y: 4;
		li: 5 y: 0! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:30'!
drawTechnical83
"
TechnicalFontRenderer testCharacter: 83.
"
	vectorGraphicsCanvas
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:30'!
drawTechnical84
"
TechnicalFontRenderer testCharacter: 84.
"
	vectorGraphicsCanvas
		m: -0.5 y: 9;
		r: 6;
		m: 2.5 y: 9;
		d: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical85
"
TechnicalFontRenderer testCharacter: 85.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 7;
		cdr: 2;
		r: 1;
		cru: 2;
		u: 7! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical86
"
TechnicalFontRenderer testCharacter: 86.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		li: 2.5 y: 0;
		li: 5 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical87
"
TechnicalFontRenderer testCharacter: 87.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		li: 2 y: 0;
		li: 4 y: 6;
		li: 6 y: 0;
		li: 8 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical88
"
TechnicalFontRenderer testCharacter: 88.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		li: 5 y: 0;
		m: 0 y: 0;
		li: 5 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical89
"
TechnicalFontRenderer testCharacter: 89.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		li: 2.5 y: 4.5;
		li: 5 y: 9;
		m: 2.5 y: 4.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:31'!
drawTechnical90
"
TechnicalFontRenderer testCharacter: 90.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		r: 5;
		li: 0 y: 0;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical91
"
TechnicalFontRenderer testCharacter: 91.
"
	vectorGraphicsCanvas
		m: 2 y: 10;
		l: 2;
		d: 12;
		r: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical92
"
TechnicalFontRenderer testCharacter: 92.
"
	vectorGraphicsCanvas
		m: 0 y: 10;
		li: 4  y: -1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical93
"
TechnicalFontRenderer testCharacter: 93.
"
	vectorGraphicsCanvas
		m: 0 y: 10;
		r: 2;
		d: 12;
		l: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical94
"
TechnicalFontRenderer testCharacter: 94.
"
	vectorGraphicsCanvas
		m: 2 y: 0;
		u: 9;
		m: 0 y: 7;
		li: 2 y: 9;
		li: 4 y: 7! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical95
"
TechnicalFontRenderer testCharacter: 95.
"
	vectorGraphicsCanvas
		m: 0 y: 3;
		r: 8;
		m: 2 y: 1;
		li: 0 y: 3;
		li: 2 y: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:32'!
drawTechnical96
"
TechnicalFontRenderer testCharacter: 96.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		li: 1 y: 8! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:33'!
drawTechnical97
"
TechnicalFontRenderer testCharacter: 97.
"
	vectorGraphicsCanvas
		m: 0.5 y: 6;
		r: 2;
		crd: 1.5;
		d: 4.5;
		l: 2.5; 
		clur: 1.5;
		r: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:33'!
drawTechnical98
"
TechnicalFontRenderer testCharacter: 98.
"
	vectorGraphicsCanvas
		m: 0 y: 9;
		d: 9;
		r: 2.5;
		cru: 1.5;
		u: 3;
		cul: 1.5;
		l: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 10/23/2014 22:34'!
drawTechnical99
"
TechnicalFontRenderer testCharacter: 99.
"
	vectorGraphicsCanvas
		m: 3 y: 6;
		l: 1.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 1.5! !

!TechnicalFontRenderer class methodsFor: 'testing' stamp: 'jmv 12/10/2017 21:14:08'!
testCharacter: code
	"
	TechnicalFontRenderer testCharacter: 100
	"
	| c r  |
	c _ (VectorCanvas onForm: Display) into: self runningWorld.
	r _ TechnicalFontRenderer new vectorGraphicsCanvas: c.
	r drawM3BoxAtX: 38 y: 22 pointSize: 160 italics: false.
	c engine strokeWidth: 16 color: Color green do: [
		r drawCharCode: code atX: 38 y: 22 pointSize: 160 italics: false ].
	Display forceToScreen! !

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
SVGElementMorph initialize!
VectorCanvas initialize!
