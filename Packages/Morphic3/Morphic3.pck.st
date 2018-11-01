'From Cuis 5.0 of 7 November 2016 [latest update: #3485] on 1 November 2018 at 10:11:40 am'!
'Description Please enter a description for this package '!
!provides: 'Morphic3' 1 54!
!requires: 'LinearAlgebra' 1 nil nil!
!requires: 'YAXO' 1 nil nil!
!requires: 'Color-Extras' 1 nil nil!
SystemOrganization addCategory: #'Morphic3-Kernel'!
SystemOrganization addCategory: #'Morphic3-SVG'!
SystemOrganization addCategory: #'Morphic3-SVG shapes'!
SystemOrganization addCategory: #'Morphic3-SVG path commands'!
SystemOrganization addCategory: #'Morphic3-Experiments'!


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

!classDefinition: #StrokeFont category: #'Morphic3-Kernel'!
AbstractFont subclass: #StrokeFont
	instanceVariableNames: 'name emphasis baseFont derivativeFonts pointSize characterToGlyphMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'StrokeFont class' category: #'Morphic3-Kernel'!
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
	instanceVariableNames: 'engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'VectorCanvas class' category: #'Morphic3-Kernel'!
VectorCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorEngine category: #'Morphic3-Kernel'!
Object subclass: #VectorEngine
	instanceVariableNames: 'targetForm targetWidth targetHeight antiAliasingWidth subPixelDelta strokeWidth strokeAlpha strokeRed strokeGreen strokeBlue fillAlpha fillRed fillGreen fillBlue doStroke doFill clipLeft clipTop clipRight clipBottom currentMorph currentOwnerIfClips boundsLeft boundsTop boundsRight boundsBottom morphAtPixel morphBoundsLeft morphBoundsRight morphBoundsTop morphBoundsBottom mask edgeCountPerPixel hopsPerPixel auxBitBlt firstX firstY currentX currentY trajectoryPoints fragmentStartIndexes geometryTransformation lastClientX lastClientY lastControlX lastControlY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'VectorEngine class' category: #'Morphic3-Kernel'!
VectorEngine class
	instanceVariableNames: ''!

!classDefinition: #CodeGeneratingVectorEngine category: #'Morphic3-Kernel'!
VectorEngine subclass: #CodeGeneratingVectorEngine
	instanceVariableNames: 'ignoreFirstMoveTo charCode code didStuff digits'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic3-Kernel'!
!classDefinition: 'CodeGeneratingVectorEngine class' category: #'Morphic3-Kernel'!
CodeGeneratingVectorEngine class
	instanceVariableNames: ''!

!classDefinition: #VectorFontRenderer category: #'Morphic3-Kernel'!
Object subclass: #VectorFontRenderer
	instanceVariableNames: 'vectorGraphicsEngine'
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

(VectorCanvas onForm: Display) fullDraw: self runningWorld. Display copy inspect.
Display writePNGfileNamed: 'm3z.png'

On any Morph:
| c |
c _ VectorCanvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self.
Display forceToScreen!

!VectorEngine commentStamp: 'jmv 1/1/2018 12:40:28' prior: 0!
A Vector Graphics rasterizer for Morphic 3.
Supports most SVG primitives.
Gives exceptional quality.

(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngine onForm: Display)
	fillColor: Color red do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen!

!CodeGeneratingVectorEngine commentStamp: '<historical>' prior: 0!
"
An aid for generating, code. For example, for:
- Italics versions of Vector Fonts in VectorFontRenderer
- For TechnicalFontRenderer, convert the 'designer friendly' protocol and space into canonic
- In the future, for generating FloatArrays for the glyph specs (instead of explicit method calls)
"

rcls _ BitstreamVeraRenderer.
selectorPrefix _ 'drawVeraSansItalics'.
methodCategory _ 'glyphs - italics'.
italics _ true.
f _ Display.
c _ VectorCanvas onForm: f.
e _ CodeGeneratingVectorEngine onForm: f.
c instVarNamed: 'engine' put: e.
r _ rcls new vectorGraphicsEngine: e.
c into: self runningWorld.
"0 to: 255 do: [ :charCode |"
"#(100) do: [ :charCode |"
0 to: 255 do: [ :charCode |
	e charCode: charCode selectorPrefix: selectorPrefix.
	e fillColor: Color green do: [
		e drawCharCode: charCode italics: italics renderer: r canvas: c ].
	e storeCode.
	e storeCodeIn: r class category: methodCategory
].
Display forceToScreen.



rcls _ TechnicalFontRenderer.
italics _ false.
italics _ true.
italics ifFalse: [
	selectorPrefix _ 'draw_Technical'.
	methodCategory _ 'glyphs - xx' ]
ifTrue: [
	selectorPrefix _ 'draw_TechnicalItalics'.
	methodCategory _ 'glyphs - italics' ].
f _ Display.
c _ VectorCanvas onForm: f.
e _ CodeGeneratingVectorEngine onForm: f.
c instVarNamed: 'engine' put: e.
r _ rcls new vectorGraphicsEngine: e.
c into: self runningWorld.
"0 to: 255 do: [ :charCode |"
"#(100) do: [ :charCode |"
0 to: 255 do: [ :charCode |
	e charCode: charCode selectorPrefix: selectorPrefix.
	e strokeWidth: 0.1 color: Color green do: [
		e drawCharCode: charCode italics: italics renderer: r canvas: c ].
	e storeCode.
	e storeCodeIn: r class category: methodCategory
].
Display forceToScreen.!

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

!StrokeFont methodsFor: 'private' stamp: 'jmv 4/15/2014 09:29'!
createCharacterToGlyphMap
	"Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."
	| map |
	map _ Array new: 256.
	0 to: self minAscii - 1 do:[:i| map at: i + 1 put: self maxAscii + 1].
	self minAscii to: self maxAscii do:[:i| map at: i + 1 put: i].
	self maxAscii + 1 to: 255 do:[:i| map at: i + 1 put: self maxAscii + 1].
	^map! !

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

!SVGElementMorph methodsFor: 'building' stamp: 'jmv 6/16/2010 10:01'!
usedAttributeNames
	^#('style' 'fill' 'fill-opacity' 'opacity' 'stroke' 'stroke-opacity' 'stroke-width' 'transform' 'stroke-dasharray' 'stroke-dashoffset')! !

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

!SVGElementMorph methodsFor: 'accessing' stamp: 'jmv 6/10/2010 15:54'!
opacity
	^opacity == #inherit
		ifFalse: [ opacity ]
		ifTrue: [
			(owner isKindOf: SVGElementMorph)
				ifTrue: [ owner opacity ]
				ifFalse: [ 1.0 ]]! !

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

!SVGElementMorph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:30'!
morphBoundsInWorld
self revisar. "por supuesto que no..."
"Intentar eliminar, directamente"
	^Display boundingBox! !

!SVGElementMorph methodsFor: 'private' stamp: 'jmv 6/10/2010 21:50'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	submorphs do: [ :each | each privatePointsDo: aBlock ]! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:16'!
attributeAt: attributeName in: anXMLElement orIn: styleDictionary

	^anXMLElement attributeAt: attributeName ifAbsent: [
		styleDictionary ifNotNil: [ styleDictionary at: attributeName ifAbsent: nil ]]! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/14/2010 17:17'!
colorAttributeAt: attributeName in: anXMLElement orIn: styleDictionary

	| data |
	data _ self attributeAt: attributeName in: anXMLElement orIn: styleDictionary.
	^data
		ifNil: [ #inherit ]
		ifNotNil: [ self svgColorOrGradientNameFor: data ]! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 09:50'!
ignoredAttributes

	^IgnoredAttributes! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 10:44'!
ignoredElements

	^IgnoredElements! !

!SVGElementMorph class methodsFor: 'building - aux' stamp: 'jmv 6/16/2010 10:08'!
ignoredPrefixes

	^IgnoredPrefixes! !

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

!SVGElementMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:25'!
classFor: anSVGName

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGName
			ifTrue: [ ^cls ] ].
	^nil! !

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

!SVGElementMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:28'!
svgName
	"Answer the name of the svg elements we can represent"

	^self subclassResponsibility! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:04:57'!
examples
"
SVGElementMorph examples
"
| m base |
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
#('lion' 'lionx' 'tiger1' 'magician' 'beer' 'butterfly' 'picasso' 'Phone' 'baby' 'longhorn' 'cowboy' 'dancer' 'b8' 'clinton' 'pixelCoverageSucks') do: [ :fn |
Transcript clear.
"["
[
[
	m _ SVGMainMorph fromFileNamed: base, fn, '.svg'.
	m privateOwner: self runningWorld.
	m morphPosition: 100@100.
	(VectorCanvas onForm: Display) fullDraw: m.
	Display forceToScreen.
"	m exportAs: '/Users/juanvuletich/Desktop/Morphic 3/pp.png'"
	"
	Display copy inspect.
	"
] on: SVGWarning do: [ :warning | warning print. warning resume ]
] on: SVGError do: [ :error | error print. error return ]
]
"] on: Error do: [ :dnu | ('Server error: ', dnu printString) print. dnu return ]"! !

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:05:13'!
examplesBeer
"
SVGElementMorph examplesBeer
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:05:42'!
examplesLion
"
SVGElementMorph examplesLion
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:05:59'!
examplesMagician
"
SVGElementMorph examplesMagician
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:06:38'!
examplesTiger
"
SVGElementMorph examplesTiger
"
| m base |
Transcript clear.
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 11/1/2018 10:09:09'!
phoneExample
"
SVGElementMorph phoneExample
"
| m base |
base _ 'Cuis-Smalltalk-Dev/Packages/Morphic3/SVGSamples/'.
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

!SVGMainMorph class methodsFor: 'instance creation' stamp: 'jmv 6/9/2010 22:25'!
svgName
	"Answer the name of the svg elements we can represent"

	^#svg! !

!SVGCircle methodsFor: 'building' stamp: 'jmv 6/10/2010 09:21'!
buildFromXml: anXMLElement
	"Perhaps some general initialization is in order?"

	super buildFromXml: anXMLElement.
	cx _ (anXMLElement attributeAt: 'cx') asNumber.
	cy _ (anXMLElement attributeAt: 'cy') asNumber.
	r _ (anXMLElement attributeAt: 'r') asNumber! !

!SVGCircle methodsFor: 'building' stamp: 'jmv 5/14/2010 22:21'!
usedAttributeNames

	^super usedAttributeNames,
		#('cx' 'cy' 'r')! !

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

!SVGEllipse methodsFor: 'building' stamp: 'jmv 5/14/2010 22:21'!
usedAttributeNames

	^super usedAttributeNames,
		#('cx' 'cy' 'rx' 'ry')! !

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

!SVGLine methodsFor: 'building' stamp: 'jmv 5/14/2010 22:17'!
usedAttributeNames

	^super usedAttributeNames,
		#('x1' 'y1' 'x2' 'y2')! !

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

!SVGPath methodsFor: 'building' stamp: 'jmv 5/4/2010 05:26'!
usedAttributeNames

	^super usedAttributeNames,
		#('d')! !

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

!SVGPolyLine methodsFor: 'building' stamp: 'jmv 4/27/2010 15:59'!
usedAttributeNames

	^super usedAttributeNames,
		#('points')! !

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

!SVGRect methodsFor: 'building' stamp: 'jmv 5/14/2010 22:17'!
usedAttributeNames

	^super usedAttributeNames,
		#('x' 'y' 'width' 'height' 'rx' 'ry')! !

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

!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 10/14/2014 11:02'!
engine
	^engine! !

!VectorCanvas methodsFor: 'context and parameters for path' stamp: 'jmv 12/10/2017 22:53:52'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	engine fillColor: aFillColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'context and parameters for path' stamp: 'jmv 12/30/2017 17:15:41'!
strokeWidth: morphStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"

	engine strokeWidth: morphStrokeWidth color: aStrokeColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'context and parameters for path' stamp: 'jmv 12/30/2017 17:15:57'!
strokeWidth: morphStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"

	engine strokeWidth: morphStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 14:20:49'!
closePath

	engine do_ClosePath! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:41:16'!
cubicBezier: points
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	| start control1 control2 end |
	start _ points first.
	control1 _ points second.
	control2 _ points third.
	end _ points fourth.
	engine abs_MoveToX: start x y: start y.
	engine
		abs_CubicBezierToX: end x y: end y
		control1X: control1 x y: control1 y
		control2X: control2 x y: control2 y! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:42:23'!
curveToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2

	engine abs_CubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:43:43'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry

	engine abs_EllipseCenterX: mcx y: mcy radiusX: mrx radiusY: mry! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:36:39'!
lineToX: x2 y: y2

	engine abs_LineToX: x2 y: y2! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:45:52'!
moveToX: x y: y

	engine abs_MoveToX: x y: y! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 12/31/2017 14:43:00'!
polyLine: points

	| p  |
	p _ points first.
	engine abs_MoveToX: p x y: p y.
	2 to: points size do: [ :i |
		p _ points at: i.	
		engine abs_LineToX: p x y: p y ]! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 12:23:51'!
quadraticBezier: points
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	| start control end |
	start _ points first.
	control _ points second.
	end _ points third.
	engine abs_MoveToX: start x y: start y.
	engine
		abs_QuadraticBezierToX: end x y: end y
		controlX: control x y: control y! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 14:26:38'!
rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax

	engine
		abs_MoveToX: xMin y: yMin;
		abs_LineToX: xMax y: yMin;
		abs_LineToX: xMax y: yMax;
		abs_LineToX: xMin y: yMax;
		do_ClosePath! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/5/2018 11:01:29'!
roundBottomRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p5 p6 p7 p8 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 _ xMin@yMin.
	corner2 _ xMax@yMin.
	corner3 _ xMax@yMax.
	corner4 _ xMin@yMax.
	dx _ mrx@0.
	dy _ 0@mry.
	p5 _ corner3 - dy.
	p6 _ corner3 - dx.
	p7 _ corner4 + dx.
	p8 _ corner4 - dy.
	"
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	doLargerArc _ false.
	useIncreasingAngles _ true.
	"
	engine
		abs_MoveToX: corner1 x y: corner1 y ;
		abs_LineToX: corner2 x y: corner2 y;
		abs_LineToX: p5 x y: p5 y;
"		abs_ArcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p6 x y: p6 y controlX: corner3 x y: corner3 y;
		abs_LineToX: p7 x y: p7 y;
"		abs_ArcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p8 x y: p8 y controlX: corner4 x y: corner4 y;
		do_ClosePath! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/5/2018 10:59:15'!
roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 p5 p6 p7 p8 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 _ xMin@yMin.
	corner2 _ xMax@yMin.
	corner3 _ xMax@yMax.
	corner4 _ xMin@yMax.
	dx _ mrx@0.
	dy _ 0@mry.
	p1 _ corner1 + dy.
	p2 _ corner1 + dx.
	p3 _ corner2 - dx.
	p4 _ corner2 + dy.
	p5 _ corner3 - dy.
	p6 _ corner3 - dx.
	p7 _ corner4 + dx.
	p8 _ corner4 - dy.
	"
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	doLargerArc _ false.
	useIncreasingAngles _ true.
	"
	engine
		abs_MoveToX: p1 x y: p1 y ;
"		abs_ArcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p2 x y: p2 y controlX: corner1 x y: corner1 y;
		abs_LineToX: p3 x y: p3 y;
"		abs_ArcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p4 x y: p4 y controlX: corner2 x y: corner2 y;
		abs_LineToX: p5 x y: p5 y;
"		abs_ArcToX: p6 x y: p6 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p6 x y: p6 y controlX: corner3 x y: corner3 y;
		abs_LineToX: p7 x y: p7 y;
"		abs_ArcToX: p8 x y: p8 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p8 x y: p8 y controlX: corner4 x y: corner4 y;
		do_ClosePath! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/5/2018 11:02:30'!
roundTopRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 _ xMin@yMin.
	corner2 _ xMax@yMin.
	corner3 _ xMax@yMax.
	corner4 _ xMin@yMax.
	dx _ mrx@0.
	dy _ 0@mry.
	p1 _ corner1 + dy.
	p2 _ corner1 + dx.
	p3 _ corner2 - dx.
	p4 _ corner2 + dy.
	"
	rx _ dx r.
	ry _ dy r.
	phi _ dx theta.
	doLargerArc _ false.
	useIncreasingAngles _ true.
	"
	engine
		abs_MoveToX: p1 x y: p1 y ;
"		abs_ArcToX: p2 x y: p2 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p2 x y: p2 y controlX: corner1 x y: corner1 y;
		abs_LineToX: p3 x y: p3 y;
"		abs_ArcToX: p4 x y: p4 y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles;"
		abs_QuadraticBezierToX: p4 x y: p4 y controlX: corner2 x y: corner2 y;
		abs_LineToX: corner3 x y: corner3 y;
		abs_LineToX: corner4 x y: corner4 y;
		do_ClosePath! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 12:37:30'!
multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless

	| to r |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				r _ rs at: i.
				engine abs_ArcToX: to x y: to y radiusX: r x radiusY: r y angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngless at: i) ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				r _ rs at: i.
				engine rel_ArcToX: to x y: to y radiusX: r x radiusY: r y angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngless at: i) ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 11:49:37'!
multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points

	| c1 c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c1 _ control1Points at: i.
				c2 _ control2Points at: i.
				engine abs_CubicBezierToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c1 _ control1Points at: i.
				c2 _ control2Points at: i.
				engine rel_CubicBezierToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 12:17:15'!
multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points

	| c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c2 _ control2Points at: i.
				engine abs_CubicBezierSmoothToX: to x y: to y control2X: c2 x y: c2 y ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c2 _ control2Points at: i.
				engine rel_CubicBezierSmoothToX: to x y: to y control2X: c2 x y: c2 y ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 11:52:20'!
multiLineTo: coordinatesAreRelative points: toPoints

	| to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				engine abs_LineToX: to x y: to y ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				engine rel_LineToX: to x y: to y ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 11:58:41'!
multiLineTo: coordinatesAreRelative xs: xs

	coordinatesAreRelative
		ifFalse: [
			xs do: [ :x |
				engine abs_LineToX: x ]]
		ifTrue: [
			xs do: [ :x |
				engine rel_LineToX: x y: 0 ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 11:59:13'!
multiLineTo: coordinatesAreRelative ys: ys

	coordinatesAreRelative
		ifFalse: [
			ys do: [ :y |
				engine abs_LineToY: y ]]
		ifTrue: [
			ys do: [ :y |
				engine rel_LineToX: 0 y: y ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 11:50:25'!
multiMoveTo: coordinatesAreRelative points: points

	| isFirst |
	isFirst _ true.
	coordinatesAreRelative
		ifFalse: [
			points do: [ :p |
				isFirst
					ifTrue: [ engine abs_MoveToX: p x y: p y ]
					ifFalse: [ engine abs_LineToX: p x y: p y ].
				isFirst _ false ]]
		ifTrue: [
			points do: [ :p |
				isFirst
					ifTrue: [ engine rel_MoveToX: p x y: p y ]
					ifFalse: [ engine rel_LineToX: p x y: p y ].
				isFirst _ false ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 12:24:56'!
multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints

	| c to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c _ controlPoints at: i.
				engine abs_QuadraticBezierToX: to x y: to y controlX: c x y: c y ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c _ controlPoints at: i.
				engine rel_QuadraticBezierToX: to x y: to y controlX: c x y: c y ]]! !

!VectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 1/1/2018 12:29:49'!
multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints

	| to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				engine abs_QuadraticBezierSmoothToX: to x y: to y ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				engine rel_QuadraticBezierSmoothToX: to x y: to y ]]! !

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 8/17/2018 14:54:43'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| font loc ps |
"	port colorMap: nil."
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	"mhhhhhhhh. Creo que es mejor que venga un font siempre. Y uno de M3!!"
	font _ fontOrNil ifNil: [ AbstractFont default ].
	"do it better!!"
	font class = StrokeFont ifTrue: [
		ps _ font pointSize.
		loc _ AffineTransformation withScale: ps.
		loc _ currentTransformation composedWith: loc.
		engine geometryTransformation: loc.
		engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint / ps
			strokeFont: font
			color: aColor.
		engine geometryTransformation: currentTransformation ]
	ifFalse: [
		ps _ font pointSize.
		loc _ AffineTransformation withScale: ps*1.3.
		loc _ currentTransformation composedWith: loc.
		engine geometryTransformation: loc.
		engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint / ps
			trueTypeFont: font
			color: aColor.
		engine geometryTransformation: currentTransformation ]! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/30/2014 20:02'!
drawButtonIconFromCurrentMorph

	currentMorph iconName ifNotNil: [ :selector |
		self perform: selector.
		^true ].
	^false! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:09:08'!
drawCloseIcon

self strokeWidth: 1.0 color: Color black do: [
	self ellipseCenterX: 9 y:  9 rx: 6 ry: 6 ]! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:06:50'!
drawCollapseIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:06:55'!
drawDownIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:06:59'!
drawExpandIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:07:03'!
drawLeftIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:07:08'!
drawMenuIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:07:16'!
drawRightIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 1/4/2018 09:07:20'!
drawUpIcon

self drawCloseIcon! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/11/2017 21:15:33'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: morphStrokeWidth borderColor: strokeColorOrGradient fillColor: fillColorOrGradient

	
	self strokeWidth: morphStrokeWidth color: strokeColorOrGradient fillColor: fillColorOrGradient do: [
		self ellipseCenterX: mcx y: mcy rx: mrx ry: mry ]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/10/2017 22:59:51'!
fillRectangle: aRectangle color: aColor
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"

	self fillColor: aColor do: [
		self rectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 ]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/22/2017 12:24:56'!
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
			self moveToX: aRectangle left + d1 y: aRectangle bottom - d2.
			self lineToX: aRectangle right - d2 y: aRectangle bottom - d2.
			self lineToX: aRectangle right - d2 y: aRectangle top + d1 ]
	]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self halt! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/11/2017 20:57:09'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	| d1 d2 |
	d1 _ borderWidth*0.5-0.5.
	d2 _ borderWidth*0.5+0.5.

	self strokeWidth: borderWidth color: borderColor do: [
		self rectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+d1 maxY: aRectangle bottom-d2 ]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 10/9/2014 09:51'!
image: aForm at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
self flag: #jmvVer4.
" 	self halt"
"super image: aForm at: aPoint"! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
image: aForm multipliedBy: aColor at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
"no!!!!!!!!"
"	self halt"
super image: aForm multipliedBy: aColor at: aPoint! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/22/2017 12:20:56'!
line: pt1 to: pt2 width: morphStrokeWidth color: aStrokeColor

	self strokeWidth: morphStrokeWidth color: aStrokeColor do: [
		self moveToX: pt1 x y: pt1 y.
		self lineToX: pt2 x y: pt2 y ]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
reverseRectangleBorder: r borderWidth: borderWidth
	self halt! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/11/2017 21:10:33'!
roundRect: aRectangle color: aColor radius: r
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 rx: r ry: r ]! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self halt! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self halt! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
stencil: stencilForm at: aPoint color: aColor
	self halt! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc
"ojo que es un quad, o sea un rect en coordenadas del morph..."
"	self halt"
super textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc! !

!VectorCanvas methodsFor: 'drawing - FormCanvas protocol' stamp: 'jmv 12/16/2017 18:32:06'!
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

!VectorCanvas methodsFor: 'testing' stamp: 'jmv 5/30/2017 15:28:19'!
isCurrentMorphVisible
	self flag: #jmvVer2. "Also #morphBoundsInWorld"
self revisar. "por supuesto que no..."
	^ true! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 10/14/2014 09:00'!
clippingRectForCurrentMorph
	"In form coordinates"

	^engine clippingRectForCurrentMorph! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 10/14/2014 16:17'!
currentOwnerIfClips: currentMorphOwnerOrNil
	engine currentOwnerIfClips: currentMorphOwnerOrNil! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 12/31/2017 13:22:48'!
initializeWith: aForm origin: aPoint
	super initializeWith: aForm origin: aPoint.
	engine geometryTransformation: currentTransformation! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 8/16/2018 15:37:39'!
into: aMorph
	super into: aMorph.
	engine currentMorph: currentMorph.
	engine geometryTransformation: currentTransformation.
	engine resetMorphBounds.! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 8/16/2018 15:37:22'!
outOfMorph
	super outOfMorph.
	engine currentMorph: currentMorph.
	engine geometryTransformation: currentTransformation! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 10/13/2014 16:17'!
setClipRect: aRectangle
	"In form coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 4/4/2018 14:04:17'!
setForm: aForm
	"
	World extent: World extent
	"
	form _ aForm.
	engine _ VectorEngine onForm: form.
	self newClipRect: nil! !

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

!VectorCanvas class methodsFor: 'BitBltCanvas protocol' stamp: 'jmv 10/9/2014 23:04'!
steButtonForm: extent
	self flag: #jmvVer2.! !

!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 1/21/2015 08:41'!
initialize
	"
	VectorCanvas initialize
	0.33: r - b
	0.1: b - r
	"
	" AffineTransformation doIntegerTransformations: false"! !

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

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
antiAliasingWidth: aNumber

	antiAliasingWidth _ aNumber! !

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

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
currentMorph: aMorph
	currentMorph _ aMorph! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/16/2017 18:17:50'!
currentOwnerIfClips: currentMorphOwnerOrNil
	currentOwnerIfClips _ currentMorphOwnerOrNil! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 8/17/2018 15:42:34'!
geometryTransformation
	^geometryTransformation! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 12/31/2017 13:22:19'!
geometryTransformation: aGeometryTransformation
	geometryTransformation _ aGeometryTransformation! !

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

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 12/31/2017 14:11:22'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	| strokeAlphaFactor aStrokeWidth |
	doStroke _ true.
	doFill _ false.
	aStrokeWidth _ geometryTransformation externalizeScalar: clientStrokeWidth.

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

!VectorEngine methodsFor: 'stroke & fill' stamp: 'jmv 1/4/2018 09:44:33'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	| aStrokeWidth |

	aStrokeColor ifNil: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	clientStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aFillColor ifNil: [ ^ self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	aStrokeWidth _ geometryTransformation externalizeScalar: clientStrokeWidth.
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock.
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

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 12/31/2017 16:46:16'!
abs_ArcToX: cx y: cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.2 position: 200@100);
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"

	| center dx dy phi rx ry |
	center _ geometryTransformation externalizePosition: cx@cy.
	dx _ geometryTransformation externalizeDelta: mrx@0.
	dy _ geometryTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ geometryTransformation radians + theta.
	self basicArcToX: center x y: center y radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 14:19:15'!
abs_CircleCenterX: cx y: cy radius: mr
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_CircleCenterX: 200 y: 150 radius: 80].
Display forceToScreen
"

	| center r |
	center _ geometryTransformation externalizePosition: cx@cy.
	r _ geometryTransformation externalizeScalar: mr.
	self
		basicArcCenterX: center x centerY: center y radiusX: r radiusY: r rotationAngle: 0.0 start: 0.0 sweep:  Float pi * 2;
		basicClosePath! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/4/2018 16:30:44'!
abs_CubicBezierSmoothToX: x3 y: y3 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 150 y: 300 radius: 10.
		engine abs_CircleCenterX: 600 y: 400 radius: 6.
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100 control1X: 150 y: 300 control2X: 600 y: 400
			].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 600 y: -200 radius: 10.
		engine abs_CircleCenterX: 100 y: -100 radius: 6.
		engine abs_MoveToX: 600 y: 100.
		engine
			abs_CubicBezierToX: 300 y: 100 control1X: 600 y: -200 control2X: 100 y: -100
			].
Display forceToScreen.
	"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100 control1X: 150 y: 300 control2X: 600 y: 400;
			abs_CubicBezierSmoothToX: 300 y: 100 control2X: 100 y: -100
			].
Display forceToScreen
	"
	| mirroredContolX mirroredContolY |
	mirroredContolX _ lastClientX + (lastClientX - lastControlX).
	mirroredContolY _ lastClientY + (lastClientY - lastControlY).
	self abs_CubicBezierToX: x3 y: y3 control1X: mirroredContolX y: mirroredContolY control2X: x2 y: y2! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 21:00:50'!
abs_CubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ].
Display forceToScreen
	"
	| p1 p2 p3 |
	p1 _ geometryTransformation externalizePosition: x1@y1.
	p2 _ geometryTransformation externalizePosition: x2@y2.
	p3 _ geometryTransformation externalizePosition: x3@y3.
	self basicCubicBezierToX: p3 x y: p3 y control1X: p1 x y: p1 y control2X: p2 x y: p2 y.
	lastClientX _ x3.
	lastClientY _ y3.
	lastControlX _ x2.
	lastControlY _ y2! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 14:19:19'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50].
Display forceToScreen
"

	| center dx dy phi rx ry |
	center _ geometryTransformation externalizePosition: cx@cy.
	dx _ geometryTransformation externalizeDelta: mrx@0.
	dy _ geometryTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ geometryTransformation radians.
	self
		basicArcCenterX: center x centerY: center y radiusX: rx radiusY: ry rotationAngle: phi start: 0.0 sweep:  Float pi * 2;
		basicClosePath! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 14:19:21'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry rotationAngle: theta
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: -0.5 ].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: -0.5 ].
Display forceToScreen
"

	| center dx dy phi rx ry |
	center _ geometryTransformation externalizePosition: cx@cy.
	dx _ geometryTransformation externalizeDelta: mrx@0.
	dy _ geometryTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ geometryTransformation radians + theta.
	self
		basicArcCenterX: center x centerY: center y radiusX: rx radiusY: ry rotationAngle: phi start: 0.0 sweep:  Float pi * 2;
		basicClosePath! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 12/31/2017 16:37:59'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 350 radiusX: 100 radiusY: 50 rotationAngle: 0.5 start: 0.1 sweep: 6 ].
Display forceToScreen.
"

	| center dx dy phi rx ry |
	center _ geometryTransformation externalizePosition: cx@cy.
	dx _ geometryTransformation externalizeDelta: mrx@0.
	dy _ geometryTransformation externalizeDelta: 0@mry.
	rx _ dx r.
	ry _ dy r.
	phi _ geometryTransformation radians + theta.
	self basicArcCenterX: center x centerY: center y radiusX: rx radiusY: ry rotationAngle: phi start: startAngle sweep:  sweepAngle! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 20:54:08'!
abs_LineToX: x2
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 50.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_LineToX: x2 y: lastClientY! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 21:02:04'!
abs_LineToX: x2 y: y2
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	| p2 |
	p2 _ geometryTransformation externalizePosition: x2@y2.
	self basicLineToX: p2 x y: p2 y.
	lastClientX _ x2.
	lastClientY _ y2.
	lastControlX _ lastClientX.
	lastControlY _ lastClientY! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 20:52:47'!
abs_LineToY: y2
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToY: 200 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToY: 50.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_LineToX: lastClientX y: y2! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 21:02:08'!
abs_MoveToX: x y: y
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	| p |
	p _ geometryTransformation externalizePosition: x@y.
	self basicMoveToX: p x y: p y.
	lastClientX _ x.
	lastClientY _ y.
	lastControlX _ lastClientX.
	lastControlY _ lastClientY! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/4/2018 16:40:01'!
abs_QuadraticBezierSmoothToX: x2 y: y2
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 450 y: 300 radius: 10.
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300
			].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 750 y: -100 radius: 10.
		engine abs_MoveToX: 600 y: 100.
		engine
			abs_QuadraticBezierToX: 300 y: 100 controlX: 750 y: -100
			].
Display forceToScreen.
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300;
			abs_QuadraticBezierSmoothToX: 300 y: 100
			].
Display forceToScreen
"
	| mirroredContolX mirroredContolY |
	mirroredContolX _ lastClientX + (lastClientX - lastControlX).
	mirroredContolY _ lastClientY + (lastClientY - lastControlY).
	self abs_QuadraticBezierToX: x2 y: y2 controlX: mirroredContolX y: mirroredContolY! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 21:00:58'!
abs_QuadraticBezierToX: x2 y: y2 controlX: x1 y: y1
	"
(VectorEngine onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ].
Display forceToScreen
	"
	| p1 p2 |
	p1 _ geometryTransformation externalizePosition: x1@y1.
	p2 _ geometryTransformation externalizePosition: x2@y2.
	self basicQuadraticBezierToX: p2 x y: p2 y controlX: p1 x y: p1 y.
	lastClientX _ x2.
	lastClientY _ y2.
	lastControlX _ x1.
	lastControlY _ y1! !

!VectorEngine methodsFor: 'paths - absolute coordinates' stamp: 'jmv 1/1/2018 21:02:11'!
do_ClosePath
	"abs/rel meaningless here"
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200;
			abs_CubicBezierToX: 300 y: 120 control1X: 310 y: 110 control2X: 200 y: 200;
			do_ClosePath;
			abs_LineToX: 150 y: 300 ].
Display forceToScreen
"
	| p2 |
	self basicClosePath.

	p2 _ geometryTransformation internalizePosition: currentX@currentY.
	lastClientX _ p2 x.
	lastClientY _ p2 y.
	lastControlX _ lastClientX.
	lastControlY _ lastClientY! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:54:10'!
rel_ArcToX: cx y: cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			rel_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false;
			do_ClosePath ].
"
	self abs_ArcToX: lastClientX + cx y: lastClientY + cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:53:39'!
rel_CubicBezierSmoothToX: x3 y: y3 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100 control1X: 150 y: 300 control2X: 600 y: 400;
			rel_CubicBezierToX: 300 y: 300 control1X: 0 y: 0 control2X: 100 y: 100
			].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100 control1X: 150 y: 300 control2X: 600 y: 400;
			rel_CubicBezierSmoothToX: 300 y: 300 control2X: 100 y: 100
			].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierSmoothToX: 600 y: 100 control2X: 600 y: 400;
			abs_CubicBezierSmoothToX: 300 y: 300 control2X: 100 y: 100
			].
Display forceToScreen
	"

	self
		abs_CubicBezierSmoothToX: lastClientX + x3 y: lastClientY + y3
		control2X: lastClientX + x2 y: lastClientY + y2! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:54:02'!
rel_CubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ];
	strokeWidth: 1.0 color: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			rel_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ];
	strokeWidth: 10 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			rel_CubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ].
Display forceToScreen
	"

	self
		abs_CubicBezierToX: lastClientX + x3 y: lastClientY + y3
		control1X: lastClientX + x1 y: lastClientY + y1
		control2X: lastClientX + x2 y: lastClientY + y2! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:53:34'!
rel_LineToX: x2 y: y2
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150];
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			rel_LineToX: 200 y: 150].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ];
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine rel_LineToX: 15 y: 30.
		engine rel_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_LineToX: lastClientX + x2 y: lastClientY + y2! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:53:32'!
rel_MoveToX: x y: y
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150];
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			rel_LineToX: 200 y: 150].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ];
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine rel_LineToX: 15 y: 30.
		engine rel_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_MoveToX: lastClientX + x y: lastClientY + y! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/4/2018 16:47:43'!
rel_QuadraticBezierSmoothToX: x2 y: y2
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 550 y: 350 radius: 10.
		engine abs_MoveToX: 100 y: 50.
		engine
			rel_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300
			].
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 850 y: -50 radius: 10.
		engine abs_MoveToX: 700 y: 150.
		engine
			rel_QuadraticBezierToX: -300 y: -50 controlX: 150 y: -200
			].
Display forceToScreen.
"
"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			rel_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300;
			rel_QuadraticBezierSmoothToX: -300 y: -50
			].
Display forceToScreen
"
	self
		abs_QuadraticBezierSmoothToX: lastClientX + x2 y: lastClientY + y2! !

!VectorEngine methodsFor: 'paths - relative coordinates' stamp: 'jmv 1/1/2018 20:53:56'!
rel_QuadraticBezierToX: x2 y: y2 controlX: x1 y: y1
	"
(VectorEngine onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ];
	strokeWidth: 3.0 color: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			rel_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ];
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color green alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine rel_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ].
Display forceToScreen
	"
	self
		abs_QuadraticBezierToX: lastClientX + x2 y: lastClientY + y2
		controlX: lastClientX + x1 y: lastClientY + y1! !

!VectorEngine methodsFor: 'text' stamp: 'jmv 4/4/2018 15:11:43'!
displayString: aString from: startIndex to: stopIndex at: aPoint strokeFont: aFont color: aStrokeColor
	"geometryTransformation is already set to honor font pointSize
	Use as little of aFont as possible!!"
	| charCode destX destY displayWidth isBold isItalic char renderer |

	self antiAliasingWidth: VectorEngine antiAliasingWidthForStrokeFonts.
	destY _ aPoint y.
	destX _ aPoint x.
	isBold _ aFont isBold.
	isItalic _ aFont isItalic.

	renderer _ TechnicalFontRenderer for: self.
	self strokeWidth: (isBold ifTrue: [ 0.15 ] ifFalse: [ 0.1 ]) color: aStrokeColor do: [
		startIndex to: stopIndex do: [ :charIndex |
			char _ aString at: charIndex.
			charCode _ char numericValue.
			displayWidth _ aFont normalizedWidthOf: char.
			self abs_MoveToX: destX y: destY+1.
			renderer drawCharCode: charCode italics: isItalic.
			destX _ destX + displayWidth.
		]
	].
	self antiAliasingWidth: VectorEngine defaultAntiAliasingWidth! !

!VectorEngine methodsFor: 'text' stamp: 'jmv 4/4/2018 15:11:30'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aFont color: aStrokeColor
	"geometryTransformation is already set to honor font pointSize
	Use as little of aFont as possible!!"
	| charCode destX destY displayWidth isBold isItalic char renderer |

	self antiAliasingWidth: VectorEngine antiAliasingWidthForOutlineFonts.
	destY _ aPoint y.
	destX _ aPoint x.
	isBold _ aFont isBold.
	isItalic _ aFont isItalic.

	renderer _ BitstreamVeraRenderer for: self.
	self strokeWidth: (isBold ifTrue: [0.062] ifFalse: [0]) color: aStrokeColor fillColor: aStrokeColor do: [
		startIndex to: stopIndex do: [ :charIndex |
			char _ aString at: charIndex.
			charCode _ char numericValue.
			displayWidth _ aFont normalizedWidthOf: char.
			self abs_MoveToX: destX/1.3 y: destY/1.3.
			renderer drawCharCode: charCode italics: isItalic.
			destX _ destX + displayWidth.
		]
	].
	self antiAliasingWidth: VectorEngine defaultAntiAliasingWidth! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/26/2017 09:27:29'!
addPenPositionX: x y: y
	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"

	trajectoryPoints add: x@y! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 1/1/2018 21:06:51'!
finishPath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| prevTruncatedY dilatedHalfWidth fragmentEndIndex pp |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	doFill ifTrue: [
		(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
			"This is actually an error condition. If we are asked to fill, then the path should be closed.
			This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
			self basicClosePath ]].

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

	trajectoryPoints do: [ :p | self updateMaskForX: p x y: p y ].

	firstX _ 0.0.
	firstY _ 0.0.
	currentX _ 0.0.
	currentY _ 0.0.
	lastClientX _ 0.0.
	lastClientY _ 0.0.
	lastControlX _ lastClientX.
	lastControlY _ lastClientY.
	trajectoryPoints _ OrderedCollection new.
	fragmentStartIndexes _ OrderedCollection new.
	
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

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/30/2017 15:48:32'!
initialize
	geometryTransformation _ MorphicTranslation new. "Identity"! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 1/1/2018 21:06:43'!
initializePath

	firstX _ 0.0.
	firstY _ 0.0.
	currentX _ 0.0.
	currentY _ 0.0.
	lastClientX _ 0.0.
	lastClientY _ 0.0.
	lastControlX _ lastClientX.
	lastControlY _ lastClientY.
	trajectoryPoints _ OrderedCollection new.
	fragmentStartIndexes _ OrderedCollection new! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 12/16/2017 18:17:50'!
resetMorphBounds

	morphBoundsTop _ targetHeight - 1.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ targetWidth - 1.
	morphBoundsRight _ 0! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 4/4/2018 15:12:09'!
setForm: aForm
	"
	World extent: World extent
	"
	| hack s |
	targetForm _ aForm.
	targetWidth _ aForm width.
	targetHeight _ aForm height.
	self resetMorphBounds.
	self clipRect: (0@0 corner: targetWidth@targetHeight).
	antiAliasingWidth _ VectorEngine defaultAntiAliasingWidth.
	subPixelDelta _ 0.25. "Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"
	s _ aForm bits size.
	(mask isNil or: [ mask size ~= s ])
		ifTrue: [
			mask _ WordArray new: s.
			edgeCountPerPixel _ WordArray new: s.
			morphAtPixel _ Array new: s.
			hack _ Form extent: targetWidth@targetHeight depth: 32 bits: mask.
			auxBitBlt _ BitBlt toForm: hack.
			auxBitBlt fillBitmap: (Bitmap with: 16rFF000000).
			auxBitBlt combinationRule: Form over ].

	"Perhaps this should be done in an ifCurtailed: block in case of Morphic drawing failure..."
	auxBitBlt destRect: targetForm boundingBox.
	auxBitBlt destForm bits: mask.
	auxBitBlt copyBits.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	morphAtPixel atAllPut: 0! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/4/2018 16:18:33'!
basicArcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngine onForm: Display)
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine basicArcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
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
	(startAngle ~= 0.0 or: [ sweepAngle ~= Float twoPi ])
		ifTrue: [ self basicLineToX: x0 y: y0 ]	"Draw a line. There are cases where we want to join an arc. For example, window frames"
		ifFalse: [
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
	currentY _ y! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 12/31/2017 16:42:44'!
basicArcToX: x2 y: y2 radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			basicMoveToX: 100 y: 100;
			basicArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			basicMoveToX: 100 y: 100;
			basicArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"
	"Calcular: cx, cy, rx, ry, phi, theta1, deltaTheta"
	| x1 y1 cosPhi sinPhi x12hd y12hd x1p y1p rxSq rySq x1pSq y1pSq bigSqR bigSqRNumerator bigSqRDenominator sign cxp cyp cx cy theta1 theta2 deltaTheta scaleSq scale newRx newRy |
	x1 _ currentX.
	y1 _ currentY.
	"Handle out of range parameters as requested by the w3 implementation notes"
	(x2 = x1 and: [ y2 = y1 ]) ifTrue: [ ^self ].
	(rx = 0.0 or: [ ry = 0.0 ]) ifTrue: [ ^self basicLineToX: x2 y: y2 ].
	rx < 0 ifTrue: [
		^self basicArcToX: x2 y: y2 radiusX: 0.0 -rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].
	ry < 0 ifTrue: [
		^self basicArcToX: x2 y: y2 radiusX: rx radiusY: 0.0-ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].
	
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
		^self basicArcToX: x2 y: y2 radiusX: newRx radiusY: newRy angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
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
	

	self basicArcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: phi start: theta1 sweep: deltaTheta! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/1/2018 16:11:54'!
basicClosePath
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			do_ClosePath ].
Display forceToScreen
"
	self basicLineToX: firstX y: firstY! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/1/2018 17:21:18'!
basicCubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100
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
	currentY _ y3.! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/1/2018 17:21:04'!
basicLineToX: x2 y: y2
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ].
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
	currentY _ y2.! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/1/2018 17:21:09'!
basicMoveToX: x y: y
"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150;
			abs_MoveToX: 210 y: 150;
			abs_LineToX: 300 y: 200 ].
Display forceToScreen
"

	"A discontinuity in the trajectory"

	"We are starting a new trajectory, or at a discontinuity in trajectory. Eventually #closeTrajectory should draw a line to here"
	fragmentStartIndexes add: trajectoryPoints size + 1.	"Next point will be first in a new trajectory fragment"
	firstX _ x.
	firstY _ y.
	currentX _ x.
	currentY _ y.! !

!VectorEngine methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 1/1/2018 17:21:01'!
basicQuadraticBezierToX: x2 y: y2 controlX: x1 y: y1
	"
(VectorEngine onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ].
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
	currentY _ y2.! !

!VectorEngine methodsFor: 'private - rasterization' stamp: 'jmv 12/22/2017 11:55:25'!
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

!VectorEngine methodsFor: 'private - rasterization' stamp: 'jmv 12/22/2017 12:01:07'!
updateMaskForX: x y: y

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
			alphaWord _ mask bytesAt: pixelIndex.
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
					mask bytesAt: pixelIndex put: alphaWord ]]]]! !

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:28'!
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
	auxBitBlt destForm bits: mask.
	auxBitBlt copyBits.
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:41'!
blendFillOnlyAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends fill over background. Despite method selector, there might be a very thin stroke of the same color as the fill. Handle this by adjusting antiAliasAlphas.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha fillAlphaToUse |

	antiAliasAlphasWord _ mask bytesAt: pixelIndex.
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

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:20'!
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
	auxBitBlt destForm bits: mask.
	auxBitBlt copyBits.
	auxBitBlt
		destX: boundsLeft destY: boundsTop
		width: boundsRight-boundsLeft+1+1 height: boundsBottom-boundsTop+1.
	auxBitBlt destForm bits: edgeCountPerPixel.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:31'!
blendStrokeAndFillAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Do an appropriate (anti aliased) mix between stoke and fill (or pick just stroke or just fill). Blend this over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha foreComponent strokeAlphaToUse fillAlphaToUse |

	antiAliasAlphasWord _ mask bytesAt: pixelIndex.
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

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:33'!
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
	auxBitBlt destForm bits: mask.
	auxBitBlt copyBits.
	Display forceToScreen! !

!VectorEngine methodsFor: 'private - blend with mask' stamp: 'jmv 12/22/2017 11:59:36'!
blendStrokeOnlyAtX: x y: y pixelIndex: pixelIndex

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord bits resultR resultG resultB antiAliasAlphasWord antiAliasRedAlpha antiAliasGreenAlpha antiAliasBlueAlpha alpha strokeAlphaToUse |

	antiAliasAlphasWord _ mask bytesAt: pixelIndex.
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

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/4/2018 15:12:52'!
experLinesAndShapes
	"
	self experLinesAndShapes
	"
	| engine y1 yy2 dy y2 |
	engine _ self onForm: Display.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	Display forceToScreen.
	engine strokeWidth: VectorEngine defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorEngine defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		yy2 _ y2 >= 1 ifTrue: [ VectorEngine defaultAntiAliasingWidth * 0.5 ] ifFalse: [0].
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 431 y: y1+yy2; abs_LineToX: 530 y: y1+yy2+dy;
				abs_LineToX: 530 y: y1+y2-yy2+dy; abs_LineToX: 431 y: y1+y2-yy2;
				do_ClosePath.
			engine
				abs_MoveToX:  y1+yy2 y: 431; abs_LineToX: y1+yy2+dy y: 530;
				abs_LineToX: y1+y2-yy2+dy y: 530; abs_LineToX: y1+y2-yy2 y: 431;
				do_ClosePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 531 y: y1+dy; abs_LineToX: 630 y: y1+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy; abs_LineToX: 531 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 531; abs_LineToX: y1+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy y: 630; abs_LineToX: y1+y2+dy y: 531;
				do_ClosePath ].
		"Solo fill."
		engine fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy y: 631;
				do_ClosePath ].
		"solo stroke, no fill"
		engine strokeWidth: y2 color: Color black do: [
			engine
				abs_MoveToX: y2*0.5+731 y: y2*0.5 + y1+dy+dy+dy; abs_LineToX: 830 y: y2 * 0.5 + y1  + dy + dy+dy+dy.
			engine
				abs_MoveToX: y2*0.5 + y1+dy+dy+dy y: y2*0.5+731; abs_LineToX: y2 * 0.5 + y1  + dy + dy+dy+dy y: 830 ].
		y1 _ y1 + y2 + VectorEngine defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/4/2018 15:12:56'!
experLinesAndShapes2
	"
	self experLinesAndShapes2
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorEngine defaultAntiAliasingWidth*4*2.
	15 timesRepeat: [
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 1.0 color: Color green fillColor: Color red do: [
			engine
				abs_MoveToX: 131 y: y1; abs_LineToX: 230 y: y1+dy;
				abs_LineToX: 230 y: y1+y2+dy; abs_LineToX: 131 y: y1+y2;
				do_ClosePath.
			engine
				abs_MoveToX: y1 y: 131; abs_LineToX: y1+dy y: 230;
				abs_LineToX: y1+y2+dy y: 230; abs_LineToX: y1+y2 y: 131;
				do_ClosePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.8 color: Color green fillColor: Color red do: [
			engine
				abs_MoveToX: 231 y: y1+dy; abs_LineToX: 330 y: y1+dy+dy;
				abs_LineToX: 330 y: y1+y2+dy+dy; abs_LineToX: 231 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 231; abs_LineToX: y1+dy+dy y: 330;
				abs_LineToX: y1+y2+dy+dy y: 330; abs_LineToX: y1+y2+dy y: 231;
				do_ClosePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.6 color: Color green fillColor: Color red do: [.
			engine
				abs_MoveToX: 331 y: y1+dy+dy; abs_LineToX: 430 y: y1+dy+dy+dy;
				abs_LineToX: 430 y: y1+y2+dy+dy+dy; abs_LineToX: 331 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 331; abs_LineToX: y1+dy+dy+dy y: 430;
				abs_LineToX: y1+y2+dy+dy+dy y: 430; abs_LineToX: y1+y2+dy+dy y: 331;
				do_ClosePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.4 color: Color green fillColor: Color red do: [
			engine
				abs_MoveToX: 431 y: y1+dy+dy+dy; abs_LineToX: 530 y: y1+dy+dy+dy+dy;
				abs_LineToX: 530 y: y1+y2+dy+dy+dy+dy; abs_LineToX: 431 y: y1+y2+dy+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy+dy y: 431; abs_LineToX: y1+dy+dy+dy+dy y: 530;
				abs_LineToX: y1+y2+dy+dy+dy+dy y: 530; abs_LineToX: y1+y2+dy+dy+dy y: 431;
				do_ClosePath ].
		"Solo fill. w chico pero aun visible"
		engine strokeWidth: 0.2 color: Color green fillColor: Color red do: [
			engine
				abs_MoveToX: 531 y: y1+dy+dy+dy+dy; abs_LineToX: 630 y: y1+dy+dy+dy+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy+dy+dy+dy; abs_LineToX: 531 y: y1+y2+dy+dy+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy+dy+dy y: 531; abs_LineToX: y1+dy+dy+dy+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy+dy+dy+dy y: 630; abs_LineToX: y1+y2+dy+dy+dy+dy y: 531;
				do_ClosePath ].
		"Solo fill. w chico. Preferimos ya usarlo para definir mejor el shape (se transparentaria demasiado)"
		engine strokeWidth: 0.0 color: Color green fillColor: Color red do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy+dy+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy+dy+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy+dy+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy+dy+dy+dy y: 631;
				do_ClosePath ].
		y1 _ y1 + y2 + VectorEngine defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/4/2018 15:13:02'!
experLinesAndShapes3
	"
	self experLinesAndShapes3
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	Display forceToScreen.
	engine strokeWidth: VectorEngine defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorEngine defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth*0.4 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 431 y: y1; abs_LineToX: 530 y: y1+dy;
				abs_LineToX: 530 y: y1+y2+dy; abs_LineToX: 431 y: y1+y2;
				do_ClosePath.
			engine
				abs_MoveToX:  y1 y: 431; abs_LineToX: y1+dy y: 530;
				abs_LineToX: y1+y2+dy y: 530; abs_LineToX: y1+y2 y: 431;
				do_ClosePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth*0.2 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 531 y: y1+dy; abs_LineToX: 630 y: y1+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy; abs_LineToX: 531 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 531; abs_LineToX: y1+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy y: 630; abs_LineToX: y1+y2+dy y: 531;
				do_ClosePath ].
		"Solo fill. w =0"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth*0.0 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy y: 631;
				do_ClosePath ].
		y1 _ y1 + y2 + VectorEngine defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/4/2018 15:13:08'!
experLinesAndShapes4
	"
	self experLinesAndShapes4
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	Display forceToScreen.
	engine strokeWidth: VectorEngine defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorEngine defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth*2 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 431 y: y1; abs_LineToX: 530 y: y1+dy;
				abs_LineToX: 530 y: y1+y2+dy; abs_LineToX: 431 y: y1+y2;
				do_ClosePath.
			engine
				abs_MoveToX:  y1 y: 431; abs_LineToX: y1+dy y: 530;
				abs_LineToX: y1+y2+dy y: 530; abs_LineToX: y1+y2 y: 431;
				do_ClosePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth*1.5 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 531 y: y1+dy; abs_LineToX: 630 y: y1+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy; abs_LineToX: 531 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 531; abs_LineToX: y1+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy y: 630; abs_LineToX: y1+y2+dy y: 531;
				do_ClosePath ].
		"Solo fill. w =0"
		engine strokeWidth: VectorEngine defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy y: 631;
				do_ClosePath ].
		y1 _ y1 + y2 + VectorEngine defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/4/2018 15:12:44'!
experLinesAndShapesx
	"
	self experLinesAndShapesx
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ VectorEngine defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		y2 print.
		"Solo fill. w =0"
		engine strokeWidth: 0.0 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 431; abs_LineToX: y1+dy+dy+dy y: 530;
				abs_LineToX: y1+y2+dy+dy+dy y: 530; abs_LineToX: y1+y2+dy+dy y: 431;
				do_ClosePath ].
		y1 _ y1 + y2 + VectorEngine defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/1/2018 16:23:28'!
experLinesAndShapesx2
	"
	self experLinesAndShapesx2
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.
	y1 _ 10.
	dy _ 3.
	y2 _ 0.1.
	y2 print.
	"Solo fill. w =0"
	engine strokeWidth: 0.5 color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 10 y: y1+dy+dy; abs_LineToX: 200 y: y1+dy+dy+dy;
			abs_LineToX: 200 y: y1+y2+dy+dy+dy; abs_LineToX: 10 y: y1+y2+dy+dy;
			do_ClosePath.
		engine
			abs_MoveToX: y1+dy+dy y: 10; abs_LineToX: y1+dy+dy+dy y: 200;
			abs_LineToX: y1+y2+dy+dy+dy y: 200; abs_LineToX: y1+y2+dy+dy y: 10;
			do_ClosePath ].
	Display forceToScreen.
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/1/2018 16:23:33'!
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
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) *10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/1/2018 16:23:38'!
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
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			abs_MoveToX: 40 y: 30;
			abs_LineToX: 190 y: 135; abs_LineToX: 40 y: 40;
			abs_LineToX: 135 y: 390; abs_LineToX: 30 y: 40;
			do_ClosePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 40 y: 30;
			abs_LineToX: 190 y: 135; abs_LineToX: 40 y: 40;
			abs_LineToX: 135 y: 390; abs_LineToX: 30 y: 40;
			do_ClosePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) * 10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/1/2018 16:23:42'!
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
	engine fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops1.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ VectorEngine onForm: f2.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f2 displayAt: 220@0.

	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 440@0.
	d _ (FloatMatrix form: f1 color: #red) - (FloatMatrix form: f2 color: #red) * 10 + 0.5.
	(d asFormMin: 0 max: 1) displayAt: 660@0.
	{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 1/1/2018 16:23:50'!
experiment1
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	self experiment1
	"
| engine h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
engine _ self onForm: Display.
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
					abs_MoveToX: x1 y: y;
					abs_LineToX: x2 y: y;
					abs_LineToX: x2p y: prevY;
					abs_LineToX: x1p y: prevY;
					do_ClosePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
"
	canvas removeSpuriousTranslucencyFloatAndBlendOver: Color red.
	f writePNGfileNamed: 'pp17.png'
"! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 1/1/2018 16:23:54'!
experiment2
	"
	self experiment2
	"
| engine h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
engine _ self onForm: Display.
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
					abs_MoveToX: x1 y: y;
					abs_LineToX: x2 y: y;
					abs_LineToX: x2p y: prevY;
					abs_LineToX: x1p y: prevY;
					do_ClosePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 12/31/2017 15:23:12'!
experiment3
	"
	self experiment3
	"
	| engine w r |
	engine _ self onForm: Display.
	w _ 10.
	r _ 15.
	100 timesRepeat: [
		engine strokeWidth: w color: Color black do: [
			engine abs_CircleCenterX: 300 y: 300 radius: r ].
		r _ r + w.
		w _ w * 0.8.
		r _ r + w.

	Display forceToScreen.
	].
Display forceToScreen! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 1/1/2018 16:14:38'!
experiment4
	"
	self experiment4
	"
	| engine w r |
	engine _ self onForm: Display.
	w _ 10.
	r _ 15.
	1 to: 100 do: [ :i |
		r _ r + w.
		w _ 20.0 / i.
		r _ r + w.
		engine strokeWidth: w color: Color black do: [
			engine abs_CircleCenterX: 300 y: 300 radius: r ].
	Display forceToScreen.
	].
Display forceToScreen! !

!VectorEngine class methodsFor: 'samples' stamp: 'jmv 8/16/2018 15:47:00'!
siemensStar
	"
	VectorEngine siemensStar display
	"
	| angle backColor canvas count extent foreColor form location offsetFromCenter |
	count _ 32.
	angle _ Float twoPi / count / 2.
	extent _ 512.
	offsetFromCenter _ 0.
	backColor _ `Color gray: 50/255`.
	foreColor _ `Color gray: 200/255`.
	form _ Form extent: extent asPoint depth: 32.
	form fillColor: backColor.
	canvas _ VectorCanvas onForm: form.
	1 to: count do: [ :i |
		location _ AffineTransformation withRadians: i * angle * 2+0.02 scale: extent*0.45 position: extent-1/2.0 + offsetFromCenter.
		canvas engine geometryTransformation: location.
		canvas fillColor: foreColor do: [
			canvas
				moveToX: 0 y: 0;
				lineToX: 1 y: 0;
				lineToX: angle cos y: angle sin;
				lineToX: 0 y: 0.
		canvas engine geometryTransformation: canvas currentTransformation
		].
	].
	^form! !

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 12/16/2017 18:17:50'!
onForm: aForm

	^ self new setForm: aForm! !

!VectorEngine class methodsFor: 'accessing' stamp: 'jmv 4/4/2018 15:11:07'!
antiAliasingWidthForOutlineFonts
	^1.4! !

!VectorEngine class methodsFor: 'accessing' stamp: 'jmv 4/4/2018 15:11:10'!
antiAliasingWidthForStrokeFonts
	^1.1! !

!VectorEngine class methodsFor: 'accessing' stamp: 'jmv 4/4/2018 15:11:13'!
defaultAntiAliasingWidth
	^1.3. "default for graphics"! !

!CodeGeneratingVectorEngine methodsFor: 'accessing' stamp: 'jmv 1/4/2018 08:21:52'!
charCode: aCharCode selectorPrefix: selectorPrefix
	charCode _ aCharCode.
	ignoreFirstMoveTo _ true.
	didStuff _ false.
	digits _ 3.
	code _ '' writeStream.
	code
		nextPutAll: selectorPrefix, aCharCode printString;
		newLine;
		tab; nextPutAll: 'vectorGraphicsEngine';
		newLine! !

!CodeGeneratingVectorEngine methodsFor: 'accessing' stamp: 'jmv 1/4/2018 07:39:17'!
code
	^ code contents withoutSuffix: ';
'! !

!CodeGeneratingVectorEngine methodsFor: 'accessing' stamp: 'jmv 1/4/2018 08:09:45'!
storeCode
	didStuff ifTrue: [
		'blah blah' ]! !

!CodeGeneratingVectorEngine methodsFor: 'accessing' stamp: 'jmv 1/4/2018 08:11:46'!
storeCodeIn: aClass category: methodCategory
	didStuff ifTrue: [
		aClass compile: self code classified: methodCategory ]! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:08:48'!
basicArcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: theta start: startAngle sweep: sweepAngle
	didStuff _ true.
	self halt! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:08:56'!
basicArcToX: x2 y: y2 radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
	didStuff _ true.
	self halt! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:09:13'!
basicClosePath
	((firstX - currentX) abs > 1.0e-8 or: [ (firstY - currentY) abs > 1.0e-8 ]) ifTrue: [
		self halt ]! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:25:56'!
basicCubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2
	| relX1 relX2 relX3 relY1 relY2 relY3 |
	relX3 _ x3 - currentX.
	relY3 _ y3 - currentY.
	relX1 _ x1 - currentX.
	relY1 _ y1 - currentY.
	relX2 _ x2 - currentX.
	relY2 _ y2 - currentY.
	code
		tab; tab;
		nextPutAll: 'rel_CubicBezierToX: ', (relX3 printStringFractionDigits: digits), ' y: ', (relY3 printStringFractionDigits: digits);
		nextPutAll: ' control1X: ', (relX1 printStringFractionDigits: digits), ' y: ', (relY1 printStringFractionDigits: digits);
		nextPutAll: ' control2X: ', (relX2 printStringFractionDigits: digits), ' y: ', (relY2 printStringFractionDigits: digits), ';';
		newLine.
	didStuff _ true.
	super basicCubicBezierToX: x3 y: y3 control1X: x1 y: y1 control2X: x2 y: y2! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:26:04'!
basicLineToX: x y: y
	| relX relY |
	relX _ x - currentX.
	relY _ y - currentY.
	code
		tab; tab;
		nextPutAll: 'rel_LineToX: ', (relX printStringFractionDigits: digits), ' y: ', (relY printStringFractionDigits: digits), ';';
		newLine.
	didStuff _ true.
	super basicLineToX: x y: y! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:26:12'!
basicMoveToX: x y: y
	| relX relY |
	ignoreFirstMoveTo
		ifTrue: [ ignoreFirstMoveTo _ false ]
		ifFalse: [
			relX _ x - currentX.
			relY _ y - currentY.
			code
				tab; tab;
				nextPutAll: 'rel_MoveToX: ', (relX printStringFractionDigits: digits), ' y: ', (relY printStringFractionDigits: digits), ';';
				newLine ].
	didStuff _ true.
	super basicMoveToX: x y: y! !

!CodeGeneratingVectorEngine methodsFor: 'basic paths' stamp: 'jmv 1/4/2018 08:09:28'!
basicQuadraticBezierToX: x2 y: y2 controlX: x1 y: y1
	didStuff _ true.
	self halt! !

!CodeGeneratingVectorEngine methodsFor: 'private' stamp: 'jmv 8/17/2018 15:45:34'!
drawCharCode: charCode italics: doItalics renderer: aVectorFontRenderer
	aVectorFontRenderer drawCharCode: charCode atX: 0 y: 0 pointSize: 1.0 italics: doItalics! !

!VectorFontRenderer methodsFor: 'accessing' stamp: 'jmv 1/4/2018 15:57:50'!
vectorGraphicsEngine: aVectorGraphicsEngine

	vectorGraphicsEngine _ aVectorGraphicsEngine! !

!VectorFontRenderer class methodsFor: 'instance creation' stamp: 'jmv 1/4/2018 16:00:15'!
for: aVectorGraphicsEngine
	^ self new vectorGraphicsEngine: aVectorGraphicsEngine! !

!BitstreamVeraRenderer methodsFor: 'services' stamp: 'jmv 8/17/2018 15:47:05'!
drawCharCode: charCode atX: x0 y: y0 pointSize: ps italics: doItalics
"No usado directamente por M3. Usado solo por CodeGeneratingVectorEngine para generar los que si se usan."
"TrueType en M3. Bitstream Vera Sans, importado asi: TTF -> PDF -> SVG -> Codigo Smalltalk.
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 1 color: Color black fillColor: Color blue do: [
		(BitstreamVeraRenderer new vectorGraphicsEngine: c engine)
			drawCharCode: 167 atX: 150 y: 200 pointSize: 200 italics: false;
			drawCharCode: 167 atX: 220 y: 200 pointSize: 200 italics: true  ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 3.25 color: Color black fillColor: Color blue do: [
		(BitstreamVeraRenderer new vectorGraphicsEngine: c engine)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 120 italics: false ].
Display forceToScreen
"
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	fillColor: Color black do: [
		(BitstreamVeraRenderer new vectorGraphicsEngine: c engine)
			drawCharCode: 97 atX: 150 y: 200 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 158 y: 200.25 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 166 y: 200.5 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 174 y: 200.75 pointSize: 12 italics: false;
			drawCharCode: 97 atX: 182 y: 201 pointSize: 12 italics: false ].
Display forceToScreen
"
	| symbol loc prevLoc |
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].

	"Recibir la location y modificarla, no?"
	prevLoc _ vectorGraphicsEngine geometryTransformation.
	loc _ AffineTransformation withRadians: 0.0 scale: ps position: x0@y0.
	doItalics ifTrue: [ loc _ loc italizing2 ].
	loc _ prevLoc composedWith: loc.
	vectorGraphicsEngine geometryTransformation: loc.

	symbol _ ('drawRelVeraSans', charCode printString) asSymbol.
	vectorGraphicsEngine abs_MoveToX: 0.0 y: 0.0.
	self perform: symbol.

	vectorGraphicsEngine geometryTransformation: prevLoc.! !

!BitstreamVeraRenderer methodsFor: 'services' stamp: 'jmv 8/17/2018 15:56:15'!
drawCharCode: charCode italics: doItalics
"Usado de verdad por M3.
Creado por CodeGeneratingVectorEngine a partir de #drawCharCode:atX:y:pointSize:italics:canvas:
Usado solo para crear las versiones italics

Todos los glyphs usados de verdad son en comandos relativos.
"
"
| e loc  r |
e _ (VectorCanvas onForm: Display)
	into: self runningWorld;
	engine.
loc _ AffineTransformation withRadians: 0.0 scale: 200 position: 38@22.
e geometryTransformation: loc.
r _ BitstreamVeraRenderer new vectorGraphicsEngine: e.
e
	strokeWidth: 0.005 color: Color black fillColor: Color blue do: [
		r drawCharCode: 167 italics: false.
		e abs_MoveToX: 0.7 y: 0.
		r drawCharCode: 167 italics: true ].
Display forceToScreen
"
"
| e loc |
e _ (VectorCanvas onForm: Display)
	into: self runningWorld;
	engine.
loc _ AffineTransformation withRadians: 0.0 scale: 200 position: 38@222.
e geometryTransformation: loc.
e
	strokeWidth: 3.25/200 color: Color black fillColor: Color blue do: [
		(BitstreamVeraRenderer new vectorGraphicsEngine: e)
			drawCharCode: 97 italics: false ].
Display forceToScreen
"
"
| e loc r |
e _ (VectorCanvas onForm: Display)
	into: self runningWorld;
	engine.
loc _ AffineTransformation withRadians: 0.0 scale: 12 position: 38@22.
r _ BitstreamVeraRenderer new vectorGraphicsEngine: e.
e geometryTransformation: loc.
e
	fillColor: Color black do: [
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 0.6 y: 1/12/3.
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 1.2 y: 2/12/3.
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 1.8 y: 1/12.
		r drawCharCode: 97 italics: false.
		].
Display forceToScreen
"
	| symbol |
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].

	doItalics ifFalse: [
		symbol _ ('drawRelVeraSans', charCode printString) asSymbol.
	] ifTrue: [
		symbol _ ('drawVeraSansItalics', charCode printString) asSymbol ].
	self perform: symbol! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans100
	vectorGraphicsEngine
		rel_MoveToX: 0.4919 y: 0.4923;
		rel_LineToX: 0.0000 y: -0.2959;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7598;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0820;
		rel_CubicBezierToX: -0.0718 y: 0.0727 control1X: -0.0189 y: 0.0325 control2X: -0.0428 y: 0.0568;
		rel_CubicBezierToX: -0.1035 y: 0.0234 control1X: -0.0287 y: 0.0156 control2X: -0.0632 y: 0.0234;
		rel_CubicBezierToX: -0.1616 y: -0.0791 control1X: -0.0661 y: 0.0000 control2X: -0.1200 y: -0.0263;
		rel_CubicBezierToX: -0.0621 y: -0.2080 control1X: -0.0414 y: -0.0527 control2X: -0.0621 y: -0.1220;
		rel_CubicBezierToX: 0.0621 y: -0.2080 control1X: 0.0000 y: -0.0859 control2X: 0.0207 y: -0.1552;
		rel_CubicBezierToX: 0.1616 y: -0.0791 control1X: 0.0416 y: -0.0527 control2X: 0.0955 y: -0.0791;
		rel_CubicBezierToX: 0.1035 y: 0.0239 control1X: 0.0403 y: 0.0000 control2X: 0.0748 y: 0.0080;
		rel_CubicBezierToX: 0.0718 y: 0.0723 control1X: 0.0290 y: 0.0157 control2X: 0.0529 y: 0.0398;
		rel_MoveToX: -0.3062 y: 0.1909;
		rel_CubicBezierToX: 0.0405 y: 0.1558 control1X: 0.0000 y: 0.0661 control2X: 0.0135 y: 0.1180;
		rel_CubicBezierToX: 0.1123 y: 0.0561 control1X: 0.0274 y: 0.0374 control2X: 0.0648 y: 0.0561;
		rel_CubicBezierToX: 0.1124 y: -0.0561 control1X: 0.0476 y: 0.0000 control2X: 0.0850 y: -0.0187;
		rel_CubicBezierToX: 0.0410 y: -0.1558 control1X: 0.0273 y: -0.0378 control2X: 0.0410 y: -0.0897;
		rel_CubicBezierToX: -0.0410 y: -0.1552 control1X: 0.0000 y: -0.0660 control2X: -0.0137 y: -0.1178;
		rel_CubicBezierToX: -0.1124 y: -0.0567 control1X: -0.0274 y: -0.0378 control2X: -0.0648 y: -0.0567;
		rel_CubicBezierToX: -0.1123 y: 0.0567 control1X: -0.0475 y: 0.0000 control2X: -0.0849 y: 0.0189;
		rel_CubicBezierToX: -0.0405 y: 0.1552 control1X: -0.0270 y: 0.0374 control2X: -0.0405 y: 0.0892! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans101
	vectorGraphicsEngine
		rel_MoveToX: 0.5998 y: 0.6603;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1396 y: 0.0484 control1X: 0.0335 y: 0.0323 control2X: 0.0800 y: 0.0484;
		rel_CubicBezierToX: 0.1001 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0679 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0991 y: 0.0313 control1X: -0.0322 y: 0.0137 control2X: -0.0653 y: 0.0241;
		rel_CubicBezierToX: -0.1031 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0682 y: 0.0107;
		rel_CubicBezierToX: -0.2075 y: -0.0761 control1X: -0.0872 y: 0.0000 control2X: -0.1564 y: -0.0253;
		rel_CubicBezierToX: -0.0762 y: -0.2061 control1X: -0.0508 y: -0.0508 control2X: -0.0762 y: -0.1195;
		rel_CubicBezierToX: 0.0723 y: -0.2129 control1X: 0.0000 y: -0.0895 control2X: 0.0241 y: -0.1605;
		rel_CubicBezierToX: 0.1958 y: -0.0791 control1X: 0.0485 y: -0.0527 control2X: 0.1138 y: -0.0791;
		rel_CubicBezierToX: 0.1743 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1317 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.0899 y: -0.0264;
		rel_CubicBezierToX: -0.0415 y: -0.1177 control1X: -0.0006 y: -0.0491 control2X: -0.0145 y: -0.0884;
		rel_CubicBezierToX: -0.1064 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0621 y: -0.0439;
		rel_CubicBezierToX: -0.1206 y: 0.0425 control1X: -0.0501 y: 0.0000 control2X: -0.0903 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3203 y: -0.0005! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans102
	vectorGraphicsEngine
		rel_MoveToX: 0.4089 y: 0.1964;
		rel_LineToX: 0.0000 y: 0.0747;
		rel_LineToX: -0.0860 y: 0.0000;
		rel_CubicBezierToX: -0.0674 y: 0.0196 control1X: -0.0322 y: 0.0000 control2X: -0.0547 y: 0.0065;
		rel_CubicBezierToX: -0.0185 y: 0.0703 control1X: -0.0123 y: 0.0130 control2X: -0.0185 y: 0.0364;
		rel_LineToX: 0.0000 y: 0.0483;
		rel_LineToX: 0.1479 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.1479 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4771;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.4771;
		rel_LineToX: -0.0860 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.0860 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0381;
		rel_CubicBezierToX: 0.0424 y: -0.1328 control1X: 0.0000 y: -0.0609 control2X: 0.0141 y: -0.1051;
		rel_CubicBezierToX: 0.1348 y: -0.0420 control1X: 0.0284 y: -0.0280 control2X: 0.0733 y: -0.0420;
		rel_LineToX: 0.0850 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans103
	vectorGraphicsEngine
		rel_MoveToX: 0.4919 y: 0.6764;
		rel_CubicBezierToX: -0.0406 y: -0.1514 control1X: 0.0000 y: -0.0651 control2X: -0.0135 y: -0.1156;
		rel_CubicBezierToX: -0.1128 y: -0.0537 control1X: -0.0267 y: -0.0358 control2X: -0.0642 y: -0.0537;
		rel_CubicBezierToX: -0.1127 y: 0.0537 control1X: -0.0481 y: 0.0000 control2X: -0.0857 y: 0.0179;
		rel_CubicBezierToX: -0.0401 y: 0.1514 control1X: -0.0267 y: 0.0358 control2X: -0.0401 y: 0.0863;
		rel_CubicBezierToX: 0.0401 y: 0.1509 control1X: 0.0000 y: 0.0648 control2X: 0.0134 y: 0.1151;
		rel_CubicBezierToX: 0.1127 y: 0.0537 control1X: 0.0270 y: 0.0358 control2X: 0.0646 y: 0.0537;
		rel_CubicBezierToX: 0.1128 y: -0.0537 control1X: 0.0486 y: 0.0000 control2X: 0.0861 y: -0.0179;
		rel_CubicBezierToX: 0.0406 y: -0.1509 control1X: 0.0271 y: -0.0358 control2X: 0.0406 y: -0.0861;
		rel_MoveToX: 0.0898 y: 0.2119;
		rel_CubicBezierToX: -0.0620 y: 0.2075 control1X: 0.0000 y: 0.0931 control2X: -0.0207 y: 0.1623;
		rel_CubicBezierToX: -0.1899 y: 0.0684 control1X: -0.0413 y: 0.0456 control2X: -0.1047 y: 0.0684;
		rel_CubicBezierToX: -0.0894 y: -0.0073 control1X: -0.0316 y: 0.0000 control2X: -0.0614 y: -0.0025;
		rel_CubicBezierToX: -0.0815 y: -0.0215 control1X: -0.0280 y: -0.0046 control2X: -0.0552 y: -0.0118;
		rel_LineToX: 0.0000 y: -0.0874;
		rel_CubicBezierToX: 0.0781 y: 0.0317 control1X: 0.0263 y: 0.0143 control2X: 0.0524 y: 0.0249;
		rel_CubicBezierToX: 0.0786 y: 0.0103 control1X: 0.0257 y: 0.0069 control2X: 0.0519 y: 0.0103;
		rel_CubicBezierToX: 0.1323 y: -0.0464 control1X: 0.0589 y: 0.0000 control2X: 0.1030 y: -0.0155;
		rel_CubicBezierToX: 0.0440 y: -0.1392 control1X: 0.0293 y: -0.0306 control2X: 0.0440 y: -0.0770;
		rel_LineToX: 0.0000 y: -0.0444;
		rel_CubicBezierToX: -0.0713 y: 0.0723 control1X: -0.0186 y: 0.0322 control2X: -0.0423 y: 0.0563;
		rel_CubicBezierToX: -0.1040 y: 0.0239 control1X: -0.0290 y: 0.0159 control2X: -0.0637 y: 0.0239;
		rel_CubicBezierToX: -0.1621 y: -0.0767 control1X: -0.0671 y: 0.0000 control2X: -0.1211 y: -0.0256;
		rel_CubicBezierToX: -0.0616 y: -0.2031 control1X: -0.0410 y: -0.0511 control2X: -0.0616 y: -0.1188;
		rel_CubicBezierToX: 0.0616 y: -0.2036 control1X: 0.0000 y: -0.0846 control2X: 0.0206 y: -0.1525;
		rel_CubicBezierToX: 0.1621 y: -0.0767 control1X: 0.0410 y: -0.0511 control2X: 0.0950 y: -0.0767;
		rel_CubicBezierToX: 0.1040 y: 0.0239 control1X: 0.0403 y: 0.0000 control2X: 0.0750 y: 0.0080;
		rel_CubicBezierToX: 0.0713 y: 0.0723 control1X: 0.0290 y: 0.0160 control2X: 0.0527 y: 0.0401;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4790! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans104
	vectorGraphicsEngine
		rel_MoveToX: 0.5866 y: 0.6261;
		rel_LineToX: 0.0000 y: 0.3301;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3272;
		rel_CubicBezierToX: -0.0303 y: -0.1162 control1X: 0.0000 y: -0.0517 control2X: -0.0101 y: -0.0905;
		rel_CubicBezierToX: -0.0908 y: -0.0386 control1X: -0.0202 y: -0.0257 control2X: -0.0505 y: -0.0386;
		rel_CubicBezierToX: -0.1148 y: 0.0464 control1X: -0.0485 y: 0.0000 control2X: -0.0868 y: 0.0155;
		rel_CubicBezierToX: -0.0420 y: 0.1265 control1X: -0.0280 y: 0.0310 control2X: -0.0420 y: 0.0731;
		rel_LineToX: 0.0000 y: 0.3091;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7598;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2979;
		rel_CubicBezierToX: 0.0757 y: -0.0738 control1X: 0.0215 y: -0.0329 control2X: 0.0467 y: -0.0575;
		rel_CubicBezierToX: 0.1011 y: -0.0244 control1X: 0.0293 y: -0.0162 control2X: 0.0630 y: -0.0244;
		rel_CubicBezierToX: 0.1426 y: 0.0586 control1X: 0.0628 y: 0.0000 control2X: 0.1103 y: 0.0196;
		rel_CubicBezierToX: 0.0483 y: 0.1714 control1X: 0.0322 y: 0.0388 control2X: 0.0483 y: 0.0959! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans105
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_MoveToX: 0.0000 y: -0.2129;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1138;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1138! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans106
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5566;
		rel_CubicBezierToX: -0.0400 y: 0.1514 control1X: 0.0000 y: 0.0697 control2X: -0.0133 y: 0.1202;
		rel_CubicBezierToX: -0.1279 y: 0.0469 control1X: -0.0264 y: 0.0313 control2X: -0.0690 y: 0.0469;
		rel_LineToX: -0.0342 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0762;
		rel_LineToX: 0.0239 y: 0.0000;
		rel_CubicBezierToX: 0.0699 y: -0.0239 control1X: 0.0342 y: 0.0000 control2X: 0.0575 y: -0.0080;
		rel_CubicBezierToX: 0.0185 y: -0.0982 control1X: 0.0123 y: -0.0156 control2X: 0.0185 y: -0.0483;
		rel_LineToX: 0.0000 y: -0.5566;
		rel_MoveToX: 0.0000 y: -0.2129;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1138;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1138! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans107
	vectorGraphicsEngine
		rel_MoveToX: 0.1286 y: 0.1964;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4487;
		rel_LineToX: 0.2681 y: -0.2358;
		rel_LineToX: 0.1147 y: 0.0000;
		rel_LineToX: -0.2900 y: 0.2559;
		rel_LineToX: 0.3022 y: 0.2910;
		rel_LineToX: -0.1171 y: 0.0000;
		rel_LineToX: -0.2779 y: -0.2671;
		rel_LineToX: 0.0000 y: 0.2671;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7598! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans108
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.1964;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7598;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7598! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans109
	vectorGraphicsEngine
		rel_MoveToX: 0.5578 y: 0.5143;
		rel_CubicBezierToX: 0.0806 y: -0.0894 control1X: 0.0224 y: -0.0404 control2X: 0.0493 y: -0.0702;
		rel_CubicBezierToX: 0.1103 y: -0.0288 control1X: 0.0312 y: -0.0192 control2X: 0.0680 y: -0.0288;
		rel_CubicBezierToX: 0.1318 y: 0.0601 control1X: 0.0570 y: 0.0000 control2X: 0.1009 y: 0.0200;
		rel_CubicBezierToX: 0.0464 y: 0.1699 control1X: 0.0310 y: 0.0397 control2X: 0.0464 y: 0.0963;
		rel_LineToX: 0.0000 y: 0.3301;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3272;
		rel_CubicBezierToX: -0.0278 y: -0.1167 control1X: 0.0000 y: -0.0524 control2X: -0.0093 y: -0.0913;
		rel_CubicBezierToX: -0.0850 y: -0.0381 control1X: -0.0186 y: -0.0254 control2X: -0.0469 y: -0.0381;
		rel_CubicBezierToX: -0.1103 y: 0.0464 control1X: -0.0465 y: 0.0000 control2X: -0.0833 y: 0.0155;
		rel_CubicBezierToX: -0.0406 y: 0.1265 control1X: -0.0271 y: 0.0310 control2X: -0.0406 y: 0.0731;
		rel_LineToX: 0.0000 y: 0.3091;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3272;
		rel_CubicBezierToX: -0.0278 y: -0.1167 control1X: 0.0000 y: -0.0527 control2X: -0.0093 y: -0.0916;
		rel_CubicBezierToX: -0.0860 y: -0.0381 control1X: -0.0186 y: -0.0254 control2X: -0.0472 y: -0.0381;
		rel_CubicBezierToX: -0.1094 y: 0.0469 control1X: -0.0459 y: 0.0000 control2X: -0.0823 y: 0.0157;
		rel_CubicBezierToX: -0.0405 y: 0.1260 control1X: -0.0270 y: 0.0309 control2X: -0.0405 y: 0.0729;
		rel_LineToX: 0.0000 y: 0.3091;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: 0.0737 y: -0.0743 control1X: 0.0205 y: -0.0336 control2X: 0.0451 y: -0.0583;
		rel_CubicBezierToX: 0.1021 y: -0.0239 control1X: 0.0287 y: -0.0159 control2X: 0.0627 y: -0.0239;
		rel_CubicBezierToX: 0.1011 y: 0.0303 control1X: 0.0397 y: 0.0000 control2X: 0.0734 y: 0.0101;
		rel_CubicBezierToX: 0.0620 y: 0.0879 control1X: 0.0280 y: 0.0202 control2X: 0.0486 y: 0.0495! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans110
	vectorGraphicsEngine
		rel_MoveToX: 0.5866 y: 0.6261;
		rel_LineToX: 0.0000 y: 0.3301;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3272;
		rel_CubicBezierToX: -0.0303 y: -0.1162 control1X: 0.0000 y: -0.0517 control2X: -0.0101 y: -0.0905;
		rel_CubicBezierToX: -0.0908 y: -0.0386 control1X: -0.0202 y: -0.0257 control2X: -0.0505 y: -0.0386;
		rel_CubicBezierToX: -0.1148 y: 0.0464 control1X: -0.0485 y: 0.0000 control2X: -0.0868 y: 0.0155;
		rel_CubicBezierToX: -0.0420 y: 0.1265 control1X: -0.0280 y: 0.0310 control2X: -0.0420 y: 0.0731;
		rel_LineToX: 0.0000 y: 0.3091;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: 0.0757 y: -0.0738 control1X: 0.0215 y: -0.0329 control2X: 0.0467 y: -0.0575;
		rel_CubicBezierToX: 0.1011 y: -0.0244 control1X: 0.0293 y: -0.0162 control2X: 0.0630 y: -0.0244;
		rel_CubicBezierToX: 0.1426 y: 0.0586 control1X: 0.0628 y: 0.0000 control2X: 0.1103 y: 0.0196;
		rel_CubicBezierToX: 0.0483 y: 0.1714 control1X: 0.0322 y: 0.0388 control2X: 0.0483 y: 0.0959! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans111
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans112
	vectorGraphicsEngine
		rel_MoveToX: 0.2189 y: 0.8742;
		rel_LineToX: 0.0000 y: 0.2900;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7549;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_CubicBezierToX: 0.0713 y: -0.0723 control1X: 0.0189 y: -0.0325 control2X: 0.0427 y: -0.0566;
		rel_CubicBezierToX: 0.1035 y: -0.0239 control1X: 0.0290 y: -0.0159 control2X: 0.0635 y: -0.0239;
		rel_CubicBezierToX: 0.1616 y: 0.0791 control1X: 0.0664 y: 0.0000 control2X: 0.1203 y: 0.0264;
		rel_CubicBezierToX: 0.0625 y: 0.2080 control1X: 0.0417 y: 0.0528 control2X: 0.0625 y: 0.1221;
		rel_CubicBezierToX: -0.0625 y: 0.2080 control1X: 0.0000 y: 0.0860 control2X: -0.0208 y: 0.1553;
		rel_CubicBezierToX: -0.1616 y: 0.0791 control1X: -0.0413 y: 0.0528 control2X: -0.0952 y: 0.0791;
		rel_CubicBezierToX: -0.1035 y: -0.0234 control1X: -0.0400 y: 0.0000 control2X: -0.0745 y: -0.0078;
		rel_CubicBezierToX: -0.0713 y: -0.0727 control1X: -0.0286 y: -0.0159 control2X: -0.0524 y: -0.0402;
		rel_MoveToX: 0.3057 y: -0.1910;
		rel_CubicBezierToX: -0.0410 y: -0.1552 control1X: 0.0000 y: -0.0660 control2X: -0.0137 y: -0.1178;
		rel_CubicBezierToX: -0.1118 y: -0.0567 control1X: -0.0271 y: -0.0378 control2X: -0.0643 y: -0.0567;
		rel_CubicBezierToX: -0.1124 y: 0.0567 control1X: -0.0476 y: 0.0000 control2X: -0.0850 y: 0.0189;
		rel_CubicBezierToX: -0.0405 y: 0.1552 control1X: -0.0270 y: 0.0374 control2X: -0.0405 y: 0.0892;
		rel_CubicBezierToX: 0.0405 y: 0.1558 control1X: 0.0000 y: 0.0661 control2X: 0.0135 y: 0.1180;
		rel_CubicBezierToX: 0.1124 y: 0.0561 control1X: 0.0274 y: 0.0374 control2X: 0.0648 y: 0.0561;
		rel_CubicBezierToX: 0.1118 y: -0.0561 control1X: 0.0475 y: 0.0000 control2X: 0.0847 y: -0.0187;
		rel_CubicBezierToX: 0.0410 y: -0.1558 control1X: 0.0273 y: -0.0378 control2X: 0.0410 y: -0.0897! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans113
	vectorGraphicsEngine
		rel_MoveToX: 0.1857 y: 0.6832;
		rel_CubicBezierToX: 0.0405 y: 0.1558 control1X: 0.0000 y: 0.0661 control2X: 0.0135 y: 0.1180;
		rel_CubicBezierToX: 0.1123 y: 0.0561 control1X: 0.0274 y: 0.0374 control2X: 0.0648 y: 0.0561;
		rel_CubicBezierToX: 0.1124 y: -0.0561 control1X: 0.0476 y: 0.0000 control2X: 0.0850 y: -0.0187;
		rel_CubicBezierToX: 0.0410 y: -0.1558 control1X: 0.0273 y: -0.0378 control2X: 0.0410 y: -0.0897;
		rel_CubicBezierToX: -0.0410 y: -0.1552 control1X: 0.0000 y: -0.0660 control2X: -0.0137 y: -0.1178;
		rel_CubicBezierToX: -0.1124 y: -0.0567 control1X: -0.0274 y: -0.0378 control2X: -0.0648 y: -0.0567;
		rel_CubicBezierToX: -0.1123 y: 0.0567 control1X: -0.0475 y: 0.0000 control2X: -0.0849 y: 0.0189;
		rel_CubicBezierToX: -0.0405 y: 0.1552 control1X: -0.0270 y: 0.0374 control2X: -0.0405 y: 0.0892;
		rel_MoveToX: 0.3062 y: 0.1910;
		rel_CubicBezierToX: -0.0718 y: 0.0727 control1X: -0.0189 y: 0.0325 control2X: -0.0428 y: 0.0568;
		rel_CubicBezierToX: -0.1035 y: 0.0234 control1X: -0.0287 y: 0.0156 control2X: -0.0632 y: 0.0234;
		rel_CubicBezierToX: -0.1616 y: -0.0791 control1X: -0.0661 y: 0.0000 control2X: -0.1200 y: -0.0263;
		rel_CubicBezierToX: -0.0621 y: -0.2080 control1X: -0.0414 y: -0.0527 control2X: -0.0621 y: -0.1220;
		rel_CubicBezierToX: 0.0621 y: -0.2080 control1X: 0.0000 y: -0.0859 control2X: 0.0207 y: -0.1552;
		rel_CubicBezierToX: 0.1616 y: -0.0791 control1X: 0.0416 y: -0.0527 control2X: 0.0955 y: -0.0791;
		rel_CubicBezierToX: 0.1035 y: 0.0239 control1X: 0.0403 y: 0.0000 control2X: 0.0748 y: 0.0080;
		rel_CubicBezierToX: 0.0718 y: 0.0723 control1X: 0.0290 y: 0.0157 control2X: 0.0529 y: 0.0398;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7549;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2900! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans114
	vectorGraphicsEngine
		rel_MoveToX: 0.4489 y: 0.4933;
		rel_CubicBezierToX: -0.0332 y: -0.0127 control1X: -0.0101 y: -0.0059 control2X: -0.0212 y: -0.0101;
		rel_CubicBezierToX: -0.0391 y: -0.0044 control1X: -0.0117 y: -0.0029 control2X: -0.0247 y: -0.0044;
		rel_CubicBezierToX: -0.1172 y: 0.0498 control1X: -0.0507 y: 0.0000 control2X: -0.0898 y: 0.0166;
		rel_CubicBezierToX: -0.0405 y: 0.1421 control1X: -0.0270 y: 0.0329 control2X: -0.0405 y: 0.0802;
		rel_LineToX: 0.0000 y: 0.2881;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: 0.0737 y: -0.0738 control1X: 0.0189 y: -0.0332 control2X: 0.0435 y: -0.0578;
		rel_CubicBezierToX: 0.1104 y: -0.0244 control1X: 0.0303 y: -0.0162 control2X: 0.0671 y: -0.0244;
		rel_CubicBezierToX: 0.0205 y: 0.0015 control1X: 0.0062 y: 0.0000 control2X: 0.0130 y: 0.0005;
		rel_CubicBezierToX: 0.0249 y: 0.0034 control1X: 0.0075 y: 0.0006 control2X: 0.0158 y: 0.0018;
		rel_LineToX: 0.0005 y: 0.0923! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans115
	vectorGraphicsEngine
		rel_MoveToX: 0.4806 y: 0.4254;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: -0.0791 y: -0.0293 control1X: -0.0254 y: -0.0130 control2X: -0.0517 y: -0.0228;
		rel_CubicBezierToX: -0.0849 y: -0.0098 control1X: -0.0273 y: -0.0065 control2X: -0.0556 y: -0.0098;
		rel_CubicBezierToX: -0.1006 y: 0.0205 control1X: -0.0446 y: 0.0000 control2X: -0.0781 y: 0.0069;
		rel_CubicBezierToX: -0.0332 y: 0.0615 control1X: -0.0221 y: 0.0137 control2X: -0.0332 y: 0.0342;
		rel_CubicBezierToX: 0.0239 y: 0.0494 control1X: 0.0000 y: 0.0209 control2X: 0.0080 y: 0.0373;
		rel_CubicBezierToX: 0.0962 y: 0.0337 control1X: 0.0160 y: 0.0117 control2X: 0.0480 y: 0.0229;
		rel_LineToX: 0.0308 y: 0.0068;
		rel_CubicBezierToX: 0.1357 y: 0.0581 control1X: 0.0638 y: 0.0137 control2X: 0.1090 y: 0.0330;
		rel_CubicBezierToX: 0.0405 y: 0.1040 control1X: 0.0270 y: 0.0247 control2X: 0.0405 y: 0.0594;
		rel_CubicBezierToX: -0.0605 y: 0.1206 control1X: 0.0000 y: 0.0508 control2X: -0.0201 y: 0.0910;
		rel_CubicBezierToX: -0.1655 y: 0.0444 control1X: -0.0401 y: 0.0296 control2X: -0.0952 y: 0.0444;
		rel_CubicBezierToX: -0.0918 y: -0.0087 control1X: -0.0293 y: 0.0000 control2X: -0.0599 y: -0.0029;
		rel_CubicBezierToX: -0.1001 y: -0.0254 control1X: -0.0316 y: -0.0056 control2X: -0.0650 y: -0.0140;
		rel_LineToX: 0.0000 y: -0.0928;
		rel_CubicBezierToX: 0.0981 y: 0.0391 control1X: 0.0332 y: 0.0172 control2X: 0.0659 y: 0.0303;
		rel_CubicBezierToX: 0.0957 y: 0.0126 control1X: 0.0322 y: 0.0084 control2X: 0.0641 y: 0.0126;
		rel_CubicBezierToX: 0.0977 y: -0.0214 control1X: 0.0423 y: 0.0000 control2X: 0.0749 y: -0.0071;
		rel_CubicBezierToX: 0.0341 y: -0.0616 control1X: 0.0228 y: -0.0147 control2X: 0.0341 y: -0.0352;
		rel_CubicBezierToX: -0.0249 y: -0.0561 control1X: 0.0000 y: -0.0244 control2X: -0.0083 y: -0.0431;
		rel_CubicBezierToX: -0.1079 y: -0.0376 control1X: -0.0162 y: -0.0130 control2X: -0.0522 y: -0.0256;
		rel_LineToX: -0.0312 y: -0.0073;
		rel_CubicBezierToX: -0.1206 y: -0.0537 control1X: -0.0557 y: -0.0118 control2X: -0.0959 y: -0.0297;
		rel_CubicBezierToX: -0.0371 y: -0.1001 control1X: -0.0248 y: -0.0245 control2X: -0.0371 y: -0.0578;
		rel_CubicBezierToX: 0.0547 y: -0.1192 control1X: 0.0000 y: -0.0515 control2X: 0.0182 y: -0.0912;
		rel_CubicBezierToX: 0.1552 y: -0.0420 control1X: 0.0364 y: -0.0280 control2X: 0.0882 y: -0.0420;
		rel_CubicBezierToX: 0.0938 y: 0.0073 control1X: 0.0332 y: 0.0000 control2X: 0.0645 y: 0.0025;
		rel_CubicBezierToX: 0.0810 y: 0.0220 control1X: 0.0293 y: 0.0049 control2X: 0.0563 y: 0.0123! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans116
	vectorGraphicsEngine
		rel_MoveToX: 0.2209 y: 0.2540;
		rel_LineToX: 0.0000 y: 0.1553;
		rel_LineToX: 0.1850 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.1850 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2969;
		rel_CubicBezierToX: 0.0180 y: 0.0859 control1X: 0.0000 y: 0.0446 control2X: 0.0060 y: 0.0732;
		rel_CubicBezierToX: 0.0747 y: 0.0191 control1X: 0.0124 y: 0.0127 control2X: 0.0373 y: 0.0191;
		rel_LineToX: 0.0923 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0752;
		rel_LineToX: -0.0923 y: 0.0000;
		rel_CubicBezierToX: -0.1435 y: -0.0386 control1X: -0.0693 y: 0.0000 control2X: -0.1171 y: -0.0129;
		rel_CubicBezierToX: -0.0396 y: -0.1416 control1X: -0.0264 y: -0.0260 control2X: -0.0396 y: -0.0732;
		rel_LineToX: 0.0000 y: -0.2969;
		rel_LineToX: -0.0659 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.0659 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1553;
		rel_LineToX: 0.0904 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans117
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 0.7404;
		rel_LineToX: 0.0000 y: -0.3311;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3276;
		rel_CubicBezierToX: 0.0302 y: 0.1167 control1X: 0.0000 y: 0.0518 control2X: 0.0101 y: 0.0907;
		rel_CubicBezierToX: 0.0909 y: 0.0386 control1X: 0.0202 y: 0.0258 control2X: 0.0505 y: 0.0386;
		rel_CubicBezierToX: 0.1147 y: -0.0464 control1X: 0.0485 y: 0.0000 control2X: 0.0867 y: -0.0154;
		rel_CubicBezierToX: 0.0425 y: -0.1264 control1X: 0.0283 y: -0.0309 control2X: 0.0425 y: -0.0730;
		rel_LineToX: 0.0000 y: -0.3101;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0840;
		rel_CubicBezierToX: -0.0762 y: 0.0742 control1X: -0.0218 y: 0.0332 control2X: -0.0472 y: 0.0579;
		rel_CubicBezierToX: -0.1001 y: 0.0239 control1X: -0.0286 y: 0.0160 control2X: -0.0620 y: 0.0239;
		rel_CubicBezierToX: -0.1430 y: -0.0586 control1X: -0.0628 y: 0.0000 control2X: -0.1105 y: -0.0195;
		rel_CubicBezierToX: -0.0489 y: -0.1713 control1X: -0.0326 y: -0.0390 control2X: -0.0489 y: -0.0961! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans118
	vectorGraphicsEngine
		rel_MoveToX: 0.0676 y: 0.4093;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: 0.1709 y: 0.4590;
		rel_LineToX: 0.1709 y: -0.4590;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: -0.2051 y: 0.5469;
		rel_LineToX: -0.1221 y: 0.0000;
		rel_LineToX: -0.2050 y: -0.5469! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans119
	vectorGraphicsEngine
		rel_MoveToX: 0.0798 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.1123 y: 0.4268;
		rel_LineToX: 0.1118 y: -0.4268;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: 0.1123 y: 0.4268;
		rel_LineToX: 0.1118 y: -0.4268;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: -0.1430 y: 0.5469;
		rel_LineToX: -0.1060 y: 0.0000;
		rel_LineToX: -0.1177 y: -0.4483;
		rel_LineToX: -0.1181 y: 0.4483;
		rel_LineToX: -0.1060 y: 0.0000;
		rel_LineToX: -0.1430 y: -0.5469! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans120
	vectorGraphicsEngine
		rel_MoveToX: 0.5866 y: 0.4093;
		rel_LineToX: -0.1978 y: 0.2661;
		rel_LineToX: 0.2080 y: 0.2808;
		rel_LineToX: -0.1059 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.2149;
		rel_LineToX: -0.1592 y: 0.2149;
		rel_LineToX: -0.1059 y: 0.0000;
		rel_LineToX: 0.2124 y: -0.2862;
		rel_LineToX: -0.1944 y: -0.2607;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: 0.1450 y: 0.1948;
		rel_LineToX: 0.1450 y: -0.1948;
		rel_LineToX: 0.1060 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans121
	vectorGraphicsEngine
		rel_MoveToX: 0.3595 y: 1.0070;
		rel_CubicBezierToX: -0.0742 y: 0.1274 control1X: -0.0253 y: 0.0651 control2X: -0.0501 y: 0.1075;
		rel_CubicBezierToX: -0.0967 y: 0.0298 control1X: -0.0241 y: 0.0199 control2X: -0.0563 y: 0.0298;
		rel_LineToX: -0.0717 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.0527 y: 0.0000;
		rel_CubicBezierToX: 0.0576 y: -0.0176 control1X: 0.0247 y: 0.0000 control2X: 0.0439 y: -0.0059;
		rel_CubicBezierToX: 0.0454 y: -0.0830 control1X: 0.0137 y: -0.0117 control2X: 0.0288 y: -0.0394;
		rel_LineToX: 0.0161 y: -0.0410;
		rel_LineToX: -0.2211 y: -0.5381;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: 0.1709 y: 0.4277;
		rel_LineToX: 0.1709 y: -0.4277;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: -0.2403 y: 0.5977! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans122
	vectorGraphicsEngine
		rel_MoveToX: 0.0929 y: 0.4093;
		rel_LineToX: 0.4268 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0820;
		rel_LineToX: -0.3379 y: 0.3931;
		rel_LineToX: 0.3379 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0718;
		rel_LineToX: -0.4390 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0820;
		rel_LineToX: 0.3379 y: -0.3931;
		rel_LineToX: -0.3257 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0718! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans123
	vectorGraphicsEngine
		rel_MoveToX: 0.5490 y: 1.0490;
		rel_LineToX: 0.0000 y: 0.0703;
		rel_LineToX: -0.0303 y: 0.0000;
		rel_CubicBezierToX: -0.1631 y: -0.0362 control1X: -0.0810 y: 0.0000 control2X: -0.1354 y: -0.0121;
		rel_CubicBezierToX: -0.0410 y: -0.1440 control1X: -0.0273 y: -0.0241 control2X: -0.0410 y: -0.0721;
		rel_LineToX: 0.0000 y: -0.1167;
		rel_CubicBezierToX: -0.0263 y: -0.1021 control1X: 0.0000 y: -0.0492 control2X: -0.0088 y: -0.0832;
		rel_CubicBezierToX: -0.0957 y: -0.0283 control1X: -0.0176 y: -0.0188 control2X: -0.0495 y: -0.0283;
		rel_LineToX: -0.0298 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.0298 y: 0.0000;
		rel_CubicBezierToX: 0.0957 y: -0.0278 control1X: 0.0465 y: 0.0000 control2X: 0.0784 y: -0.0093;
		rel_CubicBezierToX: 0.0263 y: -0.1011 control1X: 0.0175 y: -0.0189 control2X: 0.0263 y: -0.0526;
		rel_LineToX: 0.0000 y: -0.1172;
		rel_CubicBezierToX: 0.0410 y: -0.1436 control1X: 0.0000 y: -0.0719 control2X: 0.0137 y: -0.1198;
		rel_CubicBezierToX: 0.1631 y: -0.0361 control1X: 0.0277 y: -0.0240 control2X: 0.0821 y: -0.0361;
		rel_LineToX: 0.0303 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.0332 y: 0.0000;
		rel_CubicBezierToX: -0.0898 y: 0.0215 control1X: -0.0459 y: 0.0000 control2X: -0.0759 y: 0.0072;
		rel_CubicBezierToX: -0.0210 y: 0.0904 control1X: -0.0140 y: 0.0143 control2X: -0.0210 y: 0.0445;
		rel_LineToX: 0.0000 y: 0.1211;
		rel_CubicBezierToX: -0.0225 y: 0.1113 control1X: 0.0000 y: 0.0511 control2X: -0.0075 y: 0.0882;
		rel_CubicBezierToX: -0.0757 y: 0.0469 control1X: -0.0147 y: 0.0231 control2X: -0.0399 y: 0.0387;
		rel_CubicBezierToX: 0.0762 y: 0.0478 control1X: 0.0361 y: 0.0087 control2X: 0.0615 y: 0.0247;
		rel_CubicBezierToX: 0.0220 y: 0.1108 control1X: 0.0146 y: 0.0231 control2X: 0.0220 y: 0.0601;
		rel_LineToX: 0.0000 y: 0.1211;
		rel_CubicBezierToX: 0.0210 y: 0.0904 control1X: 0.0000 y: 0.0459 control2X: 0.0070 y: 0.0760;
		rel_CubicBezierToX: 0.0898 y: 0.0215 control1X: 0.0139 y: 0.0143 control2X: 0.0439 y: 0.0215;
		rel_LineToX: 0.0332 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans124
	vectorGraphicsEngine
		rel_MoveToX: 0.2477 y: 0.1920;
		rel_LineToX: 0.0000 y: 1.0000;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -1.0000;
		rel_LineToX: 0.0830 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans125
	vectorGraphicsEngine
		rel_MoveToX: 0.1628 y: 1.0490;
		rel_LineToX: 0.0341 y: 0.0000;
		rel_CubicBezierToX: 0.0889 y: -0.0210 control1X: 0.0456 y: 0.0000 control2X: 0.0752 y: -0.0070;
		rel_CubicBezierToX: 0.0210 y: -0.0909 control1X: 0.0140 y: -0.0140 control2X: 0.0210 y: -0.0443;
		rel_LineToX: 0.0000 y: -0.1211;
		rel_CubicBezierToX: 0.0220 y: -0.1108 control1X: 0.0000 y: -0.0507 control2X: 0.0073 y: -0.0877;
		rel_CubicBezierToX: 0.0762 y: -0.0478 control1X: 0.0146 y: -0.0231 control2X: 0.0400 y: -0.0391;
		rel_CubicBezierToX: -0.0762 y: -0.0469 control1X: -0.0362 y: -0.0082 control2X: -0.0616 y: -0.0238;
		rel_CubicBezierToX: -0.0220 y: -0.1113 control1X: -0.0147 y: -0.0231 control2X: -0.0220 y: -0.0602;
		rel_LineToX: 0.0000 y: -0.1211;
		rel_CubicBezierToX: -0.0210 y: -0.0904 control1X: 0.0000 y: -0.0463 control2X: -0.0070 y: -0.0764;
		rel_CubicBezierToX: -0.0889 y: -0.0215 control1X: -0.0137 y: -0.0143 control2X: -0.0433 y: -0.0215;
		rel_LineToX: -0.0341 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.0307 y: 0.0000;
		rel_CubicBezierToX: 0.1621 y: 0.0361 control1X: 0.0811 y: 0.0000 control2X: 0.1351 y: 0.0121;
		rel_CubicBezierToX: 0.0411 y: 0.1436 control1X: 0.0274 y: 0.0238 control2X: 0.0411 y: 0.0717;
		rel_LineToX: 0.0000 y: 0.1172;
		rel_CubicBezierToX: 0.0263 y: 0.1011 control1X: 0.0000 y: 0.0485 control2X: 0.0087 y: 0.0822;
		rel_CubicBezierToX: 0.0957 y: 0.0278 control1X: 0.0176 y: 0.0185 control2X: 0.0495 y: 0.0278;
		rel_LineToX: 0.0303 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.0303 y: 0.0000;
		rel_CubicBezierToX: -0.0957 y: 0.0283 control1X: -0.0462 y: 0.0000 control2X: -0.0781 y: 0.0095;
		rel_CubicBezierToX: -0.0263 y: 0.1021 control1X: -0.0176 y: 0.0189 control2X: -0.0263 y: 0.0529;
		rel_LineToX: 0.0000 y: 0.1167;
		rel_CubicBezierToX: -0.0411 y: 0.1440 control1X: 0.0000 y: 0.0719 control2X: -0.0137 y: 0.1199;
		rel_CubicBezierToX: -0.1621 y: 0.0362 control1X: -0.0270 y: 0.0241 control2X: -0.0810 y: 0.0362;
		rel_LineToX: -0.0307 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0703! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans126
	vectorGraphicsEngine
		rel_MoveToX: 0.7697 y: 0.5573;
		rel_LineToX: 0.0000 y: 0.0869;
		rel_CubicBezierToX: -0.0952 y: 0.0551 control1X: -0.0342 y: 0.0257 control2X: -0.0659 y: 0.0441;
		rel_CubicBezierToX: -0.0908 y: 0.0166 control1X: -0.0290 y: 0.0111 control2X: -0.0593 y: 0.0166;
		rel_CubicBezierToX: -0.1250 y: -0.0288 control1X: -0.0358 y: 0.0000 control2X: -0.0775 y: -0.0096;
		rel_CubicBezierToX: -0.0078 y: -0.0029 control1X: -0.0036 y: -0.0013 control2X: -0.0062 y: -0.0022;
		rel_CubicBezierToX: -0.0108 y: -0.0039 control1X: -0.0023 y: -0.0010 control2X: -0.0059 y: -0.0023;
		rel_CubicBezierToX: -0.1216 y: -0.0303 control1X: -0.0504 y: -0.0202 control2X: -0.0910 y: -0.0303;
		rel_CubicBezierToX: -0.0849 y: 0.0191 control1X: -0.0286 y: 0.0000 control2X: -0.0569 y: 0.0064;
		rel_CubicBezierToX: -0.0899 y: 0.0591 control1X: -0.0280 y: 0.0123 control2X: -0.0580 y: 0.0320;
		rel_LineToX: 0.0000 y: -0.0870;
		rel_CubicBezierToX: 0.0948 y: -0.0551 control1X: 0.0342 y: -0.0257 control2X: 0.0658 y: -0.0441;
		rel_CubicBezierToX: 0.0913 y: -0.0171 control1X: 0.0292 y: -0.0114 control2X: 0.0597 y: -0.0171;
		rel_CubicBezierToX: 0.1254 y: 0.0293 control1X: 0.0358 y: 0.0000 control2X: 0.0776 y: 0.0097;
		rel_CubicBezierToX: 0.0074 y: 0.0029 control1X: 0.0033 y: 0.0013 control2X: 0.0057 y: 0.0023;
		rel_CubicBezierToX: 0.0107 y: 0.0039 control1X: 0.0026 y: 0.0010 control2X: 0.0062 y: 0.0023;
		rel_CubicBezierToX: 0.1216 y: 0.0303 control1X: 0.0505 y: 0.0202 control2X: 0.0910 y: 0.0303;
		rel_CubicBezierToX: 0.0830 y: -0.0186 control1X: 0.0280 y: 0.0000 control2X: 0.0557 y: -0.0062;
		rel_CubicBezierToX: 0.0918 y: -0.0595 control1X: 0.0277 y: -0.0123 control2X: 0.0583 y: -0.0322! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans128
	vectorGraphicsEngine
		rel_MoveToX: 0.5046 y: 0.2272;
		rel_LineToX: 0.2651 y: 0.2720;
		rel_LineToX: -0.0981 y: 0.0000;
		rel_LineToX: -0.2149 y: -0.1929;
		rel_LineToX: -0.2148 y: 0.1929;
		rel_LineToX: -0.0982 y: 0.0000;
		rel_LineToX: 0.2652 y: -0.2720;
		rel_LineToX: 0.0957 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans129
	vectorGraphicsEngine
		rel_MoveToX: 0.5475 y: 1.1222;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.5195 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.5195 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans130
	vectorGraphicsEngine
		rel_MoveToX: 0.4347 y: 0.2238;
		rel_LineToX: 0.0440 y: 0.0000;
		rel_LineToX: 0.1914 y: 0.1923;
		rel_LineToX: -0.0586 y: 0.0586;
		rel_LineToX: -0.1133 y: -0.1142;
		rel_LineToX: 0.0000 y: 0.5957;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5957;
		rel_LineToX: -0.1142 y: 0.1142;
		rel_LineToX: -0.0586 y: -0.0586;
		rel_LineToX: 0.1923 y: -0.1923! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans131
	vectorGraphicsEngine
		rel_MoveToX: 0.0866 y: 0.6647;
		rel_LineToX: 0.0000 y: -0.0440;
		rel_LineToX: 0.1919 y: -0.1919;
		rel_LineToX: 0.0586 y: 0.0586;
		rel_LineToX: -0.1138 y: 0.1138;
		rel_LineToX: 0.5952 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.5952 y: 0.0000;
		rel_LineToX: 0.1138 y: 0.1138;
		rel_LineToX: -0.0586 y: 0.0586;
		rel_LineToX: -0.1919 y: -0.1919! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans161
	vectorGraphicsEngine
		rel_MoveToX: 0.2878 y: 0.3512;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_MoveToX: 0.0000 y: 0.6050;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3198;
		rel_LineToX: 0.0103 y: -0.1744;
		rel_LineToX: 0.0791 y: 0.0000;
		rel_LineToX: 0.0098 y: 0.1744;
		rel_LineToX: 0.0000 y: 0.3198! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans162
	vectorGraphicsEngine
		rel_MoveToX: 0.3688 y: 0.8922;
		rel_LineToX: 0.0000 y: -0.4180;
		rel_CubicBezierToX: -0.1118 y: 0.0640 control1X: -0.0478 y: 0.0059 control2X: -0.0851 y: 0.0272;
		rel_CubicBezierToX: -0.0400 y: 0.1450 control1X: -0.0267 y: 0.0368 control2X: -0.0400 y: 0.0851;
		rel_CubicBezierToX: 0.0400 y: 0.1455 control1X: 0.0000 y: 0.0603 control2X: 0.0133 y: 0.1088;
		rel_CubicBezierToX: 0.1118 y: 0.0635 control1X: 0.0267 y: 0.0368 control2X: 0.0640 y: 0.0580;
		rel_MoveToX: 0.1861 y: -0.4619;
		rel_LineToX: 0.0000 y: 0.0840;
		rel_CubicBezierToX: -0.0694 y: -0.0303 control1X: -0.0241 y: -0.0134 control2X: -0.0472 y: -0.0235;
		rel_CubicBezierToX: -0.0664 y: -0.0117 control1X: -0.0221 y: -0.0068 control2X: -0.0443 y: -0.0107;
		rel_LineToX: -0.0005 y: 0.4219;
		rel_CubicBezierToX: 0.0674 y: -0.0127 control1X: 0.0228 y: -0.0017 control2X: 0.0453 y: -0.0059;
		rel_CubicBezierToX: 0.0689 y: -0.0293 control1X: 0.0225 y: -0.0069 control2X: 0.0454 y: -0.0166;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_CubicBezierToX: -0.0655 y: 0.0229 control1X: -0.0212 y: 0.0098 control2X: -0.0430 y: 0.0174;
		rel_CubicBezierToX: -0.0708 y: 0.0122 control1X: -0.0221 y: 0.0059 control2X: -0.0457 y: 0.0100;
		rel_LineToX: 0.0000 y: 0.1387;
		rel_LineToX: -0.0498 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1406;
		rel_CubicBezierToX: -0.1826 y: -0.0845 control1X: -0.0784 y: -0.0065 control2X: -0.1393 y: -0.0347;
		rel_CubicBezierToX: -0.0644 y: -0.2007 control1X: -0.0430 y: -0.0501 control2X: -0.0644 y: -0.1170;
		rel_CubicBezierToX: 0.0644 y: -0.2007 control1X: 0.0000 y: -0.0840 control2X: 0.0214 y: -0.1508;
		rel_CubicBezierToX: 0.1826 y: -0.0854 control1X: 0.0433 y: -0.0498 control2X: 0.1042 y: -0.0782;
		rel_LineToX: 0.0000 y: -0.1401;
		rel_LineToX: 0.0498 y: 0.0000;
		rel_LineToX: 0.0005 y: 0.1401;
		rel_CubicBezierToX: 0.0689 y: 0.0103 control1X: 0.0238 y: 0.0013 control2X: 0.0467 y: 0.0047;
		rel_CubicBezierToX: 0.0669 y: 0.0229 control1X: 0.0224 y: 0.0052 control2X: 0.0447 y: 0.0128! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans163
	vectorGraphicsEngine
		rel_MoveToX: 0.5759 y: 0.2433;
		rel_LineToX: 0.0000 y: 0.0889;
		rel_CubicBezierToX: -0.0704 y: -0.0298 control1X: -0.0248 y: -0.0134 control2X: -0.0482 y: -0.0233;
		rel_CubicBezierToX: -0.0629 y: -0.0103 control1X: -0.0221 y: -0.0069 control2X: -0.0431 y: -0.0103;
		rel_CubicBezierToX: -0.1006 y: 0.0381 control1X: -0.0482 y: 0.0000 control2X: -0.0818 y: 0.0127;
		rel_CubicBezierToX: -0.0284 y: 0.1411 control1X: -0.0189 y: 0.0251 control2X: -0.0284 y: 0.0721;
		rel_LineToX: 0.0000 y: 0.1050;
		rel_LineToX: 0.1910 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.1910 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2271;
		rel_LineToX: 0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4848 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.1152 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2271;
		rel_LineToX: -0.0972 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1162;
		rel_CubicBezierToX: 0.0522 y: -0.1865 control1X: 0.0000 y: -0.0850 control2X: 0.0174 y: -0.1471;
		rel_CubicBezierToX: 0.1656 y: -0.0596 control1X: 0.0349 y: -0.0397 control2X: 0.0900 y: -0.0596;
		rel_CubicBezierToX: 0.0664 y: 0.0078 control1X: 0.0198 y: 0.0000 control2X: 0.0420 y: 0.0026;
		rel_CubicBezierToX: 0.0757 y: 0.0215 control1X: 0.0247 y: 0.0049 control2X: 0.0499 y: 0.0121! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans164
	vectorGraphicsEngine
		rel_MoveToX: 0.6076 y: 0.2833;
		rel_LineToX: 0.0000 y: 0.1040;
		rel_CubicBezierToX: -0.0859 y: -0.0732 control1X: -0.0296 y: -0.0341 control2X: -0.0583 y: -0.0586;
		rel_CubicBezierToX: -0.0909 y: -0.0220 control1X: -0.0274 y: -0.0147 control2X: -0.0577 y: -0.0220;
		rel_CubicBezierToX: -0.1259 y: 0.0488 control1X: -0.0511 y: 0.0000 control2X: -0.0931 y: 0.0163;
		rel_CubicBezierToX: -0.0650 y: 0.1402 control1X: -0.0329 y: 0.0326 control2X: -0.0546 y: 0.0793;
		rel_LineToX: 0.2818 y: 0.0000;
		rel_LineToX: -0.0269 y: 0.0600;
		rel_LineToX: -0.2627 y: 0.0000;
		rel_CubicBezierToX: -0.0015 y: 0.0230 control1X: -0.0006 y: 0.0079 control2X: -0.0011 y: 0.0155;
		rel_CubicBezierToX: 0.0000 y: 0.0283 control1X: 0.0000 y: 0.0075 control2X: 0.0000 y: 0.0169;
		rel_CubicBezierToX: 0.0000 y: 0.0269 control1X: 0.0000 y: 0.0104 control2X: 0.0000 y: 0.0194;
		rel_CubicBezierToX: 0.0015 y: 0.0229 control1X: 0.0004 y: 0.0075 control2X: 0.0009 y: 0.0151;
		rel_LineToX: 0.2178 y: 0.0000;
		rel_LineToX: -0.0274 y: 0.0601;
		rel_LineToX: -0.1826 y: 0.0000;
		rel_CubicBezierToX: 0.0650 y: 0.1406 control1X: 0.0104 y: 0.0608 control2X: 0.0321 y: 0.1077;
		rel_CubicBezierToX: 0.1259 y: 0.0493 control1X: 0.0328 y: 0.0329 control2X: 0.0748 y: 0.0493;
		rel_CubicBezierToX: 0.0909 y: -0.0220 control1X: 0.0332 y: 0.0000 control2X: 0.0635 y: -0.0073;
		rel_CubicBezierToX: 0.0859 y: -0.0732 control1X: 0.0276 y: -0.0146 control2X: 0.0563 y: -0.0390;
		rel_LineToX: 0.0000 y: 0.1030;
		rel_CubicBezierToX: -0.0889 y: 0.0528 control1X: -0.0290 y: 0.0235 control2X: -0.0586 y: 0.0410;
		rel_CubicBezierToX: -0.0918 y: 0.0175 control1X: -0.0299 y: 0.0117 control2X: -0.0605 y: 0.0175;
		rel_CubicBezierToX: -0.1914 y: -0.0708 control1X: -0.0771 y: 0.0000 control2X: -0.1409 y: -0.0236;
		rel_CubicBezierToX: -0.0947 y: -0.1972 control1X: -0.0501 y: -0.0472 control2X: -0.0817 y: -0.1129;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0268 y: -0.0601;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_CubicBezierToX: -0.0005 y: -0.0224 control1X: 0.0000 y: -0.0075 control2X: -0.0002 y: -0.0150;
		rel_CubicBezierToX: 0.0000 y: -0.0274 control1X: 0.0000 y: -0.0079 control2X: 0.0000 y: -0.0170;
		rel_CubicBezierToX: 0.0000 y: -0.0283 control1X: 0.0000 y: -0.0114 control2X: 0.0000 y: -0.0208;
		rel_CubicBezierToX: 0.0005 y: -0.0230 control1X: 0.0003 y: -0.0078 control2X: 0.0005 y: -0.0155;
		rel_LineToX: -0.0947 y: 0.0000;
		rel_LineToX: 0.0268 y: -0.0600;
		rel_LineToX: 0.0762 y: 0.0000;
		rel_CubicBezierToX: 0.0952 y: -0.1963 control1X: 0.0130 y: -0.0837 control2X: 0.0448 y: -0.1491;
		rel_CubicBezierToX: 0.1909 y: -0.0708 control1X: 0.0505 y: -0.0472 control2X: 0.1141 y: -0.0708;
		rel_CubicBezierToX: 0.0928 y: 0.0176 control1X: 0.0319 y: 0.0000 control2X: 0.0629 y: 0.0059;
		rel_CubicBezierToX: 0.0879 y: 0.0517 control1X: 0.0303 y: 0.0114 control2X: 0.0596 y: 0.0286! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans165
	vectorGraphicsEngine
		rel_MoveToX: 0.6066 y: 0.7340;
		rel_LineToX: -0.2016 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2222;
		rel_LineToX: -0.0982 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2222;
		rel_LineToX: -0.2031 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0600;
		rel_LineToX: 0.2031 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0249;
		rel_LineToX: -0.0410 y: -0.0757;
		rel_LineToX: -0.1621 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0601;
		rel_LineToX: 0.1289 y: 0.0000;
		rel_LineToX: -0.1548 y: -0.2861;
		rel_LineToX: 0.0928 y: 0.0000;
		rel_LineToX: 0.1850 y: 0.3418;
		rel_LineToX: 0.1841 y: -0.3418;
		rel_LineToX: 0.0933 y: 0.0000;
		rel_LineToX: -0.1553 y: 0.2861;
		rel_LineToX: 0.1289 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0601;
		rel_LineToX: -0.1616 y: 0.0000;
		rel_LineToX: -0.0410 y: 0.0757;
		rel_LineToX: 0.0000 y: 0.0249;
		rel_LineToX: 0.2026 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0600! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans166
	vectorGraphicsEngine
		rel_MoveToX: 0.5729 y: 0.2511;
		rel_LineToX: 0.0000 y: 0.0962;
		rel_CubicBezierToX: -0.1059 y: -0.0400 control1X: -0.0374 y: -0.0179 control2X: -0.0727 y: -0.0313;
		rel_CubicBezierToX: -0.0962 y: -0.0132 control1X: -0.0332 y: -0.0088 control2X: -0.0653 y: -0.0132;
		rel_CubicBezierToX: -0.1245 y: 0.0312 control1X: -0.0537 y: 0.0000 control2X: -0.0952 y: 0.0104;
		rel_CubicBezierToX: -0.0435 y: 0.0889 control1X: -0.0290 y: 0.0209 control2X: -0.0435 y: 0.0505;
		rel_CubicBezierToX: 0.0288 y: 0.0732 control1X: 0.0000 y: 0.0322 control2X: 0.0096 y: 0.0566;
		rel_CubicBezierToX: 0.1104 y: 0.0396 control1X: 0.0195 y: 0.0163 control2X: 0.0563 y: 0.0295;
		rel_LineToX: 0.0595 y: 0.0122;
		rel_CubicBezierToX: 0.1626 y: 0.0742 control1X: 0.0736 y: 0.0140 control2X: 0.1278 y: 0.0387;
		rel_CubicBezierToX: 0.0528 y: 0.1416 control1X: 0.0352 y: 0.0352 control2X: 0.0528 y: 0.0824;
		rel_CubicBezierToX: -0.0713 y: 0.1607 control1X: 0.0000 y: 0.0706 control2X: -0.0238 y: 0.1242;
		rel_CubicBezierToX: -0.2080 y: 0.0546 control1X: -0.0472 y: 0.0364 control2X: -0.1166 y: 0.0546;
		rel_CubicBezierToX: -0.1104 y: -0.0117 control1X: -0.0345 y: 0.0000 control2X: -0.0713 y: -0.0039;
		rel_CubicBezierToX: -0.1206 y: -0.0346 control1X: -0.0387 y: -0.0078 control2X: -0.0789 y: -0.0193;
		rel_LineToX: 0.0000 y: -0.1016;
		rel_CubicBezierToX: 0.1177 y: 0.0508 control1X: 0.0401 y: 0.0225 control2X: 0.0793 y: 0.0394;
		rel_CubicBezierToX: 0.1133 y: 0.0171 control1X: 0.0384 y: 0.0114 control2X: 0.0762 y: 0.0171;
		rel_CubicBezierToX: 0.1303 y: -0.0332 control1X: 0.0563 y: 0.0000 control2X: 0.0997 y: -0.0111;
		rel_CubicBezierToX: 0.0459 y: -0.0948 control1X: 0.0306 y: -0.0222 control2X: 0.0459 y: -0.0538;
		rel_CubicBezierToX: -0.0332 y: -0.0840 control1X: 0.0000 y: -0.0358 control2X: -0.0110 y: -0.0638;
		rel_CubicBezierToX: -0.1079 y: -0.0454 control1X: -0.0218 y: -0.0201 control2X: -0.0577 y: -0.0353;
		rel_LineToX: -0.0600 y: -0.0117;
		rel_CubicBezierToX: -0.1597 y: -0.0688 control1X: -0.0736 y: -0.0146 control2X: -0.1268 y: -0.0376;
		rel_CubicBezierToX: -0.0493 y: -0.1304 control1X: -0.0329 y: -0.0313 control2X: -0.0493 y: -0.0747;
		rel_CubicBezierToX: 0.0679 y: -0.1523 control1X: 0.0000 y: -0.0645 control2X: 0.0226 y: -0.1152;
		rel_CubicBezierToX: 0.1879 y: -0.0557 control1X: 0.0455 y: -0.0372 control2X: 0.1082 y: -0.0557;
		rel_CubicBezierToX: 0.1045 y: 0.0093 control1X: 0.0342 y: 0.0000 control2X: 0.0691 y: 0.0031;
		rel_CubicBezierToX: 0.1089 y: 0.0278 control1X: 0.0355 y: 0.0062 control2X: 0.0718 y: 0.0154;
		rel_MoveToX: -0.2632 y: -0.0933;
		rel_LineToX: -0.1030 y: -0.1298;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: 0.0810 y: 0.0869;
		rel_LineToX: 0.0811 y: -0.0869;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: -0.1031 y: 0.1298;
		rel_LineToX: -0.0918 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans167
	vectorGraphicsEngine
		rel_MoveToX: 0.2228 y: 0.4992;
		rel_CubicBezierToX: -0.0459 y: 0.0444 control1X: -0.0205 y: 0.0149 control2X: -0.0358 y: 0.0297;
		rel_CubicBezierToX: -0.0151 y: 0.0444 control1X: -0.0101 y: 0.0146 control2X: -0.0151 y: 0.0294;
		rel_CubicBezierToX: 0.0337 y: 0.0698 control1X: 0.0000 y: 0.0248 control2X: 0.0112 y: 0.0480;
		rel_CubicBezierToX: 0.1562 y: 0.0982 control1X: 0.0228 y: 0.0215 control2X: 0.0749 y: 0.0542;
		rel_CubicBezierToX: 0.0459 y: -0.0440 control1X: 0.0205 y: -0.0147 control2X: 0.0358 y: -0.0293;
		rel_CubicBezierToX: 0.0152 y: -0.0449 control1X: 0.0101 y: -0.0149 control2X: 0.0152 y: -0.0299;
		rel_CubicBezierToX: -0.0352 y: -0.0698 control1X: 0.0000 y: -0.0244 control2X: -0.0118 y: -0.0477;
		rel_CubicBezierToX: -0.1548 y: -0.0981 control1X: -0.0231 y: -0.0221 control2X: -0.0747 y: -0.0549;
		rel_MoveToX: 0.2198 y: -0.2569;
		rel_LineToX: 0.0000 y: 0.0801;
		rel_CubicBezierToX: -0.0757 y: -0.0283 control1X: -0.0271 y: -0.0127 control2X: -0.0523 y: -0.0221;
		rel_CubicBezierToX: -0.0620 y: -0.0098 control1X: -0.0231 y: -0.0065 control2X: -0.0438 y: -0.0098;
		rel_CubicBezierToX: -0.0738 y: 0.0195 control1X: -0.0316 y: 0.0000 control2X: -0.0562 y: 0.0065;
		rel_CubicBezierToX: -0.0263 y: 0.0542 control1X: -0.0176 y: 0.0131 control2X: -0.0263 y: 0.0311;
		rel_CubicBezierToX: 0.1005 y: 0.1001 control1X: 0.0000 y: 0.0293 control2X: 0.0335 y: 0.0627;
		rel_CubicBezierToX: 0.0196 y: 0.0113 control1X: 0.0085 y: 0.0049 control2X: 0.0150 y: 0.0087;
		rel_CubicBezierToX: 0.1347 y: 0.0957 control1X: 0.0687 y: 0.0387 control2X: 0.1136 y: 0.0706;
		rel_CubicBezierToX: 0.0323 y: 0.0869 control1X: 0.0215 y: 0.0250 control2X: 0.0323 y: 0.0540;
		rel_CubicBezierToX: -0.0225 y: 0.0781 control1X: 0.0000 y: 0.0293 control2X: -0.0075 y: 0.0553;
		rel_CubicBezierToX: -0.0679 y: 0.0601 control1X: -0.0150 y: 0.0228 control2X: -0.0376 y: 0.0428;
		rel_CubicBezierToX: 0.0440 y: 0.0522 control1X: 0.0202 y: 0.0169 control2X: 0.0349 y: 0.0343;
		rel_CubicBezierToX: 0.0141 y: 0.0576 control1X: 0.0094 y: 0.0176 control2X: 0.0141 y: 0.0368;
		rel_CubicBezierToX: -0.0498 y: 0.1104 control1X: 0.0000 y: 0.0463 control2X: -0.0166 y: 0.0830;
		rel_CubicBezierToX: -0.1342 y: 0.0410 control1X: -0.0332 y: 0.0273 control2X: -0.0779 y: 0.0410;
		rel_CubicBezierToX: -0.0733 y: -0.0073 control1X: -0.0238 y: 0.0000 control2X: -0.0482 y: -0.0024;
		rel_CubicBezierToX: -0.0805 y: -0.0210 control1X: -0.0250 y: -0.0046 control2X: -0.0519 y: -0.0116;
		rel_LineToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.0786 y: 0.0283 control1X: 0.0283 y: 0.0127 control2X: 0.0545 y: 0.0221;
		rel_CubicBezierToX: 0.0644 y: 0.0098 control1X: 0.0241 y: 0.0065 control2X: 0.0455 y: 0.0098;
		rel_CubicBezierToX: 0.0772 y: -0.0205 control1X: 0.0332 y: 0.0000 control2X: 0.0589 y: -0.0069;
		rel_CubicBezierToX: 0.0278 y: -0.0576 control1X: 0.0185 y: -0.0137 control2X: 0.0278 y: -0.0329;
		rel_CubicBezierToX: -0.1079 y: -0.1099 control1X: 0.0000 y: -0.0332 control2X: -0.0360 y: -0.0699;
		rel_LineToX: -0.0122 y: -0.0068;
		rel_CubicBezierToX: -0.1358 y: -0.0957 control1X: -0.0697 y: -0.0391 control2X: -0.1149 y: -0.0710;
		rel_CubicBezierToX: -0.0312 y: -0.0874 control1X: -0.0208 y: -0.0251 control2X: -0.0312 y: -0.0542;
		rel_CubicBezierToX: 0.0224 y: -0.0791 control1X: 0.0000 y: -0.0297 control2X: 0.0075 y: -0.0560;
		rel_CubicBezierToX: 0.0674 y: -0.0591 control1X: 0.0153 y: -0.0235 control2X: 0.0378 y: -0.0432;
		rel_CubicBezierToX: -0.0434 y: -0.0489 control1X: -0.0192 y: -0.0144 control2X: -0.0337 y: -0.0306;
		rel_CubicBezierToX: -0.0142 y: -0.0610 control1X: -0.0095 y: -0.0182 control2X: -0.0142 y: -0.0385;
		rel_CubicBezierToX: 0.0488 y: -0.1020 control1X: 0.0000 y: -0.0423 control2X: 0.0163 y: -0.0763;
		rel_CubicBezierToX: 0.1299 y: -0.0391 control1X: 0.0326 y: -0.0261 control2X: 0.0759 y: -0.0391;
		rel_CubicBezierToX: 0.0728 y: 0.0073 control1X: 0.0238 y: 0.0000 control2X: 0.0480 y: 0.0024;
		rel_CubicBezierToX: 0.0762 y: 0.0210 control1X: 0.0250 y: 0.0046 control2X: 0.0504 y: 0.0116! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans168
	vectorGraphicsEngine
		rel_MoveToX: 0.4806 y: 0.4254;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: -0.0791 y: -0.0293 control1X: -0.0254 y: -0.0130 control2X: -0.0517 y: -0.0228;
		rel_CubicBezierToX: -0.0849 y: -0.0098 control1X: -0.0273 y: -0.0065 control2X: -0.0556 y: -0.0098;
		rel_CubicBezierToX: -0.1006 y: 0.0205 control1X: -0.0446 y: 0.0000 control2X: -0.0781 y: 0.0069;
		rel_CubicBezierToX: -0.0332 y: 0.0615 control1X: -0.0221 y: 0.0137 control2X: -0.0332 y: 0.0342;
		rel_CubicBezierToX: 0.0239 y: 0.0494 control1X: 0.0000 y: 0.0209 control2X: 0.0080 y: 0.0373;
		rel_CubicBezierToX: 0.0962 y: 0.0337 control1X: 0.0160 y: 0.0117 control2X: 0.0480 y: 0.0229;
		rel_LineToX: 0.0308 y: 0.0068;
		rel_CubicBezierToX: 0.1357 y: 0.0581 control1X: 0.0638 y: 0.0137 control2X: 0.1090 y: 0.0330;
		rel_CubicBezierToX: 0.0405 y: 0.1040 control1X: 0.0270 y: 0.0247 control2X: 0.0405 y: 0.0594;
		rel_CubicBezierToX: -0.0605 y: 0.1206 control1X: 0.0000 y: 0.0508 control2X: -0.0201 y: 0.0910;
		rel_CubicBezierToX: -0.1655 y: 0.0444 control1X: -0.0401 y: 0.0296 control2X: -0.0952 y: 0.0444;
		rel_CubicBezierToX: -0.0918 y: -0.0087 control1X: -0.0293 y: 0.0000 control2X: -0.0599 y: -0.0029;
		rel_CubicBezierToX: -0.1001 y: -0.0254 control1X: -0.0316 y: -0.0056 control2X: -0.0650 y: -0.0140;
		rel_LineToX: 0.0000 y: -0.0928;
		rel_CubicBezierToX: 0.0981 y: 0.0391 control1X: 0.0332 y: 0.0172 control2X: 0.0659 y: 0.0303;
		rel_CubicBezierToX: 0.0957 y: 0.0126 control1X: 0.0322 y: 0.0084 control2X: 0.0641 y: 0.0126;
		rel_CubicBezierToX: 0.0977 y: -0.0214 control1X: 0.0423 y: 0.0000 control2X: 0.0749 y: -0.0071;
		rel_CubicBezierToX: 0.0341 y: -0.0616 control1X: 0.0228 y: -0.0147 control2X: 0.0341 y: -0.0352;
		rel_CubicBezierToX: -0.0249 y: -0.0561 control1X: 0.0000 y: -0.0244 control2X: -0.0083 y: -0.0431;
		rel_CubicBezierToX: -0.1079 y: -0.0376 control1X: -0.0162 y: -0.0130 control2X: -0.0522 y: -0.0256;
		rel_LineToX: -0.0312 y: -0.0073;
		rel_CubicBezierToX: -0.1206 y: -0.0537 control1X: -0.0557 y: -0.0118 control2X: -0.0959 y: -0.0297;
		rel_CubicBezierToX: -0.0371 y: -0.1001 control1X: -0.0248 y: -0.0245 control2X: -0.0371 y: -0.0578;
		rel_CubicBezierToX: 0.0547 y: -0.1192 control1X: 0.0000 y: -0.0515 control2X: 0.0182 y: -0.0912;
		rel_CubicBezierToX: 0.1552 y: -0.0420 control1X: 0.0364 y: -0.0280 control2X: 0.0882 y: -0.0420;
		rel_CubicBezierToX: 0.0938 y: 0.0073 control1X: 0.0332 y: 0.0000 control2X: 0.0645 y: 0.0025;
		rel_CubicBezierToX: 0.0810 y: 0.0220 control1X: 0.0293 y: 0.0049 control2X: 0.0563 y: 0.0123;
		rel_MoveToX: -0.2177 y: -0.0854;
		rel_LineToX: -0.1197 y: -0.1836;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: 0.0879 y: 0.1196;
		rel_LineToX: 0.0879 y: -0.1196;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: -0.1197 y: 0.1836;
		rel_LineToX: -0.0722 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans169
	vectorGraphicsEngine
		rel_MoveToX: 0.5378 y: 0.2311;
		rel_CubicBezierToX: 0.1382 y: 0.0268 control1X: 0.0494 y: 0.0000 control2X: 0.0955 y: 0.0089;
		rel_CubicBezierToX: 0.1176 y: 0.0801 control1X: 0.0429 y: 0.0179 control2X: 0.0821 y: 0.0446;
		rel_CubicBezierToX: 0.0796 y: 0.1172 control1X: 0.0355 y: 0.0355 control2X: 0.0620 y: 0.0746;
		rel_CubicBezierToX: 0.0264 y: 0.1392 control1X: 0.0176 y: 0.0426 control2X: 0.0264 y: 0.0890;
		rel_CubicBezierToX: -0.0264 y: 0.1381 control1X: 0.0000 y: 0.0494 control2X: -0.0088 y: 0.0955;
		rel_CubicBezierToX: -0.0796 y: 0.1167 control1X: -0.0176 y: 0.0424 control2X: -0.0441 y: 0.0813;
		rel_CubicBezierToX: -0.1176 y: 0.0801 control1X: -0.0355 y: 0.0355 control2X: -0.0747 y: 0.0622;
		rel_CubicBezierToX: -0.1382 y: 0.0269 control1X: -0.0427 y: 0.0179 control2X: -0.0888 y: 0.0269;
		rel_CubicBezierToX: -0.1387 y: -0.0269 control1X: -0.0495 y: 0.0000 control2X: -0.0957 y: -0.0090;
		rel_CubicBezierToX: -0.1172 y: -0.0801 control1X: -0.0426 y: -0.0179 control2X: -0.0817 y: -0.0446;
		rel_CubicBezierToX: -0.0796 y: -0.1167 control1X: -0.0355 y: -0.0354 control2X: -0.0620 y: -0.0743;
		rel_CubicBezierToX: -0.0263 y: -0.1381 control1X: -0.0176 y: -0.0426 control2X: -0.0263 y: -0.0887;
		rel_CubicBezierToX: 0.0263 y: -0.1392 control1X: 0.0000 y: -0.0502 control2X: 0.0087 y: -0.0966;
		rel_CubicBezierToX: 0.0796 y: -0.1172 control1X: 0.0176 y: -0.0426 control2X: 0.0441 y: -0.0817;
		rel_CubicBezierToX: 0.1172 y: -0.0801 control1X: 0.0355 y: -0.0355 control2X: 0.0746 y: -0.0622;
		rel_CubicBezierToX: 0.1387 y: -0.0268 control1X: 0.0430 y: -0.0179 control2X: 0.0892 y: -0.0268;
		rel_MoveToX: 0.0000 y: 0.0503;
		rel_CubicBezierToX: -0.1192 y: 0.0229 control1X: -0.0427 y: 0.0000 control2X: -0.0824 y: 0.0076;
		rel_CubicBezierToX: -0.1010 y: 0.0689 control1X: -0.0368 y: 0.0153 control2X: -0.0704 y: 0.0383;
		rel_CubicBezierToX: -0.0694 y: 0.1020 control1X: -0.0306 y: 0.0306 control2X: -0.0538 y: 0.0646;
		rel_CubicBezierToX: -0.0234 y: 0.1192 control1X: -0.0156 y: 0.0371 control2X: -0.0234 y: 0.0768;
		rel_CubicBezierToX: 0.0234 y: 0.1186 control1X: 0.0000 y: 0.0420 control2X: 0.0078 y: 0.0815;
		rel_CubicBezierToX: 0.0694 y: 0.1011 control1X: 0.0156 y: 0.0368 control2X: 0.0388 y: 0.0705;
		rel_CubicBezierToX: 0.1010 y: 0.0693 control1X: 0.0306 y: 0.0306 control2X: 0.0642 y: 0.0537;
		rel_CubicBezierToX: 0.1192 y: 0.0230 control1X: 0.0368 y: 0.0153 control2X: 0.0765 y: 0.0230;
		rel_CubicBezierToX: 0.1196 y: -0.0230 control1X: 0.0429 y: 0.0000 control2X: 0.0828 y: -0.0077;
		rel_CubicBezierToX: 0.1016 y: -0.0693 control1X: 0.0371 y: -0.0156 control2X: 0.0710 y: -0.0387;
		rel_CubicBezierToX: 0.0678 y: -0.1006 control1X: 0.0302 y: -0.0303 control2X: 0.0529 y: -0.0638;
		rel_CubicBezierToX: 0.0230 y: -0.1191 control1X: 0.0153 y: -0.0368 control2X: 0.0230 y: -0.0765;
		rel_CubicBezierToX: -0.0230 y: -0.1202 control1X: 0.0000 y: -0.0433 control2X: -0.0077 y: -0.0834;
		rel_CubicBezierToX: -0.0678 y: -0.1010 control1X: -0.0153 y: -0.0371 control2X: -0.0379 y: -0.0708;
		rel_CubicBezierToX: -0.1016 y: -0.0689 control1X: -0.0306 y: -0.0306 control2X: -0.0645 y: -0.0536;
		rel_CubicBezierToX: -0.1196 y: -0.0229 control1X: -0.0368 y: -0.0153 control2X: -0.0767 y: -0.0229;
		rel_MoveToX: 0.1460 y: 0.1196;
		rel_LineToX: 0.0000 y: 0.0630;
		rel_CubicBezierToX: -0.0640 y: -0.0239 control1X: -0.0215 y: -0.0107 control2X: -0.0428 y: -0.0187;
		rel_CubicBezierToX: -0.0640 y: -0.0078 control1X: -0.0212 y: -0.0052 control2X: -0.0425 y: -0.0078;
		rel_CubicBezierToX: -0.1137 y: 0.0429 control1X: -0.0485 y: 0.0000 control2X: -0.0864 y: 0.0143;
		rel_CubicBezierToX: -0.0406 y: 0.1192 control1X: -0.0271 y: 0.0283 control2X: -0.0406 y: 0.0681;
		rel_CubicBezierToX: 0.0415 y: 0.1201 control1X: 0.0000 y: 0.0517 control2X: 0.0139 y: 0.0918;
		rel_CubicBezierToX: 0.1177 y: 0.0425 control1X: 0.0280 y: 0.0283 control2X: 0.0673 y: 0.0425;
		rel_CubicBezierToX: 0.0611 y: -0.0074 control1X: 0.0208 y: 0.0000 control2X: 0.0412 y: -0.0025;
		rel_CubicBezierToX: 0.0620 y: -0.0244 control1X: 0.0198 y: -0.0052 control2X: 0.0405 y: -0.0133;
		rel_LineToX: 0.0000 y: 0.0620;
		rel_CubicBezierToX: -0.0664 y: 0.0210 control1X: -0.0218 y: 0.0095 control2X: -0.0440 y: 0.0165;
		rel_CubicBezierToX: -0.0674 y: 0.0069 control1X: -0.0222 y: 0.0046 control2X: -0.0446 y: 0.0069;
		rel_CubicBezierToX: -0.1670 y: -0.0596 control1X: -0.0703 y: 0.0000 control2X: -0.1260 y: -0.0199;
		rel_CubicBezierToX: -0.0611 y: -0.1611 control1X: -0.0407 y: -0.0397 control2X: -0.0611 y: -0.0934;
		rel_CubicBezierToX: 0.0611 y: -0.1612 control1X: 0.0000 y: -0.0681 control2X: 0.0204 y: -0.1218;
		rel_CubicBezierToX: 0.1670 y: -0.0590 control1X: 0.0410 y: -0.0394 control2X: 0.0967 y: -0.0590;
		rel_CubicBezierToX: 0.0688 y: 0.0068 control1X: 0.0237 y: 0.0000 control2X: 0.0467 y: 0.0022;
		rel_CubicBezierToX: 0.0650 y: 0.0200 control1X: 0.0222 y: 0.0045 control2X: 0.0438 y: 0.0112! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans170
	vectorGraphicsEngine
		rel_MoveToX: 0.1056 y: 0.6671;
		rel_LineToX: 0.3360 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0601;
		rel_LineToX: -0.3360 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0601;
		rel_MoveToX: 0.3350 y: -0.2798;
		rel_LineToX: 0.0000 y: 0.2188;
		rel_LineToX: -0.0728 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0547;
		rel_CubicBezierToX: -0.0566 y: 0.0474 control1X: -0.0143 y: 0.0205 control2X: -0.0332 y: 0.0363;
		rel_CubicBezierToX: -0.0806 y: 0.0166 control1X: -0.0234 y: 0.0110 control2X: -0.0503 y: 0.0166;
		rel_CubicBezierToX: -0.0996 y: -0.0328 control1X: -0.0416 y: 0.0000 control2X: -0.0748 y: -0.0109;
		rel_CubicBezierToX: -0.0371 y: -0.0883 control1X: -0.0247 y: -0.0221 control2X: -0.0371 y: -0.0515;
		rel_CubicBezierToX: 0.0464 y: -0.0996 control1X: 0.0000 y: -0.0440 control2X: 0.0155 y: -0.0772;
		rel_CubicBezierToX: 0.1387 y: -0.0337 control1X: 0.0313 y: -0.0225 control2X: 0.0775 y: -0.0337;
		rel_LineToX: 0.0888 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0020;
		rel_CubicBezierToX: -0.0288 y: -0.0664 control1X: 0.0000 y: -0.0296 control2X: -0.0096 y: -0.0517;
		rel_CubicBezierToX: -0.0854 y: -0.0225 control1X: -0.0188 y: -0.0150 control2X: -0.0473 y: -0.0225;
		rel_CubicBezierToX: -0.0635 y: 0.0083 control1X: -0.0202 y: 0.0000 control2X: -0.0414 y: 0.0028;
		rel_CubicBezierToX: -0.0664 y: 0.0249 control1X: -0.0221 y: 0.0056 control2X: -0.0443 y: 0.0139;
		rel_LineToX: 0.0000 y: -0.0620;
		rel_CubicBezierToX: 0.0708 y: -0.0205 control1X: 0.0238 y: -0.0091 control2X: 0.0474 y: -0.0159;
		rel_CubicBezierToX: 0.0693 y: -0.0068 control1X: 0.0238 y: -0.0045 control2X: 0.0469 y: -0.0068;
		rel_CubicBezierToX: 0.1329 y: 0.0430 control1X: 0.0596 y: 0.0000 control2X: 0.1039 y: 0.0143;
		rel_CubicBezierToX: 0.0439 y: 0.1303 control1X: 0.0293 y: 0.0286 control2X: 0.0439 y: 0.0721;
		rel_MoveToX: -0.1348 y: 0.0298;
		rel_CubicBezierToX: -0.1093 y: 0.0166 control1X: -0.0524 y: 0.0000 control2X: -0.0888 y: 0.0056;
		rel_CubicBezierToX: -0.0308 y: 0.0547 control1X: -0.0205 y: 0.0108 control2X: -0.0308 y: 0.0290;
		rel_CubicBezierToX: 0.0239 y: 0.0503 control1X: 0.0000 y: 0.0208 control2X: 0.0080 y: 0.0376;
		rel_CubicBezierToX: 0.0640 y: 0.0186 control1X: 0.0160 y: 0.0124 control2X: 0.0373 y: 0.0186;
		rel_CubicBezierToX: 0.0825 y: -0.0352 control1X: 0.0338 y: 0.0000 control2X: 0.0613 y: -0.0118;
		rel_CubicBezierToX: 0.0317 y: -0.0908 control1X: 0.0212 y: -0.0234 control2X: 0.0317 y: -0.0537;
		rel_LineToX: 0.0000 y: -0.0142;
		rel_LineToX: -0.0620 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans171
	vectorGraphicsEngine
		rel_MoveToX: 0.5558 y: 0.4391;
		rel_LineToX: 0.0000 y: 0.0933;
		rel_LineToX: -0.1469 y: 0.1308;
		rel_LineToX: 0.1469 y: 0.1309;
		rel_LineToX: 0.0000 y: 0.0932;
		rel_LineToX: -0.2290 y: -0.2041;
		rel_LineToX: 0.0000 y: -0.0400;
		rel_LineToX: 0.2290 y: -0.2041;
		rel_MoveToX: -0.2119 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0933;
		rel_LineToX: -0.1470 y: 0.1308;
		rel_LineToX: 0.1470 y: 0.1309;
		rel_LineToX: 0.0000 y: 0.0932;
		rel_LineToX: -0.2290 y: -0.2041;
		rel_LineToX: 0.0000 y: -0.0400;
		rel_LineToX: 0.2290 y: -0.2041! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans172
	vectorGraphicsEngine
		rel_MoveToX: 0.1437 y: 0.5353;
		rel_LineToX: 0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2807;
		rel_LineToX: -0.0820 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1977;
		rel_LineToX: -0.5440 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans173
	vectorGraphicsEngine
		rel_MoveToX: 0.0866 y: 0.6422;
		rel_LineToX: 0.2632 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0801;
		rel_LineToX: -0.2632 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0801! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans174
	vectorGraphicsEngine
		rel_MoveToX: 0.5378 y: 0.2814;
		rel_CubicBezierToX: -0.1192 y: 0.0229 control1X: -0.0427 y: 0.0000 control2X: -0.0824 y: 0.0076;
		rel_CubicBezierToX: -0.1010 y: 0.0689 control1X: -0.0368 y: 0.0153 control2X: -0.0704 y: 0.0383;
		rel_CubicBezierToX: -0.0694 y: 0.1020 control1X: -0.0306 y: 0.0306 control2X: -0.0538 y: 0.0646;
		rel_CubicBezierToX: -0.0234 y: 0.1192 control1X: -0.0156 y: 0.0371 control2X: -0.0234 y: 0.0768;
		rel_CubicBezierToX: 0.0234 y: 0.1186 control1X: 0.0000 y: 0.0420 control2X: 0.0078 y: 0.0815;
		rel_CubicBezierToX: 0.0694 y: 0.1011 control1X: 0.0156 y: 0.0368 control2X: 0.0388 y: 0.0705;
		rel_CubicBezierToX: 0.1010 y: 0.0693 control1X: 0.0306 y: 0.0306 control2X: 0.0642 y: 0.0537;
		rel_CubicBezierToX: 0.1192 y: 0.0230 control1X: 0.0368 y: 0.0153 control2X: 0.0765 y: 0.0230;
		rel_CubicBezierToX: 0.1196 y: -0.0230 control1X: 0.0429 y: 0.0000 control2X: 0.0828 y: -0.0077;
		rel_CubicBezierToX: 0.1016 y: -0.0693 control1X: 0.0371 y: -0.0156 control2X: 0.0710 y: -0.0387;
		rel_CubicBezierToX: 0.0678 y: -0.1006 control1X: 0.0302 y: -0.0303 control2X: 0.0529 y: -0.0638;
		rel_CubicBezierToX: 0.0230 y: -0.1191 control1X: 0.0153 y: -0.0368 control2X: 0.0230 y: -0.0765;
		rel_CubicBezierToX: -0.0230 y: -0.1202 control1X: 0.0000 y: -0.0433 control2X: -0.0077 y: -0.0834;
		rel_CubicBezierToX: -0.0678 y: -0.1010 control1X: -0.0153 y: -0.0371 control2X: -0.0379 y: -0.0708;
		rel_CubicBezierToX: -0.1016 y: -0.0689 control1X: -0.0306 y: -0.0306 control2X: -0.0645 y: -0.0536;
		rel_CubicBezierToX: -0.1196 y: -0.0229 control1X: -0.0368 y: -0.0153 control2X: -0.0767 y: -0.0229;
		rel_MoveToX: 0.0000 y: -0.0503;
		rel_CubicBezierToX: 0.1382 y: 0.0268 control1X: 0.0494 y: 0.0000 control2X: 0.0955 y: 0.0089;
		rel_CubicBezierToX: 0.1176 y: 0.0801 control1X: 0.0429 y: 0.0179 control2X: 0.0821 y: 0.0446;
		rel_CubicBezierToX: 0.0796 y: 0.1172 control1X: 0.0355 y: 0.0355 control2X: 0.0620 y: 0.0746;
		rel_CubicBezierToX: 0.0264 y: 0.1392 control1X: 0.0176 y: 0.0426 control2X: 0.0264 y: 0.0890;
		rel_CubicBezierToX: -0.0264 y: 0.1381 control1X: 0.0000 y: 0.0494 control2X: -0.0088 y: 0.0955;
		rel_CubicBezierToX: -0.0796 y: 0.1167 control1X: -0.0176 y: 0.0424 control2X: -0.0441 y: 0.0813;
		rel_CubicBezierToX: -0.1176 y: 0.0801 control1X: -0.0355 y: 0.0355 control2X: -0.0747 y: 0.0622;
		rel_CubicBezierToX: -0.1382 y: 0.0269 control1X: -0.0427 y: 0.0179 control2X: -0.0888 y: 0.0269;
		rel_CubicBezierToX: -0.1387 y: -0.0269 control1X: -0.0495 y: 0.0000 control2X: -0.0957 y: -0.0090;
		rel_CubicBezierToX: -0.1172 y: -0.0801 control1X: -0.0426 y: -0.0179 control2X: -0.0817 y: -0.0446;
		rel_CubicBezierToX: -0.0796 y: -0.1167 control1X: -0.0355 y: -0.0354 control2X: -0.0620 y: -0.0743;
		rel_CubicBezierToX: -0.0263 y: -0.1381 control1X: -0.0176 y: -0.0426 control2X: -0.0263 y: -0.0887;
		rel_CubicBezierToX: 0.0263 y: -0.1392 control1X: 0.0000 y: -0.0502 control2X: 0.0087 y: -0.0966;
		rel_CubicBezierToX: 0.0796 y: -0.1172 control1X: 0.0176 y: -0.0426 control2X: 0.0441 y: -0.0817;
		rel_CubicBezierToX: 0.1172 y: -0.0801 control1X: 0.0355 y: -0.0355 control2X: 0.0746 y: -0.0622;
		rel_CubicBezierToX: 0.1387 y: -0.0268 control1X: 0.0430 y: -0.0179 control2X: 0.0892 y: -0.0268;
		rel_MoveToX: -0.0132 y: 0.2021;
		rel_LineToX: -0.0601 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1348;
		rel_LineToX: 0.0601 y: 0.0000;
		rel_CubicBezierToX: 0.0747 y: -0.0151 control1X: 0.0358 y: 0.0000 control2X: 0.0607 y: -0.0050;
		rel_CubicBezierToX: 0.0215 y: -0.0518 control1X: 0.0143 y: -0.0101 control2X: 0.0215 y: -0.0274;
		rel_CubicBezierToX: -0.0215 y: -0.0522 control1X: 0.0000 y: -0.0247 control2X: -0.0072 y: -0.0422;
		rel_CubicBezierToX: -0.0747 y: -0.0157 control1X: -0.0143 y: -0.0105 control2X: -0.0392 y: -0.0157;
		rel_MoveToX: 0.0034 y: -0.0503;
		rel_CubicBezierToX: 0.1284 y: 0.0293 control1X: 0.0573 y: 0.0000 control2X: 0.1001 y: 0.0098;
		rel_CubicBezierToX: 0.0425 y: 0.0879 control1X: 0.0283 y: 0.0192 control2X: 0.0425 y: 0.0485;
		rel_CubicBezierToX: -0.0259 y: 0.0694 control1X: 0.0000 y: 0.0280 control2X: -0.0086 y: 0.0511;
		rel_CubicBezierToX: -0.0722 y: 0.0356 control1X: -0.0169 y: 0.0182 control2X: -0.0410 y: 0.0301;
		rel_CubicBezierToX: 0.0278 y: 0.0230 control1X: 0.0078 y: 0.0026 control2X: 0.0170 y: 0.0103;
		rel_CubicBezierToX: 0.0390 y: 0.0542 control1X: 0.0111 y: 0.0123 control2X: 0.0241 y: 0.0304;
		rel_LineToX: 0.0669 y: 0.1088;
		rel_LineToX: -0.0839 y: 0.0000;
		rel_LineToX: -0.0630 y: -0.1020;
		rel_CubicBezierToX: -0.0469 y: -0.0591 control1X: -0.0192 y: -0.0313 control2X: -0.0349 y: -0.0510;
		rel_CubicBezierToX: -0.0439 y: -0.0127 control1X: -0.0117 y: -0.0085 control2X: -0.0264 y: -0.0127;
		rel_LineToX: -0.0323 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1738;
		rel_LineToX: -0.0757 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.4082;
		rel_LineToX: 0.1392 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans175
	vectorGraphicsEngine
		rel_MoveToX: 0.1418 y: 0.2111;
		rel_LineToX: 0.2920 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0722;
		rel_LineToX: -0.2920 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0722! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans176
	vectorGraphicsEngine
		rel_MoveToX: 0.2878 y: 0.2770;
		rel_CubicBezierToX: -0.0660 y: 0.0273 control1X: -0.0261 y: 0.0000 control2X: -0.0480 y: 0.0091;
		rel_CubicBezierToX: -0.0268 y: 0.0659 control1X: -0.0179 y: 0.0179 control2X: -0.0268 y: 0.0399;
		rel_CubicBezierToX: 0.0268 y: 0.0655 control1X: 0.0000 y: 0.0258 control2X: 0.0089 y: 0.0476;
		rel_CubicBezierToX: 0.0660 y: 0.0263 control1X: 0.0180 y: 0.0176 control2X: 0.0399 y: 0.0263;
		rel_CubicBezierToX: 0.0659 y: -0.0263 control1X: 0.0260 y: 0.0000 control2X: 0.0480 y: -0.0087;
		rel_CubicBezierToX: 0.0268 y: -0.0655 control1X: 0.0179 y: -0.0179 control2X: 0.0268 y: -0.0397;
		rel_CubicBezierToX: -0.0273 y: -0.0659 control1X: 0.0000 y: -0.0257 control2X: -0.0091 y: -0.0476;
		rel_CubicBezierToX: -0.0654 y: -0.0273 control1X: -0.0179 y: -0.0182 control2X: -0.0397 y: -0.0273;
		rel_MoveToX: 0.0000 y: -0.0630;
		rel_CubicBezierToX: 0.0600 y: 0.0122 control1X: 0.0208 y: 0.0000 control2X: 0.0408 y: 0.0041;
		rel_CubicBezierToX: 0.0498 y: 0.0342 control1X: 0.0192 y: 0.0078 control2X: 0.0358 y: 0.0192;
		rel_CubicBezierToX: 0.0337 y: 0.0498 control1X: 0.0150 y: 0.0146 control2X: 0.0262 y: 0.0312;
		rel_CubicBezierToX: 0.0113 y: 0.0600 control1X: 0.0075 y: 0.0185 control2X: 0.0113 y: 0.0386;
		rel_CubicBezierToX: -0.0455 y: 0.1094 control1X: 0.0000 y: 0.0430 control2X: -0.0152 y: 0.0795;
		rel_CubicBezierToX: -0.1103 y: 0.0445 control1X: -0.0299 y: 0.0296 control2X: -0.0667 y: 0.0445;
		rel_CubicBezierToX: -0.1099 y: -0.0440 control1X: -0.0440 y: 0.0000 control2X: -0.0806 y: -0.0147;
		rel_CubicBezierToX: -0.0439 y: -0.1099 control1X: -0.0293 y: -0.0293 control2X: -0.0439 y: -0.0659;
		rel_CubicBezierToX: 0.0449 y: -0.1108 control1X: 0.0000 y: -0.0436 control2X: 0.0150 y: -0.0805;
		rel_CubicBezierToX: 0.1099 y: -0.0454 control1X: 0.0300 y: -0.0303 control2X: 0.0666 y: -0.0454! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans177
	vectorGraphicsEngine
		rel_MoveToX: 0.4977 y: 0.3292;
		rel_LineToX: 0.0000 y: 0.1890;
		rel_LineToX: 0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1890;
		rel_LineToX: -0.0820 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1890;
		rel_LineToX: -0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1890;
		rel_LineToX: 0.0820 y: 0.0000;
		rel_MoveToX: -0.3540 y: 0.5440;
		rel_LineToX: 0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans178
	vectorGraphicsEngine
		rel_MoveToX: 0.1686 y: 0.5743;
		rel_LineToX: 0.2071 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0557;
		rel_LineToX: -0.2920 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0537;
		rel_CubicBezierToX: 0.0473 y: -0.0425 control1X: 0.0110 y: -0.0101 control2X: 0.0268 y: -0.0243;
		rel_CubicBezierToX: 0.1680 y: -0.1948 control1X: 0.1120 y: -0.0993 control2X: 0.1680 y: -0.1642;
		rel_CubicBezierToX: -0.0254 y: -0.0518 control1X: 0.0000 y: -0.0215 control2X: -0.0085 y: -0.0387;
		rel_CubicBezierToX: -0.0669 y: -0.0200 control1X: -0.0169 y: -0.0133 control2X: -0.0392 y: -0.0200;
		rel_CubicBezierToX: -0.0552 y: 0.0088 control1X: -0.0169 y: 0.0000 control2X: -0.0353 y: 0.0029;
		rel_CubicBezierToX: -0.0649 y: 0.0254 control1X: -0.0198 y: 0.0055 control2X: -0.0415 y: 0.0140;
		rel_LineToX: 0.0000 y: -0.0601;
		rel_CubicBezierToX: 0.0698 y: -0.0205 control1X: 0.0251 y: -0.0091 control2X: 0.0483 y: -0.0159;
		rel_CubicBezierToX: 0.0606 y: -0.0068 control1X: 0.0218 y: -0.0045 control2X: 0.0420 y: -0.0068;
		rel_CubicBezierToX: 0.1132 y: 0.0322 control1X: 0.0472 y: 0.0000 control2X: 0.0849 y: 0.0107;
		rel_CubicBezierToX: 0.0425 y: 0.0850 control1X: 0.0284 y: 0.0215 control2X: 0.0425 y: 0.0498;
		rel_CubicBezierToX: -0.1616 y: 0.2065 control1X: 0.0000 y: 0.0452 control2X: -0.0538 y: 0.1141;
		rel_CubicBezierToX: -0.0425 y: 0.0366 control1X: -0.0182 y: 0.0156 control2X: -0.0324 y: 0.0279! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans179
	vectorGraphicsEngine
		rel_MoveToX: 0.2936 y: 0.4093;
		rel_CubicBezierToX: 0.0694 y: 0.0357 control1X: 0.0300 y: 0.0059 control2X: 0.0531 y: 0.0177;
		rel_CubicBezierToX: 0.0249 y: 0.0664 control1X: 0.0166 y: 0.0175 control2X: 0.0249 y: 0.0397;
		rel_CubicBezierToX: -0.0464 y: 0.0932 control1X: 0.0000 y: 0.0403 control2X: -0.0155 y: 0.0714;
		rel_CubicBezierToX: -0.1328 y: 0.0327 control1X: -0.0309 y: 0.0218 control2X: -0.0752 y: 0.0327;
		rel_CubicBezierToX: -0.0586 y: -0.0048 control1X: -0.0186 y: 0.0000 control2X: -0.0381 y: -0.0016;
		rel_CubicBezierToX: -0.0645 y: -0.0152 control1X: -0.0202 y: -0.0036 control2X: -0.0417 y: -0.0087;
		rel_LineToX: 0.0000 y: -0.0591;
		rel_CubicBezierToX: 0.0542 y: 0.0196 control1X: 0.0169 y: 0.0088 control2X: 0.0350 y: 0.0153;
		rel_CubicBezierToX: 0.0620 y: 0.0063 control1X: 0.0195 y: 0.0042 control2X: 0.0402 y: 0.0063;
		rel_CubicBezierToX: 0.0825 y: -0.0190 control1X: 0.0355 y: 0.0000 control2X: 0.0630 y: -0.0063;
		rel_CubicBezierToX: 0.0293 y: -0.0537 control1X: 0.0196 y: -0.0131 control2X: 0.0293 y: -0.0310;
		rel_CubicBezierToX: -0.0273 y: -0.0547 control1X: 0.0000 y: -0.0241 control2X: -0.0091 y: -0.0424;
		rel_CubicBezierToX: -0.0796 y: -0.0186 control1X: -0.0179 y: -0.0124 control2X: -0.0444 y: -0.0186;
		rel_LineToX: -0.0420 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0527;
		rel_LineToX: 0.0459 y: 0.0000;
		rel_CubicBezierToX: 0.0694 y: -0.0152 control1X: 0.0306 y: 0.0000 control2X: 0.0537 y: -0.0051;
		rel_CubicBezierToX: 0.0239 y: -0.0449 control1X: 0.0159 y: -0.0104 control2X: 0.0239 y: -0.0253;
		rel_CubicBezierToX: -0.0244 y: -0.0429 control1X: 0.0000 y: -0.0189 control2X: -0.0082 y: -0.0332;
		rel_CubicBezierToX: -0.0708 y: -0.0152 control1X: -0.0163 y: -0.0101 control2X: -0.0399 y: -0.0152;
		rel_CubicBezierToX: -0.0445 y: 0.0044 control1X: -0.0131 y: 0.0000 control2X: -0.0279 y: 0.0015;
		rel_CubicBezierToX: -0.0644 y: 0.0156 control1X: -0.0166 y: 0.0029 control2X: -0.0381 y: 0.0081;
		rel_LineToX: 0.0000 y: -0.0561;
		rel_CubicBezierToX: 0.0669 y: -0.0127 control1X: 0.0237 y: -0.0055 control2X: 0.0460 y: -0.0098;
		rel_CubicBezierToX: 0.0581 y: -0.0044 control1X: 0.0208 y: -0.0029 control2X: 0.0402 y: -0.0044;
		rel_CubicBezierToX: 0.1113 y: 0.0288 control1X: 0.0469 y: 0.0000 control2X: 0.0840 y: 0.0096;
		rel_CubicBezierToX: 0.0415 y: 0.0772 control1X: 0.0277 y: 0.0192 control2X: 0.0415 y: 0.0449;
		rel_CubicBezierToX: -0.0220 y: 0.0571 control1X: 0.0000 y: 0.0224 control2X: -0.0073 y: 0.0415;
		rel_CubicBezierToX: -0.0620 y: 0.0322 control1X: -0.0146 y: 0.0156 control2X: -0.0353 y: 0.0263! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans180
	vectorGraphicsEngine
		rel_MoveToX: 0.0939 y: 0.2272;
		rel_LineToX: 0.5728 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0752;
		rel_LineToX: -0.4610 y: 0.5708;
		rel_LineToX: 0.4722 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.5952 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.4609 y: -0.5708;
		rel_LineToX: -0.4497 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_MoveToX: 0.2407 y: -0.0694;
		rel_LineToX: -0.1030 y: -0.1298;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: 0.0810 y: 0.0869;
		rel_LineToX: 0.0811 y: -0.0869;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: -0.1031 y: 0.1298;
		rel_LineToX: -0.0918 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans181
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 1.1642;
		rel_LineToX: 0.0000 y: -0.7549;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3399;
		rel_CubicBezierToX: 0.0337 y: 0.1069 control1X: 0.0000 y: 0.0472 control2X: 0.0112 y: 0.0828;
		rel_CubicBezierToX: 0.0996 y: 0.0361 control1X: 0.0224 y: 0.0241 control2X: 0.0556 y: 0.0361;
		rel_CubicBezierToX: 0.1084 y: -0.0410 control1X: 0.0481 y: 0.0000 control2X: 0.0843 y: -0.0137;
		rel_CubicBezierToX: 0.0366 y: -0.1230 control1X: 0.0244 y: -0.0273 control2X: 0.0366 y: -0.0684;
		rel_LineToX: 0.0000 y: -0.3189;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4209;
		rel_CubicBezierToX: 0.0083 y: 0.0435 control1X: 0.0000 y: 0.0195 control2X: 0.0028 y: 0.0340;
		rel_CubicBezierToX: 0.0269 y: 0.0136 control1X: 0.0059 y: 0.0091 control2X: 0.0148 y: 0.0136;
		rel_CubicBezierToX: 0.0122 y: -0.0024 control1X: 0.0029 y: 0.0000 control2X: 0.0070 y: -0.0008;
		rel_CubicBezierToX: 0.0215 y: -0.0088 control1X: 0.0052 y: -0.0020 control2X: 0.0124 y: -0.0049;
		rel_LineToX: 0.0000 y: 0.0723;
		rel_CubicBezierToX: -0.0381 y: 0.0166 control1X: -0.0134 y: 0.0075 control2X: -0.0261 y: 0.0130;
		rel_CubicBezierToX: -0.0347 y: 0.0053 control1X: -0.0117 y: 0.0036 control2X: -0.0233 y: 0.0053;
		rel_CubicBezierToX: -0.0537 y: -0.0190 control1X: -0.0224 y: 0.0000 control2X: -0.0403 y: -0.0063;
		rel_CubicBezierToX: -0.0273 y: -0.0581 control1X: -0.0133 y: -0.0127 control2X: -0.0224 y: -0.0321;
		rel_CubicBezierToX: -0.0601 y: 0.0581 control1X: -0.0163 y: 0.0257 control2X: -0.0363 y: 0.0451;
		rel_CubicBezierToX: -0.0830 y: 0.0190 control1X: -0.0234 y: 0.0127 control2X: -0.0511 y: 0.0190;
		rel_CubicBezierToX: -0.0850 y: -0.0190 control1X: -0.0332 y: 0.0000 control2X: -0.0615 y: -0.0063;
		rel_CubicBezierToX: -0.0551 y: -0.0571 control1X: -0.0231 y: -0.0127 control2X: -0.0415 y: -0.0317;
		rel_LineToX: 0.0000 y: 0.2700;
		rel_LineToX: -0.0899 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans182
	vectorGraphicsEngine
		rel_MoveToX: 0.3468 y: 0.2272;
		rel_LineToX: 0.2188 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.8252;
		rel_LineToX: -0.0688 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7652;
		rel_LineToX: -0.0928 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7652;
		rel_LineToX: -0.0694 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.4131;
		rel_CubicBezierToX: -0.1626 y: -0.0620 control1X: -0.0699 y: -0.0055 control2X: -0.1241 y: -0.0262;
		rel_CubicBezierToX: -0.0571 y: -0.1441 control1X: -0.0380 y: -0.0362 control2X: -0.0571 y: -0.0842;
		rel_CubicBezierToX: 0.0635 y: -0.1494 control1X: 0.0000 y: -0.0618 control2X: 0.0212 y: -0.1116;
		rel_CubicBezierToX: 0.1684 y: -0.0566 control1X: 0.0423 y: -0.0377 control2X: 0.0985 y: -0.0566! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans183
	vectorGraphicsEngine
		rel_MoveToX: 0.1447 y: 0.5470;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans184
	vectorGraphicsEngine
		rel_MoveToX: 0.0929 y: 0.4093;
		rel_LineToX: 0.4268 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0820;
		rel_LineToX: -0.3379 y: 0.3931;
		rel_LineToX: 0.3379 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0718;
		rel_LineToX: -0.4390 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0820;
		rel_LineToX: 0.3379 y: -0.3931;
		rel_LineToX: -0.3257 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0718;
		rel_MoveToX: 0.1719 y: -0.0693;
		rel_LineToX: -0.1196 y: -0.1836;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: 0.0879 y: 0.1196;
		rel_LineToX: 0.0878 y: -0.1196;
		rel_LineToX: 0.0679 y: 0.0000;
		rel_LineToX: -0.1196 y: 0.1836;
		rel_LineToX: -0.0723 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans185
	vectorGraphicsEngine
		rel_MoveToX: 0.1139 y: 0.5763;
		rel_LineToX: 0.0996 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2983;
		rel_LineToX: -0.1088 y: 0.0200;
		rel_LineToX: 0.0000 y: -0.0567;
		rel_LineToX: 0.1123 y: -0.0190;
		rel_LineToX: 0.0669 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3540;
		rel_LineToX: 0.1001 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0537;
		rel_LineToX: -0.2701 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0537! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans186
	vectorGraphicsEngine
		rel_MoveToX: 0.1056 y: 0.6671;
		rel_LineToX: 0.3360 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0601;
		rel_LineToX: -0.3360 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0601;
		rel_MoveToX: 0.1680 y: -0.4531;
		rel_CubicBezierToX: 0.1377 y: 0.0542 control1X: 0.0583 y: 0.0000 control2X: 0.1042 y: 0.0181;
		rel_CubicBezierToX: 0.0503 y: 0.1470 control1X: 0.0335 y: 0.0358 control2X: 0.0503 y: 0.0848;
		rel_CubicBezierToX: -0.0503 y: 0.1469 control1X: 0.0000 y: 0.0621 control2X: -0.0168 y: 0.1111;
		rel_CubicBezierToX: -0.1377 y: 0.0533 control1X: -0.0335 y: 0.0355 control2X: -0.0794 y: 0.0533;
		rel_CubicBezierToX: -0.1382 y: -0.0537 control1X: -0.0583 y: 0.0000 control2X: -0.1043 y: -0.0179;
		rel_CubicBezierToX: -0.0508 y: -0.1465 control1X: -0.0338 y: -0.0359 control2X: -0.0508 y: -0.0847;
		rel_CubicBezierToX: 0.0508 y: -0.1470 control1X: 0.0000 y: -0.0622 control2X: 0.0170 y: -0.1112;
		rel_CubicBezierToX: 0.1382 y: -0.0542 control1X: 0.0339 y: -0.0361 control2X: 0.0799 y: -0.0542;
		rel_MoveToX: 0.0000 y: 0.0561;
		rel_CubicBezierToX: -0.0820 y: 0.0396 control1X: -0.0342 y: 0.0000 control2X: -0.0615 y: 0.0132;
		rel_CubicBezierToX: -0.0308 y: 0.1055 control1X: -0.0205 y: 0.0260 control2X: -0.0308 y: 0.0612;
		rel_CubicBezierToX: 0.0308 y: 0.1040 control1X: 0.0000 y: 0.0433 control2X: 0.0103 y: 0.0779;
		rel_CubicBezierToX: 0.0820 y: 0.0390 control1X: 0.0208 y: 0.0260 control2X: 0.0482 y: 0.0390;
		rel_CubicBezierToX: 0.0815 y: -0.0390 control1X: 0.0342 y: 0.0000 control2X: 0.0614 y: -0.0130;
		rel_CubicBezierToX: 0.0308 y: -0.1040 control1X: 0.0206 y: -0.0261 control2X: 0.0308 y: -0.0607;
		rel_CubicBezierToX: -0.0303 y: -0.1060 control1X: 0.0000 y: -0.0446 control2X: -0.0101 y: -0.0799;
		rel_CubicBezierToX: -0.0820 y: -0.0391 control1X: -0.0201 y: -0.0260 control2X: -0.0475 y: -0.0391! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans187
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4391;
		rel_LineToX: 0.2290 y: 0.2041;
		rel_LineToX: 0.0000 y: 0.0400;
		rel_LineToX: -0.2290 y: 0.2041;
		rel_LineToX: 0.0000 y: -0.0932;
		rel_LineToX: 0.1470 y: -0.1309;
		rel_LineToX: -0.1470 y: -0.1308;
		rel_LineToX: 0.0000 y: -0.0933;
		rel_MoveToX: 0.2119 y: 0.0000;
		rel_LineToX: 0.2290 y: 0.2041;
		rel_LineToX: 0.0000 y: 0.0400;
		rel_LineToX: -0.2290 y: 0.2041;
		rel_LineToX: 0.0000 y: -0.0932;
		rel_LineToX: 0.1470 y: -0.1309;
		rel_LineToX: -0.1470 y: -0.1308;
		rel_LineToX: 0.0000 y: -0.0933! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans188
	vectorGraphicsEngine
		rel_MoveToX: 1.0348 y: 0.2272;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.5200 y: 0.0000;
		rel_CubicBezierToX: -0.3208 y: -0.0928 control1X: -0.1409 y: 0.0000 control2X: -0.2479 y: -0.0309;
		rel_CubicBezierToX: -0.1089 y: -0.2725 control1X: -0.0726 y: -0.0618 control2X: -0.1089 y: -0.1527;
		rel_CubicBezierToX: 0.1089 y: -0.2709 control1X: 0.0000 y: -0.1188 control2X: 0.0363 y: -0.2091;
		rel_CubicBezierToX: 0.3208 y: -0.0928 control1X: 0.0729 y: -0.0619 control2X: 0.1799 y: -0.0928;
		rel_LineToX: 0.5112 y: 0.0000;
		rel_MoveToX: -0.4609 y: 0.0830;
		rel_LineToX: -0.0630 y: 0.0000;
		rel_CubicBezierToX: -0.2349 y: 0.0703 control1X: -0.1045 y: 0.0000 control2X: -0.1828 y: 0.0234;
		rel_CubicBezierToX: -0.0781 y: 0.2104 control1X: -0.0520 y: 0.0465 control2X: -0.0781 y: 0.1167;
		rel_CubicBezierToX: 0.0781 y: 0.2120 control1X: 0.0000 y: 0.0941 control2X: 0.0261 y: 0.1648;
		rel_CubicBezierToX: 0.2349 y: 0.0703 control1X: 0.0521 y: 0.0468 control2X: 0.1304 y: 0.0703;
		rel_LineToX: 0.0630 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5630! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans189
	vectorGraphicsEngine
		rel_MoveToX: 0.9176 y: 0.6339;
		rel_CubicBezierToX: -0.0410 y: -0.1177 control1X: -0.0006 y: -0.0495 control2X: -0.0143 y: -0.0887;
		rel_CubicBezierToX: -0.1069 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0623 y: -0.0439;
		rel_CubicBezierToX: -0.1201 y: 0.0425 control1X: -0.0498 y: 0.0000 control2X: -0.0898 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3198 y: -0.0005;
		rel_MoveToX: 0.0904 y: 0.0264;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1391 y: 0.0484 control1X: 0.0332 y: 0.0323 control2X: 0.0795 y: 0.0484;
		rel_CubicBezierToX: 0.1006 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0681 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0996 y: 0.0313 control1X: -0.0326 y: 0.0137 control2X: -0.0658 y: 0.0241;
		rel_CubicBezierToX: -0.1026 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0681 y: 0.0107;
		rel_CubicBezierToX: -0.1372 y: -0.0268 control1X: -0.0520 y: 0.0000 control2X: -0.0978 y: -0.0089;
		rel_CubicBezierToX: -0.0986 y: -0.0801 control1X: -0.0394 y: -0.0179 control2X: -0.0722 y: -0.0446;
		rel_CubicBezierToX: -0.0859 y: 0.0806 control1X: -0.0231 y: 0.0358 control2X: -0.0518 y: 0.0627;
		rel_CubicBezierToX: -0.1192 y: 0.0263 control1X: -0.0339 y: 0.0176 control2X: -0.0736 y: 0.0263;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_CubicBezierToX: 0.1196 y: 0.0274 control1X: 0.0456 y: 0.0000 control2X: 0.0855 y: 0.0091;
		rel_CubicBezierToX: 0.0840 y: 0.0796 control1X: 0.0346 y: 0.0179 control2X: 0.0625 y: 0.0444;
		rel_CubicBezierToX: 0.0948 y: -0.0796 control1X: 0.0254 y: -0.0349 control2X: 0.0570 y: -0.0614;
		rel_CubicBezierToX: 0.1264 y: -0.0274 control1X: 0.0377 y: -0.0183 control2X: 0.0799 y: -0.0274;
		rel_CubicBezierToX: 0.1748 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1319 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.6641 y: -0.1880;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans190
	vectorGraphicsEngine
		rel_MoveToX: 0.0358 y: 0.2272;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: 0.2021 y: 0.2998;
		rel_LineToX: 0.2007 y: -0.2998;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: -0.2579 y: 0.3818;
		rel_LineToX: 0.0000 y: 0.3472;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3472;
		rel_LineToX: -0.2578 y: -0.3818;
		rel_MoveToX: 0.3540 y: -0.1841;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans191
	vectorGraphicsEngine
		rel_MoveToX: 0.2819 y: 0.4230;
		rel_LineToX: 0.0928 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0762;
		rel_CubicBezierToX: -0.0137 y: 0.0810 control1X: 0.0000 y: 0.0328 control2X: -0.0046 y: 0.0598;
		rel_CubicBezierToX: -0.0571 y: 0.0742 control1X: -0.0088 y: 0.0212 control2X: -0.0278 y: 0.0459;
		rel_LineToX: -0.0440 y: 0.0430;
		rel_CubicBezierToX: -0.0410 y: 0.0488 control1X: -0.0188 y: 0.0172 control2X: -0.0325 y: 0.0335;
		rel_CubicBezierToX: -0.0122 y: 0.0469 control1X: -0.0081 y: 0.0153 control2X: -0.0122 y: 0.0309;
		rel_CubicBezierToX: 0.0318 y: 0.0703 control1X: 0.0000 y: 0.0290 control2X: 0.0106 y: 0.0524;
		rel_CubicBezierToX: 0.0854 y: 0.0269 control1X: 0.0214 y: 0.0179 control2X: 0.0499 y: 0.0269;
		rel_CubicBezierToX: 0.0820 y: -0.0171 control1X: 0.0254 y: 0.0000 control2X: 0.0527 y: -0.0057;
		rel_CubicBezierToX: 0.0909 y: -0.0498 control1X: 0.0293 y: -0.0114 control2X: 0.0596 y: -0.0280;
		rel_LineToX: 0.0000 y: 0.0918;
		rel_CubicBezierToX: -0.0928 y: 0.0415 control1X: -0.0306 y: 0.0185 control2X: -0.0616 y: 0.0324;
		rel_CubicBezierToX: -0.0972 y: 0.0136 control1X: -0.0313 y: 0.0091 control2X: -0.0637 y: 0.0136;
		rel_CubicBezierToX: -0.1445 y: -0.0473 control1X: -0.0599 y: 0.0000 control2X: -0.1081 y: -0.0157;
		rel_CubicBezierToX: -0.0547 y: -0.1250 control1X: -0.0365 y: -0.0316 control2X: -0.0547 y: -0.0733;
		rel_CubicBezierToX: 0.0176 y: -0.0703 control1X: 0.0000 y: -0.0248 control2X: 0.0059 y: -0.0482;
		rel_CubicBezierToX: 0.0615 y: -0.0757 control1X: 0.0120 y: -0.0225 control2X: 0.0325 y: -0.0477;
		rel_LineToX: 0.0430 y: -0.0420;
		rel_CubicBezierToX: 0.0327 y: -0.0357 control1X: 0.0156 y: -0.0153 control2X: 0.0265 y: -0.0272;
		rel_CubicBezierToX: 0.0132 y: -0.0253 control1X: 0.0062 y: -0.0087 control2X: 0.0106 y: -0.0172;
		rel_CubicBezierToX: 0.0048 y: -0.0249 control1X: 0.0022 y: -0.0069 control2X: 0.0039 y: -0.0152;
		rel_CubicBezierToX: 0.0015 y: -0.0411 control1X: 0.0010 y: -0.0098 control2X: 0.0015 y: -0.0235;
		rel_LineToX: 0.0000 y: -0.0600;
		rel_MoveToX: 0.0957 y: -0.0718;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans192
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290;
		rel_MoveToX: 0.0278 y: -0.1983;
		rel_LineToX: 0.0957 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1123 y: -0.1289;
		rel_LineToX: 0.0913 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans193
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290;
		rel_MoveToX: 0.0825 y: -0.1983;
		rel_LineToX: 0.0904 y: 0.0000;
		rel_LineToX: -0.1114 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans194
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290;
		rel_MoveToX: 0.0098 y: -0.1992;
		rel_LineToX: 0.0918 y: 0.0000;
		rel_LineToX: 0.1030 y: 0.1298;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0810 y: -0.0869;
		rel_LineToX: -0.0811 y: 0.0869;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: 0.1031 y: -0.1298! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans195
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290;
		rel_MoveToX: 0.0537 y: -0.1060;
		rel_LineToX: -0.0278 y: -0.0161;
		rel_CubicBezierToX: -0.0200 y: -0.0093 control1X: -0.0082 y: -0.0045 control2X: -0.0148 y: -0.0076;
		rel_CubicBezierToX: -0.0132 y: -0.0029 control1X: -0.0049 y: -0.0019 control2X: -0.0093 y: -0.0029;
		rel_CubicBezierToX: -0.0274 y: 0.0122 control1X: -0.0117 y: 0.0000 control2X: -0.0209 y: 0.0041;
		rel_CubicBezierToX: -0.0097 y: 0.0342 control1X: -0.0065 y: 0.0081 control2X: -0.0097 y: 0.0195;
		rel_LineToX: 0.0000 y: 0.0029;
		rel_LineToX: -0.0611 y: 0.0000;
		rel_CubicBezierToX: 0.0249 y: -0.0781 control1X: 0.0000 y: -0.0329 control2X: 0.0083 y: -0.0589;
		rel_CubicBezierToX: 0.0674 y: -0.0288 control1X: 0.0170 y: -0.0192 control2X: 0.0394 y: -0.0288;
		rel_CubicBezierToX: 0.0322 y: 0.0039 control1X: 0.0117 y: 0.0000 control2X: 0.0225 y: 0.0013;
		rel_CubicBezierToX: 0.0386 y: 0.0171 control1X: 0.0101 y: 0.0026 control2X: 0.0230 y: 0.0083;
		rel_LineToX: 0.0278 y: 0.0146;
		rel_CubicBezierToX: 0.0196 y: 0.0093 control1X: 0.0075 y: 0.0043 control2X: 0.0140 y: 0.0074;
		rel_CubicBezierToX: 0.0156 y: 0.0029 control1X: 0.0055 y: 0.0020 control2X: 0.0107 y: 0.0029;
		rel_CubicBezierToX: 0.0254 y: -0.0122 control1X: 0.0104 y: 0.0000 control2X: 0.0189 y: -0.0040;
		rel_CubicBezierToX: 0.0098 y: -0.0327 control1X: 0.0065 y: -0.0084 control2X: 0.0098 y: -0.0193;
		rel_LineToX: 0.0000 y: -0.0029;
		rel_LineToX: 0.0610 y: 0.0000;
		rel_CubicBezierToX: -0.0264 y: 0.0781 control1X: -0.0007 y: 0.0325 control2X: -0.0094 y: 0.0586;
		rel_CubicBezierToX: -0.0659 y: 0.0288 control1X: -0.0166 y: 0.0192 control2X: -0.0385 y: 0.0288;
		rel_CubicBezierToX: -0.0312 y: -0.0039 control1X: -0.0111 y: 0.0000 control2X: -0.0215 y: -0.0013;
		rel_CubicBezierToX: -0.0396 y: -0.0171 control1X: -0.0095 y: -0.0026 control2X: -0.0227 y: -0.0083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans196
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290;
		rel_MoveToX: 0.1016 y: -0.1841;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1910 y: 0.0000;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans197
	vectorGraphicsEngine
		rel_MoveToX: 0.4538 y: 0.1622;
		rel_CubicBezierToX: -0.0220 y: -0.0522 control1X: 0.0000 y: -0.0205 control2X: -0.0073 y: -0.0379;
		rel_CubicBezierToX: -0.0522 y: -0.0220 control1X: -0.0143 y: -0.0147 control2X: -0.0317 y: -0.0220;
		rel_CubicBezierToX: -0.0528 y: 0.0215 control1X: -0.0209 y: 0.0000 control2X: -0.0384 y: 0.0072;
		rel_CubicBezierToX: -0.0210 y: 0.0527 control1X: -0.0140 y: 0.0140 control2X: -0.0210 y: 0.0316;
		rel_CubicBezierToX: 0.0215 y: 0.0523 control1X: 0.0000 y: 0.0205 control2X: 0.0072 y: 0.0380;
		rel_CubicBezierToX: 0.0523 y: 0.0215 control1X: 0.0143 y: 0.0143 control2X: 0.0318 y: 0.0215;
		rel_CubicBezierToX: 0.0522 y: -0.0215 control1X: 0.0205 y: 0.0000 control2X: 0.0379 y: -0.0072;
		rel_CubicBezierToX: 0.0220 y: -0.0523 control1X: 0.0147 y: -0.0143 control2X: 0.0220 y: -0.0318;
		rel_MoveToX: -0.0742 y: 0.1621;
		rel_LineToX: -0.1328 y: 0.3628;
		rel_LineToX: 0.2661 y: 0.0000;
		rel_LineToX: -0.1333 y: -0.3628;
		rel_MoveToX: -0.0738 y: -0.0502;
		rel_CubicBezierToX: -0.0449 y: -0.0479 control1X: -0.0198 y: -0.0134 control2X: -0.0348 y: -0.0293;
		rel_CubicBezierToX: -0.0151 y: -0.0640 control1X: -0.0101 y: -0.0189 control2X: -0.0151 y: -0.0402;
		rel_CubicBezierToX: 0.0385 y: -0.0952 control1X: 0.0000 y: -0.0374 control2X: 0.0128 y: -0.0691;
		rel_CubicBezierToX: 0.0953 y: -0.0390 control1X: 0.0261 y: -0.0260 control2X: 0.0578 y: -0.0390;
		rel_CubicBezierToX: 0.0947 y: 0.0395 control1X: 0.0371 y: 0.0000 control2X: 0.0686 y: 0.0131;
		rel_CubicBezierToX: 0.0395 y: 0.0947 control1X: 0.0264 y: 0.0261 control2X: 0.0395 y: 0.0576;
		rel_CubicBezierToX: -0.0156 y: 0.0630 control1X: 0.0000 y: 0.0228 control2X: -0.0052 y: 0.0438;
		rel_CubicBezierToX: -0.0444 y: 0.0489 control1X: -0.0101 y: 0.0192 control2X: -0.0249 y: 0.0355;
		rel_LineToX: 0.2597 y: 0.6821;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2602 y: -0.6821! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans198
	vectorGraphicsEngine
		rel_MoveToX: 0.9386 y: 0.2272;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3618 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4702 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1870;
		rel_LineToX: -0.2578 y: 0.0000;
		rel_LineToX: -0.0781 y: 0.1870;
		rel_LineToX: -0.1001 y: 0.0000;
		rel_LineToX: 0.3051 y: -0.7290;
		rel_LineToX: 0.5918 y: 0.0000;
		rel_MoveToX: -0.5239 y: 0.0771;
		rel_LineToX: -0.1611 y: 0.3828;
		rel_LineToX: 0.2241 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3828;
		rel_LineToX: -0.0630 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans199
	vectorGraphicsEngine
		rel_MoveToX: 0.6818 y: 0.2833;
		rel_LineToX: 0.0000 y: 0.1040;
		rel_CubicBezierToX: -0.1064 y: -0.0693 control1X: -0.0332 y: -0.0309 control2X: -0.0687 y: -0.0540;
		rel_CubicBezierToX: -0.1197 y: -0.0230 control1X: -0.0375 y: -0.0153 control2X: -0.0773 y: -0.0230;
		rel_CubicBezierToX: -0.1914 y: 0.0767 control1X: -0.0833 y: 0.0000 control2X: -0.1471 y: 0.0256;
		rel_CubicBezierToX: -0.0664 y: 0.2207 control1X: -0.0442 y: 0.0508 control2X: -0.0664 y: 0.1244;
		rel_CubicBezierToX: 0.0664 y: 0.2207 control1X: 0.0000 y: 0.0960 control2X: 0.0222 y: 0.1696;
		rel_CubicBezierToX: 0.1914 y: 0.0762 control1X: 0.0443 y: 0.0508 control2X: 0.1081 y: 0.0762;
		rel_CubicBezierToX: 0.1197 y: -0.0230 control1X: 0.0424 y: 0.0000 control2X: 0.0822 y: -0.0077;
		rel_CubicBezierToX: 0.1064 y: -0.0693 control1X: 0.0377 y: -0.0153 control2X: 0.0732 y: -0.0384;
		rel_LineToX: 0.0000 y: 0.1030;
		rel_CubicBezierToX: -0.1099 y: 0.0528 control1X: -0.0345 y: 0.0235 control2X: -0.0711 y: 0.0410;
		rel_CubicBezierToX: -0.1220 y: 0.0175 control1X: -0.0384 y: 0.0117 control2X: -0.0791 y: 0.0175;
		rel_CubicBezierToX: -0.2608 y: -0.1010 control1X: -0.1104 y: 0.0000 control2X: -0.1973 y: -0.0336;
		rel_CubicBezierToX: -0.0952 y: -0.2769 control1X: -0.0634 y: -0.0677 control2X: -0.0952 y: -0.1600;
		rel_CubicBezierToX: 0.0952 y: -0.2768 control1X: 0.0000 y: -0.1172 control2X: 0.0318 y: -0.2095;
		rel_CubicBezierToX: 0.2608 y: -0.1016 control1X: 0.0635 y: -0.0678 control2X: 0.1504 y: -0.1016;
		rel_CubicBezierToX: 0.1230 y: 0.0176 control1X: 0.0436 y: 0.0000 control2X: 0.0846 y: 0.0059;
		rel_CubicBezierToX: 0.1089 y: 0.0517 control1X: 0.0388 y: 0.0114 control2X: 0.0751 y: 0.0286;
		rel_MoveToX: -0.2060 y: 0.6729;
		rel_CubicBezierToX: 0.0400 y: 0.0556 control1X: 0.0179 y: 0.0202 control2X: 0.0312 y: 0.0387;
		rel_CubicBezierToX: 0.0132 y: 0.0494 control1X: 0.0088 y: 0.0173 control2X: 0.0132 y: 0.0337;
		rel_CubicBezierToX: -0.0293 y: 0.0654 control1X: 0.0000 y: 0.0289 control2X: -0.0098 y: 0.0507;
		rel_CubicBezierToX: -0.0869 y: 0.0225 control1X: -0.0196 y: 0.0150 control2X: -0.0485 y: 0.0225;
		rel_CubicBezierToX: -0.0440 y: -0.0030 control1X: -0.0150 y: 0.0000 control2X: -0.0297 y: -0.0010;
		rel_CubicBezierToX: -0.0420 y: -0.0088 control1X: -0.0140 y: -0.0019 control2X: -0.0280 y: -0.0049;
		rel_LineToX: 0.0000 y: -0.0639;
		rel_CubicBezierToX: 0.0347 y: 0.0117 control1X: 0.0111 y: 0.0055 control2X: 0.0227 y: 0.0094;
		rel_CubicBezierToX: 0.0410 y: 0.0039 control1X: 0.0120 y: 0.0026 control2X: 0.0257 y: 0.0039;
		rel_CubicBezierToX: 0.0435 y: -0.0117 control1X: 0.0192 y: 0.0000 control2X: 0.0337 y: -0.0039;
		rel_CubicBezierToX: 0.0146 y: -0.0342 control1X: 0.0097 y: -0.0078 control2X: 0.0146 y: -0.0192;
		rel_CubicBezierToX: -0.0107 y: -0.0361 control1X: 0.0000 y: -0.0098 control2X: -0.0036 y: -0.0218;
		rel_CubicBezierToX: -0.0323 y: -0.0508 control1X: -0.0069 y: -0.0140 control2X: -0.0176 y: -0.0310;
		rel_LineToX: 0.0582 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans200
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4609 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4697 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.2012 y: -0.1983;
		rel_LineToX: 0.0957 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1123 y: -0.1289;
		rel_LineToX: 0.0913 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans201
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4609 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4697 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.2559 y: -0.1983;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: -0.1113 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans202
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4609 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4697 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.1831 y: -0.1992;
		rel_LineToX: 0.0918 y: 0.0000;
		rel_LineToX: 0.1030 y: 0.1298;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: -0.0811 y: -0.0869;
		rel_LineToX: -0.0810 y: 0.0869;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: 0.1030 y: -0.1298! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans203
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4609 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4697 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.2749 y: -0.1841;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans204
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.0220 y: -0.1983;
		rel_LineToX: 0.0957 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1123 y: -0.1289;
		rel_LineToX: 0.0913 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans205
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.0767 y: -0.1983;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: -0.1113 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans206
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.0039 y: -0.1992;
		rel_LineToX: 0.0918 y: 0.0000;
		rel_LineToX: 0.1030 y: 0.1298;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: -0.0811 y: -0.0869;
		rel_LineToX: -0.0810 y: 0.0869;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: 0.1030 y: -0.1298! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans207
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.0957 y: -0.1841;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans208
	vectorGraphicsEngine
		rel_MoveToX: 0.1408 y: 0.2272;
		rel_LineToX: 0.2031 y: 0.0000;
		rel_CubicBezierToX: 0.3106 y: 0.0884 control1X: 0.1410 y: 0.0000 control2X: 0.2445 y: 0.0294;
		rel_CubicBezierToX: 0.0991 y: 0.2753 control1X: 0.0660 y: 0.0586 control2X: 0.0991 y: 0.1503;
		rel_CubicBezierToX: -0.0996 y: 0.2769 control1X: 0.0000 y: 0.1257 control2X: -0.0332 y: 0.2180;
		rel_CubicBezierToX: -0.3101 y: 0.0884 control1X: -0.0661 y: 0.0589 control2X: -0.1695 y: 0.0884;
		rel_LineToX: -0.2031 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3418;
		rel_LineToX: -0.0982 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0703;
		rel_LineToX: 0.0982 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3169;
		rel_MoveToX: 0.0991 y: 0.0810;
		rel_LineToX: 0.0000 y: 0.2359;
		rel_LineToX: 0.1641 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0703;
		rel_LineToX: -0.1641 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2607;
		rel_LineToX: 0.1187 y: 0.0000;
		rel_CubicBezierToX: 0.2207 y: -0.0683 control1X: 0.1006 y: 0.0000 control2X: 0.1741 y: -0.0228;
		rel_CubicBezierToX: 0.0703 y: -0.2159 control1X: 0.0468 y: -0.0456 control2X: 0.0703 y: -0.1175;
		rel_CubicBezierToX: -0.0703 y: -0.2143 control1X: 0.0000 y: -0.0976 control2X: -0.0235 y: -0.1691;
		rel_CubicBezierToX: -0.2207 y: -0.0684 control1X: -0.0466 y: -0.0456 control2X: -0.1201 y: -0.0684;
		rel_LineToX: -0.1187 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans209
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.1328 y: 0.0000;
		rel_LineToX: 0.3233 y: 0.6098;
		rel_LineToX: 0.0000 y: -0.6098;
		rel_LineToX: 0.0957 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.1328 y: 0.0000;
		rel_LineToX: -0.3233 y: -0.6099;
		rel_LineToX: 0.0000 y: 0.6099;
		rel_LineToX: -0.0957 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.2739 y: -0.1060;
		rel_LineToX: -0.0278 y: -0.0161;
		rel_CubicBezierToX: -0.0200 y: -0.0093 control1X: -0.0081 y: -0.0045 control2X: -0.0148 y: -0.0076;
		rel_CubicBezierToX: -0.0132 y: -0.0029 control1X: -0.0049 y: -0.0019 control2X: -0.0093 y: -0.0029;
		rel_CubicBezierToX: -0.0273 y: 0.0122 control1X: -0.0117 y: 0.0000 control2X: -0.0208 y: 0.0041;
		rel_CubicBezierToX: -0.0098 y: 0.0342 control1X: -0.0066 y: 0.0081 control2X: -0.0098 y: 0.0195;
		rel_LineToX: 0.0000 y: 0.0029;
		rel_LineToX: -0.0610 y: 0.0000;
		rel_CubicBezierToX: 0.0249 y: -0.0781 control1X: 0.0000 y: -0.0329 control2X: 0.0083 y: -0.0589;
		rel_CubicBezierToX: 0.0673 y: -0.0288 control1X: 0.0169 y: -0.0192 control2X: 0.0393 y: -0.0288;
		rel_CubicBezierToX: 0.0323 y: 0.0039 control1X: 0.0118 y: 0.0000 control2X: 0.0225 y: 0.0013;
		rel_CubicBezierToX: 0.0385 y: 0.0171 control1X: 0.0101 y: 0.0026 control2X: 0.0229 y: 0.0083;
		rel_LineToX: 0.0279 y: 0.0146;
		rel_CubicBezierToX: 0.0195 y: 0.0093 control1X: 0.0075 y: 0.0043 control2X: 0.0140 y: 0.0074;
		rel_CubicBezierToX: 0.0156 y: 0.0029 control1X: 0.0055 y: 0.0020 control2X: 0.0107 y: 0.0029;
		rel_CubicBezierToX: 0.0254 y: -0.0122 control1X: 0.0104 y: 0.0000 control2X: 0.0189 y: -0.0040;
		rel_CubicBezierToX: 0.0098 y: -0.0327 control1X: 0.0065 y: -0.0084 control2X: 0.0098 y: -0.0193;
		rel_LineToX: 0.0000 y: -0.0029;
		rel_LineToX: 0.0610 y: 0.0000;
		rel_CubicBezierToX: -0.0263 y: 0.0781 control1X: -0.0006 y: 0.0325 control2X: -0.0094 y: 0.0586;
		rel_CubicBezierToX: -0.0660 y: 0.0288 control1X: -0.0166 y: 0.0192 control2X: -0.0386 y: 0.0288;
		rel_CubicBezierToX: -0.0312 y: -0.0039 control1X: -0.0110 y: 0.0000 control2X: -0.0214 y: -0.0013;
		rel_CubicBezierToX: -0.0396 y: -0.0171 control1X: -0.0095 y: -0.0026 control2X: -0.0226 y: -0.0083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans210
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_MoveToX: -0.0278 y: -0.1851;
		rel_LineToX: 0.0957 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1123 y: -0.1289;
		rel_LineToX: 0.0913 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans211
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_MoveToX: 0.0269 y: -0.1851;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: -0.1113 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans212
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_MoveToX: -0.0459 y: -0.1860;
		rel_LineToX: 0.0918 y: 0.0000;
		rel_LineToX: 0.1030 y: 0.1298;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: -0.0811 y: -0.0869;
		rel_LineToX: -0.0810 y: 0.0869;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: 0.1030 y: -0.1298! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans213
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_MoveToX: -0.0019 y: -0.0928;
		rel_LineToX: -0.0279 y: -0.0161;
		rel_CubicBezierToX: -0.0200 y: -0.0093 control1X: -0.0081 y: -0.0045 control2X: -0.0148 y: -0.0076;
		rel_CubicBezierToX: -0.0132 y: -0.0029 control1X: -0.0049 y: -0.0019 control2X: -0.0093 y: -0.0029;
		rel_CubicBezierToX: -0.0273 y: 0.0122 control1X: -0.0117 y: 0.0000 control2X: -0.0208 y: 0.0041;
		rel_CubicBezierToX: -0.0098 y: 0.0342 control1X: -0.0065 y: 0.0081 control2X: -0.0098 y: 0.0195;
		rel_LineToX: 0.0000 y: 0.0029;
		rel_LineToX: -0.0610 y: 0.0000;
		rel_CubicBezierToX: 0.0249 y: -0.0781 control1X: 0.0000 y: -0.0329 control2X: 0.0083 y: -0.0589;
		rel_CubicBezierToX: 0.0674 y: -0.0288 control1X: 0.0169 y: -0.0192 control2X: 0.0394 y: -0.0288;
		rel_CubicBezierToX: 0.0322 y: 0.0039 control1X: 0.0117 y: 0.0000 control2X: 0.0224 y: 0.0013;
		rel_CubicBezierToX: 0.0386 y: 0.0171 control1X: 0.0101 y: 0.0026 control2X: 0.0229 y: 0.0083;
		rel_LineToX: 0.0278 y: 0.0146;
		rel_CubicBezierToX: 0.0195 y: 0.0093 control1X: 0.0075 y: 0.0043 control2X: 0.0140 y: 0.0074;
		rel_CubicBezierToX: 0.0157 y: 0.0029 control1X: 0.0056 y: 0.0020 control2X: 0.0108 y: 0.0029;
		rel_CubicBezierToX: 0.0253 y: -0.0122 control1X: 0.0104 y: 0.0000 control2X: 0.0188 y: -0.0040;
		rel_CubicBezierToX: 0.0098 y: -0.0327 control1X: 0.0066 y: -0.0084 control2X: 0.0098 y: -0.0193;
		rel_LineToX: 0.0000 y: -0.0029;
		rel_LineToX: 0.0610 y: 0.0000;
		rel_CubicBezierToX: -0.0263 y: 0.0781 control1X: -0.0006 y: 0.0325 control2X: -0.0094 y: 0.0586;
		rel_CubicBezierToX: -0.0659 y: 0.0288 control1X: -0.0166 y: 0.0192 control2X: -0.0386 y: 0.0288;
		rel_CubicBezierToX: -0.0313 y: -0.0039 control1X: -0.0111 y: 0.0000 control2X: -0.0215 y: -0.0013;
		rel_CubicBezierToX: -0.0395 y: -0.0171 control1X: -0.0094 y: -0.0026 control2X: -0.0226 y: -0.0083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans214
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_MoveToX: 0.0459 y: -0.1709;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans215
	vectorGraphicsEngine
		rel_MoveToX: 0.7389 y: 0.4191;
		rel_LineToX: -0.2231 y: 0.2241;
		rel_LineToX: 0.2231 y: 0.2231;
		rel_LineToX: -0.0581 y: 0.0591;
		rel_LineToX: -0.2241 y: -0.2241;
		rel_LineToX: -0.2241 y: 0.2241;
		rel_LineToX: -0.0576 y: -0.0591;
		rel_LineToX: 0.2226 y: -0.2231;
		rel_LineToX: -0.2226 y: -0.2241;
		rel_LineToX: 0.0576 y: -0.0591;
		rel_LineToX: 0.2241 y: 0.2241;
		rel_LineToX: 0.2241 y: -0.2241;
		rel_LineToX: 0.0581 y: 0.0591! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans216
	vectorGraphicsEngine
		rel_MoveToX: 0.6266 y: 0.4132;
		rel_LineToX: -0.3501 y: 0.4131;
		rel_CubicBezierToX: 0.0694 y: 0.0479 control1X: 0.0202 y: 0.0208 control2X: 0.0433 y: 0.0368;
		rel_CubicBezierToX: 0.0859 y: 0.0161 control1X: 0.0263 y: 0.0107 control2X: 0.0550 y: 0.0161;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0097 y: -0.0996 control1X: 0.0000 y: -0.0364 control2X: -0.0032 y: -0.0696;
		rel_CubicBezierToX: -0.0284 y: -0.0796 control1X: -0.0062 y: -0.0299 control2X: -0.0157 y: -0.0565;
		rel_MoveToX: -0.0400 y: -0.0552;
		rel_CubicBezierToX: -0.0693 y: -0.0473 control1X: -0.0199 y: -0.0208 control2X: -0.0430 y: -0.0366;
		rel_CubicBezierToX: -0.0855 y: -0.0166 control1X: -0.0261 y: -0.0111 control2X: -0.0546 y: -0.0166;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0093 y: 0.1006 control1X: 0.0000 y: 0.0365 control2X: 0.0031 y: 0.0700;
		rel_CubicBezierToX: 0.0288 y: 0.0791 control1X: 0.0065 y: 0.0306 control2X: 0.0161 y: 0.0570;
		rel_LineToX: 0.3506 y: -0.4141;
		rel_MoveToX: -0.4160 y: 0.4912;
		rel_CubicBezierToX: -0.0576 y: -0.1142 control1X: -0.0254 y: -0.0332 control2X: -0.0446 y: -0.0712;
		rel_CubicBezierToX: -0.0191 y: -0.1426 control1X: -0.0127 y: -0.0430 control2X: -0.0191 y: -0.0905;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_CubicBezierToX: 0.1172 y: 0.0190 control1X: 0.0423 y: 0.0000 control2X: 0.0814 y: 0.0063;
		rel_CubicBezierToX: 0.0967 y: 0.0552 control1X: 0.0361 y: 0.0124 control2X: 0.0684 y: 0.0308;
		rel_LineToX: 0.0791 y: -0.0932;
		rel_LineToX: 0.0498 y: 0.0434;
		rel_LineToX: -0.0830 y: 0.0967;
		rel_CubicBezierToX: 0.0576 y: 0.1152 control1X: 0.0254 y: 0.0335 control2X: 0.0446 y: 0.0719;
		rel_CubicBezierToX: 0.0195 y: 0.1421 control1X: 0.0130 y: 0.0433 control2X: 0.0195 y: 0.0907;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.1167 y: -0.0180 control1X: -0.0417 y: 0.0000 control2X: -0.0806 y: -0.0060;
		rel_CubicBezierToX: -0.0981 y: -0.0552 control1X: -0.0358 y: -0.0124 control2X: -0.0685 y: -0.0308;
		rel_LineToX: -0.0791 y: 0.0933;
		rel_LineToX: -0.0503 y: -0.0430;
		rel_LineToX: 0.0830 y: -0.0982! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans217
	vectorGraphicsEngine
		rel_MoveToX: 0.1247 y: 0.2272;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4428;
		rel_CubicBezierToX: 0.0425 y: 0.1690 control1X: 0.0000 y: 0.0782 control2X: 0.0142 y: 0.1345;
		rel_CubicBezierToX: 0.1377 y: 0.0513 control1X: 0.0283 y: 0.0342 control2X: 0.0742 y: 0.0513;
		rel_CubicBezierToX: 0.1372 y: -0.0513 control1X: 0.0631 y: 0.0000 control2X: 0.1089 y: -0.0171;
		rel_CubicBezierToX: 0.0425 y: -0.1690 control1X: 0.0283 y: -0.0345 control2X: 0.0425 y: -0.0908;
		rel_LineToX: 0.0000 y: -0.4428;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4551;
		rel_CubicBezierToX: -0.0708 y: 0.2153 control1X: 0.0000 y: 0.0950 control2X: -0.0236 y: 0.1668;
		rel_CubicBezierToX: -0.2080 y: 0.0727 control1X: -0.0469 y: 0.0485 control2X: -0.1162 y: 0.0727;
		rel_CubicBezierToX: -0.2090 y: -0.0727 control1X: -0.0921 y: 0.0000 control2X: -0.1618 y: -0.0242;
		rel_CubicBezierToX: -0.0703 y: -0.2153 control1X: -0.0469 y: -0.0485 control2X: -0.0703 y: -0.1203;
		rel_LineToX: 0.0000 y: -0.4551;
		rel_MoveToX: 0.2514 y: -0.1983;
		rel_LineToX: 0.0957 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1123 y: -0.1289;
		rel_LineToX: 0.0913 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans218
	vectorGraphicsEngine
		rel_MoveToX: 0.1247 y: 0.2272;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4428;
		rel_CubicBezierToX: 0.0425 y: 0.1690 control1X: 0.0000 y: 0.0782 control2X: 0.0142 y: 0.1345;
		rel_CubicBezierToX: 0.1377 y: 0.0513 control1X: 0.0283 y: 0.0342 control2X: 0.0742 y: 0.0513;
		rel_CubicBezierToX: 0.1372 y: -0.0513 control1X: 0.0631 y: 0.0000 control2X: 0.1089 y: -0.0171;
		rel_CubicBezierToX: 0.0425 y: -0.1690 control1X: 0.0283 y: -0.0345 control2X: 0.0425 y: -0.0908;
		rel_LineToX: 0.0000 y: -0.4428;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4551;
		rel_CubicBezierToX: -0.0708 y: 0.2153 control1X: 0.0000 y: 0.0950 control2X: -0.0236 y: 0.1668;
		rel_CubicBezierToX: -0.2080 y: 0.0727 control1X: -0.0469 y: 0.0485 control2X: -0.1162 y: 0.0727;
		rel_CubicBezierToX: -0.2090 y: -0.0727 control1X: -0.0921 y: 0.0000 control2X: -0.1618 y: -0.0242;
		rel_CubicBezierToX: -0.0703 y: -0.2153 control1X: -0.0469 y: -0.0485 control2X: -0.0703 y: -0.1203;
		rel_LineToX: 0.0000 y: -0.4551;
		rel_MoveToX: 0.3061 y: -0.1983;
		rel_LineToX: 0.0904 y: 0.0000;
		rel_LineToX: -0.1114 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans219
	vectorGraphicsEngine
		rel_MoveToX: 0.1247 y: 0.2272;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4428;
		rel_CubicBezierToX: 0.0425 y: 0.1690 control1X: 0.0000 y: 0.0782 control2X: 0.0142 y: 0.1345;
		rel_CubicBezierToX: 0.1377 y: 0.0513 control1X: 0.0283 y: 0.0342 control2X: 0.0742 y: 0.0513;
		rel_CubicBezierToX: 0.1372 y: -0.0513 control1X: 0.0631 y: 0.0000 control2X: 0.1089 y: -0.0171;
		rel_CubicBezierToX: 0.0425 y: -0.1690 control1X: 0.0283 y: -0.0345 control2X: 0.0425 y: -0.0908;
		rel_LineToX: 0.0000 y: -0.4428;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4551;
		rel_CubicBezierToX: -0.0708 y: 0.2153 control1X: 0.0000 y: 0.0950 control2X: -0.0236 y: 0.1668;
		rel_CubicBezierToX: -0.2080 y: 0.0727 control1X: -0.0469 y: 0.0485 control2X: -0.1162 y: 0.0727;
		rel_CubicBezierToX: -0.2090 y: -0.0727 control1X: -0.0921 y: 0.0000 control2X: -0.1618 y: -0.0242;
		rel_CubicBezierToX: -0.0703 y: -0.2153 control1X: -0.0469 y: -0.0485 control2X: -0.0703 y: -0.1203;
		rel_LineToX: 0.0000 y: -0.4551;
		rel_MoveToX: 0.2334 y: -0.1992;
		rel_LineToX: 0.0918 y: 0.0000;
		rel_LineToX: 0.1030 y: 0.1298;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0810 y: -0.0869;
		rel_LineToX: -0.0811 y: 0.0869;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: 0.1030 y: -0.1298! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans220
	vectorGraphicsEngine
		rel_MoveToX: 0.1247 y: 0.2272;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4428;
		rel_CubicBezierToX: 0.0425 y: 0.1690 control1X: 0.0000 y: 0.0782 control2X: 0.0142 y: 0.1345;
		rel_CubicBezierToX: 0.1377 y: 0.0513 control1X: 0.0283 y: 0.0342 control2X: 0.0742 y: 0.0513;
		rel_CubicBezierToX: 0.1372 y: -0.0513 control1X: 0.0631 y: 0.0000 control2X: 0.1089 y: -0.0171;
		rel_CubicBezierToX: 0.0425 y: -0.1690 control1X: 0.0283 y: -0.0345 control2X: 0.0425 y: -0.0908;
		rel_LineToX: 0.0000 y: -0.4428;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4551;
		rel_CubicBezierToX: -0.0708 y: 0.2153 control1X: 0.0000 y: 0.0950 control2X: -0.0236 y: 0.1668;
		rel_CubicBezierToX: -0.2080 y: 0.0727 control1X: -0.0469 y: 0.0485 control2X: -0.1162 y: 0.0727;
		rel_CubicBezierToX: -0.2090 y: -0.0727 control1X: -0.0921 y: 0.0000 control2X: -0.1618 y: -0.0242;
		rel_CubicBezierToX: -0.0703 y: -0.2153 control1X: -0.0469 y: -0.0485 control2X: -0.0703 y: -0.1203;
		rel_LineToX: 0.0000 y: -0.4551;
		rel_MoveToX: 0.3252 y: -0.1841;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0991;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0991! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans221
	vectorGraphicsEngine
		rel_MoveToX: 0.0358 y: 0.2272;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: 0.2021 y: 0.2998;
		rel_LineToX: 0.2007 y: -0.2998;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: -0.2579 y: 0.3818;
		rel_LineToX: 0.0000 y: 0.3472;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3472;
		rel_LineToX: -0.2578 y: -0.3818;
		rel_MoveToX: 0.3350 y: -0.1983;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: -0.1113 y: 0.1289;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.0957 y: -0.1289! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans222
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1289;
		rel_LineToX: 0.1241 y: 0.0000;
		rel_CubicBezierToX: 0.1850 y: 0.0552 control1X: 0.0817 y: 0.0000 control2X: 0.1434 y: 0.0184;
		rel_CubicBezierToX: 0.0630 y: 0.1621 control1X: 0.0420 y: 0.0364 control2X: 0.0630 y: 0.0905;
		rel_CubicBezierToX: -0.0630 y: 0.1626 control1X: 0.0000 y: 0.0716 control2X: -0.0210 y: 0.1258;
		rel_CubicBezierToX: -0.1850 y: 0.0551 control1X: -0.0416 y: 0.0368 control2X: -0.1033 y: 0.0551;
		rel_LineToX: -0.1241 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1651;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_MoveToX: 0.0986 y: 0.2099;
		rel_LineToX: 0.0000 y: 0.2730;
		rel_LineToX: 0.1241 y: 0.0000;
		rel_CubicBezierToX: 0.1064 y: -0.0357 control1X: 0.0459 y: 0.0000 control2X: 0.0813 y: -0.0119;
		rel_CubicBezierToX: 0.0376 y: -0.1010 control1X: 0.0251 y: -0.0237 control2X: 0.0376 y: -0.0574;
		rel_CubicBezierToX: -0.0376 y: -0.1006 control1X: 0.0000 y: -0.0437 control2X: -0.0125 y: -0.0772;
		rel_CubicBezierToX: -0.1064 y: -0.0357 control1X: -0.0247 y: -0.0238 control2X: -0.0602 y: -0.0357;
		rel_LineToX: -0.1241 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans223
	vectorGraphicsEngine
		rel_MoveToX: 0.1286 y: 0.4010;
		rel_CubicBezierToX: 0.0581 y: -0.1509 control1X: 0.0000 y: -0.0651 control2X: 0.0194 y: -0.1154;
		rel_CubicBezierToX: 0.1650 y: -0.0537 control1X: 0.0391 y: -0.0358 control2X: 0.0941 y: -0.0537;
		rel_CubicBezierToX: 0.1548 y: 0.0567 control1X: 0.0677 y: 0.0000 control2X: 0.1193 y: 0.0189;
		rel_CubicBezierToX: 0.0552 y: 0.1660 control1X: 0.0358 y: 0.0377 control2X: 0.0542 y: 0.0931;
		rel_CubicBezierToX: -0.1148 y: 0.0322 control1X: -0.0492 y: 0.0026 control2X: -0.0874 y: 0.0133;
		rel_CubicBezierToX: -0.0410 y: 0.0747 control1X: -0.0273 y: 0.0186 control2X: -0.0410 y: 0.0435;
		rel_CubicBezierToX: 0.0142 y: 0.0430 control1X: 0.0000 y: 0.0153 control2X: 0.0048 y: 0.0296;
		rel_CubicBezierToX: 0.0459 y: 0.0400 control1X: 0.0094 y: 0.0130 control2X: 0.0247 y: 0.0263;
		rel_LineToX: 0.0278 y: 0.0181;
		rel_CubicBezierToX: 0.1045 y: 0.0864 control1X: 0.0541 y: 0.0345 control2X: 0.0889 y: 0.0633;
		rel_CubicBezierToX: 0.0235 y: 0.0835 control1X: 0.0156 y: 0.0231 control2X: 0.0235 y: 0.0509;
		rel_CubicBezierToX: -0.0552 y: 0.1289 control1X: 0.0000 y: 0.0560 control2X: -0.0184 y: 0.0990;
		rel_CubicBezierToX: -0.1577 y: 0.0444 control1X: -0.0365 y: 0.0296 control2X: -0.0891 y: 0.0444;
		rel_CubicBezierToX: -0.0645 y: -0.0063 control1X: -0.0209 y: 0.0000 control2X: -0.0424 y: -0.0021;
		rel_CubicBezierToX: -0.0688 y: -0.0176 control1X: -0.0221 y: -0.0039 control2X: -0.0451 y: -0.0098;
		rel_LineToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.0732 y: 0.0220 control1X: 0.0260 y: 0.0098 control2X: 0.0504 y: 0.0171;
		rel_CubicBezierToX: 0.0659 y: 0.0068 control1X: 0.0228 y: 0.0046 control2X: 0.0448 y: 0.0068;
		rel_CubicBezierToX: 0.0860 y: -0.0229 control1X: 0.0378 y: 0.0000 control2X: 0.0664 y: -0.0076;
		rel_CubicBezierToX: 0.0293 y: -0.0679 control1X: 0.0195 y: -0.0156 control2X: 0.0293 y: -0.0383;
		rel_CubicBezierToX: -0.0147 y: -0.0512 control1X: 0.0000 y: -0.0205 control2X: -0.0049 y: -0.0376;
		rel_CubicBezierToX: -0.0635 y: -0.0508 control1X: -0.0094 y: -0.0137 control2X: -0.0306 y: -0.0306;
		rel_LineToX: -0.0449 y: -0.0269;
		rel_CubicBezierToX: -0.0684 y: -0.0615 control1X: -0.0312 y: -0.0192 control2X: -0.0540 y: -0.0397;
		rel_CubicBezierToX: -0.0209 y: -0.0776 control1X: -0.0139 y: -0.0221 control2X: -0.0209 y: -0.0480;
		rel_CubicBezierToX: 0.0405 y: -0.1040 control1X: 0.0000 y: -0.0414 control2X: 0.0135 y: -0.0760;
		rel_CubicBezierToX: 0.1147 y: -0.0562 control1X: 0.0273 y: -0.0280 control2X: 0.0656 y: -0.0468;
		rel_CubicBezierToX: -0.0361 y: -0.0776 control1X: -0.0026 y: -0.0335 control2X: -0.0146 y: -0.0594;
		rel_CubicBezierToX: -0.0869 y: -0.0274 control1X: -0.0212 y: -0.0183 control2X: -0.0501 y: -0.0274;
		rel_CubicBezierToX: -0.0967 y: 0.0342 control1X: -0.0423 y: 0.0000 control2X: -0.0746 y: 0.0114;
		rel_CubicBezierToX: -0.0332 y: 0.0977 control1X: -0.0221 y: 0.0225 control2X: -0.0332 y: 0.0550;
		rel_LineToX: 0.0000 y: 0.5532;
		rel_LineToX: -0.0913 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5552! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans224
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.3027 y: -0.4878;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans225
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.1640 y: -0.4878;
		rel_LineToX: 0.0971 y: 0.0000;
		rel_LineToX: -0.1591 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans226
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.2680 y: -0.4878;
		rel_LineToX: 0.0722 y: 0.0000;
		rel_LineToX: 0.1197 y: 0.1836;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0879 y: -0.1197;
		rel_LineToX: -0.0879 y: 0.1197;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: 0.1197 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans227
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.2338 y: -0.3570;
		rel_LineToX: -0.0279 y: -0.0268;
		rel_CubicBezierToX: -0.0190 y: -0.0142 control1X: -0.0071 y: -0.0065 control2X: -0.0135 y: -0.0112;
		rel_CubicBezierToX: -0.0142 y: -0.0049 control1X: -0.0052 y: -0.0032 control2X: -0.0100 y: -0.0049;
		rel_CubicBezierToX: -0.0273 y: 0.0181 control1X: -0.0124 y: 0.0000 control2X: -0.0215 y: 0.0061;
		rel_CubicBezierToX: -0.0098 y: 0.0576 control1X: -0.0059 y: 0.0117 control2X: -0.0091 y: 0.0309;
		rel_LineToX: -0.0610 y: 0.0000;
		rel_CubicBezierToX: 0.0259 y: -0.1015 control1X: 0.0006 y: -0.0439 control2X: 0.0093 y: -0.0778;
		rel_CubicBezierToX: 0.0693 y: -0.0362 control1X: 0.0166 y: -0.0241 control2X: 0.0397 y: -0.0362;
		rel_CubicBezierToX: 0.0342 y: 0.0069 control1X: 0.0124 y: 0.0000 control2X: 0.0238 y: 0.0023;
		rel_CubicBezierToX: 0.0337 y: 0.0229 control1X: 0.0104 y: 0.0045 control2X: 0.0216 y: 0.0122;
		rel_LineToX: 0.0278 y: 0.0269;
		rel_CubicBezierToX: 0.0185 y: 0.0146 control1X: 0.0072 y: 0.0065 control2X: 0.0133 y: 0.0114;
		rel_CubicBezierToX: 0.0147 y: 0.0044 control1X: 0.0056 y: 0.0029 control2X: 0.0105 y: 0.0044;
		rel_CubicBezierToX: 0.0273 y: -0.0176 control1X: 0.0124 y: 0.0000 control2X: 0.0215 y: -0.0058;
		rel_CubicBezierToX: 0.0098 y: -0.0581 control1X: 0.0059 y: -0.0120 control2X: 0.0092 y: -0.0314;
		rel_LineToX: 0.0610 y: 0.0000;
		rel_CubicBezierToX: -0.0258 y: 0.1021 control1X: -0.0006 y: 0.0440 control2X: -0.0092 y: 0.0780;
		rel_CubicBezierToX: -0.0694 y: 0.0356 control1X: -0.0166 y: 0.0237 control2X: -0.0398 y: 0.0356;
		rel_CubicBezierToX: -0.0342 y: -0.0068 control1X: -0.0123 y: 0.0000 control2X: -0.0237 y: -0.0023;
		rel_CubicBezierToX: -0.0336 y: -0.0230 control1X: -0.0104 y: -0.0046 control2X: -0.0216 y: -0.0122! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans228
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.1860 y: -0.4458;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans229
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032;
		rel_MoveToX: -0.1577 y: -0.4322;
		rel_CubicBezierToX: -0.0215 y: -0.0522 control1X: 0.0000 y: -0.0205 control2X: -0.0071 y: -0.0379;
		rel_CubicBezierToX: -0.0527 y: -0.0215 control1X: -0.0143 y: -0.0143 control2X: -0.0319 y: -0.0215;
		rel_CubicBezierToX: -0.0532 y: 0.0215 control1X: -0.0212 y: 0.0000 control2X: -0.0389 y: 0.0072;
		rel_CubicBezierToX: -0.0210 y: 0.0522 control1X: -0.0140 y: 0.0140 control2X: -0.0210 y: 0.0314;
		rel_CubicBezierToX: 0.0210 y: 0.0533 control1X: 0.0000 y: 0.0212 control2X: 0.0070 y: 0.0389;
		rel_CubicBezierToX: 0.0532 y: 0.0210 control1X: 0.0143 y: 0.0140 control2X: 0.0320 y: 0.0210;
		rel_CubicBezierToX: 0.0527 y: -0.0215 control1X: 0.0208 y: 0.0000 control2X: 0.0384 y: -0.0072;
		rel_CubicBezierToX: 0.0215 y: -0.0528 control1X: 0.0144 y: -0.0143 control2X: 0.0215 y: -0.0319;
		rel_MoveToX: 0.0596 y: 0.0000;
		rel_CubicBezierToX: -0.0391 y: 0.0953 control1X: 0.0000 y: 0.0375 control2X: -0.0130 y: 0.0692;
		rel_CubicBezierToX: -0.0947 y: 0.0390 control1X: -0.0257 y: 0.0260 control2X: -0.0573 y: 0.0390;
		rel_CubicBezierToX: -0.0952 y: -0.0390 control1X: -0.0374 y: 0.0000 control2X: -0.0692 y: -0.0130;
		rel_CubicBezierToX: -0.0386 y: -0.0953 control1X: -0.0257 y: -0.0261 control2X: -0.0386 y: -0.0578;
		rel_CubicBezierToX: 0.0386 y: -0.0947 control1X: 0.0000 y: -0.0374 control2X: 0.0129 y: -0.0690;
		rel_CubicBezierToX: 0.0952 y: -0.0390 control1X: 0.0260 y: -0.0260 control2X: 0.0578 y: -0.0390;
		rel_CubicBezierToX: 0.0947 y: 0.0390 control1X: 0.0374 y: 0.0000 control2X: 0.0690 y: 0.0130;
		rel_CubicBezierToX: 0.0391 y: 0.0947 control1X: 0.0261 y: 0.0257 control2X: 0.0391 y: 0.0573! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans230
	vectorGraphicsEngine
		rel_MoveToX: 0.8766 y: 0.6339;
		rel_CubicBezierToX: -0.0410 y: -0.1177 control1X: -0.0003 y: -0.0491 control2X: -0.0140 y: -0.0884;
		rel_CubicBezierToX: -0.1069 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0623 y: -0.0439;
		rel_CubicBezierToX: -0.1201 y: 0.0425 control1X: -0.0498 y: 0.0000 control2X: -0.0899 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3198 y: -0.0005;
		rel_MoveToX: -0.3530 y: -0.1479;
		rel_CubicBezierToX: 0.0879 y: -0.0669 control1X: 0.0241 y: -0.0293 control2X: 0.0534 y: -0.0516;
		rel_CubicBezierToX: 0.1162 y: -0.0230 control1X: 0.0345 y: -0.0153 control2X: 0.0732 y: -0.0230;
		rel_CubicBezierToX: 0.1748 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1318 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0556 y: 0.1421 control1X: 0.0039 y: 0.0622 control2X: 0.0224 y: 0.1096;
		rel_CubicBezierToX: 0.1392 y: 0.0488 control1X: 0.0332 y: 0.0326 control2X: 0.0796 y: 0.0488;
		rel_CubicBezierToX: 0.0996 y: -0.0126 control1X: 0.0339 y: 0.0000 control2X: 0.0671 y: -0.0042;
		rel_CubicBezierToX: 0.0977 y: -0.0391 control1X: 0.0326 y: -0.0088 control2X: 0.0651 y: -0.0219;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0996 y: 0.0313 control1X: -0.0326 y: 0.0137 control2X: -0.0658 y: 0.0241;
		rel_CubicBezierToX: -0.1026 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0681 y: 0.0107;
		rel_CubicBezierToX: -0.1421 y: -0.0293 control1X: -0.0543 y: 0.0000 control2X: -0.1017 y: -0.0097;
		rel_CubicBezierToX: -0.0981 y: -0.0878 control1X: -0.0403 y: -0.0198 control2X: -0.0730 y: -0.0491;
		rel_CubicBezierToX: -0.0884 y: 0.0878 control1X: -0.0238 y: 0.0390 control2X: -0.0532 y: 0.0683;
		rel_CubicBezierToX: -0.1226 y: 0.0293 control1X: -0.0351 y: 0.0196 control2X: -0.0760 y: 0.0293;
		rel_CubicBezierToX: -0.1435 y: -0.0454 control1X: -0.0615 y: 0.0000 control2X: -0.1093 y: -0.0151;
		rel_CubicBezierToX: -0.0513 y: -0.1279 control1X: -0.0342 y: -0.0306 control2X: -0.0513 y: -0.0732;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1211 y: 0.0235 control1X: 0.0462 y: 0.0000 control2X: 0.0866 y: 0.0078;
		rel_CubicBezierToX: 0.0786 y: 0.0664 control1X: 0.0348 y: 0.0156 control2X: 0.0610 y: 0.0377;
		rel_MoveToX: -0.1431 y: 0.1953;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans231
	vectorGraphicsEngine
		rel_MoveToX: 0.5256 y: 0.4303;
		rel_LineToX: 0.0000 y: 0.0840;
		rel_CubicBezierToX: -0.0767 y: -0.0313 control1X: -0.0254 y: -0.0140 control2X: -0.0510 y: -0.0244;
		rel_CubicBezierToX: -0.0771 y: -0.0107 control1X: -0.0254 y: -0.0071 control2X: -0.0511 y: -0.0107;
		rel_CubicBezierToX: -0.1358 y: 0.0557 control1X: -0.0583 y: 0.0000 control2X: -0.1036 y: 0.0185;
		rel_CubicBezierToX: -0.0483 y: 0.1552 control1X: -0.0322 y: 0.0367 control2X: -0.0483 y: 0.0885;
		rel_CubicBezierToX: 0.0483 y: 0.1558 control1X: 0.0000 y: 0.0668 control2X: 0.0161 y: 0.1187;
		rel_CubicBezierToX: 0.1358 y: 0.0552 control1X: 0.0322 y: 0.0368 control2X: 0.0775 y: 0.0552;
		rel_CubicBezierToX: 0.0771 y: -0.0103 control1X: 0.0260 y: 0.0000 control2X: 0.0517 y: -0.0034;
		rel_CubicBezierToX: 0.0767 y: -0.0317 control1X: 0.0257 y: -0.0071 control2X: 0.0513 y: -0.0177;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_CubicBezierToX: -0.0782 y: 0.0264 control1X: -0.0251 y: 0.0117 control2X: -0.0511 y: 0.0205;
		rel_CubicBezierToX: -0.0854 y: 0.0087 control1X: -0.0267 y: 0.0058 control2X: -0.0551 y: 0.0087;
		rel_CubicBezierToX: -0.1963 y: -0.0776 control1X: -0.0824 y: 0.0000 control2X: -0.1478 y: -0.0258;
		rel_CubicBezierToX: -0.0728 y: -0.2095 control1X: -0.0485 y: -0.0518 control2X: -0.0728 y: -0.1216;
		rel_CubicBezierToX: 0.0733 y: -0.2104 control1X: 0.0000 y: -0.0892 control2X: 0.0245 y: -0.1593;
		rel_CubicBezierToX: 0.2016 y: -0.0767 control1X: 0.0491 y: -0.0511 control2X: 0.1164 y: -0.0767;
		rel_CubicBezierToX: 0.0811 y: 0.0088 control1X: 0.0277 y: 0.0000 control2X: 0.0547 y: 0.0030;
		rel_CubicBezierToX: 0.0767 y: 0.0254 control1X: 0.0264 y: 0.0055 control2X: 0.0519 y: 0.0140;
		rel_MoveToX: -0.1270 y: 0.5259;
		rel_CubicBezierToX: 0.0400 y: 0.0556 control1X: 0.0179 y: 0.0202 control2X: 0.0313 y: 0.0387;
		rel_CubicBezierToX: 0.0132 y: 0.0494 control1X: 0.0088 y: 0.0173 control2X: 0.0132 y: 0.0337;
		rel_CubicBezierToX: -0.0293 y: 0.0654 control1X: 0.0000 y: 0.0289 control2X: -0.0097 y: 0.0507;
		rel_CubicBezierToX: -0.0869 y: 0.0225 control1X: -0.0195 y: 0.0150 control2X: -0.0485 y: 0.0225;
		rel_CubicBezierToX: -0.0439 y: -0.0030 control1X: -0.0150 y: 0.0000 control2X: -0.0296 y: -0.0010;
		rel_CubicBezierToX: -0.0420 y: -0.0088 control1X: -0.0140 y: -0.0019 control2X: -0.0280 y: -0.0049;
		rel_LineToX: 0.0000 y: -0.0639;
		rel_CubicBezierToX: 0.0346 y: 0.0117 control1X: 0.0110 y: 0.0055 control2X: 0.0226 y: 0.0094;
		rel_CubicBezierToX: 0.0411 y: 0.0039 control1X: 0.0121 y: 0.0026 control2X: 0.0258 y: 0.0039;
		rel_CubicBezierToX: 0.0434 y: -0.0117 control1X: 0.0192 y: 0.0000 control2X: 0.0337 y: -0.0039;
		rel_CubicBezierToX: 0.0147 y: -0.0342 control1X: 0.0098 y: -0.0078 control2X: 0.0147 y: -0.0192;
		rel_CubicBezierToX: -0.0108 y: -0.0361 control1X: 0.0000 y: -0.0098 control2X: -0.0036 y: -0.0218;
		rel_CubicBezierToX: -0.0322 y: -0.0508 control1X: -0.0068 y: -0.0140 control2X: -0.0176 y: -0.0310;
		rel_LineToX: 0.0581 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans232
	vectorGraphicsEngine
		rel_MoveToX: 0.5998 y: 0.6603;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1396 y: 0.0484 control1X: 0.0335 y: 0.0323 control2X: 0.0800 y: 0.0484;
		rel_CubicBezierToX: 0.1001 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0679 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0991 y: 0.0313 control1X: -0.0322 y: 0.0137 control2X: -0.0653 y: 0.0241;
		rel_CubicBezierToX: -0.1031 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0682 y: 0.0107;
		rel_CubicBezierToX: -0.2075 y: -0.0761 control1X: -0.0872 y: 0.0000 control2X: -0.1564 y: -0.0253;
		rel_CubicBezierToX: -0.0762 y: -0.2061 control1X: -0.0508 y: -0.0508 control2X: -0.0762 y: -0.1195;
		rel_CubicBezierToX: 0.0723 y: -0.2129 control1X: 0.0000 y: -0.0895 control2X: 0.0241 y: -0.1605;
		rel_CubicBezierToX: 0.1958 y: -0.0791 control1X: 0.0485 y: -0.0527 control2X: 0.1138 y: -0.0791;
		rel_CubicBezierToX: 0.1743 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1317 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.0899 y: -0.0264;
		rel_CubicBezierToX: -0.0415 y: -0.1177 control1X: -0.0006 y: -0.0491 control2X: -0.0145 y: -0.0884;
		rel_CubicBezierToX: -0.1064 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0621 y: -0.0439;
		rel_CubicBezierToX: -0.1206 y: 0.0425 control1X: -0.0501 y: 0.0000 control2X: -0.0903 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3203 y: -0.0005;
		rel_MoveToX: -0.2251 y: -0.4775;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans233
	vectorGraphicsEngine
		rel_MoveToX: 0.5998 y: 0.6603;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1396 y: 0.0484 control1X: 0.0335 y: 0.0323 control2X: 0.0800 y: 0.0484;
		rel_CubicBezierToX: 0.1001 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0679 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0991 y: 0.0313 control1X: -0.0322 y: 0.0137 control2X: -0.0653 y: 0.0241;
		rel_CubicBezierToX: -0.1031 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0682 y: 0.0107;
		rel_CubicBezierToX: -0.2075 y: -0.0761 control1X: -0.0872 y: 0.0000 control2X: -0.1564 y: -0.0253;
		rel_CubicBezierToX: -0.0762 y: -0.2061 control1X: -0.0508 y: -0.0508 control2X: -0.0762 y: -0.1195;
		rel_CubicBezierToX: 0.0723 y: -0.2129 control1X: 0.0000 y: -0.0895 control2X: 0.0241 y: -0.1605;
		rel_CubicBezierToX: 0.1958 y: -0.0791 control1X: 0.0485 y: -0.0527 control2X: 0.1138 y: -0.0791;
		rel_CubicBezierToX: 0.1743 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1317 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.0899 y: -0.0264;
		rel_CubicBezierToX: -0.0415 y: -0.1177 control1X: -0.0006 y: -0.0491 control2X: -0.0145 y: -0.0884;
		rel_CubicBezierToX: -0.1064 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0621 y: -0.0439;
		rel_CubicBezierToX: -0.1206 y: 0.0425 control1X: -0.0501 y: 0.0000 control2X: -0.0903 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3203 y: -0.0005;
		rel_MoveToX: -0.0864 y: -0.4775;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: -0.1592 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans234
	vectorGraphicsEngine
		rel_MoveToX: 0.5998 y: 0.6603;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1396 y: 0.0484 control1X: 0.0335 y: 0.0323 control2X: 0.0800 y: 0.0484;
		rel_CubicBezierToX: 0.1001 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0679 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0991 y: 0.0313 control1X: -0.0322 y: 0.0137 control2X: -0.0653 y: 0.0241;
		rel_CubicBezierToX: -0.1031 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0682 y: 0.0107;
		rel_CubicBezierToX: -0.2075 y: -0.0761 control1X: -0.0872 y: 0.0000 control2X: -0.1564 y: -0.0253;
		rel_CubicBezierToX: -0.0762 y: -0.2061 control1X: -0.0508 y: -0.0508 control2X: -0.0762 y: -0.1195;
		rel_CubicBezierToX: 0.0723 y: -0.2129 control1X: 0.0000 y: -0.0895 control2X: 0.0241 y: -0.1605;
		rel_CubicBezierToX: 0.1958 y: -0.0791 control1X: 0.0485 y: -0.0527 control2X: 0.1138 y: -0.0791;
		rel_CubicBezierToX: 0.1743 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1317 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.0899 y: -0.0264;
		rel_CubicBezierToX: -0.0415 y: -0.1177 control1X: -0.0006 y: -0.0491 control2X: -0.0145 y: -0.0884;
		rel_CubicBezierToX: -0.1064 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0621 y: -0.0439;
		rel_CubicBezierToX: -0.1206 y: 0.0425 control1X: -0.0501 y: 0.0000 control2X: -0.0903 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3203 y: -0.0005;
		rel_MoveToX: -0.1904 y: -0.4775;
		rel_LineToX: 0.0723 y: 0.0000;
		rel_LineToX: 0.1196 y: 0.1836;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0879 y: -0.1197;
		rel_LineToX: -0.0879 y: 0.1197;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: 0.1196 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans235
	vectorGraphicsEngine
		rel_MoveToX: 0.5998 y: 0.6603;
		rel_LineToX: 0.0000 y: 0.0439;
		rel_LineToX: -0.4131 y: 0.0000;
		rel_CubicBezierToX: 0.0557 y: 0.1416 control1X: 0.0039 y: 0.0619 control2X: 0.0225 y: 0.1091;
		rel_CubicBezierToX: 0.1396 y: 0.0484 control1X: 0.0335 y: 0.0323 control2X: 0.0800 y: 0.0484;
		rel_CubicBezierToX: 0.1001 y: -0.0127 control1X: 0.0345 y: 0.0000 control2X: 0.0679 y: -0.0043;
		rel_CubicBezierToX: 0.0967 y: -0.0381 control1X: 0.0326 y: -0.0085 control2X: 0.0648 y: -0.0212;
		rel_LineToX: 0.0000 y: 0.0849;
		rel_CubicBezierToX: -0.0991 y: 0.0313 control1X: -0.0322 y: 0.0137 control2X: -0.0653 y: 0.0241;
		rel_CubicBezierToX: -0.1031 y: 0.0107 control1X: -0.0339 y: 0.0072 control2X: -0.0682 y: 0.0107;
		rel_CubicBezierToX: -0.2075 y: -0.0761 control1X: -0.0872 y: 0.0000 control2X: -0.1564 y: -0.0253;
		rel_CubicBezierToX: -0.0762 y: -0.2061 control1X: -0.0508 y: -0.0508 control2X: -0.0762 y: -0.1195;
		rel_CubicBezierToX: 0.0723 y: -0.2129 control1X: 0.0000 y: -0.0895 control2X: 0.0241 y: -0.1605;
		rel_CubicBezierToX: 0.1958 y: -0.0791 control1X: 0.0485 y: -0.0527 control2X: 0.1138 y: -0.0791;
		rel_CubicBezierToX: 0.1743 y: 0.0713 control1X: 0.0736 y: 0.0000 control2X: 0.1317 y: 0.0238;
		rel_CubicBezierToX: 0.0645 y: 0.1929 control1X: 0.0430 y: 0.0472 control2X: 0.0645 y: 0.1115;
		rel_MoveToX: -0.0899 y: -0.0264;
		rel_CubicBezierToX: -0.0415 y: -0.1177 control1X: -0.0006 y: -0.0491 control2X: -0.0145 y: -0.0884;
		rel_CubicBezierToX: -0.1064 y: -0.0439 control1X: -0.0267 y: -0.0293 control2X: -0.0621 y: -0.0439;
		rel_CubicBezierToX: -0.1206 y: 0.0425 control1X: -0.0501 y: 0.0000 control2X: -0.0903 y: 0.0142;
		rel_CubicBezierToX: -0.0518 y: 0.1196 control1X: -0.0300 y: 0.0283 control2X: -0.0472 y: 0.0682;
		rel_LineToX: 0.3203 y: -0.0005;
		rel_MoveToX: -0.1084 y: -0.4355;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans236
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_MoveToX: -0.0259 y: -0.2529;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans237
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_MoveToX: 0.1128 y: -0.2529;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: -0.1592 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans238
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_MoveToX: 0.0088 y: -0.2529;
		rel_LineToX: 0.0723 y: 0.0000;
		rel_LineToX: 0.1196 y: 0.1836;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0879 y: -0.1197;
		rel_LineToX: -0.0879 y: 0.1197;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: 0.1196 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans239
	vectorGraphicsEngine
		rel_MoveToX: 0.1320 y: 0.4093;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_MoveToX: 0.0908 y: -0.2109;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans240
	vectorGraphicsEngine
		rel_MoveToX: 0.4469 y: 0.5094;
		rel_CubicBezierToX: -0.0459 y: -0.0122 control1X: -0.0162 y: -0.0055 control2X: -0.0315 y: -0.0096;
		rel_CubicBezierToX: -0.0415 y: -0.0039 control1X: -0.0143 y: -0.0026 control2X: -0.0281 y: -0.0039;
		rel_CubicBezierToX: -0.1269 y: 0.0527 control1X: -0.0543 y: 0.0000 control2X: -0.0966 y: 0.0176;
		rel_CubicBezierToX: -0.0449 y: 0.1480 control1X: -0.0300 y: 0.0352 control2X: -0.0449 y: 0.0845;
		rel_CubicBezierToX: 0.0424 y: 0.1460 control1X: 0.0000 y: 0.0612 control2X: 0.0141 y: 0.1098;
		rel_CubicBezierToX: 0.1138 y: 0.0542 control1X: 0.0284 y: 0.0361 control2X: 0.0663 y: 0.0542;
		rel_CubicBezierToX: 0.1133 y: -0.0542 control1X: 0.0472 y: 0.0000 control2X: 0.0850 y: -0.0181;
		rel_CubicBezierToX: 0.0425 y: -0.1460 control1X: 0.0283 y: -0.0362 control2X: 0.0425 y: -0.0848;
		rel_CubicBezierToX: -0.0132 y: -0.1055 control1X: 0.0000 y: -0.0397 control2X: -0.0044 y: -0.0749;
		rel_CubicBezierToX: -0.0396 y: -0.0791 control1X: -0.0088 y: -0.0306 control2X: -0.0220 y: -0.0570;
		rel_MoveToX: 0.0308 y: -0.1103;
		rel_CubicBezierToX: 0.0894 y: 0.1401 control1X: 0.0410 y: 0.0465 control2X: 0.0708 y: 0.0932;
		rel_CubicBezierToX: 0.0278 y: 0.1548 control1X: 0.0185 y: 0.0469 control2X: 0.0278 y: 0.0985;
		rel_CubicBezierToX: -0.0693 y: 0.2007 control1X: 0.0000 y: 0.0830 control2X: -0.0231 y: 0.1499;
		rel_CubicBezierToX: -0.1817 y: 0.0756 control1X: -0.0463 y: 0.0504 control2X: -0.1068 y: 0.0756;
		rel_CubicBezierToX: -0.1821 y: -0.0756 control1X: -0.0752 y: 0.0000 control2X: -0.1359 y: -0.0252;
		rel_CubicBezierToX: -0.0689 y: -0.2007 control1X: -0.0459 y: -0.0508 control2X: -0.0689 y: -0.1177;
		rel_CubicBezierToX: 0.0674 y: -0.1978 control1X: 0.0000 y: -0.0814 control2X: 0.0225 y: -0.1473;
		rel_CubicBezierToX: 0.1753 y: -0.0762 control1X: 0.0449 y: -0.0508 control2X: 0.1034 y: -0.0762;
		rel_CubicBezierToX: 0.0215 y: 0.0015 control1X: 0.0059 y: 0.0000 control2X: 0.0130 y: 0.0005;
		rel_CubicBezierToX: 0.0332 y: 0.0034 control1X: 0.0085 y: 0.0007 control2X: 0.0195 y: 0.0018;
		rel_LineToX: -0.0776 y: -0.0879;
		rel_LineToX: -0.1558 y: 0.0523;
		rel_LineToX: -0.0161 y: -0.0484;
		rel_LineToX: 0.1372 y: -0.0449;
		rel_LineToX: -0.0884 y: -0.0996;
		rel_LineToX: 0.1114 y: 0.0000;
		rel_LineToX: 0.0620 y: 0.0708;
		rel_LineToX: 0.1626 y: -0.0542;
		rel_LineToX: 0.0161 y: 0.0474;
		rel_LineToX: -0.1441 y: 0.0478;
		rel_LineToX: 0.0801 y: 0.0909! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans241
	vectorGraphicsEngine
		rel_MoveToX: 0.5866 y: 0.6261;
		rel_LineToX: 0.0000 y: 0.3301;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3272;
		rel_CubicBezierToX: -0.0303 y: -0.1162 control1X: 0.0000 y: -0.0517 control2X: -0.0101 y: -0.0905;
		rel_CubicBezierToX: -0.0908 y: -0.0386 control1X: -0.0202 y: -0.0257 control2X: -0.0505 y: -0.0386;
		rel_CubicBezierToX: -0.1148 y: 0.0464 control1X: -0.0485 y: 0.0000 control2X: -0.0868 y: 0.0155;
		rel_CubicBezierToX: -0.0420 y: 0.1265 control1X: -0.0280 y: 0.0310 control2X: -0.0420 y: 0.0731;
		rel_LineToX: 0.0000 y: 0.3091;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5469;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0850;
		rel_CubicBezierToX: 0.0757 y: -0.0738 control1X: 0.0215 y: -0.0329 control2X: 0.0467 y: -0.0575;
		rel_CubicBezierToX: 0.1011 y: -0.0244 control1X: 0.0293 y: -0.0162 control2X: 0.0630 y: -0.0244;
		rel_CubicBezierToX: 0.1426 y: 0.0586 control1X: 0.0628 y: 0.0000 control2X: 0.1103 y: 0.0196;
		rel_CubicBezierToX: 0.0483 y: 0.1714 control1X: 0.0322 y: 0.0388 control2X: 0.0483 y: 0.0959;
		rel_MoveToX: -0.2266 y: -0.3389;
		rel_LineToX: -0.0278 y: -0.0268;
		rel_CubicBezierToX: -0.0190 y: -0.0142 control1X: -0.0072 y: -0.0065 control2X: -0.0135 y: -0.0112;
		rel_CubicBezierToX: -0.0142 y: -0.0049 control1X: -0.0053 y: -0.0032 control2X: -0.0100 y: -0.0049;
		rel_CubicBezierToX: -0.0273 y: 0.0181 control1X: -0.0124 y: 0.0000 control2X: -0.0215 y: 0.0061;
		rel_CubicBezierToX: -0.0098 y: 0.0576 control1X: -0.0059 y: 0.0117 control2X: -0.0092 y: 0.0309;
		rel_LineToX: -0.0610 y: 0.0000;
		rel_CubicBezierToX: 0.0258 y: -0.1015 control1X: 0.0006 y: -0.0439 control2X: 0.0092 y: -0.0778;
		rel_CubicBezierToX: 0.0694 y: -0.0362 control1X: 0.0166 y: -0.0241 control2X: 0.0397 y: -0.0362;
		rel_CubicBezierToX: 0.0341 y: 0.0069 control1X: 0.0123 y: 0.0000 control2X: 0.0237 y: 0.0023;
		rel_CubicBezierToX: 0.0337 y: 0.0229 control1X: 0.0105 y: 0.0045 control2X: 0.0217 y: 0.0122;
		rel_LineToX: 0.0279 y: 0.0269;
		rel_CubicBezierToX: 0.0185 y: 0.0146 control1X: 0.0071 y: 0.0065 control2X: 0.0133 y: 0.0114;
		rel_CubicBezierToX: 0.0147 y: 0.0044 control1X: 0.0056 y: 0.0029 control2X: 0.0104 y: 0.0044;
		rel_CubicBezierToX: 0.0273 y: -0.0176 control1X: 0.0123 y: 0.0000 control2X: 0.0215 y: -0.0058;
		rel_CubicBezierToX: 0.0098 y: -0.0581 control1X: 0.0059 y: -0.0120 control2X: 0.0091 y: -0.0314;
		rel_LineToX: 0.0610 y: 0.0000;
		rel_CubicBezierToX: -0.0259 y: 0.1021 control1X: -0.0006 y: 0.0440 control2X: -0.0093 y: 0.0780;
		rel_CubicBezierToX: -0.0693 y: 0.0356 control1X: -0.0166 y: 0.0237 control2X: -0.0397 y: 0.0356;
		rel_CubicBezierToX: -0.0342 y: -0.0068 control1X: -0.0124 y: 0.0000 control2X: -0.0238 y: -0.0023;
		rel_CubicBezierToX: -0.0337 y: -0.0230 control1X: -0.0104 y: -0.0046 control2X: -0.0216 y: -0.0122! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans242
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_MoveToX: -0.0708 y: -0.2397;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans243
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_MoveToX: 0.0679 y: -0.2397;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: -0.1592 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans244
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_MoveToX: -0.0361 y: -0.2397;
		rel_LineToX: 0.0723 y: 0.0000;
		rel_LineToX: 0.1196 y: 0.1836;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0879 y: -0.1197;
		rel_LineToX: -0.0879 y: 0.1197;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: 0.1196 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans245
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_MoveToX: -0.0019 y: -0.1089;
		rel_LineToX: -0.0279 y: -0.0268;
		rel_CubicBezierToX: -0.0190 y: -0.0142 control1X: -0.0071 y: -0.0065 control2X: -0.0135 y: -0.0112;
		rel_CubicBezierToX: -0.0142 y: -0.0049 control1X: -0.0052 y: -0.0032 control2X: -0.0099 y: -0.0049;
		rel_CubicBezierToX: -0.0273 y: 0.0181 control1X: -0.0123 y: 0.0000 control2X: -0.0215 y: 0.0061;
		rel_CubicBezierToX: -0.0098 y: 0.0576 control1X: -0.0059 y: 0.0117 control2X: -0.0091 y: 0.0309;
		rel_LineToX: -0.0610 y: 0.0000;
		rel_CubicBezierToX: 0.0259 y: -0.1015 control1X: 0.0006 y: -0.0439 control2X: 0.0093 y: -0.0778;
		rel_CubicBezierToX: 0.0693 y: -0.0362 control1X: 0.0166 y: -0.0241 control2X: 0.0397 y: -0.0362;
		rel_CubicBezierToX: 0.0342 y: 0.0069 control1X: 0.0124 y: 0.0000 control2X: 0.0238 y: 0.0023;
		rel_CubicBezierToX: 0.0337 y: 0.0229 control1X: 0.0104 y: 0.0045 control2X: 0.0216 y: 0.0122;
		rel_LineToX: 0.0278 y: 0.0269;
		rel_CubicBezierToX: 0.0186 y: 0.0146 control1X: 0.0072 y: 0.0065 control2X: 0.0134 y: 0.0114;
		rel_CubicBezierToX: 0.0146 y: 0.0044 control1X: 0.0055 y: 0.0029 control2X: 0.0104 y: 0.0044;
		rel_CubicBezierToX: 0.0274 y: -0.0176 control1X: 0.0124 y: 0.0000 control2X: 0.0215 y: -0.0058;
		rel_CubicBezierToX: 0.0097 y: -0.0581 control1X: 0.0058 y: -0.0120 control2X: 0.0091 y: -0.0314;
		rel_LineToX: 0.0611 y: 0.0000;
		rel_CubicBezierToX: -0.0259 y: 0.1021 control1X: -0.0007 y: 0.0440 control2X: -0.0093 y: 0.0780;
		rel_CubicBezierToX: -0.0694 y: 0.0356 control1X: -0.0166 y: 0.0237 control2X: -0.0397 y: 0.0356;
		rel_CubicBezierToX: -0.0341 y: -0.0068 control1X: -0.0123 y: 0.0000 control2X: -0.0237 y: -0.0023;
		rel_CubicBezierToX: -0.0337 y: -0.0230 control1X: -0.0105 y: -0.0046 control2X: -0.0217 y: -0.0122! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans246
	vectorGraphicsEngine
		rel_MoveToX: 0.3439 y: 0.4723;
		rel_CubicBezierToX: -0.1142 y: 0.0566 control1X: -0.0482 y: 0.0000 control2X: -0.0862 y: 0.0189;
		rel_CubicBezierToX: -0.0420 y: 0.1543 control1X: -0.0280 y: 0.0375 control2X: -0.0420 y: 0.0889;
		rel_CubicBezierToX: 0.0415 y: 0.1548 control1X: 0.0000 y: 0.0655 control2X: 0.0138 y: 0.1171;
		rel_CubicBezierToX: 0.1147 y: 0.0562 control1X: 0.0280 y: 0.0375 control2X: 0.0662 y: 0.0562;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0420 y: -0.1538 control1X: 0.0000 y: -0.0647 control2X: -0.0140 y: -0.1160;
		rel_CubicBezierToX: -0.1138 y: -0.0571 control1X: -0.0280 y: -0.0381 control2X: -0.0659 y: -0.0571;
		rel_MoveToX: 0.0000 y: -0.0762;
		rel_CubicBezierToX: 0.1841 y: 0.0762 control1X: 0.0781 y: 0.0000 control2X: 0.1395 y: 0.0254;
		rel_CubicBezierToX: 0.0669 y: 0.2109 control1X: 0.0446 y: 0.0508 control2X: 0.0669 y: 0.1211;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.1846 y: -0.0761 control1X: -0.0784 y: 0.0000 control2X: -0.1400 y: -0.0253;
		rel_CubicBezierToX: -0.0664 y: -0.2110 control1X: -0.0442 y: -0.0511 control2X: -0.0664 y: -0.1214;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_MoveToX: 0.0459 y: -0.1977;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans247
	vectorGraphicsEngine
		rel_MoveToX: 0.3967 y: 0.4020;
		rel_LineToX: 0.1201 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1201;
		rel_LineToX: -0.1201 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1201;
		rel_MoveToX: 0.0000 y: 0.3613;
		rel_LineToX: 0.1201 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1196;
		rel_LineToX: -0.1201 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1196;
		rel_MoveToX: -0.2530 y: -0.1621;
		rel_LineToX: 0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans248
	vectorGraphicsEngine
		rel_MoveToX: 0.4797 y: 0.5651;
		rel_LineToX: -0.2378 y: 0.2890;
		rel_CubicBezierToX: 0.0449 y: 0.0303 control1X: 0.0133 y: 0.0137 control2X: 0.0283 y: 0.0238;
		rel_CubicBezierToX: 0.0571 y: 0.0098 control1X: 0.0169 y: 0.0065 control2X: 0.0360 y: 0.0098;
		rel_CubicBezierToX: 0.1138 y: -0.0567 control1X: 0.0479 y: 0.0000 control2X: 0.0858 y: -0.0189;
		rel_CubicBezierToX: 0.0420 y: -0.1543 control1X: 0.0280 y: -0.0377 control2X: 0.0420 y: -0.0892;
		rel_CubicBezierToX: -0.0049 y: -0.0669 control1X: 0.0000 y: -0.0257 control2X: -0.0016 y: -0.0480;
		rel_CubicBezierToX: -0.0151 y: -0.0512 control1X: -0.0033 y: -0.0192 control2X: -0.0083 y: -0.0363;
		rel_MoveToX: -0.0347 y: -0.0528;
		rel_CubicBezierToX: -0.0459 y: -0.0298 control1X: -0.0137 y: -0.0133 control2X: -0.0290 y: -0.0232;
		rel_CubicBezierToX: -0.0552 y: -0.0102 control1X: -0.0166 y: -0.0068 control2X: -0.0350 y: -0.0102;
		rel_CubicBezierToX: -0.1152 y: 0.0576 control1X: -0.0491 y: 0.0000 control2X: -0.0875 y: 0.0192;
		rel_CubicBezierToX: -0.0410 y: 0.1602 control1X: -0.0274 y: 0.0381 control2X: -0.0410 y: 0.0915;
		rel_CubicBezierToX: 0.0044 y: 0.0620 control1X: 0.0000 y: 0.0234 control2X: 0.0014 y: 0.0441;
		rel_CubicBezierToX: 0.0146 y: 0.0493 control1X: 0.0032 y: 0.0179 control2X: 0.0081 y: 0.0343;
		rel_LineToX: 0.2383 y: -0.2891;
		rel_MoveToX: -0.2993 y: 0.3628;
		rel_CubicBezierToX: -0.0396 y: -0.0854 control1X: -0.0176 y: -0.0250 control2X: -0.0308 y: -0.0535;
		rel_CubicBezierToX: -0.0132 y: -0.1065 control1X: -0.0088 y: -0.0322 control2X: -0.0132 y: -0.0677;
		rel_CubicBezierToX: 0.0664 y: -0.2109 control1X: 0.0000 y: -0.0898 control2X: 0.0222 y: -0.1601;
		rel_CubicBezierToX: 0.1846 y: -0.0762 control1X: 0.0446 y: -0.0508 control2X: 0.1062 y: -0.0762;
		rel_CubicBezierToX: 0.0840 y: 0.0127 control1X: 0.0303 y: 0.0000 control2X: 0.0583 y: 0.0043;
		rel_CubicBezierToX: 0.0718 y: 0.0376 control1X: 0.0260 y: 0.0082 control2X: 0.0500 y: 0.0207;
		rel_LineToX: 0.0679 y: -0.0820;
		rel_LineToX: 0.0463 y: 0.0386;
		rel_LineToX: -0.0713 y: 0.0874;
		rel_CubicBezierToX: 0.0391 y: 0.0859 control1X: 0.0173 y: 0.0250 control2X: 0.0303 y: 0.0537;
		rel_CubicBezierToX: 0.0132 y: 0.1069 control1X: 0.0088 y: 0.0322 control2X: 0.0132 y: 0.0679;
		rel_CubicBezierToX: -0.0669 y: 0.2110 control1X: 0.0000 y: 0.0896 control2X: -0.0223 y: 0.1599;
		rel_CubicBezierToX: -0.1841 y: 0.0761 control1X: -0.0446 y: 0.0508 control2X: -0.1060 y: 0.0761;
		rel_CubicBezierToX: -0.0864 y: -0.0127 control1X: -0.0312 y: 0.0000 control2X: -0.0600 y: -0.0042;
		rel_CubicBezierToX: -0.0698 y: -0.0376 control1X: -0.0260 y: -0.0084 control2X: -0.0493 y: -0.0209;
		rel_LineToX: -0.0679 y: 0.0821;
		rel_LineToX: -0.0469 y: -0.0381;
		rel_LineToX: 0.0728 y: -0.0889! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans249
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 0.7404;
		rel_LineToX: 0.0000 y: -0.3311;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3276;
		rel_CubicBezierToX: 0.0302 y: 0.1167 control1X: 0.0000 y: 0.0518 control2X: 0.0101 y: 0.0907;
		rel_CubicBezierToX: 0.0909 y: 0.0386 control1X: 0.0202 y: 0.0258 control2X: 0.0505 y: 0.0386;
		rel_CubicBezierToX: 0.1147 y: -0.0464 control1X: 0.0485 y: 0.0000 control2X: 0.0867 y: -0.0154;
		rel_CubicBezierToX: 0.0425 y: -0.1264 control1X: 0.0283 y: -0.0309 control2X: 0.0425 y: -0.0730;
		rel_LineToX: 0.0000 y: -0.3101;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0840;
		rel_CubicBezierToX: -0.0762 y: 0.0742 control1X: -0.0218 y: 0.0332 control2X: -0.0472 y: 0.0579;
		rel_CubicBezierToX: -0.1001 y: 0.0239 control1X: -0.0286 y: 0.0160 control2X: -0.0620 y: 0.0239;
		rel_CubicBezierToX: -0.1430 y: -0.0586 control1X: -0.0628 y: 0.0000 control2X: -0.1105 y: -0.0195;
		rel_CubicBezierToX: -0.0489 y: -0.1713 control1X: -0.0326 y: -0.0390 control2X: -0.0489 y: -0.0961;
		rel_MoveToX: 0.1543 y: -0.5840;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans250
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 0.7404;
		rel_LineToX: 0.0000 y: -0.3311;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3276;
		rel_CubicBezierToX: 0.0302 y: 0.1167 control1X: 0.0000 y: 0.0518 control2X: 0.0101 y: 0.0907;
		rel_CubicBezierToX: 0.0909 y: 0.0386 control1X: 0.0202 y: 0.0258 control2X: 0.0505 y: 0.0386;
		rel_CubicBezierToX: 0.1147 y: -0.0464 control1X: 0.0485 y: 0.0000 control2X: 0.0867 y: -0.0154;
		rel_CubicBezierToX: 0.0425 y: -0.1264 control1X: 0.0283 y: -0.0309 control2X: 0.0425 y: -0.0730;
		rel_LineToX: 0.0000 y: -0.3101;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0840;
		rel_CubicBezierToX: -0.0762 y: 0.0742 control1X: -0.0218 y: 0.0332 control2X: -0.0472 y: 0.0579;
		rel_CubicBezierToX: -0.1001 y: 0.0239 control1X: -0.0286 y: 0.0160 control2X: -0.0620 y: 0.0239;
		rel_CubicBezierToX: -0.1430 y: -0.0586 control1X: -0.0628 y: 0.0000 control2X: -0.1105 y: -0.0195;
		rel_CubicBezierToX: -0.0489 y: -0.1713 control1X: -0.0326 y: -0.0390 control2X: -0.0489 y: -0.0961;
		rel_MoveToX: 0.2930 y: -0.5840;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: -0.1592 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans251
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 0.7404;
		rel_LineToX: 0.0000 y: -0.3311;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3276;
		rel_CubicBezierToX: 0.0302 y: 0.1167 control1X: 0.0000 y: 0.0518 control2X: 0.0101 y: 0.0907;
		rel_CubicBezierToX: 0.0909 y: 0.0386 control1X: 0.0202 y: 0.0258 control2X: 0.0505 y: 0.0386;
		rel_CubicBezierToX: 0.1147 y: -0.0464 control1X: 0.0485 y: 0.0000 control2X: 0.0867 y: -0.0154;
		rel_CubicBezierToX: 0.0425 y: -0.1264 control1X: 0.0283 y: -0.0309 control2X: 0.0425 y: -0.0730;
		rel_LineToX: 0.0000 y: -0.3101;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0840;
		rel_CubicBezierToX: -0.0762 y: 0.0742 control1X: -0.0218 y: 0.0332 control2X: -0.0472 y: 0.0579;
		rel_CubicBezierToX: -0.1001 y: 0.0239 control1X: -0.0286 y: 0.0160 control2X: -0.0620 y: 0.0239;
		rel_CubicBezierToX: -0.1430 y: -0.0586 control1X: -0.0628 y: 0.0000 control2X: -0.1105 y: -0.0195;
		rel_CubicBezierToX: -0.0489 y: -0.1713 control1X: -0.0326 y: -0.0390 control2X: -0.0489 y: -0.0961;
		rel_MoveToX: 0.1890 y: -0.5840;
		rel_LineToX: 0.0723 y: 0.0000;
		rel_LineToX: 0.1196 y: 0.1836;
		rel_LineToX: -0.0679 y: 0.0000;
		rel_LineToX: -0.0879 y: -0.1197;
		rel_LineToX: -0.0879 y: 0.1197;
		rel_LineToX: -0.0678 y: 0.0000;
		rel_LineToX: 0.1196 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans252
	vectorGraphicsEngine
		rel_MoveToX: 0.1227 y: 0.7404;
		rel_LineToX: 0.0000 y: -0.3311;
		rel_LineToX: 0.0899 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3276;
		rel_CubicBezierToX: 0.0302 y: 0.1167 control1X: 0.0000 y: 0.0518 control2X: 0.0101 y: 0.0907;
		rel_CubicBezierToX: 0.0909 y: 0.0386 control1X: 0.0202 y: 0.0258 control2X: 0.0505 y: 0.0386;
		rel_CubicBezierToX: 0.1147 y: -0.0464 control1X: 0.0485 y: 0.0000 control2X: 0.0867 y: -0.0154;
		rel_CubicBezierToX: 0.0425 y: -0.1264 control1X: 0.0283 y: -0.0309 control2X: 0.0425 y: -0.0730;
		rel_LineToX: 0.0000 y: -0.3101;
		rel_LineToX: 0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.5469;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0840;
		rel_CubicBezierToX: -0.0762 y: 0.0742 control1X: -0.0218 y: 0.0332 control2X: -0.0472 y: 0.0579;
		rel_CubicBezierToX: -0.1001 y: 0.0239 control1X: -0.0286 y: 0.0160 control2X: -0.0620 y: 0.0239;
		rel_CubicBezierToX: -0.1430 y: -0.0586 control1X: -0.0628 y: 0.0000 control2X: -0.1105 y: -0.0195;
		rel_CubicBezierToX: -0.0489 y: -0.1713 control1X: -0.0326 y: -0.0390 control2X: -0.0489 y: -0.0961;
		rel_MoveToX: 0.2710 y: -0.5420;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1909 y: 0.0000;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans253
	vectorGraphicsEngine
		rel_MoveToX: 0.3595 y: 1.0070;
		rel_CubicBezierToX: -0.0742 y: 0.1274 control1X: -0.0253 y: 0.0651 control2X: -0.0501 y: 0.1075;
		rel_CubicBezierToX: -0.0967 y: 0.0298 control1X: -0.0241 y: 0.0199 control2X: -0.0563 y: 0.0298;
		rel_LineToX: -0.0717 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.0527 y: 0.0000;
		rel_CubicBezierToX: 0.0576 y: -0.0176 control1X: 0.0247 y: 0.0000 control2X: 0.0439 y: -0.0059;
		rel_CubicBezierToX: 0.0454 y: -0.0830 control1X: 0.0137 y: -0.0117 control2X: 0.0288 y: -0.0394;
		rel_LineToX: 0.0161 y: -0.0410;
		rel_LineToX: -0.2211 y: -0.5381;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: 0.1709 y: 0.4277;
		rel_LineToX: 0.1709 y: -0.4277;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: -0.2403 y: 0.5977;
		rel_MoveToX: 0.0420 y: -0.8506;
		rel_LineToX: 0.0972 y: 0.0000;
		rel_LineToX: -0.1592 y: 0.1836;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: 0.1367 y: -0.1836! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans254
	vectorGraphicsEngine
		rel_MoveToX: 0.2189 y: 0.8742;
		rel_LineToX: 0.0000 y: 0.2900;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.9678;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2959;
		rel_CubicBezierToX: 0.0713 y: -0.0723 control1X: 0.0189 y: -0.0325 control2X: 0.0427 y: -0.0566;
		rel_CubicBezierToX: 0.1035 y: -0.0239 control1X: 0.0290 y: -0.0159 control2X: 0.0635 y: -0.0239;
		rel_CubicBezierToX: 0.1616 y: 0.0791 control1X: 0.0664 y: 0.0000 control2X: 0.1203 y: 0.0264;
		rel_CubicBezierToX: 0.0625 y: 0.2080 control1X: 0.0417 y: 0.0528 control2X: 0.0625 y: 0.1221;
		rel_CubicBezierToX: -0.0625 y: 0.2080 control1X: 0.0000 y: 0.0860 control2X: -0.0208 y: 0.1553;
		rel_CubicBezierToX: -0.1616 y: 0.0791 control1X: -0.0413 y: 0.0528 control2X: -0.0952 y: 0.0791;
		rel_CubicBezierToX: -0.1035 y: -0.0234 control1X: -0.0400 y: 0.0000 control2X: -0.0745 y: -0.0078;
		rel_CubicBezierToX: -0.0713 y: -0.0727 control1X: -0.0286 y: -0.0159 control2X: -0.0524 y: -0.0402;
		rel_MoveToX: 0.3057 y: -0.1910;
		rel_CubicBezierToX: -0.0410 y: -0.1552 control1X: 0.0000 y: -0.0660 control2X: -0.0137 y: -0.1178;
		rel_CubicBezierToX: -0.1118 y: -0.0567 control1X: -0.0271 y: -0.0378 control2X: -0.0643 y: -0.0567;
		rel_CubicBezierToX: -0.1124 y: 0.0567 control1X: -0.0476 y: 0.0000 control2X: -0.0850 y: 0.0189;
		rel_CubicBezierToX: -0.0405 y: 0.1552 control1X: -0.0270 y: 0.0374 control2X: -0.0405 y: 0.0892;
		rel_CubicBezierToX: 0.0405 y: 0.1558 control1X: 0.0000 y: 0.0661 control2X: 0.0135 y: 0.1180;
		rel_CubicBezierToX: 0.1124 y: 0.0561 control1X: 0.0274 y: 0.0374 control2X: 0.0648 y: 0.0561;
		rel_CubicBezierToX: 0.1118 y: -0.0561 control1X: 0.0475 y: 0.0000 control2X: 0.0847 y: -0.0187;
		rel_CubicBezierToX: 0.0410 y: -0.1558 control1X: 0.0273 y: -0.0378 control2X: 0.0410 y: -0.0897! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:45'!
drawRelVeraSans255
	vectorGraphicsEngine
		rel_MoveToX: 0.3595 y: 1.0070;
		rel_CubicBezierToX: -0.0742 y: 0.1274 control1X: -0.0253 y: 0.0651 control2X: -0.0501 y: 0.1075;
		rel_CubicBezierToX: -0.0967 y: 0.0298 control1X: -0.0241 y: 0.0199 control2X: -0.0563 y: 0.0298;
		rel_LineToX: -0.0717 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.0527 y: 0.0000;
		rel_CubicBezierToX: 0.0576 y: -0.0176 control1X: 0.0247 y: 0.0000 control2X: 0.0439 y: -0.0059;
		rel_CubicBezierToX: 0.0454 y: -0.0830 control1X: 0.0137 y: -0.0117 control2X: 0.0288 y: -0.0394;
		rel_LineToX: 0.0161 y: -0.0410;
		rel_LineToX: -0.2211 y: -0.5381;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: 0.1709 y: 0.4277;
		rel_LineToX: 0.1709 y: -0.4277;
		rel_LineToX: 0.0952 y: 0.0000;
		rel_LineToX: -0.2403 y: 0.5977;
		rel_MoveToX: 0.0201 y: -0.8086;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986;
		rel_MoveToX: -0.1910 y: 0.0000;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0986;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0986! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans33
	vectorGraphicsEngine
		rel_MoveToX: 0.1886 y: 0.8322;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_MoveToX: 0.0000 y: -0.6050;
		rel_LineToX: 0.0992 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3198;
		rel_LineToX: -0.0098 y: 0.1743;
		rel_LineToX: -0.0791 y: 0.0000;
		rel_LineToX: -0.0103 y: -0.1743;
		rel_LineToX: 0.0000 y: -0.3198! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans34
	vectorGraphicsEngine
		rel_MoveToX: 0.2170 y: 0.2272;
		rel_LineToX: 0.0000 y: 0.2710;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2710;
		rel_LineToX: 0.0830 y: 0.0000;
		rel_MoveToX: 0.1845 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2710;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2710;
		rel_LineToX: 0.0830 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans35
	vectorGraphicsEngine
		rel_MoveToX: 0.5490 y: 0.5162;
		rel_LineToX: -0.1421 y: 0.0000;
		rel_LineToX: -0.0410 y: 0.1631;
		rel_LineToX: 0.1431 y: 0.0000;
		rel_LineToX: 0.0400 y: -0.1631;
		rel_MoveToX: -0.0732 y: -0.2778;
		rel_LineToX: -0.0508 y: 0.2026;
		rel_LineToX: 0.1426 y: 0.0000;
		rel_LineToX: 0.0512 y: -0.2026;
		rel_LineToX: 0.0781 y: 0.0000;
		rel_LineToX: -0.0502 y: 0.2026;
		rel_LineToX: 0.1523 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0752;
		rel_LineToX: -0.1714 y: 0.0000;
		rel_LineToX: -0.0400 y: 0.1631;
		rel_LineToX: 0.1552 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0747;
		rel_LineToX: -0.1743 y: 0.0000;
		rel_LineToX: -0.0508 y: 0.2022;
		rel_LineToX: -0.0781 y: 0.0000;
		rel_LineToX: 0.0503 y: -0.2022;
		rel_LineToX: -0.1431 y: 0.0000;
		rel_LineToX: -0.0502 y: 0.2022;
		rel_LineToX: -0.0787 y: 0.0000;
		rel_LineToX: 0.0508 y: -0.2022;
		rel_LineToX: -0.1538 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0747;
		rel_LineToX: 0.1719 y: 0.0000;
		rel_LineToX: 0.0410 y: -0.1631;
		rel_LineToX: -0.1572 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.1762 y: 0.0000;
		rel_LineToX: 0.0499 y: -0.2026;
		rel_LineToX: 0.0791 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans36
	vectorGraphicsEngine
		rel_MoveToX: 0.3757 y: 1.1032;
		rel_LineToX: -0.0489 y: 0.0000;
		rel_LineToX: -0.0005 y: -0.1470;
		rel_CubicBezierToX: -0.1025 y: -0.0117 control1X: -0.0341 y: -0.0007 control2X: -0.0683 y: -0.0046;
		rel_CubicBezierToX: -0.1030 y: -0.0332 control1X: -0.0342 y: -0.0075 control2X: -0.0685 y: -0.0186;
		rel_LineToX: 0.0000 y: -0.0879;
		rel_CubicBezierToX: 0.1006 y: 0.0473 control1X: 0.0332 y: 0.0208 control2X: 0.0667 y: 0.0366;
		rel_CubicBezierToX: 0.1054 y: 0.0161 control1X: 0.0341 y: 0.0104 control2X: 0.0693 y: 0.0158;
		rel_LineToX: 0.0000 y: -0.2226;
		rel_CubicBezierToX: -0.1572 y: -0.0596 control1X: -0.0719 y: -0.0117 control2X: -0.1243 y: -0.0316;
		rel_CubicBezierToX: -0.0488 y: -0.1152 control1X: -0.0325 y: -0.0280 control2X: -0.0488 y: -0.0664;
		rel_CubicBezierToX: 0.0532 y: -0.1255 control1X: 0.0000 y: -0.0531 control2X: 0.0177 y: -0.0949;
		rel_CubicBezierToX: 0.1528 y: -0.0527 control1X: 0.0355 y: -0.0306 control2X: 0.0864 y: -0.0482;
		rel_LineToX: 0.0000 y: -0.1148;
		rel_LineToX: 0.0489 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1133;
		rel_CubicBezierToX: 0.0878 y: 0.0098 control1X: 0.0302 y: 0.0013 control2X: 0.0595 y: 0.0046;
		rel_CubicBezierToX: 0.0831 y: 0.0205 control1X: 0.0284 y: 0.0048 control2X: 0.0560 y: 0.0117;
		rel_LineToX: 0.0000 y: 0.0854;
		rel_CubicBezierToX: -0.0835 y: -0.0317 control1X: -0.0271 y: -0.0137 control2X: -0.0549 y: -0.0242;
		rel_CubicBezierToX: -0.0874 y: -0.0132 control1X: -0.0284 y: -0.0075 control2X: -0.0575 y: -0.0119;
		rel_LineToX: 0.0000 y: 0.2085;
		rel_CubicBezierToX: 0.1630 y: 0.0610 control1X: 0.0739 y: 0.0114 control2X: 0.1282 y: 0.0317;
		rel_CubicBezierToX: 0.0523 y: 0.1201 control1X: 0.0349 y: 0.0293 control2X: 0.0523 y: 0.0694;
		rel_CubicBezierToX: -0.0557 y: 0.1304 control1X: 0.0000 y: 0.0551 control2X: -0.0186 y: 0.0985;
		rel_CubicBezierToX: -0.1596 y: 0.0547 control1X: -0.0368 y: 0.0316 control2X: -0.0900 y: 0.0498;
		rel_LineToX: 0.0000 y: 0.1480;
		rel_MoveToX: -0.0489 y: -0.5230;
		rel_LineToX: 0.0000 y: -0.2002;
		rel_CubicBezierToX: -0.0864 y: 0.0322 control1X: -0.0377 y: 0.0042 control2X: -0.0665 y: 0.0150;
		rel_CubicBezierToX: -0.0298 y: 0.0689 control1X: -0.0199 y: 0.0173 control2X: -0.0298 y: 0.0402;
		rel_CubicBezierToX: 0.0274 y: 0.0654 control1X: 0.0000 y: 0.0280 control2X: 0.0091 y: 0.0498;
		rel_CubicBezierToX: 0.0888 y: 0.0337 control1X: 0.0185 y: 0.0156 control2X: 0.0481 y: 0.0269;
		rel_MoveToX: 0.0489 y: 0.0938;
		rel_LineToX: 0.0000 y: 0.2114;
		rel_CubicBezierToX: 0.0932 y: -0.0352 control1X: 0.0413 y: -0.0056 control2X: 0.0724 y: -0.0173;
		rel_CubicBezierToX: 0.0318 y: -0.0708 control1X: 0.0212 y: -0.0179 control2X: 0.0318 y: -0.0415;
		rel_CubicBezierToX: -0.0303 y: -0.0683 control1X: 0.0000 y: -0.0286 control2X: -0.0101 y: -0.0514;
		rel_CubicBezierToX: -0.0947 y: -0.0371 control1X: -0.0199 y: -0.0170 control2X: -0.0514 y: -0.0293! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans37
	vectorGraphicsEngine
		rel_MoveToX: 0.7648 y: 0.6354;
		rel_CubicBezierToX: -0.0669 y: 0.0361 control1X: -0.0283 y: 0.0000 control2X: -0.0506 y: 0.0120;
		rel_CubicBezierToX: -0.0239 y: 0.1006 control1X: -0.0159 y: 0.0241 control2X: -0.0239 y: 0.0576;
		rel_CubicBezierToX: 0.0239 y: 0.1001 control1X: 0.0000 y: 0.0423 control2X: 0.0080 y: 0.0757;
		rel_CubicBezierToX: 0.0669 y: 0.0361 control1X: 0.0163 y: 0.0241 control2X: 0.0386 y: 0.0361;
		rel_CubicBezierToX: 0.0654 y: -0.0361 control1X: 0.0277 y: 0.0000 control2X: 0.0495 y: -0.0120;
		rel_CubicBezierToX: 0.0245 y: -0.1001 control1X: 0.0163 y: -0.0244 control2X: 0.0245 y: -0.0578;
		rel_CubicBezierToX: -0.0245 y: -0.1001 control1X: 0.0000 y: -0.0426 control2X: -0.0082 y: -0.0760;
		rel_CubicBezierToX: -0.0654 y: -0.0366 control1X: -0.0159 y: -0.0244 control2X: -0.0377 y: -0.0366;
		rel_MoveToX: 0.0000 y: -0.0620;
		rel_CubicBezierToX: 0.1226 y: 0.0537 control1X: 0.0514 y: 0.0000 control2X: 0.0923 y: 0.0179;
		rel_CubicBezierToX: 0.0454 y: 0.1450 control1X: 0.0302 y: 0.0358 control2X: 0.0454 y: 0.0841;
		rel_CubicBezierToX: -0.0459 y: 0.1450 control1X: 0.0000 y: 0.0609 control2X: -0.0153 y: 0.1092;
		rel_CubicBezierToX: -0.1221 y: 0.0532 control1X: -0.0303 y: 0.0355 control2X: -0.0710 y: 0.0532;
		rel_CubicBezierToX: -0.1235 y: -0.0532 control1X: -0.0521 y: 0.0000 control2X: -0.0932 y: -0.0177;
		rel_CubicBezierToX: -0.0454 y: -0.1450 control1X: -0.0303 y: -0.0358 control2X: -0.0454 y: -0.0841;
		rel_CubicBezierToX: 0.0454 y: -0.1450 control1X: 0.0000 y: -0.0612 control2X: 0.0151 y: -0.1095;
		rel_CubicBezierToX: 0.1235 y: -0.0537 control1X: 0.0306 y: -0.0358 control2X: 0.0718 y: -0.0537;
		rel_MoveToX: -0.5039 y: -0.2974;
		rel_CubicBezierToX: -0.0664 y: 0.0366 control1X: -0.0280 y: 0.0000 control2X: -0.0501 y: 0.0122;
		rel_CubicBezierToX: -0.0239 y: 0.0996 control1X: -0.0159 y: 0.0241 control2X: -0.0239 y: 0.0573;
		rel_CubicBezierToX: 0.0239 y: 0.1006 control1X: 0.0000 y: 0.0430 control2X: 0.0080 y: 0.0765;
		rel_CubicBezierToX: 0.0664 y: 0.0362 control1X: 0.0160 y: 0.0241 control2X: 0.0381 y: 0.0362;
		rel_CubicBezierToX: 0.0664 y: -0.0362 control1X: 0.0283 y: 0.0000 control2X: 0.0505 y: -0.0121;
		rel_CubicBezierToX: 0.0244 y: -0.1006 control1X: 0.0163 y: -0.0241 control2X: 0.0244 y: -0.0576;
		rel_CubicBezierToX: -0.0244 y: -0.0996 control1X: 0.0000 y: -0.0420 control2X: -0.0081 y: -0.0752;
		rel_CubicBezierToX: -0.0664 y: -0.0366 control1X: -0.0163 y: -0.0244 control2X: -0.0384 y: -0.0366;
		rel_MoveToX: 0.4409 y: -0.0620;
		rel_LineToX: 0.0782 y: 0.0000;
		rel_LineToX: -0.4561 y: 0.7563;
		rel_LineToX: -0.0781 y: 0.0000;
		rel_LineToX: 0.4560 y: -0.7563;
		rel_MoveToX: -0.4409 y: 0.0000;
		rel_CubicBezierToX: 0.1231 y: 0.0537 control1X: 0.0514 y: 0.0000 control2X: 0.0925 y: 0.0179;
		rel_CubicBezierToX: 0.0459 y: 0.1445 control1X: 0.0306 y: 0.0355 control2X: 0.0459 y: 0.0837;
		rel_CubicBezierToX: -0.0459 y: 0.1455 control1X: 0.0000 y: 0.0616 control2X: -0.0153 y: 0.1101;
		rel_CubicBezierToX: -0.1231 y: 0.0533 control1X: -0.0303 y: 0.0355 control2X: -0.0713 y: 0.0533;
		rel_CubicBezierToX: -0.1230 y: -0.0533 control1X: -0.0517 y: 0.0000 control2X: -0.0928 y: -0.0178;
		rel_CubicBezierToX: -0.0450 y: -0.1455 control1X: -0.0300 y: -0.0358 control2X: -0.0450 y: -0.0843;
		rel_CubicBezierToX: 0.0455 y: -0.1445 control1X: 0.0000 y: -0.0605 control2X: 0.0152 y: -0.1087;
		rel_CubicBezierToX: 0.1225 y: -0.0537 control1X: 0.0302 y: -0.0358 control2X: 0.0711 y: -0.0537! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans38
	vectorGraphicsEngine
		rel_MoveToX: 0.2809 y: 0.5641;
		rel_CubicBezierToX: -0.0654 y: 0.0791 control1X: -0.0296 y: 0.0264 control2X: -0.0514 y: 0.0527;
		rel_CubicBezierToX: -0.0205 y: 0.0820 control1X: -0.0137 y: 0.0260 control2X: -0.0205 y: 0.0534;
		rel_CubicBezierToX: 0.0518 y: 0.1187 control1X: 0.0000 y: 0.0476 control2X: 0.0172 y: 0.0871;
		rel_CubicBezierToX: 0.1298 y: 0.0473 control1X: 0.0345 y: 0.0316 control2X: 0.0778 y: 0.0473;
		rel_CubicBezierToX: 0.0869 y: -0.0151 control1X: 0.0310 y: 0.0000 control2X: 0.0599 y: -0.0050;
		rel_CubicBezierToX: 0.0762 y: -0.0469 control1X: 0.0271 y: -0.0104 control2X: 0.0525 y: -0.0260;
		rel_LineToX: -0.2588 y: -0.2651;
		rel_MoveToX: 0.0689 y: -0.0547;
		rel_LineToX: 0.2480 y: 0.2539;
		rel_CubicBezierToX: 0.0449 y: -0.0928 control1X: 0.0192 y: -0.0290 control2X: 0.0342 y: -0.0599;
		rel_CubicBezierToX: 0.0191 y: -0.1054 control1X: 0.0108 y: -0.0332 control2X: 0.0171 y: -0.0683;
		rel_LineToX: 0.0908 y: 0.0000;
		rel_CubicBezierToX: -0.0312 y: 0.1274 control1X: -0.0039 y: 0.0429 control2X: -0.0143 y: 0.0854;
		rel_CubicBezierToX: -0.0708 y: 0.1245 control1X: -0.0170 y: 0.0420 control2X: -0.0406 y: 0.0835;
		rel_LineToX: 0.1362 y: 0.1392;
		rel_LineToX: -0.1231 y: 0.0000;
		rel_LineToX: -0.0698 y: -0.0718;
		rel_CubicBezierToX: -0.1064 y: 0.0649 control1X: -0.0338 y: 0.0290 control2X: -0.0693 y: 0.0506;
		rel_CubicBezierToX: -0.1197 y: 0.0210 control1X: -0.0371 y: 0.0140 control2X: -0.0770 y: 0.0210;
		rel_CubicBezierToX: -0.1923 y: -0.0669 control1X: -0.0784 y: 0.0000 control2X: -0.1425 y: -0.0223;
		rel_CubicBezierToX: -0.0747 y: -0.1723 control1X: -0.0498 y: -0.0449 control2X: -0.0747 y: -0.1023;
		rel_CubicBezierToX: 0.0327 y: -0.1172 control1X: 0.0000 y: -0.0417 control2X: 0.0109 y: -0.0807;
		rel_CubicBezierToX: 0.0981 y: -0.1035 control1X: 0.0218 y: -0.0368 control2X: 0.0545 y: -0.0713;
		rel_CubicBezierToX: -0.0356 y: -0.0611 control1X: -0.0156 y: -0.0205 control2X: -0.0275 y: -0.0409;
		rel_CubicBezierToX: -0.0122 y: -0.0600 control1X: -0.0082 y: -0.0205 control2X: -0.0122 y: -0.0405;
		rel_CubicBezierToX: 0.0542 y: -0.1270 control1X: 0.0000 y: -0.0527 control2X: 0.0180 y: -0.0951;
		rel_CubicBezierToX: 0.1440 y: -0.0483 control1X: 0.0361 y: -0.0322 control2X: 0.0841 y: -0.0483;
		rel_CubicBezierToX: 0.0806 y: 0.0088 control1X: 0.0270 y: 0.0000 control2X: 0.0539 y: 0.0029;
		rel_CubicBezierToX: 0.0820 y: 0.0264 control1X: 0.0270 y: 0.0058 control2X: 0.0543 y: 0.0146;
		rel_LineToX: 0.0000 y: 0.0888;
		rel_CubicBezierToX: -0.0811 y: -0.0347 control1X: -0.0283 y: -0.0153 control2X: -0.0553 y: -0.0268;
		rel_CubicBezierToX: -0.0717 y: -0.0122 control1X: -0.0257 y: -0.0081 control2X: -0.0496 y: -0.0122;
		rel_CubicBezierToX: -0.0835 y: 0.0274 control1X: -0.0342 y: 0.0000 control2X: -0.0620 y: 0.0092;
		rel_CubicBezierToX: -0.0318 y: 0.0698 control1X: -0.0212 y: 0.0179 control2X: -0.0318 y: 0.0412;
		rel_CubicBezierToX: 0.0142 y: 0.0503 control1X: 0.0000 y: 0.0166 control2X: 0.0048 y: 0.0334;
		rel_CubicBezierToX: 0.0591 y: 0.0708 control1X: 0.0098 y: 0.0166 control2X: 0.0295 y: 0.0402! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans39
	vectorGraphicsEngine
		rel_MoveToX: 0.2170 y: 0.2272;
		rel_LineToX: 0.0000 y: 0.2710;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2710;
		rel_LineToX: 0.0830 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans40
	vectorGraphicsEngine
		rel_MoveToX: 0.3478 y: 0.1974;
		rel_CubicBezierToX: -0.0971 y: 0.2222 control1X: -0.0436 y: 0.0749 control2X: -0.0760 y: 0.1489;
		rel_CubicBezierToX: -0.0318 y: 0.2226 control1X: -0.0212 y: 0.0732 control2X: -0.0318 y: 0.1474;
		rel_CubicBezierToX: 0.0318 y: 0.2236 control1X: 0.0000 y: 0.0752 control2X: 0.0106 y: 0.1498;
		rel_CubicBezierToX: 0.0971 y: 0.2222 control1X: 0.0214 y: 0.0736 control2X: 0.0538 y: 0.1477;
		rel_LineToX: -0.0781 y: 0.0000;
		rel_CubicBezierToX: -0.1099 y: -0.2256 control1X: -0.0488 y: -0.0765 control2X: -0.0854 y: -0.1517;
		rel_CubicBezierToX: -0.0361 y: -0.2202 control1X: -0.0241 y: -0.0739 control2X: -0.0361 y: -0.1473;
		rel_CubicBezierToX: 0.0361 y: -0.2192 control1X: 0.0000 y: -0.0726 control2X: 0.0120 y: -0.1457;
		rel_CubicBezierToX: 0.1099 y: -0.2256 control1X: 0.0241 y: -0.0736 control2X: 0.0607 y: -0.1488;
		rel_LineToX: 0.0781 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans41
	vectorGraphicsEngine
		rel_MoveToX: 0.1178 y: 0.1974;
		rel_LineToX: 0.0782 y: 0.0000;
		rel_CubicBezierToX: 0.1093 y: 0.2256 control1X: 0.0488 y: 0.0768 control2X: 0.0853 y: 0.1520;
		rel_CubicBezierToX: 0.0367 y: 0.2192 control1X: 0.0245 y: 0.0735 control2X: 0.0367 y: 0.1466;
		rel_CubicBezierToX: -0.0367 y: 0.2202 control1X: 0.0000 y: 0.0729 control2X: -0.0122 y: 0.1463;
		rel_CubicBezierToX: -0.1093 y: 0.2256 control1X: -0.0240 y: 0.0739 control2X: -0.0605 y: 0.1491;
		rel_LineToX: -0.0782 y: 0.0000;
		rel_CubicBezierToX: 0.0967 y: -0.2222 control1X: 0.0433 y: -0.0745 control2X: 0.0756 y: -0.1486;
		rel_CubicBezierToX: 0.0323 y: -0.2236 control1X: 0.0215 y: -0.0738 control2X: 0.0323 y: -0.1484;
		rel_CubicBezierToX: -0.0323 y: -0.2226 control1X: 0.0000 y: -0.0752 control2X: -0.0108 y: -0.1494;
		rel_CubicBezierToX: -0.0967 y: -0.2222 control1X: -0.0211 y: -0.0733 control2X: -0.0534 y: -0.1473! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans42
	vectorGraphicsEngine
		rel_MoveToX: 0.5080 y: 0.3473;
		rel_LineToX: -0.1753 y: 0.0947;
		rel_LineToX: 0.1753 y: 0.0952;
		rel_LineToX: -0.0283 y: 0.0479;
		rel_LineToX: -0.1641 y: -0.0991;
		rel_LineToX: 0.0000 y: 0.1840;
		rel_LineToX: -0.0557 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1840;
		rel_LineToX: -0.1640 y: 0.0991;
		rel_LineToX: -0.0283 y: -0.0479;
		rel_LineToX: 0.1752 y: -0.0952;
		rel_LineToX: -0.1752 y: -0.0947;
		rel_LineToX: 0.0283 y: -0.0483;
		rel_LineToX: 0.1640 y: 0.0991;
		rel_LineToX: 0.0000 y: -0.1841;
		rel_LineToX: 0.0557 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1841;
		rel_LineToX: 0.1641 y: -0.0991;
		rel_LineToX: 0.0283 y: 0.0483! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans43
	vectorGraphicsEngine
		rel_MoveToX: 0.4977 y: 0.3292;
		rel_LineToX: 0.0000 y: 0.2720;
		rel_LineToX: 0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2720;
		rel_LineToX: -0.0820 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2720;
		rel_LineToX: -0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.2720 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2720;
		rel_LineToX: 0.0820 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans44
	vectorGraphicsEngine
		rel_MoveToX: 0.1550 y: 0.8322;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0839;
		rel_LineToX: -0.0801 y: 0.1563;
		rel_LineToX: -0.0630 y: 0.0000;
		rel_LineToX: 0.0401 y: -0.1563;
		rel_LineToX: 0.0000 y: -0.0839! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans45
	vectorGraphicsEngine
		rel_MoveToX: 0.0866 y: 0.6422;
		rel_LineToX: 0.2632 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0801;
		rel_LineToX: -0.2632 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0801! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans46
	vectorGraphicsEngine
		rel_MoveToX: 0.1447 y: 0.8322;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans47
	vectorGraphicsEngine
		rel_MoveToX: 0.2917 y: 0.2272;
		rel_LineToX: 0.0830 y: 0.0000;
		rel_LineToX: -0.2539 y: 0.8218;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.2539 y: -0.8218! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans48
	vectorGraphicsEngine
		rel_MoveToX: 0.3556 y: 0.2921;
		rel_CubicBezierToX: -0.1147 y: 0.0752 control1X: -0.0507 y: 0.0000 control2X: -0.0890 y: 0.0251;
		rel_CubicBezierToX: -0.0381 y: 0.2251 control1X: -0.0254 y: 0.0498 control2X: -0.0381 y: 0.1249;
		rel_CubicBezierToX: 0.0381 y: 0.2251 control1X: 0.0000 y: 0.0999 control2X: 0.0127 y: 0.1750;
		rel_CubicBezierToX: 0.1147 y: 0.0747 control1X: 0.0257 y: 0.0498 control2X: 0.0640 y: 0.0747;
		rel_CubicBezierToX: 0.1148 y: -0.0747 control1X: 0.0511 y: 0.0000 control2X: 0.0894 y: -0.0249;
		rel_CubicBezierToX: 0.0386 y: -0.2251 control1X: 0.0257 y: -0.0501 control2X: 0.0386 y: -0.1252;
		rel_CubicBezierToX: -0.0386 y: -0.2251 control1X: 0.0000 y: -0.1002 control2X: -0.0129 y: -0.1753;
		rel_CubicBezierToX: -0.1148 y: -0.0752 control1X: -0.0254 y: -0.0501 control2X: -0.0637 y: -0.0752;
		rel_MoveToX: 0.0000 y: -0.0781;
		rel_CubicBezierToX: 0.1870 y: 0.0972 control1X: 0.0817 y: 0.0000 control2X: 0.1441 y: 0.0324;
		rel_CubicBezierToX: 0.0650 y: 0.2812 control1X: 0.0433 y: 0.0644 control2X: 0.0650 y: 0.1582;
		rel_CubicBezierToX: -0.0650 y: 0.2813 control1X: 0.0000 y: 0.1227 control2X: -0.0217 y: 0.2165;
		rel_CubicBezierToX: -0.1870 y: 0.0966 control1X: -0.0429 y: 0.0644 control2X: -0.1053 y: 0.0966;
		rel_CubicBezierToX: -0.1875 y: -0.0966 control1X: -0.0817 y: 0.0000 control2X: -0.1442 y: -0.0322;
		rel_CubicBezierToX: -0.0644 y: -0.2813 control1X: -0.0429 y: -0.0648 control2X: -0.0644 y: -0.1586;
		rel_CubicBezierToX: 0.0644 y: -0.2812 control1X: 0.0000 y: -0.1230 control2X: 0.0215 y: -0.2168;
		rel_CubicBezierToX: 0.1875 y: -0.0972 control1X: 0.0433 y: -0.0648 control2X: 0.1058 y: -0.0972! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans49
	vectorGraphicsEngine
		rel_MoveToX: 0.1618 y: 0.8732;
		rel_LineToX: 0.1611 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5562;
		rel_LineToX: -0.1753 y: 0.0352;
		rel_LineToX: 0.0000 y: -0.0899;
		rel_LineToX: 0.1743 y: -0.0351;
		rel_LineToX: 0.0987 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.6460;
		rel_LineToX: 0.1611 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4199 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans50
	vectorGraphicsEngine
		rel_MoveToX: 0.2297 y: 0.8732;
		rel_LineToX: 0.3442 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4629 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.1528 y: -0.1558 control1X: 0.0374 y: -0.0388 control2X: 0.0884 y: -0.0907;
		rel_CubicBezierToX: 0.1221 y: -0.1265 control1X: 0.0648 y: -0.0654 control2X: 0.1055 y: -0.1076;
		rel_CubicBezierToX: 0.0659 y: -0.0898 control1X: 0.0316 y: -0.0354 control2X: 0.0536 y: -0.0654;
		rel_CubicBezierToX: 0.0191 y: -0.0728 control1X: 0.0127 y: -0.0247 control2X: 0.0191 y: -0.0490;
		rel_CubicBezierToX: -0.0410 y: -0.0947 control1X: 0.0000 y: -0.0387 control2X: -0.0137 y: -0.0703;
		rel_CubicBezierToX: -0.1060 y: -0.0366 control1X: -0.0271 y: -0.0244 control2X: -0.0624 y: -0.0366;
		rel_CubicBezierToX: -0.0981 y: 0.0161 control1X: -0.0309 y: 0.0000 control2X: -0.0636 y: 0.0054;
		rel_CubicBezierToX: -0.1099 y: 0.0488 control1X: -0.0342 y: 0.0108 control2X: -0.0708 y: 0.0270;
		rel_LineToX: 0.0000 y: -0.0996;
		rel_CubicBezierToX: 0.1113 y: -0.0361 control1X: 0.0397 y: -0.0159 control2X: 0.0768 y: -0.0280;
		rel_CubicBezierToX: 0.0947 y: -0.0122 control1X: 0.0345 y: -0.0081 control2X: 0.0661 y: -0.0122;
		rel_CubicBezierToX: 0.1807 y: 0.0566 control1X: 0.0756 y: 0.0000 control2X: 0.1358 y: 0.0189;
		rel_CubicBezierToX: 0.0674 y: 0.1514 control1X: 0.0449 y: 0.0378 control2X: 0.0674 y: 0.0883;
		rel_CubicBezierToX: -0.0171 y: 0.0855 control1X: 0.0000 y: 0.0300 control2X: -0.0057 y: 0.0584;
		rel_CubicBezierToX: -0.0610 y: 0.0947 control1X: -0.0111 y: 0.0266 control2X: -0.0314 y: 0.0582;
		rel_CubicBezierToX: -0.0777 y: 0.0820 control1X: -0.0082 y: 0.0094 control2X: -0.0340 y: 0.0368;
		rel_CubicBezierToX: -0.1845 y: 0.1890 control1X: -0.0436 y: 0.0449 control2X: -0.1051 y: 0.1079! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans51
	vectorGraphicsEngine
		rel_MoveToX: 0.4435 y: 0.5631;
		rel_CubicBezierToX: 0.1104 y: 0.0630 control1X: 0.0472 y: 0.0101 control2X: 0.0840 y: 0.0311;
		rel_CubicBezierToX: 0.0400 y: 0.1182 control1X: 0.0267 y: 0.0319 control2X: 0.0400 y: 0.0713;
		rel_CubicBezierToX: -0.0742 y: 0.1670 control1X: 0.0000 y: 0.0719 control2X: -0.0247 y: 0.1276;
		rel_CubicBezierToX: -0.2109 y: 0.0590 control1X: -0.0495 y: 0.0393 control2X: -0.1198 y: 0.0590;
		rel_CubicBezierToX: -0.0948 y: -0.0092 control1X: -0.0306 y: 0.0000 control2X: -0.0622 y: -0.0031;
		rel_CubicBezierToX: -0.1001 y: -0.0269 control1X: -0.0322 y: -0.0059 control2X: -0.0656 y: -0.0148;
		rel_LineToX: 0.0000 y: -0.0952;
		rel_CubicBezierToX: 0.0899 y: 0.0361 control1X: 0.0274 y: 0.0159 control2X: 0.0573 y: 0.0280;
		rel_CubicBezierToX: 0.1020 y: 0.0122 control1X: 0.0325 y: 0.0082 control2X: 0.0666 y: 0.0122;
		rel_CubicBezierToX: 0.1411 y: -0.0366 control1X: 0.0619 y: 0.0000 control2X: 0.1089 y: -0.0122;
		rel_CubicBezierToX: 0.0489 y: -0.1064 control1X: 0.0326 y: -0.0244 control2X: 0.0489 y: -0.0599;
		rel_CubicBezierToX: -0.0454 y: -0.1006 control1X: 0.0000 y: -0.0430 control2X: -0.0152 y: -0.0765;
		rel_CubicBezierToX: -0.1255 y: -0.0366 control1X: -0.0300 y: -0.0244 control2X: -0.0718 y: -0.0366;
		rel_LineToX: -0.0850 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0811;
		rel_LineToX: 0.0889 y: 0.0000;
		rel_CubicBezierToX: 0.1113 y: -0.0288 control1X: 0.0485 y: 0.0000 control2X: 0.0856 y: -0.0096;
		rel_CubicBezierToX: 0.0386 y: -0.0840 control1X: 0.0257 y: -0.0195 control2X: 0.0386 y: -0.0475;
		rel_CubicBezierToX: -0.0401 y: -0.0859 control1X: 0.0000 y: -0.0374 control2X: -0.0134 y: -0.0661;
		rel_CubicBezierToX: -0.1137 y: -0.0303 control1X: -0.0263 y: -0.0202 control2X: -0.0642 y: -0.0303;
		rel_CubicBezierToX: -0.0869 y: 0.0088 control1X: -0.0270 y: 0.0000 control2X: -0.0560 y: 0.0029;
		rel_CubicBezierToX: -0.1021 y: 0.0273 control1X: -0.0310 y: 0.0059 control2X: -0.0650 y: 0.0150;
		rel_LineToX: 0.0000 y: -0.0879;
		rel_CubicBezierToX: 0.1050 y: -0.0234 control1X: 0.0374 y: -0.0104 control2X: 0.0724 y: -0.0182;
		rel_CubicBezierToX: 0.0928 y: -0.0078 control1X: 0.0329 y: -0.0052 control2X: 0.0638 y: -0.0078;
		rel_CubicBezierToX: 0.1777 y: 0.0513 control1X: 0.0748 y: 0.0000 control2X: 0.1341 y: 0.0171;
		rel_CubicBezierToX: 0.0654 y: 0.1377 control1X: 0.0436 y: 0.0338 control2X: 0.0654 y: 0.0797;
		rel_CubicBezierToX: -0.0346 y: 0.1025 control1X: 0.0000 y: 0.0403 control2X: -0.0115 y: 0.0745;
		rel_CubicBezierToX: -0.0987 y: 0.0576 control1X: -0.0232 y: 0.0277 control2X: -0.0560 y: 0.0469! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans52
	vectorGraphicsEngine
		rel_MoveToX: 0.4157 y: 0.3131;
		rel_LineToX: -0.2490 y: 0.3892;
		rel_LineToX: 0.2490 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3892;
		rel_MoveToX: -0.0259 y: -0.0859;
		rel_LineToX: 0.1240 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4751;
		rel_LineToX: 0.1040 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0820;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1719;
		rel_LineToX: -0.0981 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1719;
		rel_LineToX: -0.3291 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0952;
		rel_LineToX: 0.3032 y: -0.4619! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans53
	vectorGraphicsEngine
		rel_MoveToX: 0.1457 y: 0.2272;
		rel_LineToX: 0.3872 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.2969 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1787;
		rel_CubicBezierToX: 0.0430 y: -0.0107 control1X: 0.0143 y: -0.0049 control2X: 0.0287 y: -0.0085;
		rel_CubicBezierToX: 0.0429 y: -0.0040 control1X: 0.0143 y: -0.0026 control2X: 0.0286 y: -0.0040;
		rel_CubicBezierToX: 0.1934 y: 0.0669 control1X: 0.0814 y: 0.0000 control2X: 0.1459 y: 0.0223;
		rel_CubicBezierToX: 0.0713 y: 0.1812 control1X: 0.0475 y: 0.0446 control2X: 0.0713 y: 0.1050;
		rel_CubicBezierToX: -0.0732 y: 0.1831 control1X: 0.0000 y: 0.0784 control2X: -0.0244 y: 0.1395;
		rel_CubicBezierToX: -0.2066 y: 0.0649 control1X: -0.0489 y: 0.0433 control2X: -0.1177 y: 0.0649;
		rel_CubicBezierToX: -0.0937 y: -0.0078 control1X: -0.0306 y: 0.0000 control2X: -0.0618 y: -0.0026;
		rel_CubicBezierToX: -0.0982 y: -0.0234 control1X: -0.0316 y: -0.0052 control2X: -0.0643 y: -0.0130;
		rel_LineToX: 0.0000 y: -0.0991;
		rel_CubicBezierToX: 0.0908 y: 0.0356 control1X: 0.0293 y: 0.0159 control2X: 0.0596 y: 0.0278;
		rel_CubicBezierToX: 0.0992 y: 0.0117 control1X: 0.0313 y: 0.0078 control2X: 0.0643 y: 0.0117;
		rel_CubicBezierToX: 0.1337 y: -0.0444 control1X: 0.0563 y: 0.0000 control2X: 0.1009 y: -0.0148;
		rel_CubicBezierToX: 0.0494 y: -0.1206 control1X: 0.0329 y: -0.0296 control2X: 0.0494 y: -0.0698;
		rel_CubicBezierToX: -0.0494 y: -0.1206 control1X: 0.0000 y: -0.0508 control2X: -0.0165 y: -0.0910;
		rel_CubicBezierToX: -0.1337 y: -0.0444 control1X: -0.0328 y: -0.0296 control2X: -0.0774 y: -0.0444;
		rel_CubicBezierToX: -0.0791 y: 0.0087 control1X: -0.0264 y: 0.0000 control2X: -0.0528 y: 0.0029;
		rel_CubicBezierToX: -0.0801 y: 0.0274 control1X: -0.0261 y: 0.0059 control2X: -0.0528 y: 0.0150;
		rel_LineToX: 0.0000 y: -0.3662! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans54
	vectorGraphicsEngine
		rel_MoveToX: 0.3678 y: 0.5524;
		rel_CubicBezierToX: -0.1054 y: 0.0454 control1X: -0.0442 y: 0.0000 control2X: -0.0794 y: 0.0151;
		rel_CubicBezierToX: -0.0386 y: 0.1245 control1X: -0.0257 y: 0.0303 control2X: -0.0386 y: 0.0718;
		rel_CubicBezierToX: 0.0386 y: 0.1245 control1X: 0.0000 y: 0.0524 control2X: 0.0129 y: 0.0939;
		rel_CubicBezierToX: 0.1054 y: 0.0454 control1X: 0.0260 y: 0.0303 control2X: 0.0612 y: 0.0454;
		rel_CubicBezierToX: 0.1050 y: -0.0454 control1X: 0.0443 y: 0.0000 control2X: 0.0793 y: -0.0151;
		rel_CubicBezierToX: 0.0391 y: -0.1245 control1X: 0.0261 y: -0.0306 control2X: 0.0391 y: -0.0721;
		rel_CubicBezierToX: -0.0391 y: -0.1245 control1X: 0.0000 y: -0.0527 control2X: -0.0130 y: -0.0942;
		rel_CubicBezierToX: -0.1050 y: -0.0454 control1X: -0.0257 y: -0.0303 control2X: -0.0607 y: -0.0454;
		rel_MoveToX: 0.1958 y: -0.3091;
		rel_LineToX: 0.0000 y: 0.0898;
		rel_CubicBezierToX: -0.0751 y: -0.0268 control1X: -0.0247 y: -0.0117 control2X: -0.0498 y: -0.0206;
		rel_CubicBezierToX: -0.0748 y: -0.0093 control1X: -0.0251 y: -0.0062 control2X: -0.0500 y: -0.0093;
		rel_CubicBezierToX: -0.1494 y: 0.0659 control1X: -0.0651 y: 0.0000 control2X: -0.1149 y: 0.0220;
		rel_CubicBezierToX: -0.0586 y: 0.1992 control1X: -0.0342 y: 0.0440 control2X: -0.0537 y: 0.1104;
		rel_CubicBezierToX: 0.0723 y: -0.0649 control1X: 0.0192 y: -0.0283 control2X: 0.0433 y: -0.0499;
		rel_CubicBezierToX: 0.0957 y: -0.0230 control1X: 0.0290 y: -0.0153 control2X: 0.0609 y: -0.0230;
		rel_CubicBezierToX: 0.1733 y: 0.0669 control1X: 0.0732 y: 0.0000 control2X: 0.1310 y: 0.0223;
		rel_CubicBezierToX: 0.0640 y: 0.1812 control1X: 0.0427 y: 0.0443 control2X: 0.0640 y: 0.1047;
		rel_CubicBezierToX: -0.0664 y: 0.1802 control1X: 0.0000 y: 0.0749 control2X: -0.0221 y: 0.1349;
		rel_CubicBezierToX: -0.1768 y: 0.0678 control1X: -0.0443 y: 0.0452 control2X: -0.1032 y: 0.0678;
		rel_CubicBezierToX: -0.1933 y: -0.0966 control1X: -0.0843 y: 0.0000 control2X: -0.1487 y: -0.0322;
		rel_CubicBezierToX: -0.0669 y: -0.2813 control1X: -0.0446 y: -0.0648 control2X: -0.0669 y: -0.1586;
		rel_CubicBezierToX: 0.0820 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0273 y: -0.2070;
		rel_CubicBezierToX: 0.2202 y: -0.1030 control1X: 0.0547 y: -0.0687 control2X: 0.1281 y: -0.1030;
		rel_CubicBezierToX: 0.0747 y: 0.0073 control1X: 0.0248 y: 0.0000 control2X: 0.0497 y: 0.0024;
		rel_CubicBezierToX: 0.0791 y: 0.0220 control1X: 0.0254 y: 0.0049 control2X: 0.0518 y: 0.0122! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans55
	vectorGraphicsEngine
		rel_MoveToX: 0.1198 y: 0.2272;
		rel_LineToX: 0.4687 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0420;
		rel_LineToX: -0.2646 y: 0.6870;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.2490 y: -0.6460;
		rel_LineToX: -0.3501 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans56
	vectorGraphicsEngine
		rel_MoveToX: 0.3556 y: 0.6100;
		rel_CubicBezierToX: -0.1108 y: 0.0376 control1X: -0.0468 y: 0.0000 control2X: -0.0838 y: 0.0125;
		rel_CubicBezierToX: -0.0400 y: 0.1035 control1X: -0.0267 y: 0.0251 control2X: -0.0400 y: 0.0596;
		rel_CubicBezierToX: 0.0400 y: 0.1035 control1X: 0.0000 y: 0.0439 control2X: 0.0133 y: 0.0785;
		rel_CubicBezierToX: 0.1108 y: 0.0376 control1X: 0.0270 y: 0.0251 control2X: 0.0640 y: 0.0376;
		rel_CubicBezierToX: 0.1109 y: -0.0376 control1X: 0.0469 y: 0.0000 control2X: 0.0839 y: -0.0125;
		rel_CubicBezierToX: 0.0405 y: -0.1035 control1X: 0.0270 y: -0.0254 control2X: 0.0405 y: -0.0599;
		rel_CubicBezierToX: -0.0405 y: -0.1035 control1X: 0.0000 y: -0.0439 control2X: -0.0135 y: -0.0784;
		rel_CubicBezierToX: -0.1109 y: -0.0376 control1X: -0.0267 y: -0.0251 control2X: -0.0637 y: -0.0376;
		rel_MoveToX: -0.0986 y: -0.0420;
		rel_CubicBezierToX: -0.0991 y: -0.0591 control1X: -0.0423 y: -0.0104 control2X: -0.0754 y: -0.0301;
		rel_CubicBezierToX: -0.0352 y: -0.1059 control1X: -0.0235 y: -0.0290 control2X: -0.0352 y: -0.0643;
		rel_CubicBezierToX: 0.0620 y: -0.1382 control1X: 0.0000 y: -0.0583 control2X: 0.0207 y: -0.1044;
		rel_CubicBezierToX: 0.1709 y: -0.0508 control1X: 0.0417 y: -0.0339 control2X: 0.0987 y: -0.0508;
		rel_CubicBezierToX: 0.1709 y: 0.0508 control1X: 0.0726 y: 0.0000 control2X: 0.1296 y: 0.0169;
		rel_CubicBezierToX: 0.0620 y: 0.1382 control1X: 0.0414 y: 0.0338 control2X: 0.0620 y: 0.0799;
		rel_CubicBezierToX: -0.0356 y: 0.1059 control1X: 0.0000 y: 0.0416 control2X: -0.0118 y: 0.0769;
		rel_CubicBezierToX: -0.0981 y: 0.0591 control1X: -0.0234 y: 0.0290 control2X: -0.0561 y: 0.0487;
		rel_CubicBezierToX: 0.1108 y: 0.0649 control1X: 0.0475 y: 0.0111 control2X: 0.0844 y: 0.0327;
		rel_CubicBezierToX: 0.0400 y: 0.1182 control1X: 0.0267 y: 0.0323 control2X: 0.0400 y: 0.0717;
		rel_CubicBezierToX: -0.0649 y: 0.1626 control1X: 0.0000 y: 0.0706 control2X: -0.0216 y: 0.1248;
		rel_CubicBezierToX: -0.1851 y: 0.0566 control1X: -0.0430 y: 0.0378 control2X: -0.1047 y: 0.0566;
		rel_CubicBezierToX: -0.1855 y: -0.0566 control1X: -0.0804 y: 0.0000 control2X: -0.1422 y: -0.0188;
		rel_CubicBezierToX: -0.0645 y: -0.1626 control1X: -0.0430 y: -0.0378 control2X: -0.0645 y: -0.0920;
		rel_CubicBezierToX: 0.0401 y: -0.1182 control1X: 0.0000 y: -0.0465 control2X: 0.0134 y: -0.0859;
		rel_CubicBezierToX: 0.1113 y: -0.0649 control1X: 0.0267 y: -0.0322 control2X: 0.0638 y: -0.0538;
		rel_MoveToX: -0.0361 y: -0.1558;
		rel_CubicBezierToX: 0.0351 y: 0.0884 control1X: 0.0000 y: 0.0378 control2X: 0.0117 y: 0.0673;
		rel_CubicBezierToX: 0.0996 y: 0.0318 control1X: 0.0238 y: 0.0212 control2X: 0.0570 y: 0.0318;
		rel_CubicBezierToX: 0.0992 y: -0.0318 control1X: 0.0424 y: 0.0000 control2X: 0.0754 y: -0.0106;
		rel_CubicBezierToX: 0.0361 y: -0.0884 control1X: 0.0240 y: -0.0211 control2X: 0.0361 y: -0.0506;
		rel_CubicBezierToX: -0.0361 y: -0.0883 control1X: 0.0000 y: -0.0377 control2X: -0.0121 y: -0.0672;
		rel_CubicBezierToX: -0.0992 y: -0.0318 control1X: -0.0238 y: -0.0212 control2X: -0.0568 y: -0.0318;
		rel_CubicBezierToX: -0.0996 y: 0.0318 control1X: -0.0426 y: 0.0000 control2X: -0.0758 y: 0.0106;
		rel_CubicBezierToX: -0.0351 y: 0.0883 control1X: -0.0234 y: 0.0211 control2X: -0.0351 y: 0.0506! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans57
	vectorGraphicsEngine
		rel_MoveToX: 0.1476 y: 0.9410;
		rel_LineToX: 0.0000 y: -0.0898;
		rel_CubicBezierToX: 0.0752 y: 0.0269 control1X: 0.0248 y: 0.0117 control2X: 0.0498 y: 0.0207;
		rel_CubicBezierToX: 0.0747 y: 0.0092 control1X: 0.0254 y: 0.0061 control2X: 0.0503 y: 0.0092;
		rel_CubicBezierToX: 0.1490 y: -0.0654 control1X: 0.0651 y: 0.0000 control2X: 0.1148 y: -0.0218;
		rel_CubicBezierToX: 0.0590 y: -0.1997 control1X: 0.0345 y: -0.0439 control2X: 0.0542 y: -0.1105;
		rel_CubicBezierToX: -0.0717 y: 0.0645 control1X: -0.0188 y: 0.0280 control2X: -0.0428 y: 0.0495;
		rel_CubicBezierToX: -0.0962 y: 0.0224 control1X: -0.0290 y: 0.0149 control2X: -0.0611 y: 0.0224;
		rel_CubicBezierToX: -0.1734 y: -0.0659 control1X: -0.0729 y: 0.0000 control2X: -0.1307 y: -0.0220;
		rel_CubicBezierToX: -0.0634 y: -0.1812 control1X: -0.0423 y: -0.0443 control2X: -0.0634 y: -0.1047;
		rel_CubicBezierToX: 0.0664 y: -0.1801 control1X: 0.0000 y: -0.0748 control2X: 0.0221 y: -0.1349;
		rel_CubicBezierToX: 0.1767 y: -0.0679 control1X: 0.0442 y: -0.0453 control2X: 0.1032 y: -0.0679;
		rel_CubicBezierToX: 0.1929 y: 0.0972 control1X: 0.0843 y: 0.0000 control2X: 0.1486 y: 0.0324;
		rel_CubicBezierToX: 0.0669 y: 0.2812 control1X: 0.0446 y: 0.0644 control2X: 0.0669 y: 0.1582;
		rel_CubicBezierToX: -0.0820 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0274 y: 0.2067;
		rel_CubicBezierToX: -0.2198 y: 0.1025 control1X: -0.0544 y: 0.0684 control2X: -0.1277 y: 0.1025;
		rel_CubicBezierToX: -0.0752 y: -0.0073 control1X: -0.0247 y: 0.0000 control2X: -0.0498 y: -0.0024;
		rel_CubicBezierToX: -0.0791 y: -0.0220 control1X: -0.0254 y: -0.0049 control2X: -0.0517 y: -0.0122;
		rel_MoveToX: 0.1963 y: -0.3090;
		rel_CubicBezierToX: 0.1050 y: -0.0454 control1X: 0.0443 y: 0.0000 control2X: 0.0793 y: -0.0152;
		rel_CubicBezierToX: 0.0391 y: -0.1246 control1X: 0.0260 y: -0.0303 control2X: 0.0391 y: -0.0718;
		rel_CubicBezierToX: -0.0391 y: -0.1240 control1X: 0.0000 y: -0.0524 control2X: -0.0131 y: -0.0937;
		rel_CubicBezierToX: -0.1050 y: -0.0459 control1X: -0.0257 y: -0.0306 control2X: -0.0607 y: -0.0459;
		rel_CubicBezierToX: -0.1054 y: 0.0459 control1X: -0.0443 y: 0.0000 control2X: -0.0794 y: 0.0153;
		rel_CubicBezierToX: -0.0386 y: 0.1240 control1X: -0.0258 y: 0.0303 control2X: -0.0386 y: 0.0716;
		rel_CubicBezierToX: 0.0386 y: 0.1246 control1X: 0.0000 y: 0.0528 control2X: 0.0128 y: 0.0943;
		rel_CubicBezierToX: 0.1054 y: 0.0454 control1X: 0.0260 y: 0.0302 control2X: 0.0611 y: 0.0454! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans58
	vectorGraphicsEngine
		rel_MoveToX: 0.1550 y: 0.8322;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_MoveToX: 0.0000 y: -0.3931;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans59
	vectorGraphicsEngine
		rel_MoveToX: 0.1550 y: 0.4391;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_MoveToX: 0.0000 y: 0.3931;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0839;
		rel_LineToX: -0.0801 y: 0.1563;
		rel_LineToX: -0.0630 y: 0.0000;
		rel_LineToX: 0.0401 y: -0.1563;
		rel_LineToX: 0.0000 y: -0.0839! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans60
	vectorGraphicsEngine
		rel_MoveToX: 0.7697 y: 0.4640;
		rel_LineToX: -0.5039 y: 0.1792;
		rel_LineToX: 0.5039 y: 0.1782;
		rel_LineToX: 0.0000 y: 0.0889;
		rel_LineToX: -0.6260 y: -0.2271;
		rel_LineToX: 0.0000 y: -0.0810;
		rel_LineToX: 0.6260 y: -0.2271;
		rel_LineToX: 0.0000 y: 0.0889! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans61
	vectorGraphicsEngine
		rel_MoveToX: 0.1437 y: 0.5021;
		rel_LineToX: 0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0820;
		rel_LineToX: -0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0820;
		rel_MoveToX: 0.0000 y: 0.1992;
		rel_LineToX: 0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.6260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans62
	vectorGraphicsEngine
		rel_MoveToX: 0.1437 y: 0.4640;
		rel_LineToX: 0.0000 y: -0.0889;
		rel_LineToX: 0.6260 y: 0.2271;
		rel_LineToX: 0.0000 y: 0.0810;
		rel_LineToX: -0.6260 y: 0.2271;
		rel_LineToX: 0.0000 y: -0.0889;
		rel_LineToX: 0.5030 y: -0.1782;
		rel_LineToX: -0.5030 y: -0.1792! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans63
	vectorGraphicsEngine
		rel_MoveToX: 0.2287 y: 0.8322;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.1240;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.1240;
		rel_MoveToX: 0.0962 y: -0.0718;
		rel_LineToX: -0.0933 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_CubicBezierToX: 0.0137 y: -0.0811 control1X: 0.0000 y: -0.0329 control2X: 0.0046 y: -0.0599;
		rel_CubicBezierToX: 0.0576 y: -0.0737 control1X: 0.0091 y: -0.0211 control2X: 0.0283 y: -0.0457;
		rel_LineToX: 0.0439 y: -0.0435;
		rel_CubicBezierToX: 0.0401 y: -0.0488 control1X: 0.0186 y: -0.0172 control2X: 0.0319 y: -0.0335;
		rel_CubicBezierToX: 0.0127 y: -0.0469 control1X: 0.0085 y: -0.0153 control2X: 0.0127 y: -0.0309;
		rel_CubicBezierToX: -0.0322 y: -0.0703 control1X: 0.0000 y: -0.0289 control2X: -0.0108 y: -0.0524;
		rel_CubicBezierToX: -0.0845 y: -0.0268 control1X: -0.0212 y: -0.0179 control2X: -0.0494 y: -0.0268;
		rel_CubicBezierToX: -0.0825 y: 0.0171 control1X: -0.0257 y: 0.0000 control2X: -0.0532 y: 0.0057;
		rel_CubicBezierToX: -0.0909 y: 0.0498 control1X: -0.0290 y: 0.0114 control2X: -0.0593 y: 0.0280;
		rel_LineToX: 0.0000 y: -0.0918;
		rel_CubicBezierToX: 0.0928 y: -0.0415 control1X: 0.0306 y: -0.0186 control2X: 0.0616 y: -0.0324;
		rel_CubicBezierToX: 0.0977 y: -0.0137 control1X: 0.0316 y: -0.0091 control2X: 0.0641 y: -0.0137;
		rel_CubicBezierToX: 0.1440 y: 0.0474 control1X: 0.0599 y: 0.0000 control2X: 0.1079 y: 0.0158;
		rel_CubicBezierToX: 0.0547 y: 0.1250 control1X: 0.0365 y: 0.0315 control2X: 0.0547 y: 0.0732;
		rel_CubicBezierToX: -0.0176 y: 0.0708 control1X: 0.0000 y: 0.0247 control2X: -0.0059 y: 0.0483;
		rel_CubicBezierToX: -0.0615 y: 0.0752 control1X: -0.0117 y: 0.0221 control2X: -0.0322 y: 0.0472;
		rel_LineToX: -0.0430 y: 0.0419;
		rel_CubicBezierToX: -0.0327 y: 0.0362 control1X: -0.0153 y: 0.0153 control2X: -0.0262 y: 0.0274;
		rel_CubicBezierToX: -0.0132 y: 0.0249 control1X: -0.0062 y: 0.0084 control2X: -0.0106 y: 0.0167;
		rel_CubicBezierToX: -0.0044 y: 0.0249 control1X: -0.0019 y: 0.0068 control2X: -0.0034 y: 0.0151;
		rel_CubicBezierToX: -0.0014 y: 0.0400 control1X: -0.0009 y: 0.0097 control2X: -0.0014 y: 0.0231;
		rel_LineToX: 0.0000 y: 0.0601! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans64
	vectorGraphicsEngine
		rel_MoveToX: 0.4098 y: 0.6940;
		rel_CubicBezierToX: 0.0347 y: 0.1098 control1X: 0.0000 y: 0.0465 control2X: 0.0116 y: 0.0831;
		rel_CubicBezierToX: 0.0952 y: 0.0396 control1X: 0.0231 y: 0.0264 control2X: 0.0549 y: 0.0396;
		rel_CubicBezierToX: 0.0943 y: -0.0401 control1X: 0.0401 y: 0.0000 control2X: 0.0715 y: -0.0134;
		rel_CubicBezierToX: 0.0346 y: -0.1093 control1X: 0.0231 y: -0.0266 control2X: 0.0346 y: -0.0631;
		rel_CubicBezierToX: -0.0351 y: -0.1084 control1X: 0.0000 y: -0.0456 control2X: -0.0117 y: -0.0817;
		rel_CubicBezierToX: -0.0948 y: -0.0406 control1X: -0.0235 y: -0.0270 control2X: -0.0550 y: -0.0406;
		rel_CubicBezierToX: -0.0942 y: 0.0401 control1X: -0.0393 y: 0.0000 control2X: -0.0708 y: 0.0134;
		rel_CubicBezierToX: -0.0347 y: 0.1089 control1X: -0.0231 y: 0.0267 control2X: -0.0347 y: 0.0630;
		rel_MoveToX: 0.2662 y: 0.1460;
		rel_CubicBezierToX: -0.0674 y: 0.0556 control1X: -0.0196 y: 0.0250 control2X: -0.0420 y: 0.0436;
		rel_CubicBezierToX: -0.0879 y: 0.0176 control1X: -0.0251 y: 0.0118 control2X: -0.0544 y: 0.0176;
		rel_CubicBezierToX: -0.1367 y: -0.0605 control1X: -0.0560 y: 0.0000 control2X: -0.1016 y: -0.0202;
		rel_CubicBezierToX: -0.0523 y: -0.1587 control1X: -0.0349 y: -0.0407 control2X: -0.0523 y: -0.0936;
		rel_CubicBezierToX: 0.0527 y: -0.1587 control1X: 0.0000 y: -0.0651 control2X: 0.0176 y: -0.1180;
		rel_CubicBezierToX: 0.1363 y: -0.0611 control1X: 0.0352 y: -0.0407 control2X: 0.0806 y: -0.0611;
		rel_CubicBezierToX: 0.0884 y: 0.0186 control1X: 0.0335 y: 0.0000 control2X: 0.0630 y: 0.0062;
		rel_CubicBezierToX: 0.0669 y: 0.0552 control1X: 0.0253 y: 0.0120 control2X: 0.0476 y: 0.0304;
		rel_LineToX: 0.0000 y: -0.0640;
		rel_LineToX: 0.0698 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3594;
		rel_CubicBezierToX: 0.1113 y: -0.0650 control1X: 0.0475 y: -0.0072 control2X: 0.0846 y: -0.0288;
		rel_CubicBezierToX: 0.0405 y: -0.1411 control1X: 0.0270 y: -0.0364 control2X: 0.0405 y: -0.0834;
		rel_CubicBezierToX: -0.0156 y: -0.0981 control1X: 0.0000 y: -0.0348 control2X: -0.0052 y: -0.0675;
		rel_CubicBezierToX: -0.0464 y: -0.0850 control1X: -0.0101 y: -0.0306 control2X: -0.0255 y: -0.0589;
		rel_CubicBezierToX: -0.1240 y: -0.0976 control1X: -0.0338 y: -0.0426 control2X: -0.0752 y: -0.0752;
		rel_CubicBezierToX: -0.1587 y: -0.0342 control1X: -0.0485 y: -0.0228 control2X: -0.1014 y: -0.0342;
		rel_CubicBezierToX: -0.1152 y: 0.0161 control1X: -0.0400 y: 0.0000 control2X: -0.0784 y: 0.0054;
		rel_CubicBezierToX: -0.1021 y: 0.0469 control1X: -0.0368 y: 0.0104 control2X: -0.0708 y: 0.0260;
		rel_CubicBezierToX: -0.1201 y: 0.1308 control1X: -0.0511 y: 0.0332 control2X: -0.0911 y: 0.0768;
		rel_CubicBezierToX: -0.0430 y: 0.1748 control1X: -0.0286 y: 0.0538 control2X: -0.0430 y: 0.1120;
		rel_CubicBezierToX: 0.0279 y: 0.1456 control1X: 0.0000 y: 0.0518 control2X: 0.0093 y: 0.1003;
		rel_CubicBezierToX: 0.0815 y: 0.1196 control1X: 0.0188 y: 0.0452 control2X: 0.0460 y: 0.0851;
		rel_CubicBezierToX: 0.1187 y: 0.0771 control1X: 0.0342 y: 0.0338 control2X: 0.0737 y: 0.0596;
		rel_CubicBezierToX: 0.1440 y: 0.0269 control1X: 0.0449 y: 0.0179 control2X: 0.0929 y: 0.0269;
		rel_CubicBezierToX: 0.1235 y: -0.0215 control1X: 0.0420 y: 0.0000 control2X: 0.0832 y: -0.0072;
		rel_CubicBezierToX: 0.1119 y: -0.0605 control1X: 0.0407 y: -0.0140 control2X: 0.0780 y: -0.0342;
		rel_LineToX: 0.0439 y: 0.0542;
		rel_CubicBezierToX: -0.1333 y: 0.0722 control1X: -0.0407 y: 0.0315 control2X: -0.0851 y: 0.0556;
		rel_CubicBezierToX: -0.1460 y: 0.0254 control1X: -0.0479 y: 0.0169 control2X: -0.0965 y: 0.0254;
		rel_CubicBezierToX: -0.1704 y: -0.0322 control1X: -0.0602 y: 0.0000 control2X: -0.1170 y: -0.0107;
		rel_CubicBezierToX: -0.1426 y: -0.0928 control1X: -0.0534 y: -0.0212 control2X: -0.1009 y: -0.0521;
		rel_CubicBezierToX: -0.0952 y: -0.1411 control1X: -0.0417 y: -0.0407 control2X: -0.0734 y: -0.0877;
		rel_CubicBezierToX: -0.0327 y: -0.1729 control1X: -0.0218 y: -0.0537 control2X: -0.0327 y: -0.1113;
		rel_CubicBezierToX: 0.0332 y: -0.1694 control1X: 0.0000 y: -0.0592 control2X: 0.0111 y: -0.1157;
		rel_CubicBezierToX: 0.0947 y: -0.1416 control1X: 0.0221 y: -0.0537 control2X: 0.0537 y: -0.1009;
		rel_CubicBezierToX: 0.1455 y: -0.0947 control1X: 0.0420 y: -0.0413 control2X: 0.0905 y: -0.0729;
		rel_CubicBezierToX: 0.1748 y: -0.0332 control1X: 0.0550 y: -0.0222 control2X: 0.1133 y: -0.0332;
		rel_CubicBezierToX: 0.1919 y: 0.0425 control1X: 0.0690 y: 0.0000 control2X: 0.1330 y: 0.0141;
		rel_CubicBezierToX: 0.1489 y: 0.1206 control1X: 0.0593 y: 0.0283 control2X: 0.1089 y: 0.0685;
		rel_CubicBezierToX: 0.0557 y: 0.1040 control1X: 0.0245 y: 0.0319 control2X: 0.0430 y: 0.0665;
		rel_CubicBezierToX: 0.0195 y: 0.1162 control1X: 0.0130 y: 0.0374 control2X: 0.0195 y: 0.0761;
		rel_CubicBezierToX: -0.0776 y: 0.2026 control1X: 0.0000 y: 0.0856 control2X: -0.0258 y: 0.1531;
		rel_CubicBezierToX: -0.2143 y: 0.0772 control1X: -0.0518 y: 0.0495 control2X: -0.1232 y: 0.0752;
		rel_LineToX: 0.0000 y: -0.0752! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans65
	vectorGraphicsEngine
		rel_MoveToX: 0.3796 y: 0.3243;
		rel_LineToX: -0.1338 y: 0.3628;
		rel_LineToX: 0.2680 y: 0.0000;
		rel_LineToX: -0.1342 y: -0.3628;
		rel_MoveToX: -0.0557 y: -0.0971;
		rel_LineToX: 0.1118 y: 0.0000;
		rel_LineToX: 0.2778 y: 0.7290;
		rel_LineToX: -0.1025 y: 0.0000;
		rel_LineToX: -0.0664 y: -0.1870;
		rel_LineToX: -0.3286 y: 0.0000;
		rel_LineToX: -0.0664 y: 0.1870;
		rel_LineToX: -0.1040 y: 0.0000;
		rel_LineToX: 0.2783 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans66
	vectorGraphicsEngine
		rel_MoveToX: 0.2345 y: 0.6080;
		rel_LineToX: 0.0000 y: 0.2671;
		rel_LineToX: 0.1582 y: 0.0000;
		rel_CubicBezierToX: 0.1177 y: -0.0327 control1X: 0.0531 y: 0.0000 control2X: 0.0923 y: -0.0109;
		rel_CubicBezierToX: 0.0386 y: -0.1011 control1X: 0.0257 y: -0.0221 control2X: 0.0386 y: -0.0558;
		rel_CubicBezierToX: -0.0386 y: -0.1005 control1X: 0.0000 y: -0.0455 control2X: -0.0129 y: -0.0791;
		rel_CubicBezierToX: -0.1177 y: -0.0328 control1X: -0.0254 y: -0.0219 control2X: -0.0646 y: -0.0328;
		rel_LineToX: -0.1582 y: 0.0000;
		rel_MoveToX: 0.0000 y: -0.2998;
		rel_LineToX: 0.0000 y: 0.2198;
		rel_LineToX: 0.1460 y: 0.0000;
		rel_CubicBezierToX: 0.1075 y: -0.0269 control1X: 0.0482 y: 0.0000 control2X: 0.0840 y: -0.0090;
		rel_CubicBezierToX: 0.0356 y: -0.0830 control1X: 0.0237 y: -0.0182 control2X: 0.0356 y: -0.0459;
		rel_CubicBezierToX: -0.0356 y: -0.0825 control1X: 0.0000 y: -0.0368 control2X: -0.0119 y: -0.0643;
		rel_CubicBezierToX: -0.1075 y: -0.0274 control1X: -0.0235 y: -0.0183 control2X: -0.0593 y: -0.0274;
		rel_LineToX: -0.1460 y: 0.0000;
		rel_MoveToX: -0.0986 y: -0.0810;
		rel_LineToX: 0.2520 y: 0.0000;
		rel_CubicBezierToX: 0.1738 y: 0.0469 control1X: 0.0752 y: 0.0000 control2X: 0.1331 y: 0.0156;
		rel_CubicBezierToX: 0.0610 y: 0.1333 control1X: 0.0407 y: 0.0312 control2X: 0.0610 y: 0.0756;
		rel_CubicBezierToX: -0.0312 y: 0.1064 control1X: 0.0000 y: 0.0446 control2X: -0.0104 y: 0.0800;
		rel_CubicBezierToX: -0.0918 y: 0.0493 control1X: -0.0209 y: 0.0264 control2X: -0.0515 y: 0.0428;
		rel_CubicBezierToX: 0.1128 y: 0.0654 control1X: 0.0485 y: 0.0104 control2X: 0.0861 y: 0.0322;
		rel_CubicBezierToX: 0.0405 y: 0.1236 control1X: 0.0270 y: 0.0329 control2X: 0.0405 y: 0.0741;
		rel_CubicBezierToX: -0.0664 y: 0.1509 control1X: 0.0000 y: 0.0651 control2X: -0.0221 y: 0.1154;
		rel_CubicBezierToX: -0.1890 y: 0.0532 control1X: -0.0443 y: 0.0354 control2X: -0.1073 y: 0.0532;
		rel_LineToX: -0.2617 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans67
	vectorGraphicsEngine
		rel_MoveToX: 0.6818 y: 0.2833;
		rel_LineToX: 0.0000 y: 0.1040;
		rel_CubicBezierToX: -0.1064 y: -0.0693 control1X: -0.0332 y: -0.0309 control2X: -0.0687 y: -0.0540;
		rel_CubicBezierToX: -0.1197 y: -0.0230 control1X: -0.0375 y: -0.0153 control2X: -0.0773 y: -0.0230;
		rel_CubicBezierToX: -0.1914 y: 0.0767 control1X: -0.0833 y: 0.0000 control2X: -0.1471 y: 0.0256;
		rel_CubicBezierToX: -0.0664 y: 0.2207 control1X: -0.0442 y: 0.0508 control2X: -0.0664 y: 0.1244;
		rel_CubicBezierToX: 0.0664 y: 0.2207 control1X: 0.0000 y: 0.0960 control2X: 0.0222 y: 0.1696;
		rel_CubicBezierToX: 0.1914 y: 0.0762 control1X: 0.0443 y: 0.0508 control2X: 0.1081 y: 0.0762;
		rel_CubicBezierToX: 0.1197 y: -0.0230 control1X: 0.0424 y: 0.0000 control2X: 0.0822 y: -0.0077;
		rel_CubicBezierToX: 0.1064 y: -0.0693 control1X: 0.0377 y: -0.0153 control2X: 0.0732 y: -0.0384;
		rel_LineToX: 0.0000 y: 0.1030;
		rel_CubicBezierToX: -0.1099 y: 0.0528 control1X: -0.0345 y: 0.0235 control2X: -0.0711 y: 0.0410;
		rel_CubicBezierToX: -0.1220 y: 0.0175 control1X: -0.0384 y: 0.0117 control2X: -0.0791 y: 0.0175;
		rel_CubicBezierToX: -0.2608 y: -0.1010 control1X: -0.1104 y: 0.0000 control2X: -0.1973 y: -0.0336;
		rel_CubicBezierToX: -0.0952 y: -0.2769 control1X: -0.0634 y: -0.0677 control2X: -0.0952 y: -0.1600;
		rel_CubicBezierToX: 0.0952 y: -0.2768 control1X: 0.0000 y: -0.1172 control2X: 0.0318 y: -0.2095;
		rel_CubicBezierToX: 0.2608 y: -0.1016 control1X: 0.0635 y: -0.0678 control2X: 0.1504 y: -0.1016;
		rel_CubicBezierToX: 0.1230 y: 0.0176 control1X: 0.0436 y: 0.0000 control2X: 0.0846 y: 0.0059;
		rel_CubicBezierToX: 0.1089 y: 0.0517 control1X: 0.0388 y: 0.0114 control2X: 0.0751 y: 0.0286! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans68
	vectorGraphicsEngine
		rel_MoveToX: 0.2345 y: 0.3082;
		rel_LineToX: 0.0000 y: 0.5669;
		rel_LineToX: 0.1192 y: 0.0000;
		rel_CubicBezierToX: 0.2207 y: -0.0683 control1X: 0.1006 y: 0.0000 control2X: 0.1741 y: -0.0228;
		rel_CubicBezierToX: 0.0703 y: -0.2159 control1X: 0.0469 y: -0.0456 control2X: 0.0703 y: -0.1175;
		rel_CubicBezierToX: -0.0703 y: -0.2143 control1X: 0.0000 y: -0.0976 control2X: -0.0234 y: -0.1691;
		rel_CubicBezierToX: -0.2207 y: -0.0684 control1X: -0.0466 y: -0.0456 control2X: -0.1201 y: -0.0684;
		rel_LineToX: -0.1192 y: 0.0000;
		rel_MoveToX: -0.0986 y: -0.0810;
		rel_LineToX: 0.2026 y: 0.0000;
		rel_CubicBezierToX: 0.3111 y: 0.0884 control1X: 0.1413 y: 0.0000 control2X: 0.2450 y: 0.0294;
		rel_CubicBezierToX: 0.0991 y: 0.2753 control1X: 0.0661 y: 0.0586 control2X: 0.0991 y: 0.1503;
		rel_CubicBezierToX: -0.0996 y: 0.2769 control1X: 0.0000 y: 0.1257 control2X: -0.0332 y: 0.2180;
		rel_CubicBezierToX: -0.3106 y: 0.0884 control1X: -0.0664 y: 0.0589 control2X: -0.1699 y: 0.0884;
		rel_LineToX: -0.2026 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans69
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4609 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3623 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2158;
		rel_LineToX: 0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3472 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2642;
		rel_LineToX: 0.3711 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4697 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans70
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.4190 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.3204 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2148;
		rel_LineToX: 0.2891 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.2891 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3482;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans71
	vectorGraphicsEngine
		rel_MoveToX: 0.6330 y: 0.8522;
		rel_LineToX: 0.0000 y: -0.1958;
		rel_LineToX: -0.1612 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0811;
		rel_LineToX: 0.2588 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3130;
		rel_CubicBezierToX: -0.1259 y: 0.0615 control1X: -0.0380 y: 0.0270 control2X: -0.0800 y: 0.0475;
		rel_CubicBezierToX: -0.1470 y: 0.0205 control1X: -0.0459 y: 0.0137 control2X: -0.0949 y: 0.0205;
		rel_CubicBezierToX: -0.2676 y: -0.0996 control1X: -0.1139 y: 0.0000 control2X: -0.2031 y: -0.0332;
		rel_CubicBezierToX: -0.0962 y: -0.2783 control1X: -0.0641 y: -0.0667 control2X: -0.0962 y: -0.1595;
		rel_CubicBezierToX: 0.0962 y: -0.2783 control1X: 0.0000 y: -0.1191 control2X: 0.0321 y: -0.2119;
		rel_CubicBezierToX: 0.2676 y: -0.1001 control1X: 0.0645 y: -0.0667 control2X: 0.1537 y: -0.1001;
		rel_CubicBezierToX: 0.1352 y: 0.0176 control1X: 0.0475 y: 0.0000 control2X: 0.0926 y: 0.0059;
		rel_CubicBezierToX: 0.1187 y: 0.0517 control1X: 0.0430 y: 0.0117 control2X: 0.0826 y: 0.0289;
		rel_LineToX: 0.0000 y: 0.1050;
		rel_CubicBezierToX: -0.1162 y: -0.0698 control1X: -0.0365 y: -0.0309 control2X: -0.0752 y: -0.0542;
		rel_CubicBezierToX: -0.1294 y: -0.0235 control1X: -0.0410 y: -0.0156 control2X: -0.0842 y: -0.0235;
		rel_CubicBezierToX: -0.2012 y: 0.0748 control1X: -0.0892 y: 0.0000 control2X: -0.1563 y: 0.0250;
		rel_CubicBezierToX: -0.0669 y: 0.2226 control1X: -0.0446 y: 0.0498 control2X: -0.0669 y: 0.1240;
		rel_CubicBezierToX: 0.0669 y: 0.2222 control1X: 0.0000 y: 0.0983 control2X: 0.0223 y: 0.1724;
		rel_CubicBezierToX: 0.2012 y: 0.0747 control1X: 0.0449 y: 0.0498 control2X: 0.1120 y: 0.0747;
		rel_CubicBezierToX: 0.0933 y: -0.0088 control1X: 0.0348 y: 0.0000 control2X: 0.0659 y: -0.0029;
		rel_CubicBezierToX: 0.0737 y: -0.0283 control1X: 0.0273 y: -0.0062 control2X: 0.0519 y: -0.0156! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans72
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2988;
		rel_LineToX: 0.3584 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.2988;
		rel_LineToX: 0.0987 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0987 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3472;
		rel_LineToX: -0.3584 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3472;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans73
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans74
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.6782;
		rel_CubicBezierToX: -0.0502 y: 0.1914 control1X: 0.0000 y: 0.0879 control2X: -0.0167 y: 0.1517;
		rel_CubicBezierToX: -0.1607 y: 0.0596 control1X: -0.0333 y: 0.0397 control2X: -0.0868 y: 0.0596;
		rel_LineToX: -0.0376 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_LineToX: 0.0308 y: 0.0000;
		rel_CubicBezierToX: 0.0923 y: -0.0367 control1X: 0.0436 y: 0.0000 control2X: 0.0744 y: -0.0122;
		rel_CubicBezierToX: 0.0268 y: -0.1313 control1X: 0.0179 y: -0.0244 control2X: 0.0268 y: -0.0681;
		rel_LineToX: 0.0000 y: -0.6782! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans75
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3081;
		rel_LineToX: 0.3272 y: -0.3081;
		rel_LineToX: 0.1269 y: 0.0000;
		rel_LineToX: -0.3618 y: 0.3398;
		rel_LineToX: 0.3877 y: 0.3892;
		rel_LineToX: -0.1299 y: 0.0000;
		rel_LineToX: -0.3501 y: -0.3511;
		rel_LineToX: 0.0000 y: 0.3511;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans76
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.6460;
		rel_LineToX: 0.3550 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.4536 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans77
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.1470 y: 0.0000;
		rel_LineToX: 0.1860 y: 0.4961;
		rel_LineToX: 0.1870 y: -0.4961;
		rel_LineToX: 0.1470 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.0962 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.6402;
		rel_LineToX: -0.1880 y: 0.5000;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: -0.1880 y: -0.5000;
		rel_LineToX: 0.0000 y: 0.6402;
		rel_LineToX: -0.0957 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans78
	vectorGraphicsEngine
		rel_MoveToX: 0.1359 y: 0.2272;
		rel_LineToX: 0.1328 y: 0.0000;
		rel_LineToX: 0.3233 y: 0.6098;
		rel_LineToX: 0.0000 y: -0.6098;
		rel_LineToX: 0.0957 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7290;
		rel_LineToX: -0.1328 y: 0.0000;
		rel_LineToX: -0.3233 y: -0.6099;
		rel_LineToX: 0.0000 y: 0.6099;
		rel_LineToX: -0.0957 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans79
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.0000 y: -0.0801;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0918 y: 0.2754 control1X: 0.0000 y: 0.1149 control2X: -0.0306 y: 0.2067;
		rel_CubicBezierToX: -0.2451 y: 0.1025 control1X: -0.0612 y: 0.0684 control2X: -0.1429 y: 0.1025;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0684 control2X: -0.0918 y: -0.1602;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans80
	vectorGraphicsEngine
		rel_MoveToX: 0.2345 y: 0.3082;
		rel_LineToX: 0.0000 y: 0.2740;
		rel_LineToX: 0.1241 y: 0.0000;
		rel_CubicBezierToX: 0.1064 y: -0.0357 control1X: 0.0459 y: 0.0000 control2X: 0.0813 y: -0.0119;
		rel_CubicBezierToX: 0.0376 y: -0.1015 control1X: 0.0251 y: -0.0237 control2X: 0.0376 y: -0.0576;
		rel_CubicBezierToX: -0.0376 y: -0.1011 control1X: 0.0000 y: -0.0437 control2X: -0.0125 y: -0.0774;
		rel_CubicBezierToX: -0.1064 y: -0.0357 control1X: -0.0251 y: -0.0238 control2X: -0.0605 y: -0.0357;
		rel_LineToX: -0.1241 y: 0.0000;
		rel_MoveToX: -0.0986 y: -0.0810;
		rel_LineToX: 0.2227 y: 0.0000;
		rel_CubicBezierToX: 0.1850 y: 0.0556 control1X: 0.0817 y: 0.0000 control2X: 0.1434 y: 0.0185;
		rel_CubicBezierToX: 0.0630 y: 0.1622 control1X: 0.0420 y: 0.0368 control2X: 0.0630 y: 0.0909;
		rel_CubicBezierToX: -0.0630 y: 0.1630 control1X: 0.0000 y: 0.0719 control2X: -0.0210 y: 0.1263;
		rel_CubicBezierToX: -0.1850 y: 0.0552 control1X: -0.0416 y: 0.0368 control2X: -0.1033 y: 0.0552;
		rel_LineToX: -0.1241 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2930;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans81
	vectorGraphicsEngine
		rel_MoveToX: 0.4318 y: 0.2941;
		rel_CubicBezierToX: -0.1709 y: 0.0801 control1X: -0.0716 y: 0.0000 control2X: -0.1286 y: 0.0267;
		rel_CubicBezierToX: -0.0630 y: 0.2182 control1X: -0.0420 y: 0.0533 control2X: -0.0630 y: 0.1261;
		rel_CubicBezierToX: 0.0630 y: 0.2178 control1X: 0.0000 y: 0.0918 control2X: 0.0210 y: 0.1644;
		rel_CubicBezierToX: 0.1709 y: 0.0801 control1X: 0.0423 y: 0.0534 control2X: 0.0993 y: 0.0801;
		rel_CubicBezierToX: 0.1699 y: -0.0801 control1X: 0.0716 y: 0.0000 control2X: 0.1283 y: -0.0267;
		rel_CubicBezierToX: 0.0630 y: -0.2178 control1X: 0.0420 y: -0.0534 control2X: 0.0630 y: -0.1260;
		rel_CubicBezierToX: -0.0630 y: -0.2182 control1X: 0.0000 y: -0.0921 control2X: -0.0210 y: -0.1649;
		rel_CubicBezierToX: -0.1699 y: -0.0801 control1X: -0.0416 y: -0.0534 control2X: -0.0983 y: -0.0801;
		rel_MoveToX: 0.1382 y: 0.6489;
		rel_LineToX: 0.1299 y: 0.1421;
		rel_LineToX: -0.1192 y: 0.0000;
		rel_LineToX: -0.1079 y: -0.1167;
		rel_CubicBezierToX: -0.0249 y: 0.0015 control1X: -0.0107 y: 0.0006 control2X: -0.0190 y: 0.0011;
		rel_CubicBezierToX: -0.0161 y: 0.0004 control1X: -0.0055 y: 0.0003 control2X: -0.0109 y: 0.0004;
		rel_CubicBezierToX: -0.2461 y: -0.1025 control1X: -0.1025 y: 0.0000 control2X: -0.1846 y: -0.0341;
		rel_CubicBezierToX: -0.0918 y: -0.2754 control1X: -0.0612 y: -0.0687 control2X: -0.0918 y: -0.1605;
		rel_CubicBezierToX: 0.0918 y: -0.2754 control1X: 0.0000 y: -0.1152 control2X: 0.0306 y: -0.2070;
		rel_CubicBezierToX: 0.2461 y: -0.1030 control1X: 0.0615 y: -0.0687 control2X: 0.1436 y: -0.1030;
		rel_CubicBezierToX: 0.2451 y: 0.1030 control1X: 0.1022 y: 0.0000 control2X: 0.1839 y: 0.0343;
		rel_CubicBezierToX: 0.0918 y: 0.2754 control1X: 0.0612 y: 0.0684 control2X: 0.0918 y: 0.1602;
		rel_CubicBezierToX: -0.0512 y: 0.2173 control1X: 0.0000 y: 0.0846 control2X: -0.0171 y: 0.1571;
		rel_CubicBezierToX: -0.1475 y: 0.1333 control1X: -0.0339 y: 0.0602 control2X: -0.0831 y: 0.1047! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans82
	vectorGraphicsEngine
		rel_MoveToX: 0.4816 y: 0.6144;
		rel_CubicBezierToX: 0.0615 y: 0.0459 control1X: 0.0212 y: 0.0071 control2X: 0.0417 y: 0.0224;
		rel_CubicBezierToX: 0.0606 y: 0.0967 control1X: 0.0202 y: 0.0234 control2X: 0.0404 y: 0.0556;
		rel_LineToX: 0.1001 y: 0.1992;
		rel_LineToX: -0.1060 y: 0.0000;
		rel_LineToX: -0.0932 y: -0.1870;
		rel_CubicBezierToX: -0.0703 y: -0.0972 control1X: -0.0241 y: -0.0489 control2X: -0.0476 y: -0.0812;
		rel_CubicBezierToX: -0.0923 y: -0.0239 control1X: -0.0225 y: -0.0159 control2X: -0.0533 y: -0.0239;
		rel_LineToX: -0.1075 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.3081;
		rel_LineToX: -0.0986 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7290;
		rel_LineToX: 0.2227 y: 0.0000;
		rel_CubicBezierToX: 0.1865 y: 0.0522 control1X: 0.0833 y: 0.0000 control2X: 0.1455 y: 0.0174;
		rel_CubicBezierToX: 0.0615 y: 0.1577 control1X: 0.0410 y: 0.0349 control2X: 0.0615 y: 0.0874;
		rel_CubicBezierToX: -0.0322 y: 0.1143 control1X: 0.0000 y: 0.0459 control2X: -0.0107 y: 0.0840;
		rel_CubicBezierToX: -0.0928 y: 0.0630 control1X: -0.0212 y: 0.0303 control2X: -0.0521 y: 0.0513;
		rel_MoveToX: -0.2471 y: -0.3062;
		rel_LineToX: 0.0000 y: 0.2588;
		rel_LineToX: 0.1241 y: 0.0000;
		rel_CubicBezierToX: 0.1074 y: -0.0327 control1X: 0.0475 y: 0.0000 control2X: 0.0833 y: -0.0109;
		rel_CubicBezierToX: 0.0366 y: -0.0972 control1X: 0.0244 y: -0.0221 control2X: 0.0366 y: -0.0545;
		rel_CubicBezierToX: -0.0366 y: -0.0962 control1X: 0.0000 y: -0.0426 control2X: -0.0122 y: -0.0747;
		rel_CubicBezierToX: -0.1074 y: -0.0327 control1X: -0.0241 y: -0.0218 control2X: -0.0599 y: -0.0327;
		rel_LineToX: -0.1241 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans83
	vectorGraphicsEngine
		rel_MoveToX: 0.5729 y: 0.2511;
		rel_LineToX: 0.0000 y: 0.0962;
		rel_CubicBezierToX: -0.1059 y: -0.0400 control1X: -0.0374 y: -0.0179 control2X: -0.0727 y: -0.0313;
		rel_CubicBezierToX: -0.0962 y: -0.0132 control1X: -0.0332 y: -0.0088 control2X: -0.0653 y: -0.0132;
		rel_CubicBezierToX: -0.1245 y: 0.0312 control1X: -0.0537 y: 0.0000 control2X: -0.0952 y: 0.0104;
		rel_CubicBezierToX: -0.0435 y: 0.0889 control1X: -0.0290 y: 0.0209 control2X: -0.0435 y: 0.0505;
		rel_CubicBezierToX: 0.0288 y: 0.0732 control1X: 0.0000 y: 0.0322 control2X: 0.0096 y: 0.0566;
		rel_CubicBezierToX: 0.1104 y: 0.0396 control1X: 0.0195 y: 0.0163 control2X: 0.0563 y: 0.0295;
		rel_LineToX: 0.0595 y: 0.0122;
		rel_CubicBezierToX: 0.1626 y: 0.0742 control1X: 0.0736 y: 0.0140 control2X: 0.1278 y: 0.0387;
		rel_CubicBezierToX: 0.0528 y: 0.1416 control1X: 0.0352 y: 0.0352 control2X: 0.0528 y: 0.0824;
		rel_CubicBezierToX: -0.0713 y: 0.1607 control1X: 0.0000 y: 0.0706 control2X: -0.0238 y: 0.1242;
		rel_CubicBezierToX: -0.2080 y: 0.0546 control1X: -0.0472 y: 0.0364 control2X: -0.1166 y: 0.0546;
		rel_CubicBezierToX: -0.1104 y: -0.0117 control1X: -0.0345 y: 0.0000 control2X: -0.0713 y: -0.0039;
		rel_CubicBezierToX: -0.1206 y: -0.0346 control1X: -0.0387 y: -0.0078 control2X: -0.0789 y: -0.0193;
		rel_LineToX: 0.0000 y: -0.1016;
		rel_CubicBezierToX: 0.1177 y: 0.0508 control1X: 0.0401 y: 0.0225 control2X: 0.0793 y: 0.0394;
		rel_CubicBezierToX: 0.1133 y: 0.0171 control1X: 0.0384 y: 0.0114 control2X: 0.0762 y: 0.0171;
		rel_CubicBezierToX: 0.1303 y: -0.0332 control1X: 0.0563 y: 0.0000 control2X: 0.0997 y: -0.0111;
		rel_CubicBezierToX: 0.0459 y: -0.0948 control1X: 0.0306 y: -0.0222 control2X: 0.0459 y: -0.0538;
		rel_CubicBezierToX: -0.0332 y: -0.0840 control1X: 0.0000 y: -0.0358 control2X: -0.0110 y: -0.0638;
		rel_CubicBezierToX: -0.1079 y: -0.0454 control1X: -0.0218 y: -0.0201 control2X: -0.0577 y: -0.0353;
		rel_LineToX: -0.0600 y: -0.0117;
		rel_CubicBezierToX: -0.1597 y: -0.0688 control1X: -0.0736 y: -0.0146 control2X: -0.1268 y: -0.0376;
		rel_CubicBezierToX: -0.0493 y: -0.1304 control1X: -0.0329 y: -0.0313 control2X: -0.0493 y: -0.0747;
		rel_CubicBezierToX: 0.0679 y: -0.1523 control1X: 0.0000 y: -0.0645 control2X: 0.0226 y: -0.1152;
		rel_CubicBezierToX: 0.1879 y: -0.0557 control1X: 0.0455 y: -0.0372 control2X: 0.1082 y: -0.0557;
		rel_CubicBezierToX: 0.1045 y: 0.0093 control1X: 0.0342 y: 0.0000 control2X: 0.0691 y: 0.0031;
		rel_CubicBezierToX: 0.1089 y: 0.0278 control1X: 0.0355 y: 0.0062 control2X: 0.0718 y: 0.0154! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans84
	vectorGraphicsEngine
		rel_MoveToX: 0.0348 y: 0.2272;
		rel_LineToX: 0.6167 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.2588 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.6460;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.6460;
		rel_LineToX: -0.2588 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans85
	vectorGraphicsEngine
		rel_MoveToX: 0.1247 y: 0.2272;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4428;
		rel_CubicBezierToX: 0.0425 y: 0.1690 control1X: 0.0000 y: 0.0782 control2X: 0.0142 y: 0.1345;
		rel_CubicBezierToX: 0.1377 y: 0.0513 control1X: 0.0283 y: 0.0342 control2X: 0.0742 y: 0.0513;
		rel_CubicBezierToX: 0.1372 y: -0.0513 control1X: 0.0631 y: 0.0000 control2X: 0.1089 y: -0.0171;
		rel_CubicBezierToX: 0.0425 y: -0.1690 control1X: 0.0283 y: -0.0345 control2X: 0.0425 y: -0.0908;
		rel_LineToX: 0.0000 y: -0.4428;
		rel_LineToX: 0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.4551;
		rel_CubicBezierToX: -0.0708 y: 0.2153 control1X: 0.0000 y: 0.0950 control2X: -0.0236 y: 0.1668;
		rel_CubicBezierToX: -0.2080 y: 0.0727 control1X: -0.0469 y: 0.0485 control2X: -0.1162 y: 0.0727;
		rel_CubicBezierToX: -0.2090 y: -0.0727 control1X: -0.0921 y: 0.0000 control2X: -0.1618 y: -0.0242;
		rel_CubicBezierToX: -0.0703 y: -0.2153 control1X: -0.0469 y: -0.0485 control2X: -0.0703 y: -0.1203;
		rel_LineToX: 0.0000 y: -0.4551! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans86
	vectorGraphicsEngine
		rel_MoveToX: 0.3239 y: 0.9562;
		rel_LineToX: -0.2783 y: -0.7290;
		rel_LineToX: 0.1030 y: 0.0000;
		rel_LineToX: 0.2310 y: 0.6137;
		rel_LineToX: 0.2314 y: -0.6137;
		rel_LineToX: 0.1025 y: 0.0000;
		rel_LineToX: -0.2778 y: 0.7290;
		rel_LineToX: -0.1118 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans87
	vectorGraphicsEngine
		rel_MoveToX: 0.0710 y: 0.2272;
		rel_LineToX: 0.0996 y: 0.0000;
		rel_LineToX: 0.1533 y: 0.6162;
		rel_LineToX: 0.1528 y: -0.6162;
		rel_LineToX: 0.1109 y: 0.0000;
		rel_LineToX: 0.1533 y: 0.6162;
		rel_LineToX: 0.1528 y: -0.6162;
		rel_LineToX: 0.1001 y: 0.0000;
		rel_LineToX: -0.1831 y: 0.7290;
		rel_LineToX: -0.1240 y: 0.0000;
		rel_LineToX: -0.1538 y: -0.6328;
		rel_LineToX: -0.1553 y: 0.6328;
		rel_LineToX: -0.1240 y: 0.0000;
		rel_LineToX: -0.1826 y: -0.7290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans88
	vectorGraphicsEngine
		rel_MoveToX: 0.1008 y: 0.2272;
		rel_LineToX: 0.1059 y: 0.0000;
		rel_LineToX: 0.1812 y: 0.2710;
		rel_LineToX: 0.1821 y: -0.2710;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: -0.2344 y: 0.3501;
		rel_LineToX: 0.2500 y: 0.3789;
		rel_LineToX: -0.1060 y: 0.0000;
		rel_LineToX: -0.2051 y: -0.3101;
		rel_LineToX: -0.2065 y: 0.3101;
		rel_LineToX: -0.1064 y: 0.0000;
		rel_LineToX: 0.2602 y: -0.3892;
		rel_LineToX: -0.2270 y: -0.3398! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans89
	vectorGraphicsEngine
		rel_MoveToX: 0.0358 y: 0.2272;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: 0.2021 y: 0.2998;
		rel_LineToX: 0.2007 y: -0.2998;
		rel_LineToX: 0.1060 y: 0.0000;
		rel_LineToX: -0.2579 y: 0.3818;
		rel_LineToX: 0.0000 y: 0.3472;
		rel_LineToX: -0.0991 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.3472;
		rel_LineToX: -0.2578 y: -0.3818! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans90
	vectorGraphicsEngine
		rel_MoveToX: 0.0939 y: 0.2272;
		rel_LineToX: 0.5728 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0752;
		rel_LineToX: -0.4610 y: 0.5708;
		rel_LineToX: 0.4722 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.5952 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0752;
		rel_LineToX: 0.4609 y: -0.5708;
		rel_LineToX: -0.4497 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans91
	vectorGraphicsEngine
		rel_MoveToX: 0.1237 y: 0.1964;
		rel_LineToX: 0.2070 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.1172 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.7520;
		rel_LineToX: 0.1172 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0698;
		rel_LineToX: -0.2070 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.8916! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans92
	vectorGraphicsEngine
		rel_MoveToX: 0.1208 y: 0.2272;
		rel_LineToX: 0.2539 y: 0.8218;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: -0.2539 y: -0.8218;
		rel_LineToX: 0.0830 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans93
	vectorGraphicsEngine
		rel_MoveToX: 0.3420 y: 0.1964;
		rel_LineToX: 0.0000 y: 0.8916;
		rel_LineToX: -0.2071 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.1167 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7520;
		rel_LineToX: -0.1167 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0698;
		rel_LineToX: 0.2071 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans94
	vectorGraphicsEngine
		rel_MoveToX: 0.4347 y: 0.2238;
		rel_LineToX: 0.0440 y: 0.0000;
		rel_LineToX: 0.1914 y: 0.1923;
		rel_LineToX: -0.0586 y: 0.0586;
		rel_LineToX: -0.1133 y: -0.1142;
		rel_LineToX: 0.0000 y: 0.5957;
		rel_LineToX: -0.0830 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.5957;
		rel_LineToX: -0.1142 y: 0.1142;
		rel_LineToX: -0.0586 y: -0.0586;
		rel_LineToX: 0.1923 y: -0.1923! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans95
	vectorGraphicsEngine
		rel_MoveToX: 0.0866 y: 0.6647;
		rel_LineToX: 0.0000 y: -0.0440;
		rel_LineToX: 0.1919 y: -0.1919;
		rel_LineToX: 0.0586 y: 0.0586;
		rel_LineToX: -0.1138 y: 0.1138;
		rel_LineToX: 0.5952 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_LineToX: -0.5952 y: 0.0000;
		rel_LineToX: 0.1138 y: 0.1138;
		rel_LineToX: -0.0586 y: 0.0586;
		rel_LineToX: -0.1919 y: -0.1919! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans96
	vectorGraphicsEngine
		rel_MoveToX: 0.2170 y: 0.1564;
		rel_LineToX: 0.1377 y: 0.1826;
		rel_LineToX: -0.0747 y: 0.0000;
		rel_LineToX: -0.1592 y: -0.1826;
		rel_LineToX: 0.0962 y: 0.0000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans97
	vectorGraphicsEngine
		rel_MoveToX: 0.3805 y: 0.6813;
		rel_CubicBezierToX: -0.1508 y: 0.0249 control1X: -0.0726 y: 0.0000 control2X: -0.1228 y: 0.0083;
		rel_CubicBezierToX: -0.0420 y: 0.0849 control1X: -0.0280 y: 0.0166 control2X: -0.0420 y: 0.0449;
		rel_CubicBezierToX: 0.0312 y: 0.0762 control1X: 0.0000 y: 0.0319 control2X: 0.0104 y: 0.0573;
		rel_CubicBezierToX: 0.0860 y: 0.0278 control1X: 0.0212 y: 0.0186 control2X: 0.0498 y: 0.0278;
		rel_CubicBezierToX: 0.1196 y: -0.0527 control1X: 0.0498 y: 0.0000 control2X: 0.0896 y: -0.0175;
		rel_CubicBezierToX: 0.0454 y: -0.1411 control1X: 0.0303 y: -0.0355 control2X: 0.0454 y: -0.0825;
		rel_LineToX: 0.0000 y: -0.0200;
		rel_LineToX: -0.0894 y: 0.0000;
		rel_MoveToX: 0.1792 y: -0.0371;
		rel_LineToX: 0.0000 y: 0.3120;
		rel_LineToX: -0.0898 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: -0.0767 y: 0.0737 control1X: -0.0205 y: 0.0332 control2X: -0.0461 y: 0.0578;
		rel_CubicBezierToX: -0.1123 y: 0.0234 control1X: -0.0306 y: 0.0156 control2X: -0.0680 y: 0.0234;
		rel_CubicBezierToX: -0.1338 y: -0.0468 control1X: -0.0560 y: 0.0000 control2X: -0.1006 y: -0.0156;
		rel_CubicBezierToX: -0.0493 y: -0.1265 control1X: -0.0328 y: -0.0316 control2X: -0.0493 y: -0.0738;
		rel_CubicBezierToX: 0.0615 y: -0.1392 control1X: 0.0000 y: -0.0615 control2X: 0.0205 y: -0.1079;
		rel_CubicBezierToX: 0.1846 y: -0.0468 control1X: 0.0414 y: -0.0312 control2X: 0.1029 y: -0.0468;
		rel_LineToX: 0.1260 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.0088;
		rel_CubicBezierToX: -0.0410 y: -0.0957 control1X: 0.0000 y: -0.0414 control2X: -0.0137 y: -0.0733;
		rel_CubicBezierToX: -0.1143 y: -0.0342 control1X: -0.0270 y: -0.0228 control2X: -0.0651 y: -0.0342;
		rel_CubicBezierToX: -0.0913 y: 0.0112 control1X: -0.0312 y: 0.0000 control2X: -0.0617 y: 0.0037;
		rel_CubicBezierToX: -0.0854 y: 0.0337 control1X: -0.0296 y: 0.0075 control2X: -0.0581 y: 0.0187;
		rel_LineToX: 0.0000 y: -0.0830;
		rel_CubicBezierToX: 0.0957 y: -0.0283 control1X: 0.0328 y: -0.0127 control2X: 0.0647 y: -0.0221;
		rel_CubicBezierToX: 0.0903 y: -0.0098 control1X: 0.0309 y: -0.0065 control2X: 0.0610 y: -0.0098;
		rel_CubicBezierToX: 0.1772 y: 0.0615 control1X: 0.0791 y: 0.0000 control2X: 0.1382 y: 0.0205;
		rel_CubicBezierToX: 0.0586 y: 0.1866 control1X: 0.0391 y: 0.0411 control2X: 0.0586 y: 0.1032! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans98
	vectorGraphicsEngine
		rel_MoveToX: 0.5246 y: 0.6832;
		rel_CubicBezierToX: -0.0410 y: -0.1552 control1X: 0.0000 y: -0.0660 control2X: -0.0137 y: -0.1178;
		rel_CubicBezierToX: -0.1118 y: -0.0567 control1X: -0.0271 y: -0.0378 control2X: -0.0643 y: -0.0567;
		rel_CubicBezierToX: -0.1124 y: 0.0567 control1X: -0.0476 y: 0.0000 control2X: -0.0850 y: 0.0189;
		rel_CubicBezierToX: -0.0405 y: 0.1552 control1X: -0.0270 y: 0.0374 control2X: -0.0405 y: 0.0892;
		rel_CubicBezierToX: 0.0405 y: 0.1558 control1X: 0.0000 y: 0.0661 control2X: 0.0135 y: 0.1180;
		rel_CubicBezierToX: 0.1124 y: 0.0561 control1X: 0.0274 y: 0.0374 control2X: 0.0648 y: 0.0561;
		rel_CubicBezierToX: 0.1118 y: -0.0561 control1X: 0.0475 y: 0.0000 control2X: 0.0847 y: -0.0187;
		rel_CubicBezierToX: 0.0410 y: -0.1558 control1X: 0.0273 y: -0.0378 control2X: 0.0410 y: -0.0897;
		rel_MoveToX: -0.3057 y: -0.1909;
		rel_CubicBezierToX: 0.0713 y: -0.0723 control1X: 0.0189 y: -0.0325 control2X: 0.0427 y: -0.0566;
		rel_CubicBezierToX: 0.1035 y: -0.0239 control1X: 0.0290 y: -0.0159 control2X: 0.0635 y: -0.0239;
		rel_CubicBezierToX: 0.1616 y: 0.0791 control1X: 0.0664 y: 0.0000 control2X: 0.1203 y: 0.0264;
		rel_CubicBezierToX: 0.0625 y: 0.2080 control1X: 0.0417 y: 0.0528 control2X: 0.0625 y: 0.1221;
		rel_CubicBezierToX: -0.0625 y: 0.2080 control1X: 0.0000 y: 0.0860 control2X: -0.0208 y: 0.1553;
		rel_CubicBezierToX: -0.1616 y: 0.0791 control1X: -0.0413 y: 0.0528 control2X: -0.0952 y: 0.0791;
		rel_CubicBezierToX: -0.1035 y: -0.0234 control1X: -0.0400 y: 0.0000 control2X: -0.0745 y: -0.0078;
		rel_CubicBezierToX: -0.0713 y: -0.0727 control1X: -0.0286 y: -0.0159 control2X: -0.0524 y: -0.0402;
		rel_LineToX: 0.0000 y: 0.0820;
		rel_LineToX: -0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: -0.7598;
		rel_LineToX: 0.0903 y: 0.0000;
		rel_LineToX: 0.0000 y: 0.2959! !

!BitstreamVeraRenderer methodsFor: 'glyphs - relative coordinates' stamp: 'jmv 1/3/2018 09:58:44'!
drawRelVeraSans99
	vectorGraphicsEngine
		rel_MoveToX: 0.5256 y: 0.4303;
		rel_LineToX: 0.0000 y: 0.0840;
		rel_CubicBezierToX: -0.0767 y: -0.0313 control1X: -0.0254 y: -0.0140 control2X: -0.0510 y: -0.0244;
		rel_CubicBezierToX: -0.0771 y: -0.0107 control1X: -0.0254 y: -0.0071 control2X: -0.0511 y: -0.0107;
		rel_CubicBezierToX: -0.1358 y: 0.0557 control1X: -0.0583 y: 0.0000 control2X: -0.1036 y: 0.0185;
		rel_CubicBezierToX: -0.0483 y: 0.1552 control1X: -0.0322 y: 0.0367 control2X: -0.0483 y: 0.0885;
		rel_CubicBezierToX: 0.0483 y: 0.1558 control1X: 0.0000 y: 0.0668 control2X: 0.0161 y: 0.1187;
		rel_CubicBezierToX: 0.1358 y: 0.0552 control1X: 0.0322 y: 0.0368 control2X: 0.0775 y: 0.0552;
		rel_CubicBezierToX: 0.0771 y: -0.0103 control1X: 0.0260 y: 0.0000 control2X: 0.0517 y: -0.0034;
		rel_CubicBezierToX: 0.0767 y: -0.0317 control1X: 0.0257 y: -0.0071 control2X: 0.0513 y: -0.0177;
		rel_LineToX: 0.0000 y: 0.0830;
		rel_CubicBezierToX: -0.0782 y: 0.0264 control1X: -0.0251 y: 0.0117 control2X: -0.0511 y: 0.0205;
		rel_CubicBezierToX: -0.0854 y: 0.0087 control1X: -0.0267 y: 0.0058 control2X: -0.0551 y: 0.0087;
		rel_CubicBezierToX: -0.1963 y: -0.0776 control1X: -0.0824 y: 0.0000 control2X: -0.1478 y: -0.0258;
		rel_CubicBezierToX: -0.0728 y: -0.2095 control1X: -0.0485 y: -0.0518 control2X: -0.0728 y: -0.1216;
		rel_CubicBezierToX: 0.0733 y: -0.2104 control1X: 0.0000 y: -0.0892 control2X: 0.0245 y: -0.1593;
		rel_CubicBezierToX: 0.2016 y: -0.0767 control1X: 0.0491 y: -0.0511 control2X: 0.1164 y: -0.0767;
		rel_CubicBezierToX: 0.0811 y: 0.0088 control1X: 0.0277 y: 0.0000 control2X: 0.0547 y: 0.0030;
		rel_CubicBezierToX: 0.0767 y: 0.0254 control1X: 0.0264 y: 0.0055 control2X: 0.0519 y: 0.0140! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics100
	vectorGraphicsEngine
		rel_MoveToX: 0.393 y: 0.492;
		rel_LineToX: 0.059 y: -0.296;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.152 y: 0.760;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.016 y: -0.082;
		rel_CubicBezierToX: -0.086 y: 0.073 control1X: -0.025 y: 0.032 control2X: -0.054 y: 0.057;
		rel_CubicBezierToX: -0.108 y: 0.023 control1X: -0.032 y: 0.016 control2X: -0.068 y: 0.023;
		rel_CubicBezierToX: -0.146 y: -0.079 control1X: -0.066 y: 0.000 control2X: -0.115 y: -0.026;
		rel_CubicBezierToX: -0.020 y: -0.208 control1X: -0.031 y: -0.053 control2X: -0.038 y: -0.122;
		rel_CubicBezierToX: 0.104 y: -0.208 control1X: 0.017 y: -0.086 control2X: 0.052 y: -0.155;
		rel_CubicBezierToX: 0.177 y: -0.079 control1X: 0.052 y: -0.053 control2X: 0.111 y: -0.079;
		rel_CubicBezierToX: 0.099 y: 0.024 control1X: 0.040 y: 0.000 control2X: 0.073 y: 0.008;
		rel_CubicBezierToX: 0.057 y: 0.072 control1X: 0.026 y: 0.016 control2X: 0.045 y: 0.040;
		rel_MoveToX: -0.344 y: 0.191;
		rel_CubicBezierToX: 0.009 y: 0.156 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.118;
		rel_CubicBezierToX: 0.101 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.054 y: 0.056;
		rel_CubicBezierToX: 0.124 y: -0.056 control1X: 0.048 y: 0.000 control2X: 0.089 y: -0.019;
		rel_CubicBezierToX: 0.072 y: -0.156 control1X: 0.035 y: -0.038 control2X: 0.059 y: -0.090;
		rel_CubicBezierToX: -0.010 y: -0.155 control1X: 0.013 y: -0.066 control2X: 0.010 y: -0.118;
		rel_CubicBezierToX: -0.101 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.053 y: -0.057;
		rel_CubicBezierToX: -0.124 y: 0.057 control1X: -0.047 y: 0.000 control2X: -0.089 y: 0.019;
		rel_CubicBezierToX: -0.072 y: 0.155 control1X: -0.034 y: 0.037 control2X: -0.058 y: 0.089! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics101
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.660;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.130 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.105 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.070 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.192 y: -0.076 control1X: -0.087 y: 0.000 control2X: -0.151 y: -0.025;
		rel_CubicBezierToX: -0.035 y: -0.206 control1X: -0.041 y: -0.051 control2X: -0.052 y: -0.119;
		rel_CubicBezierToX: 0.115 y: -0.213 control1X: 0.018 y: -0.090 control2X: 0.056 y: -0.160;
		rel_CubicBezierToX: 0.212 y: -0.079 control1X: 0.059 y: -0.053 control2X: 0.130 y: -0.079;
		rel_CubicBezierToX: 0.160 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.085 y: -0.026;
		rel_CubicBezierToX: -0.018 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.053 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics102
	vectorGraphicsEngine
		rel_MoveToX: 0.370 y: 0.196;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.086 y: 0.000;
		rel_CubicBezierToX: -0.071 y: 0.020 control1X: -0.032 y: 0.000 control2X: -0.056 y: 0.007;
		rel_CubicBezierToX: -0.033 y: 0.070 control1X: -0.015 y: 0.013 control2X: -0.026 y: 0.036;
		rel_LineToX: -0.010 y: 0.048;
		rel_LineToX: 0.148 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.148 y: 0.000;
		rel_LineToX: -0.095 y: 0.477;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.095 y: -0.477;
		rel_LineToX: -0.086 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.086 y: 0.000;
		rel_LineToX: 0.008 y: -0.038;
		rel_CubicBezierToX: 0.069 y: -0.133 control1X: 0.012 y: -0.061 control2X: 0.035 y: -0.105;
		rel_CubicBezierToX: 0.143 y: -0.042 control1X: 0.034 y: -0.028 control2X: 0.082 y: -0.042;
		rel_LineToX: 0.085 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics103
	vectorGraphicsEngine
		rel_MoveToX: 0.357 y: 0.676;
		rel_CubicBezierToX: -0.010 y: -0.151 control1X: 0.013 y: -0.065 control2X: 0.010 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.054 control1X: -0.020 y: -0.036 control2X: -0.053 y: -0.054;
		rel_CubicBezierToX: -0.123 y: 0.054 control1X: -0.048 y: 0.000 control2X: -0.089 y: 0.018;
		rel_CubicBezierToX: -0.070 y: 0.151 control1X: -0.034 y: 0.036 control2X: -0.057 y: 0.086;
		rel_CubicBezierToX: 0.010 y: 0.151 control1X: -0.013 y: 0.065 control2X: -0.010 y: 0.115;
		rel_CubicBezierToX: 0.102 y: 0.054 control1X: 0.020 y: 0.036 control2X: 0.054 y: 0.054;
		rel_CubicBezierToX: 0.124 y: -0.054 control1X: 0.049 y: 0.000 control2X: 0.090 y: -0.018;
		rel_CubicBezierToX: 0.071 y: -0.151 control1X: 0.034 y: -0.036 control2X: 0.058 y: -0.086;
		rel_MoveToX: 0.047 y: 0.212;
		rel_CubicBezierToX: -0.104 y: 0.207 control1X: -0.019 y: 0.093 control2X: -0.053 y: 0.162;
		rel_CubicBezierToX: -0.204 y: 0.068 control1X: -0.050 y: 0.046 control2X: -0.118 y: 0.068;
		rel_CubicBezierToX: -0.088 y: -0.007 control1X: -0.032 y: 0.000 control2X: -0.061 y: -0.002;
		rel_CubicBezierToX: -0.077 y: -0.022 control1X: -0.027 y: -0.005 control2X: -0.053 y: -0.012;
		rel_LineToX: 0.017 y: -0.087;
		rel_CubicBezierToX: 0.072 y: 0.032 control1X: 0.023 y: 0.014 control2X: 0.047 y: 0.025;
		rel_CubicBezierToX: 0.077 y: 0.010 control1X: 0.024 y: 0.007 control2X: 0.050 y: 0.010;
		rel_CubicBezierToX: 0.142 y: -0.046 control1X: 0.059 y: 0.000 control2X: 0.106 y: -0.016;
		rel_CubicBezierToX: 0.072 y: -0.139 control1X: 0.035 y: -0.031 control2X: 0.059 y: -0.077;
		rel_LineToX: 0.009 y: -0.044;
		rel_CubicBezierToX: -0.086 y: 0.072 control1X: -0.025 y: 0.032 control2X: -0.054 y: 0.056;
		rel_CubicBezierToX: -0.109 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.068 y: 0.024;
		rel_CubicBezierToX: -0.147 y: -0.077 control1X: -0.067 y: 0.000 control2X: -0.116 y: -0.026;
		rel_CubicBezierToX: -0.021 y: -0.203 control1X: -0.031 y: -0.051 control2X: -0.038 y: -0.119;
		rel_CubicBezierToX: 0.102 y: -0.204 control1X: 0.017 y: -0.085 control2X: 0.051 y: -0.152;
		rel_CubicBezierToX: 0.177 y: -0.077 control1X: 0.051 y: -0.051 control2X: 0.110 y: -0.077;
		rel_CubicBezierToX: 0.099 y: 0.024 control1X: 0.040 y: 0.000 control2X: 0.073 y: 0.008;
		rel_CubicBezierToX: 0.057 y: 0.072 control1X: 0.026 y: 0.016 control2X: 0.045 y: 0.040;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.096 y: 0.479! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics104
	vectorGraphicsEngine
		rel_MoveToX: 0.461 y: 0.626;
		rel_LineToX: -0.066 y: 0.330;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.065 y: -0.327;
		rel_CubicBezierToX: -0.007 y: -0.116 control1X: 0.010 y: -0.052 control2X: 0.008 y: -0.091;
		rel_CubicBezierToX: -0.083 y: -0.039 control1X: -0.015 y: -0.026 control2X: -0.043 y: -0.039;
		rel_CubicBezierToX: -0.124 y: 0.046 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.016;
		rel_CubicBezierToX: -0.067 y: 0.127 control1X: -0.034 y: 0.031 control2X: -0.057 y: 0.073;
		rel_LineToX: -0.062 y: 0.309;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.152 y: -0.760;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.060 y: 0.298;
		rel_CubicBezierToX: 0.090 y: -0.074 control1X: 0.028 y: -0.033 control2X: 0.058 y: -0.057;
		rel_CubicBezierToX: 0.106 y: -0.024 control1X: 0.033 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.131 y: 0.059 control1X: 0.063 y: 0.000 control2X: 0.106 y: 0.020;
		rel_CubicBezierToX: 0.014 y: 0.171 control1X: 0.024 y: 0.039 control2X: 0.029 y: 0.096! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics105
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_MoveToX: 0.043 y: -0.213;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.023 y: 0.114;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.023 y: -0.114! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics106
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.111 y: 0.557;
		rel_CubicBezierToX: -0.070 y: 0.151 control1X: -0.014 y: 0.070 control2X: -0.037 y: 0.120;
		rel_CubicBezierToX: -0.137 y: 0.047 control1X: -0.033 y: 0.031 control2X: -0.078 y: 0.047;
		rel_LineToX: -0.034 y: 0.000;
		rel_LineToX: 0.015 y: -0.076;
		rel_LineToX: 0.024 y: 0.000;
		rel_CubicBezierToX: 0.075 y: -0.024 control1X: 0.034 y: 0.000 control2X: 0.059 y: -0.008;
		rel_CubicBezierToX: 0.038 y: -0.098 control1X: 0.015 y: -0.016 control2X: 0.028 y: -0.048;
		rel_LineToX: 0.111 y: -0.557;
		rel_MoveToX: 0.043 y: -0.213;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.023 y: 0.114;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.023 y: -0.114! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics107
	vectorGraphicsEngine
		rel_MoveToX: 0.089 y: 0.196;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.090 y: 0.449;
		rel_LineToX: 0.315 y: -0.236;
		rel_LineToX: 0.115 y: 0.000;
		rel_LineToX: -0.341 y: 0.256;
		rel_LineToX: 0.244 y: 0.291;
		rel_LineToX: -0.117 y: 0.000;
		rel_LineToX: -0.224 y: -0.267;
		rel_LineToX: -0.053 y: 0.267;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.152 y: -0.760! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics108
	vectorGraphicsEngine
		rel_MoveToX: 0.093 y: 0.196;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.152 y: 0.760;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.152 y: -0.760! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics109
	vectorGraphicsEngine
		rel_MoveToX: 0.455 y: 0.514;
		rel_CubicBezierToX: 0.098 y: -0.089 control1X: 0.030 y: -0.040 control2X: 0.063 y: -0.070;
		rel_CubicBezierToX: 0.116 y: -0.029 control1X: 0.035 y: -0.019 control2X: 0.074 y: -0.029;
		rel_CubicBezierToX: 0.120 y: 0.060 control1X: 0.057 y: 0.000 control2X: 0.097 y: 0.020;
		rel_CubicBezierToX: 0.012 y: 0.170 control1X: 0.023 y: 0.040 control2X: 0.027 y: 0.096;
		rel_LineToX: -0.066 y: 0.330;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.065 y: -0.327;
		rel_CubicBezierToX: -0.004 y: -0.117 control1X: 0.010 y: -0.052 control2X: 0.009 y: -0.091;
		rel_CubicBezierToX: -0.077 y: -0.038 control1X: -0.014 y: -0.025 control2X: -0.039 y: -0.038;
		rel_CubicBezierToX: -0.120 y: 0.046 control1X: -0.046 y: 0.000 control2X: -0.086 y: 0.016;
		rel_CubicBezierToX: -0.066 y: 0.127 control1X: -0.033 y: 0.031 control2X: -0.055 y: 0.073;
		rel_LineToX: -0.062 y: 0.309;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.065 y: -0.327;
		rel_CubicBezierToX: -0.004 y: -0.117 control1X: 0.011 y: -0.053 control2X: 0.009 y: -0.092;
		rel_CubicBezierToX: -0.078 y: -0.038 control1X: -0.014 y: -0.025 control2X: -0.040 y: -0.038;
		rel_CubicBezierToX: -0.119 y: 0.047 control1X: -0.046 y: 0.000 control2X: -0.085 y: 0.016;
		rel_CubicBezierToX: -0.066 y: 0.126 control1X: -0.033 y: 0.031 control2X: -0.055 y: 0.073;
		rel_LineToX: -0.062 y: 0.309;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: 0.089 y: -0.074 control1X: 0.027 y: -0.034 control2X: 0.057 y: -0.058;
		rel_CubicBezierToX: 0.107 y: -0.024 control1X: 0.032 y: -0.016 control2X: 0.067 y: -0.024;
		rel_CubicBezierToX: 0.095 y: 0.030 control1X: 0.040 y: 0.000 control2X: 0.071 y: 0.010;
		rel_CubicBezierToX: 0.044 y: 0.088 control1X: 0.024 y: 0.020 control2X: 0.039 y: 0.049! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics110
	vectorGraphicsEngine
		rel_MoveToX: 0.461 y: 0.626;
		rel_LineToX: -0.066 y: 0.330;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.065 y: -0.327;
		rel_CubicBezierToX: -0.007 y: -0.116 control1X: 0.010 y: -0.052 control2X: 0.008 y: -0.091;
		rel_CubicBezierToX: -0.083 y: -0.039 control1X: -0.015 y: -0.026 control2X: -0.043 y: -0.039;
		rel_CubicBezierToX: -0.124 y: 0.046 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.016;
		rel_CubicBezierToX: -0.067 y: 0.127 control1X: -0.034 y: 0.031 control2X: -0.057 y: 0.073;
		rel_LineToX: -0.062 y: 0.309;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: 0.090 y: -0.074 control1X: 0.028 y: -0.033 control2X: 0.058 y: -0.057;
		rel_CubicBezierToX: 0.106 y: -0.024 control1X: 0.033 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.131 y: 0.059 control1X: 0.063 y: 0.000 control2X: 0.106 y: 0.020;
		rel_CubicBezierToX: 0.014 y: 0.171 control1X: 0.024 y: 0.039 control2X: 0.029 y: 0.096! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics111
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics112
	vectorGraphicsEngine
		rel_MoveToX: 0.044 y: 0.874;
		rel_LineToX: -0.058 y: 0.290;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.151 y: -0.755;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_CubicBezierToX: 0.086 y: -0.072 control1X: 0.025 y: -0.033 control2X: 0.054 y: -0.057;
		rel_CubicBezierToX: 0.108 y: -0.024 control1X: 0.032 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.146 y: 0.079 control1X: 0.066 y: 0.000 control2X: 0.115 y: 0.026;
		rel_CubicBezierToX: 0.021 y: 0.208 control1X: 0.031 y: 0.053 control2X: 0.038 y: 0.122;
		rel_CubicBezierToX: -0.104 y: 0.208 control1X: -0.017 y: 0.086 control2X: -0.052 y: 0.155;
		rel_CubicBezierToX: -0.177 y: 0.079 control1X: -0.052 y: 0.053 control2X: -0.111 y: 0.079;
		rel_CubicBezierToX: -0.099 y: -0.023 control1X: -0.040 y: 0.000 control2X: -0.073 y: -0.008;
		rel_CubicBezierToX: -0.057 y: -0.073 control1X: -0.025 y: -0.016 control2X: -0.044 y: -0.040;
		rel_MoveToX: 0.344 y: -0.191;
		rel_CubicBezierToX: -0.010 y: -0.155 control1X: 0.013 y: -0.066 control2X: 0.010 y: -0.118;
		rel_CubicBezierToX: -0.100 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.053 y: -0.057;
		rel_CubicBezierToX: -0.124 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.089 y: 0.019;
		rel_CubicBezierToX: -0.072 y: 0.155 control1X: -0.034 y: 0.037 control2X: -0.058 y: 0.089;
		rel_CubicBezierToX: 0.009 y: 0.156 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.118;
		rel_CubicBezierToX: 0.101 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.054 y: 0.056;
		rel_CubicBezierToX: 0.123 y: -0.056 control1X: 0.047 y: 0.000 control2X: 0.088 y: -0.019;
		rel_CubicBezierToX: 0.072 y: -0.156 control1X: 0.035 y: -0.038 control2X: 0.059 y: -0.090! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics113
	vectorGraphicsEngine
		rel_MoveToX: 0.049 y: 0.683;
		rel_CubicBezierToX: 0.009 y: 0.156 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.118;
		rel_CubicBezierToX: 0.101 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.054 y: 0.056;
		rel_CubicBezierToX: 0.124 y: -0.056 control1X: 0.048 y: 0.000 control2X: 0.089 y: -0.019;
		rel_CubicBezierToX: 0.072 y: -0.156 control1X: 0.035 y: -0.038 control2X: 0.059 y: -0.090;
		rel_CubicBezierToX: -0.010 y: -0.155 control1X: 0.013 y: -0.066 control2X: 0.010 y: -0.118;
		rel_CubicBezierToX: -0.101 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.053 y: -0.057;
		rel_CubicBezierToX: -0.124 y: 0.057 control1X: -0.047 y: 0.000 control2X: -0.089 y: 0.019;
		rel_CubicBezierToX: -0.072 y: 0.155 control1X: -0.034 y: 0.037 control2X: -0.058 y: 0.089;
		rel_MoveToX: 0.268 y: 0.191;
		rel_CubicBezierToX: -0.086 y: 0.073 control1X: -0.025 y: 0.032 control2X: -0.054 y: 0.057;
		rel_CubicBezierToX: -0.108 y: 0.023 control1X: -0.032 y: 0.016 control2X: -0.068 y: 0.023;
		rel_CubicBezierToX: -0.146 y: -0.079 control1X: -0.066 y: 0.000 control2X: -0.115 y: -0.026;
		rel_CubicBezierToX: -0.020 y: -0.208 control1X: -0.031 y: -0.053 control2X: -0.038 y: -0.122;
		rel_CubicBezierToX: 0.104 y: -0.208 control1X: 0.017 y: -0.086 control2X: 0.052 y: -0.155;
		rel_CubicBezierToX: 0.177 y: -0.079 control1X: 0.052 y: -0.053 control2X: 0.111 y: -0.079;
		rel_CubicBezierToX: 0.099 y: 0.024 control1X: 0.040 y: 0.000 control2X: 0.073 y: 0.008;
		rel_CubicBezierToX: 0.057 y: 0.072 control1X: 0.026 y: 0.016 control2X: 0.045 y: 0.040;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.151 y: 0.755;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.058 y: -0.290! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics114
	vectorGraphicsEngine
		rel_MoveToX: 0.350 y: 0.493;
		rel_CubicBezierToX: -0.031 y: -0.013 control1X: -0.009 y: -0.006 control2X: -0.019 y: -0.010;
		rel_CubicBezierToX: -0.038 y: -0.004 control1X: -0.011 y: -0.003 control2X: -0.024 y: -0.004;
		rel_CubicBezierToX: -0.127 y: 0.050 control1X: -0.051 y: 0.000 control2X: -0.093 y: 0.017;
		rel_CubicBezierToX: -0.069 y: 0.142 control1X: -0.034 y: 0.033 control2X: -0.057 y: 0.080;
		rel_LineToX: -0.058 y: 0.288;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: 0.088 y: -0.074 control1X: 0.026 y: -0.033 control2X: 0.055 y: -0.058;
		rel_CubicBezierToX: 0.115 y: -0.024 control1X: 0.034 y: -0.016 control2X: 0.072 y: -0.024;
		rel_CubicBezierToX: 0.020 y: 0.002 control1X: 0.006 y: 0.000 control2X: 0.013 y: 0.001;
		rel_CubicBezierToX: 0.024 y: 0.003 control1X: 0.007 y: 0.001 control2X: 0.015 y: 0.002;
		rel_LineToX: -0.018 y: 0.092! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics115
	vectorGraphicsEngine
		rel_MoveToX: 0.396 y: 0.425;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.073 y: -0.029 control1X: -0.023 y: -0.013 control2X: -0.047 y: -0.023;
		rel_CubicBezierToX: -0.083 y: -0.010 control1X: -0.026 y: -0.007 control2X: -0.054 y: -0.010;
		rel_CubicBezierToX: -0.105 y: 0.021 control1X: -0.045 y: 0.000 control2X: -0.079 y: 0.007;
		rel_CubicBezierToX: -0.046 y: 0.062 control1X: -0.025 y: 0.014 control2X: -0.040 y: 0.034;
		rel_CubicBezierToX: 0.014 y: 0.049 control1X: -0.004 y: 0.021 control2X: 0.001 y: 0.037;
		rel_CubicBezierToX: 0.089 y: 0.034 control1X: 0.014 y: 0.012 control2X: 0.043 y: 0.023;
		rel_LineToX: 0.029 y: 0.007;
		rel_CubicBezierToX: 0.124 y: 0.058 control1X: 0.061 y: 0.014 control2X: 0.102 y: 0.033;
		rel_CubicBezierToX: 0.020 y: 0.104 control1X: 0.022 y: 0.025 control2X: 0.029 y: 0.059;
		rel_CubicBezierToX: -0.085 y: 0.121 control1X: -0.010 y: 0.051 control2X: -0.038 y: 0.091;
		rel_CubicBezierToX: -0.174 y: 0.044 control1X: -0.046 y: 0.030 control2X: -0.104 y: 0.044;
		rel_CubicBezierToX: -0.090 y: -0.009 control1X: -0.029 y: 0.000 control2X: -0.059 y: -0.003;
		rel_CubicBezierToX: -0.095 y: -0.025 control1X: -0.030 y: -0.006 control2X: -0.062 y: -0.014;
		rel_LineToX: 0.019 y: -0.093;
		rel_CubicBezierToX: 0.090 y: 0.039 control1X: 0.030 y: 0.017 control2X: 0.060 y: 0.030;
		rel_CubicBezierToX: 0.093 y: 0.013 control1X: 0.031 y: 0.008 control2X: 0.062 y: 0.013;
		rel_CubicBezierToX: 0.102 y: -0.021 control1X: 0.042 y: 0.000 control2X: 0.076 y: -0.007;
		rel_CubicBezierToX: 0.046 y: -0.062 control1X: 0.026 y: -0.015 control2X: 0.041 y: -0.035;
		rel_CubicBezierToX: -0.014 y: -0.056 control1X: 0.005 y: -0.024 control2X: 0.000 y: -0.043;
		rel_CubicBezierToX: -0.100 y: -0.038 control1X: -0.014 y: -0.013 control2X: -0.047 y: -0.026;
		rel_LineToX: -0.030 y: -0.007;
		rel_CubicBezierToX: -0.110 y: -0.054 control1X: -0.053 y: -0.012 control2X: -0.090 y: -0.030;
		rel_CubicBezierToX: -0.017 y: -0.100 control1X: -0.020 y: -0.024 control2X: -0.026 y: -0.058;
		rel_CubicBezierToX: 0.079 y: -0.119 control1X: 0.010 y: -0.051 control2X: 0.036 y: -0.091;
		rel_CubicBezierToX: 0.164 y: -0.042 control1X: 0.042 y: -0.028 control2X: 0.097 y: -0.042;
		rel_CubicBezierToX: 0.092 y: 0.007 control1X: 0.033 y: 0.000 control2X: 0.064 y: 0.003;
		rel_CubicBezierToX: 0.077 y: 0.022 control1X: 0.028 y: 0.005 control2X: 0.054 y: 0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics116
	vectorGraphicsEngine
		rel_MoveToX: 0.170 y: 0.254;
		rel_LineToX: -0.031 y: 0.155;
		rel_LineToX: 0.185 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.185 y: 0.000;
		rel_LineToX: -0.059 y: 0.297;
		rel_CubicBezierToX: 0.001 y: 0.086 control1X: -0.009 y: 0.045 control2X: -0.009 y: 0.073;
		rel_CubicBezierToX: 0.071 y: 0.019 control1X: 0.010 y: 0.013 control2X: 0.033 y: 0.019;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.092 y: 0.000;
		rel_CubicBezierToX: -0.136 y: -0.039 control1X: -0.069 y: 0.000 control2X: -0.115 y: -0.013;
		rel_CubicBezierToX: -0.011 y: -0.142 control1X: -0.021 y: -0.026 control2X: -0.025 y: -0.073;
		rel_LineToX: 0.059 y: -0.297;
		rel_LineToX: -0.066 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.066 y: 0.000;
		rel_LineToX: 0.031 y: -0.155;
		rel_LineToX: 0.090 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics117
	vectorGraphicsEngine
		rel_MoveToX: -0.025 y: 0.740;
		rel_LineToX: 0.066 y: -0.331;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.066 y: 0.328;
		rel_CubicBezierToX: 0.007 y: 0.117 control1X: -0.010 y: 0.052 control2X: -0.008 y: 0.091;
		rel_CubicBezierToX: 0.083 y: 0.039 control1X: 0.015 y: 0.026 control2X: 0.043 y: 0.039;
		rel_CubicBezierToX: 0.124 y: -0.046 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.015;
		rel_CubicBezierToX: 0.068 y: -0.126 control1X: 0.034 y: -0.031 control2X: 0.057 y: -0.073;
		rel_LineToX: 0.062 y: -0.310;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.084;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.028 y: 0.033 control2X: -0.059 y: 0.058;
		rel_CubicBezierToX: -0.105 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.067 y: 0.024;
		rel_CubicBezierToX: -0.131 y: -0.059 control1X: -0.063 y: 0.000 control2X: -0.107 y: -0.019;
		rel_CubicBezierToX: -0.015 y: -0.171 control1X: -0.025 y: -0.039 control2X: -0.030 y: -0.096! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics118
	vectorGraphicsEngine
		rel_MoveToX: -0.014 y: 0.409;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: 0.079 y: 0.459;
		rel_LineToX: 0.263 y: -0.459;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: -0.314 y: 0.547;
		rel_LineToX: -0.122 y: 0.000;
		rel_LineToX: -0.096 y: -0.547! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics119
	vectorGraphicsEngine
		rel_MoveToX: -0.002 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: 0.027 y: 0.427;
		rel_LineToX: 0.197 y: -0.427;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.027 y: 0.427;
		rel_LineToX: 0.197 y: -0.427;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.252 y: 0.547;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: -0.028 y: -0.448;
		rel_LineToX: -0.208 y: 0.448;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: -0.034 y: -0.547! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics120
	vectorGraphicsEngine
		rel_MoveToX: 0.505 y: 0.409;
		rel_LineToX: -0.251 y: 0.266;
		rel_LineToX: 0.152 y: 0.281;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: -0.116 y: -0.215;
		rel_LineToX: -0.202 y: 0.215;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: 0.270 y: -0.286;
		rel_LineToX: -0.142 y: -0.261;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.106 y: 0.195;
		rel_LineToX: 0.184 y: -0.195;
		rel_LineToX: 0.106 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics121
	vectorGraphicsEngine
		rel_MoveToX: 0.158 y: 1.007;
		rel_CubicBezierToX: -0.100 y: 0.127 control1X: -0.038 y: 0.065 control2X: -0.072 y: 0.107;
		rel_CubicBezierToX: -0.103 y: 0.030 control1X: -0.028 y: 0.020 control2X: -0.062 y: 0.030;
		rel_LineToX: -0.072 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.053 y: 0.000;
		rel_CubicBezierToX: 0.061 y: -0.018 control1X: 0.025 y: 0.000 control2X: 0.045 y: -0.006;
		rel_CubicBezierToX: 0.062 y: -0.083 control1X: 0.016 y: -0.012 control2X: 0.037 y: -0.039;
		rel_LineToX: 0.024 y: -0.041;
		rel_LineToX: -0.113 y: -0.538;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: 0.085 y: 0.428;
		rel_LineToX: 0.256 y: -0.428;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: -0.360 y: 0.598! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics122
	vectorGraphicsEngine
		rel_MoveToX: 0.011 y: 0.409;
		rel_LineToX: 0.427 y: 0.000;
		rel_LineToX: -0.016 y: 0.082;
		rel_LineToX: -0.417 y: 0.393;
		rel_LineToX: 0.338 y: 0.000;
		rel_LineToX: -0.014 y: 0.072;
		rel_LineToX: -0.439 y: 0.000;
		rel_LineToX: 0.016 y: -0.082;
		rel_LineToX: 0.417 y: -0.393;
		rel_LineToX: -0.326 y: 0.000;
		rel_LineToX: 0.014 y: -0.072! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics123
	vectorGraphicsEngine
		rel_MoveToX: 0.339 y: 1.049;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.030 y: 0.000;
		rel_CubicBezierToX: -0.156 y: -0.036 control1X: -0.081 y: 0.000 control2X: -0.133 y: -0.012;
		rel_CubicBezierToX: -0.012 y: -0.144 control1X: -0.022 y: -0.024 control2X: -0.027 y: -0.072;
		rel_LineToX: 0.023 y: -0.117;
		rel_CubicBezierToX: -0.006 y: -0.102 control1X: 0.010 y: -0.049 control2X: 0.008 y: -0.083;
		rel_CubicBezierToX: -0.090 y: -0.028 control1X: -0.014 y: -0.019 control2X: -0.044 y: -0.028;
		rel_LineToX: -0.030 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.030 y: 0.000;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.046 y: 0.000 control2X: 0.080 y: -0.009;
		rel_CubicBezierToX: 0.047 y: -0.101 control1X: 0.021 y: -0.019 control2X: 0.037 y: -0.053;
		rel_LineToX: 0.023 y: -0.117;
		rel_CubicBezierToX: 0.070 y: -0.144 control1X: 0.014 y: -0.072 control2X: 0.038 y: -0.120;
		rel_CubicBezierToX: 0.170 y: -0.036 control1X: 0.033 y: -0.024 control2X: 0.089 y: -0.036;
		rel_LineToX: 0.030 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.033 y: 0.000;
		rel_CubicBezierToX: -0.094 y: 0.022 control1X: -0.046 y: 0.000 control2X: -0.077 y: 0.007;
		rel_CubicBezierToX: -0.039 y: 0.090 control1X: -0.017 y: 0.014 control2X: -0.030 y: 0.044;
		rel_LineToX: -0.024 y: 0.121;
		rel_CubicBezierToX: -0.045 y: 0.111 control1X: -0.010 y: 0.051 control2X: -0.025 y: 0.088;
		rel_CubicBezierToX: -0.085 y: 0.047 control1X: -0.019 y: 0.023 control2X: -0.048 y: 0.039;
		rel_CubicBezierToX: 0.067 y: 0.048 control1X: 0.034 y: 0.009 control2X: 0.057 y: 0.025;
		rel_CubicBezierToX: 0.000 y: 0.111 control1X: 0.010 y: 0.023 control2X: 0.010 y: 0.060;
		rel_LineToX: -0.024 y: 0.121;
		rel_CubicBezierToX: 0.003 y: 0.090 control1X: -0.009 y: 0.046 control2X: -0.008 y: 0.076;
		rel_CubicBezierToX: 0.085 y: 0.022 control1X: 0.011 y: 0.014 control2X: 0.040 y: 0.022;
		rel_LineToX: 0.033 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics124
	vectorGraphicsEngine
		rel_MoveToX: 0.209 y: 0.192;
		rel_LineToX: -0.200 y: 1.000;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.200 y: -1.000;
		rel_LineToX: 0.083 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics125
	vectorGraphicsEngine
		rel_MoveToX: -0.047 y: 1.049;
		rel_LineToX: 0.034 y: 0.000;
		rel_CubicBezierToX: 0.093 y: -0.021 control1X: 0.046 y: 0.000 control2X: 0.077 y: -0.007;
		rel_CubicBezierToX: 0.039 y: -0.091 control1X: 0.017 y: -0.014 control2X: 0.030 y: -0.044;
		rel_LineToX: 0.024 y: -0.121;
		rel_CubicBezierToX: 0.044 y: -0.111 control1X: 0.010 y: -0.051 control2X: 0.025 y: -0.088;
		rel_CubicBezierToX: 0.086 y: -0.048 control1X: 0.019 y: -0.023 control2X: 0.048 y: -0.039;
		rel_CubicBezierToX: -0.067 y: -0.047 control1X: -0.035 y: -0.008 control2X: -0.057 y: -0.024;
		rel_CubicBezierToX: 0.000 y: -0.111 control1X: -0.010 y: -0.023 control2X: -0.010 y: -0.060;
		rel_LineToX: 0.024 y: -0.121;
		rel_CubicBezierToX: -0.003 y: -0.090 control1X: 0.009 y: -0.046 control2X: 0.008 y: -0.076;
		rel_CubicBezierToX: -0.085 y: -0.022 control1X: -0.011 y: -0.014 control2X: -0.039 y: -0.022;
		rel_LineToX: -0.034 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.031 y: 0.000;
		rel_CubicBezierToX: 0.155 y: 0.036 control1X: 0.081 y: 0.000 control2X: 0.133 y: 0.012;
		rel_CubicBezierToX: 0.012 y: 0.144 control1X: 0.023 y: 0.024 control2X: 0.027 y: 0.072;
		rel_LineToX: -0.023 y: 0.117;
		rel_CubicBezierToX: 0.006 y: 0.101 control1X: -0.010 y: 0.048 control2X: -0.008 y: 0.082;
		rel_CubicBezierToX: 0.090 y: 0.028 control1X: 0.014 y: 0.018 control2X: 0.044 y: 0.028;
		rel_LineToX: 0.030 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.030 y: 0.000;
		rel_CubicBezierToX: -0.101 y: 0.028 control1X: -0.046 y: 0.000 control2X: -0.080 y: 0.009;
		rel_CubicBezierToX: -0.047 y: 0.102 control1X: -0.021 y: 0.019 control2X: -0.037 y: 0.053;
		rel_LineToX: -0.023 y: 0.117;
		rel_CubicBezierToX: -0.070 y: 0.144 control1X: -0.014 y: 0.072 control2X: -0.038 y: 0.120;
		rel_CubicBezierToX: -0.169 y: 0.036 control1X: -0.032 y: 0.024 control2X: -0.088 y: 0.036;
		rel_LineToX: -0.031 y: 0.000;
		rel_LineToX: 0.014 y: -0.070! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics126
	vectorGraphicsEngine
		rel_MoveToX: 0.658 y: 0.557;
		rel_LineToX: -0.017 y: 0.087;
		rel_CubicBezierToX: -0.106 y: 0.055 control1X: -0.039 y: 0.026 control2X: -0.075 y: 0.044;
		rel_CubicBezierToX: -0.094 y: 0.017 control1X: -0.031 y: 0.011 control2X: -0.063 y: 0.017;
		rel_CubicBezierToX: -0.119 y: -0.029 control1X: -0.036 y: 0.000 control2X: -0.076 y: -0.010;
		rel_CubicBezierToX: -0.007 y: -0.003 control1X: -0.003 y: -0.001 control2X: -0.006 y: -0.002;
		rel_CubicBezierToX: -0.010 y: -0.004 control1X: -0.002 y: -0.001 control2X: -0.005 y: -0.002;
		rel_CubicBezierToX: -0.116 y: -0.030 control1X: -0.046 y: -0.020 control2X: -0.085 y: -0.030;
		rel_CubicBezierToX: -0.089 y: 0.019 control1X: -0.029 y: 0.000 control2X: -0.058 y: 0.006;
		rel_CubicBezierToX: -0.102 y: 0.059 control1X: -0.030 y: 0.012 control2X: -0.064 y: 0.032;
		rel_LineToX: 0.017 y: -0.087;
		rel_CubicBezierToX: 0.106 y: -0.055 control1X: 0.039 y: -0.026 control2X: 0.075 y: -0.044;
		rel_CubicBezierToX: 0.095 y: -0.017 control1X: 0.031 y: -0.011 control2X: 0.063 y: -0.017;
		rel_CubicBezierToX: 0.120 y: 0.029 control1X: 0.036 y: 0.000 control2X: 0.076 y: 0.010;
		rel_CubicBezierToX: 0.007 y: 0.003 control1X: 0.003 y: 0.001 control2X: 0.005 y: 0.002;
		rel_CubicBezierToX: 0.010 y: 0.004 control1X: 0.002 y: 0.001 control2X: 0.006 y: 0.002;
		rel_CubicBezierToX: 0.116 y: 0.030 control1X: 0.046 y: 0.020 control2X: 0.085 y: 0.030;
		rel_CubicBezierToX: 0.087 y: -0.019 control1X: 0.028 y: 0.000 control2X: 0.057 y: -0.006;
		rel_CubicBezierToX: 0.104 y: -0.060 control1X: 0.030 y: -0.012 control2X: 0.065 y: -0.032! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics128
	vectorGraphicsEngine
		rel_MoveToX: 0.459 y: 0.227;
		rel_LineToX: 0.211 y: 0.272;
		rel_LineToX: -0.098 y: 0.000;
		rel_LineToX: -0.176 y: -0.193;
		rel_LineToX: -0.253 y: 0.193;
		rel_LineToX: -0.098 y: 0.000;
		rel_LineToX: 0.320 y: -0.272;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics129
	vectorGraphicsEngine
		rel_MoveToX: 0.323 y: 1.122;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.520 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.520 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics130
	vectorGraphicsEngine
		rel_MoveToX: 0.390 y: 0.224;
		rel_LineToX: 0.044 y: 0.000;
		rel_LineToX: 0.153 y: 0.192;
		rel_LineToX: -0.070 y: 0.059;
		rel_LineToX: -0.090 y: -0.114;
		rel_LineToX: -0.119 y: 0.596;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.119 y: -0.596;
		rel_LineToX: -0.137 y: 0.114;
		rel_LineToX: -0.047 y: -0.059;
		rel_LineToX: 0.231 y: -0.192! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics131
	vectorGraphicsEngine
		rel_MoveToX: -0.046 y: 0.665;
		rel_LineToX: 0.009 y: -0.044;
		rel_LineToX: 0.230 y: -0.192;
		rel_LineToX: 0.047 y: 0.059;
		rel_LineToX: -0.137 y: 0.114;
		rel_LineToX: 0.595 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.595 y: 0.000;
		rel_LineToX: 0.091 y: 0.114;
		rel_LineToX: -0.070 y: 0.059;
		rel_LineToX: -0.154 y: -0.192! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics161
	vectorGraphicsEngine
		rel_MoveToX: 0.218 y: 0.351;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_MoveToX: -0.121 y: 0.605;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.064 y: -0.320;
		rel_LineToX: 0.045 y: -0.174;
		rel_LineToX: 0.079 y: 0.000;
		rel_LineToX: -0.025 y: 0.174;
		rel_LineToX: -0.064 y: 0.320! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics162
	vectorGraphicsEngine
		rel_MoveToX: 0.190 y: 0.892;
		rel_LineToX: 0.084 y: -0.418;
		rel_CubicBezierToX: -0.125 y: 0.064 control1X: -0.049 y: 0.006 control2X: -0.091 y: 0.027;
		rel_CubicBezierToX: -0.069 y: 0.145 control1X: -0.034 y: 0.037 control2X: -0.057 y: 0.085;
		rel_CubicBezierToX: 0.011 y: 0.145 control1X: -0.012 y: 0.060 control2X: -0.008 y: 0.109;
		rel_CubicBezierToX: 0.099 y: 0.064 control1X: 0.019 y: 0.037 control2X: 0.052 y: 0.058;
		rel_MoveToX: 0.278 y: -0.462;
		rel_LineToX: -0.017 y: 0.084;
		rel_CubicBezierToX: -0.063 y: -0.030 control1X: -0.021 y: -0.013 control2X: -0.042 y: -0.024;
		rel_CubicBezierToX: -0.064 y: -0.012 control1X: -0.021 y: -0.007 control2X: -0.042 y: -0.011;
		rel_LineToX: -0.085 y: 0.422;
		rel_CubicBezierToX: 0.070 y: -0.013 control1X: 0.023 y: -0.002 control2X: 0.046 y: -0.006;
		rel_CubicBezierToX: 0.075 y: -0.029 control1X: 0.024 y: -0.007 control2X: 0.049 y: -0.017;
		rel_LineToX: -0.017 y: 0.083;
		rel_CubicBezierToX: -0.070 y: 0.023 control1X: -0.023 y: 0.010 control2X: -0.046 y: 0.017;
		rel_CubicBezierToX: -0.073 y: 0.012 control1X: -0.023 y: 0.006 control2X: -0.048 y: 0.010;
		rel_LineToX: -0.028 y: 0.139;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.028 y: -0.141;
		rel_CubicBezierToX: -0.166 y: -0.085 control1X: -0.077 y: -0.006 control2X: -0.132 y: -0.035;
		rel_CubicBezierToX: -0.024 y: -0.201 control1X: -0.033 y: -0.050 control2X: -0.041 y: -0.117;
		rel_CubicBezierToX: 0.105 y: -0.201 control1X: 0.017 y: -0.084 control2X: 0.052 y: -0.151;
		rel_CubicBezierToX: 0.200 y: -0.085 control1X: 0.053 y: -0.050 control2X: 0.120 y: -0.078;
		rel_LineToX: 0.028 y: -0.140;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.028 y: 0.140;
		rel_CubicBezierToX: 0.067 y: 0.010 control1X: 0.024 y: 0.001 control2X: 0.046 y: 0.005;
		rel_CubicBezierToX: 0.062 y: 0.023 control1X: 0.021 y: 0.005 control2X: 0.042 y: 0.013! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics163
	vectorGraphicsEngine
		rel_MoveToX: 0.527 y: 0.243;
		rel_LineToX: -0.018 y: 0.089;
		rel_CubicBezierToX: -0.064 y: -0.030 control1X: -0.022 y: -0.013 control2X: -0.044 y: -0.023;
		rel_CubicBezierToX: -0.061 y: -0.010 control1X: -0.021 y: -0.007 control2X: -0.041 y: -0.010;
		rel_CubicBezierToX: -0.108 y: 0.038 control1X: -0.048 y: 0.000 control2X: -0.084 y: 0.013;
		rel_CubicBezierToX: -0.057 y: 0.141 control1X: -0.024 y: 0.025 control2X: -0.043 y: 0.072;
		rel_LineToX: -0.021 y: 0.105;
		rel_LineToX: 0.191 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.191 y: 0.000;
		rel_LineToX: -0.045 y: 0.227;
		rel_LineToX: 0.272 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.485 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.115 y: 0.000;
		rel_LineToX: 0.045 y: -0.227;
		rel_LineToX: -0.097 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: 0.023 y: -0.116;
		rel_CubicBezierToX: 0.090 y: -0.187 control1X: 0.017 y: -0.085 control2X: 0.047 y: -0.147;
		rel_CubicBezierToX: 0.178 y: -0.060 control1X: 0.043 y: -0.040 control2X: 0.102 y: -0.060;
		rel_CubicBezierToX: 0.065 y: 0.008 control1X: 0.020 y: 0.000 control2X: 0.041 y: 0.003;
		rel_CubicBezierToX: 0.071 y: 0.021 control1X: 0.024 y: 0.005 control2X: 0.047 y: 0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics164
	vectorGraphicsEngine
		rel_MoveToX: 0.551 y: 0.283;
		rel_LineToX: -0.021 y: 0.104;
		rel_CubicBezierToX: -0.071 y: -0.073 control1X: -0.023 y: -0.034 control2X: -0.047 y: -0.059;
		rel_CubicBezierToX: -0.086 y: -0.022 control1X: -0.024 y: -0.015 control2X: -0.053 y: -0.022;
		rel_CubicBezierToX: -0.136 y: 0.049 control1X: -0.051 y: 0.000 control2X: -0.096 y: 0.016;
		rel_CubicBezierToX: -0.093 y: 0.140 control1X: -0.039 y: 0.033 control2X: -0.070 y: 0.079;
		rel_LineToX: 0.282 y: 0.000;
		rel_LineToX: -0.039 y: 0.060;
		rel_LineToX: -0.263 y: 0.000;
		rel_CubicBezierToX: -0.006 y: 0.023 control1X: -0.002 y: 0.008 control2X: -0.004 y: 0.015;
		rel_CubicBezierToX: -0.006 y: 0.028 control1X: -0.002 y: 0.007 control2X: -0.003 y: 0.017;
		rel_CubicBezierToX: -0.005 y: 0.027 control1X: -0.002 y: 0.010 control2X: -0.004 y: 0.019;
		rel_CubicBezierToX: -0.003 y: 0.023 control1X: -0.001 y: 0.007 control2X: -0.002 y: 0.015;
		rel_LineToX: 0.218 y: 0.000;
		rel_LineToX: -0.039 y: 0.060;
		rel_LineToX: -0.183 y: 0.000;
		rel_CubicBezierToX: 0.037 y: 0.141 control1X: -0.002 y: 0.061 control2X: 0.011 y: 0.108;
		rel_CubicBezierToX: 0.116 y: 0.049 control1X: 0.026 y: 0.033 control2X: 0.065 y: 0.049;
		rel_CubicBezierToX: 0.095 y: -0.022 control1X: 0.033 y: 0.000 control2X: 0.065 y: -0.007;
		rel_CubicBezierToX: 0.101 y: -0.073 control1X: 0.031 y: -0.015 control2X: 0.064 y: -0.039;
		rel_LineToX: -0.021 y: 0.103;
		rel_CubicBezierToX: -0.099 y: 0.053 control1X: -0.034 y: 0.023 control2X: -0.067 y: 0.041;
		rel_CubicBezierToX: -0.095 y: 0.017 control1X: -0.032 y: 0.012 control2X: -0.064 y: 0.017;
		rel_CubicBezierToX: -0.177 y: -0.071 control1X: -0.077 y: 0.000 control2X: -0.136 y: -0.024;
		rel_CubicBezierToX: -0.055 y: -0.197 control1X: -0.041 y: -0.047 control2X: -0.059 y: -0.113;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.039 y: -0.060;
		rel_LineToX: 0.068 y: 0.000;
		rel_CubicBezierToX: 0.004 y: -0.022 control1X: 0.002 y: -0.007 control2X: 0.003 y: -0.015;
		rel_CubicBezierToX: 0.005 y: -0.027 control1X: 0.002 y: -0.008 control2X: 0.003 y: -0.017;
		rel_CubicBezierToX: 0.006 y: -0.028 control1X: 0.002 y: -0.011 control2X: 0.004 y: -0.021;
		rel_CubicBezierToX: 0.005 y: -0.023 control1X: 0.002 y: -0.008 control2X: 0.004 y: -0.015;
		rel_LineToX: -0.095 y: 0.000;
		rel_LineToX: 0.039 y: -0.060;
		rel_LineToX: 0.076 y: 0.000;
		rel_CubicBezierToX: 0.134 y: -0.196 control1X: 0.030 y: -0.084 control2X: 0.075 y: -0.149;
		rel_CubicBezierToX: 0.205 y: -0.071 control1X: 0.060 y: -0.047 control2X: 0.128 y: -0.071;
		rel_CubicBezierToX: 0.089 y: 0.018 control1X: 0.032 y: 0.000 control2X: 0.062 y: 0.006;
		rel_CubicBezierToX: 0.078 y: 0.052 control1X: 0.028 y: 0.011 control2X: 0.054 y: 0.029! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics165
	vectorGraphicsEngine
		rel_MoveToX: 0.460 y: 0.734;
		rel_LineToX: -0.202 y: 0.000;
		rel_LineToX: -0.044 y: 0.222;
		rel_LineToX: -0.098 y: 0.000;
		rel_LineToX: 0.044 y: -0.222;
		rel_LineToX: -0.203 y: 0.000;
		rel_LineToX: 0.012 y: -0.060;
		rel_LineToX: 0.203 y: 0.000;
		rel_LineToX: 0.005 y: -0.025;
		rel_LineToX: -0.026 y: -0.076;
		rel_LineToX: -0.162 y: 0.000;
		rel_LineToX: 0.012 y: -0.060;
		rel_LineToX: 0.129 y: 0.000;
		rel_LineToX: -0.098 y: -0.286;
		rel_LineToX: 0.093 y: 0.000;
		rel_LineToX: 0.117 y: 0.342;
		rel_LineToX: 0.252 y: -0.342;
		rel_LineToX: 0.093 y: 0.000;
		rel_LineToX: -0.213 y: 0.286;
		rel_LineToX: 0.129 y: 0.000;
		rel_LineToX: -0.012 y: 0.060;
		rel_LineToX: -0.162 y: 0.000;
		rel_LineToX: -0.056 y: 0.076;
		rel_LineToX: -0.005 y: 0.025;
		rel_LineToX: 0.203 y: 0.000;
		rel_LineToX: -0.012 y: 0.060! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics166
	vectorGraphicsEngine
		rel_MoveToX: 0.523 y: 0.251;
		rel_LineToX: -0.019 y: 0.096;
		rel_CubicBezierToX: -0.098 y: -0.040 control1X: -0.034 y: -0.018 control2X: -0.066 y: -0.031;
		rel_CubicBezierToX: -0.094 y: -0.013 control1X: -0.031 y: -0.009 control2X: -0.063 y: -0.013;
		rel_CubicBezierToX: -0.131 y: 0.031 control1X: -0.054 y: 0.000 control2X: -0.097 y: 0.010;
		rel_CubicBezierToX: -0.061 y: 0.089 control1X: -0.033 y: 0.021 control2X: -0.054 y: 0.050;
		rel_CubicBezierToX: 0.014 y: 0.073 control1X: -0.006 y: 0.032 control2X: -0.002 y: 0.057;
		rel_CubicBezierToX: 0.102 y: 0.040 control1X: 0.016 y: 0.016 control2X: 0.050 y: 0.030;
		rel_LineToX: 0.057 y: 0.012;
		rel_CubicBezierToX: 0.148 y: 0.074 control1X: 0.071 y: 0.014 control2X: 0.120 y: 0.039;
		rel_CubicBezierToX: 0.024 y: 0.142 control1X: 0.028 y: 0.035 control2X: 0.036 y: 0.082;
		rel_CubicBezierToX: -0.103 y: 0.161 control1X: -0.014 y: 0.071 control2X: -0.049 y: 0.124;
		rel_CubicBezierToX: -0.219 y: 0.055 control1X: -0.054 y: 0.036 control2X: -0.128 y: 0.055;
		rel_CubicBezierToX: -0.108 y: -0.012 control1X: -0.034 y: 0.000 control2X: -0.071 y: -0.004;
		rel_CubicBezierToX: -0.114 y: -0.035 control1X: -0.037 y: -0.008 control2X: -0.075 y: -0.019;
		rel_LineToX: 0.020 y: -0.102;
		rel_CubicBezierToX: 0.108 y: 0.051 control1X: 0.036 y: 0.022 control2X: 0.071 y: 0.039;
		rel_CubicBezierToX: 0.110 y: 0.017 control1X: 0.036 y: 0.011 control2X: 0.073 y: 0.017;
		rel_CubicBezierToX: 0.137 y: -0.033 control1X: 0.056 y: 0.000 control2X: 0.102 y: -0.011;
		rel_CubicBezierToX: 0.065 y: -0.095 control1X: 0.035 y: -0.022 control2X: 0.057 y: -0.054;
		rel_CubicBezierToX: -0.016 y: -0.084 control1X: 0.007 y: -0.036 control2X: 0.002 y: -0.064;
		rel_CubicBezierToX: -0.099 y: -0.045 control1X: -0.018 y: -0.020 control2X: -0.051 y: -0.035;
		rel_LineToX: -0.058 y: -0.012;
		rel_CubicBezierToX: -0.146 y: -0.069 control1X: -0.071 y: -0.015 control2X: -0.119 y: -0.038;
		rel_CubicBezierToX: -0.023 y: -0.130 control1X: -0.027 y: -0.031 control2X: -0.034 y: -0.075;
		rel_CubicBezierToX: 0.098 y: -0.152 control1X: 0.013 y: -0.065 control2X: 0.046 y: -0.115;
		rel_CubicBezierToX: 0.199 y: -0.056 control1X: 0.053 y: -0.037 control2X: 0.119 y: -0.056;
		rel_CubicBezierToX: 0.103 y: 0.009 control1X: 0.034 y: 0.000 control2X: 0.068 y: 0.003;
		rel_CubicBezierToX: 0.103 y: 0.028 control1X: 0.034 y: 0.006 control2X: 0.069 y: 0.015;
		rel_MoveToX: -0.245 y: -0.093;
		rel_LineToX: -0.077 y: -0.130;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: 0.064 y: 0.087;
		rel_LineToX: 0.098 y: -0.087;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: -0.129 y: 0.130;
		rel_LineToX: -0.092 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics167
	vectorGraphicsEngine
		rel_MoveToX: 0.123 y: 0.499;
		rel_CubicBezierToX: -0.055 y: 0.044 control1X: -0.023 y: 0.015 control2X: -0.042 y: 0.030;
		rel_CubicBezierToX: -0.024 y: 0.044 control1X: -0.013 y: 0.015 control2X: -0.021 y: 0.029;
		rel_CubicBezierToX: 0.020 y: 0.070 control1X: -0.005 y: 0.025 control2X: 0.002 y: 0.048;
		rel_CubicBezierToX: 0.137 y: 0.098 control1X: 0.018 y: 0.021 control2X: 0.064 y: 0.054;
		rel_CubicBezierToX: 0.055 y: -0.044 control1X: 0.023 y: -0.015 control2X: 0.042 y: -0.029;
		rel_CubicBezierToX: 0.024 y: -0.045 control1X: 0.013 y: -0.015 control2X: 0.021 y: -0.030;
		rel_CubicBezierToX: -0.021 y: -0.070 control1X: 0.005 y: -0.024 control2X: -0.002 y: -0.048;
		rel_CubicBezierToX: -0.135 y: -0.098 control1X: -0.019 y: -0.022 control2X: -0.064 y: -0.055;
		rel_MoveToX: 0.271 y: -0.257;
		rel_LineToX: -0.016 y: 0.080;
		rel_CubicBezierToX: -0.070 y: -0.028 control1X: -0.025 y: -0.013 control2X: -0.048 y: -0.022;
		rel_CubicBezierToX: -0.060 y: -0.010 control1X: -0.022 y: -0.007 control2X: -0.042 y: -0.010;
		rel_CubicBezierToX: -0.078 y: 0.020 control1X: -0.032 y: 0.000 control2X: -0.058 y: 0.007;
		rel_CubicBezierToX: -0.037 y: 0.054 control1X: -0.020 y: 0.013 control2X: -0.033 y: 0.031;
		rel_CubicBezierToX: 0.080 y: 0.100 control1X: -0.006 y: 0.029 control2X: 0.021 y: 0.063;
		rel_CubicBezierToX: 0.017 y: 0.011 control1X: 0.008 y: 0.005 control2X: 0.013 y: 0.009;
		rel_CubicBezierToX: 0.116 y: 0.096 control1X: 0.061 y: 0.039 control2X: 0.099 y: 0.071;
		rel_CubicBezierToX: 0.015 y: 0.087 control1X: 0.016 y: 0.025 control2X: 0.021 y: 0.054;
		rel_CubicBezierToX: -0.038 y: 0.078 control1X: -0.006 y: 0.029 control2X: -0.019 y: 0.055;
		rel_CubicBezierToX: -0.080 y: 0.060 control1X: -0.020 y: 0.023 control2X: -0.046 y: 0.043;
		rel_CubicBezierToX: 0.034 y: 0.052 control1X: 0.017 y: 0.017 control2X: 0.028 y: 0.034;
		rel_CubicBezierToX: 0.003 y: 0.058 control1X: 0.006 y: 0.018 control2X: 0.007 y: 0.037;
		rel_CubicBezierToX: -0.072 y: 0.110 control1X: -0.009 y: 0.046 control2X: -0.033 y: 0.083;
		rel_CubicBezierToX: -0.142 y: 0.041 control1X: -0.039 y: 0.027 control2X: -0.086 y: 0.041;
		rel_CubicBezierToX: -0.072 y: -0.007 control1X: -0.024 y: 0.000 control2X: -0.048 y: -0.002;
		rel_CubicBezierToX: -0.076 y: -0.021 control1X: -0.024 y: -0.005 control2X: -0.050 y: -0.012;
		rel_LineToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.073 y: 0.028 control1X: 0.026 y: 0.013 control2X: 0.050 y: 0.022;
		rel_CubicBezierToX: 0.062 y: 0.010 control1X: 0.023 y: 0.006 control2X: 0.044 y: 0.010;
		rel_CubicBezierToX: 0.081 y: -0.020 control1X: 0.033 y: 0.000 control2X: 0.060 y: -0.007;
		rel_CubicBezierToX: 0.039 y: -0.058 control1X: 0.021 y: -0.014 control2X: 0.034 y: -0.033;
		rel_CubicBezierToX: -0.086 y: -0.110 control1X: 0.007 y: -0.033 control2X: -0.022 y: -0.070;
		rel_LineToX: -0.011 y: -0.007;
		rel_CubicBezierToX: -0.117 y: -0.096 control1X: -0.062 y: -0.039 control2X: -0.101 y: -0.071;
		rel_CubicBezierToX: -0.014 y: -0.087 control1X: -0.016 y: -0.025 control2X: -0.020 y: -0.054;
		rel_CubicBezierToX: 0.038 y: -0.079 control1X: 0.006 y: -0.030 control2X: 0.019 y: -0.056;
		rel_CubicBezierToX: 0.079 y: -0.059 control1X: 0.020 y: -0.023 control2X: 0.046 y: -0.043;
		rel_CubicBezierToX: -0.034 y: -0.049 control1X: -0.016 y: -0.014 control2X: -0.028 y: -0.031;
		rel_CubicBezierToX: -0.002 y: -0.061 control1X: -0.006 y: -0.018 control2X: -0.006 y: -0.038;
		rel_CubicBezierToX: 0.069 y: -0.102 control1X: 0.008 y: -0.042 control2X: 0.032 y: -0.076;
		rel_CubicBezierToX: 0.138 y: -0.039 control1X: 0.038 y: -0.026 control2X: 0.084 y: -0.039;
		rel_CubicBezierToX: 0.071 y: 0.007 control1X: 0.024 y: 0.000 control2X: 0.048 y: 0.002;
		rel_CubicBezierToX: 0.072 y: 0.021 control1X: 0.024 y: 0.005 control2X: 0.048 y: 0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics168
	vectorGraphicsEngine
		rel_MoveToX: 0.396 y: 0.425;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.073 y: -0.029 control1X: -0.023 y: -0.013 control2X: -0.047 y: -0.023;
		rel_CubicBezierToX: -0.083 y: -0.010 control1X: -0.026 y: -0.007 control2X: -0.054 y: -0.010;
		rel_CubicBezierToX: -0.105 y: 0.021 control1X: -0.045 y: 0.000 control2X: -0.079 y: 0.007;
		rel_CubicBezierToX: -0.046 y: 0.062 control1X: -0.025 y: 0.014 control2X: -0.040 y: 0.034;
		rel_CubicBezierToX: 0.014 y: 0.049 control1X: -0.004 y: 0.021 control2X: 0.001 y: 0.037;
		rel_CubicBezierToX: 0.089 y: 0.034 control1X: 0.014 y: 0.012 control2X: 0.043 y: 0.023;
		rel_LineToX: 0.029 y: 0.007;
		rel_CubicBezierToX: 0.124 y: 0.058 control1X: 0.061 y: 0.014 control2X: 0.102 y: 0.033;
		rel_CubicBezierToX: 0.020 y: 0.104 control1X: 0.022 y: 0.025 control2X: 0.029 y: 0.059;
		rel_CubicBezierToX: -0.085 y: 0.121 control1X: -0.010 y: 0.051 control2X: -0.038 y: 0.091;
		rel_CubicBezierToX: -0.174 y: 0.044 control1X: -0.046 y: 0.030 control2X: -0.104 y: 0.044;
		rel_CubicBezierToX: -0.090 y: -0.009 control1X: -0.029 y: 0.000 control2X: -0.059 y: -0.003;
		rel_CubicBezierToX: -0.095 y: -0.025 control1X: -0.030 y: -0.006 control2X: -0.062 y: -0.014;
		rel_LineToX: 0.019 y: -0.093;
		rel_CubicBezierToX: 0.090 y: 0.039 control1X: 0.030 y: 0.017 control2X: 0.060 y: 0.030;
		rel_CubicBezierToX: 0.093 y: 0.013 control1X: 0.031 y: 0.008 control2X: 0.062 y: 0.013;
		rel_CubicBezierToX: 0.102 y: -0.021 control1X: 0.042 y: 0.000 control2X: 0.076 y: -0.007;
		rel_CubicBezierToX: 0.046 y: -0.062 control1X: 0.026 y: -0.015 control2X: 0.041 y: -0.035;
		rel_CubicBezierToX: -0.014 y: -0.056 control1X: 0.005 y: -0.024 control2X: 0.000 y: -0.043;
		rel_CubicBezierToX: -0.100 y: -0.038 control1X: -0.014 y: -0.013 control2X: -0.047 y: -0.026;
		rel_LineToX: -0.030 y: -0.007;
		rel_CubicBezierToX: -0.110 y: -0.054 control1X: -0.053 y: -0.012 control2X: -0.090 y: -0.030;
		rel_CubicBezierToX: -0.017 y: -0.100 control1X: -0.020 y: -0.024 control2X: -0.026 y: -0.058;
		rel_CubicBezierToX: 0.079 y: -0.119 control1X: 0.010 y: -0.051 control2X: 0.036 y: -0.091;
		rel_CubicBezierToX: 0.164 y: -0.042 control1X: 0.042 y: -0.028 control2X: 0.097 y: -0.042;
		rel_CubicBezierToX: 0.092 y: 0.007 control1X: 0.033 y: 0.000 control2X: 0.064 y: 0.003;
		rel_CubicBezierToX: 0.077 y: 0.022 control1X: 0.028 y: 0.005 control2X: 0.054 y: 0.012;
		rel_MoveToX: -0.201 y: -0.085;
		rel_LineToX: -0.083 y: -0.184;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: 0.064 y: 0.120;
		rel_LineToX: 0.112 y: -0.120;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: -0.156 y: 0.184;
		rel_LineToX: -0.072 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics169
	vectorGraphicsEngine
		rel_MoveToX: 0.492 y: 0.231;
		rel_CubicBezierToX: 0.133 y: 0.027 control1X: 0.049 y: 0.000 control2X: 0.094 y: 0.009;
		rel_CubicBezierToX: 0.102 y: 0.080 control1X: 0.039 y: 0.018 control2X: 0.073 y: 0.045;
		rel_CubicBezierToX: 0.056 y: 0.117 control1X: 0.028 y: 0.035 control2X: 0.047 y: 0.075;
		rel_CubicBezierToX: -0.001 y: 0.139 control1X: 0.009 y: 0.043 control2X: 0.009 y: 0.089;
		rel_CubicBezierToX: -0.054 y: 0.138 control1X: -0.010 y: 0.049 control2X: -0.028 y: 0.096;
		rel_CubicBezierToX: -0.103 y: 0.117 control1X: -0.026 y: 0.042 control2X: -0.060 y: 0.081;
		rel_CubicBezierToX: -0.134 y: 0.080 control1X: -0.043 y: 0.035 control2X: -0.087 y: 0.062;
		rel_CubicBezierToX: -0.144 y: 0.027 control1X: -0.046 y: 0.018 control2X: -0.094 y: 0.027;
		rel_CubicBezierToX: -0.133 y: -0.027 control1X: -0.049 y: 0.000 control2X: -0.094 y: -0.009;
		rel_CubicBezierToX: -0.101 y: -0.080 control1X: -0.039 y: -0.018 control2X: -0.073 y: -0.045;
		rel_CubicBezierToX: -0.056 y: -0.117 control1X: -0.028 y: -0.035 control2X: -0.047 y: -0.074;
		rel_CubicBezierToX: 0.001 y: -0.138 control1X: -0.009 y: -0.043 control2X: -0.009 y: -0.089;
		rel_CubicBezierToX: 0.054 y: -0.139 control1X: 0.010 y: -0.050 control2X: 0.028 y: -0.097;
		rel_CubicBezierToX: 0.103 y: -0.117 control1X: 0.026 y: -0.043 control2X: 0.060 y: -0.082;
		rel_CubicBezierToX: 0.133 y: -0.080 control1X: 0.043 y: -0.035 control2X: 0.087 y: -0.062;
		rel_CubicBezierToX: 0.144 y: -0.027 control1X: 0.047 y: -0.018 control2X: 0.095 y: -0.027;
		rel_MoveToX: -0.010 y: 0.050;
		rel_CubicBezierToX: -0.124 y: 0.023 control1X: -0.043 y: 0.000 control2X: -0.084 y: 0.008;
		rel_CubicBezierToX: -0.115 y: 0.069 control1X: -0.040 y: 0.015 control2X: -0.078 y: 0.038;
		rel_CubicBezierToX: -0.090 y: 0.102 control1X: -0.037 y: 0.031 control2X: -0.067 y: 0.065;
		rel_CubicBezierToX: -0.047 y: 0.119 control1X: -0.023 y: 0.037 control2X: -0.039 y: 0.077;
		rel_CubicBezierToX: 0.000 y: 0.119 control1X: -0.008 y: 0.042 control2X: -0.009 y: 0.082;
		rel_CubicBezierToX: 0.049 y: 0.101 control1X: 0.008 y: 0.037 control2X: 0.025 y: 0.071;
		rel_CubicBezierToX: 0.087 y: 0.069 control1X: 0.024 y: 0.031 control2X: 0.053 y: 0.054;
		rel_CubicBezierToX: 0.115 y: 0.023 control1X: 0.034 y: 0.015 control2X: 0.072 y: 0.023;
		rel_CubicBezierToX: 0.124 y: -0.023 control1X: 0.043 y: 0.000 control2X: 0.084 y: -0.008;
		rel_CubicBezierToX: 0.115 y: -0.069 control1X: 0.040 y: -0.016 control2X: 0.079 y: -0.039;
		rel_CubicBezierToX: 0.088 y: -0.101 control1X: 0.036 y: -0.030 control2X: 0.066 y: -0.064;
		rel_CubicBezierToX: 0.047 y: -0.119 control1X: 0.023 y: -0.037 control2X: 0.038 y: -0.077;
		rel_CubicBezierToX: 0.001 y: -0.120 control1X: 0.009 y: -0.043 control2X: 0.009 y: -0.083;
		rel_CubicBezierToX: -0.048 y: -0.101 control1X: -0.008 y: -0.037 control2X: -0.024 y: -0.071;
		rel_CubicBezierToX: -0.088 y: -0.069 control1X: -0.024 y: -0.031 control2X: -0.054 y: -0.054;
		rel_CubicBezierToX: -0.115 y: -0.023 control1X: -0.034 y: -0.015 control2X: -0.072 y: -0.023;
		rel_MoveToX: 0.122 y: 0.120;
		rel_LineToX: -0.013 y: 0.063;
		rel_CubicBezierToX: -0.059 y: -0.024 control1X: -0.019 y: -0.011 control2X: -0.039 y: -0.019;
		rel_CubicBezierToX: -0.062 y: -0.008 control1X: -0.020 y: -0.005 control2X: -0.041 y: -0.008;
		rel_CubicBezierToX: -0.122 y: 0.043 control1X: -0.048 y: 0.000 control2X: -0.089 y: 0.014;
		rel_CubicBezierToX: -0.064 y: 0.119 control1X: -0.033 y: 0.028 control2X: -0.054 y: 0.068;
		rel_CubicBezierToX: 0.017 y: 0.120 control1X: -0.010 y: 0.052 control2X: -0.004 y: 0.092;
		rel_CubicBezierToX: 0.109 y: 0.042 control1X: 0.022 y: 0.028 control2X: 0.059 y: 0.042;
		rel_CubicBezierToX: 0.063 y: -0.007 control1X: 0.021 y: 0.000 control2X: 0.042 y: -0.002;
		rel_CubicBezierToX: 0.067 y: -0.024 control1X: 0.021 y: -0.005 control2X: 0.043 y: -0.013;
		rel_LineToX: -0.012 y: 0.062;
		rel_CubicBezierToX: -0.071 y: 0.021 control1X: -0.024 y: 0.009 control2X: -0.047 y: 0.016;
		rel_CubicBezierToX: -0.069 y: 0.007 control1X: -0.023 y: 0.005 control2X: -0.046 y: 0.007;
		rel_CubicBezierToX: -0.155 y: -0.060 control1X: -0.070 y: 0.000 control2X: -0.122 y: -0.020;
		rel_CubicBezierToX: -0.029 y: -0.161 control1X: -0.033 y: -0.040 control2X: -0.042 y: -0.093;
		rel_CubicBezierToX: 0.093 y: -0.161 control1X: 0.014 y: -0.068 control2X: 0.045 y: -0.122;
		rel_CubicBezierToX: 0.179 y: -0.059 control1X: 0.049 y: -0.039 control2X: 0.109 y: -0.059;
		rel_CubicBezierToX: 0.067 y: 0.007 control1X: 0.024 y: 0.000 control2X: 0.046 y: 0.002;
		rel_CubicBezierToX: 0.061 y: 0.020 control1X: 0.021 y: 0.005 control2X: 0.042 y: 0.011! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics170
	vectorGraphicsEngine
		rel_MoveToX: -0.028 y: 0.667;
		rel_LineToX: 0.336 y: 0.000;
		rel_LineToX: -0.012 y: 0.060;
		rel_LineToX: -0.336 y: 0.000;
		rel_LineToX: 0.012 y: -0.060;
		rel_MoveToX: 0.391 y: -0.280;
		rel_LineToX: -0.044 y: 0.219;
		rel_LineToX: -0.073 y: 0.000;
		rel_LineToX: 0.011 y: -0.055;
		rel_CubicBezierToX: -0.066 y: 0.047 control1X: -0.018 y: 0.020 control2X: -0.040 y: 0.036;
		rel_CubicBezierToX: -0.084 y: 0.017 control1X: -0.026 y: 0.011 control2X: -0.054 y: 0.017;
		rel_CubicBezierToX: -0.093 y: -0.033 control1X: -0.042 y: 0.000 control2X: -0.073 y: -0.011;
		rel_CubicBezierToX: -0.019 y: -0.088 control1X: -0.020 y: -0.022 control2X: -0.027 y: -0.051;
		rel_CubicBezierToX: 0.066 y: -0.100 control1X: 0.009 y: -0.044 control2X: 0.031 y: -0.077;
		rel_CubicBezierToX: 0.145 y: -0.034 control1X: 0.036 y: -0.023 control2X: 0.084 y: -0.034;
		rel_LineToX: 0.089 y: 0.000;
		rel_LineToX: 0.000 y: -0.002;
		rel_CubicBezierToX: -0.016 y: -0.066 control1X: 0.006 y: -0.030 control2X: 0.001 y: -0.052;
		rel_CubicBezierToX: -0.081 y: -0.023 control1X: -0.016 y: -0.015 control2X: -0.043 y: -0.023;
		rel_CubicBezierToX: -0.065 y: 0.008 control1X: -0.020 y: 0.000 control2X: -0.042 y: 0.003;
		rel_CubicBezierToX: -0.071 y: 0.025 control1X: -0.023 y: 0.006 control2X: -0.047 y: 0.014;
		rel_LineToX: 0.012 y: -0.062;
		rel_CubicBezierToX: 0.075 y: -0.020 control1X: 0.026 y: -0.009 control2X: 0.051 y: -0.016;
		rel_CubicBezierToX: 0.071 y: -0.007 control1X: 0.025 y: -0.005 control2X: 0.048 y: -0.007;
		rel_CubicBezierToX: 0.124 y: 0.043 control1X: 0.060 y: 0.000 control2X: 0.101 y: 0.014;
		rel_CubicBezierToX: 0.018 y: 0.130 control1X: 0.024 y: 0.029 control2X: 0.029 y: 0.072;
		rel_MoveToX: -0.141 y: 0.030;
		rel_CubicBezierToX: -0.113 y: 0.017 control1X: -0.052 y: 0.000 control2X: -0.090 y: 0.006;
		rel_CubicBezierToX: -0.042 y: 0.055 control1X: -0.023 y: 0.011 control2X: -0.037 y: 0.029;
		rel_CubicBezierToX: 0.014 y: 0.050 control1X: -0.004 y: 0.021 control2X: 0.000 y: 0.038;
		rel_CubicBezierToX: 0.060 y: 0.019 control1X: 0.014 y: 0.012 control2X: 0.034 y: 0.019;
		rel_CubicBezierToX: 0.090 y: -0.035 control1X: 0.034 y: 0.000 control2X: 0.064 y: -0.012;
		rel_CubicBezierToX: 0.050 y: -0.091 control1X: 0.026 y: -0.023 control2X: 0.042 y: -0.054;
		rel_LineToX: 0.003 y: -0.014;
		rel_LineToX: -0.062 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics171
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.439;
		rel_LineToX: -0.019 y: 0.093;
		rel_LineToX: -0.173 y: 0.131;
		rel_LineToX: 0.121 y: 0.131;
		rel_LineToX: -0.019 y: 0.093;
		rel_LineToX: -0.188 y: -0.204;
		rel_LineToX: 0.008 y: -0.040;
		rel_LineToX: 0.270 y: -0.204;
		rel_MoveToX: -0.212 y: 0.000;
		rel_LineToX: -0.019 y: 0.093;
		rel_LineToX: -0.173 y: 0.131;
		rel_LineToX: 0.121 y: 0.131;
		rel_LineToX: -0.019 y: 0.093;
		rel_LineToX: -0.188 y: -0.204;
		rel_LineToX: 0.008 y: -0.040;
		rel_LineToX: 0.270 y: -0.204! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics172
	vectorGraphicsEngine
		rel_MoveToX: 0.037 y: 0.535;
		rel_LineToX: 0.626 y: 0.000;
		rel_LineToX: -0.056 y: 0.281;
		rel_LineToX: -0.082 y: 0.000;
		rel_LineToX: 0.040 y: -0.198;
		rel_LineToX: -0.544 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics173
	vectorGraphicsEngine
		rel_MoveToX: -0.042 y: 0.642;
		rel_LineToX: 0.263 y: 0.000;
		rel_LineToX: -0.016 y: 0.080;
		rel_LineToX: -0.263 y: 0.000;
		rel_LineToX: 0.016 y: -0.080! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics174
	vectorGraphicsEngine
		rel_MoveToX: 0.482 y: 0.281;
		rel_CubicBezierToX: -0.124 y: 0.023 control1X: -0.043 y: 0.000 control2X: -0.084 y: 0.008;
		rel_CubicBezierToX: -0.115 y: 0.069 control1X: -0.040 y: 0.015 control2X: -0.078 y: 0.038;
		rel_CubicBezierToX: -0.090 y: 0.102 control1X: -0.037 y: 0.031 control2X: -0.067 y: 0.065;
		rel_CubicBezierToX: -0.047 y: 0.119 control1X: -0.023 y: 0.037 control2X: -0.039 y: 0.077;
		rel_CubicBezierToX: 0.000 y: 0.119 control1X: -0.008 y: 0.042 control2X: -0.009 y: 0.082;
		rel_CubicBezierToX: 0.049 y: 0.101 control1X: 0.008 y: 0.037 control2X: 0.025 y: 0.071;
		rel_CubicBezierToX: 0.087 y: 0.069 control1X: 0.024 y: 0.031 control2X: 0.053 y: 0.054;
		rel_CubicBezierToX: 0.115 y: 0.023 control1X: 0.034 y: 0.015 control2X: 0.072 y: 0.023;
		rel_CubicBezierToX: 0.124 y: -0.023 control1X: 0.043 y: 0.000 control2X: 0.084 y: -0.008;
		rel_CubicBezierToX: 0.115 y: -0.069 control1X: 0.040 y: -0.016 control2X: 0.079 y: -0.039;
		rel_CubicBezierToX: 0.088 y: -0.101 control1X: 0.036 y: -0.030 control2X: 0.066 y: -0.064;
		rel_CubicBezierToX: 0.047 y: -0.119 control1X: 0.023 y: -0.037 control2X: 0.038 y: -0.077;
		rel_CubicBezierToX: 0.001 y: -0.120 control1X: 0.009 y: -0.043 control2X: 0.009 y: -0.083;
		rel_CubicBezierToX: -0.048 y: -0.101 control1X: -0.008 y: -0.037 control2X: -0.024 y: -0.071;
		rel_CubicBezierToX: -0.088 y: -0.069 control1X: -0.024 y: -0.031 control2X: -0.054 y: -0.054;
		rel_CubicBezierToX: -0.115 y: -0.023 control1X: -0.034 y: -0.015 control2X: -0.072 y: -0.023;
		rel_MoveToX: 0.010 y: -0.050;
		rel_CubicBezierToX: 0.133 y: 0.027 control1X: 0.049 y: 0.000 control2X: 0.094 y: 0.009;
		rel_CubicBezierToX: 0.102 y: 0.080 control1X: 0.039 y: 0.018 control2X: 0.073 y: 0.045;
		rel_CubicBezierToX: 0.056 y: 0.117 control1X: 0.028 y: 0.035 control2X: 0.047 y: 0.075;
		rel_CubicBezierToX: -0.001 y: 0.139 control1X: 0.009 y: 0.043 control2X: 0.009 y: 0.089;
		rel_CubicBezierToX: -0.054 y: 0.138 control1X: -0.010 y: 0.049 control2X: -0.028 y: 0.096;
		rel_CubicBezierToX: -0.103 y: 0.117 control1X: -0.026 y: 0.042 control2X: -0.060 y: 0.081;
		rel_CubicBezierToX: -0.134 y: 0.080 control1X: -0.043 y: 0.035 control2X: -0.087 y: 0.062;
		rel_CubicBezierToX: -0.144 y: 0.027 control1X: -0.046 y: 0.018 control2X: -0.094 y: 0.027;
		rel_CubicBezierToX: -0.133 y: -0.027 control1X: -0.049 y: 0.000 control2X: -0.094 y: -0.009;
		rel_CubicBezierToX: -0.101 y: -0.080 control1X: -0.039 y: -0.018 control2X: -0.073 y: -0.045;
		rel_CubicBezierToX: -0.056 y: -0.117 control1X: -0.028 y: -0.035 control2X: -0.047 y: -0.074;
		rel_CubicBezierToX: 0.001 y: -0.138 control1X: -0.009 y: -0.043 control2X: -0.009 y: -0.089;
		rel_CubicBezierToX: 0.054 y: -0.139 control1X: 0.010 y: -0.050 control2X: 0.028 y: -0.097;
		rel_CubicBezierToX: 0.103 y: -0.117 control1X: 0.026 y: -0.043 control2X: 0.060 y: -0.082;
		rel_CubicBezierToX: 0.133 y: -0.080 control1X: 0.043 y: -0.035 control2X: 0.087 y: -0.062;
		rel_CubicBezierToX: 0.144 y: -0.027 control1X: 0.047 y: -0.018 control2X: 0.095 y: -0.027;
		rel_MoveToX: -0.054 y: 0.202;
		rel_LineToX: -0.060 y: 0.000;
		rel_LineToX: -0.027 y: 0.135;
		rel_LineToX: 0.060 y: 0.000;
		rel_CubicBezierToX: 0.078 y: -0.015 control1X: 0.036 y: 0.000 control2X: 0.062 y: -0.005;
		rel_CubicBezierToX: 0.032 y: -0.052 control1X: 0.016 y: -0.010 control2X: 0.027 y: -0.027;
		rel_CubicBezierToX: -0.011 y: -0.052 control1X: 0.005 y: -0.025 control2X: 0.001 y: -0.042;
		rel_CubicBezierToX: -0.072 y: -0.016 control1X: -0.012 y: -0.011 control2X: -0.036 y: -0.016;
		rel_MoveToX: 0.013 y: -0.050;
		rel_CubicBezierToX: 0.123 y: 0.029 control1X: 0.057 y: 0.000 control2X: 0.098 y: 0.010;
		rel_CubicBezierToX: 0.025 y: 0.088 control1X: 0.024 y: 0.019 control2X: 0.033 y: 0.048;
		rel_CubicBezierToX: -0.040 y: 0.069 control1X: -0.006 y: 0.028 control2X: -0.019 y: 0.051;
		rel_CubicBezierToX: -0.079 y: 0.036 control1X: -0.021 y: 0.018 control2X: -0.047 y: 0.030;
		rel_CubicBezierToX: 0.023 y: 0.023 control1X: 0.007 y: 0.003 control2X: 0.015 y: 0.010;
		rel_CubicBezierToX: 0.028 y: 0.054 control1X: 0.009 y: 0.012 control2X: 0.018 y: 0.030;
		rel_LineToX: 0.045 y: 0.109;
		rel_LineToX: -0.084 y: 0.000;
		rel_LineToX: -0.043 y: -0.102;
		rel_CubicBezierToX: -0.035 y: -0.059 control1X: -0.013 y: -0.031 control2X: -0.025 y: -0.051;
		rel_CubicBezierToX: -0.041 y: -0.013 control1X: -0.010 y: -0.008 control2X: -0.024 y: -0.013;
		rel_LineToX: -0.032 y: 0.000;
		rel_LineToX: -0.035 y: 0.174;
		rel_LineToX: -0.076 y: 0.000;
		rel_LineToX: 0.082 y: -0.408;
		rel_LineToX: 0.139 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics175
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: 0.211;
		rel_LineToX: 0.292 y: 0.000;
		rel_LineToX: -0.014 y: 0.072;
		rel_LineToX: -0.292 y: 0.000;
		rel_LineToX: 0.014 y: -0.072! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics176
	vectorGraphicsEngine
		rel_MoveToX: 0.232 y: 0.277;
		rel_CubicBezierToX: -0.071 y: 0.027 control1X: -0.026 y: 0.000 control2X: -0.050 y: 0.009;
		rel_CubicBezierToX: -0.040 y: 0.066 control1X: -0.021 y: 0.018 control2X: -0.035 y: 0.040;
		rel_CubicBezierToX: 0.014 y: 0.066 control1X: -0.005 y: 0.026 control2X: -0.001 y: 0.048;
		rel_CubicBezierToX: 0.061 y: 0.026 control1X: 0.014 y: 0.018 control2X: 0.035 y: 0.026;
		rel_CubicBezierToX: 0.071 y: -0.026 control1X: 0.026 y: 0.000 control2X: 0.050 y: -0.009;
		rel_CubicBezierToX: 0.040 y: -0.066 control1X: 0.021 y: -0.018 control2X: 0.035 y: -0.040;
		rel_CubicBezierToX: -0.014 y: -0.066 control1X: 0.005 y: -0.026 control2X: 0.000 y: -0.048;
		rel_CubicBezierToX: -0.060 y: -0.027 control1X: -0.014 y: -0.018 control2X: -0.034 y: -0.027;
		rel_MoveToX: 0.013 y: -0.063;
		rel_CubicBezierToX: 0.058 y: 0.012 control1X: 0.021 y: 0.000 control2X: 0.040 y: 0.004;
		rel_CubicBezierToX: 0.043 y: 0.034 control1X: 0.018 y: 0.008 control2X: 0.032 y: 0.019;
		rel_CubicBezierToX: 0.024 y: 0.050 control1X: 0.012 y: 0.015 control2X: 0.020 y: 0.031;
		rel_CubicBezierToX: -0.001 y: 0.060 control1X: 0.004 y: 0.019 control2X: 0.004 y: 0.039;
		rel_CubicBezierToX: -0.067 y: 0.109 control1X: -0.009 y: 0.043 control2X: -0.031 y: 0.080;
		rel_CubicBezierToX: -0.119 y: 0.044 control1X: -0.036 y: 0.030 control2X: -0.076 y: 0.044;
		rel_CubicBezierToX: -0.101 y: -0.044 control1X: -0.044 y: 0.000 control2X: -0.078 y: -0.015;
		rel_CubicBezierToX: -0.022 y: -0.110 control1X: -0.023 y: -0.029 control2X: -0.031 y: -0.066;
		rel_CubicBezierToX: 0.067 y: -0.111 control1X: 0.009 y: -0.044 control2X: 0.031 y: -0.081;
		rel_CubicBezierToX: 0.119 y: -0.045 control1X: 0.036 y: -0.030 control2X: 0.076 y: -0.045! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics177
	vectorGraphicsEngine
		rel_MoveToX: 0.432 y: 0.329;
		rel_LineToX: -0.038 y: 0.189;
		rel_LineToX: 0.272 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.272 y: 0.000;
		rel_LineToX: -0.038 y: 0.189;
		rel_LineToX: -0.082 y: 0.000;
		rel_LineToX: 0.038 y: -0.189;
		rel_LineToX: -0.272 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.272 y: 0.000;
		rel_LineToX: 0.038 y: -0.189;
		rel_LineToX: 0.082 y: 0.000;
		rel_MoveToX: -0.463 y: 0.544;
		rel_LineToX: 0.626 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.626 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics178
	vectorGraphicsEngine
		rel_MoveToX: 0.054 y: 0.574;
		rel_LineToX: 0.207 y: 0.000;
		rel_LineToX: -0.011 y: 0.056;
		rel_LineToX: -0.292 y: 0.000;
		rel_LineToX: 0.011 y: -0.054;
		rel_CubicBezierToX: 0.056 y: -0.042 control1X: 0.013 y: -0.010 control2X: 0.032 y: -0.024;
		rel_CubicBezierToX: 0.207 y: -0.195 control1X: 0.132 y: -0.099 control2X: 0.201 y: -0.164;
		rel_CubicBezierToX: -0.015 y: -0.052 control1X: 0.004 y: -0.022 control2X: -0.001 y: -0.039;
		rel_CubicBezierToX: -0.063 y: -0.020 control1X: -0.014 y: -0.013 control2X: -0.035 y: -0.020;
		rel_CubicBezierToX: -0.057 y: 0.009 control1X: -0.017 y: 0.000 control2X: -0.036 y: 0.003;
		rel_CubicBezierToX: -0.070 y: 0.025 control1X: -0.021 y: 0.006 control2X: -0.044 y: 0.014;
		rel_LineToX: 0.012 y: -0.060;
		rel_CubicBezierToX: 0.074 y: -0.020 control1X: 0.027 y: -0.009 control2X: 0.051 y: -0.016;
		rel_CubicBezierToX: 0.062 y: -0.007 control1X: 0.023 y: -0.005 control2X: 0.043 y: -0.007;
		rel_CubicBezierToX: 0.107 y: 0.032 control1X: 0.047 y: 0.000 control2X: 0.083 y: 0.011;
		rel_CubicBezierToX: 0.025 y: 0.085 control1X: 0.024 y: 0.022 control2X: 0.033 y: 0.050;
		rel_CubicBezierToX: -0.203 y: 0.207 control1X: -0.009 y: 0.045 control2X: -0.077 y: 0.114;
		rel_CubicBezierToX: -0.050 y: 0.037 control1X: -0.021 y: 0.016 control2X: -0.038 y: 0.028! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics179
	vectorGraphicsEngine
		rel_MoveToX: 0.212 y: 0.409;
		rel_CubicBezierToX: 0.062 y: 0.036 control1X: 0.029 y: 0.006 control2X: 0.050 y: 0.018;
		rel_CubicBezierToX: 0.012 y: 0.066 control1X: 0.013 y: 0.018 control2X: 0.017 y: 0.040;
		rel_CubicBezierToX: -0.065 y: 0.093 control1X: -0.008 y: 0.040 control2X: -0.030 y: 0.071;
		rel_CubicBezierToX: -0.139 y: 0.033 control1X: -0.035 y: 0.022 control2X: -0.082 y: 0.033;
		rel_CubicBezierToX: -0.058 y: -0.005 control1X: -0.019 y: 0.000 control2X: -0.038 y: -0.002;
		rel_CubicBezierToX: -0.061 y: -0.015 control1X: -0.019 y: -0.004 control2X: -0.040 y: -0.009;
		rel_LineToX: 0.012 y: -0.059;
		rel_CubicBezierToX: 0.050 y: 0.020 control1X: 0.015 y: 0.009 control2X: 0.032 y: 0.015;
		rel_CubicBezierToX: 0.061 y: 0.006 control1X: 0.019 y: 0.004 control2X: 0.039 y: 0.006;
		rel_CubicBezierToX: 0.086 y: -0.019 control1X: 0.035 y: 0.000 control2X: 0.064 y: -0.006;
		rel_CubicBezierToX: 0.040 y: -0.054 control1X: 0.022 y: -0.013 control2X: 0.036 y: -0.031;
		rel_CubicBezierToX: -0.016 y: -0.055 control1X: 0.005 y: -0.024 control2X: -0.001 y: -0.042;
		rel_CubicBezierToX: -0.076 y: -0.019 control1X: -0.015 y: -0.012 control2X: -0.041 y: -0.019;
		rel_LineToX: -0.042 y: 0.000;
		rel_LineToX: 0.011 y: -0.053;
		rel_LineToX: 0.046 y: 0.000;
		rel_CubicBezierToX: 0.072 y: -0.015 control1X: 0.031 y: 0.000 control2X: 0.055 y: -0.005;
		rel_CubicBezierToX: 0.033 y: -0.045 control1X: 0.018 y: -0.010 control2X: 0.029 y: -0.025;
		rel_CubicBezierToX: -0.016 y: -0.043 control1X: 0.004 y: -0.019 control2X: -0.002 y: -0.033;
		rel_CubicBezierToX: -0.068 y: -0.015 control1X: -0.014 y: -0.010 control2X: -0.037 y: -0.015;
		rel_CubicBezierToX: -0.045 y: 0.004 control1X: -0.013 y: 0.000 control2X: -0.028 y: 0.002;
		rel_CubicBezierToX: -0.068 y: 0.016 control1X: -0.017 y: 0.003 control2X: -0.040 y: 0.008;
		rel_LineToX: 0.011 y: -0.056;
		rel_CubicBezierToX: 0.069 y: -0.013 control1X: 0.025 y: -0.006 control2X: 0.048 y: -0.010;
		rel_CubicBezierToX: 0.059 y: -0.004 control1X: 0.021 y: -0.003 control2X: 0.041 y: -0.004;
		rel_CubicBezierToX: 0.106 y: 0.029 control1X: 0.047 y: 0.000 control2X: 0.082 y: 0.010;
		rel_CubicBezierToX: 0.026 y: 0.077 control1X: 0.024 y: 0.019 control2X: 0.033 y: 0.045;
		rel_CubicBezierToX: -0.033 y: 0.057 control1X: -0.004 y: 0.022 control2X: -0.016 y: 0.041;
		rel_CubicBezierToX: -0.068 y: 0.032 control1X: -0.018 y: 0.016 control2X: -0.041 y: 0.026! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics180
	vectorGraphicsEngine
		rel_MoveToX: 0.048 y: 0.227;
		rel_LineToX: 0.573 y: 0.000;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.575 y: 0.571;
		rel_LineToX: 0.472 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.595 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.575 y: -0.571;
		rel_LineToX: -0.450 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_MoveToX: 0.255 y: -0.069;
		rel_LineToX: -0.077 y: -0.130;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: 0.064 y: 0.087;
		rel_LineToX: 0.098 y: -0.087;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: -0.129 y: 0.130;
		rel_LineToX: -0.092 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics181
	vectorGraphicsEngine
		rel_MoveToX: -0.110 y: 1.164;
		rel_LineToX: 0.151 y: -0.755;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.068 y: 0.340;
		rel_CubicBezierToX: 0.012 y: 0.107 control1X: -0.009 y: 0.047 control2X: -0.005 y: 0.083;
		rel_CubicBezierToX: 0.092 y: 0.036 control1X: 0.018 y: 0.024 control2X: 0.048 y: 0.036;
		rel_CubicBezierToX: 0.117 y: -0.041 control1X: 0.048 y: 0.000 control2X: 0.087 y: -0.014;
		rel_CubicBezierToX: 0.061 y: -0.123 control1X: 0.030 y: -0.027 control2X: 0.050 y: -0.068;
		rel_LineToX: 0.064 y: -0.319;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.084 y: 0.421;
		rel_CubicBezierToX: 0.000 y: 0.043 control1X: -0.004 y: 0.019 control2X: -0.004 y: 0.034;
		rel_CubicBezierToX: 0.024 y: 0.014 control1X: 0.004 y: 0.009 control2X: 0.012 y: 0.014;
		rel_CubicBezierToX: 0.013 y: -0.002 control1X: 0.003 y: 0.000 control2X: 0.007 y: -0.001;
		rel_CubicBezierToX: 0.023 y: -0.009 control1X: 0.006 y: -0.002 control2X: 0.013 y: -0.005;
		rel_LineToX: -0.014 y: 0.072;
		rel_CubicBezierToX: -0.041 y: 0.017 control1X: -0.015 y: 0.007 control2X: -0.029 y: 0.013;
		rel_CubicBezierToX: -0.036 y: 0.005 control1X: -0.012 y: 0.004 control2X: -0.024 y: 0.005;
		rel_CubicBezierToX: -0.050 y: -0.019 control1X: -0.022 y: 0.000 control2X: -0.039 y: -0.006;
		rel_CubicBezierToX: -0.016 y: -0.058 control1X: -0.011 y: -0.013 control2X: -0.016 y: -0.032;
		rel_CubicBezierToX: -0.072 y: 0.058 control1X: -0.021 y: 0.026 control2X: -0.045 y: 0.045;
		rel_CubicBezierToX: -0.087 y: 0.019 control1X: -0.026 y: 0.013 control2X: -0.055 y: 0.019;
		rel_CubicBezierToX: -0.081 y: -0.019 control1X: -0.033 y: 0.000 control2X: -0.060 y: -0.006;
		rel_CubicBezierToX: -0.044 y: -0.057 control1X: -0.021 y: -0.013 control2X: -0.035 y: -0.032;
		rel_LineToX: -0.054 y: 0.270;
		rel_LineToX: -0.090 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics182
	vectorGraphicsEngine
		rel_MoveToX: 0.301 y: 0.227;
		rel_LineToX: 0.219 y: 0.000;
		rel_LineToX: -0.165 y: 0.825;
		rel_LineToX: -0.069 y: 0.000;
		rel_LineToX: 0.153 y: -0.765;
		rel_LineToX: -0.093 y: 0.000;
		rel_LineToX: -0.153 y: 0.765;
		rel_LineToX: -0.069 y: 0.000;
		rel_LineToX: 0.083 y: -0.413;
		rel_CubicBezierToX: -0.150 y: -0.062 control1X: -0.069 y: -0.005 control2X: -0.119 y: -0.026;
		rel_CubicBezierToX: -0.028 y: -0.144 control1X: -0.031 y: -0.036 control2X: -0.040 y: -0.084;
		rel_CubicBezierToX: 0.093 y: -0.149 control1X: 0.012 y: -0.062 control2X: 0.044 y: -0.112;
		rel_CubicBezierToX: 0.180 y: -0.057 control1X: 0.050 y: -0.038 control2X: 0.110 y: -0.057! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics183
	vectorGraphicsEngine
		rel_MoveToX: 0.035 y: 0.547;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.025 y: -0.124! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics184
	vectorGraphicsEngine
		rel_MoveToX: 0.011 y: 0.409;
		rel_LineToX: 0.427 y: 0.000;
		rel_LineToX: -0.016 y: 0.082;
		rel_LineToX: -0.417 y: 0.393;
		rel_LineToX: 0.338 y: 0.000;
		rel_LineToX: -0.014 y: 0.072;
		rel_LineToX: -0.439 y: 0.000;
		rel_LineToX: 0.016 y: -0.082;
		rel_LineToX: 0.417 y: -0.393;
		rel_LineToX: -0.326 y: 0.000;
		rel_LineToX: 0.014 y: -0.072;
		rel_MoveToX: 0.186 y: -0.069;
		rel_LineToX: -0.083 y: -0.184;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: 0.064 y: 0.120;
		rel_LineToX: 0.112 y: -0.120;
		rel_LineToX: 0.068 y: 0.000;
		rel_LineToX: -0.156 y: 0.184;
		rel_LineToX: -0.072 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics185
	vectorGraphicsEngine
		rel_MoveToX: -0.001 y: 0.576;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: 0.060 y: -0.298;
		rel_LineToX: -0.113 y: 0.020;
		rel_LineToX: 0.011 y: -0.057;
		rel_LineToX: 0.116 y: -0.019;
		rel_LineToX: 0.067 y: 0.000;
		rel_LineToX: -0.071 y: 0.354;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: -0.011 y: 0.054;
		rel_LineToX: -0.270 y: 0.000;
		rel_LineToX: 0.011 y: -0.054! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics186
	vectorGraphicsEngine
		rel_MoveToX: -0.028 y: 0.667;
		rel_LineToX: 0.336 y: 0.000;
		rel_LineToX: -0.012 y: 0.060;
		rel_LineToX: -0.336 y: 0.000;
		rel_LineToX: 0.012 y: -0.060;
		rel_MoveToX: 0.259 y: -0.453;
		rel_CubicBezierToX: 0.127 y: 0.054 control1X: 0.058 y: 0.000 control2X: 0.101 y: 0.018;
		rel_CubicBezierToX: 0.021 y: 0.147 control1X: 0.026 y: 0.036 control2X: 0.033 y: 0.085;
		rel_CubicBezierToX: -0.080 y: 0.147 control1X: -0.012 y: 0.062 control2X: -0.039 y: 0.111;
		rel_CubicBezierToX: -0.148 y: 0.053 control1X: -0.041 y: 0.035 control2X: -0.090 y: 0.053;
		rel_CubicBezierToX: -0.127 y: -0.054 control1X: -0.058 y: 0.000 control2X: -0.101 y: -0.018;
		rel_CubicBezierToX: -0.021 y: -0.146 control1X: -0.027 y: -0.036 control2X: -0.034 y: -0.085;
		rel_CubicBezierToX: 0.080 y: -0.147 control1X: 0.012 y: -0.062 control2X: 0.039 y: -0.111;
		rel_CubicBezierToX: 0.149 y: -0.054 control1X: 0.041 y: -0.036 control2X: 0.091 y: -0.054;
		rel_MoveToX: -0.011 y: 0.056;
		rel_CubicBezierToX: -0.090 y: 0.040 control1X: -0.034 y: 0.000 control2X: -0.064 y: 0.013;
		rel_CubicBezierToX: -0.052 y: 0.105 control1X: -0.026 y: 0.026 control2X: -0.043 y: 0.061;
		rel_CubicBezierToX: 0.010 y: 0.104 control1X: -0.009 y: 0.043 control2X: -0.005 y: 0.078;
		rel_CubicBezierToX: 0.074 y: 0.039 control1X: 0.016 y: 0.026 control2X: 0.040 y: 0.039;
		rel_CubicBezierToX: 0.089 y: -0.039 control1X: 0.034 y: 0.000 control2X: 0.064 y: -0.013;
		rel_CubicBezierToX: 0.052 y: -0.104 control1X: 0.026 y: -0.026 control2X: 0.043 y: -0.061;
		rel_CubicBezierToX: -0.009 y: -0.106 control1X: 0.009 y: -0.045 control2X: 0.006 y: -0.080;
		rel_CubicBezierToX: -0.074 y: -0.039 control1X: -0.015 y: -0.026 control2X: -0.040 y: -0.039! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics187
	vectorGraphicsEngine
		rel_MoveToX: 0.044 y: 0.439;
		rel_LineToX: 0.188 y: 0.204;
		rel_LineToX: -0.008 y: 0.040;
		rel_LineToX: -0.270 y: 0.204;
		rel_LineToX: 0.019 y: -0.093;
		rel_LineToX: 0.173 y: -0.131;
		rel_LineToX: -0.121 y: -0.131;
		rel_LineToX: 0.019 y: -0.093;
		rel_MoveToX: 0.212 y: 0.000;
		rel_LineToX: 0.188 y: 0.204;
		rel_LineToX: -0.008 y: 0.040;
		rel_LineToX: -0.270 y: 0.204;
		rel_LineToX: 0.019 y: -0.093;
		rel_LineToX: 0.173 y: -0.131;
		rel_LineToX: -0.121 y: -0.131;
		rel_LineToX: 0.019 y: -0.093! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics188
	vectorGraphicsEngine
		rel_MoveToX: 0.989 y: 0.227;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.520 y: 0.000;
		rel_CubicBezierToX: -0.302 y: -0.093 control1X: -0.141 y: 0.000 control2X: -0.242 y: -0.031;
		rel_CubicBezierToX: -0.054 y: -0.272 control1X: -0.060 y: -0.062 control2X: -0.078 y: -0.153;
		rel_CubicBezierToX: 0.163 y: -0.271 control1X: 0.024 y: -0.119 control2X: 0.078 y: -0.209;
		rel_CubicBezierToX: 0.339 y: -0.093 control1X: 0.085 y: -0.062 control2X: 0.198 y: -0.093;
		rel_LineToX: 0.511 y: 0.000;
		rel_MoveToX: -0.478 y: 0.083;
		rel_LineToX: -0.063 y: 0.000;
		rel_CubicBezierToX: -0.249 y: 0.070 control1X: -0.104 y: 0.000 control2X: -0.187 y: 0.023;
		rel_CubicBezierToX: -0.120 y: 0.210 control1X: -0.061 y: 0.046 control2X: -0.101 y: 0.117;
		rel_CubicBezierToX: 0.036 y: 0.212 control1X: -0.019 y: 0.094 control2X: -0.007 y: 0.165;
		rel_CubicBezierToX: 0.221 y: 0.070 control1X: 0.043 y: 0.047 control2X: 0.116 y: 0.070;
		rel_LineToX: 0.063 y: 0.000;
		rel_LineToX: 0.113 y: -0.563! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics189
	vectorGraphicsEngine
		rel_MoveToX: 0.791 y: 0.634;
		rel_CubicBezierToX: -0.017 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.089;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.054 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: 0.085 y: 0.026;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.129 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.106 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.071 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.132 y: -0.027 control1X: -0.052 y: 0.000 control2X: -0.096 y: -0.009;
		rel_CubicBezierToX: -0.083 y: -0.080 control1X: -0.036 y: -0.018 control2X: -0.063 y: -0.045;
		rel_CubicBezierToX: -0.102 y: 0.081 control1X: -0.030 y: 0.036 control2X: -0.064 y: 0.063;
		rel_CubicBezierToX: -0.124 y: 0.026 control1X: -0.037 y: 0.018 control2X: -0.079 y: 0.026;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_CubicBezierToX: 0.114 y: 0.027 control1X: 0.046 y: 0.000 control2X: 0.084 y: 0.009;
		rel_CubicBezierToX: 0.068 y: 0.080 control1X: 0.031 y: 0.018 control2X: 0.054 y: 0.044;
		rel_CubicBezierToX: 0.111 y: -0.080 control1X: 0.032 y: -0.035 control2X: 0.069 y: -0.061;
		rel_CubicBezierToX: 0.132 y: -0.027 control1X: 0.041 y: -0.018 control2X: 0.085 y: -0.027;
		rel_CubicBezierToX: 0.161 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.626 y: -0.188;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics190
	vectorGraphicsEngine
		rel_MoveToX: -0.010 y: 0.227;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.142 y: 0.300;
		rel_LineToX: 0.261 y: -0.300;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: -0.334 y: 0.382;
		rel_LineToX: -0.069 y: 0.347;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.069 y: -0.347;
		rel_LineToX: -0.181 y: -0.382;
		rel_MoveToX: 0.391 y: -0.184;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics191
	vectorGraphicsEngine
		rel_MoveToX: 0.197 y: 0.423;
		rel_LineToX: 0.093 y: 0.000;
		rel_LineToX: -0.015 y: 0.076;
		rel_CubicBezierToX: -0.030 y: 0.081 control1X: -0.007 y: 0.033 control2X: -0.017 y: 0.060;
		rel_CubicBezierToX: -0.072 y: 0.074 control1X: -0.013 y: 0.021 control2X: -0.037 y: 0.046;
		rel_LineToX: -0.053 y: 0.043;
		rel_CubicBezierToX: -0.051 y: 0.049 control1X: -0.022 y: 0.017 control2X: -0.039 y: 0.033;
		rel_CubicBezierToX: -0.022 y: 0.047 control1X: -0.011 y: 0.015 control2X: -0.018 y: 0.031;
		rel_CubicBezierToX: 0.018 y: 0.070 control1X: -0.006 y: 0.029 control2X: 0.000 y: 0.052;
		rel_CubicBezierToX: 0.080 y: 0.027 control1X: 0.018 y: 0.018 control2X: 0.045 y: 0.027;
		rel_CubicBezierToX: 0.085 y: -0.017 control1X: 0.025 y: 0.000 control2X: 0.054 y: -0.006;
		rel_CubicBezierToX: 0.101 y: -0.050 control1X: 0.032 y: -0.011 control2X: 0.065 y: -0.028;
		rel_LineToX: -0.018 y: 0.092;
		rel_CubicBezierToX: -0.101 y: 0.041 control1X: -0.034 y: 0.018 control2X: -0.068 y: 0.032;
		rel_CubicBezierToX: -0.100 y: 0.014 control1X: -0.033 y: 0.009 control2X: -0.066 y: 0.014;
		rel_CubicBezierToX: -0.135 y: -0.047 control1X: -0.060 y: 0.000 control2X: -0.105 y: -0.016;
		rel_CubicBezierToX: -0.030 y: -0.125 control1X: -0.030 y: -0.032 control2X: -0.040 y: -0.073;
		rel_CubicBezierToX: 0.032 y: -0.070 control1X: 0.005 y: -0.025 control2X: 0.016 y: -0.048;
		rel_CubicBezierToX: 0.077 y: -0.076 control1X: 0.017 y: -0.022 control2X: 0.042 y: -0.048;
		rel_LineToX: 0.051 y: -0.042;
		rel_CubicBezierToX: 0.040 y: -0.036 control1X: 0.019 y: -0.015 control2X: 0.032 y: -0.027;
		rel_CubicBezierToX: 0.018 y: -0.025 control1X: 0.008 y: -0.009 control2X: 0.014 y: -0.017;
		rel_CubicBezierToX: 0.010 y: -0.025 control1X: 0.004 y: -0.007 control2X: 0.007 y: -0.015;
		rel_CubicBezierToX: 0.010 y: -0.041 control1X: 0.003 y: -0.010 control2X: 0.006 y: -0.023;
		rel_LineToX: 0.012 y: -0.060;
		rel_MoveToX: 0.110 y: -0.072;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.025 y: 0.124! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics192
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729;
		rel_MoveToX: 0.067 y: -0.198;
		rel_LineToX: 0.070 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.087 y: -0.129;
		rel_LineToX: 0.091 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics193
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729;
		rel_MoveToX: 0.122 y: -0.198;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics194
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729;
		rel_MoveToX: 0.050 y: -0.199;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: 0.077 y: 0.130;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.087;
		rel_LineToX: -0.098 y: 0.087;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.129 y: -0.130! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics195
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729;
		rel_MoveToX: 0.075 y: -0.106;
		rel_LineToX: -0.025 y: -0.016;
		rel_CubicBezierToX: -0.018 y: -0.009 control1X: -0.007 y: -0.005 control2X: -0.013 y: -0.008;
		rel_CubicBezierToX: -0.013 y: -0.003 control1X: -0.005 y: -0.002 control2X: -0.009 y: -0.003;
		rel_CubicBezierToX: -0.030 y: 0.012 control1X: -0.012 y: 0.000 control2X: -0.022 y: 0.004;
		rel_CubicBezierToX: -0.017 y: 0.034 control1X: -0.008 y: 0.008 control2X: -0.014 y: 0.020;
		rel_LineToX: -0.001 y: 0.003;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.041 y: -0.078 control1X: 0.007 y: -0.033 control2X: 0.020 y: -0.059;
		rel_CubicBezierToX: 0.073 y: -0.029 control1X: 0.021 y: -0.019 control2X: 0.045 y: -0.029;
		rel_CubicBezierToX: 0.031 y: 0.004 control1X: 0.012 y: 0.000 control2X: 0.022 y: 0.001;
		rel_CubicBezierToX: 0.035 y: 0.017 control1X: 0.010 y: 0.003 control2X: 0.021 y: 0.008;
		rel_LineToX: 0.025 y: 0.015;
		rel_CubicBezierToX: 0.018 y: 0.009 control1X: 0.007 y: 0.004 control2X: 0.013 y: 0.007;
		rel_CubicBezierToX: 0.015 y: 0.003 control1X: 0.005 y: 0.002 control2X: 0.010 y: 0.003;
		rel_CubicBezierToX: 0.028 y: -0.012 control1X: 0.010 y: 0.000 control2X: 0.020 y: -0.004;
		rel_CubicBezierToX: 0.016 y: -0.033 control1X: 0.008 y: -0.008 control2X: 0.014 y: -0.019;
		rel_LineToX: 0.001 y: -0.003;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.042 y: 0.078 control1X: -0.007 y: 0.033 control2X: -0.021 y: 0.059;
		rel_CubicBezierToX: -0.072 y: 0.029 control1X: -0.020 y: 0.019 control2X: -0.044 y: 0.029;
		rel_CubicBezierToX: -0.030 y: -0.004 control1X: -0.011 y: 0.000 control2X: -0.021 y: -0.001;
		rel_CubicBezierToX: -0.036 y: -0.017 control1X: -0.009 y: -0.003 control2X: -0.021 y: -0.008! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics196
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729;
		rel_MoveToX: 0.138 y: -0.184;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics197
	vectorGraphicsEngine
		rel_MoveToX: 0.421 y: 0.162;
		rel_CubicBezierToX: -0.012 y: -0.052 control1X: 0.004 y: -0.020 control2X: 0.000 y: -0.038;
		rel_CubicBezierToX: -0.048 y: -0.022 control1X: -0.011 y: -0.015 control2X: -0.027 y: -0.022;
		rel_CubicBezierToX: -0.057 y: 0.021 control1X: -0.021 y: 0.000 control2X: -0.040 y: 0.007;
		rel_CubicBezierToX: -0.032 y: 0.053 control1X: -0.017 y: 0.014 control2X: -0.027 y: 0.032;
		rel_CubicBezierToX: 0.011 y: 0.052 control1X: -0.004 y: 0.020 control2X: 0.000 y: 0.038;
		rel_CubicBezierToX: 0.048 y: 0.021 control1X: 0.011 y: 0.014 control2X: 0.027 y: 0.021;
		rel_CubicBezierToX: 0.057 y: -0.021 control1X: 0.021 y: 0.000 control2X: 0.039 y: -0.007;
		rel_CubicBezierToX: 0.032 y: -0.052 control1X: 0.018 y: -0.014 control2X: 0.028 y: -0.032;
		rel_MoveToX: -0.107 y: 0.162;
		rel_LineToX: -0.205 y: 0.363;
		rel_LineToX: 0.266 y: 0.000;
		rel_LineToX: -0.061 y: -0.363;
		rel_MoveToX: -0.064 y: -0.050;
		rel_CubicBezierToX: -0.035 y: -0.048 control1X: -0.017 y: -0.013 control2X: -0.029 y: -0.029;
		rel_CubicBezierToX: -0.002 y: -0.064 control1X: -0.006 y: -0.019 control2X: -0.007 y: -0.040;
		rel_CubicBezierToX: 0.058 y: -0.095 control1X: 0.007 y: -0.037 control2X: 0.027 y: -0.069;
		rel_CubicBezierToX: 0.103 y: -0.039 control1X: 0.031 y: -0.026 control2X: 0.066 y: -0.039;
		rel_CubicBezierToX: 0.087 y: 0.040 control1X: 0.037 y: 0.000 control2X: 0.066 y: 0.013;
		rel_CubicBezierToX: 0.021 y: 0.095 control1X: 0.021 y: 0.026 control2X: 0.028 y: 0.058;
		rel_CubicBezierToX: -0.028 y: 0.063 control1X: -0.005 y: 0.023 control2X: -0.014 y: 0.044;
		rel_CubicBezierToX: -0.054 y: 0.049 control1X: -0.014 y: 0.019 control2X: -0.032 y: 0.035;
		rel_LineToX: 0.123 y: 0.682;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.397 y: -0.682! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics198
	vectorGraphicsEngine
		rel_MoveToX: 0.893 y: 0.227;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.037 y: -0.187;
		rel_LineToX: -0.258 y: 0.000;
		rel_LineToX: -0.116 y: 0.187;
		rel_LineToX: -0.100 y: 0.000;
		rel_LineToX: 0.451 y: -0.729;
		rel_LineToX: 0.592 y: 0.000;
		rel_MoveToX: -0.539 y: 0.077;
		rel_LineToX: -0.238 y: 0.383;
		rel_LineToX: 0.224 y: 0.000;
		rel_LineToX: 0.077 y: -0.383;
		rel_LineToX: -0.063 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics199
	vectorGraphicsEngine
		rel_MoveToX: 0.625 y: 0.283;
		rel_LineToX: -0.021 y: 0.104;
		rel_CubicBezierToX: -0.093 y: -0.069 control1X: -0.027 y: -0.031 control2X: -0.058 y: -0.054;
		rel_CubicBezierToX: -0.115 y: -0.023 control1X: -0.034 y: -0.015 control2X: -0.073 y: -0.023;
		rel_CubicBezierToX: -0.207 y: 0.077 control1X: -0.083 y: 0.000 control2X: -0.152 y: 0.026;
		rel_CubicBezierToX: -0.111 y: 0.221 control1X: -0.054 y: 0.051 control2X: -0.091 y: 0.124;
		rel_CubicBezierToX: 0.022 y: 0.221 control1X: -0.019 y: 0.096 control2X: -0.012 y: 0.170;
		rel_CubicBezierToX: 0.176 y: 0.076 control1X: 0.034 y: 0.051 control2X: 0.093 y: 0.076;
		rel_CubicBezierToX: 0.124 y: -0.023 control1X: 0.042 y: 0.000 control2X: 0.084 y: -0.008;
		rel_CubicBezierToX: 0.120 y: -0.069 control1X: 0.041 y: -0.015 control2X: 0.081 y: -0.038;
		rel_LineToX: -0.021 y: 0.103;
		rel_CubicBezierToX: -0.120 y: 0.053 control1X: -0.039 y: 0.023 control2X: -0.079 y: 0.041;
		rel_CubicBezierToX: -0.126 y: 0.017 control1X: -0.041 y: 0.012 control2X: -0.083 y: 0.017;
		rel_CubicBezierToX: -0.241 y: -0.101 control1X: -0.110 y: 0.000 control2X: -0.191 y: -0.034;
		rel_CubicBezierToX: -0.040 y: -0.277 control1X: -0.050 y: -0.068 control2X: -0.063 y: -0.160;
		rel_CubicBezierToX: 0.151 y: -0.277 control1X: 0.023 y: -0.117 control2X: 0.074 y: -0.210;
		rel_CubicBezierToX: 0.281 y: -0.102 control1X: 0.077 y: -0.068 control2X: 0.171 y: -0.102;
		rel_CubicBezierToX: 0.119 y: 0.018 control1X: 0.044 y: 0.000 control2X: 0.083 y: 0.006;
		rel_CubicBezierToX: 0.099 y: 0.052 control1X: 0.037 y: 0.011 control2X: 0.069 y: 0.029;
		rel_MoveToX: -0.341 y: 0.673;
		rel_CubicBezierToX: 0.029 y: 0.056 control1X: 0.014 y: 0.020 control2X: 0.023 y: 0.039;
		rel_CubicBezierToX: 0.003 y: 0.049 control1X: 0.005 y: 0.017 control2X: 0.006 y: 0.034;
		rel_CubicBezierToX: -0.042 y: 0.065 control1X: -0.006 y: 0.029 control2X: -0.020 y: 0.051;
		rel_CubicBezierToX: -0.091 y: 0.022 control1X: -0.023 y: 0.015 control2X: -0.053 y: 0.022;
		rel_CubicBezierToX: -0.043 y: -0.003 control1X: -0.015 y: 0.000 control2X: -0.029 y: -0.001;
		rel_CubicBezierToX: -0.040 y: -0.009 control1X: -0.014 y: -0.002 control2X: -0.027 y: -0.005;
		rel_LineToX: 0.013 y: -0.064;
		rel_CubicBezierToX: 0.032 y: 0.012 control1X: 0.010 y: 0.006 control2X: 0.021 y: 0.009;
		rel_CubicBezierToX: 0.040 y: 0.004 control1X: 0.011 y: 0.003 control2X: 0.025 y: 0.004;
		rel_CubicBezierToX: 0.046 y: -0.012 control1X: 0.019 y: 0.000 control2X: 0.034 y: -0.004;
		rel_CubicBezierToX: 0.021 y: -0.034 control1X: 0.011 y: -0.008 control2X: 0.018 y: -0.019;
		rel_CubicBezierToX: -0.003 y: -0.036 control1X: 0.002 y: -0.010 control2X: 0.001 y: -0.022;
		rel_CubicBezierToX: -0.022 y: -0.051 control1X: -0.004 y: -0.014 control2X: -0.011 y: -0.031;
		rel_LineToX: 0.058 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics200
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.461 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.241 y: -0.198;
		rel_LineToX: 0.070 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.087 y: -0.129;
		rel_LineToX: 0.091 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics201
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.461 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.296 y: -0.198;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics202
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.461 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.223 y: -0.199;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: 0.077 y: 0.130;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.087;
		rel_LineToX: -0.098 y: 0.087;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.129 y: -0.130! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics203
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.461 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.312 y: -0.184;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics204
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.062 y: -0.198;
		rel_LineToX: 0.070 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.087 y: -0.129;
		rel_LineToX: 0.091 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics205
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.116 y: -0.198;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics206
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.044 y: -0.199;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: 0.077 y: 0.130;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.087;
		rel_LineToX: -0.098 y: 0.087;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.129 y: -0.130! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics207
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.133 y: -0.184;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics208
	vectorGraphicsEngine
		rel_MoveToX: 0.095 y: 0.227;
		rel_LineToX: 0.203 y: 0.000;
		rel_CubicBezierToX: 0.293 y: 0.088 control1X: 0.141 y: 0.000 control2X: 0.239 y: 0.029;
		rel_CubicBezierToX: 0.044 y: 0.275 control1X: 0.054 y: 0.059 control2X: 0.069 y: 0.150;
		rel_CubicBezierToX: -0.155 y: 0.277 control1X: -0.025 y: 0.126 control2X: -0.077 y: 0.218;
		rel_CubicBezierToX: -0.328 y: 0.088 control1X: -0.078 y: 0.059 control2X: -0.187 y: 0.088;
		rel_LineToX: -0.203 y: 0.000;
		rel_LineToX: 0.068 y: -0.342;
		rel_LineToX: -0.098 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.098 y: 0.000;
		rel_LineToX: 0.063 y: -0.317;
		rel_MoveToX: 0.083 y: 0.081;
		rel_LineToX: -0.047 y: 0.236;
		rel_LineToX: 0.164 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.164 y: 0.000;
		rel_LineToX: -0.052 y: 0.261;
		rel_LineToX: 0.119 y: 0.000;
		rel_CubicBezierToX: 0.234 y: -0.068 control1X: 0.101 y: 0.000 control2X: 0.179 y: -0.023;
		rel_CubicBezierToX: 0.113 y: -0.216 control1X: 0.056 y: -0.046 control2X: 0.094 y: -0.117;
		rel_CubicBezierToX: -0.027 y: -0.214 control1X: 0.020 y: -0.098 control2X: 0.010 y: -0.169;
		rel_CubicBezierToX: -0.207 y: -0.068 control1X: -0.037 y: -0.046 control2X: -0.106 y: -0.068;
		rel_LineToX: -0.119 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics209
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.133 y: 0.000;
		rel_LineToX: 0.201 y: 0.610;
		rel_LineToX: 0.122 y: -0.610;
		rel_LineToX: 0.096 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.133 y: 0.000;
		rel_LineToX: -0.201 y: -0.610;
		rel_LineToX: -0.122 y: 0.610;
		rel_LineToX: -0.096 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.295 y: -0.106;
		rel_LineToX: -0.025 y: -0.016;
		rel_CubicBezierToX: -0.018 y: -0.009 control1X: -0.007 y: -0.005 control2X: -0.013 y: -0.008;
		rel_CubicBezierToX: -0.013 y: -0.003 control1X: -0.005 y: -0.002 control2X: -0.009 y: -0.003;
		rel_CubicBezierToX: -0.030 y: 0.012 control1X: -0.012 y: 0.000 control2X: -0.022 y: 0.004;
		rel_CubicBezierToX: -0.017 y: 0.034 control1X: -0.008 y: 0.008 control2X: -0.014 y: 0.020;
		rel_LineToX: -0.001 y: 0.003;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.041 y: -0.078 control1X: 0.007 y: -0.033 control2X: 0.020 y: -0.059;
		rel_CubicBezierToX: 0.073 y: -0.029 control1X: 0.021 y: -0.019 control2X: 0.045 y: -0.029;
		rel_CubicBezierToX: 0.032 y: 0.004 control1X: 0.012 y: 0.000 control2X: 0.022 y: 0.001;
		rel_CubicBezierToX: 0.035 y: 0.017 control1X: 0.010 y: 0.003 control2X: 0.021 y: 0.008;
		rel_LineToX: 0.025 y: 0.015;
		rel_CubicBezierToX: 0.018 y: 0.009 control1X: 0.007 y: 0.004 control2X: 0.013 y: 0.007;
		rel_CubicBezierToX: 0.015 y: 0.003 control1X: 0.005 y: 0.002 control2X: 0.010 y: 0.003;
		rel_CubicBezierToX: 0.028 y: -0.012 control1X: 0.010 y: 0.000 control2X: 0.020 y: -0.004;
		rel_CubicBezierToX: 0.016 y: -0.033 control1X: 0.008 y: -0.008 control2X: 0.014 y: -0.019;
		rel_LineToX: 0.001 y: -0.003;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.042 y: 0.078 control1X: -0.007 y: 0.033 control2X: -0.021 y: 0.059;
		rel_CubicBezierToX: -0.072 y: 0.029 control1X: -0.020 y: 0.019 control2X: -0.044 y: 0.029;
		rel_CubicBezierToX: -0.030 y: -0.004 control1X: -0.011 y: 0.000 control2X: -0.021 y: -0.001;
		rel_CubicBezierToX: -0.036 y: -0.017 control1X: -0.009 y: -0.003 control2X: -0.021 y: -0.008! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics210
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_MoveToX: 0.009 y: -0.185;
		rel_LineToX: 0.070 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.087 y: -0.129;
		rel_LineToX: 0.091 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics211
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_MoveToX: 0.064 y: -0.185;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics212
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_MoveToX: -0.009 y: -0.186;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: 0.077 y: 0.130;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.087;
		rel_LineToX: -0.098 y: 0.087;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.129 y: -0.130! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics213
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_MoveToX: 0.017 y: -0.093;
		rel_LineToX: -0.025 y: -0.016;
		rel_CubicBezierToX: -0.018 y: -0.009 control1X: -0.007 y: -0.005 control2X: -0.013 y: -0.008;
		rel_CubicBezierToX: -0.013 y: -0.003 control1X: -0.005 y: -0.002 control2X: -0.009 y: -0.003;
		rel_CubicBezierToX: -0.030 y: 0.012 control1X: -0.012 y: 0.000 control2X: -0.022 y: 0.004;
		rel_CubicBezierToX: -0.017 y: 0.034 control1X: -0.008 y: 0.008 control2X: -0.014 y: 0.020;
		rel_LineToX: -0.001 y: 0.003;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.041 y: -0.078 control1X: 0.007 y: -0.033 control2X: 0.020 y: -0.059;
		rel_CubicBezierToX: 0.073 y: -0.029 control1X: 0.021 y: -0.019 control2X: 0.045 y: -0.029;
		rel_CubicBezierToX: 0.031 y: 0.004 control1X: 0.012 y: 0.000 control2X: 0.022 y: 0.001;
		rel_CubicBezierToX: 0.035 y: 0.017 control1X: 0.010 y: 0.003 control2X: 0.021 y: 0.008;
		rel_LineToX: 0.025 y: 0.015;
		rel_CubicBezierToX: 0.018 y: 0.009 control1X: 0.007 y: 0.004 control2X: 0.013 y: 0.007;
		rel_CubicBezierToX: 0.015 y: 0.003 control1X: 0.005 y: 0.002 control2X: 0.010 y: 0.003;
		rel_CubicBezierToX: 0.028 y: -0.012 control1X: 0.010 y: 0.000 control2X: 0.020 y: -0.004;
		rel_CubicBezierToX: 0.016 y: -0.033 control1X: 0.008 y: -0.008 control2X: 0.014 y: -0.019;
		rel_LineToX: 0.001 y: -0.003;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.042 y: 0.078 control1X: -0.007 y: 0.033 control2X: -0.021 y: 0.059;
		rel_CubicBezierToX: -0.072 y: 0.029 control1X: -0.020 y: 0.019 control2X: -0.044 y: 0.029;
		rel_CubicBezierToX: -0.031 y: -0.004 control1X: -0.011 y: 0.000 control2X: -0.021 y: -0.001;
		rel_CubicBezierToX: -0.036 y: -0.017 control1X: -0.009 y: -0.003 control2X: -0.021 y: -0.008! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics214
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_MoveToX: 0.080 y: -0.171;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics215
	vectorGraphicsEngine
		rel_MoveToX: 0.655 y: 0.419;
		rel_LineToX: -0.268 y: 0.224;
		rel_LineToX: 0.178 y: 0.223;
		rel_LineToX: -0.070 y: 0.059;
		rel_LineToX: -0.179 y: -0.224;
		rel_LineToX: -0.269 y: 0.224;
		rel_LineToX: -0.046 y: -0.059;
		rel_LineToX: 0.267 y: -0.223;
		rel_LineToX: -0.178 y: -0.224;
		rel_LineToX: 0.069 y: -0.059;
		rel_LineToX: 0.179 y: 0.224;
		rel_LineToX: 0.269 y: -0.224;
		rel_LineToX: 0.046 y: 0.059! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics216
	vectorGraphicsEngine
		rel_MoveToX: 0.544 y: 0.413;
		rel_LineToX: -0.433 y: 0.413;
		rel_CubicBezierToX: 0.060 y: 0.048 control1X: 0.016 y: 0.021 control2X: 0.036 y: 0.037;
		rel_CubicBezierToX: 0.083 y: 0.016 control1X: 0.024 y: 0.011 control2X: 0.052 y: 0.016;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: 0.010 y: -0.100 control1X: 0.007 y: -0.036 control2X: 0.011 y: -0.070;
		rel_CubicBezierToX: -0.012 y: -0.080 control1X: 0.000 y: -0.030 control2X: -0.004 y: -0.056;
		rel_MoveToX: -0.029 y: -0.055;
		rel_CubicBezierToX: -0.060 y: -0.047 control1X: -0.016 y: -0.021 control2X: -0.036 y: -0.037;
		rel_CubicBezierToX: -0.082 y: -0.017 control1X: -0.024 y: -0.011 control2X: -0.051 y: -0.017;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: -0.011 y: 0.101 control1X: -0.007 y: 0.036 control2X: -0.011 y: 0.070;
		rel_CubicBezierToX: 0.013 y: 0.079 control1X: 0.000 y: 0.031 control2X: 0.005 y: 0.057;
		rel_LineToX: 0.433 y: -0.414;
		rel_MoveToX: -0.514 y: 0.491;
		rel_CubicBezierToX: -0.035 y: -0.114 control1X: -0.019 y: -0.033 control2X: -0.030 y: -0.071;
		rel_CubicBezierToX: 0.009 y: -0.143 control1X: -0.004 y: -0.043 control2X: -0.001 y: -0.091;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_CubicBezierToX: 0.113 y: 0.019 control1X: 0.042 y: 0.000 control2X: 0.080 y: 0.006;
		rel_CubicBezierToX: 0.086 y: 0.055 control1X: 0.034 y: 0.012 control2X: 0.062 y: 0.031;
		rel_LineToX: 0.098 y: -0.093;
		rel_LineToX: 0.041 y: 0.043;
		rel_LineToX: -0.102 y: 0.097;
		rel_CubicBezierToX: 0.035 y: 0.115 control1X: 0.019 y: 0.033 control2X: 0.030 y: 0.072;
		rel_CubicBezierToX: -0.009 y: 0.142 control1X: 0.004 y: 0.043 control2X: 0.001 y: 0.091;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.113 y: -0.018 control1X: -0.042 y: 0.000 control2X: -0.079 y: -0.006;
		rel_CubicBezierToX: -0.087 y: -0.055 control1X: -0.033 y: -0.012 control2X: -0.062 y: -0.031;
		rel_LineToX: -0.098 y: 0.093;
		rel_LineToX: -0.042 y: -0.043;
		rel_LineToX: 0.103 y: -0.098! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics217
	vectorGraphicsEngine
		rel_MoveToX: 0.079 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.089 y: 0.443;
		rel_CubicBezierToX: 0.009 y: 0.169 control1X: -0.016 y: 0.078 control2X: -0.013 y: 0.134;
		rel_CubicBezierToX: 0.127 y: 0.051 control1X: 0.021 y: 0.034 control2X: 0.064 y: 0.051;
		rel_CubicBezierToX: 0.147 y: -0.051 control1X: 0.063 y: 0.000 control2X: 0.112 y: -0.017;
		rel_CubicBezierToX: 0.076 y: -0.169 control1X: 0.035 y: -0.034 control2X: 0.061 y: -0.091;
		rel_LineToX: 0.089 y: -0.443;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.091 y: 0.455;
		rel_CubicBezierToX: -0.114 y: 0.215 control1X: -0.019 y: 0.095 control2X: -0.057 y: 0.167;
		rel_CubicBezierToX: -0.223 y: 0.073 control1X: -0.057 y: 0.048 control2X: -0.131 y: 0.073;
		rel_CubicBezierToX: -0.194 y: -0.073 control1X: -0.092 y: 0.000 control2X: -0.157 y: -0.024;
		rel_CubicBezierToX: -0.027 y: -0.215 control1X: -0.037 y: -0.048 control2X: -0.046 y: -0.120;
		rel_LineToX: 0.091 y: -0.455;
		rel_MoveToX: 0.291 y: -0.198;
		rel_LineToX: 0.070 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.087 y: -0.129;
		rel_LineToX: 0.091 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics218
	vectorGraphicsEngine
		rel_MoveToX: 0.079 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.089 y: 0.443;
		rel_CubicBezierToX: 0.009 y: 0.169 control1X: -0.016 y: 0.078 control2X: -0.013 y: 0.134;
		rel_CubicBezierToX: 0.127 y: 0.051 control1X: 0.021 y: 0.034 control2X: 0.064 y: 0.051;
		rel_CubicBezierToX: 0.147 y: -0.051 control1X: 0.063 y: 0.000 control2X: 0.112 y: -0.017;
		rel_CubicBezierToX: 0.076 y: -0.169 control1X: 0.035 y: -0.034 control2X: 0.061 y: -0.091;
		rel_LineToX: 0.089 y: -0.443;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.091 y: 0.455;
		rel_CubicBezierToX: -0.114 y: 0.215 control1X: -0.019 y: 0.095 control2X: -0.057 y: 0.167;
		rel_CubicBezierToX: -0.223 y: 0.073 control1X: -0.057 y: 0.048 control2X: -0.131 y: 0.073;
		rel_CubicBezierToX: -0.194 y: -0.073 control1X: -0.092 y: 0.000 control2X: -0.157 y: -0.024;
		rel_CubicBezierToX: -0.027 y: -0.215 control1X: -0.037 y: -0.048 control2X: -0.046 y: -0.120;
		rel_LineToX: 0.091 y: -0.455;
		rel_MoveToX: 0.346 y: -0.198;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics219
	vectorGraphicsEngine
		rel_MoveToX: 0.079 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.089 y: 0.443;
		rel_CubicBezierToX: 0.009 y: 0.169 control1X: -0.016 y: 0.078 control2X: -0.013 y: 0.134;
		rel_CubicBezierToX: 0.127 y: 0.051 control1X: 0.021 y: 0.034 control2X: 0.064 y: 0.051;
		rel_CubicBezierToX: 0.147 y: -0.051 control1X: 0.063 y: 0.000 control2X: 0.112 y: -0.017;
		rel_CubicBezierToX: 0.076 y: -0.169 control1X: 0.035 y: -0.034 control2X: 0.061 y: -0.091;
		rel_LineToX: 0.089 y: -0.443;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.091 y: 0.455;
		rel_CubicBezierToX: -0.114 y: 0.215 control1X: -0.019 y: 0.095 control2X: -0.057 y: 0.167;
		rel_CubicBezierToX: -0.223 y: 0.073 control1X: -0.057 y: 0.048 control2X: -0.131 y: 0.073;
		rel_CubicBezierToX: -0.194 y: -0.073 control1X: -0.092 y: 0.000 control2X: -0.157 y: -0.024;
		rel_CubicBezierToX: -0.027 y: -0.215 control1X: -0.037 y: -0.048 control2X: -0.046 y: -0.120;
		rel_LineToX: 0.091 y: -0.455;
		rel_MoveToX: 0.273 y: -0.199;
		rel_LineToX: 0.092 y: 0.000;
		rel_LineToX: 0.077 y: 0.130;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.087;
		rel_LineToX: -0.098 y: 0.087;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.129 y: -0.130! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics220
	vectorGraphicsEngine
		rel_MoveToX: 0.079 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.089 y: 0.443;
		rel_CubicBezierToX: 0.009 y: 0.169 control1X: -0.016 y: 0.078 control2X: -0.013 y: 0.134;
		rel_CubicBezierToX: 0.127 y: 0.051 control1X: 0.021 y: 0.034 control2X: 0.064 y: 0.051;
		rel_CubicBezierToX: 0.147 y: -0.051 control1X: 0.063 y: 0.000 control2X: 0.112 y: -0.017;
		rel_CubicBezierToX: 0.076 y: -0.169 control1X: 0.035 y: -0.034 control2X: 0.061 y: -0.091;
		rel_LineToX: 0.089 y: -0.443;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.091 y: 0.455;
		rel_CubicBezierToX: -0.114 y: 0.215 control1X: -0.019 y: 0.095 control2X: -0.057 y: 0.167;
		rel_CubicBezierToX: -0.223 y: 0.073 control1X: -0.057 y: 0.048 control2X: -0.131 y: 0.073;
		rel_CubicBezierToX: -0.194 y: -0.073 control1X: -0.092 y: 0.000 control2X: -0.157 y: -0.024;
		rel_CubicBezierToX: -0.027 y: -0.215 control1X: -0.037 y: -0.048 control2X: -0.046 y: -0.120;
		rel_LineToX: 0.091 y: -0.455;
		rel_MoveToX: 0.362 y: -0.184;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics221
	vectorGraphicsEngine
		rel_MoveToX: -0.010 y: 0.227;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.142 y: 0.300;
		rel_LineToX: 0.261 y: -0.300;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: -0.334 y: 0.382;
		rel_LineToX: -0.069 y: 0.347;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.069 y: -0.347;
		rel_LineToX: -0.181 y: -0.382;
		rel_MoveToX: 0.375 y: -0.198;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.137 y: 0.129;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.121 y: -0.129! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics222
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.026 y: 0.129;
		rel_LineToX: 0.124 y: 0.000;
		rel_CubicBezierToX: 0.174 y: 0.055 control1X: 0.082 y: 0.000 control2X: 0.140 y: 0.018;
		rel_CubicBezierToX: 0.031 y: 0.162 control1X: 0.035 y: 0.036 control2X: 0.045 y: 0.091;
		rel_CubicBezierToX: -0.096 y: 0.163 control1X: -0.014 y: 0.072 control2X: -0.046 y: 0.126;
		rel_CubicBezierToX: -0.196 y: 0.055 control1X: -0.049 y: 0.037 control2X: -0.114 y: 0.055;
		rel_LineToX: -0.124 y: 0.000;
		rel_LineToX: -0.033 y: 0.165;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_MoveToX: 0.057 y: 0.210;
		rel_LineToX: -0.055 y: 0.273;
		rel_LineToX: 0.124 y: 0.000;
		rel_CubicBezierToX: 0.114 y: -0.036 control1X: 0.046 y: 0.000 control2X: 0.084 y: -0.012;
		rel_CubicBezierToX: 0.058 y: -0.101 control1X: 0.030 y: -0.024 control2X: 0.049 y: -0.057;
		rel_CubicBezierToX: -0.017 y: -0.101 control1X: 0.009 y: -0.044 control2X: 0.003 y: -0.077;
		rel_CubicBezierToX: -0.099 y: -0.036 control1X: -0.020 y: -0.024 control2X: -0.053 y: -0.036;
		rel_LineToX: -0.124 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics223
	vectorGraphicsEngine
		rel_MoveToX: 0.048 y: 0.401;
		rel_CubicBezierToX: 0.088 y: -0.151 control1X: 0.013 y: -0.065 control2X: 0.042 y: -0.115;
		rel_CubicBezierToX: 0.176 y: -0.054 control1X: 0.046 y: -0.036 control2X: 0.105 y: -0.054;
		rel_CubicBezierToX: 0.143 y: 0.057 control1X: 0.068 y: 0.000 control2X: 0.116 y: 0.019;
		rel_CubicBezierToX: 0.022 y: 0.166 control1X: 0.028 y: 0.038 control2X: 0.036 y: 0.093;
		rel_CubicBezierToX: -0.121 y: 0.032 control1X: -0.050 y: 0.003 control2X: -0.090 y: 0.013;
		rel_CubicBezierToX: -0.056 y: 0.075 control1X: -0.031 y: 0.019 control2X: -0.050 y: 0.043;
		rel_CubicBezierToX: 0.006 y: 0.043 control1X: -0.003 y: 0.015 control2X: -0.001 y: 0.030;
		rel_CubicBezierToX: 0.038 y: 0.040 control1X: 0.007 y: 0.013 control2X: 0.019 y: 0.026;
		rel_LineToX: 0.024 y: 0.018;
		rel_CubicBezierToX: 0.087 y: 0.086 control1X: 0.047 y: 0.034 control2X: 0.076 y: 0.063;
		rel_CubicBezierToX: 0.007 y: 0.084 control1X: 0.011 y: 0.023 control2X: 0.013 y: 0.051;
		rel_CubicBezierToX: -0.081 y: 0.129 control1X: -0.011 y: 0.056 control2X: -0.038 y: 0.099;
		rel_CubicBezierToX: -0.167 y: 0.044 control1X: -0.042 y: 0.030 control2X: -0.098 y: 0.044;
		rel_CubicBezierToX: -0.063 y: -0.006 control1X: -0.021 y: 0.000 control2X: -0.042 y: -0.002;
		rel_CubicBezierToX: -0.065 y: -0.018 control1X: -0.021 y: -0.004 control2X: -0.043 y: -0.010;
		rel_LineToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.069 y: 0.022 control1X: 0.024 y: 0.010 control2X: 0.047 y: 0.017;
		rel_CubicBezierToX: 0.065 y: 0.007 control1X: 0.022 y: 0.005 control2X: 0.043 y: 0.007;
		rel_CubicBezierToX: 0.091 y: -0.023 control1X: 0.038 y: 0.000 control2X: 0.068 y: -0.008;
		rel_CubicBezierToX: 0.043 y: -0.068 control1X: 0.023 y: -0.016 control2X: 0.037 y: -0.038;
		rel_CubicBezierToX: -0.004 y: -0.051 control1X: 0.004 y: -0.020 control2X: 0.003 y: -0.038;
		rel_CubicBezierToX: -0.053 y: -0.051 control1X: -0.007 y: -0.014 control2X: -0.024 y: -0.031;
		rel_LineToX: -0.040 y: -0.027;
		rel_CubicBezierToX: -0.056 y: -0.062 control1X: -0.027 y: -0.019 control2X: -0.046 y: -0.040;
		rel_CubicBezierToX: -0.005 y: -0.078 control1X: -0.009 y: -0.022 control2X: -0.011 y: -0.048;
		rel_CubicBezierToX: 0.061 y: -0.104 control1X: 0.008 y: -0.041 control2X: 0.029 y: -0.076;
		rel_CubicBezierToX: 0.126 y: -0.056 control1X: 0.033 y: -0.028 control2X: 0.075 y: -0.047;
		rel_CubicBezierToX: -0.021 y: -0.078 control1X: 0.004 y: -0.033 control2X: -0.003 y: -0.059;
		rel_CubicBezierToX: -0.081 y: -0.027 control1X: -0.018 y: -0.018 control2X: -0.045 y: -0.027;
		rel_CubicBezierToX: -0.104 y: 0.034 control1X: -0.042 y: 0.000 control2X: -0.077 y: 0.011;
		rel_CubicBezierToX: -0.053 y: 0.098 control1X: -0.027 y: 0.023 control2X: -0.044 y: 0.055;
		rel_LineToX: -0.111 y: 0.553;
		rel_LineToX: -0.091 y: 0.000;
		rel_LineToX: 0.111 y: -0.555! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics224
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.205 y: -0.488;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics225
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.066 y: -0.488;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics226
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.170 y: -0.488;
		rel_LineToX: 0.072 y: 0.000;
		rel_LineToX: 0.083 y: 0.184;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.120;
		rel_LineToX: -0.112 y: 0.120;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.156 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics227
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.162 y: -0.357;
		rel_LineToX: -0.023 y: -0.027;
		rel_CubicBezierToX: -0.016 y: -0.014 control1X: -0.006 y: -0.007 control2X: -0.011 y: -0.011;
		rel_CubicBezierToX: -0.013 y: -0.005 control1X: -0.005 y: -0.003 control2X: -0.009 y: -0.005;
		rel_CubicBezierToX: -0.031 y: 0.018 control1X: -0.012 y: 0.000 control2X: -0.023 y: 0.006;
		rel_CubicBezierToX: -0.021 y: 0.058 control1X: -0.008 y: 0.012 control2X: -0.015 y: 0.031;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.046 y: -0.102 control1X: 0.009 y: -0.044 control2X: 0.025 y: -0.078;
		rel_CubicBezierToX: 0.077 y: -0.036 control1X: 0.021 y: -0.024 control2X: 0.047 y: -0.036;
		rel_CubicBezierToX: 0.033 y: 0.007 control1X: 0.012 y: 0.000 control2X: 0.023 y: 0.002;
		rel_CubicBezierToX: 0.029 y: 0.023 control1X: 0.009 y: 0.005 control2X: 0.019 y: 0.012;
		rel_LineToX: 0.022 y: 0.027;
		rel_CubicBezierToX: 0.016 y: 0.015 control1X: 0.006 y: 0.007 control2X: 0.011 y: 0.011;
		rel_CubicBezierToX: 0.014 y: 0.004 control1X: 0.005 y: 0.003 control2X: 0.010 y: 0.004;
		rel_CubicBezierToX: 0.031 y: -0.018 control1X: 0.012 y: 0.000 control2X: 0.023 y: -0.006;
		rel_CubicBezierToX: 0.021 y: -0.058 control1X: 0.008 y: -0.012 control2X: 0.015 y: -0.031;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.046 y: 0.102 control1X: -0.009 y: 0.044 control2X: -0.025 y: 0.078;
		rel_CubicBezierToX: -0.077 y: 0.036 control1X: -0.021 y: 0.024 control2X: -0.047 y: 0.036;
		rel_CubicBezierToX: -0.033 y: -0.007 control1X: -0.012 y: 0.000 control2X: -0.023 y: -0.002;
		rel_CubicBezierToX: -0.029 y: -0.023 control1X: -0.009 y: -0.005 control2X: -0.019 y: -0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics228
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.097 y: -0.446;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics229
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103;
		rel_MoveToX: -0.071 y: -0.432;
		rel_CubicBezierToX: -0.011 y: -0.052 control1X: 0.004 y: -0.020 control2X: 0.000 y: -0.038;
		rel_CubicBezierToX: -0.048 y: -0.021 control1X: -0.011 y: -0.014 control2X: -0.028 y: -0.021;
		rel_CubicBezierToX: -0.058 y: 0.021 control1X: -0.021 y: 0.000 control2X: -0.040 y: 0.007;
		rel_CubicBezierToX: -0.031 y: 0.052 control1X: -0.017 y: 0.014 control2X: -0.027 y: 0.031;
		rel_CubicBezierToX: 0.010 y: 0.053 control1X: -0.004 y: 0.021 control2X: -0.001 y: 0.039;
		rel_CubicBezierToX: 0.049 y: 0.021 control1X: 0.011 y: 0.014 control2X: 0.028 y: 0.021;
		rel_CubicBezierToX: 0.057 y: -0.022 control1X: 0.021 y: 0.000 control2X: 0.040 y: -0.007;
		rel_CubicBezierToX: 0.032 y: -0.053 control1X: 0.017 y: -0.014 control2X: 0.028 y: -0.032;
		rel_MoveToX: 0.060 y: 0.000;
		rel_CubicBezierToX: -0.058 y: 0.095 control1X: -0.008 y: 0.038 control2X: -0.027 y: 0.069;
		rel_CubicBezierToX: -0.103 y: 0.039 control1X: -0.031 y: 0.026 control2X: -0.065 y: 0.039;
		rel_CubicBezierToX: -0.087 y: -0.039 control1X: -0.037 y: 0.000 control2X: -0.067 y: -0.013;
		rel_CubicBezierToX: -0.020 y: -0.095 control1X: -0.020 y: -0.026 control2X: -0.027 y: -0.058;
		rel_CubicBezierToX: 0.058 y: -0.095 control1X: 0.007 y: -0.037 control2X: 0.027 y: -0.069;
		rel_CubicBezierToX: 0.103 y: -0.039 control1X: 0.031 y: -0.026 control2X: 0.066 y: -0.039;
		rel_CubicBezierToX: 0.087 y: 0.039 control1X: 0.037 y: 0.000 control2X: 0.066 y: 0.013;
		rel_CubicBezierToX: 0.020 y: 0.095 control1X: 0.021 y: 0.026 control2X: 0.028 y: 0.057! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics230
	vectorGraphicsEngine
		rel_MoveToX: 0.750 y: 0.634;
		rel_CubicBezierToX: -0.017 y: -0.118 control1X: 0.010 y: -0.049 control2X: 0.004 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.054 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: -0.323 y: -0.148;
		rel_CubicBezierToX: 0.101 y: -0.067 control1X: 0.030 y: -0.029 control2X: 0.064 y: -0.052;
		rel_CubicBezierToX: 0.121 y: -0.023 control1X: 0.038 y: -0.015 control2X: 0.078 y: -0.023;
		rel_CubicBezierToX: 0.161 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.009 y: 0.062 control2X: 0.000 y: 0.110;
		rel_CubicBezierToX: 0.129 y: 0.049 control1X: 0.027 y: 0.033 control2X: 0.070 y: 0.049;
		rel_CubicBezierToX: 0.102 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.068 y: -0.004;
		rel_CubicBezierToX: 0.106 y: -0.039 control1X: 0.034 y: -0.009 control2X: 0.069 y: -0.022;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.106 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.071 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.136 y: -0.029 control1X: -0.054 y: 0.000 control2X: -0.100 y: -0.010;
		rel_CubicBezierToX: -0.081 y: -0.088 control1X: -0.036 y: -0.020 control2X: -0.063 y: -0.049;
		rel_CubicBezierToX: -0.106 y: 0.088 control1X: -0.032 y: 0.039 control2X: -0.067 y: 0.068;
		rel_CubicBezierToX: -0.128 y: 0.029 control1X: -0.039 y: 0.020 control2X: -0.082 y: 0.029;
		rel_CubicBezierToX: -0.134 y: -0.045 control1X: -0.061 y: 0.000 control2X: -0.106 y: -0.015;
		rel_CubicBezierToX: -0.026 y: -0.128 control1X: -0.028 y: -0.031 control2X: -0.037 y: -0.073;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.116 y: 0.024 control1X: 0.046 y: 0.000 control2X: 0.085 y: 0.008;
		rel_CubicBezierToX: 0.065 y: 0.066 control1X: 0.032 y: 0.016 control2X: 0.053 y: 0.038;
		rel_MoveToX: -0.182 y: 0.195;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics231
	vectorGraphicsEngine
		rel_MoveToX: 0.440 y: 0.430;
		rel_LineToX: -0.017 y: 0.084;
		rel_CubicBezierToX: -0.070 y: -0.031 control1X: -0.023 y: -0.014 control2X: -0.046 y: -0.024;
		rel_CubicBezierToX: -0.075 y: -0.011 control1X: -0.024 y: -0.007 control2X: -0.049 y: -0.011;
		rel_CubicBezierToX: -0.147 y: 0.056 control1X: -0.058 y: 0.000 control2X: -0.107 y: 0.019;
		rel_CubicBezierToX: -0.079 y: 0.155 control1X: -0.040 y: 0.037 control2X: -0.066 y: 0.089;
		rel_CubicBezierToX: 0.017 y: 0.156 control1X: -0.013 y: 0.067 control2X: -0.008 y: 0.119;
		rel_CubicBezierToX: 0.125 y: 0.055 control1X: 0.025 y: 0.037 control2X: 0.066 y: 0.055;
		rel_CubicBezierToX: 0.079 y: -0.010 control1X: 0.026 y: 0.000 control2X: 0.052 y: -0.003;
		rel_CubicBezierToX: 0.083 y: -0.032 control1X: 0.027 y: -0.007 control2X: 0.055 y: -0.018;
		rel_LineToX: -0.017 y: 0.083;
		rel_CubicBezierToX: -0.083 y: 0.026 control1X: -0.027 y: 0.012 control2X: -0.055 y: 0.020;
		rel_CubicBezierToX: -0.087 y: 0.009 control1X: -0.028 y: 0.006 control2X: -0.057 y: 0.009;
		rel_CubicBezierToX: -0.181 y: -0.078 control1X: -0.082 y: 0.000 control2X: -0.143 y: -0.026;
		rel_CubicBezierToX: -0.031 y: -0.210 control1X: -0.038 y: -0.052 control2X: -0.048 y: -0.122;
		rel_CubicBezierToX: 0.115 y: -0.210 control1X: 0.018 y: -0.089 control2X: 0.056 y: -0.159;
		rel_CubicBezierToX: 0.217 y: -0.077 control1X: 0.059 y: -0.051 control2X: 0.132 y: -0.077;
		rel_CubicBezierToX: 0.079 y: 0.009 control1X: 0.028 y: 0.000 control2X: 0.054 y: 0.003;
		rel_CubicBezierToX: 0.072 y: 0.025 control1X: 0.025 y: 0.006 control2X: 0.049 y: 0.014;
		rel_MoveToX: -0.232 y: 0.526;
		rel_CubicBezierToX: 0.029 y: 0.056 control1X: 0.014 y: 0.020 control2X: 0.024 y: 0.039;
		rel_CubicBezierToX: 0.003 y: 0.049 control1X: 0.005 y: 0.017 control2X: 0.006 y: 0.034;
		rel_CubicBezierToX: -0.042 y: 0.065 control1X: -0.006 y: 0.029 control2X: -0.020 y: 0.051;
		rel_CubicBezierToX: -0.091 y: 0.022 control1X: -0.023 y: 0.015 control2X: -0.053 y: 0.022;
		rel_CubicBezierToX: -0.043 y: -0.003 control1X: -0.015 y: 0.000 control2X: -0.029 y: -0.001;
		rel_CubicBezierToX: -0.040 y: -0.009 control1X: -0.014 y: -0.002 control2X: -0.027 y: -0.005;
		rel_LineToX: 0.013 y: -0.064;
		rel_CubicBezierToX: 0.032 y: 0.012 control1X: 0.010 y: 0.006 control2X: 0.021 y: 0.009;
		rel_CubicBezierToX: 0.040 y: 0.004 control1X: 0.012 y: 0.003 control2X: 0.025 y: 0.004;
		rel_CubicBezierToX: 0.046 y: -0.012 control1X: 0.019 y: 0.000 control2X: 0.034 y: -0.004;
		rel_CubicBezierToX: 0.022 y: -0.034 control1X: 0.011 y: -0.008 control2X: 0.019 y: -0.019;
		rel_CubicBezierToX: -0.004 y: -0.036 control1X: 0.002 y: -0.010 control2X: 0.001 y: -0.022;
		rel_CubicBezierToX: -0.022 y: -0.051 control1X: -0.004 y: -0.014 control2X: -0.011 y: -0.031;
		rel_LineToX: 0.058 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics232
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.660;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.130 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.105 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.070 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.192 y: -0.076 control1X: -0.087 y: 0.000 control2X: -0.151 y: -0.025;
		rel_CubicBezierToX: -0.035 y: -0.206 control1X: -0.041 y: -0.051 control2X: -0.052 y: -0.119;
		rel_CubicBezierToX: 0.115 y: -0.213 control1X: 0.018 y: -0.090 control2X: 0.056 y: -0.160;
		rel_CubicBezierToX: 0.212 y: -0.079 control1X: 0.059 y: -0.053 control2X: 0.130 y: -0.079;
		rel_CubicBezierToX: 0.160 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.085 y: -0.026;
		rel_CubicBezierToX: -0.018 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.053 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: -0.130 y: -0.478;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics233
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.660;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.130 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.105 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.070 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.192 y: -0.076 control1X: -0.087 y: 0.000 control2X: -0.151 y: -0.025;
		rel_CubicBezierToX: -0.035 y: -0.206 control1X: -0.041 y: -0.051 control2X: -0.052 y: -0.119;
		rel_CubicBezierToX: 0.115 y: -0.213 control1X: 0.018 y: -0.090 control2X: 0.056 y: -0.160;
		rel_CubicBezierToX: 0.212 y: -0.079 control1X: 0.059 y: -0.053 control2X: 0.130 y: -0.079;
		rel_CubicBezierToX: 0.160 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.085 y: -0.026;
		rel_CubicBezierToX: -0.018 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.053 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: 0.009 y: -0.478;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics234
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.660;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.130 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.105 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.070 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.192 y: -0.076 control1X: -0.087 y: 0.000 control2X: -0.151 y: -0.025;
		rel_CubicBezierToX: -0.035 y: -0.206 control1X: -0.041 y: -0.051 control2X: -0.052 y: -0.119;
		rel_CubicBezierToX: 0.115 y: -0.213 control1X: 0.018 y: -0.090 control2X: 0.056 y: -0.160;
		rel_CubicBezierToX: 0.212 y: -0.079 control1X: 0.059 y: -0.053 control2X: 0.130 y: -0.079;
		rel_CubicBezierToX: 0.160 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.085 y: -0.026;
		rel_CubicBezierToX: -0.018 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.053 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: -0.095 y: -0.478;
		rel_LineToX: 0.072 y: 0.000;
		rel_LineToX: 0.083 y: 0.184;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.120;
		rel_LineToX: -0.112 y: 0.120;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.156 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics235
	vectorGraphicsEngine
		rel_MoveToX: 0.468 y: 0.660;
		rel_LineToX: -0.009 y: 0.044;
		rel_LineToX: -0.413 y: 0.000;
		rel_CubicBezierToX: 0.027 y: 0.142 control1X: -0.008 y: 0.062 control2X: 0.001 y: 0.109;
		rel_CubicBezierToX: 0.130 y: 0.048 control1X: 0.027 y: 0.032 control2X: 0.070 y: 0.048;
		rel_CubicBezierToX: 0.103 y: -0.013 control1X: 0.034 y: 0.000 control2X: 0.069 y: -0.004;
		rel_CubicBezierToX: 0.104 y: -0.038 control1X: 0.034 y: -0.008 control2X: 0.069 y: -0.021;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.105 y: 0.031 control1X: -0.035 y: 0.014 control2X: -0.070 y: 0.024;
		rel_CubicBezierToX: -0.105 y: 0.011 control1X: -0.035 y: 0.007 control2X: -0.070 y: 0.011;
		rel_CubicBezierToX: -0.192 y: -0.076 control1X: -0.087 y: 0.000 control2X: -0.151 y: -0.025;
		rel_CubicBezierToX: -0.035 y: -0.206 control1X: -0.041 y: -0.051 control2X: -0.052 y: -0.119;
		rel_CubicBezierToX: 0.115 y: -0.213 control1X: 0.018 y: -0.090 control2X: 0.056 y: -0.160;
		rel_CubicBezierToX: 0.212 y: -0.079 control1X: 0.059 y: -0.053 control2X: 0.130 y: -0.079;
		rel_CubicBezierToX: 0.160 y: 0.071 control1X: 0.074 y: 0.000 control2X: 0.127 y: 0.024;
		rel_CubicBezierToX: 0.026 y: 0.193 control1X: 0.034 y: 0.047 control2X: 0.042 y: 0.112;
		rel_MoveToX: -0.085 y: -0.026;
		rel_CubicBezierToX: -0.018 y: -0.118 control1X: 0.009 y: -0.049 control2X: 0.003 y: -0.088;
		rel_CubicBezierToX: -0.098 y: -0.044 control1X: -0.021 y: -0.029 control2X: -0.053 y: -0.044;
		rel_CubicBezierToX: -0.129 y: 0.043 control1X: -0.050 y: 0.000 control2X: -0.093 y: 0.014;
		rel_CubicBezierToX: -0.076 y: 0.120 control1X: -0.036 y: 0.028 control2X: -0.061 y: 0.068;
		rel_LineToX: 0.320 y: 0.000;
		rel_MoveToX: -0.021 y: -0.436;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics236
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_MoveToX: 0.025 y: -0.253;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics237
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_MoveToX: 0.163 y: -0.253;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics238
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_MoveToX: 0.059 y: -0.253;
		rel_LineToX: 0.072 y: 0.000;
		rel_LineToX: 0.083 y: 0.184;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.120;
		rel_LineToX: -0.112 y: 0.120;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.156 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics239
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.409;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_MoveToX: 0.133 y: -0.211;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics240
	vectorGraphicsEngine
		rel_MoveToX: 0.345 y: 0.509;
		rel_CubicBezierToX: -0.043 y: -0.012 control1X: -0.015 y: -0.005 control2X: -0.030 y: -0.010;
		rel_CubicBezierToX: -0.041 y: -0.004 control1X: -0.014 y: -0.003 control2X: -0.027 y: -0.004;
		rel_CubicBezierToX: -0.137 y: 0.053 control1X: -0.054 y: 0.000 control2X: -0.100 y: 0.018;
		rel_CubicBezierToX: -0.075 y: 0.148 control1X: -0.037 y: 0.035 control2X: -0.062 y: 0.085;
		rel_CubicBezierToX: 0.013 y: 0.146 control1X: -0.012 y: 0.061 control2X: -0.008 y: 0.110;
		rel_CubicBezierToX: 0.103 y: 0.054 control1X: 0.021 y: 0.036 control2X: 0.055 y: 0.054;
		rel_CubicBezierToX: 0.124 y: -0.054 control1X: 0.047 y: 0.000 control2X: 0.089 y: -0.018;
		rel_CubicBezierToX: 0.072 y: -0.146 control1X: 0.036 y: -0.036 control2X: 0.059 y: -0.085;
		rel_CubicBezierToX: 0.008 y: -0.106 control1X: 0.008 y: -0.040 control2X: 0.011 y: -0.075;
		rel_CubicBezierToX: -0.024 y: -0.079 control1X: -0.003 y: -0.031 control2X: -0.011 y: -0.057;
		rel_MoveToX: 0.053 y: -0.110;
		rel_CubicBezierToX: 0.061 y: 0.140 control1X: 0.032 y: 0.046 control2X: 0.052 y: 0.093;
		rel_CubicBezierToX: -0.003 y: 0.155 control1X: 0.009 y: 0.047 control2X: 0.008 y: 0.099;
		rel_CubicBezierToX: -0.109 y: 0.201 control1X: -0.017 y: 0.083 control2X: -0.053 y: 0.150;
		rel_CubicBezierToX: -0.197 y: 0.076 control1X: -0.056 y: 0.050 control2X: -0.122 y: 0.076;
		rel_CubicBezierToX: -0.167 y: -0.076 control1X: -0.075 y: 0.000 control2X: -0.131 y: -0.025;
		rel_CubicBezierToX: -0.029 y: -0.201 control1X: -0.036 y: -0.051 control2X: -0.045 y: -0.118;
		rel_CubicBezierToX: 0.107 y: -0.198 control1X: 0.016 y: -0.081 control2X: 0.052 y: -0.147;
		rel_CubicBezierToX: 0.191 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.119 y: -0.076;
		rel_CubicBezierToX: 0.021 y: 0.002 control1X: 0.006 y: 0.000 control2X: 0.013 y: 0.001;
		rel_CubicBezierToX: 0.033 y: 0.003 control1X: 0.008 y: 0.001 control2X: 0.019 y: 0.002;
		rel_LineToX: -0.060 y: -0.088;
		rel_LineToX: -0.166 y: 0.052;
		rel_LineToX: -0.006 y: -0.048;
		rel_LineToX: 0.146 y: -0.045;
		rel_LineToX: -0.068 y: -0.100;
		rel_LineToX: 0.111 y: 0.000;
		rel_LineToX: 0.048 y: 0.071;
		rel_LineToX: 0.173 y: -0.054;
		rel_LineToX: 0.007 y: 0.047;
		rel_LineToX: -0.154 y: 0.048;
		rel_LineToX: 0.062 y: 0.091! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics241
	vectorGraphicsEngine
		rel_MoveToX: 0.461 y: 0.626;
		rel_LineToX: -0.066 y: 0.330;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.065 y: -0.327;
		rel_CubicBezierToX: -0.007 y: -0.116 control1X: 0.010 y: -0.052 control2X: 0.008 y: -0.091;
		rel_CubicBezierToX: -0.083 y: -0.039 control1X: -0.015 y: -0.026 control2X: -0.043 y: -0.039;
		rel_CubicBezierToX: -0.124 y: 0.046 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.016;
		rel_CubicBezierToX: -0.067 y: 0.127 control1X: -0.034 y: 0.031 control2X: -0.057 y: 0.073;
		rel_LineToX: -0.062 y: 0.309;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.109 y: -0.547;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: 0.090 y: -0.074 control1X: 0.028 y: -0.033 control2X: 0.058 y: -0.057;
		rel_CubicBezierToX: 0.106 y: -0.024 control1X: 0.033 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.131 y: 0.059 control1X: 0.063 y: 0.000 control2X: 0.106 y: 0.020;
		rel_CubicBezierToX: 0.014 y: 0.171 control1X: 0.024 y: 0.039 control2X: 0.029 y: 0.096;
		rel_MoveToX: -0.159 y: -0.339;
		rel_LineToX: -0.022 y: -0.027;
		rel_CubicBezierToX: -0.016 y: -0.014 control1X: -0.006 y: -0.007 control2X: -0.011 y: -0.011;
		rel_CubicBezierToX: -0.013 y: -0.005 control1X: -0.005 y: -0.003 control2X: -0.009 y: -0.005;
		rel_CubicBezierToX: -0.031 y: 0.018 control1X: -0.012 y: 0.000 control2X: -0.023 y: 0.006;
		rel_CubicBezierToX: -0.021 y: 0.058 control1X: -0.008 y: 0.012 control2X: -0.015 y: 0.031;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.046 y: -0.102 control1X: 0.009 y: -0.044 control2X: 0.025 y: -0.078;
		rel_CubicBezierToX: 0.077 y: -0.036 control1X: 0.021 y: -0.024 control2X: 0.047 y: -0.036;
		rel_CubicBezierToX: 0.033 y: 0.007 control1X: 0.012 y: 0.000 control2X: 0.023 y: 0.002;
		rel_CubicBezierToX: 0.029 y: 0.023 control1X: 0.010 y: 0.005 control2X: 0.019 y: 0.012;
		rel_LineToX: 0.023 y: 0.027;
		rel_CubicBezierToX: 0.016 y: 0.015 control1X: 0.006 y: 0.007 control2X: 0.011 y: 0.011;
		rel_CubicBezierToX: 0.014 y: 0.004 control1X: 0.005 y: 0.003 control2X: 0.010 y: 0.004;
		rel_CubicBezierToX: 0.031 y: -0.018 control1X: 0.012 y: 0.000 control2X: 0.023 y: -0.006;
		rel_CubicBezierToX: 0.021 y: -0.058 control1X: 0.008 y: -0.012 control2X: 0.015 y: -0.031;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.046 y: 0.102 control1X: -0.009 y: 0.044 control2X: -0.025 y: 0.078;
		rel_CubicBezierToX: -0.076 y: 0.036 control1X: -0.021 y: 0.024 control2X: -0.047 y: 0.036;
		rel_CubicBezierToX: -0.033 y: -0.007 control1X: -0.012 y: 0.000 control2X: -0.023 y: -0.002;
		rel_CubicBezierToX: -0.029 y: -0.023 control1X: -0.009 y: -0.005 control2X: -0.019 y: -0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics242
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_MoveToX: -0.023 y: -0.240;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics243
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_MoveToX: 0.116 y: -0.240;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics244
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_MoveToX: 0.012 y: -0.240;
		rel_LineToX: 0.072 y: 0.000;
		rel_LineToX: 0.083 y: 0.184;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.120;
		rel_LineToX: -0.112 y: 0.120;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.156 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics245
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_MoveToX: 0.020 y: -0.109;
		rel_LineToX: -0.023 y: -0.027;
		rel_CubicBezierToX: -0.016 y: -0.014 control1X: -0.006 y: -0.007 control2X: -0.011 y: -0.011;
		rel_CubicBezierToX: -0.013 y: -0.005 control1X: -0.005 y: -0.003 control2X: -0.009 y: -0.005;
		rel_CubicBezierToX: -0.031 y: 0.018 control1X: -0.012 y: 0.000 control2X: -0.023 y: 0.006;
		rel_CubicBezierToX: -0.021 y: 0.058 control1X: -0.008 y: 0.012 control2X: -0.015 y: 0.031;
		rel_LineToX: -0.061 y: 0.000;
		rel_CubicBezierToX: 0.046 y: -0.102 control1X: 0.009 y: -0.044 control2X: 0.025 y: -0.078;
		rel_CubicBezierToX: 0.077 y: -0.036 control1X: 0.021 y: -0.024 control2X: 0.047 y: -0.036;
		rel_CubicBezierToX: 0.033 y: 0.007 control1X: 0.012 y: 0.000 control2X: 0.023 y: 0.002;
		rel_CubicBezierToX: 0.029 y: 0.023 control1X: 0.009 y: 0.005 control2X: 0.019 y: 0.012;
		rel_LineToX: 0.022 y: 0.027;
		rel_CubicBezierToX: 0.016 y: 0.015 control1X: 0.006 y: 0.007 control2X: 0.011 y: 0.011;
		rel_CubicBezierToX: 0.014 y: 0.004 control1X: 0.005 y: 0.003 control2X: 0.010 y: 0.004;
		rel_CubicBezierToX: 0.031 y: -0.018 control1X: 0.012 y: 0.000 control2X: 0.023 y: -0.006;
		rel_CubicBezierToX: 0.021 y: -0.058 control1X: 0.008 y: -0.012 control2X: 0.015 y: -0.031;
		rel_LineToX: 0.061 y: 0.000;
		rel_CubicBezierToX: -0.046 y: 0.102 control1X: -0.010 y: 0.044 control2X: -0.025 y: 0.078;
		rel_CubicBezierToX: -0.077 y: 0.036 control1X: -0.021 y: 0.024 control2X: -0.047 y: 0.036;
		rel_CubicBezierToX: -0.033 y: -0.007 control1X: -0.012 y: 0.000 control2X: -0.023 y: -0.002;
		rel_CubicBezierToX: -0.029 y: -0.023 control1X: -0.010 y: -0.005 control2X: -0.019 y: -0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics246
	vectorGraphicsEngine
		rel_MoveToX: 0.249 y: 0.472;
		rel_CubicBezierToX: -0.126 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.090 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.154 control1X: -0.036 y: 0.037 control2X: -0.060 y: 0.089;
		rel_CubicBezierToX: 0.011 y: 0.155 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.117;
		rel_CubicBezierToX: 0.103 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.055 y: 0.056;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: -0.011 y: -0.154 control1X: 0.013 y: -0.065 control2X: 0.009 y: -0.116;
		rel_CubicBezierToX: -0.102 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.054 y: -0.057;
		rel_MoveToX: 0.015 y: -0.076;
		rel_CubicBezierToX: 0.169 y: 0.076 control1X: 0.078 y: 0.000 control2X: 0.134 y: 0.025;
		rel_CubicBezierToX: 0.025 y: 0.211 control1X: 0.034 y: 0.051 control2X: 0.043 y: 0.121;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.169 y: -0.076 control1X: -0.078 y: 0.000 control2X: -0.135 y: -0.025;
		rel_CubicBezierToX: -0.024 y: -0.211 control1X: -0.034 y: -0.051 control2X: -0.042 y: -0.121;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_MoveToX: 0.085 y: -0.198;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics247
	vectorGraphicsEngine
		rel_MoveToX: 0.316 y: 0.402;
		rel_LineToX: 0.120 y: 0.000;
		rel_LineToX: -0.024 y: 0.120;
		rel_LineToX: -0.120 y: 0.000;
		rel_LineToX: 0.024 y: -0.120;
		rel_MoveToX: -0.072 y: 0.361;
		rel_LineToX: 0.120 y: 0.000;
		rel_LineToX: -0.024 y: 0.120;
		rel_LineToX: -0.120 y: 0.000;
		rel_LineToX: 0.024 y: -0.120;
		rel_MoveToX: -0.221 y: -0.162;
		rel_LineToX: 0.626 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.626 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics248
	vectorGraphicsEngine
		rel_MoveToX: 0.367 y: 0.565;
		rel_LineToX: -0.296 y: 0.289;
		rel_CubicBezierToX: 0.039 y: 0.030 control1X: 0.011 y: 0.014 control2X: 0.024 y: 0.024;
		rel_CubicBezierToX: 0.055 y: 0.010 control1X: 0.016 y: 0.006 control2X: 0.034 y: 0.010;
		rel_CubicBezierToX: 0.125 y: -0.057 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.019;
		rel_CubicBezierToX: 0.073 y: -0.154 control1X: 0.036 y: -0.038 control2X: 0.060 y: -0.089;
		rel_CubicBezierToX: 0.008 y: -0.067 control1X: 0.005 y: -0.026 control2X: 0.008 y: -0.048;
		rel_CubicBezierToX: -0.005 y: -0.051 control1X: 0.001 y: -0.019 control2X: -0.001 y: -0.036;
		rel_MoveToX: -0.024 y: -0.053;
		rel_CubicBezierToX: -0.040 y: -0.030 control1X: -0.011 y: -0.013 control2X: -0.024 y: -0.023;
		rel_CubicBezierToX: -0.053 y: -0.010 control1X: -0.015 y: -0.007 control2X: -0.033 y: -0.010;
		rel_CubicBezierToX: -0.127 y: 0.058 control1X: -0.049 y: 0.000 control2X: -0.091 y: 0.019;
		rel_CubicBezierToX: -0.073 y: 0.160 control1X: -0.035 y: 0.038 control2X: -0.059 y: 0.092;
		rel_CubicBezierToX: -0.008 y: 0.062 control1X: -0.005 y: 0.023 control2X: -0.007 y: 0.044;
		rel_CubicBezierToX: 0.005 y: 0.049 control1X: 0.000 y: 0.018 control2X: 0.001 y: 0.034;
		rel_LineToX: 0.296 y: -0.289;
		rel_MoveToX: -0.372 y: 0.363;
		rel_CubicBezierToX: -0.023 y: -0.085 control1X: -0.013 y: -0.025 control2X: -0.020 y: -0.053;
		rel_CubicBezierToX: 0.008 y: -0.107 control1X: -0.002 y: -0.032 control2X: 0.000 y: -0.068;
		rel_CubicBezierToX: 0.109 y: -0.211 control1X: 0.018 y: -0.090 control2X: 0.054 y: -0.160;
		rel_CubicBezierToX: 0.200 y: -0.076 control1X: 0.055 y: -0.051 control2X: 0.121 y: -0.076;
		rel_CubicBezierToX: 0.081 y: 0.013 control1X: 0.030 y: 0.000 control2X: 0.057 y: 0.004;
		rel_CubicBezierToX: 0.064 y: 0.038 control1X: 0.024 y: 0.008 control2X: 0.046 y: 0.021;
		rel_LineToX: 0.084 y: -0.082;
		rel_LineToX: 0.039 y: 0.039;
		rel_LineToX: -0.089 y: 0.087;
		rel_CubicBezierToX: 0.022 y: 0.086 control1X: 0.012 y: 0.025 control2X: 0.020 y: 0.054;
		rel_CubicBezierToX: -0.008 y: 0.107 control1X: 0.002 y: 0.032 control2X: 0.000 y: 0.068;
		rel_CubicBezierToX: -0.109 y: 0.211 control1X: -0.018 y: 0.090 control2X: -0.054 y: 0.160;
		rel_CubicBezierToX: -0.199 y: 0.076 control1X: -0.055 y: 0.051 control2X: -0.121 y: 0.076;
		rel_CubicBezierToX: -0.084 y: -0.013 control1X: -0.031 y: 0.000 control2X: -0.059 y: -0.004;
		rel_CubicBezierToX: -0.062 y: -0.038 control1X: -0.024 y: -0.008 control2X: -0.045 y: -0.021;
		rel_LineToX: -0.084 y: 0.082;
		rel_LineToX: -0.039 y: -0.038;
		rel_LineToX: 0.091 y: -0.089! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics249
	vectorGraphicsEngine
		rel_MoveToX: -0.025 y: 0.740;
		rel_LineToX: 0.066 y: -0.331;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.066 y: 0.328;
		rel_CubicBezierToX: 0.007 y: 0.117 control1X: -0.010 y: 0.052 control2X: -0.008 y: 0.091;
		rel_CubicBezierToX: 0.083 y: 0.039 control1X: 0.015 y: 0.026 control2X: 0.043 y: 0.039;
		rel_CubicBezierToX: 0.124 y: -0.046 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.015;
		rel_CubicBezierToX: 0.068 y: -0.126 control1X: 0.034 y: -0.031 control2X: 0.057 y: -0.073;
		rel_LineToX: 0.062 y: -0.310;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.084;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.028 y: 0.033 control2X: -0.059 y: 0.058;
		rel_CubicBezierToX: -0.105 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.067 y: 0.024;
		rel_CubicBezierToX: -0.131 y: -0.059 control1X: -0.063 y: 0.000 control2X: -0.107 y: -0.019;
		rel_CubicBezierToX: -0.015 y: -0.171 control1X: -0.025 y: -0.039 control2X: -0.030 y: -0.096;
		rel_MoveToX: 0.271 y: -0.584;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics250
	vectorGraphicsEngine
		rel_MoveToX: -0.025 y: 0.740;
		rel_LineToX: 0.066 y: -0.331;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.066 y: 0.328;
		rel_CubicBezierToX: 0.007 y: 0.117 control1X: -0.010 y: 0.052 control2X: -0.008 y: 0.091;
		rel_CubicBezierToX: 0.083 y: 0.039 control1X: 0.015 y: 0.026 control2X: 0.043 y: 0.039;
		rel_CubicBezierToX: 0.124 y: -0.046 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.015;
		rel_CubicBezierToX: 0.068 y: -0.126 control1X: 0.034 y: -0.031 control2X: 0.057 y: -0.073;
		rel_LineToX: 0.062 y: -0.310;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.084;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.028 y: 0.033 control2X: -0.059 y: 0.058;
		rel_CubicBezierToX: -0.105 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.067 y: 0.024;
		rel_CubicBezierToX: -0.131 y: -0.059 control1X: -0.063 y: 0.000 control2X: -0.107 y: -0.019;
		rel_CubicBezierToX: -0.015 y: -0.171 control1X: -0.025 y: -0.039 control2X: -0.030 y: -0.096;
		rel_MoveToX: 0.410 y: -0.584;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics251
	vectorGraphicsEngine
		rel_MoveToX: -0.025 y: 0.740;
		rel_LineToX: 0.066 y: -0.331;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.066 y: 0.328;
		rel_CubicBezierToX: 0.007 y: 0.117 control1X: -0.010 y: 0.052 control2X: -0.008 y: 0.091;
		rel_CubicBezierToX: 0.083 y: 0.039 control1X: 0.015 y: 0.026 control2X: 0.043 y: 0.039;
		rel_CubicBezierToX: 0.124 y: -0.046 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.015;
		rel_CubicBezierToX: 0.068 y: -0.126 control1X: 0.034 y: -0.031 control2X: 0.057 y: -0.073;
		rel_LineToX: 0.062 y: -0.310;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.084;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.028 y: 0.033 control2X: -0.059 y: 0.058;
		rel_CubicBezierToX: -0.105 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.067 y: 0.024;
		rel_CubicBezierToX: -0.131 y: -0.059 control1X: -0.063 y: 0.000 control2X: -0.107 y: -0.019;
		rel_CubicBezierToX: -0.015 y: -0.171 control1X: -0.025 y: -0.039 control2X: -0.030 y: -0.096;
		rel_MoveToX: 0.306 y: -0.584;
		rel_LineToX: 0.072 y: 0.000;
		rel_LineToX: 0.083 y: 0.184;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: -0.064 y: -0.120;
		rel_LineToX: -0.112 y: 0.120;
		rel_LineToX: -0.068 y: 0.000;
		rel_LineToX: 0.156 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics252
	vectorGraphicsEngine
		rel_MoveToX: -0.025 y: 0.740;
		rel_LineToX: 0.066 y: -0.331;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.066 y: 0.328;
		rel_CubicBezierToX: 0.007 y: 0.117 control1X: -0.010 y: 0.052 control2X: -0.008 y: 0.091;
		rel_CubicBezierToX: 0.083 y: 0.039 control1X: 0.015 y: 0.026 control2X: 0.043 y: 0.039;
		rel_CubicBezierToX: 0.124 y: -0.046 control1X: 0.048 y: 0.000 control2X: 0.090 y: -0.015;
		rel_CubicBezierToX: 0.068 y: -0.126 control1X: 0.034 y: -0.031 control2X: 0.057 y: -0.073;
		rel_LineToX: 0.062 y: -0.310;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.109 y: 0.547;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.084;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.028 y: 0.033 control2X: -0.059 y: 0.058;
		rel_CubicBezierToX: -0.105 y: 0.024 control1X: -0.032 y: 0.016 control2X: -0.067 y: 0.024;
		rel_CubicBezierToX: -0.131 y: -0.059 control1X: -0.063 y: 0.000 control2X: -0.107 y: -0.019;
		rel_CubicBezierToX: -0.015 y: -0.171 control1X: -0.025 y: -0.039 control2X: -0.030 y: -0.096;
		rel_MoveToX: 0.379 y: -0.542;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics253
	vectorGraphicsEngine
		rel_MoveToX: 0.158 y: 1.007;
		rel_CubicBezierToX: -0.100 y: 0.127 control1X: -0.038 y: 0.065 control2X: -0.072 y: 0.107;
		rel_CubicBezierToX: -0.103 y: 0.030 control1X: -0.028 y: 0.020 control2X: -0.062 y: 0.030;
		rel_LineToX: -0.072 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.053 y: 0.000;
		rel_CubicBezierToX: 0.061 y: -0.018 control1X: 0.025 y: 0.000 control2X: 0.045 y: -0.006;
		rel_CubicBezierToX: 0.062 y: -0.083 control1X: 0.016 y: -0.012 control2X: 0.037 y: -0.039;
		rel_LineToX: 0.024 y: -0.041;
		rel_LineToX: -0.113 y: -0.538;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: 0.085 y: 0.428;
		rel_LineToX: 0.256 y: -0.428;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: -0.360 y: 0.598;
		rel_MoveToX: 0.212 y: -0.851;
		rel_LineToX: 0.097 y: 0.000;
		rel_LineToX: -0.196 y: 0.184;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: 0.173 y: -0.184! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics254
	vectorGraphicsEngine
		rel_MoveToX: 0.044 y: 0.874;
		rel_LineToX: -0.058 y: 0.290;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.194 y: -0.968;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.059 y: 0.296;
		rel_CubicBezierToX: 0.086 y: -0.072 control1X: 0.025 y: -0.033 control2X: 0.054 y: -0.057;
		rel_CubicBezierToX: 0.108 y: -0.024 control1X: 0.032 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.146 y: 0.079 control1X: 0.066 y: 0.000 control2X: 0.115 y: 0.026;
		rel_CubicBezierToX: 0.021 y: 0.208 control1X: 0.031 y: 0.053 control2X: 0.038 y: 0.122;
		rel_CubicBezierToX: -0.104 y: 0.208 control1X: -0.017 y: 0.086 control2X: -0.052 y: 0.155;
		rel_CubicBezierToX: -0.177 y: 0.079 control1X: -0.052 y: 0.053 control2X: -0.111 y: 0.079;
		rel_CubicBezierToX: -0.099 y: -0.023 control1X: -0.040 y: 0.000 control2X: -0.073 y: -0.008;
		rel_CubicBezierToX: -0.057 y: -0.073 control1X: -0.025 y: -0.016 control2X: -0.044 y: -0.040;
		rel_MoveToX: 0.344 y: -0.191;
		rel_CubicBezierToX: -0.010 y: -0.155 control1X: 0.013 y: -0.066 control2X: 0.010 y: -0.118;
		rel_CubicBezierToX: -0.100 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.053 y: -0.057;
		rel_CubicBezierToX: -0.124 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.089 y: 0.019;
		rel_CubicBezierToX: -0.072 y: 0.155 control1X: -0.034 y: 0.037 control2X: -0.058 y: 0.089;
		rel_CubicBezierToX: 0.009 y: 0.156 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.118;
		rel_CubicBezierToX: 0.101 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.054 y: 0.056;
		rel_CubicBezierToX: 0.123 y: -0.056 control1X: 0.047 y: 0.000 control2X: 0.088 y: -0.019;
		rel_CubicBezierToX: 0.072 y: -0.156 control1X: 0.035 y: -0.038 control2X: 0.059 y: -0.090! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:36'!
drawVeraSansItalics255
	vectorGraphicsEngine
		rel_MoveToX: 0.158 y: 1.007;
		rel_CubicBezierToX: -0.100 y: 0.127 control1X: -0.038 y: 0.065 control2X: -0.072 y: 0.107;
		rel_CubicBezierToX: -0.103 y: 0.030 control1X: -0.028 y: 0.020 control2X: -0.062 y: 0.030;
		rel_LineToX: -0.072 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.053 y: 0.000;
		rel_CubicBezierToX: 0.061 y: -0.018 control1X: 0.025 y: 0.000 control2X: 0.045 y: -0.006;
		rel_CubicBezierToX: 0.062 y: -0.083 control1X: 0.016 y: -0.012 control2X: 0.037 y: -0.039;
		rel_LineToX: 0.024 y: -0.041;
		rel_LineToX: -0.113 y: -0.538;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: 0.085 y: 0.428;
		rel_LineToX: 0.256 y: -0.428;
		rel_LineToX: 0.095 y: 0.000;
		rel_LineToX: -0.360 y: 0.598;
		rel_MoveToX: 0.182 y: -0.809;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099;
		rel_MoveToX: -0.191 y: 0.000;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.020 y: 0.099;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.020 y: -0.099! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics33
	vectorGraphicsEngine
		rel_MoveToX: 0.022 y: 0.832;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_MoveToX: 0.121 y: -0.605;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.064 y: 0.320;
		rel_LineToX: -0.045 y: 0.174;
		rel_LineToX: -0.079 y: 0.000;
		rel_LineToX: 0.025 y: -0.174;
		rel_LineToX: 0.064 y: -0.320! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics34
	vectorGraphicsEngine
		rel_MoveToX: 0.172 y: 0.227;
		rel_LineToX: -0.054 y: 0.271;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.054 y: -0.271;
		rel_LineToX: 0.083 y: 0.000;
		rel_MoveToX: 0.185 y: 0.000;
		rel_LineToX: -0.054 y: 0.271;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.054 y: -0.271;
		rel_LineToX: 0.083 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics35
	vectorGraphicsEngine
		rel_MoveToX: 0.446 y: 0.516;
		rel_LineToX: -0.142 y: 0.000;
		rel_LineToX: -0.074 y: 0.163;
		rel_LineToX: 0.143 y: 0.000;
		rel_LineToX: 0.073 y: -0.163;
		rel_MoveToX: -0.018 y: -0.278;
		rel_LineToX: -0.091 y: 0.203;
		rel_LineToX: 0.143 y: 0.000;
		rel_LineToX: 0.092 y: -0.203;
		rel_LineToX: 0.078 y: 0.000;
		rel_LineToX: -0.091 y: 0.203;
		rel_LineToX: 0.152 y: 0.000;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.171 y: 0.000;
		rel_LineToX: -0.073 y: 0.163;
		rel_LineToX: 0.155 y: 0.000;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.174 y: 0.000;
		rel_LineToX: -0.091 y: 0.202;
		rel_LineToX: -0.078 y: 0.000;
		rel_LineToX: 0.091 y: -0.202;
		rel_LineToX: -0.143 y: 0.000;
		rel_LineToX: -0.091 y: 0.202;
		rel_LineToX: -0.079 y: 0.000;
		rel_LineToX: 0.091 y: -0.202;
		rel_LineToX: -0.154 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.172 y: 0.000;
		rel_LineToX: 0.074 y: -0.163;
		rel_LineToX: -0.157 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.176 y: 0.000;
		rel_LineToX: 0.090 y: -0.203;
		rel_LineToX: 0.079 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics36
	vectorGraphicsEngine
		rel_MoveToX: 0.155 y: 1.103;
		rel_LineToX: -0.049 y: 0.000;
		rel_LineToX: 0.029 y: -0.147;
		rel_CubicBezierToX: -0.100 y: -0.012 control1X: -0.034 y: -0.001 control2X: -0.067 y: -0.005;
		rel_CubicBezierToX: -0.096 y: -0.033 control1X: -0.033 y: -0.007 control2X: -0.065 y: -0.019;
		rel_LineToX: 0.018 y: -0.088;
		rel_CubicBezierToX: 0.091 y: 0.047 control1X: 0.029 y: 0.021 control2X: 0.059 y: 0.037;
		rel_CubicBezierToX: 0.102 y: 0.016 control1X: 0.032 y: 0.010 control2X: 0.066 y: 0.016;
		rel_LineToX: 0.045 y: -0.223;
		rel_CubicBezierToX: -0.145 y: -0.060 control1X: -0.070 y: -0.012 control2X: -0.118 y: -0.032;
		rel_CubicBezierToX: -0.026 y: -0.115 control1X: -0.027 y: -0.028 control2X: -0.036 y: -0.066;
		rel_CubicBezierToX: 0.078 y: -0.126 control1X: 0.011 y: -0.053 control2X: 0.037 y: -0.095;
		rel_CubicBezierToX: 0.163 y: -0.053 control1X: 0.042 y: -0.031 control2X: 0.096 y: -0.048;
		rel_LineToX: 0.023 y: -0.115;
		rel_LineToX: 0.049 y: 0.000;
		rel_LineToX: -0.023 y: 0.113;
		rel_CubicBezierToX: 0.086 y: 0.010 control1X: 0.030 y: 0.001 control2X: 0.059 y: 0.005;
		rel_CubicBezierToX: 0.079 y: 0.021 control1X: 0.027 y: 0.005 control2X: 0.054 y: 0.012;
		rel_LineToX: -0.017 y: 0.085;
		rel_CubicBezierToX: -0.077 y: -0.032 control1X: -0.024 y: -0.014 control2X: -0.050 y: -0.024;
		rel_CubicBezierToX: -0.085 y: -0.013 control1X: -0.027 y: -0.008 control2X: -0.055 y: -0.012;
		rel_LineToX: -0.042 y: 0.208;
		rel_CubicBezierToX: 0.151 y: 0.061 control1X: 0.072 y: 0.011 control2X: 0.122 y: 0.032;
		rel_CubicBezierToX: 0.028 y: 0.120 control1X: 0.029 y: 0.029 control2X: 0.038 y: 0.069;
		rel_CubicBezierToX: -0.082 y: 0.130 control1X: -0.011 y: 0.055 control2X: -0.038 y: 0.099;
		rel_CubicBezierToX: -0.171 y: 0.055 control1X: -0.043 y: 0.032 control2X: -0.100 y: 0.050;
		rel_LineToX: -0.030 y: 0.148;
		rel_MoveToX: 0.056 y: -0.523;
		rel_LineToX: 0.040 y: -0.200;
		rel_CubicBezierToX: -0.093 y: 0.032 control1X: -0.039 y: 0.004 control2X: -0.070 y: 0.015;
		rel_CubicBezierToX: -0.044 y: 0.069 control1X: -0.023 y: 0.017 control2X: -0.038 y: 0.040;
		rel_CubicBezierToX: 0.014 y: 0.065 control1X: -0.006 y: 0.028 control2X: -0.001 y: 0.050;
		rel_CubicBezierToX: 0.082 y: 0.034 control1X: 0.015 y: 0.016 control2X: 0.043 y: 0.027;
		rel_MoveToX: 0.030 y: 0.094;
		rel_LineToX: -0.042 y: 0.211;
		rel_CubicBezierToX: 0.100 y: -0.035 control1X: 0.042 y: -0.006 control2X: 0.076 y: -0.017;
		rel_CubicBezierToX: 0.046 y: -0.071 control1X: 0.025 y: -0.018 control2X: 0.040 y: -0.041;
		rel_CubicBezierToX: -0.017 y: -0.068 control1X: 0.006 y: -0.029 control2X: 0.000 y: -0.051;
		rel_CubicBezierToX: -0.087 y: -0.037 control1X: -0.016 y: -0.017 control2X: -0.046 y: -0.029! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics37
	vectorGraphicsEngine
		rel_MoveToX: 0.638 y: 0.635;
		rel_CubicBezierToX: -0.074 y: 0.036 control1X: -0.028 y: 0.000 control2X: -0.053 y: 0.012;
		rel_CubicBezierToX: -0.044 y: 0.101 control1X: -0.021 y: 0.024 control2X: -0.035 y: 0.058;
		rel_CubicBezierToX: 0.004 y: 0.100 control1X: -0.008 y: 0.042 control2X: -0.007 y: 0.076;
		rel_CubicBezierToX: 0.060 y: 0.036 control1X: 0.011 y: 0.024 control2X: 0.031 y: 0.036;
		rel_CubicBezierToX: 0.073 y: -0.036 control1X: 0.028 y: 0.000 control2X: 0.052 y: -0.012;
		rel_CubicBezierToX: 0.045 y: -0.100 control1X: 0.021 y: -0.024 control2X: 0.036 y: -0.058;
		rel_CubicBezierToX: -0.004 y: -0.100 control1X: 0.009 y: -0.043 control2X: 0.007 y: -0.076;
		rel_CubicBezierToX: -0.058 y: -0.037 control1X: -0.011 y: -0.024 control2X: -0.030 y: -0.037;
		rel_MoveToX: 0.012 y: -0.062;
		rel_CubicBezierToX: 0.112 y: 0.054 control1X: 0.051 y: 0.000 control2X: 0.089 y: 0.018;
		rel_CubicBezierToX: 0.016 y: 0.145 control1X: 0.023 y: 0.036 control2X: 0.029 y: 0.084;
		rel_CubicBezierToX: -0.075 y: 0.145 control1X: -0.012 y: 0.061 control2X: -0.037 y: 0.109;
		rel_CubicBezierToX: -0.133 y: 0.053 control1X: -0.037 y: 0.035 control2X: -0.082 y: 0.053;
		rel_CubicBezierToX: -0.113 y: -0.053 control1X: -0.052 y: 0.000 control2X: -0.090 y: -0.018;
		rel_CubicBezierToX: -0.016 y: -0.145 control1X: -0.023 y: -0.036 control2X: -0.029 y: -0.084;
		rel_CubicBezierToX: 0.074 y: -0.145 control1X: 0.012 y: -0.061 control2X: 0.037 y: -0.110;
		rel_CubicBezierToX: 0.134 y: -0.054 control1X: 0.038 y: -0.036 control2X: 0.083 y: -0.054;
		rel_MoveToX: -0.444 y: -0.297;
		rel_CubicBezierToX: -0.074 y: 0.037 control1X: -0.028 y: 0.000 control2X: -0.053 y: 0.012;
		rel_CubicBezierToX: -0.044 y: 0.100 control1X: -0.021 y: 0.024 control2X: -0.035 y: 0.057;
		rel_CubicBezierToX: 0.004 y: 0.101 control1X: -0.009 y: 0.043 control2X: -0.007 y: 0.077;
		rel_CubicBezierToX: 0.059 y: 0.036 control1X: 0.011 y: 0.024 control2X: 0.031 y: 0.036;
		rel_CubicBezierToX: 0.074 y: -0.036 control1X: 0.028 y: 0.000 control2X: 0.053 y: -0.012;
		rel_CubicBezierToX: 0.045 y: -0.101 control1X: 0.021 y: -0.024 control2X: 0.036 y: -0.058;
		rel_CubicBezierToX: -0.004 y: -0.100 control1X: 0.008 y: -0.042 control2X: 0.007 y: -0.075;
		rel_CubicBezierToX: -0.059 y: -0.037 control1X: -0.011 y: -0.024 control2X: -0.031 y: -0.037;
		rel_MoveToX: 0.453 y: -0.062;
		rel_LineToX: 0.078 y: 0.000;
		rel_LineToX: -0.607 y: 0.756;
		rel_LineToX: -0.078 y: 0.000;
		rel_LineToX: 0.607 y: -0.756;
		rel_MoveToX: -0.441 y: 0.000;
		rel_CubicBezierToX: 0.112 y: 0.054 control1X: 0.051 y: 0.000 control2X: 0.089 y: 0.018;
		rel_CubicBezierToX: 0.017 y: 0.144 control1X: 0.023 y: 0.035 control2X: 0.029 y: 0.084;
		rel_CubicBezierToX: -0.075 y: 0.145 control1X: -0.012 y: 0.062 control2X: -0.037 y: 0.110;
		rel_CubicBezierToX: -0.134 y: 0.053 control1X: -0.037 y: 0.035 control2X: -0.082 y: 0.053;
		rel_CubicBezierToX: -0.112 y: -0.053 control1X: -0.052 y: 0.000 control2X: -0.089 y: -0.018;
		rel_CubicBezierToX: -0.016 y: -0.145 control1X: -0.023 y: -0.036 control2X: -0.028 y: -0.084;
		rel_CubicBezierToX: 0.074 y: -0.144 control1X: 0.012 y: -0.061 control2X: 0.037 y: -0.109;
		rel_CubicBezierToX: 0.133 y: -0.054 control1X: 0.037 y: -0.036 control2X: 0.082 y: -0.054! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics38
	vectorGraphicsEngine
		rel_MoveToX: 0.168 y: 0.564;
		rel_CubicBezierToX: -0.081 y: 0.079 control1X: -0.035 y: 0.026 control2X: -0.062 y: 0.053;
		rel_CubicBezierToX: -0.037 y: 0.082 control1X: -0.019 y: 0.026 control2X: -0.031 y: 0.053;
		rel_CubicBezierToX: 0.028 y: 0.119 control1X: -0.010 y: 0.048 control2X: 0.000 y: 0.087;
		rel_CubicBezierToX: 0.120 y: 0.047 control1X: 0.028 y: 0.032 control2X: 0.068 y: 0.047;
		rel_CubicBezierToX: 0.090 y: -0.015 control1X: 0.031 y: 0.000 control2X: 0.061 y: -0.005;
		rel_CubicBezierToX: 0.086 y: -0.047 control1X: 0.029 y: -0.010 control2X: 0.058 y: -0.026;
		rel_LineToX: -0.206 y: -0.265;
		rel_MoveToX: 0.080 y: -0.055;
		rel_LineToX: 0.197 y: 0.254;
		rel_CubicBezierToX: 0.063 y: -0.093 control1X: 0.025 y: -0.029 control2X: 0.046 y: -0.060;
		rel_CubicBezierToX: 0.040 y: -0.105 control1X: 0.017 y: -0.033 control2X: 0.031 y: -0.068;
		rel_LineToX: 0.091 y: 0.000;
		rel_CubicBezierToX: -0.057 y: 0.127 control1X: -0.012 y: 0.043 control2X: -0.031 y: 0.085;
		rel_CubicBezierToX: -0.096 y: 0.125 control1X: -0.025 y: 0.042 control2X: -0.057 y: 0.084;
		rel_LineToX: 0.108 y: 0.139;
		rel_LineToX: -0.123 y: 0.000;
		rel_LineToX: -0.055 y: -0.072;
		rel_CubicBezierToX: -0.119 y: 0.065 control1X: -0.040 y: 0.029 control2X: -0.079 y: 0.051;
		rel_CubicBezierToX: -0.124 y: 0.021 control1X: -0.040 y: 0.014 control2X: -0.081 y: 0.021;
		rel_CubicBezierToX: -0.179 y: -0.067 control1X: -0.078 y: 0.000 control2X: -0.138 y: -0.022;
		rel_CubicBezierToX: -0.040 y: -0.172 control1X: -0.041 y: -0.045 control2X: -0.054 y: -0.102;
		rel_CubicBezierToX: 0.056 y: -0.117 control1X: 0.008 y: -0.042 control2X: 0.027 y: -0.081;
		rel_CubicBezierToX: 0.119 y: -0.104 control1X: 0.029 y: -0.037 control2X: 0.069 y: -0.071;
		rel_CubicBezierToX: -0.023 y: -0.061 control1X: -0.011 y: -0.021 control2X: -0.019 y: -0.041;
		rel_CubicBezierToX: 0.000 y: -0.060 control1X: -0.004 y: -0.021 control2X: -0.004 y: -0.040;
		rel_CubicBezierToX: 0.080 y: -0.127 control1X: 0.011 y: -0.053 control2X: 0.037 y: -0.095;
		rel_CubicBezierToX: 0.154 y: -0.048 control1X: 0.043 y: -0.032 control2X: 0.094 y: -0.048;
		rel_CubicBezierToX: 0.079 y: 0.009 control1X: 0.027 y: 0.000 control2X: 0.053 y: 0.003;
		rel_CubicBezierToX: 0.077 y: 0.026 control1X: 0.026 y: 0.006 control2X: 0.051 y: 0.015;
		rel_LineToX: -0.018 y: 0.089;
		rel_CubicBezierToX: -0.074 y: -0.035 control1X: -0.025 y: -0.015 control2X: -0.050 y: -0.027;
		rel_CubicBezierToX: -0.069 y: -0.012 control1X: -0.024 y: -0.008 control2X: -0.047 y: -0.012;
		rel_CubicBezierToX: -0.089 y: 0.027 control1X: -0.034 y: 0.000 control2X: -0.064 y: 0.009;
		rel_CubicBezierToX: -0.046 y: 0.070 control1X: -0.025 y: 0.018 control2X: -0.040 y: 0.041;
		rel_CubicBezierToX: 0.004 y: 0.050 control1X: -0.003 y: 0.017 control2X: -0.002 y: 0.033;
		rel_CubicBezierToX: 0.045 y: 0.071 control1X: 0.006 y: 0.017 control2X: 0.021 y: 0.040! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics39
	vectorGraphicsEngine
		rel_MoveToX: 0.172 y: 0.227;
		rel_LineToX: -0.054 y: 0.271;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.054 y: -0.271;
		rel_LineToX: 0.083 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics40
	vectorGraphicsEngine
		rel_MoveToX: 0.308 y: 0.197;
		rel_CubicBezierToX: -0.142 y: 0.222 control1X: -0.059 y: 0.075 control2X: -0.106 y: 0.149;
		rel_CubicBezierToX: -0.076 y: 0.223 control1X: -0.036 y: 0.073 control2X: -0.061 y: 0.147;
		rel_CubicBezierToX: -0.013 y: 0.224 control1X: -0.015 y: 0.075 control2X: -0.019 y: 0.150;
		rel_CubicBezierToX: 0.053 y: 0.222 control1X: 0.007 y: 0.074 control2X: 0.024 y: 0.148;
		rel_LineToX: -0.078 y: 0.000;
		rel_CubicBezierToX: -0.065 y: -0.226 control1X: -0.033 y: -0.077 control2X: -0.055 y: -0.152;
		rel_CubicBezierToX: 0.008 y: -0.220 control1X: -0.009 y: -0.074 control2X: -0.007 y: -0.147;
		rel_CubicBezierToX: 0.080 y: -0.219 control1X: 0.015 y: -0.073 control2X: 0.041 y: -0.146;
		rel_CubicBezierToX: 0.155 y: -0.226 control1X: 0.039 y: -0.074 control2X: 0.090 y: -0.149;
		rel_LineToX: 0.078 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics41
	vectorGraphicsEngine
		rel_MoveToX: 0.078 y: 0.197;
		rel_LineToX: 0.078 y: 0.000;
		rel_CubicBezierToX: 0.064 y: 0.226 control1X: 0.033 y: 0.077 control2X: 0.055 y: 0.152;
		rel_CubicBezierToX: -0.007 y: 0.219 control1X: 0.010 y: 0.074 control2X: 0.007 y: 0.147;
		rel_CubicBezierToX: -0.081 y: 0.220 control1X: -0.015 y: 0.073 control2X: -0.041 y: 0.146;
		rel_CubicBezierToX: -0.154 y: 0.226 control1X: -0.039 y: 0.074 control2X: -0.090 y: 0.149;
		rel_LineToX: -0.078 y: 0.000;
		rel_CubicBezierToX: 0.141 y: -0.222 control1X: 0.058 y: -0.075 control2X: 0.105 y: -0.149;
		rel_CubicBezierToX: 0.077 y: -0.224 control1X: 0.036 y: -0.074 control2X: 0.062 y: -0.148;
		rel_CubicBezierToX: 0.012 y: -0.223 control1X: 0.015 y: -0.075 control2X: 0.019 y: -0.149;
		rel_CubicBezierToX: -0.052 y: -0.222 control1X: -0.006 y: -0.073 control2X: -0.024 y: -0.147! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics42
	vectorGraphicsEngine
		rel_MoveToX: 0.439 y: 0.347;
		rel_LineToX: -0.194 y: 0.095;
		rel_LineToX: 0.156 y: 0.095;
		rel_LineToX: -0.038 y: 0.048;
		rel_LineToX: -0.144 y: -0.099;
		rel_LineToX: -0.037 y: 0.184;
		rel_LineToX: -0.056 y: 0.000;
		rel_LineToX: 0.037 y: -0.184;
		rel_LineToX: -0.184 y: 0.099;
		rel_LineToX: -0.019 y: -0.048;
		rel_LineToX: 0.194 y: -0.095;
		rel_LineToX: -0.156 y: -0.095;
		rel_LineToX: 0.038 y: -0.048;
		rel_LineToX: 0.144 y: 0.099;
		rel_LineToX: 0.037 y: -0.184;
		rel_LineToX: 0.056 y: 0.000;
		rel_LineToX: -0.037 y: 0.184;
		rel_LineToX: 0.184 y: -0.099;
		rel_LineToX: 0.019 y: 0.048! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics43
	vectorGraphicsEngine
		rel_MoveToX: 0.432 y: 0.329;
		rel_LineToX: -0.054 y: 0.272;
		rel_LineToX: 0.272 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.272 y: 0.000;
		rel_LineToX: -0.054 y: 0.272;
		rel_LineToX: -0.082 y: 0.000;
		rel_LineToX: 0.054 y: -0.272;
		rel_LineToX: -0.272 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.272 y: 0.000;
		rel_LineToX: 0.054 y: -0.272;
		rel_LineToX: 0.082 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics44
	vectorGraphicsEngine
		rel_MoveToX: -0.011 y: 0.832;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.017 y: 0.084;
		rel_LineToX: -0.111 y: 0.156;
		rel_LineToX: -0.063 y: 0.000;
		rel_LineToX: 0.071 y: -0.156;
		rel_LineToX: 0.017 y: -0.084! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics45
	vectorGraphicsEngine
		rel_MoveToX: -0.042 y: 0.642;
		rel_LineToX: 0.263 y: 0.000;
		rel_LineToX: -0.016 y: 0.080;
		rel_LineToX: -0.263 y: 0.000;
		rel_LineToX: 0.016 y: -0.080! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics46
	vectorGraphicsEngine
		rel_MoveToX: -0.022 y: 0.832;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.025 y: -0.124! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics47
	vectorGraphicsEngine
		rel_MoveToX: 0.246 y: 0.227;
		rel_LineToX: 0.083 y: 0.000;
		rel_LineToX: -0.418 y: 0.822;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.418 y: -0.822! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics48
	vectorGraphicsEngine
		rel_MoveToX: 0.297 y: 0.292;
		rel_CubicBezierToX: -0.130 y: 0.075 control1X: -0.051 y: 0.000 control2X: -0.094 y: 0.025;
		rel_CubicBezierToX: -0.083 y: 0.225 control1X: -0.035 y: 0.050 control2X: -0.063 y: 0.125;
		rel_CubicBezierToX: -0.007 y: 0.225 control1X: -0.020 y: 0.100 control2X: -0.022 y: 0.175;
		rel_CubicBezierToX: 0.100 y: 0.075 control1X: 0.016 y: 0.050 control2X: 0.049 y: 0.075;
		rel_CubicBezierToX: 0.130 y: -0.075 control1X: 0.051 y: 0.000 control2X: 0.094 y: -0.025;
		rel_CubicBezierToX: 0.084 y: -0.225 control1X: 0.036 y: -0.050 control2X: 0.064 y: -0.125;
		rel_CubicBezierToX: 0.006 y: -0.225 control1X: 0.020 y: -0.100 control2X: 0.022 y: -0.175;
		rel_CubicBezierToX: -0.100 y: -0.075 control1X: -0.015 y: -0.050 control2X: -0.049 y: -0.075;
		rel_MoveToX: 0.016 y: -0.078;
		rel_CubicBezierToX: 0.168 y: 0.097 control1X: 0.082 y: 0.000 control2X: 0.138 y: 0.032;
		rel_CubicBezierToX: 0.009 y: 0.281 control1X: 0.030 y: 0.064 control2X: 0.033 y: 0.158;
		rel_CubicBezierToX: -0.121 y: 0.281 control1X: -0.025 y: 0.123 control2X: -0.065 y: 0.217;
		rel_CubicBezierToX: -0.206 y: 0.097 control1X: -0.056 y: 0.064 control2X: -0.125 y: 0.097;
		rel_CubicBezierToX: -0.168 y: -0.097 control1X: -0.082 y: 0.000 control2X: -0.138 y: -0.032;
		rel_CubicBezierToX: -0.008 y: -0.281 control1X: -0.030 y: -0.065 control2X: -0.033 y: -0.159;
		rel_CubicBezierToX: 0.121 y: -0.281 control1X: 0.025 y: -0.123 control2X: 0.065 y: -0.217;
		rel_CubicBezierToX: 0.207 y: -0.097 control1X: 0.056 y: -0.065 control2X: 0.125 y: -0.097! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics49
	vectorGraphicsEngine
		rel_MoveToX: -0.013 y: 0.873;
		rel_LineToX: 0.161 y: 0.000;
		rel_LineToX: 0.111 y: -0.556;
		rel_LineToX: -0.182 y: 0.035;
		rel_LineToX: 0.018 y: -0.090;
		rel_LineToX: 0.181 y: -0.035;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.129 y: 0.646;
		rel_LineToX: 0.161 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.420 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics50
	vectorGraphicsEngine
		rel_MoveToX: 0.055 y: 0.873;
		rel_LineToX: 0.344 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.463 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.184 y: -0.156 control1X: 0.045 y: -0.039 control2X: 0.107 y: -0.091;
		rel_CubicBezierToX: 0.147 y: -0.127 control1X: 0.078 y: -0.065 control2X: 0.127 y: -0.108;
		rel_CubicBezierToX: 0.084 y: -0.090 control1X: 0.039 y: -0.035 control2X: 0.067 y: -0.065;
		rel_CubicBezierToX: 0.034 y: -0.073 control1X: 0.018 y: -0.025 control2X: 0.029 y: -0.049;
		rel_CubicBezierToX: -0.022 y: -0.095 control1X: 0.008 y: -0.039 control2X: 0.000 y: -0.070;
		rel_CubicBezierToX: -0.099 y: -0.037 control1X: -0.022 y: -0.024 control2X: -0.055 y: -0.037;
		rel_CubicBezierToX: -0.101 y: 0.016 control1X: -0.031 y: 0.000 control2X: -0.065 y: 0.005;
		rel_CubicBezierToX: -0.120 y: 0.049 control1X: -0.036 y: 0.011 control2X: -0.076 y: 0.027;
		rel_LineToX: 0.020 y: -0.100;
		rel_CubicBezierToX: 0.119 y: -0.036 control1X: 0.043 y: -0.016 control2X: 0.082 y: -0.028;
		rel_CubicBezierToX: 0.097 y: -0.012 control1X: 0.036 y: -0.008 control2X: 0.069 y: -0.012;
		rel_CubicBezierToX: 0.169 y: 0.057 control1X: 0.076 y: 0.000 control2X: 0.132 y: 0.019;
		rel_CubicBezierToX: 0.037 y: 0.151 control1X: 0.037 y: 0.038 control2X: 0.050 y: 0.088;
		rel_CubicBezierToX: -0.034 y: 0.086 control1X: -0.006 y: 0.030 control2X: -0.017 y: 0.058;
		rel_CubicBezierToX: -0.080 y: 0.095 control1X: -0.016 y: 0.027 control2X: -0.043 y: 0.058;
		rel_CubicBezierToX: -0.094 y: 0.082 control1X: -0.010 y: 0.009 control2X: -0.041 y: 0.037;
		rel_CubicBezierToX: -0.222 y: 0.189 control1X: -0.053 y: 0.045 control2X: -0.127 y: 0.108! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics51
	vectorGraphicsEngine
		rel_MoveToX: 0.331 y: 0.563;
		rel_CubicBezierToX: 0.098 y: 0.063 control1X: 0.045 y: 0.010 control2X: 0.078 y: 0.031;
		rel_CubicBezierToX: 0.016 y: 0.118 control1X: 0.020 y: 0.032 control2X: 0.026 y: 0.071;
		rel_CubicBezierToX: -0.108 y: 0.167 control1X: -0.014 y: 0.072 control2X: -0.050 y: 0.128;
		rel_CubicBezierToX: -0.223 y: 0.059 control1X: -0.057 y: 0.039 control2X: -0.132 y: 0.059;
		rel_CubicBezierToX: -0.093 y: -0.009 control1X: -0.031 y: 0.000 control2X: -0.062 y: -0.003;
		rel_CubicBezierToX: -0.095 y: -0.027 control1X: -0.031 y: -0.006 control2X: -0.063 y: -0.015;
		rel_LineToX: 0.019 y: -0.095;
		rel_CubicBezierToX: 0.083 y: 0.036 control1X: 0.024 y: 0.016 control2X: 0.052 y: 0.028;
		rel_CubicBezierToX: 0.100 y: 0.012 control1X: 0.031 y: 0.008 control2X: 0.064 y: 0.012;
		rel_CubicBezierToX: 0.148 y: -0.037 control1X: 0.062 y: 0.000 control2X: 0.111 y: -0.012;
		rel_CubicBezierToX: 0.070 y: -0.106 control1X: 0.037 y: -0.024 control2X: 0.061 y: -0.060;
		rel_CubicBezierToX: -0.025 y: -0.101 control1X: 0.009 y: -0.043 control2X: 0.000 y: -0.077;
		rel_CubicBezierToX: -0.118 y: -0.037 control1X: -0.025 y: -0.024 control2X: -0.064 y: -0.037;
		rel_LineToX: -0.085 y: 0.000;
		rel_LineToX: 0.016 y: -0.081;
		rel_LineToX: 0.089 y: 0.000;
		rel_CubicBezierToX: 0.117 y: -0.029 control1X: 0.048 y: 0.000 control2X: 0.088 y: -0.010;
		rel_CubicBezierToX: 0.055 y: -0.084 control1X: 0.030 y: -0.020 control2X: 0.048 y: -0.047;
		rel_CubicBezierToX: -0.023 y: -0.086 control1X: 0.007 y: -0.037 control2X: 0.000 y: -0.066;
		rel_CubicBezierToX: -0.108 y: -0.030 control1X: -0.022 y: -0.020 control2X: -0.058 y: -0.030;
		rel_CubicBezierToX: -0.089 y: 0.009 control1X: -0.027 y: 0.000 control2X: -0.057 y: 0.003;
		rel_CubicBezierToX: -0.108 y: 0.027 control1X: -0.032 y: 0.006 control2X: -0.068 y: 0.015;
		rel_LineToX: 0.018 y: -0.088;
		rel_CubicBezierToX: 0.110 y: -0.023 control1X: 0.039 y: -0.010 control2X: 0.076 y: -0.018;
		rel_CubicBezierToX: 0.094 y: -0.008 control1X: 0.034 y: -0.005 control2X: 0.065 y: -0.008;
		rel_CubicBezierToX: 0.167 y: 0.051 control1X: 0.075 y: 0.000 control2X: 0.131 y: 0.017;
		rel_CubicBezierToX: 0.038 y: 0.138 control1X: 0.037 y: 0.034 control2X: 0.049 y: 0.080;
		rel_CubicBezierToX: -0.055 y: 0.102 control1X: -0.008 y: 0.040 control2X: -0.026 y: 0.075;
		rel_CubicBezierToX: -0.110 y: 0.058 control1X: -0.029 y: 0.028 control2X: -0.065 y: 0.047! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics52
	vectorGraphicsEngine
		rel_MoveToX: 0.353 y: 0.313;
		rel_LineToX: -0.327 y: 0.389;
		rel_LineToX: 0.249 y: 0.000;
		rel_LineToX: 0.078 y: -0.389;
		rel_MoveToX: -0.009 y: -0.086;
		rel_LineToX: 0.124 y: 0.000;
		rel_LineToX: -0.095 y: 0.475;
		rel_LineToX: 0.104 y: 0.000;
		rel_LineToX: -0.016 y: 0.082;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: -0.034 y: 0.172;
		rel_LineToX: -0.098 y: 0.000;
		rel_LineToX: 0.034 y: -0.172;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: 0.019 y: -0.095;
		rel_LineToX: 0.396 y: -0.462! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics53
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: 0.227;
		rel_LineToX: 0.387 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.297 y: 0.000;
		rel_LineToX: -0.036 y: 0.179;
		rel_CubicBezierToX: 0.045 y: -0.011 control1X: 0.015 y: -0.005 control2X: 0.030 y: -0.009;
		rel_CubicBezierToX: 0.044 y: -0.004 control1X: 0.015 y: -0.003 control2X: 0.029 y: -0.004;
		rel_CubicBezierToX: 0.180 y: 0.067 control1X: 0.081 y: 0.000 control2X: 0.141 y: 0.022;
		rel_CubicBezierToX: 0.035 y: 0.181 control1X: 0.039 y: 0.045 control2X: 0.050 y: 0.105;
		rel_CubicBezierToX: -0.110 y: 0.183 control1X: -0.016 y: 0.078 control2X: -0.052 y: 0.140;
		rel_CubicBezierToX: -0.220 y: 0.065 control1X: -0.058 y: 0.043 control2X: -0.131 y: 0.065;
		rel_CubicBezierToX: -0.092 y: -0.008 control1X: -0.031 y: 0.000 control2X: -0.061 y: -0.003;
		rel_CubicBezierToX: -0.094 y: -0.023 control1X: -0.031 y: -0.005 control2X: -0.062 y: -0.013;
		rel_LineToX: 0.020 y: -0.099;
		rel_CubicBezierToX: 0.084 y: 0.036 control1X: 0.026 y: 0.016 control2X: 0.054 y: 0.028;
		rel_CubicBezierToX: 0.097 y: 0.012 control1X: 0.030 y: 0.008 control2X: 0.062 y: 0.012;
		rel_CubicBezierToX: 0.143 y: -0.044 control1X: 0.056 y: 0.000 control2X: 0.104 y: -0.015;
		rel_CubicBezierToX: 0.074 y: -0.121 control1X: 0.039 y: -0.030 control2X: 0.063 y: -0.070;
		rel_CubicBezierToX: -0.025 y: -0.121 control1X: 0.010 y: -0.051 control2X: 0.002 y: -0.091;
		rel_CubicBezierToX: -0.125 y: -0.044 control1X: -0.027 y: -0.030 control2X: -0.069 y: -0.044;
		rel_CubicBezierToX: -0.081 y: 0.009 control1X: -0.026 y: 0.000 control2X: -0.053 y: 0.003;
		rel_CubicBezierToX: -0.086 y: 0.027 control1X: -0.027 y: 0.006 control2X: -0.056 y: 0.015;
		rel_LineToX: 0.073 y: -0.366! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics54
	vectorGraphicsEngine
		rel_MoveToX: 0.257 y: 0.552;
		rel_CubicBezierToX: -0.114 y: 0.045 control1X: -0.044 y: 0.000 control2X: -0.082 y: 0.015;
		rel_CubicBezierToX: -0.064 y: 0.124 control1X: -0.032 y: 0.030 control2X: -0.053 y: 0.072;
		rel_CubicBezierToX: 0.014 y: 0.125 control1X: -0.010 y: 0.052 control2X: -0.006 y: 0.094;
		rel_CubicBezierToX: 0.096 y: 0.045 control1X: 0.020 y: 0.030 control2X: 0.052 y: 0.045;
		rel_CubicBezierToX: 0.114 y: -0.045 control1X: 0.044 y: 0.000 control2X: 0.082 y: -0.015;
		rel_CubicBezierToX: 0.064 y: -0.125 control1X: 0.032 y: -0.031 control2X: 0.054 y: -0.072;
		rel_CubicBezierToX: -0.014 y: -0.125 control1X: 0.011 y: -0.053 control2X: 0.006 y: -0.094;
		rel_CubicBezierToX: -0.096 y: -0.045 control1X: -0.020 y: -0.030 control2X: -0.052 y: -0.045;
		rel_MoveToX: 0.258 y: -0.309;
		rel_LineToX: -0.018 y: 0.090;
		rel_CubicBezierToX: -0.070 y: -0.027 control1X: -0.022 y: -0.012 control2X: -0.046 y: -0.021;
		rel_CubicBezierToX: -0.073 y: -0.009 control1X: -0.024 y: -0.006 control2X: -0.048 y: -0.009;
		rel_CubicBezierToX: -0.163 y: 0.066 control1X: -0.065 y: 0.000 control2X: -0.119 y: 0.022;
		rel_CubicBezierToX: -0.098 y: 0.199 control1X: -0.043 y: 0.044 control2X: -0.076 y: 0.110;
		rel_CubicBezierToX: 0.085 y: -0.065 control1X: 0.025 y: -0.028 control2X: 0.053 y: -0.050;
		rel_CubicBezierToX: 0.100 y: -0.023 control1X: 0.032 y: -0.015 control2X: 0.066 y: -0.023;
		rel_CubicBezierToX: 0.160 y: 0.067 control1X: 0.073 y: 0.000 control2X: 0.127 y: 0.022;
		rel_CubicBezierToX: 0.028 y: 0.181 control1X: 0.034 y: 0.044 control2X: 0.043 y: 0.105;
		rel_CubicBezierToX: -0.102 y: 0.180 control1X: -0.015 y: 0.075 control2X: -0.049 y: 0.135;
		rel_CubicBezierToX: -0.190 y: 0.068 control1X: -0.053 y: 0.045 control2X: -0.117 y: 0.068;
		rel_CubicBezierToX: -0.174 y: -0.097 control1X: -0.084 y: 0.000 control2X: -0.142 y: -0.032;
		rel_CubicBezierToX: -0.011 y: -0.281 control1X: -0.032 y: -0.065 control2X: -0.035 y: -0.159;
		rel_CubicBezierToX: 0.137 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.069 y: -0.207;
		rel_CubicBezierToX: 0.241 y: -0.103 control1X: 0.068 y: -0.069 control2X: 0.149 y: -0.103;
		rel_CubicBezierToX: 0.073 y: 0.007 control1X: 0.025 y: 0.000 control2X: 0.049 y: 0.002;
		rel_CubicBezierToX: 0.075 y: 0.022 control1X: 0.024 y: 0.005 control2X: 0.049 y: 0.012! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics55
	vectorGraphicsEngine
		rel_MoveToX: 0.074 y: 0.227;
		rel_LineToX: 0.469 y: 0.000;
		rel_LineToX: -0.008 y: 0.042;
		rel_LineToX: -0.402 y: 0.687;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.378 y: -0.646;
		rel_LineToX: -0.350 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics56
	vectorGraphicsEngine
		rel_MoveToX: 0.234 y: 0.610;
		rel_CubicBezierToX: -0.118 y: 0.038 control1X: -0.047 y: 0.000 control2X: -0.086 y: 0.012;
		rel_CubicBezierToX: -0.061 y: 0.104 control1X: -0.032 y: 0.025 control2X: -0.052 y: 0.060;
		rel_CubicBezierToX: 0.019 y: 0.104 control1X: -0.009 y: 0.044 control2X: -0.002 y: 0.079;
		rel_CubicBezierToX: 0.103 y: 0.038 control1X: 0.022 y: 0.025 control2X: 0.056 y: 0.038;
		rel_CubicBezierToX: 0.118 y: -0.038 control1X: 0.047 y: 0.000 control2X: 0.086 y: -0.012;
		rel_CubicBezierToX: 0.061 y: -0.104 control1X: 0.032 y: -0.025 control2X: 0.052 y: -0.060;
		rel_CubicBezierToX: -0.020 y: -0.104 control1X: 0.009 y: -0.044 control2X: 0.002 y: -0.078;
		rel_CubicBezierToX: -0.103 y: -0.038 control1X: -0.022 y: -0.025 control2X: -0.056 y: -0.038;
		rel_MoveToX: -0.090 y: -0.042;
		rel_CubicBezierToX: -0.087 y: -0.059 control1X: -0.040 y: -0.010 control2X: -0.069 y: -0.030;
		rel_CubicBezierToX: -0.014 y: -0.106 control1X: -0.018 y: -0.029 control2X: -0.022 y: -0.064;
		rel_CubicBezierToX: 0.090 y: -0.138 control1X: 0.012 y: -0.058 control2X: 0.042 y: -0.104;
		rel_CubicBezierToX: 0.181 y: -0.051 control1X: 0.048 y: -0.034 control2X: 0.109 y: -0.051;
		rel_CubicBezierToX: 0.161 y: 0.051 control1X: 0.073 y: 0.000 control2X: 0.126 y: 0.017;
		rel_CubicBezierToX: 0.034 y: 0.138 control1X: 0.035 y: 0.034 control2X: 0.046 y: 0.080;
		rel_CubicBezierToX: -0.057 y: 0.106 control1X: -0.008 y: 0.042 control2X: -0.027 y: 0.077;
		rel_CubicBezierToX: -0.110 y: 0.059 control1X: -0.029 y: 0.029 control2X: -0.066 y: 0.049;
		rel_CubicBezierToX: 0.098 y: 0.065 control1X: 0.045 y: 0.011 control2X: 0.078 y: 0.033;
		rel_CubicBezierToX: 0.016 y: 0.118 control1X: 0.020 y: 0.032 control2X: 0.026 y: 0.072;
		rel_CubicBezierToX: -0.097 y: 0.163 control1X: -0.014 y: 0.071 control2X: -0.047 y: 0.125;
		rel_CubicBezierToX: -0.196 y: 0.057 control1X: -0.051 y: 0.038 control2X: -0.116 y: 0.057;
		rel_CubicBezierToX: -0.174 y: -0.057 control1X: -0.080 y: 0.000 control2X: -0.138 y: -0.019;
		rel_CubicBezierToX: -0.032 y: -0.163 control1X: -0.035 y: -0.038 control2X: -0.046 y: -0.092;
		rel_CubicBezierToX: 0.064 y: -0.118 control1X: 0.009 y: -0.046 control2X: 0.031 y: -0.086;
		rel_CubicBezierToX: 0.124 y: -0.065 control1X: 0.033 y: -0.032 control2X: 0.075 y: -0.054;
		rel_MoveToX: -0.005 y: -0.156;
		rel_CubicBezierToX: 0.017 y: 0.088 control1X: -0.008 y: 0.038 control2X: -0.002 y: 0.067;
		rel_CubicBezierToX: 0.093 y: 0.032 control1X: 0.020 y: 0.021 control2X: 0.051 y: 0.032;
		rel_CubicBezierToX: 0.106 y: -0.032 control1X: 0.042 y: 0.000 control2X: 0.078 y: -0.011;
		rel_CubicBezierToX: 0.054 y: -0.088 control1X: 0.028 y: -0.021 control2X: 0.046 y: -0.051;
		rel_CubicBezierToX: -0.018 y: -0.088 control1X: 0.008 y: -0.038 control2X: 0.001 y: -0.067;
		rel_CubicBezierToX: -0.093 y: -0.032 control1X: -0.020 y: -0.021 control2X: -0.050 y: -0.032;
		rel_CubicBezierToX: -0.106 y: 0.032 control1X: -0.043 y: 0.000 control2X: -0.078 y: 0.011;
		rel_CubicBezierToX: -0.053 y: 0.088 control1X: -0.028 y: 0.021 control2X: -0.045 y: 0.051! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics57
	vectorGraphicsEngine
		rel_MoveToX: -0.041 y: 0.941;
		rel_LineToX: 0.018 y: -0.090;
		rel_CubicBezierToX: 0.070 y: 0.027 control1X: 0.022 y: 0.012 control2X: 0.046 y: 0.021;
		rel_CubicBezierToX: 0.073 y: 0.009 control1X: 0.024 y: 0.006 control2X: 0.048 y: 0.009;
		rel_CubicBezierToX: 0.162 y: -0.065 control1X: 0.065 y: 0.000 control2X: 0.119 y: -0.022;
		rel_CubicBezierToX: 0.099 y: -0.200 control1X: 0.043 y: -0.044 control2X: 0.076 y: -0.111;
		rel_CubicBezierToX: -0.085 y: 0.065 control1X: -0.024 y: 0.028 control2X: -0.053 y: 0.049;
		rel_CubicBezierToX: -0.101 y: 0.022 control1X: -0.032 y: 0.015 control2X: -0.066 y: 0.022;
		rel_CubicBezierToX: -0.160 y: -0.066 control1X: -0.073 y: 0.000 control2X: -0.126 y: -0.022;
		rel_CubicBezierToX: -0.027 y: -0.181 control1X: -0.033 y: -0.044 control2X: -0.042 y: -0.105;
		rel_CubicBezierToX: 0.102 y: -0.180 control1X: 0.015 y: -0.075 control2X: 0.049 y: -0.135;
		rel_CubicBezierToX: 0.190 y: -0.068 control1X: 0.053 y: -0.045 control2X: 0.117 y: -0.068;
		rel_CubicBezierToX: 0.173 y: 0.097 control1X: 0.084 y: 0.000 control2X: 0.142 y: 0.032;
		rel_CubicBezierToX: 0.011 y: 0.281 control1X: 0.032 y: 0.064 control2X: 0.035 y: 0.158;
		rel_CubicBezierToX: -0.137 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.069 y: 0.207;
		rel_CubicBezierToX: -0.240 y: 0.103 control1X: -0.068 y: 0.068 control2X: -0.148 y: 0.103;
		rel_CubicBezierToX: -0.074 y: -0.007 control1X: -0.025 y: 0.000 control2X: -0.049 y: -0.002;
		rel_CubicBezierToX: -0.075 y: -0.022 control1X: -0.024 y: -0.005 control2X: -0.049 y: -0.012;
		rel_MoveToX: 0.258 y: -0.309;
		rel_CubicBezierToX: 0.114 y: -0.045 control1X: 0.044 y: 0.000 control2X: 0.082 y: -0.015;
		rel_CubicBezierToX: 0.064 y: -0.125 control1X: 0.032 y: -0.030 control2X: 0.053 y: -0.072;
		rel_CubicBezierToX: -0.014 y: -0.124 control1X: 0.010 y: -0.052 control2X: 0.006 y: -0.094;
		rel_CubicBezierToX: -0.096 y: -0.046 control1X: -0.020 y: -0.031 control2X: -0.052 y: -0.046;
		rel_CubicBezierToX: -0.115 y: 0.046 control1X: -0.044 y: 0.000 control2X: -0.082 y: 0.015;
		rel_CubicBezierToX: -0.063 y: 0.124 control1X: -0.032 y: 0.030 control2X: -0.053 y: 0.072;
		rel_CubicBezierToX: 0.014 y: 0.125 control1X: -0.011 y: 0.053 control2X: -0.006 y: 0.094;
		rel_CubicBezierToX: 0.096 y: 0.045 control1X: 0.020 y: 0.030 control2X: 0.052 y: 0.045! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics58
	vectorGraphicsEngine
		rel_MoveToX: -0.011 y: 0.832;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_MoveToX: 0.079 y: -0.393;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.025 y: -0.124! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics59
	vectorGraphicsEngine
		rel_MoveToX: 0.067 y: 0.439;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_MoveToX: -0.079 y: 0.393;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.017 y: 0.084;
		rel_LineToX: -0.111 y: 0.156;
		rel_LineToX: -0.063 y: 0.000;
		rel_LineToX: 0.071 y: -0.156;
		rel_LineToX: 0.017 y: -0.084! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics60
	vectorGraphicsEngine
		rel_MoveToX: 0.677 y: 0.464;
		rel_LineToX: -0.540 y: 0.179;
		rel_LineToX: 0.468 y: 0.178;
		rel_LineToX: -0.018 y: 0.089;
		rel_LineToX: -0.581 y: -0.227;
		rel_LineToX: 0.016 y: -0.081;
		rel_LineToX: 0.671 y: -0.227;
		rel_LineToX: -0.018 y: 0.089! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics61
	vectorGraphicsEngine
		rel_MoveToX: 0.043 y: 0.502;
		rel_LineToX: 0.626 y: 0.000;
		rel_LineToX: -0.016 y: 0.082;
		rel_LineToX: -0.626 y: 0.000;
		rel_LineToX: 0.016 y: -0.082;
		rel_MoveToX: -0.040 y: 0.199;
		rel_LineToX: 0.626 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.626 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics62
	vectorGraphicsEngine
		rel_MoveToX: 0.051 y: 0.464;
		rel_LineToX: 0.018 y: -0.089;
		rel_LineToX: 0.581 y: 0.227;
		rel_LineToX: -0.016 y: 0.081;
		rel_LineToX: -0.671 y: 0.227;
		rel_LineToX: 0.018 y: -0.089;
		rel_LineToX: 0.539 y: -0.178;
		rel_LineToX: -0.467 y: -0.179! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics63
	vectorGraphicsEngine
		rel_MoveToX: 0.062 y: 0.832;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.025 y: 0.124;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.025 y: -0.124;
		rel_MoveToX: 0.111 y: -0.072;
		rel_LineToX: -0.093 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_CubicBezierToX: 0.030 y: -0.081 control1X: 0.007 y: -0.033 control2X: 0.017 y: -0.060;
		rel_CubicBezierToX: 0.072 y: -0.074 control1X: 0.013 y: -0.021 control2X: 0.037 y: -0.046;
		rel_LineToX: 0.053 y: -0.043;
		rel_CubicBezierToX: 0.050 y: -0.049 control1X: 0.022 y: -0.017 control2X: 0.039 y: -0.033;
		rel_CubicBezierToX: 0.022 y: -0.047 control1X: 0.012 y: -0.015 control2X: 0.019 y: -0.031;
		rel_CubicBezierToX: -0.018 y: -0.070 control1X: 0.006 y: -0.029 control2X: 0.000 y: -0.052;
		rel_CubicBezierToX: -0.079 y: -0.027 control1X: -0.018 y: -0.018 control2X: -0.044 y: -0.027;
		rel_CubicBezierToX: -0.086 y: 0.017 control1X: -0.026 y: 0.000 control2X: -0.054 y: 0.006;
		rel_CubicBezierToX: -0.101 y: 0.050 control1X: -0.031 y: 0.011 control2X: -0.065 y: 0.028;
		rel_LineToX: 0.018 y: -0.092;
		rel_CubicBezierToX: 0.101 y: -0.042 control1X: 0.034 y: -0.019 control2X: 0.068 y: -0.032;
		rel_CubicBezierToX: 0.100 y: -0.014 control1X: 0.033 y: -0.009 control2X: 0.067 y: -0.014;
		rel_CubicBezierToX: 0.135 y: 0.047 control1X: 0.060 y: 0.000 control2X: 0.105 y: 0.016;
		rel_CubicBezierToX: 0.030 y: 0.125 control1X: 0.030 y: 0.031 control2X: 0.040 y: 0.073;
		rel_CubicBezierToX: -0.032 y: 0.071 control1X: -0.005 y: 0.025 control2X: -0.016 y: 0.048;
		rel_CubicBezierToX: -0.077 y: 0.075 control1X: -0.016 y: 0.022 control2X: -0.042 y: 0.047;
		rel_LineToX: -0.051 y: 0.042;
		rel_CubicBezierToX: -0.040 y: 0.036 control1X: -0.018 y: 0.015 control2X: -0.032 y: 0.027;
		rel_CubicBezierToX: -0.018 y: 0.025 control1X: -0.008 y: 0.008 control2X: -0.014 y: 0.017;
		rel_CubicBezierToX: -0.009 y: 0.025 control1X: -0.003 y: 0.007 control2X: -0.006 y: 0.015;
		rel_CubicBezierToX: -0.009 y: 0.040 control1X: -0.003 y: 0.010 control2X: -0.006 y: 0.023;
		rel_LineToX: -0.012 y: 0.060! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics64
	vectorGraphicsEngine
		rel_MoveToX: 0.271 y: 0.694;
		rel_CubicBezierToX: 0.013 y: 0.110 control1X: -0.009 y: 0.046 control2X: -0.005 y: 0.083;
		rel_CubicBezierToX: 0.087 y: 0.040 control1X: 0.018 y: 0.026 control2X: 0.047 y: 0.040;
		rel_CubicBezierToX: 0.102 y: -0.040 control1X: 0.040 y: 0.000 control2X: 0.074 y: -0.013;
		rel_CubicBezierToX: 0.056 y: -0.109 control1X: 0.028 y: -0.027 control2X: 0.047 y: -0.063;
		rel_CubicBezierToX: -0.013 y: -0.108 control1X: 0.009 y: -0.046 control2X: 0.005 y: -0.082;
		rel_CubicBezierToX: -0.087 y: -0.041 control1X: -0.018 y: -0.027 control2X: -0.047 y: -0.041;
		rel_CubicBezierToX: -0.102 y: 0.040 control1X: -0.039 y: 0.000 control2X: -0.073 y: 0.013;
		rel_CubicBezierToX: -0.056 y: 0.109 control1X: -0.028 y: 0.027 control2X: -0.047 y: 0.063;
		rel_MoveToX: 0.237 y: 0.146;
		rel_CubicBezierToX: -0.079 y: 0.056 control1X: -0.025 y: 0.025 control2X: -0.051 y: 0.044;
		rel_CubicBezierToX: -0.091 y: 0.018 control1X: -0.027 y: 0.012 control2X: -0.058 y: 0.018;
		rel_CubicBezierToX: -0.125 y: -0.061 control1X: -0.056 y: 0.000 control2X: -0.098 y: -0.020;
		rel_CubicBezierToX: -0.021 y: -0.159 control1X: -0.027 y: -0.041 control2X: -0.034 y: -0.094;
		rel_CubicBezierToX: 0.084 y: -0.159 control1X: 0.013 y: -0.065 control2X: 0.041 y: -0.118;
		rel_CubicBezierToX: 0.149 y: -0.061 control1X: 0.043 y: -0.041 control2X: 0.093 y: -0.061;
		rel_CubicBezierToX: 0.085 y: 0.019 control1X: 0.033 y: 0.000 control2X: 0.062 y: 0.006;
		rel_CubicBezierToX: 0.056 y: 0.055 control1X: 0.023 y: 0.012 control2X: 0.042 y: 0.030;
		rel_LineToX: 0.013 y: -0.064;
		rel_LineToX: 0.070 y: 0.000;
		rel_LineToX: -0.072 y: 0.359;
		rel_CubicBezierToX: 0.124 y: -0.065 control1X: 0.049 y: -0.007 control2X: 0.090 y: -0.029;
		rel_CubicBezierToX: 0.069 y: -0.141 control1X: 0.034 y: -0.036 control2X: 0.057 y: -0.083;
		rel_CubicBezierToX: 0.004 y: -0.098 control1X: 0.007 y: -0.035 control2X: 0.008 y: -0.068;
		rel_CubicBezierToX: -0.029 y: -0.085 control1X: -0.004 y: -0.031 control2X: -0.014 y: -0.059;
		rel_CubicBezierToX: -0.104 y: -0.098 control1X: -0.025 y: -0.043 control2X: -0.060 y: -0.075;
		rel_CubicBezierToX: -0.152 y: -0.034 control1X: -0.044 y: -0.023 control2X: -0.095 y: -0.034;
		rel_CubicBezierToX: -0.118 y: 0.016 control1X: -0.040 y: 0.000 control2X: -0.079 y: 0.005;
		rel_CubicBezierToX: -0.111 y: 0.047 control1X: -0.039 y: 0.010 control2X: -0.076 y: 0.026;
		rel_CubicBezierToX: -0.146 y: 0.131 control1X: -0.058 y: 0.033 control2X: -0.106 y: 0.077;
		rel_CubicBezierToX: -0.078 y: 0.175 control1X: -0.039 y: 0.054 control2X: -0.065 y: 0.112;
		rel_CubicBezierToX: -0.001 y: 0.146 control1X: -0.010 y: 0.052 control2X: -0.011 y: 0.100;
		rel_CubicBezierToX: 0.058 y: 0.120 control1X: 0.010 y: 0.045 control2X: 0.029 y: 0.085;
		rel_CubicBezierToX: 0.103 y: 0.077 control1X: 0.027 y: 0.034 control2X: 0.062 y: 0.060;
		rel_CubicBezierToX: 0.139 y: 0.027 control1X: 0.041 y: 0.018 control2X: 0.088 y: 0.027;
		rel_CubicBezierToX: 0.128 y: -0.022 control1X: 0.042 y: 0.000 control2X: 0.085 y: -0.007;
		rel_CubicBezierToX: 0.124 y: -0.061 control1X: 0.044 y: -0.014 control2X: 0.085 y: -0.034;
		rel_LineToX: 0.033 y: 0.054;
		rel_CubicBezierToX: -0.148 y: 0.072 control1X: -0.047 y: 0.032 control2X: -0.096 y: 0.056;
		rel_CubicBezierToX: -0.151 y: 0.025 control1X: -0.051 y: 0.017 control2X: -0.102 y: 0.025;
		rel_CubicBezierToX: -0.164 y: -0.032 control1X: -0.060 y: 0.000 control2X: -0.115 y: -0.011;
		rel_CubicBezierToX: -0.124 y: -0.093 control1X: -0.049 y: -0.021 control2X: -0.090 y: -0.052;
		rel_CubicBezierToX: -0.067 y: -0.141 control1X: -0.034 y: -0.041 control2X: -0.056 y: -0.088;
		rel_CubicBezierToX: 0.002 y: -0.173 control1X: -0.011 y: -0.054 control2X: -0.010 y: -0.111;
		rel_CubicBezierToX: 0.067 y: -0.169 control1X: 0.012 y: -0.059 control2X: 0.034 y: -0.116;
		rel_CubicBezierToX: 0.123 y: -0.142 control1X: 0.033 y: -0.054 control2X: 0.074 y: -0.101;
		rel_CubicBezierToX: 0.164 y: -0.095 control1X: 0.050 y: -0.041 control2X: 0.105 y: -0.073;
		rel_CubicBezierToX: 0.181 y: -0.033 control1X: 0.059 y: -0.022 control2X: 0.120 y: -0.033;
		rel_CubicBezierToX: 0.183 y: 0.042 control1X: 0.069 y: 0.000 control2X: 0.130 y: 0.014;
		rel_CubicBezierToX: 0.125 y: 0.121 control1X: 0.054 y: 0.028 control2X: 0.095 y: 0.069;
		rel_CubicBezierToX: 0.035 y: 0.104 control1X: 0.018 y: 0.032 control2X: 0.030 y: 0.067;
		rel_CubicBezierToX: -0.004 y: 0.116 control1X: 0.006 y: 0.037 control2X: 0.004 y: 0.076;
		rel_CubicBezierToX: -0.118 y: 0.203 control1X: -0.017 y: 0.086 control2X: -0.056 y: 0.153;
		rel_CubicBezierToX: -0.230 y: 0.077 control1X: -0.062 y: 0.049 control2X: -0.138 y: 0.075;
		rel_LineToX: 0.015 y: -0.075! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics65
	vectorGraphicsEngine
		rel_MoveToX: 0.315 y: 0.324;
		rel_LineToX: -0.206 y: 0.363;
		rel_LineToX: 0.268 y: 0.000;
		rel_LineToX: -0.062 y: -0.363;
		rel_MoveToX: -0.036 y: -0.097;
		rel_LineToX: 0.112 y: 0.000;
		rel_LineToX: 0.132 y: 0.729;
		rel_LineToX: -0.103 y: 0.000;
		rel_LineToX: -0.029 y: -0.187;
		rel_LineToX: -0.329 y: 0.000;
		rel_LineToX: -0.104 y: 0.187;
		rel_LineToX: -0.104 y: 0.000;
		rel_LineToX: 0.424 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics66
	vectorGraphicsEngine
		rel_MoveToX: 0.113 y: 0.608;
		rel_LineToX: -0.053 y: 0.267;
		rel_LineToX: 0.158 y: 0.000;
		rel_CubicBezierToX: 0.124 y: -0.033 control1X: 0.053 y: 0.000 control2X: 0.094 y: -0.011;
		rel_CubicBezierToX: 0.059 y: -0.101 control1X: 0.030 y: -0.022 control2X: 0.050 y: -0.056;
		rel_CubicBezierToX: -0.018 y: -0.101 control1X: 0.009 y: -0.045 control2X: 0.003 y: -0.079;
		rel_CubicBezierToX: -0.111 y: -0.033 control1X: -0.021 y: -0.022 control2X: -0.058 y: -0.033;
		rel_LineToX: -0.158 y: 0.000;
		rel_MoveToX: 0.060 y: -0.300;
		rel_LineToX: -0.044 y: 0.220;
		rel_LineToX: 0.146 y: 0.000;
		rel_CubicBezierToX: 0.113 y: -0.027 control1X: 0.048 y: 0.000 control2X: 0.086 y: -0.009;
		rel_CubicBezierToX: 0.052 y: -0.083 control1X: 0.027 y: -0.018 control2X: 0.045 y: -0.046;
		rel_CubicBezierToX: -0.019 y: -0.083 control1X: 0.007 y: -0.037 control2X: 0.001 y: -0.064;
		rel_CubicBezierToX: -0.102 y: -0.027 control1X: -0.020 y: -0.018 control2X: -0.054 y: -0.027;
		rel_LineToX: -0.146 y: 0.000;
		rel_MoveToX: -0.082 y: -0.081;
		rel_LineToX: 0.252 y: 0.000;
		rel_CubicBezierToX: 0.164 y: 0.047 control1X: 0.075 y: 0.000 control2X: 0.130 y: 0.016;
		rel_CubicBezierToX: 0.034 y: 0.133 control1X: 0.034 y: 0.031 control2X: 0.046 y: 0.076;
		rel_CubicBezierToX: -0.052 y: 0.106 control1X: -0.009 y: 0.045 control2X: -0.026 y: 0.080;
		rel_CubicBezierToX: -0.102 y: 0.049 control1X: -0.026 y: 0.026 control2X: -0.060 y: 0.043;
		rel_CubicBezierToX: 0.100 y: 0.065 control1X: 0.046 y: 0.010 control2X: 0.080 y: 0.032;
		rel_CubicBezierToX: 0.016 y: 0.124 control1X: 0.020 y: 0.033 control2X: 0.026 y: 0.074;
		rel_CubicBezierToX: -0.097 y: 0.151 control1X: -0.013 y: 0.065 control2X: -0.045 y: 0.115;
		rel_CubicBezierToX: -0.200 y: 0.053 control1X: -0.051 y: 0.035 control2X: -0.118 y: 0.053;
		rel_LineToX: -0.262 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics67
	vectorGraphicsEngine
		rel_MoveToX: 0.625 y: 0.283;
		rel_LineToX: -0.021 y: 0.104;
		rel_CubicBezierToX: -0.093 y: -0.069 control1X: -0.027 y: -0.031 control2X: -0.058 y: -0.054;
		rel_CubicBezierToX: -0.115 y: -0.023 control1X: -0.034 y: -0.015 control2X: -0.073 y: -0.023;
		rel_CubicBezierToX: -0.207 y: 0.077 control1X: -0.083 y: 0.000 control2X: -0.152 y: 0.026;
		rel_CubicBezierToX: -0.111 y: 0.221 control1X: -0.054 y: 0.051 control2X: -0.091 y: 0.124;
		rel_CubicBezierToX: 0.022 y: 0.221 control1X: -0.019 y: 0.096 control2X: -0.012 y: 0.170;
		rel_CubicBezierToX: 0.176 y: 0.076 control1X: 0.034 y: 0.051 control2X: 0.093 y: 0.076;
		rel_CubicBezierToX: 0.124 y: -0.023 control1X: 0.042 y: 0.000 control2X: 0.084 y: -0.008;
		rel_CubicBezierToX: 0.120 y: -0.069 control1X: 0.041 y: -0.015 control2X: 0.081 y: -0.038;
		rel_LineToX: -0.021 y: 0.103;
		rel_CubicBezierToX: -0.120 y: 0.053 control1X: -0.039 y: 0.023 control2X: -0.079 y: 0.041;
		rel_CubicBezierToX: -0.126 y: 0.017 control1X: -0.041 y: 0.012 control2X: -0.083 y: 0.017;
		rel_CubicBezierToX: -0.241 y: -0.101 control1X: -0.110 y: 0.000 control2X: -0.191 y: -0.034;
		rel_CubicBezierToX: -0.040 y: -0.277 control1X: -0.050 y: -0.068 control2X: -0.063 y: -0.160;
		rel_CubicBezierToX: 0.151 y: -0.277 control1X: 0.023 y: -0.117 control2X: 0.074 y: -0.210;
		rel_CubicBezierToX: 0.281 y: -0.102 control1X: 0.077 y: -0.068 control2X: 0.171 y: -0.102;
		rel_CubicBezierToX: 0.119 y: 0.018 control1X: 0.044 y: 0.000 control2X: 0.083 y: 0.006;
		rel_CubicBezierToX: 0.099 y: 0.052 control1X: 0.037 y: 0.011 control2X: 0.069 y: 0.029! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics68
	vectorGraphicsEngine
		rel_MoveToX: 0.173 y: 0.308;
		rel_LineToX: -0.113 y: 0.567;
		rel_LineToX: 0.119 y: 0.000;
		rel_CubicBezierToX: 0.234 y: -0.068 control1X: 0.101 y: 0.000 control2X: 0.179 y: -0.023;
		rel_CubicBezierToX: 0.113 y: -0.216 control1X: 0.056 y: -0.046 control2X: 0.094 y: -0.117;
		rel_CubicBezierToX: -0.027 y: -0.214 control1X: 0.020 y: -0.098 control2X: 0.010 y: -0.169;
		rel_CubicBezierToX: -0.207 y: -0.068 control1X: -0.037 y: -0.046 control2X: -0.106 y: -0.068;
		rel_LineToX: -0.119 y: 0.000;
		rel_MoveToX: -0.082 y: -0.081;
		rel_LineToX: 0.203 y: 0.000;
		rel_CubicBezierToX: 0.293 y: 0.088 control1X: 0.141 y: 0.000 control2X: 0.239 y: 0.029;
		rel_CubicBezierToX: 0.044 y: 0.275 control1X: 0.054 y: 0.059 control2X: 0.069 y: 0.150;
		rel_CubicBezierToX: -0.155 y: 0.277 control1X: -0.025 y: 0.126 control2X: -0.077 y: 0.218;
		rel_CubicBezierToX: -0.328 y: 0.088 control1X: -0.078 y: 0.059 control2X: -0.188 y: 0.088;
		rel_LineToX: -0.203 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics69
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.461 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.362 y: 0.000;
		rel_LineToX: -0.043 y: 0.216;
		rel_LineToX: 0.347 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.347 y: 0.000;
		rel_LineToX: -0.053 y: 0.264;
		rel_LineToX: 0.371 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.470 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics70
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.419 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.320 y: 0.000;
		rel_LineToX: -0.043 y: 0.215;
		rel_LineToX: 0.289 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.289 y: 0.000;
		rel_LineToX: -0.070 y: 0.348;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics71
	vectorGraphicsEngine
		rel_MoveToX: 0.463 y: 0.852;
		rel_LineToX: 0.039 y: -0.196;
		rel_LineToX: -0.161 y: 0.000;
		rel_LineToX: 0.016 y: -0.081;
		rel_LineToX: 0.259 y: 0.000;
		rel_LineToX: -0.063 y: 0.313;
		rel_CubicBezierToX: -0.138 y: 0.062 control1X: -0.043 y: 0.027 control2X: -0.090 y: 0.047;
		rel_CubicBezierToX: -0.151 y: 0.020 control1X: -0.049 y: 0.014 control2X: -0.099 y: 0.020;
		rel_CubicBezierToX: -0.248 y: -0.100 control1X: -0.114 y: 0.000 control2X: -0.196 y: -0.033;
		rel_CubicBezierToX: -0.041 y: -0.278 control1X: -0.051 y: -0.067 control2X: -0.064 y: -0.159;
		rel_CubicBezierToX: 0.152 y: -0.278 control1X: 0.024 y: -0.119 control2X: 0.074 y: -0.212;
		rel_CubicBezierToX: 0.288 y: -0.100 control1X: 0.078 y: -0.067 control2X: 0.174 y: -0.100;
		rel_CubicBezierToX: 0.132 y: 0.018 control1X: 0.047 y: 0.000 control2X: 0.091 y: 0.006;
		rel_CubicBezierToX: 0.108 y: 0.052 control1X: 0.041 y: 0.012 control2X: 0.077 y: 0.029;
		rel_LineToX: -0.021 y: 0.105;
		rel_CubicBezierToX: -0.102 y: -0.070 control1X: -0.030 y: -0.031 control2X: -0.064 y: -0.054;
		rel_CubicBezierToX: -0.125 y: -0.024 control1X: -0.038 y: -0.016 control2X: -0.079 y: -0.024;
		rel_CubicBezierToX: -0.216 y: 0.075 control1X: -0.089 y: 0.000 control2X: -0.161 y: 0.025;
		rel_CubicBezierToX: -0.111 y: 0.223 control1X: -0.055 y: 0.050 control2X: -0.092 y: 0.124;
		rel_CubicBezierToX: 0.022 y: 0.222 control1X: -0.020 y: 0.098 control2X: -0.012 y: 0.172;
		rel_CubicBezierToX: 0.186 y: 0.075 control1X: 0.035 y: 0.050 control2X: 0.097 y: 0.075;
		rel_CubicBezierToX: 0.095 y: -0.009 control1X: 0.035 y: 0.000 control2X: 0.066 y: -0.003;
		rel_CubicBezierToX: 0.079 y: -0.028 control1X: 0.029 y: -0.006 control2X: 0.055 y: -0.016! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics72
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.060 y: 0.299;
		rel_LineToX: 0.358 y: 0.000;
		rel_LineToX: 0.060 y: -0.299;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.069 y: -0.347;
		rel_LineToX: -0.358 y: 0.000;
		rel_LineToX: -0.069 y: 0.347;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics73
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics74
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.136 y: 0.678;
		rel_CubicBezierToX: -0.088 y: 0.191 control1X: -0.018 y: 0.088 control2X: -0.047 y: 0.152;
		rel_CubicBezierToX: -0.173 y: 0.060 control1X: -0.041 y: 0.040 control2X: -0.099 y: 0.060;
		rel_LineToX: -0.038 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_LineToX: 0.031 y: 0.000;
		rel_CubicBezierToX: 0.100 y: -0.037 control1X: 0.044 y: 0.000 control2X: 0.077 y: -0.012;
		rel_CubicBezierToX: 0.053 y: -0.131 control1X: 0.023 y: -0.024 control2X: 0.040 y: -0.068;
		rel_LineToX: 0.136 y: -0.678! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics75
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.062 y: 0.308;
		rel_LineToX: 0.389 y: -0.308;
		rel_LineToX: 0.127 y: 0.000;
		rel_LineToX: -0.430 y: 0.340;
		rel_LineToX: 0.310 y: 0.389;
		rel_LineToX: -0.130 y: 0.000;
		rel_LineToX: -0.280 y: -0.351;
		rel_LineToX: -0.070 y: 0.351;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics76
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.129 y: 0.646;
		rel_LineToX: 0.355 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.454 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics77
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.147 y: 0.000;
		rel_LineToX: 0.087 y: 0.496;
		rel_LineToX: 0.286 y: -0.496;
		rel_LineToX: 0.147 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.096 y: 0.000;
		rel_LineToX: 0.128 y: -0.640;
		rel_LineToX: -0.288 y: 0.500;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: -0.088 y: -0.500;
		rel_LineToX: -0.128 y: 0.640;
		rel_LineToX: -0.096 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics78
	vectorGraphicsEngine
		rel_MoveToX: 0.090 y: 0.227;
		rel_LineToX: 0.133 y: 0.000;
		rel_LineToX: 0.201 y: 0.610;
		rel_LineToX: 0.122 y: -0.610;
		rel_LineToX: 0.096 y: 0.000;
		rel_LineToX: -0.146 y: 0.729;
		rel_LineToX: -0.133 y: 0.000;
		rel_LineToX: -0.201 y: -0.610;
		rel_LineToX: -0.122 y: 0.610;
		rel_LineToX: -0.096 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics79
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.016 y: -0.080;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.147 y: 0.275 control1X: -0.023 y: 0.115 control2X: -0.072 y: 0.207;
		rel_CubicBezierToX: -0.266 y: 0.103 control1X: -0.075 y: 0.068 control2X: -0.163 y: 0.103;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.103 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.048 y: -0.068 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics80
	vectorGraphicsEngine
		rel_MoveToX: 0.173 y: 0.308;
		rel_LineToX: -0.055 y: 0.274;
		rel_LineToX: 0.124 y: 0.000;
		rel_CubicBezierToX: 0.114 y: -0.036 control1X: 0.046 y: 0.000 control2X: 0.084 y: -0.012;
		rel_CubicBezierToX: 0.058 y: -0.102 control1X: 0.030 y: -0.024 control2X: 0.049 y: -0.058;
		rel_CubicBezierToX: -0.017 y: -0.101 control1X: 0.009 y: -0.044 control2X: 0.003 y: -0.077;
		rel_CubicBezierToX: -0.099 y: -0.036 control1X: -0.020 y: -0.024 control2X: -0.053 y: -0.036;
		rel_LineToX: -0.124 y: 0.000;
		rel_MoveToX: -0.082 y: -0.081;
		rel_LineToX: 0.223 y: 0.000;
		rel_CubicBezierToX: 0.174 y: 0.056 control1X: 0.082 y: 0.000 control2X: 0.140 y: 0.018;
		rel_CubicBezierToX: 0.031 y: 0.162 control1X: 0.035 y: 0.037 control2X: 0.045 y: 0.091;
		rel_CubicBezierToX: -0.096 y: 0.163 control1X: -0.014 y: 0.072 control2X: -0.046 y: 0.126;
		rel_CubicBezierToX: -0.196 y: 0.055 control1X: -0.049 y: 0.037 control2X: -0.114 y: 0.055;
		rel_LineToX: -0.124 y: 0.000;
		rel_LineToX: -0.059 y: 0.293;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics81
	vectorGraphicsEngine
		rel_MoveToX: 0.373 y: 0.294;
		rel_CubicBezierToX: -0.187 y: 0.080 control1X: -0.072 y: 0.000 control2X: -0.134 y: 0.027;
		rel_CubicBezierToX: -0.107 y: 0.218 control1X: -0.053 y: 0.053 control2X: -0.088 y: 0.126;
		rel_CubicBezierToX: 0.019 y: 0.218 control1X: -0.018 y: 0.092 control2X: -0.012 y: 0.164;
		rel_CubicBezierToX: 0.155 y: 0.080 control1X: 0.032 y: 0.053 control2X: 0.083 y: 0.080;
		rel_CubicBezierToX: 0.186 y: -0.080 control1X: 0.072 y: 0.000 control2X: 0.134 y: -0.027;
		rel_CubicBezierToX: 0.107 y: -0.218 control1X: 0.053 y: -0.053 control2X: 0.088 y: -0.126;
		rel_CubicBezierToX: -0.019 y: -0.218 control1X: 0.018 y: -0.092 control2X: 0.012 y: -0.165;
		rel_CubicBezierToX: -0.154 y: -0.080 control1X: -0.031 y: -0.053 control2X: -0.082 y: -0.080;
		rel_MoveToX: 0.008 y: 0.649;
		rel_LineToX: 0.101 y: 0.142;
		rel_LineToX: -0.119 y: 0.000;
		rel_LineToX: -0.085 y: -0.117;
		rel_CubicBezierToX: -0.025 y: 0.001 control1X: -0.011 y: 0.001 control2X: -0.019 y: 0.001;
		rel_CubicBezierToX: -0.016 y: 0.000 control1X: -0.006 y: 0.000 control2X: -0.011 y: 0.000;
		rel_CubicBezierToX: -0.226 y: -0.103 control1X: -0.102 y: 0.000 control2X: -0.178 y: -0.034;
		rel_CubicBezierToX: -0.037 y: -0.275 control1X: -0.047 y: -0.069 control2X: -0.060 y: -0.160;
		rel_CubicBezierToX: 0.147 y: -0.275 control1X: 0.023 y: -0.115 control2X: 0.072 y: -0.207;
		rel_CubicBezierToX: 0.267 y: -0.103 control1X: 0.075 y: -0.069 control2X: 0.164 y: -0.103;
		rel_CubicBezierToX: 0.224 y: 0.103 control1X: 0.102 y: 0.000 control2X: 0.177 y: 0.034;
		rel_CubicBezierToX: 0.037 y: 0.275 control1X: 0.048 y: 0.068 control2X: 0.060 y: 0.160;
		rel_CubicBezierToX: -0.095 y: 0.217 control1X: -0.017 y: 0.085 control2X: -0.049 y: 0.157;
		rel_CubicBezierToX: -0.174 y: 0.133 control1X: -0.046 y: 0.060 control2X: -0.104 y: 0.105! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics82
	vectorGraphicsEngine
		rel_MoveToX: 0.359 y: 0.614;
		rel_CubicBezierToX: 0.052 y: 0.046 control1X: 0.020 y: 0.007 control2X: 0.037 y: 0.022;
		rel_CubicBezierToX: 0.041 y: 0.097 control1X: 0.016 y: 0.023 control2X: 0.029 y: 0.056;
		rel_LineToX: 0.060 y: 0.199;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: -0.056 y: -0.187;
		rel_CubicBezierToX: -0.051 y: -0.097 control1X: -0.014 y: -0.049 control2X: -0.031 y: -0.081;
		rel_CubicBezierToX: -0.088 y: -0.024 control1X: -0.019 y: -0.016 control2X: -0.049 y: -0.024;
		rel_LineToX: -0.107 y: 0.000;
		rel_LineToX: -0.062 y: 0.308;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.146 y: -0.729;
		rel_LineToX: 0.223 y: 0.000;
		rel_CubicBezierToX: 0.176 y: 0.052 control1X: 0.083 y: 0.000 control2X: 0.142 y: 0.017;
		rel_CubicBezierToX: 0.030 y: 0.158 control1X: 0.034 y: 0.035 control2X: 0.044 y: 0.087;
		rel_CubicBezierToX: -0.055 y: 0.114 control1X: -0.009 y: 0.046 control2X: -0.028 y: 0.084;
		rel_CubicBezierToX: -0.105 y: 0.063 control1X: -0.027 y: 0.030 control2X: -0.062 y: 0.051;
		rel_MoveToX: -0.186 y: -0.306;
		rel_LineToX: -0.052 y: 0.259;
		rel_LineToX: 0.124 y: 0.000;
		rel_CubicBezierToX: 0.114 y: -0.033 control1X: 0.047 y: 0.000 control2X: 0.085 y: -0.011;
		rel_CubicBezierToX: 0.056 y: -0.097 control1X: 0.029 y: -0.022 control2X: 0.048 y: -0.054;
		rel_CubicBezierToX: -0.017 y: -0.096 control1X: 0.009 y: -0.043 control2X: 0.003 y: -0.075;
		rel_CubicBezierToX: -0.101 y: -0.033 control1X: -0.020 y: -0.022 control2X: -0.053 y: -0.033;
		rel_LineToX: -0.124 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics83
	vectorGraphicsEngine
		rel_MoveToX: 0.523 y: 0.251;
		rel_LineToX: -0.019 y: 0.096;
		rel_CubicBezierToX: -0.098 y: -0.040 control1X: -0.034 y: -0.018 control2X: -0.066 y: -0.031;
		rel_CubicBezierToX: -0.094 y: -0.013 control1X: -0.031 y: -0.009 control2X: -0.063 y: -0.013;
		rel_CubicBezierToX: -0.131 y: 0.031 control1X: -0.054 y: 0.000 control2X: -0.097 y: 0.010;
		rel_CubicBezierToX: -0.061 y: 0.089 control1X: -0.033 y: 0.021 control2X: -0.054 y: 0.050;
		rel_CubicBezierToX: 0.014 y: 0.073 control1X: -0.006 y: 0.032 control2X: -0.002 y: 0.057;
		rel_CubicBezierToX: 0.102 y: 0.040 control1X: 0.016 y: 0.016 control2X: 0.050 y: 0.030;
		rel_LineToX: 0.057 y: 0.012;
		rel_CubicBezierToX: 0.148 y: 0.074 control1X: 0.071 y: 0.014 control2X: 0.120 y: 0.039;
		rel_CubicBezierToX: 0.024 y: 0.142 control1X: 0.028 y: 0.035 control2X: 0.036 y: 0.082;
		rel_CubicBezierToX: -0.103 y: 0.161 control1X: -0.014 y: 0.071 control2X: -0.049 y: 0.124;
		rel_CubicBezierToX: -0.219 y: 0.055 control1X: -0.054 y: 0.036 control2X: -0.128 y: 0.055;
		rel_CubicBezierToX: -0.108 y: -0.012 control1X: -0.034 y: 0.000 control2X: -0.071 y: -0.004;
		rel_CubicBezierToX: -0.114 y: -0.035 control1X: -0.037 y: -0.008 control2X: -0.075 y: -0.019;
		rel_LineToX: 0.020 y: -0.102;
		rel_CubicBezierToX: 0.108 y: 0.051 control1X: 0.036 y: 0.022 control2X: 0.071 y: 0.039;
		rel_CubicBezierToX: 0.110 y: 0.017 control1X: 0.036 y: 0.011 control2X: 0.073 y: 0.017;
		rel_CubicBezierToX: 0.137 y: -0.033 control1X: 0.056 y: 0.000 control2X: 0.102 y: -0.011;
		rel_CubicBezierToX: 0.065 y: -0.095 control1X: 0.035 y: -0.022 control2X: 0.057 y: -0.054;
		rel_CubicBezierToX: -0.016 y: -0.084 control1X: 0.007 y: -0.036 control2X: 0.002 y: -0.064;
		rel_CubicBezierToX: -0.099 y: -0.045 control1X: -0.018 y: -0.020 control2X: -0.051 y: -0.035;
		rel_LineToX: -0.058 y: -0.012;
		rel_CubicBezierToX: -0.146 y: -0.069 control1X: -0.071 y: -0.015 control2X: -0.119 y: -0.038;
		rel_CubicBezierToX: -0.023 y: -0.130 control1X: -0.027 y: -0.031 control2X: -0.034 y: -0.075;
		rel_CubicBezierToX: 0.098 y: -0.152 control1X: 0.013 y: -0.065 control2X: 0.046 y: -0.115;
		rel_CubicBezierToX: 0.199 y: -0.056 control1X: 0.053 y: -0.037 control2X: 0.119 y: -0.056;
		rel_CubicBezierToX: 0.103 y: 0.009 control1X: 0.034 y: 0.000 control2X: 0.068 y: 0.003;
		rel_CubicBezierToX: 0.103 y: 0.028 control1X: 0.034 y: 0.006 control2X: 0.069 y: 0.015! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics84
	vectorGraphicsEngine
		rel_MoveToX: -0.011 y: 0.227;
		rel_LineToX: 0.617 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.259 y: 0.000;
		rel_LineToX: -0.129 y: 0.646;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.129 y: -0.646;
		rel_LineToX: -0.259 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics85
	vectorGraphicsEngine
		rel_MoveToX: 0.079 y: 0.227;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.089 y: 0.443;
		rel_CubicBezierToX: 0.009 y: 0.169 control1X: -0.016 y: 0.078 control2X: -0.013 y: 0.134;
		rel_CubicBezierToX: 0.127 y: 0.051 control1X: 0.021 y: 0.034 control2X: 0.064 y: 0.051;
		rel_CubicBezierToX: 0.147 y: -0.051 control1X: 0.063 y: 0.000 control2X: 0.112 y: -0.017;
		rel_CubicBezierToX: 0.076 y: -0.169 control1X: 0.035 y: -0.034 control2X: 0.061 y: -0.091;
		rel_LineToX: 0.089 y: -0.443;
		rel_LineToX: 0.099 y: 0.000;
		rel_LineToX: -0.091 y: 0.455;
		rel_CubicBezierToX: -0.114 y: 0.215 control1X: -0.019 y: 0.095 control2X: -0.057 y: 0.167;
		rel_CubicBezierToX: -0.223 y: 0.073 control1X: -0.057 y: 0.048 control2X: -0.131 y: 0.073;
		rel_CubicBezierToX: -0.194 y: -0.073 control1X: -0.092 y: 0.000 control2X: -0.157 y: -0.024;
		rel_CubicBezierToX: -0.027 y: -0.215 control1X: -0.037 y: -0.048 control2X: -0.046 y: -0.120;
		rel_LineToX: 0.091 y: -0.455! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics86
	vectorGraphicsEngine
		rel_MoveToX: 0.133 y: 0.956;
		rel_LineToX: -0.132 y: -0.729;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: 0.108 y: 0.614;
		rel_LineToX: 0.354 y: -0.614;
		rel_LineToX: 0.103 y: 0.000;
		rel_LineToX: -0.424 y: 0.729;
		rel_LineToX: -0.112 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics87
	vectorGraphicsEngine
		rel_MoveToX: 0.026 y: 0.227;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: 0.030 y: 0.616;
		rel_LineToX: 0.276 y: -0.616;
		rel_LineToX: 0.111 y: 0.000;
		rel_LineToX: 0.030 y: 0.616;
		rel_LineToX: 0.276 y: -0.616;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: -0.329 y: 0.729;
		rel_LineToX: -0.124 y: 0.000;
		rel_LineToX: -0.027 y: -0.633;
		rel_LineToX: -0.282 y: 0.633;
		rel_LineToX: -0.124 y: 0.000;
		rel_LineToX: -0.037 y: -0.729! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics88
	vectorGraphicsEngine
		rel_MoveToX: 0.055 y: 0.227;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.127 y: 0.271;
		rel_LineToX: 0.236 y: -0.271;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: -0.304 y: 0.350;
		rel_LineToX: 0.174 y: 0.379;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: -0.143 y: -0.310;
		rel_LineToX: -0.269 y: 0.310;
		rel_LineToX: -0.106 y: 0.000;
		rel_LineToX: 0.338 y: -0.389;
		rel_LineToX: -0.159 y: -0.340! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics89
	vectorGraphicsEngine
		rel_MoveToX: -0.010 y: 0.227;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: 0.142 y: 0.300;
		rel_LineToX: 0.261 y: -0.300;
		rel_LineToX: 0.106 y: 0.000;
		rel_LineToX: -0.334 y: 0.382;
		rel_LineToX: -0.069 y: 0.347;
		rel_LineToX: -0.099 y: 0.000;
		rel_LineToX: 0.069 y: -0.347;
		rel_LineToX: -0.181 y: -0.382! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics90
	vectorGraphicsEngine
		rel_MoveToX: 0.048 y: 0.227;
		rel_LineToX: 0.573 y: 0.000;
		rel_LineToX: -0.015 y: 0.075;
		rel_LineToX: -0.575 y: 0.571;
		rel_LineToX: 0.472 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.595 y: 0.000;
		rel_LineToX: 0.015 y: -0.075;
		rel_LineToX: 0.575 y: -0.571;
		rel_LineToX: -0.450 y: 0.000;
		rel_LineToX: 0.017 y: -0.083! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics91
	vectorGraphicsEngine
		rel_MoveToX: 0.084 y: 0.196;
		rel_LineToX: 0.207 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.117 y: 0.000;
		rel_LineToX: -0.150 y: 0.752;
		rel_LineToX: 0.117 y: 0.000;
		rel_LineToX: -0.014 y: 0.070;
		rel_LineToX: -0.207 y: 0.000;
		rel_LineToX: 0.178 y: -0.892! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics92
	vectorGraphicsEngine
		rel_MoveToX: 0.075 y: 0.227;
		rel_LineToX: 0.090 y: 0.822;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: -0.090 y: -0.822;
		rel_LineToX: 0.083 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics93
	vectorGraphicsEngine
		rel_MoveToX: 0.303 y: 0.196;
		rel_LineToX: -0.178 y: 0.892;
		rel_LineToX: -0.207 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.117 y: 0.000;
		rel_LineToX: 0.150 y: -0.752;
		rel_LineToX: -0.117 y: 0.000;
		rel_LineToX: 0.014 y: -0.070;
		rel_LineToX: 0.207 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics94
	vectorGraphicsEngine
		rel_MoveToX: 0.390 y: 0.224;
		rel_LineToX: 0.044 y: 0.000;
		rel_LineToX: 0.153 y: 0.192;
		rel_LineToX: -0.070 y: 0.059;
		rel_LineToX: -0.090 y: -0.114;
		rel_LineToX: -0.119 y: 0.596;
		rel_LineToX: -0.083 y: 0.000;
		rel_LineToX: 0.119 y: -0.596;
		rel_LineToX: -0.137 y: 0.114;
		rel_LineToX: -0.047 y: -0.059;
		rel_LineToX: 0.231 y: -0.192! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics95
	vectorGraphicsEngine
		rel_MoveToX: -0.046 y: 0.665;
		rel_LineToX: 0.009 y: -0.044;
		rel_LineToX: 0.230 y: -0.192;
		rel_LineToX: 0.047 y: 0.059;
		rel_LineToX: -0.137 y: 0.114;
		rel_LineToX: 0.595 y: 0.000;
		rel_LineToX: -0.017 y: 0.083;
		rel_LineToX: -0.595 y: 0.000;
		rel_LineToX: 0.091 y: 0.114;
		rel_LineToX: -0.070 y: 0.059;
		rel_LineToX: -0.154 y: -0.192! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics96
	vectorGraphicsEngine
		rel_MoveToX: 0.186 y: 0.156;
		rel_LineToX: 0.101 y: 0.183;
		rel_LineToX: -0.075 y: 0.000;
		rel_LineToX: -0.123 y: -0.183;
		rel_LineToX: 0.096 y: 0.000! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics97
	vectorGraphicsEngine
		rel_MoveToX: 0.244 y: 0.681;
		rel_CubicBezierToX: -0.156 y: 0.025 control1X: -0.073 y: 0.000 control2X: -0.124 y: 0.008;
		rel_CubicBezierToX: -0.059 y: 0.085 control1X: -0.031 y: 0.017 control2X: -0.051 y: 0.045;
		rel_CubicBezierToX: 0.016 y: 0.076 control1X: -0.006 y: 0.032 control2X: -0.001 y: 0.057;
		rel_CubicBezierToX: 0.080 y: 0.028 control1X: 0.017 y: 0.019 control2X: 0.044 y: 0.028;
		rel_CubicBezierToX: 0.130 y: -0.053 control1X: 0.050 y: 0.000 control2X: 0.093 y: -0.017;
		rel_CubicBezierToX: 0.074 y: -0.141 control1X: 0.037 y: -0.035 control2X: 0.062 y: -0.083;
		rel_LineToX: 0.004 y: -0.020;
		rel_LineToX: -0.089 y: 0.000;
		rel_MoveToX: 0.187 y: -0.037;
		rel_LineToX: -0.062 y: 0.312;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: -0.091 y: 0.074 control1X: -0.027 y: 0.033 control2X: -0.058 y: 0.058;
		rel_CubicBezierToX: -0.117 y: 0.023 control1X: -0.034 y: 0.016 control2X: -0.073 y: 0.023;
		rel_CubicBezierToX: -0.124 y: -0.047 control1X: -0.056 y: 0.000 control2X: -0.097 y: -0.016;
		rel_CubicBezierToX: -0.024 y: -0.127 control1X: -0.026 y: -0.032 control2X: -0.035 y: -0.074;
		rel_CubicBezierToX: 0.089 y: -0.139 control1X: 0.012 y: -0.062 control2X: 0.042 y: -0.108;
		rel_CubicBezierToX: 0.194 y: -0.047 control1X: 0.048 y: -0.031 control2X: 0.112 y: -0.047;
		rel_LineToX: 0.126 y: 0.000;
		rel_LineToX: 0.002 y: -0.009;
		rel_CubicBezierToX: -0.022 y: -0.096 control1X: 0.008 y: -0.041 control2X: 0.001 y: -0.073;
		rel_CubicBezierToX: -0.107 y: -0.034 control1X: -0.022 y: -0.023 control2X: -0.058 y: -0.034;
		rel_CubicBezierToX: -0.094 y: 0.011 control1X: -0.031 y: 0.000 control2X: -0.062 y: 0.004;
		rel_CubicBezierToX: -0.092 y: 0.034 control1X: -0.031 y: 0.008 control2X: -0.062 y: 0.019;
		rel_LineToX: 0.017 y: -0.083;
		rel_CubicBezierToX: 0.101 y: -0.028 control1X: 0.035 y: -0.013 control2X: 0.069 y: -0.022;
		rel_CubicBezierToX: 0.092 y: -0.010 control1X: 0.032 y: -0.007 control2X: 0.063 y: -0.010;
		rel_CubicBezierToX: 0.165 y: 0.062 control1X: 0.079 y: 0.000 control2X: 0.134 y: 0.021;
		rel_CubicBezierToX: 0.021 y: 0.187 control1X: 0.031 y: 0.041 control2X: 0.038 y: 0.103! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics98
	vectorGraphicsEngine
		rel_MoveToX: 0.388 y: 0.683;
		rel_CubicBezierToX: -0.010 y: -0.155 control1X: 0.013 y: -0.066 control2X: 0.010 y: -0.118;
		rel_CubicBezierToX: -0.100 y: -0.057 control1X: -0.020 y: -0.038 control2X: -0.053 y: -0.057;
		rel_CubicBezierToX: -0.124 y: 0.057 control1X: -0.048 y: 0.000 control2X: -0.089 y: 0.019;
		rel_CubicBezierToX: -0.072 y: 0.155 control1X: -0.034 y: 0.037 control2X: -0.058 y: 0.089;
		rel_CubicBezierToX: 0.009 y: 0.156 control1X: -0.013 y: 0.066 control2X: -0.010 y: 0.118;
		rel_CubicBezierToX: 0.101 y: 0.056 control1X: 0.020 y: 0.037 control2X: 0.054 y: 0.056;
		rel_CubicBezierToX: 0.123 y: -0.056 control1X: 0.047 y: 0.000 control2X: 0.088 y: -0.019;
		rel_CubicBezierToX: 0.072 y: -0.156 control1X: 0.035 y: -0.038 control2X: 0.059 y: -0.090;
		rel_MoveToX: -0.268 y: -0.191;
		rel_CubicBezierToX: 0.086 y: -0.072 control1X: 0.025 y: -0.032 control2X: 0.054 y: -0.057;
		rel_CubicBezierToX: 0.108 y: -0.024 control1X: 0.032 y: -0.016 control2X: 0.068 y: -0.024;
		rel_CubicBezierToX: 0.146 y: 0.079 control1X: 0.066 y: 0.000 control2X: 0.115 y: 0.026;
		rel_CubicBezierToX: 0.021 y: 0.208 control1X: 0.031 y: 0.053 control2X: 0.038 y: 0.122;
		rel_CubicBezierToX: -0.104 y: 0.208 control1X: -0.017 y: 0.086 control2X: -0.052 y: 0.155;
		rel_CubicBezierToX: -0.177 y: 0.079 control1X: -0.052 y: 0.053 control2X: -0.111 y: 0.079;
		rel_CubicBezierToX: -0.099 y: -0.023 control1X: -0.040 y: 0.000 control2X: -0.073 y: -0.008;
		rel_CubicBezierToX: -0.057 y: -0.073 control1X: -0.025 y: -0.016 control2X: -0.044 y: -0.040;
		rel_LineToX: -0.016 y: 0.082;
		rel_LineToX: -0.090 y: 0.000;
		rel_LineToX: 0.152 y: -0.760;
		rel_LineToX: 0.090 y: 0.000;
		rel_LineToX: -0.059 y: 0.296! !

!BitstreamVeraRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:35'!
drawVeraSansItalics99
	vectorGraphicsEngine
		rel_MoveToX: 0.440 y: 0.430;
		rel_LineToX: -0.017 y: 0.084;
		rel_CubicBezierToX: -0.070 y: -0.031 control1X: -0.023 y: -0.014 control2X: -0.046 y: -0.024;
		rel_CubicBezierToX: -0.075 y: -0.011 control1X: -0.024 y: -0.007 control2X: -0.049 y: -0.011;
		rel_CubicBezierToX: -0.147 y: 0.056 control1X: -0.058 y: 0.000 control2X: -0.107 y: 0.019;
		rel_CubicBezierToX: -0.079 y: 0.155 control1X: -0.040 y: 0.037 control2X: -0.066 y: 0.089;
		rel_CubicBezierToX: 0.017 y: 0.156 control1X: -0.013 y: 0.067 control2X: -0.008 y: 0.119;
		rel_CubicBezierToX: 0.125 y: 0.055 control1X: 0.025 y: 0.037 control2X: 0.066 y: 0.055;
		rel_CubicBezierToX: 0.079 y: -0.010 control1X: 0.026 y: 0.000 control2X: 0.052 y: -0.003;
		rel_CubicBezierToX: 0.083 y: -0.032 control1X: 0.027 y: -0.007 control2X: 0.055 y: -0.018;
		rel_LineToX: -0.017 y: 0.083;
		rel_CubicBezierToX: -0.083 y: 0.026 control1X: -0.027 y: 0.012 control2X: -0.055 y: 0.020;
		rel_CubicBezierToX: -0.087 y: 0.009 control1X: -0.028 y: 0.006 control2X: -0.057 y: 0.009;
		rel_CubicBezierToX: -0.181 y: -0.078 control1X: -0.082 y: 0.000 control2X: -0.143 y: -0.026;
		rel_CubicBezierToX: -0.031 y: -0.210 control1X: -0.038 y: -0.052 control2X: -0.048 y: -0.122;
		rel_CubicBezierToX: 0.115 y: -0.210 control1X: 0.018 y: -0.089 control2X: 0.056 y: -0.159;
		rel_CubicBezierToX: 0.217 y: -0.077 control1X: 0.059 y: -0.051 control2X: 0.132 y: -0.077;
		rel_CubicBezierToX: 0.079 y: 0.009 control1X: 0.028 y: 0.000 control2X: 0.054 y: 0.003;
		rel_CubicBezierToX: 0.072 y: 0.025 control1X: 0.025 y: 0.006 control2X: 0.049 y: 0.014! !

!BitstreamVeraRenderer class methodsFor: 'testing' stamp: 'jmv 8/16/2018 15:52:10'!
testCharacter: code
	"
	BitstreamVeraRenderer testCharacter: 100
	"
	| c r  loc |
	c _ (VectorCanvas onForm: Display) into: self runningWorld.
	loc _ AffineTransformation withRadians: 0.0 scale: 160 position: 38@22.
	c engine geometryTransformation: loc.
	r _ BitstreamVeraRenderer new vectorGraphicsEngine: c engine.
	c engine fillColor: Color green do: [ :e |
		r drawCharCode: code italics: false ].
	Display forceToScreen! !

!TechnicalFontRenderer methodsFor: 'services' stamp: 'jmv 8/17/2018 15:47:34'!
drawCharCode: charCode atX: x0 y: y0 pointSize: ps italics: doItalics
"No usado directamente por M3.
Usado solo por CodeGeneratingVectorEngine para generar los que si se usan.
Definicion de glyphs comoda para el diseniador"
	| symbol loc prevLoc |
"
| c |
c _ (VectorCanvas onForm: Display)
	into: self runningWorld.
c engine
	strokeWidth: 20 color: Color black do: [
		(TechnicalFontRenderer new vectorGraphicsEngine: c engine)
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
		(TechnicalFontRenderer new vectorGraphicsEngine: c engine)
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
	prevLoc _ vectorGraphicsEngine geometryTransformation.
	loc _ (AffineTransformation withRadians: 0.0 scale: ps/10 position: x0@(y0+ps)) withYAxisNegated.
	doItalics ifTrue: [ loc _ loc italizing ].
	loc _ prevLoc composedWith: loc.
	vectorGraphicsEngine geometryTransformation: loc.

	symbol _ ('drawTechnical', charCode printString) asSymbol.
	vectorGraphicsEngine abs_MoveToX: 0.0 y: 0.0.
	self perform: symbol.

	vectorGraphicsEngine geometryTransformation: prevLoc.! !

!TechnicalFontRenderer methodsFor: 'services' stamp: 'jmv 8/17/2018 15:59:59'!
drawCharCode: charCode italics: doItalics
"Usado de verdad por M3.
Creado por CodeGeneratingVectorEngine a partir de #drawCharCode:atX:y:pointSize:italics:canvas:

Todos los glyphs usados de verdad son en comandos relativos.
"
	| symbol |
"
| e loc  r |
e _ (VectorCanvas onForm: Display)
	into: self runningWorld;
	engine.
loc _ AffineTransformation withRadians: 0.0 scale: 200 position: 38@222.
e geometryTransformation: loc.
r _ TechnicalFontRenderer new vectorGraphicsEngine: e.
e
	strokeWidth: 0.1 color: Color black do: [
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 0.52 y: 0.
		r drawCharCode: 97 italics: true ].
Display forceToScreen
"
"
| e loc r |
e _ (VectorCanvas onForm: Display)
	into: self runningWorld;
	engine.
loc _ AffineTransformation withRadians: 0.0 scale: 12 position: 38@22.
r _ TechnicalFontRenderer new vectorGraphicsEngine: e.
e geometryTransformation: loc.
e
	strokeWidth: 0.1 color: Color black do: [
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 0.6 y: 1/12/3.
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 1.2 y: 2/12/3.
		r drawCharCode: 97 italics: false.
		e abs_MoveToX: 1.8 y: 1/12.
		r drawCharCode: 97 italics: false.
		].
Display forceToScreen
"
	"revisar todo este metodo"
	self revisar.
	charCode < 33 ifTrue: [ ^self ].
	charCode = 127 ifTrue: [ ^self ].
	(charCode between: 132 and: 160) ifTrue: [ ^self ].
	"Implement them!!"
	charCode > 127 ifTrue: [ ^self ].

	doItalics ifFalse: [
		symbol _ ('draw_Technical', charCode printString) asSymbol.
	] ifTrue: [
		symbol _ ('draw_TechnicalItalics', charCode printString) asSymbol ].
	self perform: symbol! !

!TechnicalFontRenderer methodsFor: 'other' stamp: 'jmv 8/17/2018 15:50:56'!
drawM3BoxAtX: x0 y: y0 pointSize: ps italics: doItalics canvas: aVectorCanvas

	vectorGraphicsEngine geometryTransformation: (AffineTransformation withRadians: 0 scale: ps//10 position: x0@y0).
	aVectorCanvas strokeWidth: 0.15 color: Color red do: [
		-0.5 to: 9.5 do: [ :x |
			aVectorCanvas
				moveToX: x y: 0.5;
				lineToX: x y: 10.5 ].
		0.5 to: 13.5 do: [ :y |
			aVectorCanvas
				moveToX: -0.5 y: y;
				lineToX: 5.5 y: y ]].
	vectorGraphicsEngine geometryTransformation: aVectorCanvas currentTransformation! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:21'!
drawTechnical100
"
TechnicalFontRenderer testCharacter: 100.
"
	self
		m: 4 y: 6;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:19'!
drawTechnical101
"
TechnicalFontRenderer testCharacter: 101.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:46'!
drawTechnical102
"
TechnicalFontRenderer testCharacter: 102.
"
	self
		m: 1 y: 0;
		u: 7.5;
		cur: 1.5;
		r: 0.5;
		m: -3 y: -3;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:49'!
drawTechnical103
"
TechnicalFontRenderer testCharacter: 103.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:47'!
drawTechnical104
"
TechnicalFontRenderer testCharacter: 104.
"
	self
		m: 0 y: 0;
		u: 9;
		m: 0 y: -3;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:14'!
drawTechnical105
"
TechnicalFontRenderer testCharacter: 105.
"
	self
		m: 0 y: 0;
		u: 6;
		m: 0 y: 2.5;
		l: 0.15;
		u: 0.5;
		r: 0.3;
		d: 0.5;
		l: 0.15! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:26'!
drawTechnical106
"
TechnicalFontRenderer testCharacter: 106.
"
	self
		m: 1 y: 6;
		d: 7.5;
		cdl: 1.5;
		l: 0.5;
		m: 2 y: 11.5;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:30'!
drawTechnical107
"
TechnicalFontRenderer testCharacter: 107.
"
	self
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3;
		li: 4 y: 3;
		m: -2.6 y: -1.95;
		li: 2.6 y: -4.05! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:32'!
drawTechnical108
"
TechnicalFontRenderer testCharacter: 108.
"
	self
		m: 0 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:04'!
drawTechnical109
"
TechnicalFontRenderer testCharacter: 109.
"
	self
		m: 0 y: 0;
		u: 6;
		r: 4.5;
		crd: 1.5;
		d: 4.5;
		m: -3 y: 6;
		d: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:53'!
drawTechnical110
"
TechnicalFontRenderer testCharacter: 110.
"
	self
		m: 0 y: 0;
		u: 6;
		r: 2.5;
		crd: 1.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:18'!
drawTechnical111
"
TechnicalFontRenderer testCharacter: 111.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:28'!
drawTechnical112
"
TechnicalFontRenderer testCharacter: 112.
"
	self
		m: 0 y: -3;
		u: 9;
		r: 2.5;
		crd: 1.5;
		d: 3;
		cdl: 1.5;
		l: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:31'!
drawTechnical113
"
TechnicalFontRenderer testCharacter: 113.
"
	self
		m: 4 y: -3;
		u: 9;
		l: 2.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:40'!
drawTechnical114
"
TechnicalFontRenderer testCharacter: 114.
"
	self
		m: 0 y: 0;
		u: 6;
		r: 1.5;
		cDx: 1.5 dy: -0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:06'!
drawTechnical115
"
TechnicalFontRenderer testCharacter: 115.
"
	self
		m: 2 y: 3;
		l: 0.5;
		clur: 1.5;
		r: 1;
		cDx: 1.5 dy: -0.5;
		m: -2 y: -2.5 ;
		r: 0.5;
		crdl: 1.5;
		l: 1;
		cDx: -1.5 dy: 0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:11'!
drawTechnical116
"
TechnicalFontRenderer testCharacter: 116.
"
	self
		m: 1 y: 9;
		d: 7.5;
		cdr: 1.5;
		r: 0.5;
		m: -3 y: 6;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:08'!
drawTechnical117
"
TechnicalFontRenderer testCharacter: 117.
"
	self
		m: 0 y: 6;
		d: 4.5;
		cdr: 1.5;
		r: 2.5;
		u: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:43'!
drawTechnical118
"
TechnicalFontRenderer testCharacter: 118.
"
	self
		m: 0 y: 6;
		li: 2 y: -6;
		li: 2 y: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:40'!
drawTechnical119
"
TechnicalFontRenderer testCharacter: 119.
"
	self
		m: 0 y: 6;
		li: 1.5 y: -6;
		li: 1.5 y: 4;
		li: 1.5 y: -4;
		li: 1.5  y: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:34'!
drawTechnical120
"
TechnicalFontRenderer testCharacter: 120.
"
	self
		m: 0 y: 6;
		li: 4 y: -6;
		m: 0 y: 6;
		li: -4 y: -6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:11'!
drawTechnical121
"
TechnicalFontRenderer testCharacter: 121.
"
	self
		m: 0 y: 6;
		li: 2 y: -6;
		m: 2 y: 6;
		li: -3 y: -9;
		l: 1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:10'!
drawTechnical122
"
TechnicalFontRenderer testCharacter: 122.
"
	self
		m: 0 y: 6;
		r: 4;
		li: -4 y: -6;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:01'!
drawTechnical123
"
TechnicalFontRenderer testCharacter: 123.
"
	self
		m: 0 y: 4;
		cru: 1.5;
		u: 3;
		cur: 1.5;
		m: -3 y: -6;
		crd: 1.5;
		d: 3;
		cdr: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:38'!
drawTechnical124
"
TechnicalFontRenderer testCharacter: 124.
"
	self
		m: 0 y: 10;
		d: 11! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:47'!
drawTechnical125
"
TechnicalFontRenderer testCharacter: 125.
"
	self
		m: 3 y: 4;
		clu: 1.5;
		u: 3;
		cul: 1.5;
		m: 3 y: -6;
		cld: 1.5;
		d: 3;
		cdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:46'!
drawTechnical126
"
TechnicalFontRenderer testCharacter: 126.
"
	self
		m: 0 y: 3;
		curd: 1;
		cdru: 1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:50'!
drawTechnical127
"
TechnicalFontRenderer testCharacter: 127.
"
	self
		m: 0 y: 0;
		u: 6;
		r: 4;
		d: 6;
		l: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 9/30/2014 20:02'!
drawTechnical32! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:56'!
drawTechnical33
"
TechnicalFontRenderer testCharacter: 33.
"
	self
		m: 0 y: 9;
		d: 6;
		m: 0 y: -3;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:22'!
drawTechnical34
"
TechnicalFontRenderer testCharacter: 34.
"
	self
		m: 0 y: 9;
		d: 2;
		m: 2 y: 2;
		d: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:28'!
drawTechnical35
"
TechnicalFontRenderer testCharacter: 35.
"
	self
		m: 0.5 y: 0;
		li: 1.5 y: 9;
		m: 1 y: -9;
		li: 1.5 y: 9;
		m: -4.5 y: -6;
		r: 5;
		m: -5 y: 3;
		r: 5
		! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:27'!
drawTechnical36
"
TechnicalFontRenderer testCharacter: 36.
"
	self
		m: 2 y: 3.5;
		l: 0.5;
		clur: 1.5;
		r: 1;
		cDx: 1.5 dy: -0.5;

		m: -2 y: -2.5;
		r: 0.5;
		crdl: 1.5;
		l: 1;
		cDx: -1.5 dy: 0.5;

		m: 2 y: 8;
		d: 11! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:58'!
drawTechnical37
"
TechnicalFontRenderer testCharacter: 37.
"
	self
		m: 1.5 y: 0;
		li: 5 y: 9;
		m: -5 y: -3.3;
		clur: 1.5;
		crdl: 1.5;
		m: 5 y: -5.4;
		clur: 1.5;
		crdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:05'!
drawTechnical38
"
TechnicalFontRenderer testCharacter: 38.
"
	self
		m: 6 y: 0;
		li: -5 y: 6;
		li: -0.5 y: 1;
		u: 0.5;
		cur: 1.5;
		crd: 1.5;
		d: 0.5;
		li: -3.5 y: -4.5;
		d: 1;
		cdr: 1.5;
		r: 1;
		li: 1.5 y: 1;
		li: 1 y: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:59'!
drawTechnical39
"
TechnicalFontRenderer testCharacter: 39.
"
	self
		m: 0 y: 9;
		d: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:24'!
drawTechnical40
"
TechnicalFontRenderer testCharacter: 40.
"
	self
		m: 2 y: 10;
		cld: 1.5;
		li: -0.5 y: -2.5 ;
		d: 4;
		li: 0.5 y: -2.5;
		cdr: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:27'!
drawTechnical41
"
TechnicalFontRenderer testCharacter: 41.
"
	self
		m: 0 y: 10;
		crd: 1.5;
		li: 0.5 y: -2.5;
		d: 4;
		li: -0.5 y: -2.5;
		cdl: 1.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:23'!
drawTechnical42
"
TechnicalFontRenderer testCharacter: 42.
"
	self
		m: 0 y: 7.5;
		li: 5 y: -3;
		m: -5 y: 0;
		li: 5 y: 3;
		m: -2.5 y: 1.5;
		d: 6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:03'!
drawTechnical43
"
TechnicalFontRenderer testCharacter: 43.
"
	self
		m: 2.5 y: 0;
		u: 6;
		m: -2.5 y: -3;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:03'!
drawTechnical44
"
TechnicalFontRenderer testCharacter: 44.
"
	self
		m: 1.25 y: 0;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		li: -1.25 y: -2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:55'!
drawTechnical45
"
TechnicalFontRenderer testCharacter: 45.
"
	self
		m: 0 y: 3;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:32'!
drawTechnical46
"
TechnicalFontRenderer testCharacter: 46.
"
	self
		m: 0 y: 0;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:36'!
drawTechnical47
"
TechnicalFontRenderer testCharacter: 47.
"
	self
		m: 0 y: -1;
		li: 4 y: 11! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:37'!
drawTechnical48
"
TechnicalFontRenderer testCharacter: 48.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:43'!
drawTechnical49
"
TechnicalFontRenderer testCharacter: 49.
"
	self
		m: 1 y: 7;
		li: 2 y: 2;
		d: 9;
		m: -2 y: 0;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:08'!
drawTechnical50
"
TechnicalFontRenderer testCharacter: 50.
"
	self
		m: 0 y: 7;
		cur: 2;
		r: 1;
		crd: 2;
		d: 1;
		li: -5 y: -6;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:57'!
drawTechnical51
"
TechnicalFontRenderer testCharacter: 51.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:36'!
drawTechnical52
"
TechnicalFontRenderer testCharacter: 52.
"
	self
		m: 2 y: 9;
		li: -2 y: -7;
		r: 5;
		m: -1 y: -2;
		u: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:38'!
drawTechnical53
"
TechnicalFontRenderer testCharacter: 53.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:39'!
drawTechnical54
"
TechnicalFontRenderer testCharacter: 54.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:45'!
drawTechnical55
"
TechnicalFontRenderer testCharacter: 55.
"
	self
		m: 0 y: 9;
		r: 5;
		li: -4 y: -9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:18'!
drawTechnical56
"
TechnicalFontRenderer testCharacter: 56.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:19'!
drawTechnical57
"
TechnicalFontRenderer testCharacter: 57.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:09'!
drawTechnical58
"
TechnicalFontRenderer testCharacter: 58.
"
	self
		m: 0 y: 5;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25;
		m: 0 y: -4;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:54'!
drawTechnical59
"
TechnicalFontRenderer testCharacter: 59.
"
	self
		m: 1.25 y: 4;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25;
		m: 0.25 y: -4;
		l: 0.5;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		li: -1.25 y: -2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:41'!
drawTechnical60
"
TechnicalFontRenderer testCharacter: 60.
"
	self
		m: 4 y: 5;
		li: -4 y: -2.5;
		li: 4 y: -2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:48'!
drawTechnical61
"
TechnicalFontRenderer testCharacter: 61.
"
	self
		m: 0 y: 4;
		r: 4;
		m: -4 y: -3;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:16'!
drawTechnical62
"
TechnicalFontRenderer testCharacter: 62.
"
	self
		m: 0 y: 5;
		li: 4 y: -2.5;
		li: -4 y: -2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:22'!
drawTechnical63
"
TechnicalFontRenderer testCharacter: 63.
"
	self
		m: 0 y: 7;
		curd: 2;
		d: 0.5;
		cdl: 1.5;
		l: 0.5;
		d: 2;
		m: 0 y: -3;
		l: 0.25;
		u: 0.5;
		r: 0.5;
		d: 0.5;
		l: 0.25! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:13'!
drawTechnical64
"
TechnicalFontRenderer testCharacter: 64.
"
	self
		m: 5 y: 5.5;
		crdl: 1.5;
		m: 0 y: 3;
		cldr: 1.5;
		m: 1.5 y: 1.5;
		d: 2;
		r: 0.5;
		cDx: 2 dy: 3;
		cDy: 4 dx: -4;
		cDx:  -5 dy: -5;
		cDy: -5 dx: 5;
		r: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:52'!
drawTechnical65
"
TechnicalFontRenderer testCharacter: 65.
"
	self
		m: 0 y: 0;
		li: 2.5 y: 9;
		li: 2.5 y: -9;
		m: -4.44 y: 2;
		r: 3.88! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:53'!
drawTechnical66
"
TechnicalFontRenderer testCharacter: 66.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:50'!
drawTechnical67
"
TechnicalFontRenderer testCharacter: 67.
"
	self
		m: 5 y: 9;
		l: 3;
		cld: 2;
		d: 5;
		cdr: 2;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:27'!
drawTechnical68
"
TechnicalFontRenderer testCharacter: 68.
"
	self
		m: 0 y: 9;
		r: 2;
		crd: 3;
		d: 3;
		cdl: 3;
		l: 2;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:32'!
drawTechnical69
"
TechnicalFontRenderer testCharacter: 69.
"
	self
		m: 5 y: 0;
		l: 5;
		u: 9;
		r: 5;
		m: -5 y: -4;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:15'!
drawTechnical70
"
TechnicalFontRenderer testCharacter: 70.
"
	self
		m: 0 y: 0;
		u: 9;
		r: 5;
		m: -5 y: -4;
		r: 4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:50'!
drawTechnical71
"
TechnicalFontRenderer testCharacter: 71.
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

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:51'!
drawTechnical72
"
TechnicalFontRenderer testCharacter: 72.
"
	self
		m: 0 y: 0;
		u: 9;
		m: 5 y: -9;
		u: 9;
		m: -5 y: -4;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:52'!
drawTechnical73
"
TechnicalFontRenderer testCharacter: 73.
"
	self
		m: 0.5 y: 0;
		r: 3;
		m: -1.5 y: 0;
		u: 9;
		m: -1.5 y: 0;
		r: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:29'!
drawTechnical74
"
TechnicalFontRenderer testCharacter: 74.
"
	self
		m: 0 y: 0;
		r: 2;
		cru: 2;
		u: 7! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:34'!
drawTechnical75
"
TechnicalFontRenderer testCharacter: 75.
"
	self
		m: 0 y: 9;
		d: 9;
		m: 0 y: 3.5;
		li: 5 y: 5.5;
		m: -3.07 y: -3.4;
		li: 3.07  y: -5.6! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/4/2018 09:25:21'!
drawTechnical76
"
TechnicalFontRenderer testCharacter: 76.
"
	self
		m: 0 y: 9;
		d: 9;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:41'!
drawTechnical77
"
TechnicalFontRenderer testCharacter: 77.
"
	self
		m: 0 y: 0;
		u: 9;
		li: 3 y: -5;
		li: 3 y: 5;
		d: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:14'!
drawTechnical78
"
TechnicalFontRenderer testCharacter: 78.
"
	self
		m: 0 y: 0;
		u: 9;
		li: 5 y: -9;
		u: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:15'!
drawTechnical79
"
TechnicalFontRenderer testCharacter: 79.
"
	self
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:24'!
drawTechnical80
"
TechnicalFontRenderer testCharacter: 80.
"
	self
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:30'!
drawTechnical81
"
TechnicalFontRenderer testCharacter: 81.
"
	self
		m: 3 y: 9;
		crd: 3;
		d: 3;
		cdl: 3;
		clu: 3;
		u: 3;
		cur: 3;
		m: 0 y: -7;
		li: 3 y: -3! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:33'!
drawTechnical82
"
TechnicalFontRenderer testCharacter: 82.
"
	self
		m: 0 y: 0;
		u: 9;
		r: 3.5;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 3.5;
		m: 3 y: 0;
		li: 2 y: -4! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:36'!
drawTechnical83
"
TechnicalFontRenderer testCharacter: 83.
"
	self
		m: 2.5 y: 5;
		l: 1;
		clu: 1.5;
		u: 1;
		cur: 1.5;
		r: 2;
		cDx: 1.5 dy: -0.5;

		m: -2.5 y: -3.5;
		r: 1;
		crd: 1.5;
		d: 2;
		cdl: 1.5;
		l: 2;
		cDx: -1.5 dy: 0.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:08'!
drawTechnical84
"
TechnicalFontRenderer testCharacter: 84.
"
	self
		m: -0.5 y: 9;
		r: 6;
		m: -3 y: 0;
		d: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:17'!
drawTechnical85
"
TechnicalFontRenderer testCharacter: 85.
"
	self
		m: 0 y: 9;
		d: 7;
		cdr: 2;
		r: 1;
		cru: 2;
		u: 7! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:05'!
drawTechnical86
"
TechnicalFontRenderer testCharacter: 86.
"
	self
		m: 0 y: 9;
		li: 2.5 y: -9;
		li: 2.5 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:45'!
drawTechnical87
"
TechnicalFontRenderer testCharacter: 87.
"
	self
		m: 0 y: 9;
		li: 2 y: -9;
		li: 2 y: 6;
		li: 2 y: -6;
		li: 2 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:45'!
drawTechnical88
"
TechnicalFontRenderer testCharacter: 88.
"
	self
		m: 0 y: 9;
		li: 5 y: -9;
		m: -5 y: 0;
		li: 5 y: 9! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:43'!
drawTechnical89
"
TechnicalFontRenderer testCharacter: 89.
"
	self
		m: 0 y: 9;
		li: 2.5 y: -4.5;
		li: 2.5 y: 4.5;
		m: -2.5 y: -4.5;
		d: 4.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:10'!
drawTechnical90
"
TechnicalFontRenderer testCharacter: 90.
"
	self
		m: 0 y: 9;
		r: 5;
		li: -5 y: -9;
		r: 5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:13'!
drawTechnical91
"
TechnicalFontRenderer testCharacter: 91.
"
	self
		m: 2 y: 10;
		l: 2;
		d: 12;
		r: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:06'!
drawTechnical92
"
TechnicalFontRenderer testCharacter: 92.
"
	self
		m: 0 y: 10;
		li: 4  y: -11! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:17:41'!
drawTechnical93
"
TechnicalFontRenderer testCharacter: 93.
"
	self
		m: 0 y: 10;
		r: 2;
		d: 12;
		l: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:42'!
drawTechnical94
"
TechnicalFontRenderer testCharacter: 94.
"
	self
		m: 2 y: 0;
		u: 9;
		m: -2 y: -2;
		li: 2 y: 2;
		li: 2 y: -2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:44'!
drawTechnical95
"
TechnicalFontRenderer testCharacter: 95.
"
	self
		m: 0 y: 3;
		r: 8;
		m: -6 y: -2;
		li: -2 y: 2;
		li: 2 y: 2! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:52'!
drawTechnical96
"
TechnicalFontRenderer testCharacter: 96.
"
	self
		m: 0 y: 9;
		li: 1 y: -1! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:18:20'!
drawTechnical97
"
TechnicalFontRenderer testCharacter: 97.
"
	self
		m: 0.5 y: 6;
		r: 2;
		crd: 1.5;
		d: 4.5;
		l: 2.5; 
		clur: 1.5;
		r: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:19:24'!
drawTechnical98
"
TechnicalFontRenderer testCharacter: 98.
"
	self
		m: 0 y: 9;
		d: 9;
		r: 2.5;
		cru: 1.5;
		u: 3;
		cul: 1.5;
		l: 2.5! !

!TechnicalFontRenderer methodsFor: 'glyphs' stamp: 'jmv 1/3/2018 10:20:17'!
drawTechnical99
"
TechnicalFontRenderer testCharacter: 99.
"
	self
		m: 3 y: 6;
		l: 1.5;
		cld: 1.5;
		d: 3;
		cdr: 1.5;
		r: 1.5! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:12:08'!
cDx: dx dy: dy
	"Curve comes from an horizontal segment, and is followed by a vertical segment"

	vectorGraphicsEngine
		rel_CubicBezierToX: dx y: dy
		control1X: dx*0.5 y: 0
		control2X: dx y: dy*0.5! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:12'!
cDy: dy dx: dx
	"Curve comes from a vertical segment, and is followed by an horizontal segment"

	vectorGraphicsEngine
		rel_CubicBezierToX: dx y: dy
		control1X: 0 y: dy*0.5
		control2X: dx*0.5 y: dy! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:15'!
cdl: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0-r y: 0-r
		control1X: 0 y: 0-hr
		control2X: 0-hr y: 0-r! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:18'!
cdr: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: r y: 0-r
		control1X: 0 y: 0-hr
		control2X: hr y: 0-r! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:20'!
cdru: r

	| dr cr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: dr y: 0
		control1X: 0 y: 0-cr
		control2X: dr y: 0-cr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:23'!
cld: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0-r y: 0-r
		control1X: 0-hr y: 0
		control2X: 0-r y: 0-hr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:26'!
cldr: r

	|  cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0 y: 0-dr
		control1X: 0-cr y: 0
		control2X: 0-cr y: 0-dr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:29'!
clu: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0-r y: r
		control1X: 0-hr y: 0
		control2X: 0-r y: hr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:33'!
clur: r

	| cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0 y: dr
		control1X: 0-cr y: 0
		control2X: 0-cr y: dr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:36'!
crd: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: r y: 0-r
		control1X: hr y: 0
		control2X: r y: 0-hr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:44'!
crdl: r

	| cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0 y: 0-dr
		control1X: cr y: 0
		control2X: cr y: 0-dr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:49'!
cru: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: r y: r
		control1X: hr y: 0
		control2X: r y: hr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:13:52'!
cul: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: 0-r y: r
		control1X: 0 y: hr
		control2X: 0-hr y: r! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:14:26'!
cur: r

	| hr |
	hr _ 0.5 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: r y: r
		control1X: 0 y: hr
		control2X: hr y: r! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:14:41'!
curd: r

	| cr dr |
	dr _ 2 * r.
	cr _ 1.35 * r.
	vectorGraphicsEngine
		rel_CubicBezierToX: dr y: 0
		control1X: 0 y: cr
		control2X: dr y: cr! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:04'!
d: dy

	vectorGraphicsEngine rel_LineToX: 0 y: 0-dy! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:10'!
l: dx

	vectorGraphicsEngine rel_LineToX: 0-dx y: 0! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:19'!
li: x y: y
	"no confundir con l: !!!!!!!!"

	vectorGraphicsEngine rel_LineToX: x y: y! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:26'!
m: x y: y

	vectorGraphicsEngine rel_MoveToX: x y: y! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:35'!
r: dx

	vectorGraphicsEngine rel_LineToX: dx y: 0! !

!TechnicalFontRenderer methodsFor: 'paths - strike font elements' stamp: 'jmv 1/3/2018 10:15:42'!
u: dy

	vectorGraphicsEngine rel_LineToX: 0 y: dy! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical100
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: -0.600;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: 0.000 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical101
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.300;
		rel_LineToX: 0.400 y: 0.000;
		rel_LineToX: 0.000 y: -0.150;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: -0.075 y: -0.150;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical102
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: 0.000;
		rel_LineToX: 0.000 y: -0.750;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_LineToX: 0.050 y: 0.000;
		rel_MoveToX: -0.300 y: 0.300;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical103
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: 0.000;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: 0.000 y: 0.750;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical104
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_MoveToX: 0.000 y: 0.300;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical105
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.600;
		rel_MoveToX: 0.000 y: -0.250;
		rel_LineToX: -0.015 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.030 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.015 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical106
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.600;
		rel_LineToX: 0.000 y: 0.750;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.050 y: 0.000;
		rel_MoveToX: 0.200 y: -1.150;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical107
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.900;
		rel_MoveToX: 0.000 y: -0.300;
		rel_LineToX: 0.400 y: -0.300;
		rel_MoveToX: -0.260 y: 0.195;
		rel_LineToX: 0.260 y: 0.405! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical108
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.750;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.050 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical109
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.600;
		rel_LineToX: 0.450 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.450;
		rel_MoveToX: -0.300 y: -0.600;
		rel_LineToX: 0.000 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical110
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.600;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical111
	vectorGraphicsEngine
		rel_MoveToX: 0.150 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical112
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.300;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical113
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: 0.300;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical114
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.600;
		rel_LineToX: 0.150 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical115
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -0.300;
		rel_LineToX: -0.050 y: 0.000;
		rel_CubicBezierToX: 0.000 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: -0.300;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.025;
		rel_MoveToX: -0.200 y: 0.250;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.203 y: 0.300;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical116
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.900;
		rel_LineToX: 0.000 y: 0.750;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.050 y: 0.000;
		rel_MoveToX: -0.300 y: -0.600;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical117
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.000 y: 0.450;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: 0.000 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical118
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.200 y: 0.600;
		rel_LineToX: 0.200 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical119
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.150 y: 0.600;
		rel_LineToX: 0.150 y: -0.400;
		rel_LineToX: 0.150 y: 0.400;
		rel_LineToX: 0.150 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical120
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.400 y: 0.600;
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: -0.400 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical121
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.200 y: 0.600;
		rel_MoveToX: 0.200 y: -0.600;
		rel_LineToX: -0.300 y: 0.900;
		rel_LineToX: -0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical122
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.600;
		rel_LineToX: 0.400 y: 0.000;
		rel_LineToX: -0.400 y: 0.600;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical123
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.400;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_MoveToX: -0.300 y: 0.600;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical124
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -1.000;
		rel_LineToX: 0.000 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical125
	vectorGraphicsEngine
		rel_MoveToX: 0.300 y: -0.400;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: -0.075 y: -0.150;
		rel_MoveToX: 0.300 y: 0.600;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical126
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.200 y: 0.000 control1X: 0.000 y: -0.135 control2X: 0.200 y: -0.135;
		rel_CubicBezierToX: 0.200 y: 0.000 control1X: 0.000 y: 0.135 control2X: 0.200 y: 0.135! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical33
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.600;
		rel_MoveToX: 0.000 y: 0.300;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical34
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.200;
		rel_MoveToX: 0.200 y: -0.200;
		rel_LineToX: 0.000 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical35
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.000;
		rel_LineToX: 0.150 y: -0.900;
		rel_MoveToX: 0.100 y: 0.900;
		rel_LineToX: 0.150 y: -0.900;
		rel_MoveToX: -0.450 y: 0.600;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.500 y: -0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical36
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -0.350;
		rel_LineToX: -0.050 y: 0.000;
		rel_CubicBezierToX: 0.000 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: -0.300;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.025;
		rel_MoveToX: -0.200 y: 0.250;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.203 y: 0.300;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.025;
		rel_MoveToX: 0.200 y: -0.800;
		rel_LineToX: 0.000 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical37
	vectorGraphicsEngine
		rel_MoveToX: 0.150 y: 0.000;
		rel_LineToX: 0.500 y: -0.900;
		rel_MoveToX: -0.500 y: 0.330;
		rel_CubicBezierToX: 0.000 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: -0.300;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.203 y: 0.300;
		rel_MoveToX: 0.500 y: 0.540;
		rel_CubicBezierToX: 0.000 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: -0.300;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.203 y: 0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical38
	vectorGraphicsEngine
		rel_MoveToX: 0.600 y: 0.000;
		rel_LineToX: -0.500 y: -0.600;
		rel_LineToX: -0.050 y: -0.100;
		rel_LineToX: 0.000 y: -0.050;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.350 y: 0.450;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: 0.150 y: -0.100;
		rel_LineToX: 0.100 y: -0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical39
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical40
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: -0.050 y: 0.250;
		rel_LineToX: 0.000 y: 0.400;
		rel_LineToX: 0.050 y: 0.250;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical41
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -1.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.050 y: 0.250;
		rel_LineToX: 0.000 y: 0.400;
		rel_LineToX: -0.050 y: 0.250;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical42
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.750;
		rel_LineToX: 0.500 y: 0.300;
		rel_MoveToX: -0.500 y: 0.000;
		rel_LineToX: 0.500 y: -0.300;
		rel_MoveToX: -0.250 y: -0.150;
		rel_LineToX: 0.000 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical43
	vectorGraphicsEngine
		rel_MoveToX: 0.250 y: 0.000;
		rel_LineToX: 0.000 y: -0.600;
		rel_MoveToX: -0.250 y: 0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:42'!
draw_Technical44
	vectorGraphicsEngine
		rel_MoveToX: 0.125 y: 0.000;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.125 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical45
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical46
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical47
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.100;
		rel_LineToX: 0.400 y: -1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical48
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.700;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.000 y: -0.100 control2X: 0.100 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.500;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.200 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.500! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical49
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.700;
		rel_LineToX: 0.200 y: -0.200;
		rel_LineToX: 0.000 y: 0.900;
		rel_MoveToX: -0.200 y: 0.000;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical50
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.700;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.000 y: -0.100 control2X: 0.100 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_LineToX: -0.500 y: 0.600;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical51
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical52
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -0.900;
		rel_LineToX: -0.200 y: 0.700;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.100 y: 0.200;
		rel_LineToX: 0.000 y: -0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical53
	vectorGraphicsEngine
		rel_MoveToX: 0.430 y: -0.900;
		rel_LineToX: -0.430 y: 0.000;
		rel_LineToX: 0.000 y: 0.400;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.150 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical54
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.500;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.200 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.400;
		rel_CubicBezierToX: 0.300 y: -0.300 control1X: 0.000 y: -0.150 control2X: 0.150 y: -0.300;
		rel_LineToX: 0.150 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical55
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_LineToX: -0.400 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical56
	vectorGraphicsEngine
		rel_MoveToX: 0.300 y: -0.900;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.200 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.100;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.000 y: -0.100 control2X: 0.100 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.200 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: -0.100;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.000 y: -0.100 control2X: 0.100 y: -0.200;
		rel_LineToX: 0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical57
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: -0.400;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.200 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.100;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.000 y: -0.100 control2X: 0.100 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.400;
		rel_CubicBezierToX: -0.300 y: 0.300 control1X: 0.000 y: 0.150 control2X: -0.150 y: 0.300;
		rel_LineToX: -0.150 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical58
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.500;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000;
		rel_MoveToX: 0.000 y: 0.400;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical59
	vectorGraphicsEngine
		rel_MoveToX: 0.125 y: -0.400;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000;
		rel_MoveToX: 0.025 y: 0.400;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.125 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical60
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: -0.500;
		rel_LineToX: -0.400 y: 0.250;
		rel_LineToX: 0.400 y: 0.250! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical61
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.400;
		rel_LineToX: 0.400 y: 0.000;
		rel_MoveToX: -0.400 y: 0.300;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical62
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.500;
		rel_LineToX: 0.400 y: 0.250;
		rel_LineToX: -0.400 y: 0.250! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical63
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.700;
		rel_CubicBezierToX: 0.400 y: 0.000 control1X: 0.000 y: -0.270 control2X: 0.400 y: -0.270;
		rel_LineToX: 0.000 y: 0.050;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.200;
		rel_MoveToX: 0.000 y: 0.300;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.000 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: 0.000 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical64
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: -0.550;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.203 y: 0.300;
		rel_MoveToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.000 y: 0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: 0.300;
		rel_MoveToX: 0.150 y: -0.150;
		rel_LineToX: 0.000 y: 0.200;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: 0.200 y: -0.300 control1X: 0.100 y: 0.000 control2X: 0.200 y: -0.150;
		rel_CubicBezierToX: -0.400 y: -0.400 control1X: 0.000 y: -0.200 control2X: -0.200 y: -0.400;
		rel_CubicBezierToX: -0.500 y: 0.500 control1X: -0.250 y: 0.000 control2X: -0.500 y: 0.250;
		rel_CubicBezierToX: 0.500 y: 0.500 control1X: 0.000 y: 0.250 control2X: 0.250 y: 0.500;
		rel_LineToX: 0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical65
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.250 y: -0.900;
		rel_LineToX: 0.250 y: 0.900;
		rel_MoveToX: -0.444 y: -0.200;
		rel_LineToX: 0.388 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical66
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.250 y: 0.000;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.100;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: -0.100 y: 0.200;
		rel_LineToX: -0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical67
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: -0.900;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.500;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: 0.100 y: 0.200;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical68
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.300 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.300 y: 0.150;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.300 y: 0.300 control1X: 0.000 y: 0.150 control2X: -0.150 y: 0.300;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: 0.000 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical69
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: 0.000;
		rel_LineToX: -0.500 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.500 y: 0.400;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical70
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.500 y: 0.400;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical71
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: -0.900;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.200 y: 0.200 control1X: -0.100 y: 0.000 control2X: -0.200 y: 0.100;
		rel_LineToX: 0.000 y: 0.500;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: 0.100 y: 0.200;
		rel_LineToX: 0.300 y: 0.000;
		rel_LineToX: 0.000 y: -0.500;
		rel_LineToX: -0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical72
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_MoveToX: 0.500 y: 0.900;
		rel_LineToX: 0.000 y: -0.900;
		rel_MoveToX: -0.500 y: 0.400;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical73
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.000;
		rel_LineToX: 0.300 y: 0.000;
		rel_MoveToX: -0.150 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_MoveToX: -0.150 y: 0.000;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical74
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.700! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical75
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.900;
		rel_MoveToX: 0.000 y: -0.350;
		rel_LineToX: 0.500 y: -0.550;
		rel_MoveToX: -0.307 y: 0.340;
		rel_LineToX: 0.307 y: 0.560! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical76
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.900;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical77
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.300 y: 0.500;
		rel_LineToX: 0.300 y: -0.500;
		rel_LineToX: 0.000 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical78
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.900;
		rel_LineToX: 0.000 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical79
	vectorGraphicsEngine
		rel_MoveToX: 0.300 y: -0.900;
		rel_CubicBezierToX: 0.300 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.300 y: 0.150;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.300 y: 0.300 control1X: 0.000 y: 0.150 control2X: -0.150 y: 0.300;
		rel_CubicBezierToX: -0.300 y: -0.300 control1X: -0.150 y: 0.000 control2X: -0.300 y: -0.150;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.300 y: -0.300 control1X: 0.000 y: -0.150 control2X: 0.150 y: -0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical80
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.350 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.200;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.350 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical81
	vectorGraphicsEngine
		rel_MoveToX: 0.300 y: -0.900;
		rel_CubicBezierToX: 0.300 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.300 y: 0.150;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: -0.300 y: 0.300 control1X: 0.000 y: 0.150 control2X: -0.150 y: 0.300;
		rel_CubicBezierToX: -0.300 y: -0.300 control1X: -0.150 y: 0.000 control2X: -0.300 y: -0.150;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: 0.300 y: -0.300 control1X: 0.000 y: -0.150 control2X: 0.150 y: -0.300;
		rel_MoveToX: 0.000 y: 0.700;
		rel_LineToX: 0.300 y: 0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical82
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_LineToX: 0.350 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.200;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.350 y: 0.000;
		rel_MoveToX: 0.300 y: 0.000;
		rel_LineToX: 0.200 y: 0.400! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical83
	vectorGraphicsEngine
		rel_MoveToX: 0.250 y: -0.500;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.100;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: 0.075 y: -0.150;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.025;
		rel_MoveToX: -0.250 y: 0.350;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.200;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: -0.075 y: 0.150;
		rel_LineToX: -0.200 y: 0.000;
		rel_CubicBezierToX: -0.150 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.150 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical84
	vectorGraphicsEngine
		rel_MoveToX: -0.050 y: -0.900;
		rel_LineToX: 0.600 y: 0.000;
		rel_MoveToX: -0.300 y: 0.000;
		rel_LineToX: 0.000 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical85
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.700;
		rel_CubicBezierToX: 0.200 y: 0.200 control1X: 0.000 y: 0.100 control2X: 0.100 y: 0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.200 y: -0.200 control1X: 0.100 y: 0.000 control2X: 0.200 y: -0.100;
		rel_LineToX: 0.000 y: -0.700! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical86
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.250 y: 0.900;
		rel_LineToX: 0.250 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical87
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.200 y: 0.900;
		rel_LineToX: 0.200 y: -0.600;
		rel_LineToX: 0.200 y: 0.600;
		rel_LineToX: 0.200 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical88
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.900;
		rel_MoveToX: -0.500 y: 0.000;
		rel_LineToX: 0.500 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical89
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.250 y: 0.450;
		rel_LineToX: 0.250 y: -0.450;
		rel_MoveToX: -0.250 y: 0.450;
		rel_LineToX: 0.000 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical90
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_LineToX: -0.500 y: 0.900;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical91
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: 0.000 y: 1.200;
		rel_LineToX: 0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical92
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -1.000;
		rel_LineToX: 0.400 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical93
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -1.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_LineToX: 0.000 y: 1.200;
		rel_LineToX: -0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical94
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: 0.000;
		rel_LineToX: 0.000 y: -0.900;
		rel_MoveToX: -0.200 y: 0.200;
		rel_LineToX: 0.200 y: -0.200;
		rel_LineToX: 0.200 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical95
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.300;
		rel_LineToX: 0.800 y: 0.000;
		rel_MoveToX: -0.600 y: 0.200;
		rel_LineToX: -0.200 y: -0.200;
		rel_LineToX: 0.200 y: -0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical96
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.100 y: 0.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical97
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: -0.600;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.450;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: 0.000 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.203 y: -0.300;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical98
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: -0.900;
		rel_LineToX: 0.000 y: 0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.150 y: -0.150 control1X: 0.075 y: 0.000 control2X: 0.150 y: -0.075;
		rel_LineToX: 0.000 y: -0.300;
		rel_CubicBezierToX: -0.150 y: -0.150 control1X: 0.000 y: -0.075 control2X: -0.075 y: -0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - xx' stamp: 'jmv 1/4/2018 11:25:43'!
draw_Technical99
	vectorGraphicsEngine
		rel_MoveToX: 0.300 y: -0.600;
		rel_LineToX: -0.150 y: 0.000;
		rel_CubicBezierToX: -0.150 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.150 y: 0.075;
		rel_LineToX: 0.000 y: 0.300;
		rel_CubicBezierToX: 0.150 y: 0.150 control1X: 0.000 y: 0.075 control2X: 0.075 y: 0.150;
		rel_LineToX: 0.150 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics100
	vectorGraphicsEngine
		rel_MoveToX: 0.520 y: -0.600;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: 0.180 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics101
	vectorGraphicsEngine
		rel_MoveToX: 0.060 y: -0.300;
		rel_LineToX: 0.400 y: 0.000;
		rel_LineToX: 0.030 y: -0.150;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: 0.015 y: -0.075 control2X: -0.045 y: -0.150;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics102
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: 0.000;
		rel_LineToX: 0.150 y: -0.750;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_LineToX: 0.050 y: 0.000;
		rel_MoveToX: -0.360 y: 0.300;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics103
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: 0.000;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.135 y: -0.075;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: -0.150 y: 0.750;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics104
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_MoveToX: -0.060 y: 0.300;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.090 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics105
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.120 y: -0.600;
		rel_MoveToX: 0.050 y: -0.250;
		rel_LineToX: -0.015 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.030 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.015 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics106
	vectorGraphicsEngine
		rel_MoveToX: 0.220 y: -0.600;
		rel_LineToX: -0.150 y: 0.750;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.050 y: 0.000;
		rel_MoveToX: 0.430 y: -1.150;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics107
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.180 y: 0.900;
		rel_MoveToX: 0.060 y: -0.300;
		rel_LineToX: 0.460 y: -0.300;
		rel_MoveToX: -0.299 y: 0.195;
		rel_LineToX: 0.179 y: 0.405! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics108
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.150 y: 0.750;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.050 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics109
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.120 y: -0.600;
		rel_LineToX: 0.450 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.090 y: 0.450;
		rel_MoveToX: -0.180 y: -0.600;
		rel_LineToX: -0.120 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics110
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.120 y: -0.600;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.090 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics111
	vectorGraphicsEngine
		rel_MoveToX: 0.150 y: 0.000;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.135 y: -0.075;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics112
	vectorGraphicsEngine
		rel_MoveToX: -0.060 y: 0.300;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics113
	vectorGraphicsEngine
		rel_MoveToX: 0.340 y: 0.300;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics114
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.120 y: -0.600;
		rel_LineToX: 0.150 y: 0.000;
		rel_CubicBezierToX: 0.140 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.145 y: 0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics115
	vectorGraphicsEngine
		rel_MoveToX: 0.260 y: -0.300;
		rel_LineToX: -0.050 y: 0.000;
		rel_CubicBezierToX: 0.060 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.143 y: -0.300;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.140 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.145 y: 0.025;
		rel_MoveToX: -0.250 y: 0.250;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.143 y: 0.300;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.140 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.145 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics116
	vectorGraphicsEngine
		rel_MoveToX: 0.280 y: -0.900;
		rel_LineToX: -0.150 y: 0.750;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.050 y: 0.000;
		rel_MoveToX: -0.180 y: -0.600;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics117
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: -0.090 y: 0.450;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.250 y: 0.000;
		rel_LineToX: 0.120 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics118
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: 0.080 y: 0.600;
		rel_LineToX: 0.320 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics119
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: 0.030 y: 0.600;
		rel_LineToX: 0.230 y: -0.400;
		rel_LineToX: 0.070 y: 0.400;
		rel_LineToX: 0.270 y: -0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics120
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: 0.280 y: 0.600;
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: -0.520 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics121
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: 0.080 y: 0.600;
		rel_MoveToX: 0.320 y: -0.600;
		rel_LineToX: -0.480 y: 0.900;
		rel_LineToX: -0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics122
	vectorGraphicsEngine
		rel_MoveToX: 0.120 y: -0.600;
		rel_LineToX: 0.400 y: 0.000;
		rel_LineToX: -0.520 y: 0.600;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics123
	vectorGraphicsEngine
		rel_MoveToX: 0.080 y: -0.400;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.075 y: 0.000 control2X: 0.165 y: -0.075;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_MoveToX: -0.420 y: 0.600;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics124
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_LineToX: -0.220 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics125
	vectorGraphicsEngine
		rel_MoveToX: 0.380 y: -0.400;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.135 y: -0.075;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: 0.015 y: -0.075 control2X: -0.045 y: -0.150;
		rel_MoveToX: 0.180 y: 0.600;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics126
	vectorGraphicsEngine
		rel_MoveToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.200 y: 0.000 control1X: 0.027 y: -0.135 control2X: 0.227 y: -0.135;
		rel_CubicBezierToX: 0.200 y: 0.000 control1X: -0.027 y: 0.135 control2X: 0.173 y: 0.135! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics33
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.120 y: 0.600;
		rel_MoveToX: -0.060 y: 0.300;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics34
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.040 y: 0.200;
		rel_MoveToX: 0.240 y: -0.200;
		rel_LineToX: -0.040 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics35
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.000;
		rel_LineToX: 0.330 y: -0.900;
		rel_MoveToX: -0.080 y: 0.900;
		rel_LineToX: 0.330 y: -0.900;
		rel_MoveToX: -0.570 y: 0.600;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.440 y: -0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics36
	vectorGraphicsEngine
		rel_MoveToX: 0.270 y: -0.350;
		rel_LineToX: -0.050 y: 0.000;
		rel_CubicBezierToX: 0.060 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.143 y: -0.300;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.140 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.145 y: 0.025;
		rel_MoveToX: -0.250 y: 0.250;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.143 y: 0.300;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.140 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.145 y: -0.025;
		rel_MoveToX: 0.360 y: -0.800;
		rel_LineToX: -0.220 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics37
	vectorGraphicsEngine
		rel_MoveToX: 0.150 y: 0.000;
		rel_LineToX: 0.680 y: -0.900;
		rel_MoveToX: -0.566 y: 0.330;
		rel_CubicBezierToX: 0.060 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.143 y: -0.300;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.143 y: 0.300;
		rel_MoveToX: 0.392 y: 0.540;
		rel_CubicBezierToX: 0.060 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.143 y: -0.300;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.143 y: 0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics38
	vectorGraphicsEngine
		rel_MoveToX: 0.600 y: 0.000;
		rel_LineToX: -0.380 y: -0.600;
		rel_LineToX: -0.030 y: -0.100;
		rel_LineToX: 0.010 y: -0.050;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.440 y: 0.450;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: 0.170 y: -0.100;
		rel_LineToX: 0.140 y: -0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics39
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.040 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics40
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: -1.000;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.100 y: 0.250;
		rel_LineToX: -0.080 y: 0.400;
		rel_LineToX: 0.000 y: 0.250;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics41
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: 0.000 y: 0.250;
		rel_LineToX: -0.080 y: 0.400;
		rel_LineToX: -0.100 y: 0.250;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics42
	vectorGraphicsEngine
		rel_MoveToX: 0.150 y: -0.750;
		rel_LineToX: 0.440 y: 0.300;
		rel_MoveToX: -0.500 y: 0.000;
		rel_LineToX: 0.560 y: -0.300;
		rel_MoveToX: -0.220 y: -0.150;
		rel_LineToX: -0.120 y: 0.600! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics43
	vectorGraphicsEngine
		rel_MoveToX: 0.250 y: 0.000;
		rel_LineToX: 0.120 y: -0.600;
		rel_MoveToX: -0.310 y: 0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics44
	vectorGraphicsEngine
		rel_MoveToX: 0.125 y: 0.000;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.165 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics45
	vectorGraphicsEngine
		rel_MoveToX: 0.060 y: -0.300;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics46
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics47
	vectorGraphicsEngine
		rel_MoveToX: -0.020 y: 0.100;
		rel_LineToX: 0.620 y: -1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics48
	vectorGraphicsEngine
		rel_MoveToX: 0.140 y: -0.700;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.020 y: -0.100 control2X: 0.140 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.100 y: 0.500;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.160 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.180 y: -0.100;
		rel_LineToX: 0.100 y: -0.500! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics49
	vectorGraphicsEngine
		rel_MoveToX: 0.240 y: -0.700;
		rel_LineToX: 0.240 y: -0.200;
		rel_LineToX: -0.180 y: 0.900;
		rel_MoveToX: -0.200 y: 0.000;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics50
	vectorGraphicsEngine
		rel_MoveToX: 0.140 y: -0.700;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.020 y: -0.100 control2X: 0.140 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_LineToX: -0.620 y: 0.600;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics51
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics52
	vectorGraphicsEngine
		rel_MoveToX: 0.380 y: -0.900;
		rel_LineToX: -0.340 y: 0.700;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.140 y: 0.200;
		rel_LineToX: 0.090 y: -0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics53
	vectorGraphicsEngine
		rel_MoveToX: 0.610 y: -0.900;
		rel_LineToX: -0.430 y: 0.000;
		rel_LineToX: -0.080 y: 0.400;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.150 y: 0.000;
		rel_CubicBezierToX: -0.140 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.145 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics54
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.500;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.160 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.180 y: -0.100;
		rel_LineToX: 0.080 y: -0.400;
		rel_CubicBezierToX: 0.360 y: -0.300 control1X: 0.030 y: -0.150 control2X: 0.210 y: -0.300;
		rel_LineToX: 0.150 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics55
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_LineToX: -0.580 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics56
	vectorGraphicsEngine
		rel_MoveToX: 0.480 y: -0.900;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.160 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.180 y: -0.100;
		rel_LineToX: 0.020 y: -0.100;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.020 y: -0.100 control2X: 0.140 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.160 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.180 y: -0.100;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.020 y: -0.100 control2X: 0.140 y: -0.200;
		rel_LineToX: 0.100 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics57
	vectorGraphicsEngine
		rel_MoveToX: 0.580 y: -0.400;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.160 y: -0.200 control1X: -0.100 y: 0.000 control2X: -0.180 y: -0.100;
		rel_LineToX: 0.020 y: -0.100;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.020 y: -0.100 control2X: 0.140 y: -0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.080 y: 0.400;
		rel_CubicBezierToX: -0.360 y: 0.300 control1X: -0.030 y: 0.150 control2X: -0.210 y: 0.300;
		rel_LineToX: -0.150 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics58
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.500;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000;
		rel_MoveToX: -0.080 y: 0.400;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics59
	vectorGraphicsEngine
		rel_MoveToX: 0.205 y: -0.400;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000;
		rel_MoveToX: -0.055 y: 0.400;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.165 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics60
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: -0.500;
		rel_LineToX: -0.450 y: 0.250;
		rel_LineToX: 0.350 y: 0.250! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics61
	vectorGraphicsEngine
		rel_MoveToX: 0.080 y: -0.400;
		rel_LineToX: 0.400 y: 0.000;
		rel_MoveToX: -0.460 y: 0.300;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics62
	vectorGraphicsEngine
		rel_MoveToX: 0.100 y: -0.500;
		rel_LineToX: 0.350 y: 0.250;
		rel_LineToX: -0.450 y: 0.250! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics63
	vectorGraphicsEngine
		rel_MoveToX: 0.140 y: -0.700;
		rel_CubicBezierToX: 0.400 y: 0.000 control1X: 0.054 y: -0.270 control2X: 0.454 y: -0.270;
		rel_LineToX: -0.010 y: 0.050;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.050 y: 0.000;
		rel_LineToX: -0.040 y: 0.200;
		rel_MoveToX: -0.060 y: 0.300;
		rel_LineToX: -0.025 y: 0.000;
		rel_LineToX: 0.010 y: -0.050;
		rel_LineToX: 0.050 y: 0.000;
		rel_LineToX: -0.010 y: 0.050;
		rel_LineToX: -0.025 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics64
	vectorGraphicsEngine
		rel_MoveToX: 0.610 y: -0.550;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: 0.203 y: 0.000 control2X: 0.143 y: 0.300;
		rel_MoveToX: 0.060 y: -0.300;
		rel_CubicBezierToX: -0.060 y: 0.300 control1X: -0.203 y: 0.000 control2X: -0.263 y: 0.300;
		rel_MoveToX: 0.180 y: -0.150;
		rel_LineToX: -0.040 y: 0.200;
		rel_LineToX: 0.050 y: 0.000;
		rel_CubicBezierToX: 0.260 y: -0.300 control1X: 0.100 y: 0.000 control2X: 0.230 y: -0.150;
		rel_CubicBezierToX: -0.320 y: -0.400 control1X: 0.040 y: -0.200 control2X: -0.120 y: -0.400;
		rel_CubicBezierToX: -0.600 y: 0.500 control1X: -0.250 y: 0.000 control2X: -0.550 y: 0.250;
		rel_CubicBezierToX: 0.400 y: 0.500 control1X: -0.050 y: 0.250 control2X: 0.150 y: 0.500;
		rel_LineToX: 0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics65
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.430 y: -0.900;
		rel_LineToX: 0.070 y: 0.900;
		rel_MoveToX: -0.404 y: -0.200;
		rel_LineToX: 0.388 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics66
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.250 y: 0.000;
		rel_LineToX: 0.300 y: 0.000;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: 0.100 y: 0.000 control2X: 0.180 y: 0.100;
		rel_LineToX: -0.020 y: 0.100;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.020 y: 0.100 control2X: -0.140 y: 0.200;
		rel_LineToX: -0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics67
	vectorGraphicsEngine
		rel_MoveToX: 0.680 y: -0.900;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.100 y: 0.000 control2X: -0.220 y: 0.100;
		rel_LineToX: -0.100 y: 0.500;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: -0.020 y: 0.100 control2X: 0.060 y: 0.200;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics68
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.240 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.270 y: 0.150;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.360 y: 0.300 control1X: -0.030 y: 0.150 control2X: -0.210 y: 0.300;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: 0.180 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics69
	vectorGraphicsEngine
		rel_MoveToX: 0.500 y: 0.000;
		rel_LineToX: -0.500 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.580 y: 0.400;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics70
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_MoveToX: -0.580 y: 0.400;
		rel_LineToX: 0.400 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics71
	vectorGraphicsEngine
		rel_MoveToX: 0.680 y: -0.900;
		rel_LineToX: -0.300 y: 0.000;
		rel_CubicBezierToX: -0.240 y: 0.200 control1X: -0.100 y: 0.000 control2X: -0.220 y: 0.100;
		rel_LineToX: -0.100 y: 0.500;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: -0.020 y: 0.100 control2X: 0.060 y: 0.200;
		rel_LineToX: 0.300 y: 0.000;
		rel_LineToX: 0.100 y: -0.500;
		rel_LineToX: -0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics72
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_MoveToX: 0.320 y: 0.900;
		rel_LineToX: 0.180 y: -0.900;
		rel_MoveToX: -0.580 y: 0.400;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics73
	vectorGraphicsEngine
		rel_MoveToX: 0.050 y: 0.000;
		rel_LineToX: 0.300 y: 0.000;
		rel_MoveToX: -0.150 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_MoveToX: -0.150 y: 0.000;
		rel_LineToX: 0.300 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics74
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.100 y: 0.000 control2X: 0.220 y: -0.100;
		rel_LineToX: 0.140 y: -0.700! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics75
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.180 y: 0.900;
		rel_MoveToX: 0.070 y: -0.350;
		rel_LineToX: 0.610 y: -0.550;
		rel_MoveToX: -0.375 y: 0.340;
		rel_LineToX: 0.195 y: 0.560! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics76
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.180 y: 0.900;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics77
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.200 y: 0.500;
		rel_LineToX: 0.400 y: -0.500;
		rel_LineToX: -0.180 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics78
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.320 y: 0.900;
		rel_LineToX: 0.180 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics79
	vectorGraphicsEngine
		rel_MoveToX: 0.480 y: -0.900;
		rel_CubicBezierToX: 0.240 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.270 y: 0.150;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.360 y: 0.300 control1X: -0.030 y: 0.150 control2X: -0.210 y: 0.300;
		rel_CubicBezierToX: -0.240 y: -0.300 control1X: -0.150 y: 0.000 control2X: -0.270 y: -0.150;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.360 y: -0.300 control1X: 0.030 y: -0.150 control2X: 0.210 y: -0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics80
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.350 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.040 y: 0.200;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.350 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics81
	vectorGraphicsEngine
		rel_MoveToX: 0.480 y: -0.900;
		rel_CubicBezierToX: 0.240 y: 0.300 control1X: 0.150 y: 0.000 control2X: 0.270 y: 0.150;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: -0.360 y: 0.300 control1X: -0.030 y: 0.150 control2X: -0.210 y: 0.300;
		rel_CubicBezierToX: -0.240 y: -0.300 control1X: -0.150 y: 0.000 control2X: -0.270 y: -0.150;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: 0.360 y: -0.300 control1X: 0.030 y: -0.150 control2X: 0.210 y: -0.300;
		rel_MoveToX: -0.140 y: 0.700;
		rel_LineToX: 0.240 y: 0.300! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics82
	vectorGraphicsEngine
		rel_MoveToX: 0.000 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_LineToX: 0.350 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.040 y: 0.200;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.350 y: 0.000;
		rel_MoveToX: 0.300 y: 0.000;
		rel_LineToX: 0.120 y: 0.400! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics83
	vectorGraphicsEngine
		rel_MoveToX: 0.350 y: -0.500;
		rel_LineToX: -0.100 y: 0.000;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: -0.075 y: 0.000 control2X: -0.135 y: -0.075;
		rel_LineToX: 0.020 y: -0.100;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.015 y: -0.075 control2X: 0.105 y: -0.150;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.140 y: 0.050 control1X: 0.075 y: 0.000 control2X: 0.145 y: 0.025;
		rel_MoveToX: -0.320 y: 0.350;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.040 y: 0.200;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.015 y: 0.075 control2X: -0.105 y: 0.150;
		rel_LineToX: -0.200 y: 0.000;
		rel_CubicBezierToX: -0.140 y: -0.050 control1X: -0.075 y: 0.000 control2X: -0.145 y: -0.025! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics84
	vectorGraphicsEngine
		rel_MoveToX: 0.130 y: -0.900;
		rel_LineToX: 0.600 y: 0.000;
		rel_MoveToX: -0.300 y: 0.000;
		rel_LineToX: -0.180 y: 0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics85
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.140 y: 0.700;
		rel_CubicBezierToX: 0.160 y: 0.200 control1X: -0.020 y: 0.100 control2X: 0.060 y: 0.200;
		rel_LineToX: 0.100 y: 0.000;
		rel_CubicBezierToX: 0.240 y: -0.200 control1X: 0.100 y: 0.000 control2X: 0.220 y: -0.100;
		rel_LineToX: 0.140 y: -0.700! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics86
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.070 y: 0.900;
		rel_LineToX: 0.430 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics87
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.020 y: 0.900;
		rel_LineToX: 0.320 y: -0.600;
		rel_LineToX: 0.080 y: 0.600;
		rel_LineToX: 0.380 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics88
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.320 y: 0.900;
		rel_MoveToX: -0.500 y: 0.000;
		rel_LineToX: 0.680 y: -0.900! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics89
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.160 y: 0.450;
		rel_LineToX: 0.340 y: -0.450;
		rel_MoveToX: -0.340 y: 0.450;
		rel_LineToX: -0.090 y: 0.450! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics90
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.500 y: 0.000;
		rel_LineToX: -0.680 y: 0.900;
		rel_LineToX: 0.500 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics91
	vectorGraphicsEngine
		rel_MoveToX: 0.400 y: -1.000;
		rel_LineToX: -0.200 y: 0.000;
		rel_LineToX: -0.240 y: 1.200;
		rel_LineToX: 0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics92
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_LineToX: 0.180 y: 1.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics93
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: -1.000;
		rel_LineToX: 0.200 y: 0.000;
		rel_LineToX: -0.240 y: 1.200;
		rel_LineToX: -0.200 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics94
	vectorGraphicsEngine
		rel_MoveToX: 0.200 y: 0.000;
		rel_LineToX: 0.180 y: -0.900;
		rel_MoveToX: -0.240 y: 0.200;
		rel_LineToX: 0.240 y: -0.200;
		rel_LineToX: 0.160 y: 0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics95
	vectorGraphicsEngine
		rel_MoveToX: 0.060 y: -0.300;
		rel_LineToX: 0.800 y: 0.000;
		rel_MoveToX: -0.640 y: 0.200;
		rel_LineToX: -0.160 y: -0.200;
		rel_LineToX: 0.240 y: -0.200! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics96
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: 0.080 y: 0.100! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics97
	vectorGraphicsEngine
		rel_MoveToX: 0.170 y: -0.600;
		rel_LineToX: 0.200 y: 0.000;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: 0.075 y: 0.000 control2X: 0.135 y: 0.075;
		rel_LineToX: -0.090 y: 0.450;
		rel_LineToX: -0.250 y: 0.000;
		rel_CubicBezierToX: 0.060 y: -0.300 control1X: -0.203 y: 0.000 control2X: -0.143 y: -0.300;
		rel_LineToX: 0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics98
	vectorGraphicsEngine
		rel_MoveToX: 0.180 y: -0.900;
		rel_LineToX: -0.180 y: 0.900;
		rel_LineToX: 0.250 y: 0.000;
		rel_CubicBezierToX: 0.180 y: -0.150 control1X: 0.075 y: 0.000 control2X: 0.165 y: -0.075;
		rel_LineToX: 0.060 y: -0.300;
		rel_CubicBezierToX: -0.120 y: -0.150 control1X: 0.015 y: -0.075 control2X: -0.045 y: -0.150;
		rel_LineToX: -0.250 y: 0.000! !

!TechnicalFontRenderer methodsFor: 'glyphs - italics' stamp: 'jmv 1/4/2018 12:08:57'!
draw_TechnicalItalics99
	vectorGraphicsEngine
		rel_MoveToX: 0.420 y: -0.600;
		rel_LineToX: -0.150 y: 0.000;
		rel_CubicBezierToX: -0.180 y: 0.150 control1X: -0.075 y: 0.000 control2X: -0.165 y: 0.075;
		rel_LineToX: -0.060 y: 0.300;
		rel_CubicBezierToX: 0.120 y: 0.150 control1X: -0.015 y: 0.075 control2X: 0.045 y: 0.150;
		rel_LineToX: 0.150 y: 0.000! !

!TechnicalFontRenderer class methodsFor: 'testing' stamp: 'jmv 8/16/2018 15:44:56'!
testCharacter: code
	"
	TechnicalFontRenderer testCharacter: 100
	"
	| c r  loc |
	c _ (VectorCanvas onForm: Display) into: self runningWorld.
	r _ TechnicalFontRenderer new vectorGraphicsEngine: c engine.
	r drawM3BoxAtX: 38 y: 22 pointSize: 160 italics: false canvas: c.
	loc _ AffineTransformation withRadians: 0.0 scale: 160 position: 38@182.
	c engine geometryTransformation: loc.
	c engine strokeWidth: 0.1 color: Color green do: [
		r drawCharCode: code italics: false ].
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

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 5/13/2010 09:56'!
name
	^name! !

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 6/14/2010 08:54'!
stops
	^stops
		ifNotNil: [ stops ]
		ifNil: [ (def defAt: inheritFrom) stops ]! !

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

!SVGRadialGradient methodsFor: 'building' stamp: 'jmv 6/16/2010 09:53'!
usedAttributeNames
	^super usedAttributeNames,
		#('cx' 'cy' 'r')! !

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

!SVGGradientStop methodsFor: 'building' stamp: 'jmv 6/16/2010 10:02'!
usedAttributeNames
	^#('style' 'offset' 'stop-color' 'stop-opacity')! !

!SVGGradientStop methodsFor: 'accessing' stamp: 'jmv 5/10/2010 11:07'!
color
	^color! !

!SVGGradientStop methodsFor: 'accessing' stamp: 'jmv 5/7/2010 17:27'!
offset
	^offset! !

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

!SVGTransform methodsFor: 'initialize-release' stamp: 'jmv 4/29/2011 22:03'!
initialize
"	a _ FloatMatrix identity: 3."
	"nil means identity, i.e. no transformation"
	a _ nil.! !

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

!SVGPathArcCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:07:26'!
runOn: aCanvas

	aCanvas multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless! !

!SVGPathArcCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$A! !

!SVGPathCloseCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:10'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream! !

!SVGPathCloseCommand methodsFor: 'private' stamp: 'jmv 5/11/2010 10:05'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."! !

!SVGPathCloseCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:07:07'!
runOn: aCanvas

	aCanvas closePath! !

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

!SVGPathCurveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:51'!
runOn: aCanvas

	aCanvas multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points! !

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

!SVGPathCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:33'!
runOn: aCanvas

	aCanvas multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points! !

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

!SVGPathLineToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:08'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative points: points! !

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

!SVGPathLineToHCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:04:56'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative xs: xs! !

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

!SVGPathLineToVCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:04:32'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative ys: ys! !

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

!SVGPathMoveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:03:47'!
runOn: aCanvas

	aCanvas multiMoveTo: coordinatesAreRelative points: points! !

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

!SVGPathQuadraticCurveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:03:23'!
runOn: aCanvas

	aCanvas multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints! !

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

!SVGPathQuadraticCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:02:58'!
runOn: aCanvas

	aCanvas multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints! !

!SVGPathQuadraticCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:09'!
svgName

	^$T! !

!Color methodsFor: '*Morphic3' stamp: 'jmv 1/21/2015 12:48'!
externalizedWith: aMorphicLocation

	^self! !
SVGElementMorph initialize!
VectorCanvas initialize!
