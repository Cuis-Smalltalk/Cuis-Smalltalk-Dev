'From Cuis 5.0 of 7 November 2016 [latest update: #3792] on 11 June 2019 at 2:05:25 pm'!
'Description '!
!provides: 'VectorGraphics' 1 77!
!requires: 'YAXO' 1 nil nil!
!requires: 'Collections-CompactArrays' 1 2 nil!
!requires: 'Color-Extras' 1 nil nil!
SystemOrganization addCategory: #'VectorGraphics-Kernel'!
SystemOrganization addCategory: #'VectorGraphics-TrueType'!
SystemOrganization addCategory: #'VectorGraphics-SVG'!
SystemOrganization addCategory: #'VectorGraphics-SVG shapes'!
SystemOrganization addCategory: #'VectorGraphics-SVG path commands'!
SystemOrganization addCategory: #'VectorGraphics-Experiments'!


!classDefinition: #SVGError category: #'VectorGraphics-SVG'!
Error subclass: #SVGError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGError class' category: #'VectorGraphics-SVG'!
SVGError class
	instanceVariableNames: ''!

!classDefinition: #SVGWarning category: #'VectorGraphics-SVG'!
Warning subclass: #SVGWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGWarning class' category: #'VectorGraphics-SVG'!
SVGWarning class
	instanceVariableNames: ''!

!classDefinition: #TrueTypeFont category: #'VectorGraphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis replacedGlyphs glyphForms family baseFont derivativeFonts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TrueTypeFont class' category: #'VectorGraphics-TrueType'!
TrueTypeFont class
	instanceVariableNames: ''!

!classDefinition: #TrueTypeFontFamily category: #'VectorGraphics-TrueType'!
FontFamily subclass: #TrueTypeFontFamily
	instanceVariableNames: 'baseTTFontDescription ttFontDescriptionsByEmphasis'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TrueTypeFontFamily class' category: #'VectorGraphics-TrueType'!
TrueTypeFontFamily class
	instanceVariableNames: ''!

!classDefinition: #SVGElementMorph category: #'VectorGraphics-SVG'!
Morph subclass: #SVGElementMorph
	instanceVariableNames: 'defs opacity strokeWidth strokeColorOrGradient strokeOpacity fillColorOrGradient fillOpacity strokeDashArray strokeDashArrayOffset'
	classVariableNames: 'DefaultedAtributes IgnoredAttributes IgnoredElements IgnoredPrefixes'
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGElementMorph class' category: #'VectorGraphics-SVG'!
SVGElementMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGGroupMorph category: #'VectorGraphics-SVG'!
SVGElementMorph subclass: #SVGGroupMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGGroupMorph class' category: #'VectorGraphics-SVG'!
SVGGroupMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGMainMorph category: #'VectorGraphics-SVG'!
SVGGroupMorph subclass: #SVGMainMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGMainMorph class' category: #'VectorGraphics-SVG'!
SVGMainMorph class
	instanceVariableNames: ''!

!classDefinition: #SVGCircle category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGCircle
	instanceVariableNames: 'cx cy r'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGCircle class' category: #'VectorGraphics-SVG shapes'!
SVGCircle class
	instanceVariableNames: ''!

!classDefinition: #SVGEllipse category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGEllipse
	instanceVariableNames: 'cx cy rx ry'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGEllipse class' category: #'VectorGraphics-SVG shapes'!
SVGEllipse class
	instanceVariableNames: ''!

!classDefinition: #SVGLine category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGLine
	instanceVariableNames: 'x1 y1 x2 y2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGLine class' category: #'VectorGraphics-SVG shapes'!
SVGLine class
	instanceVariableNames: ''!

!classDefinition: #SVGPath category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGPath
	instanceVariableNames: 'commands'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGPath class' category: #'VectorGraphics-SVG shapes'!
SVGPath class
	instanceVariableNames: ''!

!classDefinition: #SVGPolyLine category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGPolyLine
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGPolyLine class' category: #'VectorGraphics-SVG shapes'!
SVGPolyLine class
	instanceVariableNames: ''!

!classDefinition: #SVGPolygon category: #'VectorGraphics-SVG shapes'!
SVGPolyLine subclass: #SVGPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGPolygon class' category: #'VectorGraphics-SVG shapes'!
SVGPolygon class
	instanceVariableNames: ''!

!classDefinition: #SVGRect category: #'VectorGraphics-SVG shapes'!
SVGElementMorph subclass: #SVGRect
	instanceVariableNames: 'xMin xMax yMin yMax rx ry'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG shapes'!
!classDefinition: 'SVGRect class' category: #'VectorGraphics-SVG shapes'!
SVGRect class
	instanceVariableNames: ''!

!classDefinition: #M3Exp01Morph category: #'VectorGraphics-Experiments'!
Morph subclass: #M3Exp01Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp01Morph class' category: #'VectorGraphics-Experiments'!
M3Exp01Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp02Morph category: #'VectorGraphics-Experiments'!
Morph subclass: #M3Exp02Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp02Morph class' category: #'VectorGraphics-Experiments'!
M3Exp02Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp03Morph category: #'VectorGraphics-Experiments'!
Morph subclass: #M3Exp03Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp03Morph class' category: #'VectorGraphics-Experiments'!
M3Exp03Morph class
	instanceVariableNames: ''!

!classDefinition: #VectorCanvas category: #'VectorGraphics-Kernel'!
MorphicCanvas subclass: #VectorCanvas
	instanceVariableNames: 'engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorCanvas class' category: #'VectorGraphics-Kernel'!
VectorCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorEngine category: #'VectorGraphics-Kernel'!
Object subclass: #VectorEngine
	instanceVariableNames: 'targetForm targetWidth targetHeight antiAliasingWidth subPixelDelta strokeWidth strokeAlpha strokeRed strokeGreen strokeBlue fillAlpha fillRed fillGreen fillBlue doStroke doFill clipLeft clipTop clipRight clipBottom currentMorph currentOwnerIfClips boundsLeft boundsTop boundsRight boundsBottom morphAtPixel morphBoundsLeft morphBoundsRight morphBoundsTop morphBoundsBottom mask edgeCountPerPixel hopsPerPixel auxBitBlt firstX firstY currentX currentY trajectoryPoints fragmentStartIndexes geometryTransformation lastClientX lastClientY lastControlX lastControlY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngine class' category: #'VectorGraphics-Kernel'!
VectorEngine class
	instanceVariableNames: ''!

!classDefinition: #FormGlyph category: #'VectorGraphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'FormGlyph class' category: #'VectorGraphics-TrueType'!
FormGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTContourConstruction category: #'VectorGraphics-TrueType'!
Object subclass: #TTContourConstruction
	instanceVariableNames: 'points controlPointFlags'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTContourConstruction class' category: #'VectorGraphics-TrueType'!
TTContourConstruction class
	instanceVariableNames: ''!

!classDefinition: #TTFontDescription category: #'VectorGraphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'glyphTable glyphs kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontDescription class' category: #'VectorGraphics-TrueType'!
TTFontDescription class
	instanceVariableNames: ''!

!classDefinition: #TTCFontDescription category: #'VectorGraphics-TrueType'!
TTFontDescription subclass: #TTCFontDescription
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTCFontDescription class' category: #'VectorGraphics-TrueType'!
TTCFontDescription class
	instanceVariableNames: ''!

!classDefinition: #TTFontReader category: #'VectorGraphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'charMap glyphs nGlyphs kernPairs infoBar fontDescription'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontReader class' category: #'VectorGraphics-TrueType'!
TTFontReader class
	instanceVariableNames: ''!

!classDefinition: #TTCFontReader category: #'VectorGraphics-TrueType'!
TTFontReader subclass: #TTCFontReader
	instanceVariableNames: 'fonts'
	classVariableNames: 'EncodingTag'
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTCFontReader class' category: #'VectorGraphics-TrueType'!
TTCFontReader class
	instanceVariableNames: ''!

!classDefinition: #TTFontTableDirEntry category: #'VectorGraphics-TrueType'!
Object subclass: #TTFontTableDirEntry
	instanceVariableNames: 'tag fontData offset length checkSum'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontTableDirEntry class' category: #'VectorGraphics-TrueType'!
TTFontTableDirEntry class
	instanceVariableNames: ''!

!classDefinition: #TTGlyph category: #'VectorGraphics-TrueType'!
Object subclass: #TTGlyph
	instanceVariableNames: 'boundsLeft boundsRight boundsTop boundsBottom contours advanceWidth leftSideBearing rightSideBearing glyphIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTGlyph class' category: #'VectorGraphics-TrueType'!
TTGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTCompositeGlyph category: #'VectorGraphics-TrueType'!
TTGlyph subclass: #TTCompositeGlyph
	instanceVariableNames: 'glyphs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTCompositeGlyph class' category: #'VectorGraphics-TrueType'!
TTCompositeGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTKernPair category: #'VectorGraphics-TrueType'!
Object subclass: #TTKernPair
	instanceVariableNames: 'left right value mask'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTKernPair class' category: #'VectorGraphics-TrueType'!
TTKernPair class
	instanceVariableNames: ''!

!classDefinition: #SVGDefElement category: #'VectorGraphics-SVG'!
Object subclass: #SVGDefElement
	instanceVariableNames: 'def'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGDefElement class' category: #'VectorGraphics-SVG'!
SVGDefElement class
	instanceVariableNames: ''!

!classDefinition: #SVGGradient category: #'VectorGraphics-SVG'!
SVGDefElement subclass: #SVGGradient
	instanceVariableNames: 'stops name inheritFrom averageColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGGradient class' category: #'VectorGraphics-SVG'!
SVGGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGLinearGradient category: #'VectorGraphics-SVG'!
SVGGradient subclass: #SVGLinearGradient
	instanceVariableNames: 'vectorX1 vectorY1 vectorX2 vectorY2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGLinearGradient class' category: #'VectorGraphics-SVG'!
SVGLinearGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGRadialGradient category: #'VectorGraphics-SVG'!
SVGGradient subclass: #SVGRadialGradient
	instanceVariableNames: 'cx cy r'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGRadialGradient class' category: #'VectorGraphics-SVG'!
SVGRadialGradient class
	instanceVariableNames: ''!

!classDefinition: #SVGGradientStop category: #'VectorGraphics-SVG'!
SVGDefElement subclass: #SVGGradientStop
	instanceVariableNames: 'offset color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGGradientStop class' category: #'VectorGraphics-SVG'!
SVGGradientStop class
	instanceVariableNames: ''!

!classDefinition: #SVGDefs category: #'VectorGraphics-SVG'!
Object subclass: #SVGDefs
	instanceVariableNames: 'defs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGDefs class' category: #'VectorGraphics-SVG'!
SVGDefs class
	instanceVariableNames: ''!

!classDefinition: #SVGTransform category: #'VectorGraphics-SVG'!
Object subclass: #SVGTransform
	instanceVariableNames: 'a'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG'!
!classDefinition: 'SVGTransform class' category: #'VectorGraphics-SVG'!
SVGTransform class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCommand category: #'VectorGraphics-SVG path commands'!
Object subclass: #SVGPathCommand
	instanceVariableNames: 'coordinatesAreRelative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathArcCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathArcCommand
	instanceVariableNames: 'rs xAxisRotations toPoints doLargerArcs useIncreasingAngless'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathArcCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathArcCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCloseCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathCloseCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathCloseCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathCloseCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCurveToCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathCurveToCommand
	instanceVariableNames: 'toPoints control1Points control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathCurveToCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathCurveToSmoothCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathCurveToSmoothCommand
	instanceVariableNames: 'toPoints control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathCurveToSmoothCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathCurveToSmoothCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathLineToCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathLineToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToHCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToHCommand
	instanceVariableNames: 'xs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathLineToHCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathLineToHCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathLineToVCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathLineToVCommand
	instanceVariableNames: 'ys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathLineToVCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathLineToVCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathMoveToCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathMoveToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathMoveToCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathMoveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathQuadraticCurveToCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathQuadraticCurveToCommand
	instanceVariableNames: 'toPoints controlPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathQuadraticCurveToCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathQuadraticCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #SVGPathQuadraticCurveToSmoothCommand category: #'VectorGraphics-SVG path commands'!
SVGPathCommand subclass: #SVGPathQuadraticCurveToSmoothCommand
	instanceVariableNames: 'toPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-SVG path commands'!
!classDefinition: 'SVGPathQuadraticCurveToSmoothCommand class' category: #'VectorGraphics-SVG path commands'!
SVGPathQuadraticCurveToSmoothCommand class
	instanceVariableNames: ''!


!TrueTypeFont commentStamp: '<historical>' prior: 0!
TrueType fonts are the most widely used format for pixel independent vector Fonts.

In Cuis what is usually called TrueType Font is represented by TTFontDescription and TTCFontDescription.

TrueTypeFonts, being part of the traditional AbstractFont hierarchy, adds a specific pointSize, and is the object used in texts.!

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

self runningWorld backgroundImage displayAt: 0@0. (VectorCanvas onForm: Display) fullDraw: self runningWorld. Display copy inspect.
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

!FormGlyph commentStamp: '<historical>' prior: 0!
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html

leftSideBearing, topSideBearing, advanceWidth are in (possibly fractional) pixels!

!TTContourConstruction commentStamp: '<historical>' prior: 0!
This class represents a temporary contour structure during the construction of a TTGlyph from a TrueType file.

Instance variables:
	points	<Array of: TTPoint>	The points defining this contour!

!TTFontDescription commentStamp: '<historical>' prior: 0!
Holds a TrueType font in memory.  Is used by TTSampleStringMorph as its font.  

See
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html!

!TTCFontDescription commentStamp: '<historical>' prior: 0!
TrueType Collection (TTC) is an extension of TrueType format that allows combining multiple fonts into a single file, creating substantial space savings for collection of fonts that only use different glyphs on some characters. 

A regular TrueType font comes with .ttf extension, while a TTC comes with .ttc extension.!

!TTFontReader commentStamp: '<historical>' prior: 0!
TTFontReader constructs a TTFontDescription from a TrueType font (.ttf).!

!TTCFontReader commentStamp: '<historical>' prior: 0!
TrueType Collection (TTC) is an extension of TrueType format that allows combining multiple fonts into a single file, creating substantial space savings for collection of fonts that only use different glyphs on some characters. 

A regular TrueType font comes with .ttf extension, while a TTC comes with .ttc extension.

TTCFontReader constructs a TTCFontDescription from a TrueType Collection file (.ttc).!

!TTFontTableDirEntry commentStamp: '<historical>' prior: 0!
This class represents an entry in a truetype font table directory. Used by TTFontReader only.!

!TTGlyph commentStamp: '<historical>' prior: 0!
This class represents a glyph of a TrueType font.

Instance variables:
	bounds			<Rectangle>	The receiver's bounds
	contours		<Array of: PointArray> The compressed contours in the receiver
	advanceWidth	<Integer>	advance width of the glyph
	leftSideBearing	<Integer>	left side bearing
	rightSideBearing <Integer>	right side bearing
	glyphIndex 		<Integer>	the original index of the glyph (used for kerning)!

!TTCompositeGlyph commentStamp: '<historical>' prior: 0!
This class represents a composite TrueType glyph, e.g.one which contains many simple TTGlyphs.!

!TTKernPair commentStamp: '<historical>' prior: 0!
A TTKernPair represents a TrueType kerning pair.

Instance variables:
	left	<Integer>	The glyph index for the left character.
	right <Integer>	The glyph index for the right character.
	value <Integer>	The amount of kerning.
	mask <Integer>	An efficient representation for the left and the right value.!

!TrueTypeFont methodsFor: 'copying' stamp: 'jmv 5/11/2019 18:04:40'!
postCopy
	super postCopy.
	replacedGlyphs _ replacedGlyphs copy! !

!TrueTypeFont methodsFor: 'printing' stamp: 'KenD 5/13/2019 09:03:06'!
printOn: aStream
	aStream
		nextPutAll: 'TrueTypeFont(''';
		nextPutAll: self familyName;
		nextPutAll: ''' ';
		space;
		print: self pointSize;
		nextPut: $)! !

!TrueTypeFont methodsFor: 'objects from disk' stamp: 'jmv 5/17/2019 09:52:48'!
objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:pointSize:
		args: (Array with: self familyName with: self pointSize)! !

!FormGlyph methodsFor: 'copying' stamp: 'jmv 5/11/2019 21:32:14'!
postCopy
	form _ form copy! !

!TTContourConstruction methodsFor: 'printing'!
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: points size;
		"space;
		print: self type;"
		nextPut:$)! !

!TTFontDescription methodsFor: 'accessing' stamp: 'yo 11/30/2002 22:38'!
name

	^ self familyName copyWithout: Character space.
! !

!TTFontDescription methodsFor: 'printing' stamp: 'jmv 5/4/2019 14:34:47'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	fullName printOn: aStream.
	aStream nextPut:$).! !

!TTGlyph methodsFor: 'printing' stamp: 'tk 9/13/1999 09:54'!
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: (contours ifNil: [0] ifNotNil: [contours size]);
		nextPut:$).! !

!SVGGradient methodsFor: 'accessing' stamp: 'jmv 5/13/2010 09:56'!
name
	^name! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 5/7/2019 09:43:20'!
addDerivativeFont: aTrueTypeFont
	derivativeFonts at: aTrueTypeFont emphasis put: aTrueTypeFont! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 5/11/2019 17:13:08'!
baseFont: aStrikeFont

	baseFont _ aStrikeFont.
	baseFont ifNotNil: [
		derivativeFonts _ nil ]! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 5/11/2019 17:24:15'!
family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
	ttFontDescription _ aTTFontDescription.
	emphasis _ aTTFontDescription emphasis.
	pointSize _ aNumber.
	glyphForms _ Array new: 256.
	replacedGlyphs _ Dictionary new.
	family _ aTrueTypeFontFamily.
	baseFont _ nil.
	derivativeFonts _ emphasis = 0 ifTrue: [Dictionary new].! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:36:59'!
ascent
^ttFontDescription ascent * pointSize / ttFontDescription letterMTopSideBearing * 1.0! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/7/2019 10:11:03'!
baseFont
	^ baseFont ifNil: [self]! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/21/2019 11:01:04'!
baseKern
	"self forwarding: #baseKern printOneStack: true."
	self revisar.
^0 "uh?"! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/22/2019 13:29:21'!
cachedFormDepth
^32! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:36:44'!
descent
^ttFontDescription descent negated * pointSize / ttFontDescription letterMTopSideBearing * 1.0! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/4/2019 15:26:55'!
familyName
	^ ttFontDescription familyName ! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:56:44'!
fontDescription
	^ ttFontDescription ! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/20/2019 09:51:32'!
leftOffsetAt: aCharacter
	self revisar. "Should not use cached FormGlyph. Better for VectorEngine."
^(self glyphAt: aCharacter) leftOffset! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:27:24'!
lineSpacing
	"Answer the height of the receiver including any additional line gap."

	^self ascent + self descent! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/16/2019 14:15:13'!
pointSize
"
p.ej.
scrollbarThickness
"
"self forwarding: #pointSize printOneStack: true."
	^pointSize! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/20/2019 09:51:27'!
rightOffsetAt: aCharacter
	self revisar. "Should not use cached FormGlyph. Better for VectorEngine."
^(self glyphAt: aCharacter) rightOffset! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/9/2019 09:04:27'!
widthOf: aCharacter

	| glyph toPixelsScale |
	self revisar. "Without using cached FormGlyph. Better for VectorEngine."
	false ifTrue: [
		glyph _ ttFontDescription at: aCharacter.
		toPixelsScale _ pointSize * 1.0 / ttFontDescription letterMTopSideBearing.
		^ glyph advanceWidth * toPixelsScale ].
	glyph _ self glyphAt: aCharacter.
	^ glyph advanceWidth! !

!TrueTypeFont methodsFor: 'displaying' stamp: 'jmv 5/21/2019 10:56:54'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color

	^ aGrafPort
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		trueTypeFont: self
		color: color

! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/11/2019 17:10:59'!
isBaseFont
	^baseFont isNil! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/4/2019 15:28:52'!
isBold
	^self emphasis allMask: 1! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/4/2019 15:28:59'!
isItalic
	^self emphasis allMask: 2! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/13/2019 23:37:58'!
isStruckThrough
	^emphasis allMask: 8! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/13/2019 23:38:01'!
isSubscript
	^emphasis allMask: 32! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/13/2019 23:38:05'!
isSuperscript
	^emphasis allMask: 16! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/13/2019 23:38:11'!
isUnderlined
	^emphasis allMask: 4! !

!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 5/21/2019 10:52:20'!
glyphAt: aCharacter

	| index |
	index _ aCharacter numericValue + 1.
	^ (glyphForms at: index) ifNil: [
		| ttGlyph formGlyph |
		ttGlyph _ self ttGlyphAt: aCharacter.
		formGlyph _ (ttFontDescription formGlyphFor: ttGlyph pixelSize: pointSize)
			ifNil: [#absentGlyph]
			ifNotNil: [ :theGlyph |
				(emphasis allMask: 4) ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				(emphasis allMask: 8) ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		glyphForms at: index put: formGlyph.
		formGlyph ]! !

!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 5/21/2019 10:46:47'!
ttGlyphAt: aCharacter
	replacedGlyphs at: aCharacter ifPresent: [ :actualCharacter |
		(ttFontDescription at: actualCharacter) ifNotNil: [ :found | ^ found ]].
	(ttFontDescription at: aCharacter) ifNotNil: [ :found | ^ found ].
	^ ttFontDescription at: Character null! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 5/21/2019 11:14:05'!
addEmphasis: code
	"Set the integer code for emphasis. See senders."

	emphasis _ code.
	(emphasis anyMask: 4+8) ifTrue: [
		"They need to be recreated"
		glyphForms _ Array new: 256 ].
	(emphasis allMask: 64) ifTrue: [
		self perform: Preferences assignmentGlyphSelector ].! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 5/7/2019 08:45:26'!
emphasis
	^ emphasis! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 5/13/2019 23:25:18'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	derivative _ derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: code highBit - 1.
	derivedFrom _ self emphasized: code - addedEmphasis.

	"Order is Bold(B), Italic(i), Underlined(U), StruckThrough(X), Superscript(Sup), Subscript(Sub), WithST80Glyphs(ST80)"
	derivative _ addedEmphasis caseOf: {
		[ 1 ] -> [ ^ derivedFrom ].	"No synthetic Bold or Italic. Just use whatever we have."
		[ 2 ] -> [ ^ derivedFrom ].	"No synthetic Bold or Italic. Just use whatever we have."
		[ 4 ] -> [ derivedFrom copy ].
		[ 8 ] -> [ derivedFrom copy ].
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].
		[ 32 ] -> [ derivedFrom syntheticSubscript ].
		[ 64 ] -> [ derivedFrom copy ] }.

	derivative baseFont: self.
	derivative addEmphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/13/2019 23:19:51'!
makeStruckThroughGlyph: aFormGlyph
	| form |
	form _ aFormGlyph form.
	form fillBlack: (0 @ (aFormGlyph topOffset - self descent) extent: form width @ 1)! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/13/2019 23:17:16'!
makeUnderlinedGlyph: aFormGlyph
	| height y oldForm form d |
	d _ (self descent / 4) ceiling.
	y _ aFormGlyph topOffset + d.
	form _ aFormGlyph form.
	height _ aFormGlyph topOffset + (2*d).
	height > form height ifTrue: [
		oldForm _ form.
		form _ Form extent: oldForm width @ height depth: oldForm depth.
		form fillWhite.
		form copyBits: oldForm boundingBox from: oldForm at: `0@0` clippingBox: form boundingBox rule: Form over.
		aFormGlyph form: form ].
	form fillBlack: (0 @ y extent: form width @ d).! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'len 5/28/2019 14:12:42'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'len 5/28/2019 14:12:49'!
syntheticSuperscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

!TrueTypeFont methodsFor: 'swapping glyphs' stamp: 'jmv 5/11/2019 17:31:56'!
useLeftArrow
	replacedGlyphs at: $_ put: Character arrowLeft.
	replacedGlyphs at: $^ put: Character arrowUp.! !

!TrueTypeFont methodsFor: 'swapping glyphs' stamp: 'jmv 5/11/2019 17:32:07'!
useRightArrow
	replacedGlyphs at: $_ put: Character arrowRight.
	replacedGlyphs at: $^ put: Character arrowUp.! !

!TrueTypeFont methodsFor: 'swapping glyphs' stamp: 'jmv 5/11/2019 17:18:55'!
useShortUnderscore! !

!TrueTypeFont methodsFor: 'swapping glyphs' stamp: 'jmv 5/11/2019 17:32:45'!
useUnderscore
	replacedGlyphs removeKey: $_.
	replacedGlyphs removeKey: $^.! !

!TrueTypeFont class methodsFor: 'instance creation' stamp: 'jmv 5/7/2019 09:25:01'!
family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
	^ self new family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber! !

!TrueTypeFontFamily methodsFor: 'initialization' stamp: 'jmv 5/7/2019 09:13:51'!
addTTFontDescription: aTTFontDescription
	ttFontDescriptionsByEmphasis at: aTTFontDescription emphasis put: aTTFontDescription.! !

!TrueTypeFontFamily methodsFor: 'initialization' stamp: 'jmv 5/7/2019 09:38:18'!
baseTTFontDescription: aTTFontDescription
	self assert: aTTFontDescription emphasis = 0.
	familyName _ aTTFontDescription familyName.
	baseFontBySizes _ Dictionary new.
	baseTTFontDescription _ aTTFontDescription.
	ttFontDescriptionsByEmphasis _ Dictionary new.! !

!TrueTypeFontFamily methodsFor: 'initialization' stamp: 'jmv 5/7/2019 17:57:19'!
clearCache
	"
	TrueTypeFontFamily  allInstancesDo: [ :each | each clearCache ].
	"
	| emphasized |
	emphasized _ ttFontDescriptionsByEmphasis values.
	self baseTTFontDescription: baseTTFontDescription.
	emphasized do: [ :each |
		self addTTFontDescription: each ]! !

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:23:52'!
aroundPointSize: aNumber
	^self atPointSize: aNumber! !

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:43:36'!
atPointSize: aNumber
	| baseFont |
	baseFontBySizes at: aNumber ifPresent: [ :existingBaseFont | ^ existingBaseFont ].
	baseFont _ TrueTypeFont family: self ttFontDescription: baseTTFontDescription pointSize: aNumber.
	baseFontBySizes at: aNumber put: baseFont.
	ttFontDescriptionsByEmphasis do: [ :emphasizedTTFontDescription | | emphasizedFont |
		emphasizedFont _ TrueTypeFont family: self ttFontDescription: emphasizedTTFontDescription pointSize: aNumber.
		emphasizedFont baseFont: baseFont.
		baseFont addDerivativeFont: emphasizedFont ].
	^ baseFont! !

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:23:22'!
pointSizes
	^#(6 7 8 9 10 11 12 13 14 15 18 22 26 36 48 72)! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 5/7/2019 09:01:43'!
baseTTFontDescription: aTTFontDescription
	^ self new baseTTFontDescription: aTTFontDescription! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 5/7/2019 09:18:36'!
read: aDirectoryEntry
	"
	TrueTypeFontFamily read: '/home/juan/Downloads/DejaVu2.37/dejavu-fonts-ttf-2.37/ttf/' asDirectoryEntry
	"
	| readFamilyNames |
	readFamilyNames _ TTFontDescription read: aDirectoryEntry.
	readFamilyNames do: [ :familyName | | ttDescriptions baseTTDescription family |
		ttDescriptions _ TTFontDescription withFamilyName: familyName.
		baseTTDescription _ ttDescriptions at: 0.
		family _ TrueTypeFontFamily baseTTFontDescription: baseTTDescription.
		ttDescriptions do: [ :ttfDesc |
			ttfDesc emphasis = 0 ifFalse: [
				family addTTFontDescription: ttfDesc ]].
		FontFamily addFamily: family ]! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 5/10/2019 09:47:25'!
readAdditionalFonts

	TrueTypeFontFamily read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'! !

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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:15'!
examples
"
SVGElementMorph examples
"
| m base |
base _ 'VectorGraphics/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:21'!
examplesBeer
"
SVGElementMorph examplesBeer
"
| m base |
Transcript clear.
base _ 'VectorGraphics/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:25'!
examplesLion
"
SVGElementMorph examplesLion
"
| m base |
Transcript clear.
base _ 'VectorGraphics/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:27'!
examplesMagician
"
SVGElementMorph examplesMagician
"
| m base |
Transcript clear.
base _ 'VectorGraphics/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:30'!
examplesTiger
"
SVGElementMorph examplesTiger
"
| m base |
Transcript clear.
base _ 'VectorGraphics/SVGSamples/'.
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

!SVGElementMorph class methodsFor: 'examples' stamp: 'jmv 3/19/2019 09:59:33'!
phoneExample
"
SVGElementMorph phoneExample
"
| m base |
base _ 'VectorGraphics/SVGSamples/'.
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

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 5/21/2019 11:01:45'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| font toPixelsScale loc |
"	port colorMap: nil."
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	"mhhhhhhhh. Creo que es mejor que venga un font siempre. Y uno de M3!!"
	font _ fontOrNil ifNil: [ AbstractFont default ].
	toPixelsScale _ font pointSize * 1.0 / font fontDescription letterMTopSideBearing.
	loc _ AffineTransformation withScale: toPixelsScale.
	loc _ currentTransformation composedWith: loc.
	engine geometryTransformation: loc.
	engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint / toPixelsScale + (0@(font fontDescription ascent + font fontDescription lineGap))
		trueTypeFont: font
		color: aColor
		canvas: self.
	engine geometryTransformation: currentTransformation.! !

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

!VectorCanvas methodsFor: 'testing' stamp: 'jmv 6/10/2019 10:45:02'!
isCurrentMorphVisible
	self flag: #jmvVer2. "Also #morphBoundsInWorld"
self revisar. "Of course not true. Do it right. But first, assess the need for this method at all!!"
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

!VectorCanvas methodsFor: 'private' stamp: 'jmv 6/10/2019 11:06:59'!
setForm: aForm
	"
	World extent: World extent
	"
	form _ aForm.
	engine _ VectorEngine onForm: form.
	self newClipRect: nil.! !

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

!VectorEngine methodsFor: 'text' stamp: 'jmv 5/21/2019 10:46:54'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aFont color: aColor canvas: aCanvas
	"geometryTransformation is already set to honor font pointSize
	Use as little of aFont as possible!!"
	| glyph destX destY loc |

	self antiAliasingWidth: VectorEngine antiAliasingWidthForOutlineFonts.
	loc _ geometryTransformation.
	self fillColor: aColor do: [
		destY _ aPoint y.
		destX _ aPoint x.
		startIndex to: stopIndex do: [ :charIndex |
			glyph _ aFont ttGlyphAt: (aString at: charIndex).
			glyph contours do: [ :contour |
				contour isEmpty ifFalse: [
					geometryTransformation _ loc composedWith: (AffineTransformation withTranslation: destX@destY) withYAxisNegated.
					contour initialMoveToCommand runOn: aCanvas .
					contour pathCommand runOn: aCanvas ].
			].
			destX _ destX + glyph advanceWidth.
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

!FormGlyph methodsFor: 'initialization' stamp: 'jmv 5/13/2019 22:20:59'!
form: aForm
	form _ aForm! !

!FormGlyph methodsFor: 'initialization' stamp: 'jmv 4/17/2019 11:03:20'!
form: aForm leftOffset: aNumber topOffset: otherNumber advanceWidth: deltaX
	form _ aForm.
	leftOffset _ aNumber.
	topOffset _ otherNumber.
	advanceWidth _ deltaX.! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 4/17/2019 11:20:53'!
advanceWidth
	^advanceWidth! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 4/22/2019 13:15:55'!
form
^form! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 4/22/2019 13:40:41'!
leftOffset
^leftOffset! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 5/20/2019 09:47:16'!
rightOffset
^form width + leftOffset - advanceWidth! !

!FormGlyph methodsFor: 'accessing' stamp: 'jmv 4/22/2019 13:40:49'!
topOffset
^topOffset! !

!FormGlyph class methodsFor: 'instance creation' stamp: 'jmv 4/17/2019 11:03:33'!
form: aForm leftOffset: aNumber topOffset: otherNumber advanceWidth: deltaX
	^ self new form: aForm leftOffset: aNumber topOffset: otherNumber advanceWidth: deltaX! !

!TTContourConstruction methodsFor: 'accessing' stamp: 'jmv 5/16/2019 16:35:20'!
controlPointFlags
	^controlPointFlags! !

!TTContourConstruction methodsFor: 'accessing'!
points
	^points! !

!TTContourConstruction methodsFor: 'accessing' stamp: 'jmv 5/16/2019 16:35:10'!
points: aShortPointArray
	points _ aShortPointArray.
	controlPointFlags _ ByteArray newBits: aShortPointArray size! !

!TTContourConstruction methodsFor: 'converting' stamp: 'jmv 5/16/2019 16:10:02'!
makePointsAbsolute: aPoint
	| t |
	t _ aPoint.
	1 to: points size do: [ :i |
		t _ t + (points at: i).
		points at: i put: t ].
	^ t! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/16/2019 16:14:19'!
initialMoveToCommand

	^ SVGPathMoveToCommand new toPoint: points first! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 4/22/2019 20:36:47'!
isEmpty
	^points size < 2! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/17/2019 08:54:36'!
pathCommand
	"Answer SVG Path Commands of the receiver. This may either be straight line
	segments or quadratic bezier curves. The decision is made upon the type flags
	in TTPoint as follows:
	a) Two subsequent #isEndPoint points define a straight segment
	b) An #isEndPoint point followed by an #isControlPoint point followed 
		by an #isEndPoint point defines a quadratic bezier segment
	c)Two subsequent #isControlPoint points have an implicitely defined 
		#isEndPoint point at half the distance between them"
	| previousPoint firstEndPoint endPoints controlPoints |
	endPoints _ OrderedCollection new.
	controlPoints _ OrderedCollection new.
	previousPoint _ nil.
	1 to: points size do: [ :i | | point |
		point _ points at: i.
		previousPoint
			ifNil: [ firstEndPoint _ point ]
			ifNotNil: [
				(controlPointFlags bitBooleanAt: i)
					ifTrue: [
						(controlPointFlags bitBooleanAt: i-1)
							ifTrue: [	"Quadratic Bezier"
								controlPoints add: previousPoint.
								endPoints add: point + previousPoint / 2.0 ]]
					ifFalse: [
						(controlPointFlags bitBooleanAt: i-1)
							ifTrue: [	"Quadratic Bezier"
								controlPoints add: previousPoint.
								endPoints add: point ]
							ifFalse: [	"Line segment"
								controlPoints add: point.
								endPoints add: point ]]].
		previousPoint _ point.
	].
	(controlPointFlags bitBooleanAt: points size)
		ifTrue: [
			controlPoints add: previousPoint.
			endPoints add: firstEndPoint.]
		ifFalse: [
			controlPoints add: firstEndPoint.
			endPoints add: firstEndPoint.].
	^ SVGPathQuadraticCurveToCommand new toPoints: endPoints controlPoints: controlPoints
	! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/17/2019 08:54:43'!
pathCommandsDo: aBlock
"
Usar paths relativos o absolutos?
"
	"Evaluate aBlock with the SVG Path Commands of the receiver. This may either be straight line
	segments or quadratic bezier curves. The decision is made upon the type flags
	in TTPoint as follows:
	a) Two subsequent #isEndPoint points define a straight segment
	b) An #isEndPoint point followed by an #isControlPoint point followed 
		by an #isEndPoint point defines a quadratic bezier segment
	c)Two subsequent #isControlPoint points have an implicitely defined 
		#isEndPoint point at half the distance between them"
	| lastControlPoint lastEndPoint previousEndPoint previousPoint firstEndPoint |

	previousPoint _ nil.
	lastEndPoint _ nil.
	lastControlPoint _ nil.
	previousEndPoint _ nil.
	1 to: points size do: [ :i | | point |
		point _ points at: i.
		previousPoint
			ifNil: [ firstEndPoint _ lastEndPoint _ point ]
			ifNotNil: [
				(controlPointFlags bitBooleanAt: i)
					ifTrue: [
						(controlPointFlags bitBooleanAt: i-1)
							ifTrue: [
								previousEndPoint _ lastEndPoint. lastEndPoint _ point + previousPoint / 2.0.
								{'bezier1'. previousEndPoint. lastControlPoint. lastEndPoint } print.
								].
							lastControlPoint _ point.
							]
					ifFalse: [
						previousEndPoint _ lastEndPoint. lastEndPoint _ point.
						(controlPointFlags at: i-1) = 1
							ifTrue: [
								lastControlPoint _ previousPoint.
								{'bezier2'. previousEndPoint. lastControlPoint. lastEndPoint } print.
								]
							ifFalse: [
								lastControlPoint _ nil.
								{'recta'. previousEndPoint. lastEndPoint } print.
								]]].
		previousPoint _ point.
	].
	lastEndPoint = firstEndPoint ifFalse: [
		(controlPointFlags bitBooleanAt: points size)
			ifTrue: [
				lastControlPoint _ previousPoint.
				{'bezier3'. lastEndPoint. lastControlPoint. firstEndPoint } print]
			ifFalse: [{'recta'. lastEndPoint. firstEndPoint } print]
		 ]
	! !

!TTContourConstruction class methodsFor: 'instance creation'!
on: points

	^self new points: points! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 6/11/2019 11:16:18'!
at: aCharOrInteger
	| codePoint answer fallbackGlyph |
	fallbackGlyph _ glyphTable at: 1.
	aCharOrInteger isNumber
		ifTrue: [
			answer _ glyphTable at: aCharOrInteger + 1 ]
		ifFalse: [
			codePoint _ aCharOrInteger codePointOfGlyphToUse.
			answer _ glyphTable at: codePoint + 1.
			answer == fallbackGlyph ifTrue: [
				answer _ glyphTable at: aCharOrInteger codePoint + 1 ]].
	"If not included, don't answer fallback but nil."
	(answer == fallbackGlyph and: [codePoint ~= 0])
		ifTrue: [ ^ nil ].
	^ answer! !

!TTFontDescription methodsFor: 'accessing' stamp: 'ar 11/2/1998 01:08'!
at: index put: value
	^self shouldNotImplement! !

!TTFontDescription methodsFor: 'accessing' stamp: 'topa 3/9/2015 23:00'!
fallbackGlyph
	"Answer the fallback glyph, the first in the loca table "
	^ glyphs at: 1! !

!TTFontDescription methodsFor: 'accessing' stamp: 'yo 12/7/2007 10:33'!
first

	^ self.
! !

!TTFontDescription methodsFor: 'accessing' stamp: 'bf 9/29/2010 11:09'!
size

	^ glyphTable size.
! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
copyright
	^copyright! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
familyName
	^familyName! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
fullName
	^fullName! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
postscriptName
	^postscriptName! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
subfamilyName
	^subfamilyName! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
trademark
	^trademark! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:48'!
uniqueName
	^uniqueName! !

!TTFontDescription methodsFor: 'information' stamp: 'ar 11/14/1998 23:49'!
versionName
	^versionName! !

!TTFontDescription methodsFor: 'migration' stamp: 'jmv 4/8/2019 14:14:51'!
blankGlyphForSeparators

	| space |
	space := (self at: Character space codePoint) copy.
	space contours: #().
	Character separators do: [:s | 
		glyphTable at: s codePoint +1 put: space.
	].
! !

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 4/24/2019 20:23:48'!
ascent
	"Ascent of the font. Relative to unitsPerEm. Usually positive."
	^ascent! !

!TTFontDescription methodsFor: 'properties' stamp: 'ar 11/14/1998 23:48'!
bounds
	^bounds! !

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 4/24/2019 20:23:44'!
descent
	"Descender of the font. Relative to unitsPerEm. Usually negative."
	^descent! !

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 4/24/2019 20:24:52'!
letterMTopSideBearing
	"This number is what needs to be scaled by i.e. 12 to render a 12 pixel font"
	letterMTopSideBearing ifNil: [ letterMTopSideBearing _ (self at: $M) topSideBearing ].
	^letterMTopSideBearing! !

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 4/24/2019 20:23:51'!
lineGap
	"Leading of the font. Relative to unitsPerEm."
	^lineGap! !

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 4/24/2019 20:24:36'!
lineSpacing
	^ascent - descent + lineGap! !

!TTFontDescription methodsFor: 'properties' stamp: 'ar 11/14/1998 23:49'!
unitsPerEm
	^unitsPerEm! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 4/24/2019 20:12:58'!
setAscent: asc descent: desc lineGap: lgap
	ascent := asc.
	descent := desc.
	lineGap := lgap! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'ar 11/2/1998 00:28'!
setBounds: aRect unitsPerEm: aNumber
	bounds := aRect.
	unitsPerEm := aNumber.! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 5/17/2019 09:08:36'!
setGlyphs: glyphArray mapping: mappingTable
	glyphs _ glyphArray.
	glyphTable _ mappingTable.
	self blankGlyphForSeparators! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'ar 11/2/1998 00:48'!
setKernPairs: array
	kernPairs := array! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'ar 11/2/1998 00:46'!
setStrings: anArray
	copyright := anArray at: 1.
	familyName := anArray at: 2.
	subfamilyName := anArray at: 3.
	uniqueName := anArray at: 4.
	fullName := anArray at: 5.
	versionName := anArray at: 6.
	postscriptName := anArray at: 7.
	trademark := anArray at: 8.
! !

!TTFontDescription methodsFor: 'testing' stamp: 'jmv 5/4/2019 14:28:56'!
isBaseFont
	^ self emphasis = 0! !

!TTFontDescription methodsFor: 'emphasis' stamp: 'jmv 5/2/2019 22:04:03'!
emphasis
	"Use same encoding as StrikeFont. See senders of emphasis:"
	| emphasis |
	familyName = fullName ifTrue: [ ^ 0 ].
	emphasis := 0.
	('*Bold*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 1 ].
	('*Oblique*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	('*Italic*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	^ emphasis! !

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 5/21/2019 10:47:47'!
formGlyphFor: aTTGlyph pixelSize: pixelSize
	| toPixelsScale glyphOffsetFromFormLeft formWidth glyphOffsetFromFormTop formHeight scaledGlyphBoundsBottom scaledGlyphBoundsLeft scaledGlyphBoundsRight scaledGlyphBoundsTop leftPixelOffset unPixelDeMasParaAAjmvX unPixelDeMasParaAAjmvY offsetVerticalParaQueY0caigaEnHalfPixel topPixelOffset form canvas |
	unPixelDeMasParaAAjmvX _ 1.
	unPixelDeMasParaAAjmvY _ 1.
	offsetVerticalParaQueY0caigaEnHalfPixel _ 0.5.
	toPixelsScale _ pixelSize * 1.0 / self letterMTopSideBearing.

	scaledGlyphBoundsLeft _ aTTGlyph boundsLeft * toPixelsScale.
	scaledGlyphBoundsRight _ aTTGlyph boundsRight * toPixelsScale.
	leftPixelOffset _ (scaledGlyphBoundsLeft - unPixelDeMasParaAAjmvX) floor. "If positive, pixels we save at the left. if negative, xtra pix needed"
	"Offset desde el pixel mas a la izquierda del Form hasta el x=0.0 del glyph TrueType"
	glyphOffsetFromFormLeft _ leftPixelOffset > 0
		ifTrue: [ scaledGlyphBoundsLeft - leftPixelOffset ] "Draw glyph taking into account unneeded pixels shaved from the left"
		ifFalse: [ leftPixelOffset ]. "Make glyph parts at the left of x=0 fint in the form: draw glyph this ammount to the right"
	formWidth _ (scaledGlyphBoundsRight + unPixelDeMasParaAAjmvX - glyphOffsetFromFormLeft) ceiling.
	"{scaledGlyphBoundsLeft. deltaXForDisplayingForm. glyphOffsetFromFormLeft} print."
	
	scaledGlyphBoundsTop _ aTTGlyph boundsBottom * toPixelsScale.
	scaledGlyphBoundsBottom _ aTTGlyph boundsTop * toPixelsScale.
	topPixelOffset _ (scaledGlyphBoundsTop + unPixelDeMasParaAAjmvY) ceiling. "Usually positive. Needed pixels above the baseline"
	"Offset desde el pixel mas a la arriba del Form hasta el y=0.0 del glyph TrueType"
	glyphOffsetFromFormTop _ topPixelOffset + offsetVerticalParaQueY0caigaEnHalfPixel.
	"
	glyphOffsetFromFormTop _ scaledGlyphBoundsTop - offsetVerticalParaQueY0caigaEnHalfPixel + unPixelDeMasParaAAjmvY.
glyphOffsetFromFormTop _ glyphOffsetFromFormTop ceiling.
	topPixelOffset _ glyphOffsetFromFormTop rounded."
	formHeight _ (scaledGlyphBoundsBottom negated + unPixelDeMasParaAAjmvY + glyphOffsetFromFormTop) ceiling.
	"
	{scaledGlyphBoundsLeft. scaledGlyphBoundsRight. scaledGlyphBoundsTop. scaledGlyphBoundsBottom} print.
	{deltaXForDisplayingForm. glyphOffsetFromFormLeft. formWidth. deltaYForDisplayingForm. glyphOffsetFromFormTop. formHeight } print.
	"
	
	form _ Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	
	canvas _ VectorCanvas onForm: form.
	canvas engine geometryTransformation: (AffineTransformation withRadians: 0 scale: toPixelsScale 
		position: glyphOffsetFromFormLeft negated@glyphOffsetFromFormTop) withYAxisNegated.
	canvas fillColor: Color black do: [
		aTTGlyph drawGlyphOn: canvas ].
	^FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (aTTGlyph advanceWidth * toPixelsScale)"ceiling" "Convertir a entero puede ayudar a que se vea mejor en BitBlt, pero el layout es distinto que en VectorGraphics..."! !

!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 5/10/2019 09:42:44'!
read: aDirectoryEntry
	"
	TTFontDescription read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'
	"
	| readFamilyNames |
	Descriptions ifNil: [ Descriptions _ Dictionary new ].
	readFamilyNames _ Set new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttfCollection ttf |
			ttfCollection _ TTCFontReader readTTFFrom: file binaryContents.
			self assert: ttfCollection size = 1. "Or else must understand!!"
			ttf _ ttfCollection first.
			(Descriptions at: ttf familyName ifAbsentPut: [ Dictionary new]) at: ttf emphasis put: ttf.
			readFamilyNames add: ttf familyName ]
		matches: [ :file | '*.ttf' match: file name ].
	^readFamilyNames! !

!TTFontDescription class methodsFor: 'instance accessing' stamp: 'jmv 5/7/2019 09:18:33'!
withFamilyName: aString
	"
	TTFontDescription withFamilyName: 'DejaVu Sans'
	"
	^Descriptions at: aString! !

!TTFontDescription class methodsFor: 'class initialization' stamp: 'jmv 5/7/2019 16:14:43'!
initialize
	"
	TTFontDescription  initialize
	"
	Descriptions _ nil! !

!TTCFontDescription methodsFor: 'accessing' stamp: 'yo 11/14/2002 18:28'!
size

	^ glyphs size.
! !

!TTFontReader methodsFor: 'processing' stamp: 'topa 3/19/2015 11:48'!
processCharacterMappingTable: entry
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| copy initialOffset nSubTables pID sID offset cmap assoc |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUShort.
	1 to: nSubTables do:[:i|
		pID := entry nextUShort.
		sID := entry nextUShort.
		offset := entry nextULong.
		"Check if this is either a Macintosh encoded table
		or a Windows encoded table"
		(#(0 1 3) includes: pID) ifTrue: [
			(assoc notNil and: [assoc key = pID]) ifFalse: [
				"Go to the beginning of the table"
				copy := entry copy.
				copy offset: initialOffset + offset.
				cmap := self decodeCmapFmtTable: copy.
				(pID = 0 and: [cmap notNil]) "Prefer Unicode encoding over everything else"
					ifTrue: [^ pID -> cmap].
				"(pID = 1 and: [cmap notNil])" "Prefer Macintosh encoding over everything else"
					"ifTrue: [pID -> cmap]."
				assoc := pID -> cmap. "Keep it in case we don't find a better table"
			].
		].
	].
	^assoc! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/16/2019 21:21:08'!
processCompositeGlyph: glyph contours: nContours from: entry
	"Read a composite glyph from the font data. The glyph passed into this method contains some state variables that must be copied into the resulting composite glyph."
	| flags glyphIndex hasInstr cGlyph ofsX ofsY iLen a11 a12 a21 a22 m |
	cGlyph := TTCompositeGlyph new.
	a11 := a22 := 16r4000.	"1.0 in F2Dot14"
	a21 := a12 := 0.		"0.0 in F2Dot14"
	"Copy state"
	cGlyph
		boundsLeft: glyph boundsLeft boundsRight: glyph boundsRight
			boundsTop: glyph boundsTop boundsBottom: glyph boundsBottom;
		glyphIndex: glyph glyphIndex.
	hasInstr := false.
	[ flags := entry nextUShort.
	glyphIndex := entry nextUShort + 1.
	(flags bitAnd: 1) = 1 ifTrue:[
		ofsX := entry nextShort.
		ofsY := entry nextShort.
	] ifFalse:[
		(ofsX := entry nextByte) > 127 ifTrue:[ofsX := ofsX - 256].
		(ofsY := entry nextByte) > 127 ifTrue:[ofsY := ofsY - 256]].
	((flags bitAnd: 2) = 2) ifFalse:[self halt].
	(flags bitAnd: 8) = 8 ifTrue:[
		a11 := a22 := entry nextShort].
	(flags bitAnd: 64) = 64 ifTrue:[
		a11 := entry nextShort.
		a22 := entry nextShort].
	(flags bitAnd: 128) = 128 ifTrue:[
		"2x2 transformation"
		a11 := entry nextShort.
		a21 := entry nextShort.
		a12 := entry nextShort.
		a22 := entry nextShort].
	m := AffineTransformation new.
	"Convert entries from F2Dot14 to float"
	m a11: (a11 asFloat / 16r4000).
	m a12: (a12 asFloat / 16r4000).
	m a21: (a21 asFloat / 16r4000).
	m a22: (a22 asFloat / 16r4000).
	m a13: ofsX.
	m a23: ofsY.
	cGlyph addGlyph: (glyphs at: glyphIndex) transformation: m.
	hasInstr := hasInstr or:[ (flags bitAnd: 256) = 256].
	"Continue as long as the MORE_COMPONENTS bit is set"
	(flags bitAnd: 32) = 32] whileTrue.
	hasInstr ifTrue:[
		iLen := entry nextUShort.
		entry skip: iLen].
	^cGlyph! !

!TTFontReader methodsFor: 'processing' stamp: 'ar 11/2/1998 00:42'!
processFontHeaderTable: entry
"Value				Data Type    Description
unitsPerEm			USHORT      Granularity of the font's em square.
xMax				USHORT      Maximum X-coordinate for the entire font.
xMin				USHORT      Minimum X-coordinate for the entire font.
yMax				USHORT      Maximum Y-coordinate for the entire font.
yMin				USHORT      Minimum Y-coordinate for the entire font.
indexToLocFormat	SHORT       Used when processing the Index To Loc Table."
	| origin corner units indexToLocFormat |
	entry skip: 4. "Skip table version number"
	entry skip: 4. "Skip font revision number"
	entry skip: 4. "Skip check sum adjustment"
	entry skip: 4. "Skip magic number"
	entry skip: 2. "Skip flags"

	units := entry nextUShort.

	entry skip: 8. "Skip creation date"
	entry skip: 8. "Skip modification date"

	"Get min/max values of all glyphs"
	origin := entry nextShort @ entry nextShort.
	corner := entry nextShort @ entry nextShort.

	entry skip: 2. "Skip mac style"
	entry skip: 2. "Skip lowest rec PPEM"
	entry skip: 2. "Skip font direction hint"
	indexToLocFormat := entry nextShort.

	fontDescription setBounds: (origin corner: corner) unitsPerEm: units.
	^indexToLocFormat! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/17/2019 09:01:28'!
processGlyphDataTable: entry offsets: offsetArray
	"Read the actual glyph data from the font.
	offsetArray contains the start offsets in the data for each glyph."
	| initialOffset |
	initialOffset _ entry offset.
	glyphs _ Array new: nGlyphs.
	1 to: nGlyphs do: [ :i | 
		glyphs at: i put: (TTGlyph new glyphIndex: i-1)].
	'Reading glyph data' 
		displayProgressAt: 100@100
		from: 1 
		to: nGlyphs 
		during: [ :bar |
			| glyph glyphOffset nextOffset glyphLength numberOfContours bb bl br bt |
			1 to: nGlyphs do: [ :glyphIndex |
				bar value: glyphIndex.
				glyph _ glyphs at: glyphIndex.
				glyphOffset _ offsetArray at: glyphIndex.
				nextOffset _ offsetArray at: glyphIndex+1.
				glyphLength _ nextOffset - glyphOffset.
				glyphLength = 0 ifFalse:[
					entry offset: initialOffset + glyphOffset.
					numberOfContours _ entry nextShort.
					bl _ entry nextShort.
					bt _ entry nextShort.
					br _ entry nextShort.
					bb _ entry nextShort.
					glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
					numberOfContours >= 0
						ifTrue: [
							self processSimpleGlyph: glyph contours: numberOfContours from: entry ]
						ifFalse: [
							glyph _ self processCompositeGlyph: glyph contours: numberOfContours from: entry.
							glyphs at: glyphIndex put: glyph ]]]].! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 4/24/2019 20:12:00'!
processHorizontalHeaderTable: entry
"See 
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
https://docs.microsoft.com/en-us/typography/opentype/spec/hhea
https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html
http://chanae.walon.org/pub/ttf/ttf_glyphs.htm

https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6.html
"
"
ascent  						SHORT          Distance from baseline of highest ascender.
descent  					SHORT          Distance from baseline of lowest descender.
lineGap  					SHORT          Typographic line gap.
numOfLongHorMetrics  	USHORT       Number of advance widths in metrics table (HTMX Table); may be smaller than the total number of glyphs.
"
	| ascent descent lineGap numOfLongHorMetrics |
	entry skip: 4. "Skip table version"
	ascent := entry nextShort.
	descent := entry nextShort.
	lineGap := entry nextShort.
	entry skip: 2. "Skip advanceWidthMax"
	entry skip: 2. "Skip minLeftSideBearing"
	entry skip: 2. "Skip minRightSideBearing"
	entry skip: 2. "Skip xMaxExtent"
	entry skip: 2. "Skip caretSlopeRise"
	entry skip: 2. "Skip caretSlopeRun"
	entry skip: 10. "Skip 5 reserved shorts"
	entry skip: 2. "Skip metricDataFormat"

	numOfLongHorMetrics := entry nextUShort.
	fontDescription setAscent: ascent descent: descent lineGap: lineGap.
	^numOfLongHorMetrics! !

!TTFontReader methodsFor: 'processing' stamp: 'ar 11/2/1998 00:40'!
processHorizontalMetricsTable: entry length: numHMetrics
	"Extract the advance width, left side bearing, and right
	side bearing for each glyph from the Horizontal Metrics Table."
	|  index lastAW glyph |
	index := 1.
	[index <= numHMetrics] whileTrue:[
		glyph := glyphs at: index.
		glyph advanceWidth: entry nextUShort.
		glyph leftSideBearing: entry nextShort.
		glyph updateRightSideBearing.
		index := index + 1].
	index = (nGlyphs +1) ifTrue:[^true].
	lastAW := (glyphs at: index-1) advanceWidth.

	[index <= nGlyphs] whileTrue:[
		glyph := glyphs at: index.
		glyph advanceWidth: lastAW.
		glyph leftSideBearing: entry nextShort.
		glyph updateRightSideBearing.
		index := index + 1].! !

!TTFontReader methodsFor: 'processing' stamp: 'ar 11/2/1998 00:43'!
processIndexToLocationTable: entry format: indexToLocFormat
"glyphOffset    ULONG[numGlyphs]   An array that contains each glyph's
                                 offset into the Glyph Data Table.
"	| glyphOffset offset|
	glyphOffset := Array new: nGlyphs+1.
	1 to: nGlyphs+1 do:[:i|
		(indexToLocFormat = 0) ifTrue:[ "Format0: offset/2 is stored"
			offset := entry nextUShort * 2.
		] ifFalse:["Format1: store actual offset"
			offset := entry nextULong].
		glyphOffset at: i put: offset].
	^glyphOffset! !

!TTFontReader methodsFor: 'processing' stamp: 'topa 3/19/2015 01:33'!
processKerningSubTableType0: entry

	| kp nKernPairs |
	nKernPairs := entry nextUShort.
	entry skip: 2. "Skip search range"
	entry skip: 2. "Skip entry selector"
	entry skip: 2. "Skip range shift"
	kernPairs := Array new: nKernPairs.
	1 to: nKernPairs do:[:i|
		kp := TTKernPair new.
		kp left: entry nextUShort.
		kp right: entry nextUShort.
		kp value: entry nextShort.
		kernPairs at: i put: kp].
	^true
! !

!TTFontReader methodsFor: 'processing' stamp: 'topa 3/19/2015 01:34'!
processKerningTable: entry
	"Extract the kerning information for pairs of glyphs."
	| version  nTables covLow covHigh |
	version := entry nextUShort. "Skip table version"
	nTables := entry nextUShort. "Skip number of sub tables -- we're using the first one only"
	nTables = 0 ifTrue: [" This detection is hacky "
		entry skip: -4. "rewind"
		^ self processOSXKerningTable: entry].
	entry skip: 2. "Skip current subtable number"
	entry skip: 2. "Skip length of subtable"
	covHigh := entry nextByte.
	covLow := entry nextByte.

	"Make sure the format is right (kerning table and format type 0)"
	((covLow bitAnd: 2) = 2 or:[ covHigh ~= 0]) ifTrue:[^false].
	"Subtable"
	^ self processKerningSubTableType0: entry.
! !

!TTFontReader methodsFor: 'processing'!
processMaximumProfileTable: entry
"
numGlyphs         USHORT      The number of glyphs in the font.
"
	entry skip: 4. "Skip Table version number"
	nGlyphs := entry nextUShort.! !

!TTFontReader methodsFor: 'processing' stamp: 'topa 3/7/2015 02:16'!
processNamingTable: entry
"copyright         CHARPTR     The font's copyright notice.
familyName        CHARPTR     The font's family name.
subfamilyName     CHARPTR     The font's subfamily name.
uniqueName        CHARPTR     A unique identifier for this font.
fullName          CHARPTR     The font's full name (a combination of
                                          familyName and subfamilyName).
versionName       CHARPTR     The font's version string.
"	| nRecords initialOffset format storageOffset  strings |
	strings := Array new: 8.
	strings atAllPut:''.
	initialOffset := entry offset.
	format := entry nextUShort.
	format = 0 ifFalse: [self error: 'Cannot handle format 1 naming tables'].
	"Get the number of name records"
	nRecords := entry nextUShort.
	"Offset from the beginning of this table"
	storageOffset := entry nextUShort + initialOffset.
	1 to: nRecords do:[:i| | pID sID lID nID length offset multiBytes string |
		pID := entry nextUShort.
		sID := entry nextUShort.
		lID := entry nextUShort.
		nID := entry nextUShort.
		length := entry nextUShort.
		offset := entry nextUShort.
		"Read only Macintosh or Microsoft strings"
		(pID = 1 or:[pID = 3 and:[sID = 1]]) ifTrue:[
			"MS uses Unicode all others single byte"
			multiBytes := pID = 3.
			string := entry stringAt: storageOffset + offset length: length multiByte: multiBytes.

			"Select only English names.
			Note: We prefer Macintosh strings about everything."
			((pID = 1 and: [lID = 0]) or: [pID = 3 and: [lID = 16r0409]]) ifTrue: [
				"Put the name at the right location."
				nID < strings size ifTrue:[
					(pID = 1 or:[(strings at: nID+1) = ''])
						ifTrue:[strings at: nID+1 put: string].
				].
			].
		].
	].
	fontDescription setStrings: strings.! !

!TTFontReader methodsFor: 'processing' stamp: 'topa 3/19/2015 01:35'!
processOSXKerningTable: entry
	"From Apple's TrueType reference:
	
	Previous versions of the 'kern' table defined both the version and nTables fields in the header as UInt16 values and not UInt32 values. Use of the older format on OS X is discouraged (although AAT can sense an old kerning table and still make correct use of it). Microsoft Windows still uses the older format for the 'kern' table and will not recognize the newer one. Fonts targeted for OS X only should use the new format; fonts targeted for both OS X and Windows should use the old format.
	"
	"Extract the kerning information for pairs of glyphs."
	| version  nTables length coverage tupleIndex |
	version := entry nextULong. "Skip table version"
	nTables := entry nextULong. "Skip number of sub tables -- we're using the first one only"

	length := entry nextULong. "Skip length of subtable"
	coverage := entry nextUShort.
	tupleIndex := entry nextUShort. "Skip tuple index".
	"Make sure the format is right (kerning table and format type 0)"
	coverage = 0 ifFalse: [^false].
	
	"Subtable"
	^ self processKerningSubTableType0: entry.
! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 4/10/2019 09:33:27'!
processSimpleGlyph: glyph contours: nContours from: entry

	| endPts  nPts iLength flags |
	endPts := Array new: nContours.
	1 to: nContours do:[:i| endPts at: i put: entry nextUShort].
	glyph initializeContours: nContours with: endPts.
	nContours = 0 ifTrue: [^ self].
	nPts := endPts last + 1.
	iLength := entry nextUShort. "instruction length"
	entry skip: iLength.
	flags := self getGlyphFlagsFrom: entry size: nPts.
	self readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	self readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	glyph makePointsAbsolute! !

!TTFontReader methodsFor: 'public' stamp: 'jmv 4/8/2019 17:55:29'!
readFrom: fontData

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset cmap numHMetrics indexToLocFormat |

	"Read the raw font byte data"
	fontDescription := TTFontDescription new.

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData) == nil ifTrue:[
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData) == nil ifTrue:[
		Transcript cr; show:'This font does not have a kerning table';endEntry].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	cmap := self processCharacterMappingTable: charMapEntry.
	(cmap == nil or:[cmap value == nil])
		ifTrue:[^self error:'This font has no suitable character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue:[kernPairs := #()]
		ifFalse:[self processKerningTable: kerningEntry].
	charMap := self processCharMap: cmap.
	fontDescription setGlyphs: glyphs mapping: charMap.
	fontDescription setKernPairs: kernPairs.
	^fontDescription! !

!TTFontReader methodsFor: 'private' stamp: 'topa 9/1/2015 20:14'!
decodeCmapFmtTable: entry
	| cmapFmt length entryCount segCount segments offset cmap firstCode |
	cmapFmt := entry nextUShort.
	length := entry nextUShort.
	entry skip: 2. "skip version"

	cmapFmt = 0 ifTrue: "byte encoded table"
		[length := length - 6. 		"should be always 256"
		length <= 0 ifTrue: [^ nil].	"but sometimes, this table is empty"
		cmap := Array new: length.
		entry nextBytes: length into: cmap startingAt: entry offset.
		^ cmap].

	cmapFmt = 4 ifTrue: "segment mapping to deltavalues"
		[segCount := entry nextUShort // 2.
		entry skip: 6. "skip searchRange, entrySelector, rangeShift"
		segments := Array new: segCount.
		segments := (1 to: segCount) collect: [:e | Array new: 4].
		1 to: segCount do: [:i | (segments at: i) at: 2 put: entry nextUShort]. "endCount"
		entry skip: 2. "skip reservedPad"
		1 to: segCount do: [:i | (segments at: i) at: 1 put: entry nextUShort]. "startCount"
		1 to: segCount do: [:i | (segments at: i) at: 3 put: entry nextShort]. "idDelta"
		offset := entry offset.
		1 to: segCount do: [:i | (segments at: i) at: 4 put: entry nextUShort]. "idRangeOffset"
		entryCount := segments inject: 0 into: [:max :seg | max max: seg second].
		cmap := Array new: entryCount+1 withAll: 0..
		segments withIndexDo:
			[:seg :si | | code |
			seg first to: seg second do:
				[:i |
					seg last > 0 ifTrue:
						["offset to glypthIdArray - this is really C-magic!!"
						entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 
						code := entry nextUShort.
						code > 0 ifTrue: [code := code + seg third]]
					ifFalse:
						["simple offset"
						code := i + seg third].
					cmap at: i + 1 put: code]].
		^ cmap].

	cmapFmt = 6 ifTrue: "trimmed table"
		[firstCode := entry nextUShort.
		entryCount := entry nextUShort.
		cmap := Array new: entryCount + firstCode withAll: 0.
		entryCount timesRepeat:
			[cmap at: (firstCode := firstCode + 1) put: entry nextUShort].
		^ cmap].
	^ nil! !

!TTFontReader methodsFor: 'private' stamp: 'ar 11/2/1998 01:33'!
getGlyphFlagsFrom: entry size: nPts
	"Read in the flags for this glyph.  The outer loop gathers the flags that
	are actually contained in the table.  If the repeat bit is set in a flag
	then the next byte is read from the table; this is the number of times
	to repeat the last flag.  The inner loop does this, incrementing the
	outer loops index each time."
	| flags index repCount flagBits |
	flags := ByteArray new: nPts.
	index := 1.
	[index <= nPts] whileTrue:[
		flagBits := entry nextByte.
		flags at: index put: flagBits.
		(flagBits bitAnd: 8) = 8 ifTrue:[
			repCount := entry nextByte.
			repCount timesRepeat:[
				index := index + 1.
				flags at: index put: flagBits]].
		index := index + 1].
	^flags! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/2/2019 21:21:40'!
getTableDirEntry: tagString from: fontData
	"Find the table named tagString in fontData and return a table directory entry for it."
	| nTables pos currentTag tag |
	nTables := fontData shortAt: 5 bigEndian: true.
	tag := ByteArray new: 4.
	1 to: 4 do:[:i| tag byteAt: i put: (tagString at: i) asciiValue].
	tag := tag longAt: 1 bigEndian: true.
	pos := 13.
	1 to: nTables do:[:i|
		currentTag := fontData longAt: pos bigEndian: true.
		currentTag = tag ifTrue:[^TTFontTableDirEntry on: fontData at: pos].
		pos := pos+16].
	^nil! !

!TTFontReader methodsFor: 'private' stamp: 'ar 4/10/2005 16:10'!
macToWin: index
	^ (index - 1) asCharacter macToSqueak asciiValue + 1! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/16/2019 16:35:47'!
readGlyphXCoords:entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
	"Read the x coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits xValue contour |
	startPoint _ 1.
	1 to: nContours do: [ :i |
		contour _ glyph contours at: i.
		"Get the end point"
		endPoint _ (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits _ flags at: j.
			"If bit zero in the flag is set then this point is an on-curve
			point, if not, then it is an off-curve point."
			contour controlPointFlags bitAt: (j - startPoint + 1) put: 1-(flagBits bitAnd: 1).
			"First we check to see if bit one is set.  This would indicate that
			the corresponding coordinate data in the table is 1 byte long.
			If the bit is not set, then the coordinate data is 2 bytes long."
			xValue _ (flagBits bitAnd: 2) = 2
				ifTrue: [ "one byte"
					(flagBits bitAnd: 16)=16 
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					"If bit four is set, then this coordinate is the same as the
					last one, so the relative offset (of zero) is stored.  If bit
					is not set, then read in two bytes and store it as a signed value."
					(flagBits bitAnd: 16) = 16
						ifTrue: [ 0 ]
						ifFalse: [ entry nextShort ]].
			contour points at: (j - startPoint + 1) putX: xValue.
		].
		startPoint _ endPoint + 1 ]! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/16/2019 16:32:24'!
readGlyphYCoords:entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
	"Read the y coordinates for the given glyph from the font file."
	| startPoint endPoint flagBits yValue contour |
	startPoint _ 1.
	1 to: nContours do: [ :i |
		contour _ glyph contours at: i.
		"Get the end point"
		endPoint _ (endPts at: i) + 1.
		"Store number of points"
		startPoint to: endPoint do: [ :j |
			flagBits _ flags at: j.
			"Check if this value one or two byte encoded"
			yValue _ (flagBits bitAnd: 4) = 4
				ifTrue: [ "one byte"
					(flagBits bitAnd: 32)=32
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					(flagBits bitAnd: 32) = 32
						ifTrue: [ 0 ]
						ifFalse: [ entry nextShort ]].
			contour points at: (j - startPoint + 1) putY: yValue.
		].
		startPoint _ endPoint + 1 ]! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'nk 7/16/2003 15:56'!
fileReaderServicesForFile: fullName suffix: suffix


	^(suffix = 'fnt')  | (suffix = '*') 
		ifTrue: [ self services]
		ifFalse: [#()]
! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'ar 9/5/2010 09:38'!
initialize
	"self initialize"

	FileList registerFileReader: self! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 19:31'!
openTTFFile: fullName 

	(TTFontReader parseFileNamed: fullName) asMorph open! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'KR 3/21/2008 20:52'!
serviceOpenTrueTypeFont

	^ SimpleServiceEntry 
				provider: self 
				label: 'open true type font' translatedNoop
				selector: #openTTFFile:
				description: 'open true type font' translatedNoop! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:03'!
services

	^ Array with: self serviceOpenTrueTypeFont
! !

!TTFontReader class methodsFor: 'class initialization' stamp: 'ar 9/5/2010 09:38'!
unload

	FileList unregisterFileReader: self ! !

!TTFontReader class methodsFor: 'instance creation' stamp: 'jmv 4/8/2019 17:55:05'!
parseFileNamed: fileName
	"TTFontReader parseFileNamed:'c:\windows\fonts\arial.ttf'"
	"TTFontReader parseFileNamed:'c:\windows\times.ttf'"
	
	^ self readFrom: fileName asFileEntry binaryContents! !

!TTCFontReader methodsFor: 'as yet unclassified' stamp: 'jmv 4/8/2019 17:47:24'!
getTableDirEntry: tagString from: fontData offset: offset
	"Find the table named tagString in fontData and return a table directory entry for it."

	| numTables pos currentTag tag |
	numTables := fontData shortAt: 5 + offset bigEndian: true.
	tag := ByteArray new: 4.
	1 to: 4 do:[:i| tag byteAt: i put: (tagString at: i) asciiValue].
	tag := tag longAt: 1 bigEndian: true.
	pos := 13 + offset.
	1 to: numTables do:[:i|
		currentTag := fontData longAt: pos bigEndian: true.
		currentTag = tag ifTrue:[^TTFontTableDirEntry on: fontData at: pos].
		pos := pos+16].
	^nil! !

!TTCFontReader methodsFor: 'as yet unclassified' stamp: 'yo 11/8/2002 15:47'!
parseTTCHeaderFrom: fontData

	| pos nTables |
	nTables := fontData longAt: 9 bigEndian: true.
	fonts := Array new: nTables.
	pos := 13.
	1 to: nTables do: [:i |
		fonts at: i put: (fontData longAt: pos bigEndian: true).
		pos := pos + 4.
	].

	^ fonts
! !

!TTCFontReader methodsFor: 'as yet unclassified' stamp: 'jmv 4/8/2019 12:50:44'!
processCharMap: assoc
	"Process the given character map"

	| glyph cmap encode0 encode1 char value null |
	cmap := assoc value.
	null := (glyphs at: (cmap at: Character space codePoint + 1) + 1) copy.
	null contours: #().

	encode0 := Array new: 256 withAll: glyphs first.
	encode1 := Array new: 65536 withAll: glyphs first.

	0 to: 255 do: [:i |
		char := Character numericValue: i.
		glyph := glyphs at: (cmap at: char codePoint + 1) + 1.
		encode0 at: i+1 put: glyph.
	].
	Character separators do: [:c |
		encode0 at: (c asciiValue + 1) put: null.
	].
	0 to: 65536 - 1 do: [:i |
		value := cmap at: i+1.
		value = 65535 ifFalse: [ "???"
			| g |
			g := glyphs at: value+1 ifAbsent: [ null. ].
			(g isKindOf: TTCompositeGlyph) ifFalse: [
				encode1 at: i+1 put: g.
			] ifTrue: [
				g basicGlyphs: (((glyphs at: value+1) basicGlyphs) collect: [:t | t key->(glyphs at: (t value glyphIndex+1))]).
				encode1 at: i+1 put: g
			].
		]
	].

	^ {encode0. encode1}.
! !

!TTCFontReader methodsFor: 'as yet unclassified' stamp: 'ul 4/24/2017 01:53'!
readFrom: fontData fromOffset: offset at: encodingTag

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset cmap numHMetrics indexToLocFormat fontDescription0 fontDescription1 array result |

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData  offset: offset) == nil ifTrue:[
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData offset: offset) == nil ifTrue:[
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData offset: offset) == nil ifTrue:[
		Transcript cr; show:'This font does not have a kerning table';endEntry].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	cmap := self processCharacterMappingTable: charMapEntry.
	(cmap == nil or:[cmap value == nil])
		ifTrue:[^self error:'This font has no suitable character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue:[kernPairs := #()]
		ifFalse:[self processKerningTable: kerningEntry].
	array := self processCharMap: cmap.
	fontDescription0 := fontDescription shallowCopy.
	fontDescription1 := fontDescription shallowCopy.
	fontDescription0 setGlyphs: (array at: 1) mapping: (array at: 1)..
	fontDescription1 setGlyphs: (array at: 2) mapping: (array at: 2)..
	fontDescription0 setKernPairs: kernPairs.
	fontDescription1 setKernPairs: kernPairs.
	result := OrderedCollection new.
	(encodingTag = nil or: [encodingTag = 0]) ifTrue: [^ Array with: fontDescription1].
	result add: fontDescription0.
	encodingTag -1 timesRepeat: [result add: nil].
	result add: fontDescription1.
	^ result asArray.

! !

!TTCFontReader methodsFor: 'as yet unclassified' stamp: 'jmv 5/4/2019 15:04:54'!
readTTFFrom: aByteArray
	"For .ttf (not .ttc) files."
	"Read the raw font byte data"
	fontDescription := TTCFontDescription new.

	^ self readFrom: aByteArray fromOffset: 0 at: EncodingTag.
! !

!TTCFontReader class methodsFor: 'as yet unclassified' stamp: 'yo 2/15/2004 18:45'!
encodingTag: aNumber
"
	TTCFontReader encodingTag: 6
"

	EncodingTag := aNumber.
! !

!TTCFontReader class methodsFor: 'as yet unclassified' stamp: 'jmv 5/4/2019 15:18:14'!
readTTFFrom: aByteArray
	"For .ttf (not .ttc) files."

	^self new readTTFFrom: aByteArray! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextByte

	| value |
	value := fontData byteAt: offset.
	offset := offset + 1.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextBytes: numBytes into: array startingAt: byteOffset

	1 to: numBytes do:[:i|
		array at: i put: (fontData byteAt: byteOffset + i - 1)].! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextLong

	| value |
	value := fontData longAt: offset bigEndian: true.
	offset := offset + 4.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextShort

	| value |
	value := fontData shortAt: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextULong

	| value |
	value := fontData unsignedLongAt: offset bigEndian: true.
	offset := offset + 4.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing'!
nextUShort

	| value |
	value := fontData unsignedShortAt: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing'!
offset
	^offset! !

!TTFontTableDirEntry methodsFor: 'accessing'!
offset: newOffset
	offset := newOffset! !

!TTFontTableDirEntry methodsFor: 'accessing'!
skip: n
	"Skip n bytes"
	offset := offset + n.! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 4/8/2019 12:29:42'!
stringAt: stringOffset length: byteLength multiByte: aBoolean

	| string index stringLength |
	aBoolean ifFalse:[
		stringLength := byteLength.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character asciiValue: (fontData byteAt: index + i - 1))].
		^string
	] ifTrue:[
		"UTF-16BE
		https://docs.microsoft.com/en-us/typography/opentype/spec/name#enc3
		This code appears to assume only lower byte is needed, most likely only ascii is supported here (jmv)"
		stringLength := byteLength // 2.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character asciiValue: (fontData byteAt: index + 1)).
			index := index + 2].
		^string]! !

!TTFontTableDirEntry methodsFor: 'initialize-release'!
on: fd at: index

	fontData := fd.
	tag := fontData longAt: index bigEndian: true.
	checkSum := fontData longAt: index+4 bigEndian: true.
	offset := (fontData longAt: index+8 bigEndian: true) + 1.
	length := fontData longAt: index+12 bigEndian: true.! !

!TTFontTableDirEntry class methodsFor: 'instance creation'!
on: fontData at: index

	^self new on: fontData at: index! !

!TTGlyph methodsFor: 'accessing'!
advanceWidth
	^advanceWidth! !

!TTGlyph methodsFor: 'accessing'!
advanceWidth: aNumber
	advanceWidth := aNumber.! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:19:48'!
boundsBottom
 ^boundsBottom
"
	boundsRight
	boundsTop
	boundsBottom
	"! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:19:28'!
boundsLeft
 ^boundsLeft
"
	boundsRight
	boundsTop
	boundsBottom
	"! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:16:39'!
boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bm
	boundsLeft _ bl.
	boundsRight _ br.
	boundsTop _ bt.
	boundsBottom _ bm.! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:19:35'!
boundsRight
 ^boundsRight
"
	boundsRight
	boundsTop
	boundsBottom
	"! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:19:43'!
boundsTop
 ^boundsTop
"
	boundsRight
	boundsTop
	boundsBottom
	"! !

!TTGlyph methodsFor: 'accessing'!
contours
	^contours! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 4/10/2019 09:41:20'!
contours: aCollection
	contours := aCollection asArray.! !

!TTGlyph methodsFor: 'accessing'!
glyphIndex
	^glyphIndex! !

!TTGlyph methodsFor: 'accessing'!
glyphIndex: anInteger
	glyphIndex := anInteger! !

!TTGlyph methodsFor: 'accessing'!
leftSideBearing
	^leftSideBearing! !

!TTGlyph methodsFor: 'accessing'!
leftSideBearing: aNumber
	leftSideBearing := aNumber.! !

!TTGlyph methodsFor: 'accessing'!
rightSideBearing
	^rightSideBearing! !

!TTGlyph methodsFor: 'accessing'!
rightSideBearing: aNumber
	rightSideBearing := aNumber.! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 5/16/2019 21:22:54'!
topSideBearing
	"
	https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html
	"
	^boundsBottom! !

!TTGlyph methodsFor: 'initialize-release' stamp: 'jmv 5/16/2019 21:23:03'!
initialize

	boundsLeft _ boundsRight _ boundsTop _ boundsBottom _ 0.
	contours := #().
	advanceWidth := 0.
	leftSideBearing := 0.
	rightSideBearing := 0.! !

!TTGlyph methodsFor: 'testing'!
isComposite
	^false! !

!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 5/16/2019 16:05:09'!
initializeContours: numContours with: endPoints
	"Initialize the contours for creation of the glyph."
	| startPt pts endPt |
	contours := Array new: numContours.
	startPt := -1.
	1 to: numContours do:[:i|
		endPt := endPoints at: i.
		pts := ShortPointArray new: endPt - startPt.
		contours at: i put: (TTContourConstruction on: pts).
		startPt := endPt].! !

!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 4/10/2019 09:41:12'!
makePointsAbsolute
	"Build the contours in the receiver glyph.
	The contour is constructed by converting the points
	form each contour into an absolute value and then
	compressing the contours into PointArrays."
	| contourOrigin |
	contourOrigin _ 0@0.
	contours do: [ :contour | 
		contourOrigin _ contour makePointsAbsolute: contourOrigin ]! !

!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 5/16/2019 21:21:51'!
updateRightSideBearing
	"Update the right side bearing value"
	"@@: Is the following really correct?!!?!!"
	rightSideBearing := advanceWidth - leftSideBearing - boundsRight + boundsLeft! !

!TTGlyph methodsFor: 'drawing' stamp: 'jmv 5/11/2019 16:54:15'!
drawGlyphOn: aVectorCanvas
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			contour initialMoveToCommand runOn: aVectorCanvas .
			contour pathCommand runOn: aVectorCanvas ]].! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'ar 11/2/1998 01:21'!
addGlyph: aGlyph transformation: aMatrix
	glyphs := glyphs copyWith: (aMatrix -> aGlyph)! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'yo 12/10/2007 17:15'!
basicGlyphs

	^ glyphs
! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'topa 3/10/2015 21:01'!
basicGlyphs: colOfAssocs

	glyphs := colOfAssocs
! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'ar 11/2/1998 01:20'!
glyphs

	^glyphs collect:[:assoc| assoc value].! !

!TTCompositeGlyph methodsFor: 'initialize' stamp: 'ar 11/2/1998 01:20'!
initialize
	glyphs := #().! !

!TTCompositeGlyph methodsFor: 'testing'!
isComposite
	^true! !

!TTCompositeGlyph methodsFor: 'drawing' stamp: 'jmv 5/11/2019 16:59:35'!
drawGlyphOn: aVectorCanvas
	| initialGeometry |
	initialGeometry _ aVectorCanvas engine geometryTransformation.
	glyphs do: [ :each |
		aVectorCanvas engine geometryTransformation: (initialGeometry composedWith: each key).
		each value drawGlyphOn: aVectorCanvas ]! !

!TTKernPair methodsFor: 'accessing'!
left
	^left! !

!TTKernPair methodsFor: 'accessing'!
left: aNumber

	left := aNumber! !

!TTKernPair methodsFor: 'accessing' stamp: 'ar 11/1/1998 20:08'!
mask
	^mask ifNil:[mask := self class maskFor: left with: right]! !

!TTKernPair methodsFor: 'accessing'!
right
	^right! !

!TTKernPair methodsFor: 'accessing'!
right: aNumber

	right := aNumber! !

!TTKernPair methodsFor: 'accessing'!
value
	^value! !

!TTKernPair methodsFor: 'accessing'!
value: aNumber

	value := aNumber! !

!TTKernPair class methodsFor: 'accessing'!
maskFor: left with: right
	^(left bitShift: 12) + right! !

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

!SVGPathMoveToCommand methodsFor: 'building' stamp: 'jmv 4/9/2019 13:36:26'!
toPoint: endPoint
	coordinatesAreRelative _ false.
	points _ {endPoint}! !

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

!SVGPathQuadraticCurveToCommand methodsFor: 'building' stamp: 'jmv 4/9/2019 13:30:05'!
toPoints: endPoints controlPoints: aControlPoints
	coordinatesAreRelative _ false.
	toPoints _ endPoints.
	controlPoints _ aControlPoints! !

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

!Color methodsFor: '*VectorGraphics' stamp: 'jmv 1/21/2015 12:48'!
externalizedWith: aMorphicLocation

	^self! !

!GrafPort methodsFor: '*VectorGraphics-TrueType' stamp: 'len 5/28/2019 15:20:31'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer last affected pixel position"
	| fdestX char glyph maxHeight ddy toPixelsScale |

	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	startIndex to: stopIndex do: [ :charIndex |
		char _ aString at: charIndex.
		glyph _ aTrueTypeFont glyphAt: char.
		destX _ (fdestX + glyph leftOffset) rounded.
		self revisar.
		ddy _ aTrueTypeFont fontDescription ascent + aTrueTypeFont fontDescription lineGap.
		toPixelsScale _ aTrueTypeFont pointSize * 1.0 / aTrueTypeFont fontDescription letterMTopSideBearing.
		destY _ aPoint y + (ddy * toPixelsScale) - glyph topOffset. 
		aTrueTypeFont isSuperscript ifTrue: [destY _ destY - (aTrueTypeFont ascent*0.55)] ifFalse: [aTrueTypeFont isSubscript ifTrue: [destY _ destY + (aTrueTypeFont ascent*0.3)]].
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height.
	].
	^ destX ceiling@(destY+maxHeight)! !

!GrafPort methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 5/21/2019 10:56:45'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer last affected pixel position
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	"self installStrikeFont: font foregroundColor: (foregroundColor alpha: 1)."
	sourceForm _ nil. "Set it for each glyph"
	sourceX _ 0.
	sourceY _ 0.
	height _ nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !
SVGElementMorph initialize!
VectorCanvas initialize!
TTFontDescription initialize!
TTFontReader initialize!
