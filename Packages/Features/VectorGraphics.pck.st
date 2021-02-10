'From Cuis 5.0 [latest update: #4531] on 10 February 2021 at 5:30:37 pm'!
'Description '!
!provides: 'VectorGraphics' 1 189!
!requires: 'Collections-CompactArrays' 1 10 nil!
SystemOrganization addCategory: 'VectorGraphics-Kernel'!
SystemOrganization addCategory: 'VectorGraphics-PathCommands'!
SystemOrganization addCategory: 'VectorGraphics-TrueType'!
SystemOrganization addCategory: 'VectorGraphics-Experiments'!
SystemOrganization addCategory: 'VectorGraphics-Examples'!


!classDefinition: #PathCommandError category: 'VectorGraphics-PathCommands'!
Error subclass: #PathCommandError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathCommandError class' category: 'VectorGraphics-PathCommands'!
PathCommandError class
	instanceVariableNames: ''!

!classDefinition: #TrueTypeFont category: 'VectorGraphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphForms glyphFormsByUtf8 family baseFont derivativeFonts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TrueTypeFont class' category: 'VectorGraphics-TrueType'!
TrueTypeFont class
	instanceVariableNames: ''!

!classDefinition: #TrueTypeFontFamily category: 'VectorGraphics-TrueType'!
FontFamily subclass: #TrueTypeFontFamily
	instanceVariableNames: 'baseTTFontDescription ttFontDescriptionsByEmphasis'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TrueTypeFontFamily class' category: 'VectorGraphics-TrueType'!
TrueTypeFontFamily class
	instanceVariableNames: ''!

!classDefinition: #M3Exp01Morph category: 'VectorGraphics-Experiments'!
MovableMorph subclass: #M3Exp01Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp01Morph class' category: 'VectorGraphics-Experiments'!
M3Exp01Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp02Morph category: 'VectorGraphics-Experiments'!
MovableMorph subclass: #M3Exp02Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp02Morph class' category: 'VectorGraphics-Experiments'!
M3Exp02Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp03Morph category: 'VectorGraphics-Experiments'!
MovableMorph subclass: #M3Exp03Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp03Morph class' category: 'VectorGraphics-Experiments'!
M3Exp03Morph class
	instanceVariableNames: ''!

!classDefinition: #M3Exp04Morph category: 'VectorGraphics-Experiments'!
MovableMorph subclass: #M3Exp04Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Experiments'!
!classDefinition: 'M3Exp04Morph class' category: 'VectorGraphics-Experiments'!
M3Exp04Morph class
	instanceVariableNames: ''!

!classDefinition: #HybridCanvas category: 'VectorGraphics-Kernel'!
BitBltCanvas subclass: #HybridCanvas
	instanceVariableNames: 'vectorCanvas morphIDsForm drawingOnMorphIDs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'HybridCanvas class' category: 'VectorGraphics-Kernel'!
HybridCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorCanvas category: 'VectorGraphics-Kernel'!
MorphicCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderEngine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorCanvas class' category: 'VectorGraphics-Kernel'!
VectorCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineAbstract category: 'VectorGraphics-Kernel'!
Object subclass: #VectorEngineAbstract
	instanceVariableNames: 'targetForm morphIds targetWidth antiAliasingWidth strokeWidth geometryTransformation currentMorphId currentClipsSubmorphs clipCurrentMorph morphBoundsLeft morphBoundsTop morphBoundsRight morphBoundsBottom hopsPerPixel pathFirstX pathFirstY pathCurrentX pathCurrentY previousCallX previousCallY previousCallControlX previousCallControlY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineAbstract class' category: 'VectorGraphics-Kernel'!
VectorEngineAbstract class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineBoundsFinder category: 'VectorGraphics-Kernel'!
VectorEngineAbstract subclass: #VectorEngineBoundsFinder
	instanceVariableNames: 'auxStrokeWidthDilatedHalf'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineBoundsFinder class' category: 'VectorGraphics-Kernel'!
VectorEngineBoundsFinder class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineSubPixel category: 'VectorGraphics-Kernel'!
VectorEngineAbstract subclass: #VectorEngineSubPixel
	instanceVariableNames: 'edgeCounts alphaMask blendLeft blendTop blendRight blendBottom strokeColor fillColor clipLeft clipTop clipRight clipBottom prevYTruncated auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineSubPixel class' category: 'VectorGraphics-Kernel'!
VectorEngineSubPixel class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineWholePixel category: 'VectorGraphics-Kernel'!
VectorEngineAbstract subclass: #VectorEngineWholePixel
	instanceVariableNames: 'edgeCounts alphaMask blendLeft blendTop blendRight blendBottom strokeColor fillColor clipLeft clipTop clipRight clipBottom prevYTruncated auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineWholePixel class' category: 'VectorGraphics-Kernel'!
VectorEngineWholePixel class
	instanceVariableNames: ''!

!classDefinition: #PathCommand category: 'VectorGraphics-PathCommands'!
Object subclass: #PathCommand
	instanceVariableNames: 'coordinatesAreRelative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathCommand class' category: 'VectorGraphics-PathCommands'!
PathCommand class
	instanceVariableNames: ''!

!classDefinition: #PathArcCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathArcCommand
	instanceVariableNames: 'rs xAxisRotations toPoints doLargerArcs useIncreasingAngless'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathArcCommand class' category: 'VectorGraphics-PathCommands'!
PathArcCommand class
	instanceVariableNames: ''!

!classDefinition: #PathCloseCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathCloseCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathCloseCommand class' category: 'VectorGraphics-PathCommands'!
PathCloseCommand class
	instanceVariableNames: ''!

!classDefinition: #PathCurveToCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathCurveToCommand
	instanceVariableNames: 'toPoints control1Points control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathCurveToCommand class' category: 'VectorGraphics-PathCommands'!
PathCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #PathCurveToSmoothCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathCurveToSmoothCommand
	instanceVariableNames: 'toPoints control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathCurveToSmoothCommand class' category: 'VectorGraphics-PathCommands'!
PathCurveToSmoothCommand class
	instanceVariableNames: ''!

!classDefinition: #PathLineToCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathLineToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathLineToCommand class' category: 'VectorGraphics-PathCommands'!
PathLineToCommand class
	instanceVariableNames: ''!

!classDefinition: #PathLineToHCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathLineToHCommand
	instanceVariableNames: 'xs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathLineToHCommand class' category: 'VectorGraphics-PathCommands'!
PathLineToHCommand class
	instanceVariableNames: ''!

!classDefinition: #PathLineToVCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathLineToVCommand
	instanceVariableNames: 'ys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathLineToVCommand class' category: 'VectorGraphics-PathCommands'!
PathLineToVCommand class
	instanceVariableNames: ''!

!classDefinition: #PathMoveToCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathMoveToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathMoveToCommand class' category: 'VectorGraphics-PathCommands'!
PathMoveToCommand class
	instanceVariableNames: ''!

!classDefinition: #PathQuadraticCurveToCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathQuadraticCurveToCommand
	instanceVariableNames: 'toPoints controlPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathQuadraticCurveToCommand class' category: 'VectorGraphics-PathCommands'!
PathQuadraticCurveToCommand class
	instanceVariableNames: ''!

!classDefinition: #PathQuadraticCurveToSmoothCommand category: 'VectorGraphics-PathCommands'!
PathCommand subclass: #PathQuadraticCurveToSmoothCommand
	instanceVariableNames: 'toPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-PathCommands'!
!classDefinition: 'PathQuadraticCurveToSmoothCommand class' category: 'VectorGraphics-PathCommands'!
PathQuadraticCurveToSmoothCommand class
	instanceVariableNames: ''!

!classDefinition: #FormGlyph category: 'VectorGraphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'FormGlyph class' category: 'VectorGraphics-TrueType'!
FormGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTContourConstruction category: 'VectorGraphics-TrueType'!
Object subclass: #TTContourConstruction
	instanceVariableNames: 'points controlPointFlags'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTContourConstruction class' category: 'VectorGraphics-TrueType'!
TTContourConstruction class
	instanceVariableNames: ''!

!classDefinition: #TTFontDescription category: 'VectorGraphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForIso8859s15 contourDataIndexesByIso8859s15 contourDataIndexesForSmalltalkGlyphs contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontDescription class' category: 'VectorGraphics-TrueType'!
TTFontDescription class
	instanceVariableNames: ''!

!classDefinition: #TTFontReader category: 'VectorGraphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'glyphs nGlyphs kernPairs fontDescription'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontReader class' category: 'VectorGraphics-TrueType'!
TTFontReader class
	instanceVariableNames: ''!

!classDefinition: #TTFontTableDirEntry category: 'VectorGraphics-TrueType'!
Object subclass: #TTFontTableDirEntry
	instanceVariableNames: 'tag fontData offset length checkSum'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontTableDirEntry class' category: 'VectorGraphics-TrueType'!
TTFontTableDirEntry class
	instanceVariableNames: ''!

!classDefinition: #TTGlyph category: 'VectorGraphics-TrueType'!
Object subclass: #TTGlyph
	instanceVariableNames: 'boundsLeft boundsRight boundsTop boundsBottom contours advanceWidth leftSideBearing rightSideBearing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTGlyph class' category: 'VectorGraphics-TrueType'!
TTGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTCompositeGlyph category: 'VectorGraphics-TrueType'!
TTGlyph subclass: #TTCompositeGlyph
	instanceVariableNames: 'glyphs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTCompositeGlyph class' category: 'VectorGraphics-TrueType'!
TTCompositeGlyph class
	instanceVariableNames: ''!

!classDefinition: #TTKernPair category: 'VectorGraphics-TrueType'!
Object subclass: #TTKernPair
	instanceVariableNames: 'left right value mask'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTKernPair class' category: 'VectorGraphics-TrueType'!
TTKernPair class
	instanceVariableNames: ''!


!TrueTypeFont commentStamp: '<historical>' prior: 0!
TrueType fonts are the most widely used format for pixel independent vector Fonts.

In Cuis what is usually called TrueType Font is represented by TTFontDescription.

TrueTypeFonts, being part of the traditional AbstractFont hierarchy, adds a specific pointSize, and is the object used in texts.!

!M3Exp01Morph commentStamp: '<historical>' prior: 0!
"Just an experiment to exercise drawing operations."
| m |
m _ M3Exp01Morph new.
m morphPosition: 15@30.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.!

!M3Exp02Morph commentStamp: '<historical>' prior: 0!
"Just an experiment to exercise drawing operations."

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
"Just an experiment to exercise drawing operations."

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

!M3Exp04Morph commentStamp: '<historical>' prior: 0!
"Just an experiment to exercise drawing operations."
| m |
m _ M3Exp01Morph new.
m morphPosition: 15@30.
(VectorCanvas onForm: Display) fullDraw: m.
Display forceToScreen.!

!HybridCanvas commentStamp: '<historical>' prior: 0!
A HybridCanvas works as a BitBltCanvas until faced with a situation that BitBlitCanvas can't handle. At that point, the vectorCanvas takes command.

self runningWorld setCanvas: (HybridCanvas onForm: Display).
self runningWorld canvas class!

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

!VectorEngineAbstract commentStamp: 'jmv 6/6/2020 17:28:45' prior: 0!
A Vector Graphics rasterizer for Morphic 3.
Supports most SVG primitives.
Gives exceptional quality.

(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.

(VectorEngineSubPixel onForm: Display)
	fillColor: Color red do: [ :engine |
		engine abs_EllipseCenterX: 100 y: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen!

!VectorEngineBoundsFinder commentStamp: '<historical>' prior: 0!
Morph Bounds Finder!

!VectorEngineWholePixel commentStamp: '<historical>' prior: 0!
Does only whole pixel anti aliasing. Appropriate for very high resolution displays.
edgeCounts and alphaMask are single byte per pixel.!

!FormGlyph commentStamp: '<historical>' prior: 0!
Needed only for BitBltCanvasEngine

https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html

leftSideBearing, topSideBearing, advanceWidth are in (possibly fractional) pixels.!

!TTContourConstruction commentStamp: '<historical>' prior: 0!
This class represents a temporary contour structure during the construction of a TTGlyph from a TrueType file.

Instance variables:
	points				<ShortPointArray>	The points defining this contour
	controlPointFlags	<BitArray (ByteArray with bit content)> Flags for each point!

!TTFontDescription commentStamp: '<historical>' prior: 0!
Holds a TrueType font in memory.

See
https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html!

!TTFontReader commentStamp: '<historical>' prior: 0!
TTFontReader builds a TTFontDescription from a TrueType font file (.ttf).!

!TTFontTableDirEntry commentStamp: '<historical>' prior: 0!
This class represents an entry in a truetype font table directory. Used by TTFontReader only.!

!TTGlyph commentStamp: '<historical>' prior: 0!
This class represents a glyph of a TrueType font.

Instance variables:
	bounds					<Rectangle>	The receiver's bounds
	contours					<Array of: TTContourConstruction>
	advanceWidth			<Integer>	advance width of the glyph
	leftSideBearing  		<Integer>	left side bearing
	rightSideBearing  	<Integer>	right side bearing!

!TTCompositeGlyph commentStamp: '<historical>' prior: 0!
This class represents a composite TrueType glyph, e.g. one which contains many simple TTGlyphs.!

!TTKernPair commentStamp: '<historical>' prior: 0!
A TTKernPair represents a TrueType kerning pair.

Instance variables:
	left	<Integer>	The glyph index for the left character.
	right <Integer>	The glyph index for the right character.
	value <Integer>	The amount of kerning.
	mask <Integer>	An efficient representation for the left and the right value.!

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

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 5/7/2019 09:43:20'!
addDerivativeFont: aTrueTypeFont
	derivativeFonts at: aTrueTypeFont emphasis put: aTrueTypeFont! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 5/11/2019 17:13:08'!
baseFont: aStrikeFont

	baseFont _ aStrikeFont.
	baseFont ifNotNil: [
		derivativeFonts _ nil ]! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 3/22/2020 16:15:49'!
family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
	ttFontDescription _ aTTFontDescription.
	emphasis _ aTTFontDescription emphasis.
	pointSize _ aNumber.
	glyphForms _ Array new: 256.
	glyphFormsByUtf8 _ Array new: 256.
	family _ aTrueTypeFontFamily.
	baseFont _ nil.
	derivativeFonts _ emphasis = 0 ifTrue: [Dictionary new].! !

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 3/21/2020 10:22:19'!
resetAssignmentGlyph
	"Uses ST80 glyphs:"
	(emphasis anyMask: 64) ifTrue: [
		"Re-cache glyphs that could have changed"
		glyphForms at: $_ numericValue + 1 put: nil ].! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 3/2/2020 17:25:54'!
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

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 3/2/2020 17:26:04'!
descent
	^ttFontDescription descent negated * pointSize / ttFontDescription letterMTopSideBearing * 1.0! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/4/2019 15:26:55'!
familyName
	^ ttFontDescription familyName ! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:56:44'!
fontDescription
	^ ttFontDescription ! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 3/2/2020 17:25:42'!
lineGap
	^ttFontDescription lineGap * pointSize / ttFontDescription letterMTopSideBearing * 1.0! !

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

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 4/1/2020 11:47:08'!
widthOf: aCharacter

	^ (aCharacter widthInTrueTypeFontDescription: ttFontDescription useST80Glyphs: self isST80Glyphs) * pointSize! !

!TrueTypeFont methodsFor: 'displaying' stamp: 'jmv 4/1/2020 12:27:51'!
onBitBltCanvasEngine: engine displayString: aString from: startIndex to: stopIndex at: p color: color
	"Answer last affected pixel position.
	Answer nil if nothing was done."

	^aString displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p trueTypeFont: self color: color! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/11/2019 17:10:59'!
isBaseFont
	^baseFont isNil! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/4/2019 15:28:52'!
isBold
	^self emphasis allMask: 1! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 5/4/2019 15:28:59'!
isItalic
	^self emphasis allMask: 2! !

!TrueTypeFont methodsFor: 'testing' stamp: 'jmv 1/26/2020 09:45:41'!
isST80Glyphs
	^emphasis allMask: 64! !

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

!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 3/17/2020 22:59:51'!
glyphAt: aCharacter
	"Needed only for BitBltCanvasEngine"

	| index |
	index _ aCharacter numericValue + 1.
	^ (glyphForms at: index) ifNil: [
		| formGlyph |
		formGlyph _ (ttFontDescription formGlyphFor: aCharacter useST80Glyphs: self isST80Glyphs pixelSize: pointSize)
			ifNil: [#absentGlyph]
			ifNotNil: [ :theGlyph |
				(emphasis allMask: 4) ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				(emphasis allMask: 8) ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		glyphForms at: index put: formGlyph.
		formGlyph ]! !

!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 3/22/2020 16:25:02'!
glyphAtUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil
	"Needed only for BitBltCanvasEngine"

	| lastArray lastIndex |
	lastArray _ glyphFormsByUtf8 .
	lastIndex _ byte1 + 1.
	byte2OrNil notNil ifTrue: [
		(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
		lastArray _ lastArray at: lastIndex.
		lastIndex _ (byte2OrNil bitAnd: 63) + 1.
		byte3OrNil notNil ifTrue: [
			(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
			lastArray _ lastArray at: lastIndex.
			lastIndex _ (byte3OrNil bitAnd: 63) + 1.
			byte4OrNil notNil ifTrue: [
				(lastArray at: lastIndex) ifNil: [ lastArray at: lastIndex put: (Array new: 64) ].
				lastArray _ lastArray at: lastIndex.
				lastIndex _ (byte4OrNil bitAnd: 63) + 1 ]]].
	^ (lastArray at: lastIndex) ifNil: [
		| formGlyph |
		formGlyph _ (ttFontDescription formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil
								byte3: byte3OrNil byte4: byte4OrNil pixelSize: pointSize)
			ifNil: [#absentGlyph]
			ifNotNil: [ :theGlyph |
				(emphasis allMask: 4) ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				(emphasis allMask: 8) ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		lastArray at: lastIndex put: formGlyph.
		formGlyph ]! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 1/26/2020 16:07:11'!
addEmphasis: code
	"Set the integer code for emphasis. See senders."

	emphasis _ code.
	(emphasis anyMask: 4+8+64) ifTrue: [
		"They need to be recreated"
		glyphForms _ Array new: 256 ].! !

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

!TrueTypeFont methodsFor: 'swapping glyphs' stamp: 'jmv 5/11/2019 17:18:55'!
useShortUnderscore! !

!TrueTypeFont methodsFor: 'as yet unclassified' stamp: 'len 5/25/2020 10:25:39'!
releaseCachedState
	glyphForms atAllPut: nil.
	glyphFormsByUtf8 atAllPut: nil.
	derivativeFonts notNil ifTrue: [
		derivativeFonts keysAndValuesDo: [ :anEmphasis :derivedFont |
			derivedFont releaseCachedState  ]]! !

!TrueTypeFont class methodsFor: 'instance creation' stamp: 'jmv 5/7/2019 09:25:01'!
family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber
	^ self new family: aTrueTypeFontFamily ttFontDescription: aTTFontDescription pointSize: aNumber! !

!TrueTypeFont class methodsFor: 'initialization' stamp: 'jmv 3/21/2020 10:22:09'!
assignmentGlyphSelectorPreferenceChanged
	| selector |
	selector _ Preferences assignmentGlyphSelector.
	TTFontDescription allInstancesDo: [ :ttFontDescription | ttFontDescription perform: selector ].
	self allInstancesDo: [ :each | each resetAssignmentGlyph ]! !

!TrueTypeFontFamily methodsFor: 'initialization' stamp: 'jmv 5/7/2019 09:13:51'!
addTTFontDescription: aTTFontDescription
	ttFontDescriptionsByEmphasis at: aTTFontDescription emphasis put: aTTFontDescription.! !

!TrueTypeFontFamily methodsFor: 'initialization' stamp: 'jmv 6/16/2020 20:13:35'!
baseTTFontDescription: aTTFontDescription
	self assert: aTTFontDescription emphasis = 0.
	familyName _ aTTFontDescription familyName.
	baseFontBySizes _ Dictionary new.
	baseTTFontDescription _ aTTFontDescription.
	ttFontDescriptionsByEmphasis isNil ifTrue: [ ttFontDescriptionsByEmphasis _ Dictionary new ].! !

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

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 6/16/2020 18:33:55'!
folderName
	^baseTTFontDescription folderName! !

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:23:22'!
pointSizes
	^#(6 7 8 9 10 11 12 13 14 15 18 22 26 36 48 72)! !

!TrueTypeFontFamily methodsFor: 'testing' stamp: 'jmv 8/3/2020 19:01:56'!
isTrueTypeFontFamily
	^true! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 5/7/2019 09:01:43'!
baseTTFontDescription: aTTFontDescription
	^ self new baseTTFontDescription: aTTFontDescription! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'KenD 4/22/2020 14:29:05'!
fileReaderServicesForFileEntry: filename suffix: suffix
	"FileList buttons that read a TrueType font on contents."

	(#( 'ttf' ) includes: suffix) ifTrue: [
		^ { self serviceReadTTFont } ].

	^#()! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 6/14/2020 01:04:18'!
read: aDirectoryEntry
	"
	TrueTypeFontFamily read: '/home/juan/Downloads/DejaVu2.37/dejavu-fonts-ttf-2.37/ttf/' asDirectoryEntry
	"
	| readFamilyNames |
	readFamilyNames _ TTFontDescription read: aDirectoryEntry.
	readFamilyNames do: [ :familyName |
		| ttDescriptions baseTTDescription family |
		ttDescriptions _ TTFontDescription withFamilyName: familyName.
		baseTTDescription _ ttDescriptions at: 0.
		family _ TrueTypeFontFamily baseTTFontDescription: baseTTDescription.
		ttDescriptions do: [ :ttfDesc |
			ttfDesc emphasis = 0 ifFalse: [
				family addTTFontDescription: ttfDesc ]].
		FontFamily addFamily: family ].
	^readFamilyNames! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 6/14/2020 01:02:48'!
readAdditionalFonts

	^TrueTypeFontFamily read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 6/18/2020 15:32:05'!
readTrueTypeFontEntry: ttFontFileEntry

	| ttFontDescription familyName ttFontFamily |
	ttFontDescription := TTFontReader readTTFFrom: ttFontFileEntry binaryContents.
	familyName := ttFontDescription familyName.
	ttFontFamily := FontFamily familyNamed: familyName.
	ttFontFamily
		ifNil: [ FontFamily addFamily: (self new baseTTFontDescription: ttFontDescription ) ]
		ifNotNil: [ :fam |
			ttFontDescription emphasis = 0
				ifTrue: [fam baseTTFontDescription: ttFontDescription]
				ifFalse: [fam addTTFontDescription: ttFontDescription].
			fam clearCache ].
	familyName = FontFamily defaultFamilyName ifTrue: [
		Preferences setDefaultFont:	 familyName ].
	PopUpMenu inform: 'loaded font ', familyName.! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'KenD 4/22/2020 13:52:56'!
serviceReadTTFont
	"Answer the service of reading a TrueType font"

	^ (SimpleServiceEntry
		provider: self 
		label: 'load font'
		selector: #readTrueTypeFontEntry:
		description: 'load TrueType font'
		buttonLabel: 'read font'
		icon: ((Theme content from: 'Theme' get: {'16x16'. 'mimetypes'}) at: 'font-x-generic.png')
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

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

!M3Exp02Morph methodsFor: 'drawing' stamp: 'jmv 12/11/2017 21:08:39'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas strokeWidth: 15 color: (Color green alpha: 0.8) fillColor: (Color red alpha: 0.6) do: [
		aCanvas
			roundRectangleMinX: -10 maxX: 80 minY: -10 maxY: 60  rx: 40 ry: 20 ]! !

!M3Exp02Morph methodsFor: 'geometry testing' stamp: 'jmv 11/16/2020 22:41:10'!
clipsSubmorphs
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance"

	^ true! !

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

!M3Exp04Morph methodsFor: 'drawing' stamp: 'jmv 1/3/2021 12:11:47'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."



aCanvas image: ImageMorph defaultForm at: -10@ -10.

aCanvas strokeWidth: 5 color: (Color green alpha: 0.6) fillColor: (Color red alpha: 0.3) do: [
	aCanvas roundRectangleMinX: -10 maxX: 75 minY: -10 maxY: 47 rx: 40 ry: 20 ].

! !

!HybridCanvas methodsFor: 'drawing-images' stamp: 'jmv 11/30/2020 16:12:07'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency."
	| p e |

	engine ifNil: [ ^nil ].
	drawingOnMorphIDs ifTrue: [
		"We transform p but not e. This is on purpose. We want to affect exactly the same rect as super."
		e _ sourceRect extent.
		e > `0@0` ifFalse: [ ^self ].
		p _ (currentTransformation transform: aPoint) rounded.
		engine combinationRule: 0.
		engine fillRect: (p extent: e).
		^self ].

	super image: aForm at: aPoint sourceRect: sourceRect! !

!HybridCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/30/2020 16:12:00'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	| rect |
	engine ifNil: [ ^nil ].

	drawingOnMorphIDs ifTrue: [
		aRectangle area = 0 ifTrue: [ ^self ].
		rect _ (currentTransformation externalizeRectangle: aRectangle) rounded.
		engine combinationRule: 0.
		engine fillRect: rect.
		^self ].

	super fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/30/2020 12:10:02'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:27:46'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: fontOrNil color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: fontOrNil color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/21/2020 17:42:05'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: fontOrNil color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: fontOrNil color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/30/2020 12:12:22'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 11/30/2020 12:12:26'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc! !

!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 10/24/2020 16:04:17'!
world: aWorldMorph
	super world: aWorldMorph.
	vectorCanvas world: aWorldMorph! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 7/11/2020 09:55:52'!
canvasToUse

	currentMorph requiresVectorCanvas ifTrue: [
		^ vectorCanvas ].
	^self.! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 11/17/2020 14:51:02'!
clipCurrentMorph: aBoolean
	super clipCurrentMorph: aBoolean.
	vectorCanvas clipCurrentMorph: aBoolean! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 11/30/2020 12:28:51'!
onMorphIdsDo: aBlock
	
	"
	QQQ _ DateAndTime now.
	"
	engine instVarNamed: 'destForm' put: morphIDsForm.
	drawingOnMorphIDs _ true.
	aBlock value.
	engine instVarNamed: 'destForm' put: form.
	drawingOnMorphIDs _ false.! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 7/11/2020 10:55:26'!
setClipRect: aRectangle
	super setClipRect: aRectangle.
	vectorCanvas setClipRect: aRectangle! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 11/30/2020 10:30:45'!
setForm: aForm
	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas _ VectorCanvas onForm: form.
	morphIDsForm _ Form extent: form extent depth: 32 bits: (vectorCanvas engine instVarNamed: 'morphIds').
	drawingOnMorphIDs _ false.
	self newClipRect: nil.! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 11/30/2020 16:11:47'!
setPaintColor: aColor

	drawingOnMorphIDs ifTrue: [
		engine sourceForm: nil.
		engine combinationRule: 0.
		^self ].

	super setPaintColor: aColor! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 7/25/2020 08:26:28'!
updatingMorphBoundsDo: aBlock

	| prevEngine |
	prevEngine _ engine.
	[
		engine _ nil.
		vectorCanvas updatingMorphBoundsDo: aBlock
	] ensure: [ engine _ prevEngine ]! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 8/3/2020 17:03:44'!
canDraw: aMorph
	^true! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 1/4/2021 17:22:17'!
currentMorphDrawingFails

	| c |
	c _ self canvasToUse.
	c == self ifFalse: [ ^c currentMorphDrawingFails ].
	super currentMorphDrawingFails! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 1/13/2021 11:11:53'!
drawCurrentAndSubmorphs
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	| b |
	self isCurrentMorphVisible ifTrue: [

		currentMorph drawOn: self.
		self onMorphIdsDo: [
			currentMorph drawOn: self ].

		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph halo ifNotNil: [ b _ currentMorph morphLocalBounds ].
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].
		
		currentMorph postDrawOn: self.
		self onMorphIdsDo: [
			currentMorph postDrawOn: self ].
		b ifNotNil: [
			self drawCoordinateSystem: b ].
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].

		currentMorph isHighlighted ifTrue: [
			self drawCurrentMorphHighlight ].
	].! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/9/2020 16:36:45'!
into: aMorph
	super into: aMorph.
	vectorCanvas into: aMorph! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/13/2020 16:51:29'!
morph: aMorph isAtPoint: aPoint

	^aMorph requiresVectorCanvas
		ifTrue: [vectorCanvas morph: aMorph isAtPoint: aPoint]
		ifFalse: [super morph: aMorph isAtPoint: aPoint]! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/9/2020 16:36:59'!
outOfMorph
	super outOfMorph.
	vectorCanvas outOfMorph! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/26/2020 20:25:44'!
restorePatch: savedPatch
	"Argument must previously be obtained by calling #savePatch:bounds:
	Senders should not assume anything about the returned object, except that it will not be nil."

	vectorCanvas restorePatch: savedPatch! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/26/2020 20:25:21'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	^ vectorCanvas savePatch: prevSavedPatch bounds: aRectangle! !

!HybridCanvas methodsFor: 'morphic world' stamp: 'jmv 11/30/2020 12:35:28'!
drawWorldBackground: aWorldMorph rects: worldBackgroundDamage
	"Draw worldBackgroundDamage rects for aWorldMorph.
	Do not include submorphs."
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	worldBackgroundDamage do: [ :r |
		self newClipRect: r.
		aWorldMorph drawOn: self.
		self onMorphIdsDo: [
			aWorldMorph drawOn: self ]].! !

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

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 5/9/2020 19:11:27'!
curveToX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2

	engine abs_CubicBezierToX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 1/1/2018 11:43:43'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry

	engine abs_EllipseCenterX: mcx y: mcy radiusX: mrx radiusY: mry! !

!VectorCanvas methodsFor: 'paths' stamp: 'jmv 5/9/2020 19:15:50'!
lineToX: xTo y: yTo

	engine abs_LineToX: xTo y: yTo! !

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

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/21/2020 17:38:40'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"
| s c f |
c _ VectorCanvas onForm: Display.
c engine geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
c drawString:  s from: 1 to: s size at: 100@100 font: f color: Color black.
c strokeWidth: 1 color: Color red do: [ c moveToX: 100 y: 100; lineToX: 1000 y: 100 ].
Display forceToScreen
	"
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	| font |
	font _ self fontToUse: fontOrNil.
	^font
		ifNil: [ aPoint ] 
		ifNotNil: [
			engine
				displayString: aString
				from: firstIndex
				to: lastIndex
				at: aPoint + (0@(font ascent + font lineGap-1))
				trueTypeFont: font fontDescription
				useST80Glyphs: font isST80Glyphs
				color: aColor
				scale: font pointSize
				preventMirroring: true ]
		
	"The conversion from top of glyph (morphic canvas) to baseline (engine, TrueType) is done here."! !

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/21/2020 17:39:25'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: fontOrNil color: aColor
	"
| s c f |
c _ VectorCanvas onForm: Display.
c engine geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
c drawString:  s from: 1 to: s size atBaseline: 100@100 font: f color: Color black.
c strokeWidth: 1 color: Color red do: [ c moveToX: 100 y: 100; lineToX: 1000 y: 100 ].
Display forceToScreen
	"
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	| font |
	font _ self fontToUse: fontOrNil.
	^font
		ifNil: [ aPoint ] 
		ifNotNil: [
			engine
				displayString: aString
				from: firstIndex
				to: lastIndex
				at: aPoint
				trueTypeFont: font fontDescription
				useST80Glyphs: font isST80Glyphs
				color: aColor
				scale: font pointSize
				preventMirroring: true ].! !

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/21/2020 17:43:39'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: fontOrNil color: aColor
	"
| s c f |
c _ VectorCanvas onForm: Display.
c engine geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
c drawString:  s from: 1 to: s size atWaist: 100@100 font: f color: Color black.
c strokeWidth: 1 color: Color red do: [ c moveToX: 100 y: 100; lineToX: 1000 y: 100 ].
Display forceToScreen
	"
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	| font |
	font _ self fontToUse: fontOrNil.
	^font
		ifNil: [ aPoint ] 
		ifNotNil: [
			engine
				displayString: aString
				from: firstIndex
				to: lastIndex
				at: aPoint + (0@(font ascent / 3))
				trueTypeFont: font fontDescription
				useST80Glyphs: font isST80Glyphs
				color: aColor
				scale: font pointSize
				preventMirroring: true ]
		
	"The conversion from top of glyph (morphic canvas) to baseline (engine, TrueType) is done here."! !

!VectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/2/2020 16:18:58'!
drawUtf8: aByteArray at: aPoint font: aTrueTypeFont color: aColor
	"
| c f |
Preferences loadOnlyLatinGlyphData ifTrue: [
	Preferences disable: #loadOnlyLatinGlyphData. TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf' ].
c _ VectorCanvas onForm: Display.
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
c drawString: 'Hola' at: 10@10 font: f color: Color random.
c drawUtf8: #[208 176 208 177 208 178 208 179 208 180 208 181 209 145 208 182 208 183 208 184 208 185 208 186 208 187 208 188 208 189 208 190 209 143 209 142 209 141 209 140 209 139 209 138 209 137 209 136 209 135 209 134 209 133 209 132 209 131 209 130 209 129 209 128 208 191] at: 10@100 font: f color: Color random.
c drawUtf8:  #[208 146 208 190 209 130 32 208 184 208 183 32 208 181 208 189 32 208 184 208 179 208 183 208 176 208 188 208 191 208 187 32 208 190 209 132 32 208 181 208 189 32 208 152 208 189 208 179 208 187 208 184 209 136 32 209 130 208 181 208 186 209 129 209 130 32 209 128 208 184 209 130 208 189 32 208 184 208 189 32 208 180 32 208 161 208 184 209 128 208 184 208 187 208 184 208 186 32 208 176 208 187 209 132 208 176 208 177 208 181 209 130 63]  at: 10@200 font: f color: Color random.
c drawString: 'año' at: 10@300 font: f color: Color random.
c drawUtf8:  #[97 195 177 111 ]  at: 10@400 font: f color: Color random.
Display forceToScreen
	"
	"Slight optimization when there's nothing to do."
	"
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	"
	| font |
	font _ aTrueTypeFont ifNil: [ FontFamily defaultFamilyAndPointSize ].
	engine
		displayUtf8: aByteArray
		at: aPoint + (0@font ascent + font lineGap-1)
		trueTypeFont: font fontDescription
		color: aColor
		scaleX: 1.0 * font pointSize
		scaleY: -1.0 * font pointSize
		
	"The conversion from top of glyph (morphic canvas) to baseline (engine, TrueType) is done here.
	The conversion from Y-increases-downwards (morphic canvas) to Y-increases-upwards (engine, TrueType) is done here.
	
	The scaling of internal TrueType integer points to FloatingPoint where[0.0...1.0] equals #letterMTopSideBearing is done in the engine.
	It should actually be done in TrueType. No one else cares!!"! !

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

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/11/2017 21:15:33'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: morphStrokeWidth borderColor: strokeColorOrGradient fillColor: fillColorOrGradient

	
	self strokeWidth: morphStrokeWidth color: strokeColorOrGradient fillColor: fillColorOrGradient do: [
		self ellipseCenterX: mcx y: mcy rx: mrx ry: mry ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/10/2017 22:59:51'!
fillRectangle: aRectangle color: aColor
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"

	self fillColor: aColor do: [
		self rectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 11/17/2020 09:37:10'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	aSymbol = #simple ifTrue: [
		^ self frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: baseColorForBorder ].
	
	super fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 6/23/2020 12:22:22'!
frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: borderColor
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"

	self strokeWidth: borderWidth color: borderColor fillColor: aColor do: [
		self rectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/11/2017 20:57:09'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	| d1 d2 |
	d1 _ borderWidth*0.5-0.5.
	d2 _ borderWidth*0.5+0.5.

	self strokeWidth: borderWidth color: borderColor do: [
		self rectangleMinX: aRectangle left+d1 maxX: aRectangle right-d2 minY: aRectangle top+d1 maxY: aRectangle bottom-d2 ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 11/9/2020 15:24:57'!
frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: morphStrokeWidth
	"For BitBltCanvas protocol, aRectangle:
	- means the bounding rectangle of the affected pixels, not the rectangle followed by the tip
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	"
	(VectorCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| d1 d2 |
	d1 _ morphStrokeWidth*0.5-0.5.
	d2 _ morphStrokeWidth*0.5+0.5.

	self strokeWidth: morphStrokeWidth color: brColor do: [
		self moveToX: aRectangle left + d1 y: aRectangle bottom - d2.
		self lineToX: aRectangle right - d2 y: aRectangle bottom - d2.
		self lineToX: aRectangle right - d2 y: aRectangle top + d1 ].

	self strokeWidth: morphStrokeWidth color: tlColor do: [
		self moveToX: aRectangle left + d1 y: aRectangle bottom - d2.
		self lineToX: aRectangle left + d1 y: aRectangle top + d1.
		self lineToX: aRectangle right - d2 y: aRectangle top + d1 ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 1/3/2021 12:25:56'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine"

	| cellSize srcBox dstBox srcQuad |

	"hack to affect morphIds in a reasonably way"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black alpha: 0.01`.

	cellSize _ 4.
	srcBox _ srcForm boundingBox.
	dstBox _ currentTransformation displayBoundsOfTransformOf: srcBox.
	srcQuad _ dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) floor ].
	dstBox _ currentTransformation displayBoundsOfTransformOf: (srcBox translatedBy: aPoint).
	(WarpBlt toForm: form)
		clipRect: clipRect;
		sourceForm: srcForm;
		colorMap: (srcForm colormapIfNeededFor: form);
		cellSize: cellSize;
		combinationRule: Form blend;
		copyQuad: srcQuad toRect: dstBox.! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
image: aForm multipliedBy: aColor at: aPoint
"NO quiero usar imagenes en M3... ver como hacer... quizas meter un servicio en el canvas que en m2 use imagenes y en m3 no..."
"no!!!!!!!!"
"	self halt"
super image: aForm multipliedBy: aColor at: aPoint! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 1/18/2021 17:26:20'!
imageUnrotated: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	Scale and position like #image:at:, but don't rotate srcForm:
	Display it in its natural orientation."

	| cellSize srcBox dstBox srcQuad |

	"hack to affect morphIds in a reasonably way"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black alpha: 0.01`.

	cellSize _ 4.
	srcBox _ srcForm boundingBox.
	srcQuad _ srcBox corners.
	dstBox _ Rectangle
		center: (currentTransformation transform: srcBox center + aPoint) rounded
		extent: (srcBox extent * currentTransformation scale) rounded.
	(WarpBlt toForm: form)
		clipRect: clipRect;
		sourceForm: srcForm;
		colorMap: (srcForm colormapIfNeededFor: form);
		cellSize: cellSize;
		combinationRule: Form blend;
		copyQuad: srcQuad toRect: dstBox.! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/22/2017 12:20:56'!
line: pt1 to: pt2 width: morphStrokeWidth color: aStrokeColor

	self strokeWidth: morphStrokeWidth color: aStrokeColor do: [
		self moveToX: pt1 x y: pt1 y.
		self lineToX: pt2 x y: pt2 y ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
reverseRectangleBorder: r borderWidth: borderWidth
	self halt! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/11/2017 21:10:33'!
roundRect: aRectangle color: aColor radius: r
	"For BitBltCanvas protocol, aRectangle:
	- uses Squeak / Smalltalk-80 convention: do include topLeft, but don't include bottomRight. (too pixel oriented for M3 taste)"
	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left-0.5 maxX: aRectangle right-0.5 minY: aRectangle top-0.5 maxY: aRectangle bottom-0.5 rx: r ry: r ]! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self halt! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self halt! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
stencil: stencilForm at: aPoint color: aColor
	self halt! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/30/2014 20:03'!
textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc
"ojo que es un quad, o sea un rect en coordenadas del morph..."
"	self halt"
super textComposition: aTextComposition bounds: boundddsRect color: c selectionColor: sc! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 12/16/2017 18:32:06'!
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

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 7/27/2020 10:24:44'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph."

	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | engine morphBoundsAfterDraw intersect: ownerClips ]
		ifNil: [ engine morphBoundsAfterDraw ]! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 11/17/2020 14:50:49'!
clipCurrentMorph: aBoolean
	engine clipCurrentMorph: aBoolean! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 1/13/2021 11:57:17'!
drawCurrentAndSubmorphs
	| r corners |
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph halo ifNotNil: [
				r _ engine morphBoundsAfterDraw.
				r extent >= (200@200) ifFalse: [
					r _ Rectangle center: r center extent: (r extent max: 200@200) ].
				corners _ r corners collect: [ :pt |
					currentTransformation inverseTransform: pt ]].
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].
		currentMorph postDrawOn: self.
		corners ifNotNil: [ self drawCoordinateSystem: corners ].
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].

		currentMorph isHighlighted ifTrue: [
			self drawCurrentMorphHighlight ].
	].! !

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

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 7/13/2020 16:52:13'!
morph: aMorph isAtPoint: aPoint
	^engine morph: aMorph isAtPoint: aPoint! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 12/22/2020 14:55:18'!
outOfMorph
	super outOfMorph.
	engine currentMorph: currentMorph.
	engine geometryTransformation: currentTransformation.
	engine resetMorphBounds.! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 8/6/2020 18:02:26'!
restorePatch: savedPatch
	"Argument must previously be obtained by calling #savePatch:bounds:
	Senders should not assume anything about the returned object, except that it will not be nil."

	| savedIdsPatch hack bb savedDisplayPatch |
"	super restorePatch: savedPatch first."
"aForm _ savedPatch first.
	self image: aForm
		at: aForm offset
		sourceRect: aForm boundingBox."
savedDisplayPatch _ savedPatch first.
	bb _ BitBlt toForm: form.
	bb sourceForm: savedDisplayPatch; combinationRule: Form over;
		sourceX: 0; sourceY: 0;
		destX: savedDisplayPatch offset x destY: savedDisplayPatch offset y
			width: savedDisplayPatch width height: savedDisplayPatch height;
		copyBits.

"Tanto el hack como el bb podrian guardarse."
savedIdsPatch _ savedPatch second.
	hack _ Form extent: form extent depth: 32 bits: (engine instVarNamed: 'morphIds').
	bb _ BitBlt toForm: hack.
	bb sourceForm: savedIdsPatch; combinationRule: Form over;
		sourceX: 0; sourceY: 0;
		destX: savedIdsPatch offset x destY: savedIdsPatch offset y
			width: savedIdsPatch width height: savedIdsPatch height;
		copyBits.! !

!VectorCanvas methodsFor: 'Morphic support' stamp: 'jmv 8/6/2020 20:16:37'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| patchForm savedIdsPatch hack bb answer morphIds |
self flag: #jmvHacks.
	answer _ prevSavedPatch.
	(answer is: #Array) ifFalse: [ answer _ Array new: 2 ].
	patchForm _ super savePatch: answer first bounds: aRectangle.

	patchForm == answer first
		ifTrue: [
			savedIdsPatch _ answer second.
			savedIdsPatch offset: 0@0.
			]
		ifFalse: [
			savedIdsPatch _ Form extent: aRectangle extent depth: 32.
			answer at: 1 put: patchForm.
			answer at: 2 put: savedIdsPatch ].

morphIds _ engine instVarNamed: 'morphIds'.
"Tanto el hack como el bb podrian guardarse."
	hack _ Form extent: form extent depth: 32 bits: morphIds.
	bb _ BitBlt toForm: savedIdsPatch.
	bb sourceForm: hack; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	savedIdsPatch offset: aRectangle topLeft.
	^answer! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 6/8/2020 12:24:42'!
doSubPixelAntiAliasing
	"
	Instead of just whole pixel anti-aliasing.
	"
	engine _ VectorEngineSubPixel onForm: form.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 12/21/2020 17:36:45'!
fontToUse: fontOrNil
	"Answer a suitable font, aFont if possible."

	^fontOrNil ifNil: [ FontFamily defaultOrTrueTypeFamilyAndPointSize ].! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 1/14/2020 09:32:24'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 7/24/2020 17:08:49'!
setForm: aForm
	"
	World extent: World extent
	"
	aForm depth = 32  ifFalse: [ self error: 'VectorCanvas can only work on 32 bit destination' ].
	form _ aForm.
	engine _ VectorEngineWholePixel onForm: form.
	boundsFinderEngine _ VectorEngineBoundsFinder onForm: form.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 7/24/2020 17:09:33'!
updatingMorphBoundsDo: aBlock

	| prevEngine |
	prevEngine _ engine.
	[
		engine _ boundsFinderEngine.
		aBlock value.
	] ensure: [ engine _ prevEngine ]! !

!VectorCanvas methodsFor: 'drawing' stamp: 'jmv 12/23/2020 16:33:17'!
drawCoordinateSystem: corners
	| c stepX stepY p1 p2 p3 p4 x0 x1 y0 y1 |
	stepX _ 50.
	stepY _ 20.
	p1 _ corners first.
	p2 _ corners second.
	p3 _ corners third.
	p4 _ corners fourth.

	engine currentMorph: nil.

	self fillColor: `Color lightGray alpha: 0.4` do: [
		self
			moveToX: p1 x y: p1 y;
			lineToX: p2 x y: p2 y;
			lineToX: p3 x y: p3 y;
			lineToX: p4 x y: p4 y. ].

	x0 _ x1 _ y0 _ y1 _ nil.
	corners withPreviousCyclicDo: [ :pa :pb | | xx yy |
		pa y sign = pb y sign ifFalse: [
			xx _ pb x - pa x * (pa y / (pa y - pb y)) + pa x.
			(x0 isNil or: [ xx < x0 ]) ifTrue: [ x0 _ xx ].
			(x1 isNil or: [ xx > x1 ]) ifTrue: [ x1 _ xx ]].
		pa x sign = pb x sign ifFalse: [
			yy _ pb y - pa y * (pa x / (pa x - pb x)) + pa y.
			(y0 isNil or: [ yy < y0 ]) ifTrue: [ y0 _ yy ].
			(y1 isNil or: [ yy > y1 ]) ifTrue: [ y1 _ yy ]]].

	x0 isNil ifTrue: [
		x0 _ -100. x1 _ 500 ].
	y0 isNil ifTrue: [
		y0 _ -100. y1 _ 400 ].

	c _ `Color white alpha: 0.6`.
	self strokeWidth: 1 color: c do: [
		self moveToX: x0 y: 0; lineToX: x1 y: 0.
		self moveToX: 0 y: y0; lineToX: 0 y: y1. ].
	
	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		self strokeWidth: 1 color: c do: [
			self moveToX: x y: -5; lineToX: x y: 5 ].
	   	self drawString: x printString atCenterX: x @ 5 font: nil color: c ].
	  self drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		self strokeWidth: 1 color: c do: [
			self moveToX: -5 y: y; lineToX: 5 y: y ].
	   	self drawString: y printString, ' ' atWaistRight: 0 @ y font: nil color: c ].
	  self drawString: 'y' atWaistRight: 0 @ (y1 - 20) font: nil color: c.! !

!VectorCanvas methodsFor: 'drawing' stamp: 'jmv 1/13/2021 11:57:42'!
drawCurrentMorphHighlight

	currentMorph displayBounds ifNotNil: [ :r |
		(BitBltCanvasEngine toForm: form)
			combinationRule: Form blend;
			fillColor: `Color black`;
			frameRect: r borderWidth: 4;
			fillColor: `Color pink alpha: 0.2`;
			fillRect: (r insetBy: 4).
	]! !

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

!VectorEngineAbstract methodsFor: 'accessing' stamp: 'jmv 5/15/2020 09:27:38'!
antiAliasingWidth: aNumber

	antiAliasingWidth _ aNumber.! !

!VectorEngineAbstract methodsFor: 'accessing' stamp: 'jmv 4/15/2020 17:33:22'!
geometryTransformation: aGeometryTransformation
	"geometryTransformation is a GeometryTransformation that when applied to points in the public path protocol (i.e. Morph or domain coordinate system), it transform them into global, i.e. targetForm pixel coordinates. Therefore, the method used for converting client coordinates to actual pixel coordinates in the targetForm is #externalizePosition:, while the seldom used conversion from pixel coordinates to client coordinates is #internalizePosition:"

	geometryTransformation _ aGeometryTransformation! !

!VectorEngineAbstract methodsFor: 'accessing' stamp: 'jmv 7/27/2020 10:24:19'!
morphBoundsAfterDraw
	"In targetForm coordinates.
	Doesn't include any clipping rect (neither clip rect parameter to bound drawn area, nor possible clipping to owner shape or bounds).
	Only valid _after_ drawing currentMorph."

	^morphBoundsLeft @ morphBoundsTop corner: morphBoundsRight @ morphBoundsBottom + 1! !

!VectorEngineAbstract methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:24:58'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.
	hopsPerPixel _ 8.										"See #experimentHopsPerPixel3"
	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath: true! !

!VectorEngineAbstract methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:27:50'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: false! !

!VectorEngineAbstract methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:27:04'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	| aStrokeWidth |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	clientStrokeWidth = 0.0 ifTrue: [ ^self ].
	aStrokeWidth _ geometryTransformation externalizeScalar: clientStrokeWidth.
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"If the requested stroke width is too thin, make them wide enough to be properly sampled."
			self pvtStrokeWidth: antiAliasingWidth.
			self pvtStrokeColor: (aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / antiAliasingWidth) ]
		ifFalse: [
			self pvtStrokeWidth: aStrokeWidth.
			self pvtStrokeColor:  aStrokeColor ].
	self pvtFillColor: nil.
	hopsPerPixel _ 10.0 / strokeWidth + 1.		"See #experimentHopsPerPixel2"

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath: ensureClosePath! !

!VectorEngineAbstract methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:28:44'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	| aStrokeWidth |

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	clientStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	aStrokeWidth _ geometryTransformation externalizeScalar: clientStrokeWidth.
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: true.
		^ self ].

	self pvtStrokeWidth: aStrokeWidth.
	self pvtStrokeColor: aStrokeColor.
	self pvtFillColor: aFillColor.
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"Strokes are really thin, but strokeColor is same as fillColor, so there's no problem sampling them."
			hopsPerPixel _ 8 ]								"See #experimentHopsPerPixel1"
		ifFalse: [
			hopsPerPixel _ 10.0 / strokeWidth + 1 max: 2 ].		"See #experimentHopsPerPixel2. Additionally, fills require at least 2."

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath: true! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:06'!
abs_ArcToX: cx y: cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorEngineSubPixel onForm: Display)
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

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:13'!
abs_CircleCenterX: cx y: cy radius: mr
"
(VectorEngineSubPixel onForm: Display)
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

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:23'!
abs_CubicBezierSmoothToX: xTo y: yTo control2X: xC y: yC
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorEngineSubPixel  onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 150 y: 300 radius: 10.
		engine abs_CircleCenterX: 600 y: 400 radius: 6.
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100 control1X: 150 y: 300 control2X: 600 y: 400
			].
(VectorEngineSubPixel  onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
	mirroredContolX _ previousCallX + (previousCallX - previousCallControlX).
	mirroredContolY _ previousCallY + (previousCallY - previousCallControlY).
	self abs_CubicBezierToX: xTo y: yTo control1X: mirroredContolX y: mirroredContolY control2X: xC y: yC! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:35'!
abs_CubicBezierToX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ].
Display forceToScreen
	"
	| c1 c2 to |
	c1 _ geometryTransformation externalizePosition: xC1@yC1.
	c2 _ geometryTransformation externalizePosition: xC2@yC2.
	to _ geometryTransformation externalizePosition: xTo@yTo.
	self basicCubicBezierToX: to x y: to y control1X: c1 x y: c1 y control2X: c2 x y: c2 y.
	previousCallX _ xTo.
	previousCallY _ yTo.
	previousCallControlX _ xC2.
	previousCallControlY _ yC2! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:51'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50].
(VectorEngineSubPixel onForm: Display)
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

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:29:57'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry rotationAngle: theta
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: -0.5 ].
(VectorEngineSubPixel onForm: Display)
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

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:30:05'!
abs_EllipseCenterX: cx y: cy radiusX: mrx radiusY: mry rotationAngle: theta start: startAngle sweep: sweepAngle
	"Currently draws a line from pathCurrentX@pathCurrentY to start (if startAngle ~= 0.0 or sweepAngle ~= Float twoPi).
	This could be useful for proper fills. But might not be what is desired in some cases.
	Implement desired behavior when clear."
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_EllipseCenterX: 200 y: 150 radiusX: 100 radiusY: 50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorEngineSubPixel onForm: Display)
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

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:30:12'!
abs_LineToX: xTo
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 50.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_LineToX: xTo y: previousCallY! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:30:36'!
abs_LineToX: xTo y: yTo
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToX: 15 y: 30.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	| to |
	to _ geometryTransformation externalizePosition: xTo@yTo.
	self basicLineToX: to x y: to y.
	previousCallX _ xTo.
	previousCallY _ yTo.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:30:46'!
abs_LineToY: yTo
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToY: 200 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_LineToY: 50.
		engine abs_LineToX: 60 y: 10 ].
Display forceToScreen
"
	self abs_LineToX: previousCallX y: yTo.! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:30:56'!
abs_MoveToX: x y: y
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
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
	previousCallX _ x.
	previousCallY _ y.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:31:07'!
abs_QuadraticBezierSmoothToX: xTo y: yTo
"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 450 y: 300 radius: 10.
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300
			].
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
	mirroredContolX _ previousCallX + (previousCallX - previousCallControlX).
	mirroredContolY _ previousCallY + (previousCallY - previousCallControlY).
	self abs_QuadraticBezierToX: xTo y: yTo controlX: mirroredContolX y: mirroredContolY! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:31:16'!
abs_QuadraticBezierToX: xTo y: yTo controlX: xC y: yC
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ].
Display forceToScreen
	"
	| c to |
	c _ geometryTransformation externalizePosition: xC@yC.
	to _ geometryTransformation externalizePosition: xTo@yTo.
	self basicQuadraticBezierToX: to x y: to y controlX: c x y: c y.
	previousCallX _ xTo.
	previousCallY _ yTo.
	previousCallControlX _ xC.
	previousCallControlY _ yC.! !

!VectorEngineAbstract methodsFor: 'paths - absolute coordinates' stamp: 'jmv 6/6/2020 17:32:23'!
do_ClosePath
	"abs/rel meaningless here"
"
(VectorEngineSubPixel onForm: Display)
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

	"pathCurrentX and pathCurrentY are in (float) targetForm pixel coordinates (i.e. engine basic coordinates). 
	previousCallX, previousCallY, previousCallControlX, previousCallControlY are in client coordinates."
	p2 _ geometryTransformation internalizePosition: pathCurrentX@pathCurrentY.
	previousCallX _ p2 x.
	previousCallY _ p2 y.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:32:49'!
rel_ArcToX: cx y: cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			rel_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: false;
			do_ClosePath ].
Display forceToScreen
"
	self abs_ArcToX: previousCallX + cx y: previousCallY + cy radiusX: mrx radiusY: mry angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:33:02'!
rel_CubicBezierSmoothToX: xTo y: yTo control2X: xC y: yC
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
		abs_CubicBezierSmoothToX: previousCallX + xTo y: previousCallY + yTo
		control2X: previousCallX + xC y: previousCallY + yC.! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:33:15'!
rel_CubicBezierToX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
		abs_CubicBezierToX: previousCallX + xTo y: previousCallY + yTo
		control1X: previousCallX + xC1 y: previousCallY + yC1
		control2X: previousCallX + xC2 y: previousCallY + yC2! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:33:24'!
rel_LineToX: xTo y: yTo
"
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
	self abs_LineToX: previousCallX + xTo y: previousCallY + yTo! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:33:36'!
rel_MoveToX: x y: y
"
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
	self abs_MoveToX: previousCallX + x y: previousCallY + y! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:33:57'!
rel_QuadraticBezierSmoothToX: xTo y: yTo
"
(VectorEngineSubPixel onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :engine |
		engine abs_CircleCenterX: 550 y: 350 radius: 10.
		engine abs_MoveToX: 100 y: 50.
		engine
			rel_QuadraticBezierToX: 600 y: 100 controlX: 450 y: 300
			].
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
		abs_QuadraticBezierSmoothToX: previousCallX + xTo y: previousCallY + yTo.! !

!VectorEngineAbstract methodsFor: 'paths - relative coordinates' stamp: 'jmv 6/6/2020 17:34:04'!
rel_QuadraticBezierToX: xTo y: yTo controlX: xC y: yC
	"
(VectorEngineSubPixel onForm: Display)
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
(VectorEngineSubPixel onForm: Display)
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
		abs_QuadraticBezierToX: previousCallX + xTo y: previousCallY + yTo
		controlX: previousCallX + xC y: previousCallY + yC.! !

!VectorEngineAbstract methodsFor: 'text' stamp: 'jmv 12/23/2020 15:07:12'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| destX destY sx sy contourData contourDataIndexes trueTypeScale cpX cpY epX epY moveToX moveToY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForIso8859s15.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByIso8859s15ST80Glyphs: aBoolean.
	self fillColor: aColor do: [
		destY _ aPoint y.
		destX _ aPoint x.
		startIndex to: stopIndex do: [ :charIndex | | iso8859s15 i |
			iso8859s15 _ (aString at: charIndex) iso8859s15Code.
			i _ contourDataIndexes at: iso8859s15 + 1.
			advanceWidth _ contourData at: i. i _ i + 1.
			boundsLeft _ contourData at: i. i _ i + 1.
			boundsRight _ contourData at: i. i _ i + 1.
			boundsTop _ contourData at: i. i _ i + 1.
			boundsBottom _ contourData at: i. i _ i + 1.
			numContours _ (contourData at: i) asInteger. i _ i + 1.
			numContours timesRepeat: [
				numBeziers _ (contourData at: i) asInteger. i _ i + 1.
				moveToX _ contourData at: i. i _ i + 1.
				moveToY _ contourData at: i. i _ i + 1.
				self abs_MoveToX: moveToX * sx + destX y: moveToY * sy + destY.
				numBeziers timesRepeat: [
					epX _ contourData at: i. i _ i + 1.
					epY _ contourData at: i. i _ i + 1.
					cpX _ contourData at: i. i _ i + 1.
					cpY _ contourData at: i. i _ i + 1.
					self rel_QuadraticBezierToX: epX * sx y: epY * sy controlX: cpX * sx y: cpY * sy
					]
				].
			destX _ destX + (advanceWidth * sx).
		]
	].
	^ destX ceiling@destY! !

!VectorEngineAbstract methodsFor: 'text' stamp: 'jmv 6/17/2020 14:33:41'!
displayUtf8: aByteArray at: aPoint trueTypeFont: aTTFontDescription color: aColor scaleX: ssx scaleY: ssy
	"ssx and ssy to honor font pointSize"
	| destX destY sx sy contourData contourDataIndexes trueTypeScale cpX cpY epX epY moveToX moveToY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers baseIndex |

	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.
	sx _ ssx * trueTypeScale.
	sy _ ssy * trueTypeScale.
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	self fillColor: aColor do: [
		destY _ aPoint y.
		destX _ aPoint x.
		baseIndex _ 0.
		aByteArray do: [ :byte | | i |
			i _ contourDataIndexes at: baseIndex + byte + 1.
			i < 0
				ifTrue: [ baseIndex _ i negated ]
				ifFalse: [
					advanceWidth _ contourData at: i. i _ i + 1.
					boundsLeft _ contourData at: i. i _ i + 1.
					boundsRight _ contourData at: i. i _ i + 1.
					boundsTop _ contourData at: i. i _ i + 1.
					boundsBottom _ contourData at: i. i _ i + 1.
					numContours _ (contourData at: i) asInteger. i _ i + 1.
					numContours timesRepeat: [
						numBeziers _ (contourData at: i) asInteger. i _ i + 1.
						moveToX _ contourData at: i. i _ i + 1.
						moveToY _ contourData at: i. i _ i + 1.
						self abs_MoveToX: moveToX * sx + destX y: moveToY * sy + destY.
						numBeziers timesRepeat: [
							epX _ contourData at: i. i _ i + 1.
							epY _ contourData at: i. i _ i + 1.
							cpX _ contourData at: i. i _ i + 1.
							cpY _ contourData at: i. i _ i + 1.
							self rel_QuadraticBezierToX: epX * sx y: epY * sy controlX: cpX * sx y: cpY * sy
							]
						].
					destX _ destX + (advanceWidth * sx).
					baseIndex _ 0.
				]
			]
	].! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 11/16/2020 21:20:09'!
initializeOnForm: aForm
	geometryTransformation _ MorphicTranslation new. "Identity"
	currentMorphId _ 0.
	currentClipsSubmorphs _ false.
	clipCurrentMorph _ false.
	self pvtSetForm: aForm! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 5/14/2020 16:04:16'!
initializePath

	pathFirstX _ 0.
	pathFirstY _ 0.
	pathCurrentX _ 0.0.
	pathCurrentY _ 0.0.
	previousCallX _ 0.0.
	previousCallY _ 0.0.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY.
	self newTrajectoryFragment.! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 5/14/2020 16:03:34'!
newTrajectoryFragment! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 6/26/2020 16:11:12'!
pvtSetForm: aForm

	targetForm _ aForm.
	morphIds _ WordArray new: targetForm width * targetForm height.
	targetWidth _ targetForm width.
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 5/15/2020 09:27:20'!
pvtStrokeWidth: aNumber

	strokeWidth _ aNumber.! !

!VectorEngineAbstract methodsFor: 'private' stamp: 'jmv 4/11/2020 19:07:10'!
resetMorphBounds

	morphBoundsTop _ targetForm height - 1.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ targetWidth - 1.
	morphBoundsRight _ 0! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:31:25'!
basicArcCenterX: cx centerY: cy radiusX: rx radiusY: ry rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine basicArcCenterX: 100 centerY: 100 radiusX: 80 radiusY: 40 rotationAngle: -0.2 start: 0 sweep: Float pi * 2 ].
Display forceToScreen.
"
	| d hops angle thetaCos thetaSin xp yp x0 y0 |

	"Por supuesto probar tambien rx no igual a ry, y uno (o ambos) menor que el ancho de trazo..."
	thetaCos _ theta cos.
	thetaSin _ theta sin.
	angle _ startAngle.
	xp _ angle cos * rx.
	yp _ angle sin * ry.
	x0 _ thetaCos * xp - (thetaSin * yp) + cx.
	y0 _ thetaSin * xp + (thetaCos * yp) + cy.
	(startAngle ~= 0.0 or: [ sweepAngle ~= Float twoPi ])
		ifTrue: [ self basicLineToX: x0 y: y0 ] "If an arc (not whole circle/ellipse, join with a line, to ensure closed path. For example, window frames"
		ifFalse: [
			"trajectory starts here. Eventually, #closePath should draw a line to here"
			"Next point will be first in a new trajectory fragment"
			self newTrajectoryFragment.
			pathFirstX _ x0.
			pathFirstY _ y0 ].

	hops _ ((rx+ry * 4 * sweepAngle abs / (2*Float pi)) * hopsPerPixel) truncated + 1.
	d _ sweepAngle / hops.

	self primArcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle rotationCos: thetaCos rotationSin: thetaSin hops: hops angleDelta: d.

	angle _ startAngle + sweepAngle.
	xp _ angle cos * rx.
	yp _ angle sin * ry.
	pathCurrentX _ thetaCos * xp - (thetaSin * yp) + cx.
	pathCurrentY _ thetaSin * xp + (thetaCos * yp) + cy.! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:31:30'!
basicArcToX: x2 y: y2 radiusX: rx radiusY: ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngineSubPixel onForm: Display)
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
	x1 _ pathCurrentX.
	y1 _ pathCurrentY.
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

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:31:34'!
basicClosePath
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_ArcToX: 200 y: 200 radiusX: 200 radiusY: 100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			do_ClosePath ].
Display forceToScreen
"
	self basicLineToX: pathFirstX y: pathFirstY! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:31:48'!
basicCubicBezierToX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_CubicBezierToX: 300 y: 200 control1X: 110 y: 110 control2X: 200 y: 200 ].
Display forceToScreen
	"
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine abs_MoveToX: 100 y: 50.
		engine
			abs_CubicBezierToX: 600 y: 100
			control1X: 150 y: 300
			control2X: 600 y: 400 ].
Display forceToScreen
	"

	| dx dy hops |
	dx _ (xC1-pathCurrentX) abs + (xC2-xC1) abs + (xTo-xC2) abs.
	dy _ (yC1-pathCurrentY) abs + (yC2-yC1) abs + (yTo-yC2) abs.
	hops _ ((dx max: dy) * hopsPerPixel) truncated + 1.

	self primCubicBezierFromX: pathCurrentX y: pathCurrentY toX: xTo y: yTo control1X: xC1 y: yC1 control2X: xC2 y: yC2 hops: hops.

	pathCurrentX _ xTo.
	pathCurrentY _ yTo.! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:31:58'!
basicLineToX: xTo y: yTo
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 300 y: 150 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 25.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 500 y: 200 ].
Display forceToScreen
"
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 30 y: 15.
		engine abs_LineToX: 45 y: 90.
		engine abs_LineToX: 180 y: 30 ].
Display forceToScreen
"
	| dx dy hops incrementX incrementY |
	dy _ yTo - pathCurrentY.
	dx _ xTo - pathCurrentX.
	"Optimizar. En todas las lineas, bounds. En verticales, mucho mas. En horiz, no EdgeCounts!! Ver 02"
	hops _ ((dx abs max: dy abs) * hopsPerPixel) truncated + 1.
	incrementX _ dx *1.0 / hops.
	incrementY _ dy *1.0 / hops.

	self primLineFromX: pathCurrentX y: pathCurrentY toX: xTo y: yTo hops: hops incrementX: incrementX incrementY: incrementY.

	pathCurrentX _ xTo.
	pathCurrentY _ yTo.! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:32:11'!
basicMoveToX: x y: y
"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_LineToX: 200 y: 150;
			abs_MoveToX: 210 y: 150;
			abs_LineToX: 300 y: 200 ].
Display forceToScreen
"

	"A discontinuity in the trajectory"

	"We are starting a new trajectory, or at a discontinuity in trajectory.
	Eventually #closeTrajectory should draw a line to here"

	"Next point will be first in a new trajectory fragment"
	self newTrajectoryFragment.
	pathFirstX _ x.
	pathFirstY _ y.
	pathCurrentX _ x.
	pathCurrentY _ y.! !

!VectorEngineAbstract methodsFor: 'private - paths - pixel coordinates' stamp: 'jmv 6/6/2020 17:32:17'!
basicQuadraticBezierToX: xTo y: yTo controlX: xC y: yC
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			abs_MoveToX: 100 y: 100;
			abs_QuadraticBezierToX: 300 y: 200 controlX: 100 y: 210 ].
Display forceToScreen
	"
	"
(VectorEngineSubPixel onForm: Display)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine abs_MoveToX: 10 y: 5.
		engine abs_QuadraticBezierToX: 60 y: 10 controlX: 15 y: 30 ].
Display forceToScreen
	"
	| dx dy hops |
	dx _ (xC-pathCurrentX) abs + (xTo-xC) abs.
	dy _ (yC-pathCurrentY) abs + (yTo-yC) abs.
	hops _ ((dx max: dy) * hopsPerPixel) truncated + 1.

	self primQuadraticBezierFromX: pathCurrentX y: pathCurrentY toX: xTo y: yTo controlX: xC y: yC hops: hops.

	pathCurrentX _ xTo.
	pathCurrentY _ yTo.! !

!VectorEngineAbstract methodsFor: 'morphic' stamp: 'jmv 11/17/2020 09:27:07'!
morph: aMorph isAtPoint: aPoint
	| index |
	index _ aPoint y * targetWidth + aPoint x + 1.
	(index between: 1 and: morphIds size) ifFalse: [ ^false ].
	^((morphIds at: index) >> 8) = aMorph morphId! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:34:12'!
experLinesAndShapes
	"
	VectorEngineSubPixel experLinesAndShapes
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
	engine strokeWidth: self defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ self defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		yy2 _ y2 >= 1 ifTrue: [ self defaultAntiAliasingWidth * 0.5 ] ifFalse: [0].
		engine strokeWidth: self defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
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
		y1 _ y1 + y2 + self defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:34:24'!
experLinesAndShapes2
	"
	VectorEngineSubPixel experLinesAndShapes2
	"
	| engine y1 dy y2 |
	engine _ self onForm: Display.

	y1 _ 10.
	dy _ 3.
	y2 _ self defaultAntiAliasingWidth*4*2.
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
		y1 _ y1 + y2 + self defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:34:41'!
experLinesAndShapes3
	"
	VectorEngineSubPixel experLinesAndShapes3
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
	engine strokeWidth: self defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ self defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: self defaultAntiAliasingWidth*0.4 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 431 y: y1; abs_LineToX: 530 y: y1+dy;
				abs_LineToX: 530 y: y1+y2+dy; abs_LineToX: 431 y: y1+y2;
				do_ClosePath.
			engine
				abs_MoveToX:  y1 y: 431; abs_LineToX: y1+dy y: 530;
				abs_LineToX: y1+y2+dy y: 530; abs_LineToX: y1+y2 y: 431;
				do_ClosePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: self defaultAntiAliasingWidth*0.2 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 531 y: y1+dy; abs_LineToX: 630 y: y1+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy; abs_LineToX: 531 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 531; abs_LineToX: y1+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy y: 630; abs_LineToX: y1+y2+dy y: 531;
				do_ClosePath ].
		"Solo fill. w =0"
		engine strokeWidth: self defaultAntiAliasingWidth*0.0 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy y: 631;
				do_ClosePath ].
		y1 _ y1 + y2 + self defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:34:51'!
experLinesAndShapes4
	"
	VectorEngineSubPixel experLinesAndShapes4
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
	engine strokeWidth: self defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
		engine
			abs_MoveToX: 220 y: 10;
			abs_LineToX: 410 y: 15; abs_LineToX: 220 y: 20;
			abs_LineToX: 215 y: 410; abs_LineToX: 210 y: 20;
			do_ClosePath ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ self defaultAntiAliasingWidth * 2.
	15 timesRepeat: [
		"stroke con w > aaw"
		engine strokeWidth: self defaultAntiAliasingWidth*2 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 431 y: y1; abs_LineToX: 530 y: y1+dy;
				abs_LineToX: 530 y: y1+y2+dy; abs_LineToX: 431 y: y1+y2;
				do_ClosePath.
			engine
				abs_MoveToX:  y1 y: 431; abs_LineToX: y1+dy y: 530;
				abs_LineToX: y1+y2+dy y: 530; abs_LineToX: y1+y2 y: 431;
				do_ClosePath ].
		"Solo fill. w casi=0"
		engine strokeWidth: self defaultAntiAliasingWidth*1.5 color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 531 y: y1+dy; abs_LineToX: 630 y: y1+dy+dy;
				abs_LineToX: 630 y: y1+y2+dy+dy; abs_LineToX: 531 y: y1+y2+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy y: 531; abs_LineToX: y1+dy+dy y: 630;
				abs_LineToX: y1+y2+dy+dy y: 630; abs_LineToX: y1+y2+dy y: 531;
				do_ClosePath ].
		"Solo fill. w =0"
		engine strokeWidth: self defaultAntiAliasingWidth color: Color black fillColor: Color black do: [
			engine
				abs_MoveToX: 631 y: y1+dy+dy; abs_LineToX: 730 y: y1+dy+dy+dy;
				abs_LineToX: 730 y: y1+y2+dy+dy+dy; abs_LineToX: 631 y: y1+y2+dy+dy;
				do_ClosePath.
			engine
				abs_MoveToX: y1+dy+dy y: 631; abs_LineToX: y1+dy+dy+dy y: 730;
				abs_LineToX: y1+y2+dy+dy+dy y: 730; abs_LineToX: y1+y2+dy+dy y: 631;
				do_ClosePath ].
		y1 _ y1 + y2 + self defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.
! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:35:01'!
experLinesAndShapesx
	"
	VectorEngineSubPixel experLinesAndShapesx
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
	y2 _ self defaultAntiAliasingWidth * 2.
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
		y1 _ y1 + y2 + self defaultAntiAliasingWidth + 3.
		Display forceToScreen.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen.! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'db 6/15/2020 14:02:01'!
experLinesAndShapesx2
	"
	VectorEngineSubPixel experLinesAndShapesx2
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

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:35:56'!
experimentHopsPerPixel1
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 1: Very thin stroke, the same color as fill
	VectorEngineSubPixel experimentHopsPerPixel1
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 8.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ self onForm: f1.
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
	engine _ self onForm: f2.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) *10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:36:06'!
experimentHopsPerPixel2
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 2: Stroke of various widths, no fill
	VectorEngineSubPixel experimentHopsPerPixel2
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
	engine _ self onForm: f1.
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
	engine _ self onForm: f2.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 40 y: 30;
			abs_LineToX: 190 y: 135; abs_LineToX: 40 y: 40;
			abs_LineToX: 135 y: 390; abs_LineToX: 30 y: 40;
			do_ClosePath ].
	f2 displayAt: 220@0.

Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngineAbstract class methodsFor: 'experiments' stamp: 'jmv 6/6/2020 17:36:17'!
experimentHopsPerPixel3
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 3: No stroke, just fill
	VectorEngineSubPixel experimentHopsPerPixel3
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 8.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ self onForm: f1.
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
	engine _ self onForm: f2.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hopsPerPixel' put: hops2.
		engine
			abs_MoveToX: 20 y: 10;
			abs_LineToX: 210 y: 15; abs_LineToX: 20 y: 20;
			abs_LineToX: 15 y: 410; abs_LineToX: 10 y: 20;
			do_ClosePath ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngineAbstract class methodsFor: 'samples' stamp: 'db 6/15/2020 14:02:28'!
experiment1
	"
	No perfecto aun, pero se puede empezar a comparar con
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	Ni que hablar que es mucho mas rapido...
	VectorEngineSubPixel experiment1
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

!VectorEngineAbstract class methodsFor: 'samples' stamp: 'db 6/15/2020 14:02:47'!
experiment2
	"
	VectorEngineSubPixel experiment2
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

!VectorEngineAbstract class methodsFor: 'samples' stamp: 'db 6/15/2020 14:02:58'!
experiment3
	"
	VectorEngineSubPixel experiment3
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

!VectorEngineAbstract class methodsFor: 'samples' stamp: 'db 6/15/2020 14:03:02'!
experiment4
	"
	VectorEngineSubPixel experiment4
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

!VectorEngineAbstract class methodsFor: 'samples' stamp: 'db 6/15/2020 14:03:08'!
siemensStar
	"
	VectorEngineSubPixel siemensStar display
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
		].
	].
	^form! !

!VectorEngineAbstract class methodsFor: 'instance creation' stamp: 'jmv 6/17/2020 14:34:36'!
defaultAntiAliasingWidth
	^1.0! !

!VectorEngineAbstract class methodsFor: 'instance creation' stamp: 'jmv 4/17/2020 09:47:42'!
onForm: aForm

	^ self new initializeOnForm: aForm! !

!VectorEngineBoundsFinder methodsFor: 'accessing' stamp: 'jmv 11/17/2020 14:49:32'!
clipCurrentMorph: aBoolean! !

!VectorEngineBoundsFinder methodsFor: 'accessing' stamp: 'jmv 7/24/2020 16:52:27'!
clipRect: aRectangle! !

!VectorEngineBoundsFinder methodsFor: 'accessing' stamp: 'jmv 11/17/2020 10:04:59'!
currentMorph: aMorph! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:25:26'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	hopsPerPixel _ 8.										"See #experimentHopsPerPixel3"
	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath: true! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:27:25'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	| aStrokeWidth |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	clientStrokeWidth = 0.0 ifTrue: [ ^self ].
	aStrokeWidth _ geometryTransformation externalizeScalar: clientStrokeWidth.
	self pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth).
	hopsPerPixel _ 10.0 / strokeWidth + 1.		"See #experimentHopsPerPixel2"
	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: self.
	self finishPath: ensureClosePath! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/6/2020 17:28:31'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	clientStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	^ self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: true! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 7/24/2020 16:35:39'!
primArcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle rotationCos: thetaCos rotationSin: thetaSin hops: hops angleDelta: d

	| angle xp yp x y |
	angle _ startAngle.
	hops+1 timesRepeat: [
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		x _ thetaCos * xp - (thetaSin * yp) + cx.
		y _ thetaSin * xp + (thetaCos * yp) + cy.
		self updateBoundsForX: x y: y.
		angle _ angle + d. ].! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 7/24/2020 16:35:54'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 hops: hops
	| t oneLessT increment f2 f3 f1 f23 f4 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * xFrom) + (f2 * xControl1) + (f3 * xControl2) + (f4 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl1) + (f3 * yControl2) + (f4 * yTo).
		self updateBoundsForX: x y: y.
		t _ t + increment. ].
	self updateBoundsForX: xTo y: yTo.! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 7/24/2020 16:37:25'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo hops: hops incrementX: incrementX incrementY: incrementY

	self updateBoundsForX: xFrom y: yFrom.
	self updateBoundsForX: xTo y: yTo.! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 7/24/2020 16:36:29'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl hops: hops
	| t oneLessT increment f2 f3 f1 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2* oneLessT * t.
		f3 _ t * t.
		x _ (f1 * xFrom) + (f2 * xControl) + (f3 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl) + (f3 * yTo).
		self updateBoundsForX: x y: y.
		t _ t + increment. ].
	self updateBoundsForX: xTo y: yTo.! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms - aux' stamp: 'jmv 7/24/2020 16:49:09'!
updateBoundsForX: x y: y

	morphBoundsLeft _ morphBoundsLeft min: x.
	morphBoundsTop _ morphBoundsTop min: y.
	morphBoundsRight _ morphBoundsRight max: x.
	morphBoundsBottom _ morphBoundsBottom max: y.! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 8/6/2020 17:24:36'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"

	morphBoundsLeft _ (morphBoundsLeft - auxStrokeWidthDilatedHalf) truncated +1.
	morphBoundsTop _ (morphBoundsTop - auxStrokeWidthDilatedHalf) truncated +1.
	morphBoundsRight _ (morphBoundsRight + auxStrokeWidthDilatedHalf) truncated "+1". "Make room not just for updated mask, but also edges (hence, +1) NOT IN THIS CLASS"
	morphBoundsBottom _ (morphBoundsBottom + auxStrokeWidthDilatedHalf) truncated.

	"Most likely useless."
	pathFirstX _ 0.0.
	pathFirstY _ 0.0.
	pathCurrentX _ 0.0.
	pathCurrentY _ 0.0.
	previousCallX _ 0.0.
	previousCallY _ 0.0.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY.! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 7/24/2020 17:14:59'!
morph: aMorph isAtPoint: aPoint
	self shouldNotHappen! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 7/24/2020 17:13:17'!
pvtSetForm: aForm

	targetForm _ aForm.
	targetWidth _ targetForm width.
	self resetMorphBounds.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 7/24/2020 16:44:36'!
pvtStrokeWidth: aNumber

	super pvtStrokeWidth: aNumber.
	auxStrokeWidthDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.! !

!VectorEngineBoundsFinder class methodsFor: 'instance creation' stamp: 'jmv 7/24/2020 16:31:03'!
defaultAntiAliasingWidth
	^1.7! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 6/6/2020 17:21:13'!
antiAliasingWidth: aNumber

	super antiAliasingWidth: aNumber.
	auxAntiAliasingWidthScaledInverse _ 127.0 / antiAliasingWidth.! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 11/17/2020 15:20:46'!
clipCurrentMorph: aBoolean
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex _ displayY * targetWidth +  clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				pixelIndex _ pixelIndex + 1.
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) >>8 << 8) ]]].
	clipCurrentMorph _ aBoolean! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 8/6/2020 17:43:05'!
clipRect: aRectangle 
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipLeft _ aRectangle left asInteger max: 0.
	clipTop _ aRectangle top asInteger max: 0.
	clipRight _ (aRectangle right asInteger min: targetWidth) - 1.
	clipBottom _ (aRectangle bottom asInteger min: targetForm height) - 1! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 12/1/2020 10:46:15'!
currentMorph: aMorph
	currentMorphId _ aMorph ifNil: [0] ifNotNil: [aMorph morphId].
	currentClipsSubmorphs _ aMorph notNil and: [aMorph clipsSubmorphsReally].! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:23:25'!
primArcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle rotationCos: thetaCos rotationSin: thetaSin hops: hops angleDelta: d

	| angle xp yp x y |
	angle _ startAngle.
	hops+1 timesRepeat: [
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		x _ thetaCos * xp - (thetaSin * yp) + cx.
		y _ thetaSin * xp + (thetaCos * yp) + cy.
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		angle _ angle + d. ].! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:23:32'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 hops: hops
	| t oneLessT increment f2 f3 f1 f23 f4 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * xFrom) + (f2 * xControl1) + (f3 * xControl2) + (f4 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl1) + (f3 * yControl2) + (f4 * yTo).
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:23:36'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo hops: hops incrementX: incrementX incrementY: incrementY

	| x y |
	x _ xFrom.
	y _ yFrom.
	hops timesRepeat: [
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		x _ x + incrementX.
		y _ y + incrementY ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:23:41'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl hops: hops
	| t oneLessT increment f2 f3 f1 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2* oneLessT * t.
		f3 _ t * t.
		x _ (f1 * xFrom) + (f2 * xControl) + (f3 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl) + (f3 * yTo).
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 6/6/2020 17:24:46'!
updateAlphasForX: x y: y

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dxSquared dy pixelIndex dxp |

	"Compute affected rect. Honor clipRect"
	t _ (y -auxStrokeWidthDilatedHalf) truncated +1.
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y +auxStrokeWidthDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x -auxStrokeWidthDilatedHalf -subPixelDelta) truncated +1.
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x +auxStrokeWidthDilatedHalf +subPixelDelta) truncated.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  l.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaWord _ alphaMask bytesAt: pixelIndex.
			alphaWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00] ifFalse: [
				doUpdate _ false.
				dy _ displayY - y.
				dySquared _ dy * dy.
				dx _ displayX - x.
				"Red"
				dxp _ dx - subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) rounded ].
						alpha > (alphaWord at: 3) ifTrue: [
							alphaWord at: 3 put: alpha.
							doUpdate _ true ]].
				"Green"
				dxSquared _ dx * dx.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) rounded ].
						alpha > (alphaWord at: 2) ifTrue: [
							alphaWord at: 2 put: alpha.
							doUpdate _ true ]].
				"Blue"
				dxp _ dx + subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) rounded ].
						alpha > (alphaWord at: 1) ifTrue: [
							alphaWord at: 1 put: alpha.
							doUpdate _ true ]].
				doUpdate ifTrue: [
					alphaMask bytesAt: pixelIndex put: alphaWord ]]]]! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 7/14/2020 09:57:29'!
updateEdgeCountAtX: x y: y
	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex increment countWord redCount greenCount blueCount redOffset greenOffset blueOffset |

	thisYTruncated _ y truncated .
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ self ].
	prevYTruncated ifNil: [ 
		^ prevYTruncated _ thisYTruncated ].
	thisYTruncated = prevYTruncated ifTrue: [
		^ self].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY _ thisYTruncated.
			increment _ 1 ]
		ifFalse: [
			pixelY _ prevYTruncated.
			increment _ -1 ].
	prevYTruncated _ thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase _ pixelY * targetWidth.
	redOffset _ (x+subPixelDelta) truncated + 1 max: clipLeft.
	greenOffset _ x truncated + 1 max: clipLeft.
	blueOffset _ (x-subPixelDelta) truncated + 1 max: clipLeft.
	redPixelIndex _ pixelIndexBase + redOffset +1.
	greenPixelIndex _ pixelIndexBase + greenOffset +1.
	bluePixelIndex _ pixelIndexBase + blueOffset +1.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord _ edgeCounts bytesAt: redPixelIndex.
				redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
				countWord at: 3 put: redCount.
				greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
				countWord at: 2 put: greenCount.
				blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
				countWord at: 1 put: blueCount.
				edgeCounts bytesAt: redPixelIndex put: countWord ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: bluePixelIndex.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: bluePixelIndex.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				]
		].! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 6/6/2020 17:24:58'!
updateEdgesAndBoundsForX: x y: y

"
Inlinear en senders. Extraer afectar bounds si es posible (linea, circulo)
"

	blendLeft _ blendLeft min: x.
	blendTop _ blendTop min: y.
	blendRight _ blendRight max: x.
	blendBottom _ blendBottom max: y.

	"Compute edges intersecting with this horizontal line, for fills."
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 8/6/2020 17:30:19'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	
	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		(pathFirstX = pathCurrentX and: [ pathFirstY = pathCurrentY ]) ifFalse: [
			"This is actually an error condition. If we are asked to fill, then the path should be closed.
			This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
			self basicClosePath ]].

	blendLeft _ (blendLeft - auxStrokeWidthDilatedHalf - subPixelDelta) truncated +1.
	blendTop _ (blendTop - auxStrokeWidthDilatedHalf) truncated +1.
	blendRight _ (blendRight + auxStrokeWidthDilatedHalf + subPixelDelta) truncated +1. "Make room not just for updated mask, but also edges (hence, +1)"
	blendBottom _ (blendBottom + auxStrokeWidthDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft _ morphBoundsLeft min: blendLeft.
	morphBoundsTop _ morphBoundsTop min: blendTop.
	morphBoundsRight _ morphBoundsRight max: blendRight.
	morphBoundsBottom _ morphBoundsBottom max: blendBottom.

	blendLeft< clipLeft ifTrue: [ blendLeft _ clipLeft ].
	blendTop < clipTop ifTrue: [ blendTop _ clipTop ].
	blendRight > clipRight ifTrue: [ blendRight _ clipRight ].
	blendBottom > clipBottom ifTrue: [ blendBottom _ clipBottom ].

	(blendRight >= blendLeft and: [ blendBottom >= blendTop ]) ifTrue: [
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self blendStrokeAndFill ]
				ifFalse: [ self blendFillOnly ]]
			ifFalse: [
				self blendStrokeOnly ]].

	pathFirstX _ 0.0.
	pathFirstY _ 0.0.
	pathCurrentX _ 0.0.
	pathCurrentY _ 0.0.
	previousCallX _ 0.0.
	previousCallY _ 0.0.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY.! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:21:13'!
initializePath

	super initializePath.

	blendLeft _ targetWidth.			"drawable right. Will later be refined."
	blendTop _ targetForm height.		"drawable bottom. Will later be refined."
	blendRight _ 0.						"drawable left. Will later be refined."
	blendBottom _ 0.						"drawable top. Will later be refined."! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:21:13'!
newTrajectoryFragment

	prevYTruncated _ nil.! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:21:13'!
pvtFillColor: aFillColorOrNil
	fillColor _ aFillColorOrNil! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/26/2020 16:11:50'!
pvtSetForm: aForm

	| s |
	super pvtSetForm: aForm.
	s _ targetForm width * targetForm height.
	alphaMask _ WordArray new: s.
	edgeCounts _ WordArray new: s.
	subPixelDelta _ 0.25. "Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:21:13'!
pvtStrokeColor: aStrokeColorOrNil
	strokeColor _ aStrokeColorOrNil! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:21:13'!
pvtStrokeWidth: aNumber

	| swErodedHalf |
	super pvtStrokeWidth: aNumber.
	auxStrokeWidthDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.
	auxStrokeWidthDilatedHalfSquared _ auxStrokeWidthDilatedHalf * auxStrokeWidthDilatedHalf.
	swErodedHalf _ strokeWidth - antiAliasingWidth * 0.5.
	auxStrokeWidthErodedHalfSquared _ swErodedHalf * swErodedHalf * swErodedHalf sign.! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 21:02:35'!
blendFillOnly

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixel _ edgeCounts bytesAt: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			edgesThisPixelR _ edgesThisPixel at: 3.
			edgesThisPixelG _ edgesThisPixel at: 2.
			edgesThisPixelB _ edgesThisPixel at: 1.

			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
			edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
			edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.
			isRedInside _ edgesUpToThisPixelR ~= 0.
			isGreenInside _ edgesUpToThisPixelG ~= 0.
			isBlueInside _ edgesUpToThisPixelB ~= 0.

			self
				blendFillOnlyAtX: displayX y: displayY pixelIndex: pixelIndex 
				redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 12/23/2020 15:20:12'!
blendFillOnlyAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends fill over background. Despite method selector, there might be a very thin stroke of the same color as the fill. Handle this by adjusting antiAliasAlphas.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

	antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.

	antiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [	"Not in the stroke"
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [ 		"Not inside"
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	isRedInside ifTrue: [ antiAliasRedAlphaBits _ 127 - antiAliasRedAlphaBits ].
	isGreenInside ifTrue: [ antiAliasGreenAlphaBits _ 127 - antiAliasGreenAlphaBits ].
	isBlueInside ifTrue: [ antiAliasBlueAlphaBits _ 127 - antiAliasBlueAlphaBits ].
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.
	alphaR _ antiAliasRedAlpha * fillColor alpha.
	alphaG _ antiAliasGreenAlpha * fillColor alpha.
	alphaB _ antiAliasBlueAlpha * fillColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * fillColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * fillColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * fillColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 21:02:31'!
blendStrokeAndFill

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixel _ edgeCounts bytesAt: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			edgesThisPixelR _ edgesThisPixel at: 3.
			edgesThisPixelG _ edgesThisPixel at: 2.
			edgesThisPixelB _ edgesThisPixel at: 1.

			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
			edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
			edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.
			isRedInside _ edgesUpToThisPixelR ~= 0.
			isGreenInside _ edgesUpToThisPixelG ~= 0.
			isBlueInside _ edgesUpToThisPixelB ~= 0.

			self
				blendStrokeAndFillAtX: displayX y: displayY pixelIndex: pixelIndex 
				redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 12/23/2020 15:20:25'!
blendStrokeAndFillAtX: x y: y pixelIndex: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Do an appropriate (anti aliased) mix between stoke and fill (or pick just stroke or just fill). Blend this over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |

	antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.
	antiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR _ antiAliasRedAlpha * strokeColor alpha + ((1-antiAliasRedAlpha) * fillColor alpha).
			foreR _ antiAliasRedAlpha * strokeColor red * 255 + ((1-antiAliasRedAlpha) * fillColor red * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaR _ antiAliasRedAlpha * strokeColor alpha.
			foreR _ strokeColor red * 255 ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG _ antiAliasGreenAlpha * strokeColor alpha + ((1-antiAliasGreenAlpha) * fillColor alpha).
			foreG _ antiAliasGreenAlpha * strokeColor green * 255 + ((1-antiAliasGreenAlpha) * fillColor green * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaG _ antiAliasGreenAlpha * strokeColor alpha.
			foreG _ strokeColor green * 255 ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB _ antiAliasBlueAlpha * strokeColor alpha + ((1-antiAliasBlueAlpha) * fillColor alpha).
			foreB _ antiAliasBlueAlpha * strokeColor blue * 255 + ((1-antiAliasBlueAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaB _ antiAliasBlueAlpha * strokeColor alpha.
			foreB _ strokeColor blue * 255 ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isGreenInside
				ifTrue: [clippingAntiAlias _ 127]
				ifFalse: [
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * foreR + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].
		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * foreG + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].
		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * foreB + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 18:07:27'!
blendStrokeOnly

	| pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			self blendStrokeOnlyAtX: displayX y: displayY pixelIndex: pixelIndex.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 12/23/2020 15:20:35'!
blendStrokeOnlyAtX: x y: y pixelIndex: pixelIndex

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

	antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.
	antiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
		^self].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.
	alphaR _ antiAliasRedAlpha * strokeColor alpha.
	alphaG _ antiAliasGreenAlpha * strokeColor alpha.
	alphaB _ antiAliasBlueAlpha * strokeColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * strokeColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * strokeColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * strokeColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineSubPixel class methodsFor: 'instance creation' stamp: 'jmv 6/17/2020 14:34:51'!
defaultAntiAliasingWidth
	^1.4! !

!VectorEngineWholePixel methodsFor: 'accessing' stamp: 'jmv 6/6/2020 17:37:09'!
antiAliasingWidth: aNumber

	super antiAliasingWidth: aNumber.
	auxAntiAliasingWidthScaledInverse _ 127.0 / antiAliasingWidth.! !

!VectorEngineWholePixel methodsFor: 'accessing' stamp: 'jmv 11/17/2020 15:20:43'!
clipCurrentMorph: aBoolean
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex _ displayY * targetWidth +  clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				pixelIndex _ pixelIndex + 1.
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) >>8 << 8) ]]].
	clipCurrentMorph _ aBoolean! !

!VectorEngineWholePixel methodsFor: 'accessing' stamp: 'jmv 8/6/2020 17:43:30'!
clipRect: aRectangle 
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipLeft _ aRectangle left asInteger max: 0.
	clipTop _ aRectangle top asInteger max: 0.
	clipRight _ (aRectangle right asInteger min: targetWidth) - 1.
	clipBottom _ (aRectangle bottom asInteger min: targetForm height) - 1! !

!VectorEngineWholePixel methodsFor: 'accessing' stamp: 'jmv 12/1/2020 10:46:19'!
currentMorph: aMorph
	currentMorphId _ aMorph ifNil: [0] ifNotNil: [aMorph morphId].
	currentClipsSubmorphs _ aMorph notNil and: [aMorph clipsSubmorphsReally].! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:37:09'!
primArcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle rotationCos: thetaCos rotationSin: thetaSin hops: hops angleDelta: d

	| angle xp yp x y |
	angle _ startAngle.
	hops+1 timesRepeat: [
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		x _ thetaCos * xp - (thetaSin * yp) + cx.
		y _ thetaSin * xp + (thetaCos * yp) + cy.
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		angle _ angle + d. ].! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:37:09'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 hops: hops
	| t oneLessT increment f2 f3 f1 f23 f4 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * xFrom) + (f2 * xControl1) + (f3 * xControl2) + (f4 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl1) + (f3 * yControl2) + (f4 * yTo).
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:37:09'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo hops: hops incrementX: incrementX incrementY: incrementY

	| x y |
	x _ xFrom.
	y _ yFrom.
	hops timesRepeat: [
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		x _ x + incrementX.
		y _ y + incrementY ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 6/6/2020 17:37:09'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl hops: hops
	| t oneLessT increment f2 f3 f1 x y |
	t _ 0.0.
	increment _ 1.0 / hops.
	hops timesRepeat: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2* oneLessT * t.
		f3 _ t * t.
		x _ (f1 * xFrom) + (f2 * xControl) + (f3 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl) + (f3 * yTo).
		self updateAlphasForX: x y: y.
		self updateEdgesAndBoundsForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	self updateEdgesAndBoundsForX: xTo y: yTo.! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 6/6/2020 21:02:57'!
updateAlphasForX: x y: y

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex |

	"Compute affected rect. Honor clipRect"
	t _ (y -auxStrokeWidthDilatedHalf) truncated +1.
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y +auxStrokeWidthDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x -auxStrokeWidthDilatedHalf) truncated +1.
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x +auxStrokeWidthDilatedHalf) truncated.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  l.
		dy _ displayY - y.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			prevAlpha _ alphaMask at: pixelIndex.
			prevAlpha = 16r7F ifFalse: [
				dx _ displayX - x.
				distanceToAxisSquared _ (dx*dx) + (dy*dy).
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) rounded ].
						alpha > prevAlpha ifTrue: [
							alphaMask at: pixelIndex put: alpha ]]]]]! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 7/14/2020 09:50:49'!
updateEdgesAndBoundsForX: x y: y

	| thisYTruncated pixelY pixelIndex increment count pixelX |

	blendLeft _ blendLeft min: x.
	blendTop _ blendTop min: y.
	blendRight _ blendRight max: x.
	blendBottom _ blendBottom max: y.

	"Compute edges intersecting with this horizontal line, for fills."
	fillColor notNil ifTrue: [
		thisYTruncated _ y truncated.
		(thisYTruncated between: clipTop-1 and: clipBottom) ifTrue: [
			prevYTruncated isNil
				ifTrue: [ prevYTruncated _ thisYTruncated ]
				ifFalse: [
					thisYTruncated = prevYTruncated ifFalse: [
						"All edge count at the left of the clipRect are added there (at the left of the clipRect).
						The effect is the same, and we need to clean up less stuff afterwards.
						More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
						pixelX _ x truncated + 1 max: clipLeft.
						pixelX <= clipRight ifTrue: [
							pixelY _ thisYTruncated max: prevYTruncated.
							increment _ (thisYTruncated - prevYTruncated) sign.
							pixelIndex _ pixelY * targetWidth + pixelX +1.
							count _ edgeCounts at: pixelIndex.
							count _ count + increment bitAnd: 16rFF.
							edgeCounts at: pixelIndex put: count ].
						prevYTruncated _ thisYTruncated.
						]]]].! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 8/6/2020 17:30:26'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	
	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		(pathFirstX = pathCurrentX and: [ pathFirstY = pathCurrentY ]) ifFalse: [
			"This is actually an error condition. If we are asked to fill, then the path should be closed.
			This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
			self basicClosePath ]].

	blendLeft _ (blendLeft - auxStrokeWidthDilatedHalf) truncated +1.
	blendTop _ (blendTop - auxStrokeWidthDilatedHalf) truncated +1.
	blendRight _ (blendRight + auxStrokeWidthDilatedHalf) truncated +1. "Make room not just for updated mask, but also edges (hence, +1)"
	blendBottom _ (blendBottom + auxStrokeWidthDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft _ morphBoundsLeft min: blendLeft.
	morphBoundsTop _ morphBoundsTop min: blendTop.
	morphBoundsRight _ morphBoundsRight max: blendRight.
	morphBoundsBottom _ morphBoundsBottom max: blendBottom.

	blendLeft< clipLeft ifTrue: [ blendLeft _ clipLeft ].
	blendTop < clipTop ifTrue: [ blendTop _ clipTop ].
	blendRight > clipRight ifTrue: [ blendRight _ clipRight ].
	blendBottom > clipBottom ifTrue: [ blendBottom _ clipBottom ].

	(blendRight >= blendLeft and: [ blendBottom >= blendTop ]) ifTrue: [
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self blendStrokeAndFill ]
				ifFalse: [ self blendFillOnly ]]
			ifFalse: [
				self blendStrokeOnly ]].

	pathFirstX _ 0.0.
	pathFirstY _ 0.0.
	pathCurrentX _ 0.0.
	pathCurrentY _ 0.0.
	previousCallX _ 0.0.
	previousCallY _ 0.0.
	previousCallControlX _ previousCallX.
	previousCallControlY _ previousCallY.! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:37:09'!
initializePath

	super initializePath.

	blendLeft _ targetWidth.			"drawable right. Will later be refined."
	blendTop _ targetForm height.		"drawable bottom. Will later be refined."
	blendRight _ 0.						"drawable left. Will later be refined."
	blendBottom _ 0.						"drawable top. Will later be refined."! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:37:09'!
newTrajectoryFragment

	prevYTruncated _ nil.! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:37:09'!
pvtFillColor: aFillColorOrNil
	fillColor _ aFillColorOrNil! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/26/2020 16:11:56'!
pvtSetForm: aForm

	| s |
	super pvtSetForm: aForm.
	s _ targetForm width * targetForm height.
	alphaMask _ ByteArray new: s.
	edgeCounts _ ByteArray new: s.! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:37:09'!
pvtStrokeColor: aStrokeColorOrNil
	strokeColor _ aStrokeColorOrNil! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 6/6/2020 17:37:09'!
pvtStrokeWidth: aNumber

	| swErodedHalf |
	super pvtStrokeWidth: aNumber.
	auxStrokeWidthDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.
	auxStrokeWidthDilatedHalfSquared _ auxStrokeWidthDilatedHalf * auxStrokeWidthDilatedHalf.
	swErodedHalf _ strokeWidth - antiAliasingWidth * 0.5.
	auxStrokeWidthErodedHalfSquared _ swErodedHalf * swErodedHalf * swErodedHalf sign.! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 21:02:05'!
blendFillOnly

	| edgesUpToThisPixel edgesThisPixel pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCounts at: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ].
			edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel.

			self blendFillOnlyAtX: displayX y: displayY pixelIndex: pixelIndex isInside: edgesUpToThisPixel ~= 0.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 12/23/2020 15:20:44'!
blendFillOnlyAtX: x y: y pixelIndex: pixelIndex isInside: isInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends fill over background. Despite method selector, there might be a very thin stroke of the same color as the fill. Handle this by adjusting antiAliasAlphas.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| resultR resultG resultB antiAliasAlphaBits antiAliasAlpha alpha unAlpha resultAlpha targetWord targetAlpha clippingAntiAlias |
	antiAliasAlphaBits _ alphaMask at: pixelIndex.
	antiAliasAlphaBits = 16r00 ifTrue: [	"Not in the stroke"
		isInside ifFalse: [ 		"Not inside"
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	isInside ifTrue: [
		antiAliasAlphaBits _ 127 - antiAliasAlphaBits ].

	antiAliasAlpha _ antiAliasAlphaBits / 127.0.
	alpha _ antiAliasAlpha * fillColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.
		
		resultR _ alpha * fillColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * fillColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * fillColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 18:08:12'!
blendStrokeAndFill

	| edgesUpToThisPixel edgesThisPixel pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCounts at: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ].
			edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel.

			self blendStrokeAndFillAtX: displayX y: displayY pixelIndex: pixelIndex
				isInside: edgesUpToThisPixel ~= 0.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 12/23/2020 15:20:56'!
blendStrokeAndFillAtX: x y: y pixelIndex: pixelIndex isInside: isInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Do an appropriate (anti aliased) mix between stoke and fill (or pick just stroke or just fill). Blend this over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlphaBits antiAliasAlpha alpha unAlpha foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |
	antiAliasAlphaBits _ alphaMask at: pixelIndex.
	antiAliasAlphaBits = 16r00  ifTrue: [
		isInside ifFalse: [
			^self]].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasAlpha _ antiAliasAlphaBits / 127.0.

	isInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha _ antiAliasAlpha * strokeColor alpha + ((1-antiAliasAlpha) * fillColor alpha).
			foreR _ antiAliasAlpha * strokeColor red * 255 + ((1-antiAliasAlpha) * fillColor red * 255).
			foreG _ antiAliasAlpha * strokeColor green * 255 + ((1-antiAliasAlpha) * fillColor green * 255).
			foreB _ antiAliasAlpha * strokeColor blue * 255 + ((1-antiAliasAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alpha _ antiAliasAlpha * strokeColor alpha.
			foreR _ strokeColor red * 255.
			foreG _ strokeColor green * 255.
			foreB _ strokeColor blue * 255 ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isInside
				ifTrue: [clippingAntiAlias _ 127]
				ifFalse: [
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.

		resultR _ alpha * foreR + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * foreG + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * foreB + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 11/16/2020 18:08:30'!
blendStrokeOnly

	| pixelIndex |
	blendTop to: blendBottom do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  blendLeft.
		blendLeft to: blendRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			self blendStrokeOnlyAtX: displayX y: displayY pixelIndex: pixelIndex.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 1/14/2021 18:04:43'!
blendStrokeOnlyAtX: x y: y pixelIndex: pixelIndex

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlphaBits antiAliasAlpha alpha unAlpha resultAlpha targetAlpha clippingAntiAlias |
	antiAliasAlphaBits _ alphaMask at: pixelIndex.
	antiAliasAlphaBits = 16r00  ifTrue: [
		^self].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasAlpha _ antiAliasAlphaBits / 127.0.

	alpha _ antiAliasAlpha * strokeColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.

		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.

		resultR _ alpha * strokeColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * strokeColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * strokeColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ]].! !

!VectorEngineWholePixel class methodsFor: 'instance creation' stamp: 'jmv 6/17/2020 14:58:55'!
defaultAntiAliasingWidth
	^1.7! !

!PathCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:15'!
buildFrom: commandChar stream: aStream

	coordinatesAreRelative _ commandChar isLowercase! !

!PathCommand methodsFor: 'building' stamp: 'jmv 5/4/2010 06:16'!
buildFromStream: aStream in: parentToBe

	coordinatesAreRelative _ aStream next isLowercase! !

!PathCommand methodsFor: 'as yet unclassified' stamp: 'jmv 5/6/2011 22:18'!
coordinatesAreRelative
^coordinatesAreRelative! !

!PathCommand methodsFor: 'private' stamp: 'jmv 5/11/2010 10:05'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	self subclassResponsibility! !

!PathCommand methodsFor: 'drawing' stamp: 'jmv 11/9/2012 17:09'!
runOn: aCanvas

	self subclassResponsibility! !

!PathCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:03'!
classFor: anSVGPathCommandName

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGPathCommandName asUppercase
			ifTrue: [ ^cls ] ].
	^nil! !

!PathCommand class methodsFor: 'instance creation' stamp: 'jmv 7/14/2019 14:30:24'!
from: commandChar stream: aStream
	| cls|
	cls _ self classFor: commandChar.
	cls ifNil: [
		PathCommandError signal: ('Unhandled path command: 'copyWith: commandChar).
		^nil ].
	^cls new buildFrom: commandChar stream: aStream
	! !

!PathCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:03'!
svgName
	"Answer the name of the svg elements we can represent"

	^self subclassResponsibility! !

!PathArcCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:41'!
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

!PathArcCommand methodsFor: 'private' stamp: 'jmv 6/15/2010 15:30'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

"si los puntos son relativos, tirar una excepcion...
Y completar los que falten!!"
self flag: #revisarM3.
	toPoints do: aBlock! !

!PathArcCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:07:26'!
runOn: aCanvas

	aCanvas multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngless: useIncreasingAngless! !

!PathArcCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$A! !

!PathCloseCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:10'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream! !

!PathCloseCommand methodsFor: 'private' stamp: 'jmv 5/11/2010 10:05'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."! !

!PathCloseCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:07:07'!
runOn: aCanvas

	aCanvas closePath! !

!PathCloseCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$Z! !

!PathCurveToCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 09:56'!
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

!PathCurveToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 09:57'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!PathCurveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:51'!
runOn: aCanvas

	aCanvas multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points! !

!PathCurveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$C! !

!PathCurveToSmoothCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 09:59'!
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

!PathCurveToSmoothCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:01'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!PathCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:33'!
runOn: aCanvas

	aCanvas multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points! !

!PathCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$S! !

!PathLineToCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:33'!
buildFrom: commandChar stream: aStream

	| px py |
	super buildFrom: commandChar stream: aStream.
	points _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !

!PathLineToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 09:32'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."
	"Atencion: Si son relativas todo esto esta mal... Igual, lo que quiero es reimplementar todo"

	points do: aBlock! !

!PathLineToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:05:08'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative points: points! !

!PathLineToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$L! !

!PathLineToHCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:45'!
buildFrom: commandChar stream: aStream

	| px |
	super buildFrom: commandChar stream: aStream.
	xs _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		px notNil ] whileTrue:
			[ strm nextPut: px ]]! !

!PathLineToHCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:04:56'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative xs: xs! !

!PathLineToHCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$H! !

!PathLineToVCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:45'!
buildFrom: commandChar stream: aStream

	| py |
	super buildFrom: commandChar stream: aStream.
	ys _ Array streamContents: [ :strm |
		[py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: py ]]! !

!PathLineToVCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:04:32'!
runOn: aCanvas

	aCanvas multiLineTo: coordinatesAreRelative ys: ys! !

!PathLineToVCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$V! !

!PathMoveToCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:34'!
buildFrom: commandChar stream: aStream

	| px py |
	super buildFrom: commandChar stream: aStream.
	points _ Array streamContents: [ :strm |
		[px _ aStream nextNumber.
		py _ aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !

!PathMoveToCommand methodsFor: 'building' stamp: 'jmv 4/9/2019 13:36:26'!
toPoint: endPoint
	coordinatesAreRelative _ false.
	points _ {endPoint}! !

!PathMoveToCommand methodsFor: 'building' stamp: 'jmv 3/2/2020 11:41:49'!
toRelativePoint: endPoint
	coordinatesAreRelative _ true.
	points _ {endPoint}! !

!PathMoveToCommand methodsFor: 'private' stamp: 'jmv 6/11/2010 14:34'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	points do: aBlock! !

!PathMoveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:03:47'!
runOn: aCanvas

	aCanvas multiMoveTo: coordinatesAreRelative points: points! !

!PathMoveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$M! !

!PathQuadraticCurveToCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:12'!
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

!PathQuadraticCurveToCommand methodsFor: 'building' stamp: 'jmv 4/9/2019 13:30:05'!
toPoints: endPoints controlPoints: aControlPoints
	coordinatesAreRelative _ false.
	toPoints _ endPoints.
	controlPoints _ aControlPoints! !

!PathQuadraticCurveToCommand methodsFor: 'building' stamp: 'jmv 2/29/2020 18:01:39'!
toRelativePoints: endPoints controlPoints: aControlPoints
	coordinatesAreRelative _ true.
	toPoints _ endPoints.
	controlPoints _ aControlPoints! !

!PathQuadraticCurveToCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:14'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!PathQuadraticCurveToCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:03:23'!
runOn: aCanvas

	aCanvas multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints! !

!PathQuadraticCurveToCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:08'!
svgName

	^$Q! !

!PathQuadraticCurveToSmoothCommand methodsFor: 'building' stamp: 'jmv 6/14/2010 10:15'!
buildFrom: commandChar stream: aStream

	| x y |
	super buildFrom: commandChar stream: aStream.
	toPoints _ OrderedCollection new.
	[
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y]! !

!PathQuadraticCurveToSmoothCommand methodsFor: 'private' stamp: 'jmv 6/14/2010 10:17'!
privatePointsDo: aBlock
	"evaluate aBlock with the x and y coordinates for our defining points."

	toPoints do: aBlock! !

!PathQuadraticCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:02:58'!
runOn: aCanvas

	aCanvas multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints! !

!PathQuadraticCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:09'!
svgName

	^$T! !

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

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 3/2/2020 13:16:44'!
initialMoveToCommand: anAffineTransformationOrNil
	"TrueType reasonably assumes Y coordinates increase upwards."

	| p |
	p _ points first.
	anAffineTransformationOrNil notNil ifTrue: [
		p _ anAffineTransformationOrNil transform: p ].
	^ PathMoveToCommand new toRelativePoint: p! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 4/22/2019 20:36:47'!
isEmpty
	^points size < 2! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/10/2020 11:08:15'!
pathCommand: anAffineTransformationOrNil
	"TrueType reasonably assumes Y coordinates increase upwards."

	| prevEndPoint endPoints controlPoints cp ep |
	prevEndPoint _ points first.
	anAffineTransformationOrNil notNil ifTrue: [
		prevEndPoint _ anAffineTransformationOrNil transform: prevEndPoint ].
	endPoints _ OrderedCollection new.
	controlPoints _ OrderedCollection new.
	self quadraticBeziersDo: [ :endPoint :controlPoint |
		ep _ endPoint.
		cp _ controlPoint.
		anAffineTransformationOrNil notNil ifTrue: [
			ep _ anAffineTransformationOrNil transform: ep.
			cp _ anAffineTransformationOrNil transform: cp ].
		endPoints add: ep - prevEndPoint.
		controlPoints add:  cp - prevEndPoint.
		prevEndPoint _ ep ].
	^ PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/10/2020 11:08:03'!
quadraticBeziersDo: aBlock
	"Iterate over Bezier curves of the receiver.
	aBlock has two arguments: endPoint and controlPoint.
	StartPoint is assumed to be previous endPoint, or points first for the first curve.
	Contour parts may either be straight line segments or quadratic bezier curves.
	The decision is made upon the type flags in controlPointFlags as follows:
	a) Two subsequent #isEndPoint points define a straight segment
	b) An #isEndPoint point followed by an #isControlPoint point followed 
		by an #isEndPoint point defines a quadratic bezier segment
	c)Two subsequent #isControlPoint points have an implicitely defined 
		#isEndPoint point at half the distance between them"
	| firstEndPoint pointIsControlPoint previousPoint previousPointWasControlPoint |
	firstEndPoint _ points at: 1.
	previousPoint _ firstEndPoint.
	previousPointWasControlPoint _ controlPointFlags bitBooleanAt: 1.
	2 to: points size do: [ :i | | point |
		point _ points at: i.
		pointIsControlPoint _ controlPointFlags bitBooleanAt: i.
		pointIsControlPoint
			ifTrue: [
				previousPointWasControlPoint
					ifTrue: [	"Quadratic Bezier"
						aBlock value: point + previousPoint / 2.0 value: previousPoint ]]
			ifFalse: [
				previousPointWasControlPoint
					ifTrue: [	"Quadratic Bezier"
						aBlock value: point value: previousPoint ]
					ifFalse: [	"Line segment"
						aBlock value: point value: point ]].
		previousPoint _ point.
		previousPointWasControlPoint _ pointIsControlPoint.
	].
	(previousPoint = firstEndPoint) ifFalse: [
		previousPointWasControlPoint
			ifTrue: [
				aBlock value: firstEndPoint value: previousPoint ]
			ifFalse: [
				aBlock value: firstEndPoint value: firstEndPoint ]]! !

!TTContourConstruction methodsFor: 'primitive format' stamp: 'jmv 5/10/2020 11:08:11'!
addContourDataTo: stream tx: aGeometryTransformationOrNil
	| bezierCount firstPoint cp ep prevEndPoint cpr epr |
	self isEmpty ifTrue: [
		^self ].

	bezierCount _ 0.
	self quadraticBeziersDo: [ :endPoint :controlPoint | bezierCount _ bezierCount + 1 ].
	stream nextPut: bezierCount.

	firstPoint _ points first.
	aGeometryTransformationOrNil ifNotNil: [
		firstPoint _ aGeometryTransformationOrNil transform: firstPoint ].
	stream nextPut: firstPoint x; nextPut: firstPoint y.


	prevEndPoint _ firstPoint.
	self quadraticBeziersDo: [ :endPoint :controlPoint |
		ep _ endPoint.
		cp _ controlPoint.
		aGeometryTransformationOrNil notNil ifTrue: [
			ep _ aGeometryTransformationOrNil transform: ep.
			cp _ aGeometryTransformationOrNil transform: cp ].
		epr _ ep - prevEndPoint.
		cpr _ cp - prevEndPoint.
		stream nextPut: epr x; nextPut: epr y.
		stream nextPut: cpr x; nextPut: cpr y.
		prevEndPoint _ ep ].! !

!TTContourConstruction class methodsFor: 'instance creation'!
on: points

	^self new points: points! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 3/13/2020 14:18:19'!
contourDataForIso8859s15
	^contourDataForIso8859s15! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 3/12/2020 12:45:59'!
contourDataForUtf8
	^contourDataForUtf8! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 3/4/2020 12:06:14'!
contourDataIndexesByIso8859s15
	^contourDataIndexesByIso8859s15! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 3/18/2020 20:19:27'!
contourDataIndexesByIso8859s15ST80Glyphs: aBoolean
	^ aBoolean
		ifTrue: [contourDataIndexesForSmalltalkGlyphs]
		ifFalse: [contourDataIndexesByIso8859s15]! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 3/12/2020 12:46:10'!
contourDataIndexesByUtf8
	^contourDataIndexesByUtf8 ! !

!TTFontDescription methodsFor: 'accessing' stamp: 'yo 12/7/2007 10:33'!
first

	^ self.
! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 6/16/2020 18:33:27'!
folderName
	^folderName! !

!TTFontDescription methodsFor: 'accessing' stamp: 'jmv 6/16/2020 18:33:31'!
folderName: aString
	folderName _ aString! !

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

!TTFontDescription methodsFor: 'properties' stamp: 'jmv 3/14/2020 23:23:22'!
letterMTopSideBearing
	"This number is what needs to be scaled by i.e. 12 to render a 12 pixel font"

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

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 3/13/2020 09:16:46'!
appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex
		glyphsArray withIndexDo: [ :glyphOrArrayOrNil :index |
			glyphOrArrayOrNil notNil ifTrue: [
				glyphOrArrayOrNil isArray
					ifTrue: [
						| nextBaseIndex |
						nextBaseIndex := indexes size - 128.
						indexes addAll: (IntegerArray new: 64).
						indexes at: baseIndex + index put: nextBaseIndex negated.
						self appendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNil indexes: indexes base: nextBaseIndex
					]
					ifFalse: [
						indexes at: baseIndex + index put: stream position+1.
						glyphOrArrayOrNil addContourDataTo: stream.
					]
				]]! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 9/3/2020 17:12:45'!
glyphsByIso8859s15: array8 glyphsByUtf8Bytes: arrayUtf8 unknownGlyph: theUnknownGlyph

	| mGlyph |
	
	mGlyph _ array8 at: $M numericValue + 1.
	letterMTopSideBearing _ mGlyph topSideBearing.

	contourDataIndexesByIso8859s15 _ IntegerArray new: array8 size.
	contourDataForIso8859s15 _ Float32Array streamContents: [ :stream |
		theUnknownGlyph addContourDataTo: stream. 	"First data is for unknownGlyph"
		array8 withIndexDo: [ :ttGlyphOrNil :index |
			ttGlyphOrNil notNil
				ifTrue: [
					contourDataIndexesByIso8859s15 at: index put: stream position+1.
					ttGlyphOrNil addContourDataTo: stream ]
				ifFalse: [ contourDataIndexesByIso8859s15 at: index put: 1]]].
	self perform: Preferences assignmentGlyphSelector.

	"This takes a significant amout of space, makes loading slower, and we're still not able to take full advantage of it."
	Preferences loadOnlyLatinGlyphData ifFalse: [
		contourDataIndexesByUtf8 _ (IntegerArray new: 256) as: OrderedCollection.
		contourDataForUtf8 _ Float32Array streamContents: [ :stream |
			theUnknownGlyph addContourDataTo: stream.		"First data is for unknownGlyph"
			self appendGlyphDataTo: stream glyphsArray: arrayUtf8 indexes: contourDataIndexesByUtf8 base: 0 ].
		contourDataIndexesByUtf8 _ contourDataIndexesByUtf8 as: IntegerArray]! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 4/24/2019 20:12:58'!
setAscent: asc descent: desc lineGap: lgap
	ascent := asc.
	descent := desc.
	lineGap := lgap! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'ar 11/2/1998 00:28'!
setBounds: aRect unitsPerEm: aNumber
	bounds := aRect.
	unitsPerEm := aNumber.! !

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

!TTFontDescription methodsFor: 'private-initialization' stamp: 'len 5/22/2020 20:06:56'!
useAlwaysLeftArrow
	"Not only when Smalltalk glyphs requested."
	| arrowIndex |
	contourDataIndexesForSmalltalkGlyphs _ contourDataIndexesByIso8859s15 copy.
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowLeft numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesByIso8859s15 at: $_ numericValue + 1 put: arrowIndex.
		contourDataIndexesForSmalltalkGlyphs at: $_ numericValue + 1 put: arrowIndex ].
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowUp numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesForSmalltalkGlyphs at: $^ numericValue + 1 put: arrowIndex ].! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 3/18/2020 20:33:17'!
useLeftArrow
	"When Smalltalk glyphs requested"
	| arrowIndex |
	contourDataIndexesForSmalltalkGlyphs _ contourDataIndexesByIso8859s15 copy.
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowLeft numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesForSmalltalkGlyphs at: $_ numericValue + 1 put: arrowIndex ].
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowUp numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesForSmalltalkGlyphs at: $^ numericValue + 1 put: arrowIndex ].! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 3/18/2020 20:33:12'!
useRightArrow
	"When Smalltalk glyphs requested"
	| arrowIndex |
	contourDataIndexesForSmalltalkGlyphs _ contourDataIndexesByIso8859s15 copy.
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowRight numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesForSmalltalkGlyphs at: $_ numericValue + 1 put: arrowIndex ].
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowUp numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesForSmalltalkGlyphs at: $^ numericValue + 1 put: arrowIndex ].! !

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

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 12/17/2020 14:54:04'!
formGlyphFor: aCharacter useST80Glyphs: aBoolean pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i |

	i _ (self contourDataIndexesByIso8859s15ST80Glyphs: aBoolean) at: aCharacter numericValue + 1.
	advanceWidth _ contourDataForIso8859s15 at: i. i _ i + 1.
	boundsLeft _ contourDataForIso8859s15 at: i. i _ i + 1.
	boundsRight _ contourDataForIso8859s15 at: i. i _ i + 1.
	boundsTop _ contourDataForIso8859s15 at: i. i _ i + 1.
	boundsBottom _ contourDataForIso8859s15 at: i. i _ i + 1.

	horizontalStrokesAtPixelCenter _ 0.5.
	xtraSpaceForAntiAliasingAndRounding _ 3.
	toPixelsScale _ pixelSize * 1.0 / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset _ (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth _ (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset _ (boundsBottom * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight _ (boundsBottom - boundsTop) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
	form _ Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	canvas _ VectorCanvas onForm: form.
	canvas doSubPixelAntiAliasing.
	canvas 
		engine
			displayString: aCharacter asString
			from: 1
			to: 1
			at: leftPixelOffset negated@(topPixelOffset - horizontalStrokesAtPixelCenter)
			trueTypeFont: self
			useST80Glyphs: aBoolean
			color: Color black
			scale: 1.0 * pixelSize
			preventMirroring: true.
	^FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale)! !

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 6/8/2020 12:25:24'!
formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i |

	i _ contourDataIndexesByUtf8 at: byte1 + 1.
	utf8ByteArray _ ByteArray with: byte1.
	byte2OrNil notNil ifTrue: [
		i _ contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil.
		byte3OrNil notNil ifTrue: [
			i _ contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil with: byte3OrNil.
			byte4OrNil notNil ifTrue: [
				i _ contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1.
				utf8ByteArray _ ByteArray with: byte1 with: byte2OrNil with: byte3OrNil with: byte4OrNil ]]].

	advanceWidth _ contourDataForUtf8 at: i. i _ i + 1.
	boundsLeft _ contourDataForUtf8 at: i. i _ i + 1.
	boundsRight _ contourDataForUtf8 at: i. i _ i + 1.
	boundsTop _ contourDataForUtf8 at: i. i _ i + 1.
	boundsBottom _ contourDataForUtf8 at: i. i _ i + 1.

	horizontalStrokesAtPixelCenter _ 0.5.
	xtraSpaceForAntiAliasingAndRounding _ 3.
	toPixelsScale _ pixelSize * 1.0 / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset _ (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth _ (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset _ (boundsBottom * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight _ (boundsBottom - boundsTop) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
	form _ Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	canvas _ VectorCanvas onForm: form.
	canvas doSubPixelAntiAliasing.
	canvas 
		engine
			displayUtf8: utf8ByteArray
			at: leftPixelOffset negated@(topPixelOffset - horizontalStrokesAtPixelCenter)
			trueTypeFont: self
			color: Color black
			scaleX: 1.0 * pixelSize
			scaleY: -1.0 * pixelSize.
	^FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale)! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 3/18/2020 20:25:08'!
widthOf: aCharacter useST80Glyphs: aBoolean

	| i advanceWidth |
	i _ (self contourDataIndexesByIso8859s15ST80Glyphs: aBoolean) at: aCharacter numericValue + 1.
	advanceWidth _ contourDataForIso8859s15 at: i.
	^ advanceWidth / letterMTopSideBearing! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 4/1/2020 12:06:13'!
widthOfCodePoint: aUnicodeCodePoint

	| i advanceWidth |
	i _ contourDataIndexesByUtf8 at: aUnicodeCodePoint + 1.
	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |

			i _ contourDataIndexesByUtf8 at: byte1 + 1.
			byte2OrNil notNil ifTrue: [
				i _ contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
				byte3OrNil notNil ifTrue: [
					i _ contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
					byte4OrNil notNil ifTrue: [
						i _ contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1 ]]]]
		withUtf8BytesOfUnicodeCodePoint: aUnicodeCodePoint.
	advanceWidth _ contourDataForUtf8 at: i.
	^ advanceWidth / letterMTopSideBearing! !

!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 6/16/2020 18:35:35'!
read: aDirectoryEntry
	"
	TTFontDescription read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'
	"
	| readFamilyNames |
	Descriptions ifNil: [ Descriptions _ Dictionary new ].
	readFamilyNames _ Set new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf _ TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/18/2020 14:17:01'!
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 4/21/2020 14:07:14'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByIso5559s15 glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint |
	unknownGlyph _ glyphs first.
	nullGlyph _ glyphs second.
	mapSize _ characterMap size.
	glyphsByIso5559s15 _ Array new: 256.
	glyphsByUTF8 _ Array new: 256.
	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint _ codePoint.
		codePoint = $- codePoint ifTrue: [ preferredCodePoint _ 16r2212 ]. "WIDE MINUS"
		codePoint = $* codePoint ifTrue: [ preferredCodePoint _ 16r2217 ]. "CENTERED ASTERISK"
		g _ glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g _ glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		g == unknownGlyph ifFalse: [
			(Character iso8859s15CodeForUnicodeCodePoint: codePoint) ifNotNil: [ :iso8859s15 |
				glyphsByIso5559s15 at: iso8859s15 + 1 put: g ]].

		array _ glyphsByUTF8.
		prevByte _ nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array _ (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte _ byte.
				]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph _ g ].

	].
	Character separators do: [ :s |
		glyphsByIso5559s15 at: s codePoint +1 put: spaceGlyph.
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph.
	].
	fontDescription glyphsByIso8859s15: glyphsByIso5559s15 glyphsByUtf8Bytes: glyphsByUTF8 unknownGlyph: unknownGlyph! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/18/2020 15:32:49'!
processCharacterMappingTable: entry
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| initialOffset nSubTables platformID platformSpecificEncodingID offset |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUShort.
	1 to: nSubTables do: [ :i |
		platformID := entry nextUShort.
		platformSpecificEncodingID := entry nextUShort.
		offset := entry nextULong.
		platformID = 0 ifTrue: [	"Only consider Unicode"
			"Go to the beginning of the table"
			entry offset: initialOffset + offset.
			^ self decodeCmapFmtTable: entry ]].
	^nil! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 2/28/2020 18:23:06'!
processCompositeGlyphFrom: entry
	"Read a composite glyph from the font data."
	| glyph flags glyphIndex hasInstructions ofsX ofsY iLen geometryTransformation bb bl br bt |
	glyph _ TTCompositeGlyph new.
	bl _ entry nextShort.
	bt _ entry nextShort.
	br _ entry nextShort.
	bb _ entry nextShort.
	glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
	hasInstructions := false.
	[
		flags := entry nextUShort.
		glyphIndex := entry nextUShort + 1.

		(flags allMask: 1 "ARG_1_AND_2_ARE_WORDS") ifTrue: [
			"'Hay Offset short x@y' print."
			ofsX := entry nextShort.
			ofsY := entry nextShort.
		] ifFalse: [
			"'Hay Offset byte x@y' print."
			(ofsX := entry nextByte) > 127 ifTrue: [ofsX := ofsX - 256].
			(ofsY := entry nextByte) > 127 ifTrue: [ofsY := ofsY - 256]].

		(flags allMask: 2 "ARGS_ARE_XY_VALUES") ifFalse: [self halt]. "Unsupported"
		"ROUND_XY_TO_GRID = 4. Ignored"
		(flags anyMask: 8+64+128) ifFalse: [
			geometryTransformation := MorphicTranslation withTranslation: ofsX@ofsY ].
		(flags allMask: 8 "WE_HAVE_A_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation withScale: entry nextShort asFloat / 16r4000 ].
		(flags allMask: 64 "WE_HAVE_AN_X_AND_Y_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation
				withPointScale: (entry nextShort asFloat / 16r4000)@(entry nextShort asFloat / 16r4000)
				position: ofsX@ofsY ].
		(flags allMask: 128 "WE_HAVE_A_TWO_BY_TWO") ifTrue: [
			"2x2 transformation"
			geometryTransformation := AffineTransformation new
				a11: entry nextShort asFloat / 16r4000;
				a21: entry nextShort asFloat / 16r4000;
				a12: entry nextShort asFloat / 16r4000;
				a22: entry nextShort asFloat / 16r4000;
				a13: ofsX;
				a23: ofsY ].

		glyph addGlyph: glyphIndex transformation: geometryTransformation.
		hasInstructions := hasInstructions or: [ flags allMask: 256 "WE_HAVE_INSTRUCTIONS"].
		"Continue as long as the MORE_COMPONENTS bit is set"
		flags allMask: 32 "MORE_COMPONENTS"
	] whileTrue.
	hasInstructions ifTrue: [
		"Ignore them"
		iLen _ entry nextUShort.
		entry skip: iLen].
	^glyph! !

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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/18/2020 19:48:15'!
processGlyphDataTable: entry offsets: offsetArray
	"Read the actual glyph data from the font.
	offsetArray contains the start offsets in the data for each glyph."
	| initialOffset |
	initialOffset _ entry offset.
	glyphs _ Array new: nGlyphs.
	'Reading glyph data' 
		displayProgressAt: 100@100
		from: 1 
		to: nGlyphs 
		during: [ :bar |
			| glyph glyphOffset nextOffset glyphLength numberOfContours |
			1 to: nGlyphs do: [ :glyphIndex |
				bar value: glyphIndex.
				glyphOffset _ offsetArray at: glyphIndex.
				nextOffset _ offsetArray at: glyphIndex+1.
				glyphLength _ nextOffset - glyphOffset.
				glyph _ glyphLength = 0
					ifTrue: [ TTGlyph new. ]
					ifFalse: [
						entry offset: initialOffset + glyphOffset.
						numberOfContours _ entry nextShort.
						numberOfContours >= 0
							ifTrue: [ self processSimpleGlyphContours: numberOfContours from: entry ]
							ifFalse: [ self processCompositeGlyphFrom: entry ]].
				glyphs at: glyphIndex put: glyph ]].
	glyphs do: [ :g | g replaceIndexesByComponentGlyphsIn: glyphs ]
	! !

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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 2/28/2020 10:46:04'!
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
	((covLow allMask: 2) or: [ covHigh ~= 0]) ifTrue: [^false].
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/18/2020 19:45:30'!
processSimpleGlyphContours: nContours from: entry

	| glyph endPts nPts iLength flags bb bl br bt |
	glyph _ TTGlyph new.
	bl _ entry nextShort.
	bt _ entry nextShort.
	br _ entry nextShort.
	bb _ entry nextShort.
	glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
	endPts _ Array new: nContours.
	1 to: nContours do: [ :i | endPts at: i put: entry nextUShort ].
	glyph initializeContours: nContours with: endPts.
	nContours = 0 ifTrue: [^ self].
	nPts _ endPts last + 1.
	iLength _ entry nextUShort. "instruction length"
	entry skip: iLength.
	flags _ self getGlyphFlagsFrom: entry size: nPts.
	self readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	self readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts.
	glyph makePointsAbsolute.
	^glyph! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/18/2020 21:19:39'!
readFrom: fontData fromOffset: offset

	| headerEntry maxProfileEntry nameEntry indexLocEntry charMapEntry glyphEntry horzHeaderEntry horzMetricsEntry kerningEntry glyphOffset characterMap numHMetrics indexToLocFormat |

	"Search the tables required to build the font"
	(headerEntry := self getTableDirEntry: 'head' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a header table'].
	(maxProfileEntry := self getTableDirEntry: 'maxp' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a maximum profile table'].
	(nameEntry := self getTableDirEntry: 'name' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a name table'].
	(indexLocEntry := self getTableDirEntry: 'loca' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a relocation table'].
	(charMapEntry := self getTableDirEntry: 'cmap' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a character map table'].
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData  offset: offset) == nil ifTrue: [
		^self error:'This font does not have a glyph table'].
	(horzHeaderEntry := self getTableDirEntry: 'hhea' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal header table'].
	(horzMetricsEntry := self getTableDirEntry: 'hmtx' from: fontData offset: offset) == nil ifTrue: [
		^self error:'This font does not have a horizontal metrics table'].
	(kerningEntry := self getTableDirEntry: 'kern' from: fontData offset: offset) == nil ifTrue: [
		"Transcript cr; show:'This font does not have a kerning table';endEntry" ].


	"Process the data"
	indexToLocFormat := self processFontHeaderTable: headerEntry.
	self processMaximumProfileTable: maxProfileEntry.
	self processNamingTable: nameEntry.
	glyphOffset := self processIndexToLocationTable: indexLocEntry format: indexToLocFormat.
	characterMap := self processCharacterMappingTable: charMapEntry.
	characterMap isNil
		ifTrue: [^self error:'This font has no Unicode character mappings'].
	self processGlyphDataTable: glyphEntry offsets: glyphOffset.
	numHMetrics := self processHorizontalHeaderTable: horzHeaderEntry.
	self processHorizontalMetricsTable: horzMetricsEntry length: numHMetrics.
	kerningEntry isNil 
		ifTrue: [kernPairs := #()]
		ifFalse: [self processKerningTable: kerningEntry].
	self mapAndStoreGlyphsUsing: characterMap.
	"jmv kernPairs refer to glyph number (in the .ttf file) while fontDescription glyphs are indexed by Unicode code point...
	Correct this before making any use of the kernPairs data."
	fontDescription setKernPairs: kernPairs.! !

!TTFontReader methodsFor: 'public' stamp: 'jmv 1/18/2020 15:13:49'!
readTTFFrom: aByteArray
	"Read the raw font byte data"
	fontDescription := TTFontDescription new.
	self readFrom: aByteArray fromOffset: 0.
	^ fontDescription! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 1/18/2020 15:07:33'!
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

!TTFontReader methodsFor: 'private' stamp: 'jmv 2/28/2020 10:45:12'!
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
		(flagBits allMask: 8) ifTrue:[
			repCount := entry nextByte.
			repCount timesRepeat:[
				index := index + 1.
				flags at: index put: flagBits]].
		index := index + 1].
	^flags! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 2/28/2020 11:05:54'!
readGlyphXCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
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
			xValue _ (flagBits allMask: 2)
				ifTrue: [ "one byte"
					(flagBits allMask: 16)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					"If bit four is set, then this coordinate is the same as the
					last one, so the relative offset (of zero) is stored.  If bit
					is not set, then read in two bytes and store it as a signed value."
					(flagBits allMask: 16)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextShort ]].
			contour points at: (j - startPoint + 1) putX: xValue.
		].
		startPoint _ endPoint + 1 ]! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 2/28/2020 11:05:57'!
readGlyphYCoords: entry glyph: glyph nContours: nContours flags: flags endPoints: endPts
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
			yValue _ (flagBits allMask: 4)
				ifTrue: [ "one byte"
					(flagBits allMask: 32)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					(flagBits allMask: 32)
						ifTrue: [ 0 ]
						ifFalse: [ entry nextShort ]].
			contour points at: (j - startPoint + 1) putY: yValue.
		].
		startPoint _ endPoint + 1 ]! !

!TTFontReader class methodsFor: 'instance creation' stamp: 'jmv 1/18/2020 14:18:14'!
readTTFFrom: aByteArray

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

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 3/3/2020 09:43:36'!
contourCount
	^contours count: [ :each | each isEmpty not ]! !

!TTGlyph methodsFor: 'accessing'!
contours
	^contours! !

!TTGlyph methodsFor: 'accessing' stamp: 'jmv 4/10/2019 09:41:20'!
contours: aCollection
	contours := aCollection asArray.! !

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

!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 1/18/2020 15:54:31'!
replaceIndexesByComponentGlyphsIn: dummy
	"NOP"! !

!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 5/16/2019 21:21:51'!
updateRightSideBearing
	"Update the right side bearing value"
	"@@: Is the following really correct?!!?!!"
	rightSideBearing := advanceWidth - leftSideBearing - boundsRight + boundsLeft! !

!TTGlyph methodsFor: 'drawing' stamp: 'jmv 3/2/2020 12:52:41'!
drawGlyphOn: aVectorCanvas
	self drawGlyphOn: aVectorCanvas geometryTransformation: nil! !

!TTGlyph methodsFor: 'drawing' stamp: 'jmv 3/2/2020 12:52:01'!
drawGlyphOn: aVectorCanvas geometryTransformation: txOrNil
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			aVectorCanvas moveToX: 0 y: 0.
			(contour initialMoveToCommand: txOrNil) runOn: aVectorCanvas .
			(contour pathCommand: txOrNil) runOn: aVectorCanvas ]].! !

!TTGlyph methodsFor: 'primitive format' stamp: 'jmv 3/14/2020 23:03:58'!
addContourDataTo: stream
	stream nextPut: self advanceWidth.
	stream nextPut: self boundsLeft.
	stream nextPut: self boundsRight.
	stream nextPut: self boundsTop.
	stream nextPut: self boundsBottom.
	stream nextPut: self contourCount.
	self addContourDataTo: stream tx: nil! !

!TTGlyph methodsFor: 'primitive format' stamp: 'jmv 3/3/2020 09:46:30'!
addContourDataTo: stream tx: aGeometryTransformationOrNil

	contours do: [ :c | c addContourDataTo: stream tx: aGeometryTransformationOrNil ]! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'ar 11/2/1998 01:21'!
addGlyph: aGlyph transformation: aMatrix
	glyphs := glyphs copyWith: (aMatrix -> aGlyph)! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'jmv 3/3/2020 09:43:58'!
contourCount
	| glyph count |
	count _ 0.
	glyphs do: [ :txAndGlyph |
		glyph _ txAndGlyph value.
		count _ count + glyph contourCount.
		].
	^count! !

!TTCompositeGlyph methodsFor: 'accessing' stamp: 'ar 11/2/1998 01:20'!
glyphs

	^glyphs collect:[:assoc| assoc value].! !

!TTCompositeGlyph methodsFor: 'initialize' stamp: 'ar 11/2/1998 01:20'!
initialize
	glyphs := #().! !

!TTCompositeGlyph methodsFor: 'testing'!
isComposite
	^true! !

!TTCompositeGlyph methodsFor: 'drawing' stamp: 'jmv 3/2/2020 12:52:20'!
drawGlyphOn: aVectorCanvas
	glyphs do: [ :each |
		each value drawGlyphOn: aVectorCanvas geometryTransformation: each key ]! !

!TTCompositeGlyph methodsFor: 'drawing' stamp: 'jmv 1/18/2020 16:29:44'!
txAndContoursDo: aBlock
	glyphs do: [ :assoc |
		aBlock value: assoc key value: assoc value contours ]! !

!TTCompositeGlyph methodsFor: 'private-initialization' stamp: 'jmv 1/18/2020 15:54:09'!
replaceIndexesByComponentGlyphsIn: anArray
	glyphs _ glyphs collect: [ :assoc | assoc key -> (anArray at: assoc value) ]! !

!TTCompositeGlyph methodsFor: 'primitive format' stamp: 'jmv 3/4/2020 11:29:26'!
addContourDataTo: stream tx: aGeometryTransformationOrNil
	| glyph tx |
	glyphs do: [ :txAndGlyph |
		tx _ txAndGlyph key.
		aGeometryTransformationOrNil notNil ifTrue: [
			tx _ aGeometryTransformationOrNil composedWith: tx ].
		glyph _ txAndGlyph value.
		glyph addContourDataTo: stream tx: tx ]! !

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

!Color methodsFor: '*VectorGraphics' stamp: 'jmv 1/21/2015 12:48'!
externalizedWith: aMorphicLocation

	^self! !

!Character methodsFor: '*vectorGraphics' stamp: 'jmv 4/1/2020 11:45:59'!
widthInTrueTypeFontDescription: aTTFontDescription useST80Glyphs: aBoolean
	"Double dispatch"

	^aTTFontDescription widthOf: self useST80Glyphs: aBoolean! !

!String methodsFor: '*vectorGraphics' stamp: 'jmv 4/1/2020 12:26:06'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p trueTypeFont: aTrueTypeFont color: color
	"Double dispatch"

	^ engine
		displayString: self
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color! !

!Text methodsFor: '*vectorGraphics' stamp: 'jmv 4/21/2020 13:13:17'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p trueTypeFont: aTrueTypeFont color: color
	"Double dispatch"

	^ engine
		displayString: self
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/22/2020 23:24:00'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer last affected pixel position"
	| fdestX char glyph maxHeight |

	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	startIndex to: stopIndex do: [ :charIndex |
		char _ aString at: charIndex.
		glyph _ aTrueTypeFont glyphAt: char.
		destX _ (fdestX + glyph leftOffset) rounded.
		destY _ aPoint y + aTrueTypeFont ascent + aTrueTypeFont lineGap-1 - glyph topOffset. 
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (aTrueTypeFont ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (aTrueTypeFont ascent*0.3)]].
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height.
	].
	^ destX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 4/1/2020 12:30:47'!
basicDisplayUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer last affected pixel position"
	| fdestX glyph maxHeight |
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	startIndex to: stopIndex do: [ :i | | codePoint |
		codePoint _ anIntegerArray at: i.
		Character
			evaluateOnce: [ :byte1 :byte2 :byte3 :byte4 |
				glyph _ aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
		destX _ (fdestX + glyph leftOffset) rounded.
		destY _ aPoint y + aTrueTypeFont ascent + aTrueTypeFont lineGap-1 - glyph topOffset.
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (aTrueTypeFont ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (aTrueTypeFont ascent*0.3)]].
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height ].
	^ destX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/22/2020 23:23:08'!
basicDisplayUtf8: aByteArray at: aPoint trueTypeFont: aTrueTypeFont
	"Answer last affected pixel position"
	| fdestX glyph maxHeight utf8Stream |
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	utf8Stream _ aByteArray readStream.
	[ utf8Stream atEnd ] whileFalse: [
		glyph _ Character
			nextCodePointBytesFromUtf8: utf8Stream
			into: [ :byte1 :byte2 :byte3 :byte4 |
				aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ].
		destX _ (fdestX + glyph leftOffset) rounded.
		destY _ aPoint y + aTrueTypeFont ascent + aTrueTypeFont lineGap-1 - glyph topOffset.
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (aTrueTypeFont ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (aTrueTypeFont ascent*0.3)]].
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height ].
	^ destX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 5/21/2019 10:56:45'!
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

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 4/1/2020 12:31:45'!
displayUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
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
		^self basicDisplayUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/21/2020 19:15:23'!
displayUtf8: aByteArray at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
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
		^self basicDisplayUtf8: aByteArray at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf8: aByteArray at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf8: aByteArray at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!Morph methodsFor: '*vectorGraphics' stamp: 'hlsf 1/18/2021 17:31:38'!
addLabel: string font: font centeredAt: position rotationDegrees: angle
	"Add a label top-centered at position and rotated of angle degrees"
	| p label |
	p _ position - (position normal * (font widthOfString: string asString) / 2).
	label _ LabelMorph
		contents: string
		font: font.
	label rotation: (90 - angle) degreesToRadians.
	self addMorph: label.
	label morphPosition: p.
	^ label.! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 2/10/2021 17:26:14'!
imageForm: depth
	| auxCanvas p r  answer |

	self requiresVectorCanvas ifFalse: [
		auxCanvas _ MorphicCanvas depth: depth over: (self morphPosition extent: self morphExtent).
		auxCanvas fullDraw: self.
		^ auxCanvas form divideByAlpha ].

	p _ self morphPosition.
	auxCanvas _ VectorCanvas onForm: Display.
	auxCanvas updatingMorphBoundsDo: [
		auxCanvas into: self.
		auxCanvas updateCurrentDisplayBounds.
		auxCanvas outOfMorph ].
	r _ self displayFullBounds.

	self morphPosition: self morphPosition - r origin.
	answer _ Form extent: r extent depth: 32.
	(VectorCanvas onForm: answer) fullDraw: self.

	self morphPosition: p.
	owner notNil ifTrue: [
		self redrawNeeded ].
	^answer! !

!MorphicCanvas methodsFor: '*vectorGraphics-drawing-text' stamp: 'jmv 3/22/2020 20:03:06'!
drawUtf8: aByteArray at: aPoint font: aTrueTypeFont color: aColor
	self subclassResponsibility! !

!BitBltCanvas methodsFor: '*vectorGraphics-drawing-text' stamp: 'jmv 12/2/2020 16:18:44'!
drawUtf32: anIntegerArray from: startIndex to: stopIndex at: aPoint font: aTrueTypeFont color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	"
| c f utf32 |
Preferences loadOnlyLatinGlyphData ifTrue: [
	Preferences disable: #loadOnlyLatinGlyphData. TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf' ].
c _ BitBltCanvas onForm: Display.
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
utf32 _ #(923 959 961 949 956 32 953 960 963 952 956 32 948 959 955 959 961 32 963 953 964 32 945 956 949 964 44 32 956 959 948 959 32 118 949 957 953 945 956 32 118 953 948 949 961 949 961 32 953 957 32 99 952 956 44 32 949 958 32 945 952 964 949 956 32 956 945 955 953 963 32 959 966 966 949 957 948 953 964 32 949 963 964 46 32 921 948 32 964 959 955 955 953 964 32 963 952 963 99 953 960 953 964 32 963 945 955 952 964 945 964 952 963 32 113 952 953 44 32 99 952 32 113 952 945 957 948 959 32 960 961 959 960 961 953 945 949 32 953 956 960 949 961 948 953 949 964 32 118 949 955 44 32 956 949 945 32 949 964 32 949 961 961 949 956 32 947 955 959 961 953 945 964 952 961 46 32 917 958 949 961 99 953 32 948 949 966 953 957 953 964 953 959 957 949 956 32 956 949 955 32 949 945 46 32 917 958 32 963 949 945 32 949 955 953 964 32 948 953 963 963 949 957 964 953 949 964 44 32 956 945 955 953 963 32 964 961 953 964 945 957 953 32 118 953 963 32 953 948 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@200 font: f color: Color random.
c drawUtf8:  #[97 195 177 111 ]  at: 10@400 font: f color: Color random.
Display forceToScreen
	"

	| p font |


	engine ifNil: [ ^nil ].

	p _ (currentTransformation transform: aPoint) rounded.
	engine colorMap: nil.
	font _ aTrueTypeFont ifNil: [ FontFamily defaultFamilyAndPointSize ].
	^ engine
		displayUtf32: anIntegerArray
		from: startIndex
		to: stopIndex 
		at: p
		trueTypeFont: font
		color: aColor! !

!BitBltCanvas methodsFor: '*vectorGraphics-drawing-text' stamp: 'jmv 12/2/2020 16:18:53'!
drawUtf8: aByteArray at: aPoint font: aTrueTypeFont color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	"
| c f |
Preferences loadOnlyLatinGlyphData ifTrue: [
	Preferences disable: #loadOnlyLatinGlyphData. TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf' ].
c _ BitBltCanvas onForm: Display.
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
c drawString: 'Hola' at: 10@10 font: f color: Color random.
c drawUtf8: #[208 176 208 177 208 178 208 179 208 180 208 181 209 145 208 182 208 183 208 184 208 185 208 186 208 187 208 188 208 189 208 190 209 143 209 142 209 141 209 140 209 139 209 138 209 137 209 136 209 135 209 134 209 133 209 132 209 131 209 130 209 129 209 128 208 191] at: 10@100 font: f color: Color random.
c drawUtf8:  #[208 146 208 190 209 130 32 208 184 208 183 32 208 181 208 189 32 208 184 208 179 208 183 208 176 208 188 208 191 208 187 32 208 190 209 132 32 208 181 208 189 32 208 152 208 189 208 179 208 187 208 184 209 136 32 209 130 208 181 208 186 209 129 209 130 32 209 128 208 184 209 130 208 189 32 208 184 208 189 32 208 180 32 208 161 208 184 209 128 208 184 208 187 208 184 208 186 32 208 176 208 187 209 132 208 176 208 177 208 181 209 130 63]  at: 10@200 font: f color: Color random.
c drawString: 'año' at: 10@300 font: f color: Color random.
c drawUtf8:  #[97 195 177 111 ]  at: 10@400 font: f color: Color random.
Display forceToScreen
	"

	| p font |

	engine ifNil: [ ^nil ].

	p _ (currentTransformation transform: aPoint) rounded.
	engine colorMap: nil.
	font _ aTrueTypeFont ifNil: [ FontFamily defaultFamilyAndPointSize ].
	^ engine
		displayUtf8: aByteArray
		at: p
		trueTypeFont: font
		color: aColor! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:49:45'!
exampleArabic: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _  #[217 131 217 132 32 216 163 217 136 216 179 216 185 32 216 167 217 132 216 170 217 134 216 167 216 178 217 132 217 138 32 217 129 216 181 217 132 44 32 217 133 216 185 32 216 185 217 133 217 132 217 138 216 169 32 216 167 217 132 216 180 216 177 217 130 217 138 32 216 167 217 134 216 170 216 168 216 167 217 135 32 217 138 217 131 217 134 44 32 216 185 217 132 32 216 168 216 173 217 130 32 216 171 217 133 217 145 216 169 32 217 136 216 173 217 132 217 129 216 167 216 164 217 135 216 167 46 32 217 133 216 185 32 216 182 216 177 216 168 32 216 175 217 129 217 145 216 169 32 216 180 216 177 216 179 216 169 32 216 167 217 132 216 181 217 129 216 173 216 167 216 170 44 32 217 133 216 179 216 164 217 136 217 132 217 138 216 169 32 216 167 217 132 216 177 216 168 217 138 216 185 216 140 32 217 132 216 168 217 136 217 132 217 134 216 175 216 167 216 140 32 217 136 216 170 217 133 32 217 130 216 175 46 32 216 174 216 183 217 145 216 169 32 217 132 217 133 216 173 216 167 217 131 217 133 32 217 136 216 181 217 132 32 217 129 217 138 44 32 217 136 217 129 217 134 217 132 217 134 216 175 216 167 32 216 167 217 132 216 165 217 134 216 178 216 167 217 132 32 216 167 217 132 217 133 216 170 216 167 216 174 217 133 216 169 32 216 185 217 134 32 216 173 217 138 216 171 46 32 216 167 217 132 217 137 32 217 135 216 167 217 133 216 180 32 216 167 217 132 217 138 216 167 216 168 216 167 217 134 32 217 132 217 133 46 32 216 170 216 183 217 136 217 138 216 177 32 217 129 217 135 216 177 216 179 216 170 32 217 132 217 133 32 217 129 216 185 217 132 46 32 216 175 217 134 217 136 32 216 167 217 132 216 185 216 179 217 131 216 177 217 138 32 217 136 217 133 216 183 216 167 217 132 216 168 216 169 32 216 168 217 132 44 32 217 133 217 132 217 138 217 136 217 134 32 217 136 216 168 216 167 216 179 216 170 216 171 217 134 216 167 216 161 32 217 133 216 185 32 216 175 217 136 217 134 44 32 217 133 217 134 32 217 133 217 131 217 134 32 216 165 216 168 217 145 216 167 217 134 32 216 167 217 132 216 172 217 134 217 136 216 168 217 138 46 32 216 168 217 128 32 217 133 216 167 216 176 216 167 32 216 163 217 136 216 177 216 167 217 130 217 135 217 133 32 216 173 217 138 216 171 44 32 217 131 216 167 217 134 32 216 170 217 133 32 216 179 216 167 216 168 217 130 32 217 133 217 131 217 145 217 134 32 216 167 217 132 216 179 217 138 216 161 44 32 216 163 216 179 216 177 32 217 129 217 135 216 177 216 179 216 170 32 216 167 217 132 216 179 216 167 216 175 216 179 32 216 167 217 132 216 165 216 173 216 170 217 129 216 167 216 184 32 217 136 46].
c drawUtf8: utf8 at: 10@210 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:50:06'!
exampleArmenian: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _  #[213 172 213 184 213 188 213 165 213 180 32 213 171 213 186 213 189 213 184 214 130 213 180 32 213 164 213 184 213 172 213 184 213 188 32 213 189 213 171 213 169 32 213 161 213 180 213 165 213 169 44 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 165 213 184 214 130 213 171 213 189 213 180 213 184 213 164 32 213 188 213 165 213 186 213 184 214 130 213 164 213 171 213 161 213 188 213 165 32 213 184 214 130 213 169 32 213 189 213 171 213 169 46 32 213 190 213 165 213 188 213 184 32 213 188 213 165 214 134 213 184 213 188 213 180 213 171 213 164 213 161 213 182 213 189 32 213 174 213 184 214 130 32 213 189 213 171 213 169 44 32 213 164 213 165 213 162 213 165 213 169 32 213 186 213 165 213 188 214 134 213 165 213 174 213 169 213 184 32 213 189 213 161 213 172 213 184 214 130 213 169 213 161 213 169 213 184 214 130 213 189 32 213 165 213 169 32 213 174 213 184 214 130 213 180 44 32 213 176 213 161 213 189 32 213 169 213 188 213 171 213 169 213 161 213 182 213 171 32 213 182 213 184 213 182 213 184 214 130 213 180 213 165 213 189 32 213 182 213 165 46 32 213 180 213 165 213 171 32 213 182 213 184 32 213 189 213 184 213 172 213 184 214 130 213 169 213 161 32 213 172 213 161 213 162 213 184 213 188 213 165 213 189 32 213 184 213 180 213 171 213 169 213 169 213 161 213 180 44 32 213 165 213 173 32 213 163 213 188 213 161 213 165 213 174 213 184 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 189 32 213 176 213 161 213 189 44 32 213 165 213 161 32 213 180 213 165 213 172 32 214 132 213 184 214 130 213 171 213 189 32 213 182 213 171 213 189 213 172 32 213 161 213 189 213 189 213 184 214 130 213 180 46 32 213 171 213 182 32 213 161 213 180 213 165 213 169 32 213 182 213 165 213 180 213 184 213 188 213 165 32 213 184 214 130 213 189 213 184 214 130 46 32 213 165 213 188 213 161 213 169 32 213 171 213 184 214 130 213 190 213 161 213 188 213 165 213 169 32 213 165 213 161 32 213 189 213 165 213 164 44 32 213 176 213 161 213 189 32 213 161 213 172 213 171 213 161 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 174 213 184 213 188 213 186 213 184 213 188 213 161 32 213 161 213 164 46 32 213 165 213 184 213 189 32 214 134 213 161 213 189 213 169 213 171 213 164 213 171 213 171 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 180 32 213 182 213 165 46].
c drawUtf8: utf8  at: 10@130 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:50:23'!
exampleChinese: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[233 159 179 229 138 155 233 164 168 229 136 134 233 167 191 229 155 163 230 152 147 229 142 154 229 165 189 230 149 151 231 132 188 233 150 139 229 186 183 229 134 134 227 128 130 230 149 133 229 176 143 229 162 151 233 157 162 230 150 135 230 169 139 232 187 162 229 149 143 233 150 128 228 188 154 231 142 132 233 150 139 229 142 187 230 137 128 230 173 163 230 131 179 232 169 177 230 157 165 227 128 130 229 185 180 228 186 161 229 166 187 229 173 152 232 186 171 230 143 144 233 128 159 228 186 172 229 186 171 230 153 130 228 184 188 230 177 186 232 131 189 227 128 130 229 143 184 229 174 159 229 134 133 229 177 164 232 179 170 229 143 163 232 178 162 230 177 130 229 133 172 233 157 153 230 179 129 229 136 182 229 188 149 230 153 130 229 157 135 233 171 152 230 150 176 227 128 130 231 162 186 230 156 159 230 149 176 233 155 134 230 156 168 232 172 155 230 173 162 230 178 191 229 130 153 233 129 142 229 155 189 232 179 188 230 168 170 229 150 156 229 136 183 229 186 183 230 136 144 229 175 186 227 128 130 232 168 152 229 136 134 229 143 130 229 145 188 229 179 182 229 155 189 232 170 160 230 168 169 229 155 158 230 173 163 229 185 180 232 187 141 230 167 152 228 187 138 229 132 170 230 150 135 231 166 129 229 144 166 227 128 130 230 149 180 230 130 163 232 182 163 231 164 190 230 152 142 230 164 156 231 149 170 230 158 156 230 142 168 231 178 149 228 184 150 230 156 172 229 176 143 231 132 161 231 167 176 231 148 186 227 128 130 230 138 149 230 160 188 229 140 187 229 190 140 228 187 187 231 179 187 232 187 162 230 148 191 230 157 161 232 169 177 230 142 178 230 156 136 231 144 131 227 128 130 230 130 148 233 133 184 232 166 139 229 164 169 229 134 153 229 164 137 229 143 130 233 153 184 229 155 155 229 144 141 229 140 150 232 139 151 229 141 136 230 178 187 233 154 155 228 189 143 232 166 135 228 184 184 230 149 153 231 149 140].
c drawUtf8: utf8 at: 10@290 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:50:43'!
exampleCyrillic: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[208 155 208 190 209 128 208 181 208 188 32 208 184 208 191 209 129 209 131 208 188 32 208 180 208 190 208 187 208 190 209 128 32 209 129 208 184 209 130 32 208 176 208 188 208 181 209 130 44 32 208 180 208 181 208 177 208 181 209 130 32 209 129 208 176 208 187 209 131 209 130 208 176 208 189 208 180 208 184 32 208 188 208 181 208 184 32 208 181 208 184 46 32 208 144 208 189 32 209 133 208 184 209 129 32 208 179 209 128 208 176 208 181 209 134 208 181 32 209 131 209 130 208 176 208 188 209 131 209 128 32 208 180 208 190 208 187 208 190 209 128 209 131 208 188 44 32 208 187 208 190 209 128 208 181 208 188 32 208 191 209 128 208 184 208 189 209 134 208 184 208 191 208 181 209 129 32 208 188 208 190 208 187 208 181 209 129 209 130 208 184 208 176 208 181 32 208 180 209 131 208 190 32 208 184 208 189 46 32 208 149 208 184 32 208 188 208 190 208 178 208 181 209 130 32 208 189 208 190 209 129 209 130 209 128 208 190 32 208 190 208 191 209 130 208 184 208 190 208 189 32 208 189 208 176 208 188 44 32 208 181 208 190 209 129 32 208 181 208 191 208 184 209 134 209 131 209 128 208 184 32 208 190 208 188 208 184 209 130 209 130 208 176 208 189 209 130 209 131 209 128 32 209 131 209 130 46 32 208 163 209 130 32 208 188 209 131 208 189 208 181 209 128 208 181 32 208 187 208 176 208 177 208 190 209 128 208 181 32 208 184 208 189 209 130 208 181 208 187 208 187 208 181 208 179 208 181 208 177 208 176 209 130 32 209 131 209 129 209 131 44 32 208 181 208 176 32 209 129 208 181 208 180 32 208 180 208 181 208 177 208 181 209 130 32 208 187 209 131 208 191 209 130 208 176 209 130 209 131 208 188 44 32 208 178 208 184 208 180 208 184 209 130 32 208 184 208 187 208 187 209 131 208 180 32 208 184 209 131 208 180 208 184 209 134 208 190 32 209 143 209 131 208 190 32 209 131 209 130 46].
c drawUtf8: utf8 at: 10@50 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:50:59'!
exampleGeorgian: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[225 131 154 225 131 157 225 131 160 225 131 148 225 131 155 32 225 131 152 225 131 164 225 131 161 225 131 163 225 131 155 32 225 131 147 225 131 157 225 131 154 225 131 157 225 131 160 32 225 131 161 225 131 152 225 131 151 32 225 131 144 225 131 155 225 131 148 225 131 151 44 32 225 131 167 225 131 163 225 131 157 32 225 131 163 225 131 151 32 225 131 147 225 131 152 225 131 170 225 131 144 225 131 151 32 225 131 144 225 131 154 225 131 152 225 131 167 225 131 163 225 131 152 225 131 147 44 32 225 131 144 225 131 147 32 225 131 154 225 131 152 225 131 145 225 131 160 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 160 225 131 148 225 131 164 225 131 160 225 131 148 225 131 176 225 131 148 225 131 156 225 131 147 225 131 163 225 131 156 225 131 151 32 225 131 148 225 131 161 225 131 151 46 32 225 131 154 225 131 148 225 131 146 225 131 148 225 131 160 225 131 148 32 225 131 170 225 131 157 225 131 160 225 131 164 225 131 157 225 131 160 225 131 144 32 225 131 148 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 144 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 144 225 131 151 32 225 131 148 225 131 144 225 131 155 44 32 225 131 156 225 131 157 32 225 131 167 225 131 163 225 131 144 225 131 161 32 225 131 155 225 131 148 225 131 156 225 131 144 225 131 156 225 131 147 225 131 160 225 131 152 32 225 131 160 225 131 148 225 131 164 225 131 148 225 131 160 225 131 160 225 131 148 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 167 225 131 163 225 131 152 46 32 225 131 151 225 131 148 32 225 131 164 225 131 163 225 131 151 225 131 148 225 131 156 225 131 151 32 225 131 155 225 131 157 225 131 154 225 131 154 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 149 225 131 152 225 131 161 46 32 225 131 149 225 131 152 225 131 155 32 225 131 147 225 131 148 225 131 156 225 131 152 225 131 167 225 131 163 225 131 148 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 151 225 131 176 225 131 148 225 131 157 225 131 164 225 131 176 225 131 160 225 131 144 225 131 161 225 131 151 225 131 163 225 131 161 32 225 131 148 225 131 174 44 32 225 131 148 225 131 144 32 225 131 176 225 131 144 225 131 161 32 225 131 149 225 131 157 225 131 154 225 131 163 225 131 155 225 131 163 225 131 161 32 225 131 164 225 131 144 225 131 161 225 131 151 225 131 152 225 131 147 225 131 152 225 131 152 32 225 131 147 225 131 148 225 131 161 225 131 148 225 131 160 225 131 163 225 131 156 225 131 151 46 32 225 131 148 225 131 174 32 225 131 157 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 152 225 131 161 32 225 131 151 225 131 157 225 131 160 225 131 167 225 131 163 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 155 225 131 148 225 131 152 44 32 225 131 147 225 131 157 225 131 170 225 131 151 225 131 163 225 131 161 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 152 225 131 156 32 225 131 167 225 131 163 225 131 157 46 32 225 131 161 225 131 163 225 131 155 225 131 155 225 131 157 32 225 131 151 225 131 160 225 131 144 225 131 170 225 131 151 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 148 225 131 152 32 225 131 152 225 131 163 225 131 161 44 32 225 131 163 225 131 151 32 225 131 157 225 131 155 225 131 156 225 131 148 225 131 161 32 225 131 149 225 131 152 225 131 160 225 131 151 225 131 163 225 131 151 225 131 148 32 225 131 147 225 131 148 225 131 151 225 131 160 225 131 144 225 131 174 225 131 152 225 131 151 46].
c drawUtf8: utf8 at: 10@170 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:51:24'!
exampleGreek: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[206 155 206 191 207 129 206 181 206 188 32 206 185 207 128 207 131 206 184 206 188 32 206 180 206 191 206 187 206 191 207 129 32 207 131 206 185 207 132 32 206 177 206 188 206 181 207 132 44 32 206 188 206 191 206 180 206 191 32 118 206 181 206 189 206 185 206 177 206 188 32 118 206 185 206 180 206 181 207 129 206 181 207 129 32 206 185 206 189 32 99 206 184 206 188 44 32 206 181 206 190 32 206 177 206 184 207 132 206 181 206 188 32 206 188 206 177 206 187 206 185 207 131 32 206 191 207 134 207 134 206 181 206 189 206 180 206 185 207 132 32 206 181 207 131 207 132 46 32 206 153 206 180 32 207 132 206 191 206 187 206 187 206 185 207 132 32 207 131 206 184 207 131 99 206 185 207 128 206 185 207 132 32 207 131 206 177 206 187 206 184 207 132 206 177 207 132 206 184 207 131 32 113 206 184 206 185 44 32 99 206 184 32 113 206 184 206 177 206 189 206 180 206 191 32 207 128 207 129 206 191 207 128 207 129 206 185 206 177 206 181 32 206 185 206 188 207 128 206 181 207 129 206 180 206 185 206 181 207 132 32 118 206 181 206 187 44 32 206 188 206 181 206 177 32 206 181 207 132 32 206 181 207 129 207 129 206 181 206 188 32 206 179 206 187 206 191 207 129 206 185 206 177 207 132 206 184 207 129 46 32 206 149 206 190 206 181 207 129 99 206 185 32 206 180 206 181 207 134 206 185 206 189 206 185 207 132 206 185 206 191 206 189 206 181 206 188 32 206 188 206 181 206 187 32 206 181 206 177 46 32 206 149 206 190 32 207 131 206 181 206 177 32 206 181 206 187 206 185 207 132 32 206 180 206 185 207 131 207 131 206 181 206 189 207 132 206 185 206 181 207 132 44 32 206 188 206 177 206 187 206 185 207 131 32 207 132 207 129 206 185 207 132 206 177 206 189 206 185 32 118 206 185 207 131 32 206 185 206 180 46].
c drawUtf8: utf8 at: 10@90 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:51:40'!
exampleHebrew: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[215 162 215 156 32 215 158 215 144 215 158 215 168 32 215 150 215 167 215 149 215 167 32 215 169 215 147 215 168 215 149 215 170 32 215 144 215 168 215 165 44 32 215 148 215 158 215 156 215 166 215 170 32 215 158 215 147 215 149 215 153 215 167 215 153 215 157 32 215 169 215 155 215 156 32 215 169 215 156 44 32 215 162 215 153 215 166 215 149 215 145 32 215 160 215 153 215 149 215 149 215 152 32 215 169 215 153 215 170 215 149 215 164 215 153 215 170 32 215 162 215 149 215 147 32 215 145 46 32 215 161 215 147 215 168 32 215 144 215 149 32 215 168 215 164 215 149 215 144 215 148 32 215 169 215 153 215 158 215 149 215 169 215 153 32 215 158 215 149 215 162 215 158 215 147 215 153 215 157 46 32 215 148 215 153 215 144 32 215 164 215 153 215 161 215 153 215 167 215 148 32 215 148 215 146 215 168 215 164 215 153 215 157 32 215 169 215 153 215 158 215 149 215 169 215 153 215 153 215 157 32 215 169 215 156 46 32 215 169 215 155 215 156 32 215 162 215 156 32 215 162 215 150 215 168 215 148 32 215 156 215 148 215 164 215 149 215 154 32 215 144 215 149 215 149 215 153 215 168 215 149 215 160 215 144 215 149 215 152 215 153 215 167 215 148 44 32 215 144 215 160 215 144 32 215 144 215 170 32 215 162 215 150 215 168 215 148 32 215 156 215 158 215 160 215 149 215 162 32 215 156 215 158 215 151 215 153 215 167 215 148 46].
c drawUtf8: utf8 at: 10@250 font: aTrueTypeFont color: Color random. Display forceToScreen.
Display forceToScreen! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:51:58'!
exampleLatin: aTrueTypeFont

| c utf8 |
c _ BitBltCanvas onForm: Display.
utf8 _ #[76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46].
c drawUtf8: utf8 at: 10@10 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:57:30'!
exampleUtf32Arabic: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(1603 1604 32 1571 1608 1587 1593 32 1575 1604 1578 1606 1575 1586 1604 1610 32 1601 1589 1604 44 32 1605 1593 32 1593 1605 1604 1610 1577 32 1575 1604 1588 1585 1602 1610 32 1575 1606 1578 1576 1575 1607 32 1610 1603 1606 44 32 1593 1604 32 1576 1581 1602 32 1579 1605 1617 1577 32 1608 1581 1604 1601 1575 1572 1607 1575 46 32 1605 1593 32 1590 1585 1576 32 1583 1601 1617 1577 32 1588 1585 1587 1577 32 1575 1604 1589 1601 1581 1575 1578 44 32 1605 1587 1572 1608 1604 1610 1577 32 1575 1604 1585 1576 1610 1593 1548 32 1604 1576 1608 1604 1606 1583 1575 1548 32 1608 1578 1605 32 1602 1583 46 32 1582 1591 1617 1577 32 1604 1605 1581 1575 1603 1605 32 1608 1589 1604 32 1601 1610 44 32 1608 1601 1606 1604 1606 1583 1575 32 1575 1604 1573 1606 1586 1575 1604 32 1575 1604 1605 1578 1575 1582 1605 1577 32 1593 1606 32 1581 1610 1579 46 32 1575 1604 1609 32 1607 1575 1605 1588 32 1575 1604 1610 1575 1576 1575 1606 32 1604 1605 46 32 1578 1591 1608 1610 1585 32 1601 1607 1585 1587 1578 32 1604 1605 32 1601 1593 1604 46 32 1583 1606 1608 32 1575 1604 1593 1587 1603 1585 1610 32 1608 1605 1591 1575 1604 1576 1577 32 1576 1604 44 32 1605 1604 1610 1608 1606 32 1608 1576 1575 1587 1578 1579 1606 1575 1569 32 1605 1593 32 1583 1608 1606 44 32 1605 1606 32 1605 1603 1606 32 1573 1576 1617 1575 1606 32 1575 1604 1580 1606 1608 1576 1610 46 32 1576 1600 32 1605 1575 1584 1575 32 1571 1608 1585 1575 1602 1607 1605 32 1581 1610 1579 44 32 1603 1575 1606 32 1578 1605 32 1587 1575 1576 1602 32 1605 1603 1617 1606 32 1575 1604 1587 1610 1569 44 32 1571 1587 1585 32 1601 1607 1585 1587 1578 32 1575 1604 1587 1575 1583 1587 32 1575 1604 1573 1581 1578 1601 1575 1592 32 1608 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@210 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:58:41'!
exampleUtf32Armenian: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(1388 1400 1404 1381 1396 32 1387 1402 1405 1400 1410 1396 32 1380 1400 1388 1400 1404 32 1405 1387 1385 32 1377 1396 1381 1385 44 32 1412 1400 1410 1377 1381 1412 1400 1410 1381 32 1381 1400 1410 1387 1405 1396 1400 1380 32 1404 1381 1402 1400 1410 1380 1387 1377 1404 1381 32 1400 1410 1385 32 1405 1387 1385 46 32 1406 1381 1404 1400 32 1404 1381 1414 1400 1404 1396 1387 1380 1377 1398 1405 32 1390 1400 1410 32 1405 1387 1385 44 32 1380 1381 1378 1381 1385 32 1402 1381 1404 1414 1381 1390 1385 1400 32 1405 1377 1388 1400 1410 1385 1377 1385 1400 1410 1405 32 1381 1385 32 1390 1400 1410 1396 44 32 1392 1377 1405 32 1385 1404 1387 1385 1377 1398 1387 32 1398 1400 1398 1400 1410 1396 1381 1405 32 1398 1381 46 32 1396 1381 1387 32 1398 1400 32 1405 1400 1388 1400 1410 1385 1377 32 1388 1377 1378 1400 1404 1381 1405 32 1400 1396 1387 1385 1385 1377 1396 44 32 1381 1389 32 1379 1404 1377 1381 1390 1400 32 1380 1381 1414 1387 1398 1387 1385 1387 1400 1398 1381 1405 32 1392 1377 1405 44 32 1381 1377 32 1396 1381 1388 32 1412 1400 1410 1387 1405 32 1398 1387 1405 1388 32 1377 1405 1405 1400 1410 1396 46 32 1387 1398 32 1377 1396 1381 1385 32 1398 1381 1396 1400 1404 1381 32 1400 1410 1405 1400 1410 46 32 1381 1404 1377 1385 32 1387 1400 1410 1406 1377 1404 1381 1385 32 1381 1377 32 1405 1381 1380 44 32 1392 1377 1405 32 1377 1388 1387 1377 32 1412 1400 1410 1377 1381 1412 1400 1410 1381 32 1390 1400 1404 1402 1400 1404 1377 32 1377 1380 46 32 1381 1400 1405 32 1414 1377 1405 1385 1387 1380 1387 1387 32 1380 1381 1414 1387 1398 1387 1385 1387 1400 1398 1381 1396 32 1398 1381 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@130 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:58:46'!
exampleUtf32Chinese: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(38899 21147 39208 20998 39423 22243 26131 21402 22909 25943 28988 38283 24247 20870 12290 25925 23567 22679 38754 25991 27211 36578 21839 38272 20250 29572 38283 21435 25152 27491 24819 35441 26469 12290 24180 20129 22971 23384 36523 25552 36895 20140 24235 26178 20028 27770 33021 12290 21496 23455 20869 23652 36074 21475 36002 27714 20844 38745 27841 21046 24341 26178 22343 39640 26032 12290 30906 26399 25968 38598 26408 35611 27490 27839 20633 36942 22269 36092 27178 21916 21047 24247 25104 23546 12290 35352 20998 21442 21628 23798 22269 35488 27177 22238 27491 24180 36557 27096 20170 20778 25991 31105 21542 12290 25972 24739 36259 31038 26126 26908 30058 26524 25512 31893 19990 26412 23567 28961 31216 30010 12290 25237 26684 21307 24460 20219 31995 36578 25919 26465 35441 25522 26376 29699 12290 24724 37240 35211 22825 20889 22793 21442 38520 22235 21517 21270 33495 21320 27835 38555 20303 35207 20024 25945 30028) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@290 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:58:49'!
exampleUtf32Cyrillic: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(1051 1086 1088 1077 1084 32 1080 1087 1089 1091 1084 32 1076 1086 1083 1086 1088 32 1089 1080 1090 32 1072 1084 1077 1090 44 32 1076 1077 1073 1077 1090 32 1089 1072 1083 1091 1090 1072 1085 1076 1080 32 1084 1077 1080 32 1077 1080 46 32 1040 1085 32 1093 1080 1089 32 1075 1088 1072 1077 1094 1077 32 1091 1090 1072 1084 1091 1088 32 1076 1086 1083 1086 1088 1091 1084 44 32 1083 1086 1088 1077 1084 32 1087 1088 1080 1085 1094 1080 1087 1077 1089 32 1084 1086 1083 1077 1089 1090 1080 1072 1077 32 1076 1091 1086 32 1080 1085 46 32 1045 1080 32 1084 1086 1074 1077 1090 32 1085 1086 1089 1090 1088 1086 32 1086 1087 1090 1080 1086 1085 32 1085 1072 1084 44 32 1077 1086 1089 32 1077 1087 1080 1094 1091 1088 1080 32 1086 1084 1080 1090 1090 1072 1085 1090 1091 1088 32 1091 1090 46 32 1059 1090 32 1084 1091 1085 1077 1088 1077 32 1083 1072 1073 1086 1088 1077 32 1080 1085 1090 1077 1083 1083 1077 1075 1077 1073 1072 1090 32 1091 1089 1091 44 32 1077 1072 32 1089 1077 1076 32 1076 1077 1073 1077 1090 32 1083 1091 1087 1090 1072 1090 1091 1084 44 32 1074 1080 1076 1080 1090 32 1080 1083 1083 1091 1076 32 1080 1091 1076 1080 1094 1086 32 1103 1091 1086 32 1091 1090 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@50 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:58:53'!
exampleUtf32Georgian: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@170 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:58:57'!
exampleUtf32Greek: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(923 959 961 949 956 32 953 960 963 952 956 32 948 959 955 959 961 32 963 953 964 32 945 956 949 964 44 32 956 959 948 959 32 118 949 957 953 945 956 32 118 953 948 949 961 949 961 32 953 957 32 99 952 956 44 32 949 958 32 945 952 964 949 956 32 956 945 955 953 963 32 959 966 966 949 957 948 953 964 32 949 963 964 46 32 921 948 32 964 959 955 955 953 964 32 963 952 963 99 953 960 953 964 32 963 945 955 952 964 945 964 952 963 32 113 952 953 44 32 99 952 32 113 952 945 957 948 959 32 960 961 959 960 961 953 945 949 32 953 956 960 949 961 948 953 949 964 32 118 949 955 44 32 956 949 945 32 949 964 32 949 961 961 949 956 32 947 955 959 961 953 945 964 952 961 46 32 917 958 949 961 99 953 32 948 949 966 953 957 953 964 953 959 957 949 956 32 956 949 955 32 949 945 46 32 917 958 32 963 949 945 32 949 955 953 964 32 948 953 963 963 949 957 964 953 949 964 44 32 956 945 955 953 963 32 964 961 953 964 945 957 953 32 118 953 963 32 953 948 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@90 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:59:01'!
exampleUtf32Hebrew: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(1506 1500 32 1502 1488 1502 1512 32 1494 1511 1493 1511 32 1513 1491 1512 1493 1514 32 1488 1512 1509 44 32 1492 1502 1500 1510 1514 32 1502 1491 1493 1497 1511 1497 1501 32 1513 1499 1500 32 1513 1500 44 32 1506 1497 1510 1493 1489 32 1504 1497 1493 1493 1496 32 1513 1497 1514 1493 1508 1497 1514 32 1506 1493 1491 32 1489 46 32 1505 1491 1512 32 1488 1493 32 1512 1508 1493 1488 1492 32 1513 1497 1502 1493 1513 1497 32 1502 1493 1506 1502 1491 1497 1501 46 32 1492 1497 1488 32 1508 1497 1505 1497 1511 1492 32 1492 1490 1512 1508 1497 1501 32 1513 1497 1502 1493 1513 1497 1497 1501 32 1513 1500 46 32 1513 1499 1500 32 1506 1500 32 1506 1494 1512 1492 32 1500 1492 1508 1493 1498 32 1488 1493 1493 1497 1512 1493 1504 1488 1493 1496 1497 1511 1492 44 32 1488 1504 1488 32 1488 1514 32 1506 1494 1512 1492 32 1500 1502 1504 1493 1506 32 1500 1502 1495 1497 1511 1492 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@250 font: aTrueTypeFont color: Color random. Display forceToScreen.
Display forceToScreen! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 4/1/2020 12:59:04'!
exampleUtf32Latin: aTrueTypeFont

| c utf32 |
c _ BitBltCanvas onForm: Display.
utf32 _ #(76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46) asIntegerArray.
c drawUtf32: utf32 from: 1 to: utf32 size at: 10@10 font: aTrueTypeFont color: Color random. Display forceToScreen.! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 12/18/2020 16:12:10'!
unicodeExamples
	"
	Preferences disable: #loadOnlyLatinGlyphData.
	BitBltCanvas unicodeExamples
	"
	"Note: For Chinese text sample, you need HanWangMingMedium or other suitable Unicode TrueType font with Chinese glyphs."
	| f |
	"[ f class == TrueTypeFont ] whileFalse: [ 
		f _ FontFamily promptUser atPointSize: 24 ]."
	f _ FontFamily familyName: 'DejaVu Sans' pointSize: 24.
	(f isNil or: [ f fontDescription contourDataForUtf8 isNil ]) ifTrue: [
		TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf'.
		f _ FontFamily familyName: 'DejaVu Sans' pointSize: 24. ].
	self exampleLatin: f.
	self exampleCyrillic: f.
	self exampleGreek: f.
	self exampleArmenian: f.
	self exampleGeorgian: f.
	self exampleArabic: f.
	self exampleHebrew: f.
	(FontFamily familyName: 'HanWangMingMedium' pointSize: 24) ifNil: [
		TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry currentDirectory // 'wt002.ttf' ].
	(FontFamily familyName: 'HanWangMingMedium' pointSize: 24) ifNotNil: [ :ff |
		self exampleChinese: ff ].! !

!BitBltCanvas class methodsFor: '*vectorGraphics-examples' stamp: 'jmv 6/16/2020 19:48:31'!
unicodeUtf32Examples
	"
	Preferences disable: #loadOnlyLatinGlyphData.
	TTFontDescription allInstancesDo: [ :each | each folderName: nil ]. 'hacky'.
	BitBltCanvas unicodeUtf32Examples
	"
	| f |
	[ f class == TrueTypeFont ] whileFalse: [ 
		f _ FontFamily promptUser atPointSize: 24 ].
	self exampleUtf32Latin: f.
	self exampleUtf32Cyrillic: f.
	self exampleUtf32Greek: f.
	self exampleUtf32Armenian: f.
	self exampleUtf32Georgian: f.
	self exampleUtf32Arabic: f.
	self exampleUtf32Hebrew: f.
	self exampleUtf32Chinese: f.! !
VectorCanvas initialize!
TTFontDescription initialize!
