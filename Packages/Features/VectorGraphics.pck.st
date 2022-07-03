'From Cuis 6.0 [latest update: #5171] on 15 June 2022 at 12:29:27 pm'!
'Description '!
!provides: 'VectorGraphics' 1 355!
!requires: 'Cuis-Base' 60 5160 nil!
!requires: 'Collections-CompactArrays' 1 10 nil!
SystemOrganization addCategory: 'VectorGraphics-Kernel'!
SystemOrganization addCategory: 'VectorGraphics-PathCommands'!
SystemOrganization addCategory: 'VectorGraphics-TrueType'!
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

!classDefinition: #Sample01Star category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample01Star
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample01Star class' category: 'VectorGraphics-Examples'!
Sample01Star class
	instanceVariableNames: ''!

!classDefinition: #Sample02Bezier category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample02Bezier
	instanceVariableNames: 'aStart aEnd aControlPoint bStart bEnd bControlPoint1 bControlPoint2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample02Bezier class' category: 'VectorGraphics-Examples'!
Sample02Bezier class
	instanceVariableNames: ''!

!classDefinition: #Sample03Smiley category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample03Smiley
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample03Smiley class' category: 'VectorGraphics-Examples'!
Sample03Smiley class
	instanceVariableNames: ''!

!classDefinition: #Sample04Pacman category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample04Pacman
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample04Pacman class' category: 'VectorGraphics-Examples'!
Sample04Pacman class
	instanceVariableNames: ''!

!classDefinition: #Sample06EasyToGrab category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample06EasyToGrab
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample06EasyToGrab class' category: 'VectorGraphics-Examples'!
Sample06EasyToGrab class
	instanceVariableNames: ''!

!classDefinition: #Sample07Clipping category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample07Clipping
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample07Clipping class' category: 'VectorGraphics-Examples'!
Sample07Clipping class
	instanceVariableNames: ''!

!classDefinition: #Sample08Form category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample08Form
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample08Form class' category: 'VectorGraphics-Examples'!
Sample08Form class
	instanceVariableNames: ''!

!classDefinition: #Sample09Clock category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample09Clock
	instanceVariableNames: 'hourHand minuteHand secondHand'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample09Clock class' category: 'VectorGraphics-Examples'!
Sample09Clock class
	instanceVariableNames: ''!

!classDefinition: #Sample09ClockHourHand category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample09ClockHourHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample09ClockHourHand class' category: 'VectorGraphics-Examples'!
Sample09ClockHourHand class
	instanceVariableNames: ''!

!classDefinition: #Sample09ClockMinuteHand category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample09ClockMinuteHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample09ClockMinuteHand class' category: 'VectorGraphics-Examples'!
Sample09ClockMinuteHand class
	instanceVariableNames: ''!

!classDefinition: #Sample09ClockSecondHand category: 'VectorGraphics-Examples'!
PlacedMorph subclass: #Sample09ClockSecondHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Examples'!
!classDefinition: 'Sample09ClockSecondHand class' category: 'VectorGraphics-Examples'!
Sample09ClockSecondHand class
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

!classDefinition: #AbstractVectorCanvas category: 'VectorGraphics-Kernel'!
MorphicCanvas subclass: #AbstractVectorCanvas
	instanceVariableNames: 'firstPoint lastControlPoint currentPoint'
	classVariableNames: 'IconPathData'
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'AbstractVectorCanvas class' category: 'VectorGraphics-Kernel'!
AbstractVectorCanvas class
	instanceVariableNames: ''!

!classDefinition: #BoundsFinderCanvas category: 'VectorGraphics-Kernel'!
AbstractVectorCanvas subclass: #BoundsFinderCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'BoundsFinderCanvas class' category: 'VectorGraphics-Kernel'!
BoundsFinderCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorCanvas category: 'VectorGraphics-Kernel'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorCanvas class' category: 'VectorGraphics-Kernel'!
VectorCanvas class
	instanceVariableNames: ''!

!classDefinition: #VectorEngine category: 'VectorGraphics-Kernel'!
Object subclass: #VectorEngine
	instanceVariableNames: 'canvas antiAliasingWidth geometryTransformation strokeWidth morphBoundsLeft morphBoundsTop morphBoundsRight morphBoundsBottom hop firstX firstY currentX currentY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngine class' category: 'VectorGraphics-Kernel'!
VectorEngine class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineBoundsFinder category: 'VectorGraphics-Kernel'!
VectorEngine subclass: #VectorEngineBoundsFinder
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom swDilatedHalf subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineBoundsFinder class' category: 'VectorGraphics-Kernel'!
VectorEngineBoundsFinder class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineDrawer category: 'VectorGraphics-Kernel'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor fillColor clipLeft clipTop clipRight clipBottom currentMorphId currentClipsSubmorphs clipCurrentMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineDrawer class' category: 'VectorGraphics-Kernel'!
VectorEngineDrawer class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineWithPlugin category: 'VectorGraphics-Kernel'!
VectorEngineDrawer subclass: #VectorEngineWithPlugin
	instanceVariableNames: ''
	classVariableNames: 'AccessLock'
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineWithPlugin class' category: 'VectorGraphics-Kernel'!
VectorEngineWithPlugin class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineWithPluginSubPixel category: 'VectorGraphics-Kernel'!
VectorEngineWithPlugin subclass: #VectorEngineWithPluginSubPixel
	instanceVariableNames: 'subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineWithPluginSubPixel class' category: 'VectorGraphics-Kernel'!
VectorEngineWithPluginSubPixel class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineWithPluginWholePixel category: 'VectorGraphics-Kernel'!
VectorEngineWithPlugin subclass: #VectorEngineWithPluginWholePixel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorGraphics-Kernel'!
!classDefinition: 'VectorEngineWithPluginWholePixel class' category: 'VectorGraphics-Kernel'!
VectorEngineWithPluginWholePixel class
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
	instanceVariableNames: 'rs xAxisRotations toPoints doLargerArcs useIncreasingAngles'
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
	instanceVariableNames: 'folderName contourDataForIso8859s15 contourDataIndexesByIso8859s15 contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'VectorGraphics-TrueType'!
!classDefinition: 'TTFontDescription class' category: 'VectorGraphics-TrueType'!
TTFontDescription class
	instanceVariableNames: ''!

!classDefinition: #TTFontReader category: 'VectorGraphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'glyphs nGlyphs kernPairs fontDescription'
	classVariableNames: 'FallbackArrowGlyphs'
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

!Sample01Star commentStamp: 'jmv 9/21/2021 10:09:51' prior: 0!
This class is a very simple Morph. It shows how to build your own Morph, and some of the features the framework provides.
This morph includes one important method: #drawOn: , that specifies the morph appareance.

You don't need to write any additional code, and yet this morph can:
- Be drawn with high quality, anti-aliased vector graphics, including translucency
- Be picked by the Hand (click and drag) only when you touch it, regardless of complex geometry
- Show the coordinate system in which it is drawn (open the halo with the middle mouse button)
- Be scaled and rotated (use the zoom or rotate halo handle)
- Be copied, explored, inspected, and browsed (use the appropriate halo handle)
- Be embedded in other morphs, and hold embedded morphs (use the menu halo handle)
- Be embedded in text (see below)

This class also includes #yAxisPointsUp. By default, like in most computer software, Y axis points down. By it is nicer to think of it increasing upwards, like in Math. This method allows for that, meaning a nicer #drawOn:.
Additionally, #rotationCenter is not really required, but it helps the star rotate neatly. See what happens if you delete it, and rotate the star.

Evaluate:
Sample01Star new openInWorld

Or try
Sample01Star new copyToClipboard
and then paste into the About Cuis text, or any text editor. See how you can still use the halo to scale and rotate. See how the morph is moved around as you edit the text.!

!Sample02Bezier commentStamp: '<historical>' prior: 0!
This class will help you familiarize with Bezier curves.
See also class comment at Sample01Star.

Each instance has random control points. There is very little code, so you can understand it all easily.

Evaluate:
10 timesRepeat: [Sample02Bezier new openInWorld].

Now have a Sample04Pacman at hand, and let it eat all these!!!

!Sample03Smiley commentStamp: '<historical>' prior: 0!
This class shows how to use the #pathSequence drawing operation, that can represent many drawing operations in one compact literal, and draw them at once.
See also class comment at Sample01Star.

0.0		x	y									Move pen to position x@y
1.0		x	y									Draw a line to position x@y
2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve

Evaluate:
Sample03Smiley new openInWorld!

!Sample04Pacman commentStamp: '<historical>' prior: 0!
This class shows how to use the #contour and #collides: features to detect if a morph touches another.
See also class comment at Sample01Star.

Beware!! This Pacman will try to eat anything!! Except their own kind. They are not cannibals!! Instead, they get happy when they meet.

Evaluate:
5 timesRepeat: [Sample04Pacman new openInWorld ]!

!Sample06EasyToGrab commentStamp: '<historical>' prior: 0!
This morph uses the #coversPixel: and #bitMask features to make it easy to grab although it is made of very thin lines. See #includesPixel: and play with it.
See also class comment at Sample01Star.

Evaluate:
Sample06EasyToGrab new openInWorld!

!Sample07Clipping commentStamp: '<historical>' prior: 0!
This morph shows clipping to arbitrary shapes. You can get a halo on the star and play with it (move, rotate, resize), and see how clipping works.

See also class comment at Sample01Star.

Evaluate:
Sample07Clipping new openInWorld!

!Sample08Form commentStamp: '<historical>' prior: 0!
This morph shows image rotating and scaling. Currently using WarpBlt. On top, some Vector Graphics.

See also class comment at Sample01Star.

Evaluate:
Sample08Form new openInWorld!

!Sample09Clock commentStamp: '<historical>' prior: 0!
A more sophisticated morph, showing stepping and submorphs. See how little code is needed for a good deal of functionality!!

See also class comment at Sample01Star.

Evaluate:
Sample09Clock new openInWorld!

!Sample09ClockHourHand commentStamp: '<historical>' prior: 0!
Sample09Clock new openInWorld!

!Sample09ClockMinuteHand commentStamp: '<historical>' prior: 0!
Sample09Clock new openInWorld!

!Sample09ClockSecondHand commentStamp: '<historical>' prior: 0!
Sample09Clock new openInWorld!

!HybridCanvas commentStamp: '<historical>' prior: 0!
A HybridCanvas works as a BitBltCanvas until faced with a situation that BitBlitCanvas can't handle. At that point, the vectorCanvas takes command.

self runningWorld setCanvas: (HybridCanvas onForm: Display).
self runningWorld canvas class!

!AbstractVectorCanvas commentStamp: 'jmv 10/8/2021 09:46:48' prior: 0!
A VectorGraphics canvas specially built for Morphic.

self runningWorld backgroundImage displayAt: 0@0. (VectorCanvas onForm: Display) fullDraw: self runningWorld. Display copy inspect.
Display writePNGfileNamed: 'm3z.png'

On any Morph:
| c |
c _ VectorCanvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self.
Display forceToScreen!

!VectorEngine commentStamp: '<historical>' prior: 0!
A Vector Graphics rasterizer for Morphic.
Supports most SVG primitives.
Gives exceptional quality.

Based on https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics!

!VectorEngineBoundsFinder commentStamp: '<historical>' prior: 0!
Morph Bounds Finder!

!VectorEngineWithPlugin commentStamp: 'jmv 10/8/2021 09:49:07' prior: 0!
"Just an experiment to exercise drawing operations."
| m |
m _ M3Exp01Morph new.
m morphPosition: 15@30.
(VectorCanvas onForm: Display) inspect; fullDraw: m.
Display forceToScreen.
"Not sure if fallback code should use a pluginSmalltalkCode ivar, calling the VectorEnginePlugin in smalltalk, or duplicate that code here for fallback code (this would be better if those start to diverge because of optimizations)"!

!VectorEngineWithPluginSubPixel commentStamp: 'jmv 1/16/2022 12:58:41' prior: 0!
Does sub pixel antialiasing, both for text and graphics. Appropriate for low to medium resolution displays, where subpixel antialiasing improves quality. Consumes more CPU than VectorEngineWithPluginWholePixel.
Variables edgeCounts and alphaMask are 4 bytes per pixel (3 actually used).
Uses VectorEnginePlugin for performance.!

!VectorEngineWithPluginWholePixel commentStamp: '<historical>' prior: 0!
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineWithPluginSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.
As a performance optimization, assumes targetForm opacity is 1 for all pixels. Does not compute target translucency. If you need target translucency computed correctly, please use VectorEngineWithPlginSubPixel (maybe setting subPixelDelta to 0.0, to effectively disable subpixel rasterization).
Uses VectorEnginePlugin for performance.!

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

!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 6/15/2022 12:27:30'!
resetAssignmentGlyph
	"Uses ST80 glyphs:"
	(emphasis anyMask: 64) ifTrue: [
		"Re-cache glyphs that could have changed"
		glyphForms at: $_ numericValue + 1 put: nil.
		glyphFormsByUtf8 at: $_ numericValue + 1 put: nil ].! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 3/2/2020 17:25:54'!
ascent
	^ttFontDescription ascent * pointSize / ttFontDescription letterMTopSideBearing * 1.0! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/7/2019 10:11:03'!
baseFont
	^ baseFont ifNil: [self]! !

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

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 3/13/2022 19:03:59'!
widthOf: aCharacter

	^ (ttFontDescription widthOf: aCharacter useST80Glyphs: self isST80Glyphs) * pointSize! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/5/2022 12:04:37'!
widthOfCodePoint: aNumber

	^ (ttFontDescription widthOfCodePoint: aNumber useST80Glyphs: self isST80Glyphs) * pointSize! !

!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 5/5/2022 11:41:15'!
widthOfUtf8Byte1: aByte1 b2: aByte2OrNil b3: aByte3OrNil b4: aByte4OrNil

	^ (ttFontDescription
		widthOfUtf8Byte1: aByte1 b2: aByte2OrNil b3: aByte3OrNil b4: aByte4OrNil
		useST80Glyphs: self isST80Glyphs)
			* pointSize! !

!TrueTypeFont methodsFor: 'displaying' stamp: 'jmv 5/2/2022 16:41:54'!
onBitBltCanvasEngine: engine displayString: aString from: startIndex to: stopIndex at: p color: color
	"Answer position to place next glyph.
	Answer nil if nothing was done."

	^aString displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: self color: color! !

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

!TrueTypeFont methodsFor: 'glyph access' stamp: 'jmv 3/26/2021 10:40:47'!
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
		formGlyph _ (ttFontDescription
				formGlyphForUtf8Byte1: byte1 byte2: byte2OrNil
				byte3: byte3OrNil byte4: byte4OrNil
				pixelSize: pointSize
				useST80Glyphs: self isST80Glyphs)
			ifNil: [#absentGlyph]
			ifNotNil: [ :theGlyph |
				(emphasis allMask: 4) ifTrue: [
					self makeUnderlinedGlyph: theGlyph ].
				(emphasis allMask: 8) ifTrue: [
					self makeStruckThroughGlyph: theGlyph ].
				theGlyph].
		lastArray at: lastIndex put: formGlyph.
		formGlyph ]! !

!TrueTypeFont methodsFor: 'emphasis' stamp: 'jmv 6/15/2022 12:27:40'!
addEmphasis: code
	"Set the integer code for emphasis. See senders."

	emphasis _ code.
	(emphasis anyMask: 4+8+64) ifTrue: [
		"They need to be recreated"
		glyphForms _ Array new: 256.
		glyphFormsByUtf8 _ Array new: 256 ].! !

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
	selector _ Preferences at: #assignmentGlyphSelector.
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

!TrueTypeFontFamily methodsFor: 'accessing' stamp: 'jmv 9/24/2021 10:09:50'!
pointSizes
	^#(6 7 8 9 10 11 12 13 14 15 18 22 26 36 48 72 96 144)! !

!TrueTypeFontFamily methodsFor: 'testing' stamp: 'jmv 5/12/2022 10:36:10'!
includesAscii
	"Answer true if we include the basic ASCII characters.
	If not, we are not suitable, for instance, for displaying Smalltalk code."
	^baseTTFontDescription includesAscii! !

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

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:25:29'!
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
		PreferenceSet setDefaultFont:	 familyName ].
	('loaded font ', familyName) print.! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 7/18/2021 20:39:48'!
serviceReadTTFont
	"Answer the service of reading a TrueType font"

	^ (SimpleServiceEntry
		provider: self 
		label: 'load font'
		selector: #readTrueTypeFontEntry:
		description: 'load TrueType font'
		buttonLabel: 'read font'
		icon: #fontIcon
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!Sample01Star methodsFor: 'drawing' stamp: 'jmv 9/14/2021 18:38:15'!
drawOn: aCanvas
	"A 5 point star.
	Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!!
	Sample01Star new openInHand
	"

	aCanvas strokeWidth: 12 color: Color lightOrange do: [
		aCanvas
				moveTo: (Point rho: 100 theta: 90 degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*2+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*4+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*6+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*8+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: 90 degreesToRadians).
		].! !

!Sample01Star methodsFor: 'geometry testing' stamp: 'jmv 9/14/2021 19:07:06'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!Sample01Star methodsFor: 'geometry' stamp: 'jmv 9/14/2021 19:06:41'!
rotationCenter
	"Smooth rotation around the origin.
	(Optional. See what happens if you delete this method)"

	^0@0! !

!Sample02Bezier methodsFor: 'initialization' stamp: 'jmv 9/1/2021 16:21:44'!
initialize
	super initialize.
	aStart _ 500 atRandom @ 500 atRandom.
	aEnd _ 500 atRandom @ 500 atRandom.
	aControlPoint _ 500 atRandom @ 500 atRandom.
	bStart _ 500 atRandom @ 500 atRandom.
	bEnd _ 500 atRandom @ 500 atRandom.
	bControlPoint1 _ 500 atRandom @ 500 atRandom.
	bControlPoint2 _ 500 atRandom @ 500 atRandom.! !

!Sample02Bezier methodsFor: 'drawing' stamp: 'jmv 9/1/2021 16:20:48'!
drawOn: aCanvas
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 10 color: Color green do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].
	aCanvas strokeWidth: 2 color: Color green * 0.5 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 10 color: Color red do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 2 color: Color red * 0.5 do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].! !

!Sample02Bezier methodsFor: 'geometry testing' stamp: 'jmv 9/14/2021 19:07:14'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!Sample03Smiley methodsFor: 'drawing' stamp: 'jmv 8/27/2021 12:01:35'!
drawOn: aCanvas
	"Example of the #pathSequence drawing operation, that can represent many drawing operations
			in one compact literal, and draw them at once.
	0.0		x	y									Move pen to position x@y
	1.0		x	y									Draw a line to position x@y
	2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
	3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve
	See #circleApproximationExamples
	Sample03Smiley new openInHand
	"

	aCanvas strokeWidth: 30 color: `Color darkGray alpha: 0.8` fillColor: `Color lightGreen alpha: 0.7` do: [
		aCanvas
				pathSequence: `#[
					0.0  150.0  150.0
					3.0  300.0  300.0  150.0  233.0  217.0  300.0
					3.0  450.0  150.0  380.3  300.0  450.0  233.0
					3.0  300.0  0.0      450.0  67.0    383.0  0.0
					3.0  150.0  150.0  210.7  0.0      150.0  67.0
					] asFloat32Array`
		].

	aCanvas strokeWidth: 30 color: `Color darkGray` do: [
		aCanvas
				pathSequence: `#[
					0.0 220.0 190.0
					2.0 300.0 240.0 245.0 240.0
					2.0 380.0 190.0 355.0 240.0
					0.0 250.0 100.0
					1.0 250.0 120.0
					0.0 350.0 100.0
					1.0 350.0 120.0
					] asFloat32Array`
		].! !

!Sample04Pacman methodsFor: 'initialization' stamp: 'jmv 8/27/2021 12:28:49'!
initialize

	super initialize.
	color _ Color yellow! !

!Sample04Pacman methodsFor: 'drawing' stamp: 'jmv 9/15/2021 15:21:15'!
drawOn: aCanvas

	aCanvas fillColor: color do: [
		aCanvas circleCenter: 30@60 radius: 15.
		aCanvas
			moveTo: 0@0;
			lineTo: 100@ -50;
			arcTo: 100@ 50 radius: 110 angleOfXAxis: 0 largeFlag: true sweepFlag: false;
			lineTo: 0@0.
		].! !

!Sample04Pacman methodsFor: 'geometry testing' stamp: 'jmv 8/27/2021 14:07:53'!
wantsContour
	"Because we want precise #collides:"

	^true! !

!Sample04Pacman methodsFor: 'geometry testing' stamp: 'jmv 9/14/2021 19:08:46'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!Sample04Pacman methodsFor: 'updating' stamp: 'jmv 8/27/2021 14:07:15'!
colorize
	color _ Color random.
	self redrawNeeded.! !

!Sample04Pacman methodsFor: 'updating' stamp: 'jmv 9/2/2021 16:59:16'!
redrawNeeded
	"for experimenting with collision detection"

	self isOwnedByHand ifTrue: [
		self world ifNotNil: [ :w |
			w submorphsDo: [ :m |
				(self collides: m) ifTrue: [
					m class == self class
						ifTrue: [ m colorize ]
						ifFalse: [ m delete ]]]]].

	^super redrawNeeded! !

!Sample06EasyToGrab methodsFor: 'drawing' stamp: 'jmv 9/2/2021 16:45:31'!
drawOn: aCanvas

	aCanvas strokeWidth: 1 color: Color black do: [
		aCanvas
			moveTo: 0@0;
			lineTo: 200@200;
			lineTo: -200@200;
			closePath ].! !

!Sample06EasyToGrab methodsFor: 'geometry testing' stamp: 'jmv 2/24/2022 15:32:33'!
includesPixel: worldPoint
	"Allow grabbing if close enough.
	See other implementors.
	This is an example of the use of #coversAnyPixelCloserThan:to:
	Usually, you'd prefer something like (better if super call is actually inlined):
	-10 to: 10 do: [ :dy |
		-10 to: 10 do: [ :dx |
			(super includesPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]].
	^false
	"

	^ self visible and: [self coversAnyPixelCloserThan: 10 to: worldPoint].! !

!Sample07Clipping methodsFor: 'geometry testing' stamp: 'jmv 9/2/2021 16:52:16'!
clipsSubmorphs

	^ true! !

!Sample07Clipping methodsFor: 'initialization' stamp: 'jmv 9/2/2021 16:53:09'!
initialize

	super initialize.
	self addMorph: Sample01Star new.! !

!Sample07Clipping methodsFor: 'drawing' stamp: 'jmv 9/2/2021 16:55:26'!
drawOn: aCanvas

	aCanvas strokeWidth: 15 color: (Color green alpha: 0.8) fillColor: (Color red alpha: 0.6) do: [
		aCanvas
			roundRectangleMinX: -10 maxX: 220 minY: -10 maxY: 120  rx: 120 ry: 50 ]! !

!Sample08Form methodsFor: 'drawing' stamp: 'jmv 9/2/2021 17:20:56'!
drawOn: aCanvas

	aCanvas image: ImageMorph defaultForm at: -10@ -10.

	aCanvas strokeWidth: 4 color: (Color green alpha: 0.6) do: [
		aCanvas
			ellipseCenter: 36@60 radius: 8 @ 8 ].

	aCanvas strokeWidth: 10 color: (Color black alpha: 0.2) fillColor: (Color yellow alpha: 0.4) do: [
		aCanvas
			ellipseCenter: 36@60 radius: 24 @ 24 ].
! !

!Sample09Clock methodsFor: 'initialization' stamp: 'jmv 9/2/2021 17:40:09'!
initialize

	super initialize.
	self addMorph: (hourHand _ Sample09ClockHourHand new).
	self addMorph: (minuteHand _ Sample09ClockMinuteHand new).
	self addMorph: (secondHand _ Sample09ClockSecondHand new).

	(FontFamily familyName: 'Alex Brush' pointSize: 16) ifNil: [
		TrueTypeFontFamily readTrueTypeFontEntry:
			DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'AlexBrush' // 'AlexBrush-Regular.ttf' ]! !

!Sample09Clock methodsFor: 'drawing' stamp: 'jmv 9/15/2021 15:27:51'!
drawOn: aCanvas

	| f |
	aCanvas
		strokeWidth: 10 color: Color lightCyan
		fillColor: Color veryVeryLightGray do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	aCanvas drawString: 'XII' at: -13 @ 58 font: nil color: Color brown.
	aCanvas drawString: 'III' at: 72 @ -23 font: nil color: Color brown.
	aCanvas drawString: 'VI' at: -8 @ -105 font: nil color: Color brown.
	aCanvas drawString: 'IX' at: -90 @ -23 font: nil color: Color brown.
	
	f _ FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Jacquier' atCenterX: 0@ 18 font: f color: Color black.
	f _ FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: 0@ -60 font: f color: Color black.! !

!Sample09Clock methodsFor: 'stepping' stamp: 'jmv 9/15/2021 09:51:55'!
step

	| time |
	time _ Time now.
	hourHand rotationDegrees: time hour * -30 + (time minute * -0.5).
	minuteHand rotationDegrees: time minute * -6.
	secondHand rotationDegrees: time second * -6.! !

!Sample09Clock methodsFor: 'stepping' stamp: 'jmv 9/2/2021 17:35:52'!
wantsSteps

	^ true! !

!Sample09Clock methodsFor: 'geometry testing' stamp: 'jmv 9/15/2021 09:56:32'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!Sample09ClockHourHand methodsFor: 'drawing' stamp: 'jmv 9/2/2021 17:33:33'!
drawOn: aCanvas

	aCanvas fillColor: (Color black alpha: 0.6) do: [
		aCanvas
			moveTo: 0 @ 10;
			lineTo: -5 @ 0;
			lineTo: 0 @ -50;
			lineTo: 5 @ 0;
			lineTo: 0 @ 10 ].! !

!Sample09ClockMinuteHand methodsFor: 'drawing' stamp: 'jmv 9/2/2021 17:32:59'!
drawOn: aCanvas

	aCanvas fillColor: (Color black alpha: 0.6) do: [
		aCanvas
			moveTo: 0 @ 8;
			lineTo: -4 @ 0;
			lineTo: 0 @ -82;
			lineTo: 4 @ 0;
			lineTo: 0 @ 8 ].! !

!Sample09ClockSecondHand methodsFor: 'drawing' stamp: 'jmv 9/2/2021 17:32:20'!
drawOn: aCanvas

	aCanvas strokeWidth: 3.5 color: Color red do: [
		aCanvas
			moveTo: 0 @ 0;
			lineTo: 0 @ -85 ].! !

!HybridCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 10:35:56'!
boundsFinderCanvas
	^vectorCanvas boundsFinderCanvas! !

!HybridCanvas methodsFor: 'drawing-images' stamp: 'jmv 10/29/2021 10:54:36'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency."
	| p e |

	drawingOnMorphIDs ifTrue: [
		"We transform p but not e. This is on purpose. We want to affect exactly the same rect as super."
		e _ sourceRect extent.
		e > `0@0` ifFalse: [ ^self ].
		p _ (currentTransformation transform: aPoint) roundedHAFZ.
		engine combinationRule: 0.
		engine fillRect: (p extent: e).
		^self ].

	super image: aForm at: aPoint sourceRect: sourceRect! !

!HybridCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/29/2021 10:54:34'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	| rect |

	drawingOnMorphIDs ifTrue: [
		aRectangle area = 0 ifTrue: [ ^self ].
		rect _ (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.
		engine combinationRule: 0.
		engine fillRect: rect.
		^self ].

	super fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:39:40'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/16/2021 08:50:23'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor! !

!HybridCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/26/2022 17:15:20'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: strict

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: strict! !

!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 9/27/2021 17:20:10'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	vectorCanvas initializeWithTranslation: aPoint.! !

!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 9/6/2021 11:22:58'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	super resetCanvas.
	vectorCanvas resetCanvas.! !

!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 10/24/2020 16:04:17'!
world: aWorldMorph
	super world: aWorldMorph.
	vectorCanvas world: aWorldMorph! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/6/2021 11:16:05'!
canvasToUse

	currentMorph ifNotNil: [
		currentMorph requiresVectorCanvas ifTrue: [
			^ vectorCanvas ]].
	^self.! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 8/16/2021 16:32:08'!
onMorphIdsDo: aBlock

	engine instVarNamed: 'destForm' put: morphIDsForm.
	drawingOnMorphIDs _ true.
	aBlock ensure: [
		engine instVarNamed: 'destForm' put: form.
		drawingOnMorphIDs _ false ].! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 7/11/2020 10:55:26'!
setClipRect: aRectangle
	super setClipRect: aRectangle.
	vectorCanvas setClipRect: aRectangle! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 10/8/2021 09:46:00'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

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

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 1/4/2021 17:22:17'!
currentMorphDrawingFails

	| c |
	c _ self canvasToUse.
	c == self ifFalse: [ ^c currentMorphDrawingFails ].
	super currentMorphDrawingFails! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:26'!
drawCurrentAndSubmorphs
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	currentMorph visible ifTrue: [

		currentMorph drawOn: self.
		self onMorphIdsDo: [
			currentMorph drawOn: self ].
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			self onMorphIdsDo: [
				currentMorph postDrawOn: self ].
			self displayBoundsUpdateOnCurrentMorph ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 8/27/2021 19:50:41'!
drawCurrentAsError

	vectorCanvas drawCurrentAsError! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:18:28'!
drawHand

	vectorCanvas drawHand! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/9/2020 16:36:45'!
into: aMorph
	super into: aMorph.
	vectorCanvas into: aMorph! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 9/20/2021 12:08:20'!
morphIdAt: aPoint

	^ vectorCanvas morphIdAt: aPoint! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/9/2020 16:36:59'!
outOfMorph
	super outOfMorph.
	vectorCanvas outOfMorph! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 15:41:44'!
restorePatch: savedPatch bounds: savedBounds
	"Argument must previously be obtained by calling #savePatch:bounds:"

	vectorCanvas restorePatch: savedPatch bounds: savedBounds! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 15:42:20'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:bounds:"
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

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:52:18'!
drawCloseIcon
	vectorCanvas  drawCloseIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:52:45'!
drawCollapseIcon
	vectorCanvas  drawCollapseIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:53:03'!
drawDownIcon
	vectorCanvas  drawDownIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:53:20'!
drawExpandIcon
	vectorCanvas  drawExpandIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:53:33'!
drawLeftIcon
	vectorCanvas  drawLeftIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:53:44'!
drawMenuIcon
	vectorCanvas  drawMenuIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 15:01:48'!
drawPushPinIcon
	vectorCanvas  drawPushPinIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:54:32'!
drawRightIcon
	vectorCanvas  drawRightIcon! !

!HybridCanvas methodsFor: 'drawing-icons' stamp: 'jmv 8/19/2021 14:54:44'!
drawUpIcon
	vectorCanvas  drawUpIcon! !

!HybridCanvas methodsFor: 'drawing-windows' stamp: 'jmv 8/19/2021 15:00:13'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight
	"Title area is not inside window borders"
	| bottomFactor topFactor |
			topFactor _ 1.
			bottomFactor _ 1.
	self
		windowFrame: aRectangle
		color: borderColor
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: insideColor! !

!HybridCanvas methodsFor: 'testing' stamp: 'jmv 10/27/2021 16:10:11'!
canDoVectorGraphics
	^true! !

!HybridCanvas methodsFor: 'testing' stamp: 'jmv 8/27/2021 16:04:24'!
usesVectorEnginePlugin
	^vectorCanvas usesVectorEnginePlugin! !

!AbstractVectorCanvas methodsFor: 'accessing' stamp: 'jmv 10/14/2014 11:02'!
engine
	^engine! !

!AbstractVectorCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 10:31:49'!
geometryTransformation: aGeometryTransformation
	"Only used for testing. Usually, transformations are handled as we traverse the Morphs tree."

	currentTransformation _ aGeometryTransformation.
	engine geometryTransformation: aGeometryTransformation.! !

!AbstractVectorCanvas methodsFor: 'accessing' stamp: 'jmv 8/16/2021 15:34:57'!
morphIds
	^engine morphIds! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 3/17/2021 10:15:59'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self initializePath.
	engine fillColor: aFillColor do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/10/2021 11:17:46'!
initializePath
	"Perform any initialization needed before starting a Path"

	firstPoint _ nil.
	currentPoint _ `0.0@0.0`.
	lastControlPoint _ currentPoint.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/12/2021 15:45:37'!
strokeWidth: strokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/12/2021 15:47:24'!
strokeWidth: strokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."
	"Gradients should be transformed with currentTransformation!!"

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:13'!
arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.2 position: 200@100);
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"

	engine arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles.
	currentPoint _ aPoint.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:46:54'!
circleCenter: aPoint radius: mr
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			circleCenter: 200 @ 150 radius: 80].
Display forceToScreen
"

	engine circleCenter: aPoint radius: mr! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:46:57'!
closePath
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePath;
			lineTo: 150@300 ].
Display forceToScreen
"

	firstPoint notNil ifTrue: [
		self lineTo: firstPoint ].! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 12/16/2021 12:12:24'!
closePathAndMoveBack
	"Close the current path, but don't change the current position"
"
(VectorCanvas onForm: Display)
	strokeWidth: 12.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePath;
			lineTo: 150@300 ].
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePathAndMoveBack;
			lineTo: 150@300 ].
Display forceToScreen

"

	| cp |
	(firstPoint notNil and: [ firstPoint ~= currentPoint]) ifTrue: [
		cp _ currentPoint.
		self lineTo: firstPoint.
		self moveTo: cp ].! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 5/10/2021 11:41:11'!
cubicBezier: points
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	self moveTo: points first.
	self
		cubicBezierTo: points fourth
		control1: points second
		control2: points third! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:49:13'!
cubicBezierSmoothTo: aPoint control2: controlPoint
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 150@300 radius: 10.
		canvas circleCenter: 600@400 radius: 6.
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 600 @ -200 radius: 10.
		canvas circleCenter: 100 @ -100 radius: 6.
		canvas moveTo: 600 @ 100.
		canvas
			cubicBezierTo: 300@100 control1: 600 @ -200 control2: 100 @ -100
			].
Display forceToScreen.
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			cubicBezierSmoothTo: 300@100 control2: 100 @ -100
			].
Display forceToScreen
	"
	| mirroredLastControlPoint |
	mirroredLastControlPoint _ currentPoint + (currentPoint - lastControlPoint).
	self cubicBezierTo: aPoint control1: mirroredLastControlPoint control2: controlPoint! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:18'!
cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"
	engine cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2.
	currentPoint _ aPoint.
	lastControlPoint _ currentPoint.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 5/10/2021 11:47:09'!
curveTo: aPoint control1: control1Point control2: control2Point

	self cubicBezierTo: aPoint control1: control1Point control2: control2Point! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:20'!
ellipseCenter: aPoint radius: radiusPoint
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50].
Display forceToScreen
"
	engine ellipseCenter: aPoint radius: radiusPoint ! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:22'!
ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
Display forceToScreen
"

	engine ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:49:15'!
ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
	"Currently draws a line from pathCurrentX@pathCurrentY to start (if startAngle ~= 0.0 or sweepAngle ~= Float twoPi).
	This could be useful for proper fills. But might not be what is desired in some cases.
	Implement desired behavior when clear."
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@350 radius: 100@50 rotationAngle: 0.5 start: 0.1 sweep: 6 ].
Display forceToScreen.
"

	engine ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:26'!
lineTo: aPoint
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineTo: 15@30.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	engine lineTo: aPoint.
	currentPoint _ aPoint.
	lastControlPoint _ currentPoint.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:28'!
lineToX: xTo
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineToX: 200 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineToX: 50.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: xTo @ currentPoint y! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:30'!
lineToY: yTo
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineToY: 200 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineToY: 50.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: currentPoint x @ yTo.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:47:40'!
moveTo: aPoint
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150 ].
Display forceToScreen
"
	engine moveTo: aPoint.
	firstPoint _ aPoint.
	currentPoint _ aPoint.
	lastControlPoint _ currentPoint.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 5/18/2021 10:09:21'!
polyLine: points

	| p |
	p _ points first.
	self moveTo: p.
	2 to: points size do: [ :i |
		p _ points at: i.	
		self lineTo: p]! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 5/10/2021 11:58:30'!
quadraticBezier: points
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	self moveTo: points first.
	self quadraticBezierTo: points third control: points second.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:34'!
quadraticBezierSmoothTo: aPoint
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 450@300 radius: 10.
		canvas moveTo: 100@50.
		canvas
			quadraticBezierTo: 600@100 control: 450@300
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 750 @ -100 radius: 10.
		canvas moveTo: 600@100.
		canvas
			quadraticBezierTo: 300@100 control: 750 @ -100
			].
Display forceToScreen.
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			quadraticBezierTo: 600@100 control: 450@300;
			quadraticBezierSmoothTo: 300@100
			].
Display forceToScreen
"
	| mirroredLastControlPoint |
	mirroredLastControlPoint _ currentPoint + (currentPoint - lastControlPoint).
	self quadraticBezierTo: aPoint control: mirroredLastControlPoint! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 10/8/2021 09:48:35'!
quadraticBezierTo: aPoint control: controlPoint
	"
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			quadraticBezierTo: 300@200 control: 100@210 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas quadraticBezierTo: 60@10 control: 15@30 ].
Display forceToScreen
	"
	engine quadraticBezierTo: aPoint control: controlPoint.
	currentPoint _ aPoint.
	lastControlPoint _ currentPoint.! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 5/10/2021 11:28:30'!
rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax

	self
		moveTo: xMin @ yMin;
		lineTo: xMax @ yMin;
		lineTo: xMax @ yMax;
		lineTo: xMin @ yMax;
		closePath! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 7/20/2021 10:22:50'!
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
	self
		moveTo: corner1;
		lineTo: corner2;
		lineTo: p5;
		quadraticBezierTo: p6 control: corner3;
		lineTo: p7;
		quadraticBezierTo: p8 control: corner4;
		closePath! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 7/20/2021 10:23:09'!
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
	self
		moveTo: p1;
		quadraticBezierTo: p2 control: corner1;
		lineTo: p3;
		quadraticBezierTo: p4 control: corner2;
		lineTo: p5;
		quadraticBezierTo: p6 control: corner3;
		lineTo: p7;
		quadraticBezierTo: p8 control: corner4;
		closePath! !

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'jmv 7/20/2021 10:23:21'!
roundTopRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^ self rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

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
	self
		moveTo: p1;
		quadraticBezierTo: p2 control: corner1;
		lineTo: p3;
		quadraticBezierTo: p4 control: corner2;
		lineTo: corner3;
		lineTo: corner4;
		closePath! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:47:49'!
rel_ArcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorCanvas onForm: Display)
	strokeWidth: 5.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			rel_ArcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false;
			closePath ].
Display forceToScreen
"
	self arcTo: currentPoint + aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:49:17'!
rel_CubicBezierSmoothTo: aPoint control2: controlPoint
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!!"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			rel_CubicBezierTo: 300@300 control1: 0@0 control2: 100@100
			].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			rel_CubicBezierSmoothTo: 300@300 control2: 100@100
			].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierSmoothTo: 600@100 control2: 600@400;
			cubicBezierSmoothTo: 300@300 control2: 100@100
			].
Display forceToScreen
	"

	self
		cubicBezierSmoothTo: currentPoint + aPoint
		control2: currentPoint + controlPoint! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:48:39'!
rel_CubicBezierTo: aPoint control1: control1Point control2: control2Point
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ];
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_CubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ];
	strokeWidth: 10 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			rel_CubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"

	self
		cubicBezierTo: currentPoint+aPoint
		control1: currentPoint+control1Point
		control2: currentPoint+control2Point! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:48:43'!
rel_LineTo: aPoint
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150];
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_LineTo: 200@150].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineTo: 15@30.
		canvas lineTo: 60@10 ];
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas rel_LineTo: 15@30.
		canvas rel_LineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: currentPoint + aPoint! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:47:56'!
rel_MoveTo: aPoint
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150];
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_MoveTo: 10@20;
			lineTo: 200@150].
Display forceToScreen
"
	self moveTo: currentPoint + aPoint! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:49:19'!
rel_QuadraticBezierSmoothTo: aPoint
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 550@350 radius: 10.
		canvas moveTo: 100@50.
		canvas
			rel_QuadraticBezierTo: 600@100 control: 450@300
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 850 @ -50 radius: 10.
		canvas moveTo: 700@150.
		canvas
			rel_QuadraticBezierTo: -300 @ -50 control: 150 @ -200
			].
Display forceToScreen.
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			rel_QuadraticBezierTo: 600@100 control: 450@300;
			rel_QuadraticBezierSmoothTo: -300 @ -50
			].
Display forceToScreen
"
	self
		quadraticBezierSmoothTo: currentPoint + aPoint! !

!AbstractVectorCanvas methodsFor: 'paths - relative coordinates' stamp: 'jmv 10/8/2021 09:48:48'!
rel_QuadraticBezierTo: aPoint control: controlPoint
	"
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			quadraticBezierTo: 300@200 control: 100@210 ];
	strokeWidth: 3.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_QuadraticBezierTo: 300@200 control: 100@210 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas quadraticBezierTo: 60@10 control: 15@30 ];
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color green alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas rel_QuadraticBezierTo: 60@10 control: 15@30 ].
Display forceToScreen
	"
	self
		quadraticBezierTo: currentPoint + aPoint
		control: currentPoint + controlPoint! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 6/18/2021 11:56:58'!
multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngles: useIncreasingAngles

	| to r |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				r _ rs at: i.
				self arcTo: to radius: r angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngles at: i) ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				r _ rs at: i.
				self rel_ArcTo: to radius: r angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngles at: i) ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/18/2021 10:09:24'!
multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points

	| c1 c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c1 _ control1Points at: i.
				c2 _ control2Points at: i.
				self cubicBezierTo: to control1: c1 control2: c2 ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c1 _ control1Points at: i.
				c2 _ control2Points at: i.
				self rel_CubicBezierTo: to control1: c1 control2: c2 ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 11:45:30'!
multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points

	| c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c2 _ control2Points at: i.
				self cubicBezierSmoothTo: to control2: c2 ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c2 _ control2Points at: i.
				self rel_CubicBezierSmoothTo: to control2: c2 ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 11:34:55'!
multiLineTo: coordinatesAreRelative points: toPoints

	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				self lineTo: (toPoints at: i) ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				self rel_LineTo:(toPoints at: i) ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 11:35:18'!
multiLineTo: coordinatesAreRelative xs: xs

	coordinatesAreRelative
		ifFalse: [
			xs do: [ :x |
				self lineToX: x ]]
		ifTrue: [
			xs do: [ :x |
				self rel_LineTo: x @ 0 ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 11:35:26'!
multiLineTo: coordinatesAreRelative ys: ys

	coordinatesAreRelative
		ifFalse: [
			ys do: [ :y |
				self lineToY: y ]]
		ifTrue: [
			ys do: [ :y |
				self rel_LineTo: 0 @ y ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 11:27:37'!
multiMoveTo: coordinatesAreRelative points: points

	| isFirst |
	isFirst _ true.
	coordinatesAreRelative
		ifFalse: [
			points do: [ :p |
				isFirst
					ifTrue: [ self moveTo: p ]
					ifFalse: [ self lineTo: p ].
				isFirst _ false ]]
		ifTrue: [
			points do: [ :p |
				isFirst
					ifTrue: [ self rel_MoveTo: p ]
					ifFalse: [ self rel_LineTo: p ].
				isFirst _ false ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 12:02:30'!
multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints

	| c to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c _ controlPoints at: i.
				self quadraticBezierTo: to control: c ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				c _ controlPoints at: i.
				self rel_QuadraticBezierTo: to control: c ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 5/10/2021 12:00:41'!
multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints

	| to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				self quadraticBezierSmoothTo: to ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to _ toPoints at: i.
				self rel_QuadraticBezierSmoothTo: to ]]! !

!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 10/8/2021 09:47:42'!
pathSequence: aFloat32Array
	"
	'Circle approximation by bezier curves
	https://spencermortensen.com/articles/bezier-circle/'.
	(VectorCanvas onForm: Display)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color random do: [ :engine |
			engine
				pathSequence: `#[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 23.3 21.7 30.0
					3.0 45.0 15.0 38.3 30.0 45.0 23.3
					3.0 30.0 0.0 45.0 6.7 38.3 0.0
					3.0 15.0 15.0 21.7 0.0 15.0 6.7
					0.0 22.0 19.0
					2.0 30.0 24.0 24.5 24.0
					2.0 38.0 19.0 35.5 24.0
					0.0 25.0 10.0
					1.0 25.0 12.0
					0.0 35.0 10.0
					1.0 35.0 12.0
					] asFloat32Array`
				].
	Display forceToScreen
	"
	engine pathSequence: aFloat32Array! !

!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 5/2/2022 16:46:56'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString displayOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/5/2021 14:39:31'!
drawCloseIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c _ `Color r: 0.996 g: 0.329 b: 0.329`.
			s _ `Color veryDarkGray` ]
		ifTrue: [
			c _ `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s _ `Color black` ].

	size _ currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonClosePathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/5/2021 14:43:27'!
drawCollapseIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c _ `Color r: 0.988 g: 0.557 b: 0.102`.
			s _ `Color veryDarkGray` ]
		ifTrue: [
			c _ `(Color r: 0.988 g: 0.557 b: 0.102) * 0.85`.
			s _ `Color black` ].

	size _ currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonCollapsePathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/26/2021 12:23:01'!
drawDownIcon
	| c size |
	c _ currentMorph isPressed
		ifFalse: [`Color darkGray`]
		ifTrue: [`Color veryDarkGray`].
	size _ currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollDownPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/5/2021 14:43:53'!
drawExpandIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c _ `Color r: 0.337 g: 0.604 b: 0.161`.
			s _ `Color veryDarkGray` ]
		ifTrue: [
			c _ `(Color r: 0.337 g: 0.604 b: 0.161) * 0.85`.
			s _ `Color black` ].

	size _ currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonExpandPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/2/2021 14:35:41'!
drawExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	self fillColor: `Color gray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: `0 @ -4`;
			rel_LineTo: `5@10`;
			rel_LineTo: `5 @ -10`;
			rel_LineTo: `-10 @ 0` ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 7/29/2021 12:00:21'!
drawHandLarge

	self strokeWidth: 2.2 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			`#[
				0.0    10.97    35.70
				1.0    -3.17    26.78
				2.0    -7.18    20.17    -8.01    23.69
				2.0    -3.16    18.50    -6.55    18.00
				1.0    4.84    20.92
				1.0    5.97    23.63
				1.0    -2.81    2.44
				2.0    -1.35    -3.26    -4.61    -1.91
				2.0    3.71    -0.26    1.91    -4.61
				1.0    9.56    13.87
				1.0    6.41    6.26
				1.0    9.67    4.91
				2.0    13.20    7.27    11.85    4.01
				1.0    15.45    12.70
				1.0    12.75    6.18
				1.0    16.01    4.83
				2.0    19.53    7.19    18.18    3.93
				1.0    21.33    11.54
				1.0    19.08    6.10
				1.0    22.34    4.75
				2.0    25.87    7.11    24.52    3.85
				1.0    27.67    11.46
				1.0    30.82    19.07
				2.0    30.79    34.36    33.52    25.59
				1.0    17.74    39.77
				2.0    11.42    36.78    14.48    41.12
				1.0    10.97    35.70
			] asFloat32Array` ].
! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 7/29/2021 12:01:08'!
drawHandSmall

	self strokeWidth: 1.1 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			`#[
				0.0    5.33    17.34
				1.0    -1.54    13.01
				2.0    -3.49    9.79    -3.89    11.51
				2.0    -1.54    8.99    -3.18    8.74
				1.0    2.35    10.16
				1.0    2.90    11.48
				1.0    -1.37    1.18
				2.0    -0.66    -1.58    -2.24    -0.93
				2.0    1.80    -0.13    0.93    -2.24
				1.0    4.65    6.74
				1.0    3.11    3.04
				1.0    4.70    2.38
				2.0    6.41    3.53    5.75    1.95
				1.0    7.50    6.17
				1.0    6.19    3.00
				1.0    7.78    2.35
				2.0    9.49    3.49    8.83    1.91
				1.0    10.36    5.60
				1.0    9.27    2.96
				1.0    10.85    2.31
				2.0    12.56    3.45    11.91    1.87
				1.0    13.44    5.57
				1.0    14.97    9.26
				2.0    14.95    16.69    16.28    12.43
				1.0    8.62    19.31
				2.0    5.55    17.87    7.04    19.97
				1.0    5.33    17.34
			] asFloat32Array` ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/26/2021 12:25:07'!
drawLeftIcon
	| c size |
	c _ currentMorph isPressed
		ifFalse: [`Color darkGray`]
		ifTrue: [`Color veryDarkGray`].
	size _ currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollLeftPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/5/2021 14:42:50'!
drawMenuIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c _ `Color r: 0.212 g: 0.439 b: 0.784`.
			s _ `Color veryDarkGray` ]
		ifTrue: [
			c _ `(Color r: 0.212 g: 0.439 b: 0.784) * 0.85`.
			s _ `Color black` ].

	size _ currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self fillColor: s do: [
		self pathSequence: (self class windowButtonMenuPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/2/2021 14:52:28'!
drawNotExpandedAt: aPoint
	"For Tree View (IndentingListItemMorph)"

	self fillColor: `Color gray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: `2@ -5`;
			rel_LineTo: `10@5`;
			rel_LineTo: `-10 @ 5`;
			rel_LineTo: `0 @ -10` ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 9/5/2021 14:47:36'!
drawPushPinIcon
	"For Menu stay-up button"
	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c _ `(Color r: 0.996 g: 0.329 b: 0.329)`.
			s _ `Color veryDarkGray` ]
		ifTrue: [
			c _ `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s _ `Color black` ].
	size _ currentMorph morphWidth.
	self strokeWidth: size/30.0 color: s fillColor: c do: [
		self pathSequence: (self class pushPinIconData: size) ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/26/2021 12:25:16'!
drawRightIcon
	| c size |
	c _ currentMorph isPressed
		ifFalse: [`Color darkGray`]
		ifTrue: [`Color veryDarkGray`].
	size _ currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollRightPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 8/26/2021 12:25:30'!
drawUpIcon
	| c size |
	c _ currentMorph isPressed
		ifFalse: [`Color darkGray`]
		ifTrue: [`Color veryDarkGray`].
	size _ currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollUpPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/12/2021 14:41:01'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight
	"
	BitBltCanvas onForm: Display ::  windowFrame: (10@10 extent: 200@100) color: Color red radius: 10 border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	VectorCanvas onForm: Display :: drawRoundedFrame: (210@10 extent: 200@100) border: 5 color: Color red insideColor: Color green labelHeight: 25.
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

	| d lh r |
	d _ borderWidth*0.5.
	lh _ labelHeight + borderWidth.
	r _ Theme current roundedWindowRadius.

	self strokeWidth: borderWidth color: borderColor fillColor: borderColor do: [
		self 
			roundTopRectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+d maxY: aRectangle top+lh-d rx: r ry: r ].
	self strokeWidth: borderWidth color: borderColor fillColor: insideColor do: [
		self
			roundBottomRectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+lh-d maxY: aRectangle bottom-d rx: r ry: r ]! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/11/2021 09:28:51'!
roundBottomLeftCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor

	| d1 d2 r corner p1 p2 p3 p4 |
	d1 _ borderWidth*0.5.
	d2 _ borderWidth*0.5.
	r _ Theme current roundedWindowRadius.
	corner _ (rectOriginX + d1)@(rectOriginY + l - d2).
	p1 _ corner - (0@l).
	p2 _ corner - (0@r).
	p3 _ corner + (r@0).
	p4 _ corner + (l@0).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/11/2021 09:28:56'!
roundBottomRightCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor

	| d2 r corner p1 p2 p3 p4 |
	d2 _ borderWidth*0.5.
	r _ Theme current roundedWindowRadius.
	corner _ (rectOriginX + l - d2)@(rectOriginY + l - d2).
	p1 _ corner - (l@0).
	p2 _ corner - (r@0).
	p3 _ corner - (0@r).
	p4 _ corner - (0@l).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/11/2021 18:38:11'!
roundEdge: aRectangle border: borderWidth color: borderColor

	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: aRectangle origin;
			lineTo: aRectangle corner ].! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/11/2021 09:29:02'!
roundTopLeftCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor

	| d1 r corner p1 p2 p3 p4 |
	d1 _ borderWidth*0.5.
	r _ Theme current roundedWindowRadius.
	corner _ rectOriginX@rectOriginY + d1.
	p1 _ corner + (0@l).
	p2 _ corner + (0@r).
	p3 _ corner + (r@0).
	p4 _ corner + (l@0).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].! !

!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 10/11/2021 09:29:09'!
roundTopRightCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor

	| d1 d2 r corner p1 p2 p3 p4 |
	d1 _ borderWidth*0.5.
	d2 _ borderWidth*0.5.
	r _ Theme current roundedWindowRadius.
	corner _ (rectOriginX + l - d2)@(rectOriginY + d1).
	p1 _ corner - (l@0).
	p2 _ corner - (r@0).
	p3 _ corner + (0@r).
	p4 _ corner + (0@l).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 6/13/2021 18:06:10'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: morphStrokeWidth borderColor: strokeColor fillColor: fillColor

	| radiusPoint |
	radiusPoint _ radiusPointOrNumber asPoint.
	self strokeWidth: morphStrokeWidth color: strokeColor fillColor: fillColor do: [
		self ellipseCenter: center radius: radiusPoint ]! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/12/2021 14:34:35'!
fillRectangle: aRectangle color: aColor

	self fillColor: aColor do: [
		self
			rectangleMinX: aRectangle left maxX: aRectangle right
			minY: aRectangle top maxY: aRectangle bottom ].! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 11/17/2020 09:37:10'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	aSymbol = #simple ifTrue: [
		^ self frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: baseColorForBorder ].
	
	super fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/12/2021 14:35:18'!
frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: borderColor
	| d |
	d _ borderWidth * 0.5.
	self strokeWidth: borderWidth color: borderColor fillColor: aColor do: [
		self
			rectangleMinX: aRectangle left+d maxX: aRectangle right-d
			minY: aRectangle top+d maxY: aRectangle bottom-d ]! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/12/2021 14:33:48'!
frameRectangle: aRectangle borderWidth: borderWidth color: borderColor
	| d |
	d _ borderWidth*0.5.
	self strokeWidth: borderWidth color: borderColor do: [
		self
			rectangleMinX: aRectangle left+d maxX: aRectangle right-d
			minY: aRectangle top+d maxY: aRectangle bottom-d ]! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/12/2021 14:35:41'!
frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: morphStrokeWidth
	"
	(VectorCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| d |
	d _ morphStrokeWidth*0.5.
	self strokeWidth: morphStrokeWidth color: brColor do: [
		self moveTo: (aRectangle left + d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle right - d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle right - d) @ (aRectangle top + d) ].

	self strokeWidth: morphStrokeWidth color: tlColor do: [
		self moveTo: (aRectangle left + d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle left + d) @ (aRectangle top + d).
		self lineTo: (aRectangle right - d) @ (aRectangle top + d) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 5/10/2021 11:25:57'!
line: pt1 to: pt2 width: morphStrokeWidth color: aStrokeColor

	self strokeWidth: morphStrokeWidth color: aStrokeColor do: [
		self moveTo: pt1.
		self lineTo: pt2 ]! !

!AbstractVectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/12/2021 14:36:14'!
roundRect: aRectangle color: aColor radius: r

	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left maxX: aRectangle right
			minY: aRectangle top maxY: aRectangle bottom rx: r ry: r ]! !

!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:44:10'!
drawHand

	self drawsHandLarge
		ifTrue: [self drawHandLarge]
		ifFalse: [self drawHandSmall].! !

!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:44:58'!
drawsHandLarge

	^true! !

!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 10:31:08'!
into: aMorph
	super into: aMorph.
	engine
		currentMorph: currentMorph;
		resetMorphBounds;
		geometryTransformation: currentTransformation.! !

!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 10:30:46'!
outOfMorph
	super outOfMorph.
	engine
		currentMorph: currentMorph;
		resetMorphBounds;
		geometryTransformation: currentTransformation.! !

!AbstractVectorCanvas methodsFor: 'private' stamp: 'jmv 12/21/2020 17:36:45'!
fontToUse: fontOrNil
	"Answer a suitable font, aFont if possible."

	^fontOrNil ifNil: [ FontFamily defaultOrTrueTypeFamilyAndPointSize ].! !

!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 9/1/2021 15:36:04'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used only as an optimization of the area to be redrawn."

	| boundingRect |
	"A good optimization in many cases.
	But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
	for example) this is essential."
	currentMorph knowsOwnLocalBounds ifTrue: [
		^super boundingRectOfCurrentMorphAfterDraw ].

	"Use the bounds bound by the engine during #drawOn.
	This method must therefore be called after calling #drawOn:."
	boundingRect _ engine morphBoundsAfterDraw.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:19'!
experiment1
	"
	Not perfect, but can be compared with
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	BTW, this is so much faster...
	VectorCanvas experiment1
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
canvas _ self onForm: Display.
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
"	canvas _ self on: f.
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
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
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

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:21'!
experiment2
	"
	VectorCanvas experiment2
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
canvas _ self onForm: Display.
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
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
! !

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:23'!
experiment3
	"
	VectorCanvas experiment3
	"
	| canvas w r |
	canvas _ self onForm: Display.
	w _ 10.
	r _ 15.
	100 timesRepeat: [
		canvas strokeWidth: w color: Color black do: [
			canvas circleCenter: 300 @ 300 radius: r ].
		r _ r + w.
		w _ w * 0.8.
		r _ r + w.

	Display forceToScreen.
	].
Display forceToScreen! !

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:25'!
experiment4
	"
	VectorCanvas experiment4
	"
	| canvas w r |
	canvas _ self onForm: Display.
	w _ 10.
	r _ 15.
	1 to: 100 do: [ :i |
		r _ r + w.
		w _ 20.0 / i.
		r _ r + w.
		canvas strokeWidth: w color: Color black do: [
			canvas circleCenter: 300 @ 300 radius: r ].
	Display forceToScreen.
	].
Display forceToScreen! !

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:48:02'!
siemensStar
	"
	VectorCanvas siemensStar display
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
		canvas geometryTransformation: location.
		canvas fillColor: foreColor do: [
			canvas
				moveTo: 0 @ 0;
				lineTo: 1 @ 0;
				lineTo: angle cos @ angle sin;
				lineTo: 0 @ 0.
		].
	].
	^form! !

!AbstractVectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 8/26/2021 16:22:54'!
guiSizePreferenceChanged
	"Some preference related to size of gui elements may have changed."
	IconPathData removeAll.! !

!AbstractVectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 10/8/2021 09:45:19'!
initialize
	"
	AbstractVectorCanvas initialize
	"
	IconPathData _ Dictionary new.! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:15:36'!
pushPinIconData: aSize
	| scale |
	^IconPathData at: #pushPinIcon ifAbsentPut: [
		scale _ aSize / 19.0.
		self scalePathData: `#[
			0 0 21
			1 10 13
			1 15 18
			1 14 13
			1 21 6
			1 15 0
			1 8 7
			1 3 6
			1 8 11
			1 0 21
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:23:26'!
scrollDownPathData: aSize
	| scale |
	^IconPathData at: #scrollDown ifAbsentPut: [
		scale _ aSize / 18.0.
		self scalePathData: `#[
			0 4 7
			1 9 13
			1 14 7
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:23:37'!
scrollLeftPathData: aSize
	| scale |
	^IconPathData at: #scrollLeft ifAbsentPut: [
		scale _ aSize / 18.0.
		self scalePathData: `#[
			0 12 4
			1 6 9
			1 12 14
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:23:43'!
scrollRightPathData: aSize
	| scale |
	^IconPathData at: #scrollRight ifAbsentPut: [
		scale _ aSize / 18.0.
		self scalePathData: `#[
			0 7 4
			1 13 9
			1 7 14
			] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:23:48'!
scrollUpPathData: aSize
	| scale |
	^IconPathData at: #scrollUp ifAbsentPut: [
		scale _ aSize / 18.0.
		self scalePathData: `#[
			0 4 12
			1 9 6
			1 14 12
			] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:15:31'!
windowButtonClosePathData: aSize
	| scale |
	^IconPathData at: #windowButtonClose ifAbsentPut: [
		scale _ aSize / 22.0.
		self scalePathData: `#[
			0 6 14
			1 14 6
			0 6 6
			1 14 14
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:15:27'!
windowButtonCollapsePathData: aSize
	| scale |
	^IconPathData at: #windowButtonCollapse ifAbsentPut: [
		scale _ aSize / 22.0.
		self scalePathData: `#[
			0 5 10
			1 15 10
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:15:21'!
windowButtonExpandPathData: aSize
	| scale |
	^IconPathData at: #windowButtonExpand ifAbsentPut: [
		scale _ aSize / 22.0.
		self scalePathData: `#[
			0 5 10
			1 15 10
			0 10 5
			1 10 15
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 8/26/2021 12:15:16'!
windowButtonMenuPathData: aSize
	| scale |
	^IconPathData at: #windowButtonMenu ifAbsentPut: [
		scale _ aSize / 22.0.
		self scalePathData: `#[
			0 4 8
			1 16 8
			1 10 16
			1 4 8
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 9/5/2021 14:52:27'!
windowButtonPathData: aSize
	| scale |
	^IconPathData at: #windowButton ifAbsentPut: [
		scale _ aSize / 22.0.
		self scalePathData: `#[
			0.0 0 10
			2    10 20 0.5 19.5
			2    20 10 19.5 19.5
			2    10 0 19.5 0.5
			2    0 10 0.5 0.5
		] asFloat32Array` scale: scale ].! !

!AbstractVectorCanvas class methodsFor: 'private' stamp: 'jmv 8/26/2021 11:17:52'!
scalePathData: aFloat32Array scale: scale
	"aFloatArray is a #pathSequence: argument. It encodes a sequence of path commands in a Float32Array.
	See #pathSequence:"

	| kind scaledPathData | 
	scaledPathData _ Float32Array streamContents: [ :scaledStrm | | unscaledStrm |
		unscaledStrm _ aFloat32Array readStream.
		[ unscaledStrm atEnd ] whileFalse: [
			kind _ unscaledStrm next.
			scaledStrm nextPut: kind.
			(kind asInteger *2 max: 2) timesRepeat: [
				scaledStrm nextPut: unscaledStrm next * scale ]]
		].
	^scaledPathData.! !

!BoundsFinderCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 8/12/2021 15:52:34'!
image: srcForm at: aPoint

	"Compute bounds"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black`.! !

!BoundsFinderCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 8/12/2021 15:53:59'!
imageUnrotated: srcForm at: aPoint

	"Compute bounds"
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color black`.! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:56:02'!
clipCurrentMorph: aBoolean! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/4/2021 16:55:34'!
displayBoundsForHand: aHand
	"See #drawHand."

	^self drawsHandLarge
		ifTrue: [aHand morphPosition asIntegerPoint-`9@5` extent: `45@48`]
		ifFalse: [aHand morphPosition asIntegerPoint-`4@3` extent: `21@24`].! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:43:30'!
drawCurrentAndSubmorphs
	
	currentMorph visible ifTrue: [

		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/23/2021 14:19:05'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/23/2021 11:21:40'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	
	aMorph visible ifTrue: [
		(aMorph isRedrawNeeded or: [aMorph isSubmorphRedrawNeeded]) ifTrue: [
			self into: aMorph.
			self fullAddCurrentMorphTo: aDamageRecorder.
			self outOfMorph
		]]! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:43:37'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	currentMorph knowsOwnLocalBounds
		ifTrue: [self displayBoundsSetOnCurrentMorph]
		ifFalse: [
			currentMorph isKnownFailing
				ifTrue: [
					self drawCurrentAsError ]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph drawOn: self.
					self displayBoundsSetOnCurrentMorph.
					(currentMorph postDrawOn: self) ifTrue: [
						self displayBoundsUpdateOnCurrentMorph ].
					(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphsReally not]) ifTrue: [
						currentMorph submorphsDo: [ :m |
							self fullUpdateProtrudingBounds: m ]]]].! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:11:00'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'len 1/31/2022 13:56:05'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:44:10'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	currentMorph knowsOwnLocalBounds
		ifTrue: [self displayBoundsSetOnCurrentMorph]
		ifFalse: [
			currentMorph isKnownFailing
				ifTrue: [
					self drawCurrentAsError ]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph drawOn: self.
					self displayBoundsSetOnCurrentMorph.
					(currentMorph postDrawOn: self) ifTrue: [
						self displayBoundsUpdateOnCurrentMorph ]].]! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/25/2021 16:40:23'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph basicDisplayBounds ifNil: [
					self fullUpdateProtrudingBounds: morph ]].
			self outOfMorph ]].! !

!BoundsFinderCanvas methodsFor: 'testing' stamp: 'jmv 8/12/2021 14:47:56'!
isBoundsFinderCanvas
	^true! !

!BoundsFinderCanvas methodsFor: 'initialization' stamp: 'jmv 8/11/2021 14:30:58'!
initialize
	engine _ VectorEngineBoundsFinder new.
	engine canvas: self.
	clippingMorphDisplayBounds _ nil.! !

!BoundsFinderCanvas methodsFor: 'initialization' stamp: 'jmv 8/18/2021 11:42:44'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.! !

!BoundsFinderCanvas methodsFor: 'initialization' stamp: 'jmv 8/17/2021 14:43:31'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	engine _ VectorEngineBoundsFinder new.
	clippingMorphDisplayBounds _ nil.! !

!BoundsFinderCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:25:05'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor! !

!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 10:34:48'!
boundsFinderCanvas
	^boundsFinderCanvas! !

!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/2/2022 16:45:43'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	| p1 answer |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	currentTransformation isPureTranslation ifTrue: [
		(Preferences at: #cacheTrueTypeGlyphs) ifTrue: [
			p1 _ currentTransformation transform: aPoint roundedHAFZ.
			p1 _ p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 _ p1 roundedHAFZ.
			answer _ aString displayOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/29/2021 10:54:55'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need tow of them!!"
	currentMorph == world ifTrue: [
		p _ (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: 0; sourceY: 0;
			destX: p x destY: p y width: srcForm width height: srcForm height;
			copyBits.
		bits _ form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p _ (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: 0; sourceY: 0;
				destX: p x destY: p y width: srcForm width height: srcForm height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize _ 4.
			srcBox _ srcForm boundingBox.
			dstBox _ (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad _ dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox _ (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb _ WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/29/2021 10:54:49'!
imageUnrotated: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	Scale and position like #image:at:, but don't rotate srcForm:
	Display it in its natural orientation."

	| cellSize srcBox dstBox srcQuad bb |

	"If no rotation or scaling, BitBlt is enough. But there's no need to repeat the code:
	If no rotation, 'unrotated' is the same as 'defaultlyRotated'."
	currentTransformation isPureTranslation ifTrue: [
		^self image: srcForm at: aPoint ].

	"Compute bounds, set morphIds for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.

	cellSize _ 4.
	srcBox _ srcForm boundingBox.
	srcQuad _ srcBox corners.
	dstBox _ Rectangle
		center: (currentTransformation transform: srcBox center + aPoint) roundedHAFZ
		extent: (srcBox extent * currentTransformation scale) roundedHAFZ.
	bb _ WarpBlt toForm: form.
	bb
		sourceForm: srcForm; combinationRule: Form blend;
		colorMap: (srcForm colormapIfNeededFor: form);
		clipRect: clipRect;
		cellSize: cellSize;
		copyQuad: srcQuad toRect: dstBox.
! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 8/12/2021 15:55:58'!
clipCurrentMorph: aBoolean
	engine clipCurrentMorph: aBoolean! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 9/1/2021 11:25:09'!
contourOfCurrentMorphAfterDrawInto: anArray into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or use a new array if not."

	engine morphContourAfterDrawInto: anArray into: aBlock.! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 09:33:25'!
contourOfCurrentMorphAfterDrawUpdate: anArray oldTop: oldTop oldBottom: oldBottom into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or a new array if not."

	anArray
		ifNil: [ engine morphContourAfterDrawInto: nil into: aBlock ]
		ifNotNil: [
			engine morphContourAfterDrawUpdate: anArray 
				oldTop: oldTop oldBottom: oldBottom into: aBlock ].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:18'!
contourSetOnCurrentMorph
	"Set contour. To be called immediately after drawing currentMorph."

	currentMorph canvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawInto: (currentMorph valueOfProperty: #contour) 
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]
				ifFalse: [
					self contourClearOnCurrentMorph ]]].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 12:08:43'!
contourUpdateOnAllOwners
	"Update contour on all owners of currentMorph.
	To also include the effect of #postDrawOn: and submorphs."

	currentMorph canvas ifNotNil: [ :c |
		c canvasToUse = self ifTrue: [
			1 to: cti-1 do: [ :i |
				(drawingMorphStack at: i) ifNotNil: [ :m |
					m wantsContour ifTrue: [
						self
							contourOfCurrentMorphAfterDrawUpdate: (m valueOfProperty: #contour)
							oldTop: (m valueOfProperty: #contourY0)
							oldBottom: (m valueOfProperty: #contourY1)
							into: [ :contourArray :contourY0 :contourY1 |
								m setProperty: #contour toValue: contourArray.
								m setProperty: #contourY0 toValue: contourY0.
								m setProperty: #contourY1 toValue: contourY1 ]]]]]].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 12:07:51'!
contourUpdateOnCurrentMorph
	"Update contour. To also include the effect of #postDrawOn: and submorphs."

	currentMorph canvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawUpdate: (currentMorph valueOfProperty: #contour)
						oldTop: (currentMorph valueOfProperty: #contourY0)
						oldBottom: (currentMorph valueOfProperty: #contourY1)
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]]].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 12:08:57'!
drawCurrentAndSubmorphs
	
	currentMorph visible ifTrue: [

		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourSetOnCurrentMorph.
		self contourUpdateOnAllOwners.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph.
			self contourUpdateOnCurrentMorph.
			self contourUpdateOnAllOwners ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:49:16'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBoundsForError.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	r _ r insetBy: w.
	self line: r topLeft to: r bottomRight width: w color: `Color yellow`.
	self line: r topRight to: r bottomLeft width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourSetOnCurrentMorph.! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 9/20/2021 12:07:54'!
morphIdAt: aPoint
	^engine morphIdAt: aPoint.! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 15:41:49'!
restorePatch: savedPatch bounds: savedBounds
	"Argument must previously be obtained by calling #savePatch:bounds:"

	| bb bits |
	bb _ BitBlt toForm: form.
	bb
		combinationRule: Form over;
		sourceX: 0; sourceY: 0;
		destX: savedBounds origin x destY: savedBounds origin y
			width: savedBounds width height: savedBounds height.
	bb sourceForm: savedPatch first;
		copyBits.
	bits _ form bits.
	[
		form bits: engine morphIds.
		bb sourceForm: savedPatch second;
			copyBits.
	] ensure: [ form bits: bits ].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 16:37:55'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:bounds:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| patchForm prevExtent extent savedIdsPatch bb answer bits |
	answer _ prevSavedPatch.
	(answer is: #Array) ifFalse: [ answer _ Array new: 2 ].

	prevExtent _ 0@0.
	((answer first is: #Form) and: [
		prevExtent _ answer first extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				patchForm _ answer first.
				patchForm offset: 0@0.
				savedIdsPatch _ answer second.
				savedIdsPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent _ aRectangle extent max: prevExtent.
				extent _ (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				patchForm _ Form extent: extent depth: form depth.
				savedIdsPatch _ Form extent: extent depth: 32.
				answer at: 1 put: patchForm.
				answer at: 2 put: savedIdsPatch ].

	bb _ BitBlt new.
	bb sourceForm: form;
		combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height.
	bb setDestForm: patchForm;
		copyBits.
	bits _ form bits.
	[
		form bits: engine morphIds.
		bb setDestForm: savedIdsPatch;
			copyBits.
	] ensure: [ form bits: bits ].

	^answer! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 8/18/2021 11:42:58'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.

	boundsFinderCanvas _ BoundsFinderCanvas new initializeWithTranslation: aPoint.
	"So bounds found are exactly the same as our engine would find."
	boundsFinderCanvas engine antiAliasingWidth: engine antiAliasingWidth.
	boundsFinderCanvas engine subPixelDelta: engine subPixelDelta.! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 9/7/2021 09:53:54'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	super resetCanvas.
	boundsFinderCanvas resetCanvas.
	engine clearArrays.! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 1/17/2022 12:32:19'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setCanvas
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form _ aForm.
	engine _ VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	auxBitBltEngine _ BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 8/12/2021 15:26:50'!
world: aWorldMorph
	super world: aWorldMorph.
	boundsFinderCanvas world: aWorldMorph! !

!VectorCanvas methodsFor: 'testing' stamp: 'jmv 10/27/2021 16:10:18'!
canDoVectorGraphics
	^true! !

!VectorCanvas methodsFor: 'testing' stamp: 'jmv 8/27/2021 16:04:50'!
usesVectorEnginePlugin
	^engine usesVectorEnginePlugin! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 8/12/2021 16:05:57'!
pvtDrawButtonFaceForm: aForm
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	| color |
	color _ currentMorph isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			currentMorph mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].
	self
		image: aForm
		at: (currentMorph morphExtent - aForm extent //2).! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 10/7/2021 17:36:57'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle.
	auxBitBltEngine _ BitBltCanvasEngine toForm: form.
	auxBitBltEngine clipRect: aRectangle.
	boundsFinderCanvas ifNotNil: [
		boundsFinderCanvas setClipRect: aRectangle ].! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:23:38'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(VectorCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color yellow
		borderWidth: 20
		color: Color green.
	Display forceToScreen
	"
	auxBitBltEngine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:23:56'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(VectorCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	auxBitBltEngine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:24:14'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(VectorCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	auxBitBltEngine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!VectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 2/21/2022 12:17:18'!
initialize
	| family folder |
	(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
		Feature require: 'VectorEngineInSmalltalk' ].
	UISupervisor whenUIinSafeState: [
		(FontFamily familyNamed: FontFamily defaultFamilyName) isTrueTypeFontFamily ifFalse: [
			folder _ DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts' / 'DejaVu' / 'DejaVuSans'.
			family _ FontFamily familyNamed: (TrueTypeFontFamily read: folder) anyOne.
			PreferenceSet setDefaultFont: family familyName.
			FontFamily enableTrueTypeFontsOnly ].
		(VectorEngineWithPlugin isPluginAvailable
			ifTrue: [ MorphicCanvas activeSubclass: VectorCanvas ]
			ifFalse: [
				'VectorEnginePlugin (VM plugin) is not available. HybridCanvas will be activated.' print.
				MorphicCanvas activeSubclass: HybridCanvas ]) ].! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 3/22/2021 14:42:49'!
antiAliasingWidth

	^antiAliasingWidth! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 5/15/2020 09:27:38'!
antiAliasingWidth: aNumber

	antiAliasingWidth _ aNumber.! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 8/8/2021 23:22:47'!
clipCurrentMorph: aBoolean
	"To be redefined as needed"! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 5/12/2021 11:07:01'!
geometryTransformation: aGeometryTransformation

	geometryTransformation _ aGeometryTransformation.! !

!VectorEngine methodsFor: 'accessing' stamp: 'jmv 7/5/2021 10:09:41'!
morphBoundsAfterDraw
	"In targetForm coordinates.
	Doesn't account for any clipping rect (neither clip rect parameter to bound drawn area, nor possible clipping to owner shape or bounds).
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph."

	^morphBoundsLeft @ morphBoundsTop corner: morphBoundsRight @ morphBoundsBottom + 1! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:06:09'!
arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine arcCenter: 100@100 radius: 80@40 rotationAngle: -0.2 start: 0 sweep: Float twoPi ].
Display forceToScreen.
"
	| angle thetaCos thetaSin xp yp x0 y0 ttheta tthetaCos tthetaSin |
	thetaCos _ theta cos.
	thetaSin _ theta sin.
	angle _ startAngle.
	xp _ angle cos * radiusPoint x.
	yp _ angle sin * radiusPoint y.
	x0 _ thetaCos * xp - (thetaSin * yp) + aPoint x.
	y0 _ thetaSin * xp + (thetaCos * yp) + aPoint y.
	(sweepAngle ~= Float twoPi and: [firstX isNaN not])
		ifTrue: [
			"If an arc (not whole circle/ellipse), join with a line, to ensure closed path.
			For example, for rounded rectangles."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX _ x0.
			currentY _ y0 ].

	ttheta _ theta + geometryTransformation radians.
	tthetaCos _ ttheta cos.
	tthetaSin _ ttheta sin.
	geometryTransformation doesMirror
		ifTrue: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle+sweepAngle sweep: sweepAngle negated rotationCos: tthetaCos rotationSin: tthetaSin ]
		ifFalse: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin ].

	angle _ startAngle + sweepAngle.
	xp _ angle cos * radiusPoint x.
	yp _ angle sin * radiusPoint y.
	currentX _ thetaCos * xp - (thetaSin * yp) + aPoint x.
	currentY _ thetaSin * xp + (thetaCos * yp) + aPoint y.

	sweepAngle = Float twoPi
		ifTrue: [
			"If we were asked for a full circle/ellipse, ensure we end exactly at the same point we started,
			even if there is some rounding error."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX _ x0.
			currentY _ y0 ].! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:06:21'!
arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.2 position: 200@100);
	strokeWidth: 1.0 color: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"
	| x1 y1 x2 y2 radiusPoint rx ry cosPhi sinPhi x12hd y12hd x1p y1p rxSq rySq x1pSq y1pSq bigSqR bigSqRNumerator bigSqRDenominator sign cxp cyp cx cy theta1 theta2 deltaTheta scaleSq scale newRx newRy |
	x1 _ currentX.
	y1 _ currentY.
	x2 _ aPoint x.
	y2 _ aPoint y.
	radiusPoint _ radiusPointOrNumber asPoint.
	rx _ radiusPoint x.
	ry _ radiusPoint y.
	"Handle out of range parameters as requested by the w3 implementation notes"
	(x2 = x1 and: [ y2 = y1 ]) ifTrue: [ ^self ].
	(rx = 0.0 or: [ ry = 0.0 ]) ifTrue: [
		self lineFromX: currentX y: currentY toX: x2 y: y2.
		currentX _ x2.
		currentY _ y2.
		^self ].
	rx < 0 ifTrue: [
		^self arcTo: aPoint radius: 0.0 -rx @ ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].
	ry < 0 ifTrue: [
		^self arcTo: aPoint radius: rx @ (0.0-ry) angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles ].

	cosPhi _ phi cos.
	sinPhi _ phi sin.
	"step 1 in `B.2. Elliptical arc parameter conversion` at http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes"
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
		^self arcTo: aPoint radius: newRx @ newRy angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAngles
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

	"step 4"
	theta1 _ (((x1p - cxp) / rx) @ ((y1p - cyp) / ry)) theta.
	theta2 _ (((0 - x1p - cxp) / rx) @ (( 0 -y1p - cyp) / ry)) theta.
	deltaTheta _ theta2 - theta1.
	useIncreasingAngles = (deltaTheta > 0) ifFalse: [
		deltaTheta > 0
			ifTrue: [ deltaTheta _ deltaTheta - Float pi - Float pi ]
			ifFalse: [ deltaTheta _ deltaTheta + Float pi + Float pi ]
		].

	self arcCenter: cx@cy radius: rx@ry rotationAngle: phi start: theta1 sweep: deltaTheta! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:06:30'!
circleCenter: aPoint radius: mr
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			circleCenter: 200 @ 150 radius: 80].
Display forceToScreen
"

	self arcCenter: aPoint radius: mr@mr rotationAngle: 0.0 start: 0.0 sweep: Float twoPi.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:06:55'!
cubicBezierTo: toPoint control1: controlPoint1 control2: controlPoint2
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@50.
		engine
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 150@400
			control1: 500@400
			control2: -150@300 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 150@400
			control1: 600@400
			control2: -150@300 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 400@150
			control1: 400@4600
			control2: 300 @ -15].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 120@1000
			control1: 140@200
			control2: 130@1400].
Display forceToScreen
	"

	self cubicBezierFromX: currentX y: currentY toX: toPoint x y: toPoint y control1X: controlPoint1 x y: controlPoint1 y control2X: controlPoint2 x y: controlPoint2 y.
	currentX _ toPoint x.
	currentY _ toPoint y.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:07:24'!
ellipseCenter: aPoint radius: radiusPoint
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50].
Display forceToScreen
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: 0.0 start: 0.0 sweep: Float twoPi.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:07:33'!
ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
Display forceToScreen
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: 0.0 sweep: Float twoPi.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:07:43'!
ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
	"Currently draws a line from pathCurrentX@pathCurrentY to start (if startAngle ~= 0.0 or sweepAngle ~= Float twoPi).
	This could be useful for proper fills. But might not be what is desired in some cases.
	Implement desired behavior when clear."
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@350 radius: 100@50 rotationAngle: 0.5 start: 0.1 sweep: 6 ].
Display forceToScreen.
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:07:53'!
lineTo: aPoint
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 300@150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 25.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 500@200 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine moveTo: 30@115.
		engine lineTo: 45@190.
		engine lineTo: 180@130 ].
Display forceToScreen
"
	self lineFromX: currentX y: currentY toX: aPoint x y: aPoint y.
	currentX _ aPoint x.
	currentY _ aPoint y.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:08:03'!
lineToFirstPoint
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineToFirstPoint ].
Display forceToScreen
"

	self lineFromX: currentX y: currentY toX: firstX y: firstY.
	currentX _ firstX.
	currentY _ firstY.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:08:08'!
moveTo: aPoint
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 200@150;
			moveTo: 210@150;
			lineTo: 300@200 ].
Display forceToScreen
"
	"A discontinuity in the trajectory"
	"We are starting a new trajectory, or at a discontinuity in trajectory.
	Eventually #closeTrajectory should draw a line to here"

	"Next point will be first in a new trajectory fragment"
	self newTrajectoryFragment.
	firstX _ aPoint x.
	firstY _ aPoint y.
	currentX _ firstX.
	currentY _ firstY.! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 1/17/2022 13:08:34'!
quadraticBezierTo: toPoint control: controlPoint
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			quadraticBezierTo: 120@130 control: 120@110 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			quadraticBezierTo: 120@130 control: 1300@110 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@270;
			quadraticBezierTo: 200@130 control: 110@600 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@550;
			quadraticBezierTo: 200@130 control: 110@600 ].
Display forceToScreen
	"
	self quadraticBezierFromX: currentX y: currentY toX: toPoint x y: toPoint y controlX: controlPoint x y: controlPoint y.
	currentX _ toPoint x.
	currentY _ toPoint y.! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 5/13/2021 14:31:14'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers x y cx cy contourStart |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForIso8859s15.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByIso8859s15.
	self fillColor: aColor do: [
		nextGlyphX _ aPoint x.
		nextGlyphY _ aPoint y.
		startIndex to: stopIndex do: [ :charIndex | | iso8859s15 i |
			iso8859s15 _ (aString at: charIndex) iso8859s15Code.
			aBoolean ifTrue: [
				iso8859s15 = 95 ifTrue: [ iso8859s15 _ 28 ].												"If underscore, use left arrow"
				iso8859s15 = 94 ifTrue: [ iso8859s15 _ 30 ].												"If caret, use up arrow"
				].
			i _ contourDataIndexes at: iso8859s15 + 1.
			advanceWidth _ contourData at: i. i _ i + 1.
			boundsLeft _ contourData at: i. i _ i + 1.
			boundsRight _ contourData at: i. i _ i + 1.
			boundsTop _ contourData at: i. i _ i + 1.
			boundsBottom _ contourData at: i. i _ i + 1.
			numContours _ (contourData at: i) asInteger. i _ i + 1.
			numContours timesRepeat: [
				numBeziers _ (contourData at: i) asInteger. i _ i + 1.
				ttMoveToX _ contourData at: i. i _ i + 1.
				ttMoveToY _ contourData at: i. i _ i + 1.
				x _ ttMoveToX * sx + nextGlyphX.
				y _ ttMoveToY * sy + nextGlyphY.
				contourStart _ x@y.
				self moveTo: contourStart.
				numBeziers timesRepeat: [
					ttEndX _ contourData at: i. i _ i + 1.
					ttEndY _ contourData at: i. i _ i + 1.
					ttControlX _ contourData at: i. i _ i + 1.
					ttControlY _ contourData at: i. i _ i + 1.
					cx _ ttControlX * sx + x.
					cy _ ttControlY * sy + y.
					x _ ttEndX * sx + x.
					y _ ttEndY * sy + y.
					self quadraticBezierTo: x@y control: cx@cy
					].
				self lineTo: contourStart.
				].
			nextGlyphX _ nextGlyphX + (advanceWidth * sx).
		]
	].
	^ nextGlyphX@nextGlyphY! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 5/13/2021 14:31:23'!
displayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers x y cx cy contourStart |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	self fillColor: aColor do: [
		nextGlyphX _ aPoint x.
		nextGlyphY _ aPoint y.
		startIndex to: stopIndex do: [ :utf32Index | | utf32 utf8Byte1 uft8Byte2 utf8Byte3 utf8Byte4 i |
			utf32 _ aWordArray at: utf32Index.
			aBoolean ifTrue: [
				utf32 = 95 ifTrue: [ utf32 _ 8592 ].												"If underscore, use left arrow"
				utf32 = 94 ifTrue: [ utf32 _ 8593 ].												"If caret, use up arrow"
				].
			utf32 <= 16r7F
				ifTrue: [
					utf8Byte1 _ utf32.
					i _ contourDataIndexes at: utf8Byte1 + 1.
					]
				ifFalse: [ utf32 <= 16r7FF
				ifTrue: [
					utf8Byte1 _ (utf32 bitShift: -6) bitOr: 2r11000000.
					uft8Byte2 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i _ contourDataIndexes at: utf8Byte1 + 1.
					i _ contourDataIndexes at: uft8Byte2 - i + 1.
					]
				ifFalse: [ utf32 <= 16rFFFF
				ifTrue: [
					utf8Byte1 _ (utf32 bitShift: -12) bitOr: 2r11100000.
					uft8Byte2 _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte3 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i _ contourDataIndexes at: utf8Byte1 + 1.
					i _ contourDataIndexes at: uft8Byte2 - i + 1.
					i _ contourDataIndexes at: utf8Byte3 - i + 1.
					]
				ifFalse: [
					utf8Byte1 _ (utf32 bitShift: -18) bitOr: 2r11110000.
					uft8Byte2 _ ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte3 _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					utf8Byte4 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					i _ contourDataIndexes at: utf8Byte1 + 1.
					i _ contourDataIndexes at: uft8Byte2 - i + 1.
					i _ contourDataIndexes at: utf8Byte3 - i + 1.
					i _ contourDataIndexes at: utf8Byte4 - i + 1.
					]]].
			advanceWidth _ contourData at: i. i _ i + 1.
			boundsLeft _ contourData at: i. i _ i + 1.
			boundsRight _ contourData at: i. i _ i + 1.
			boundsTop _ contourData at: i. i _ i + 1.
			boundsBottom _ contourData at: i. i _ i + 1.
			numContours _ (contourData at: i) asInteger. i _ i + 1.
			numContours timesRepeat: [
				numBeziers _ (contourData at: i) asInteger. i _ i + 1.
				ttMoveToX _ contourData at: i. i _ i + 1.
				ttMoveToY _ contourData at: i. i _ i + 1.
				x _ ttMoveToX * sx + nextGlyphX.
				y _ ttMoveToY * sy + nextGlyphY.
				contourStart _ x@y.
				self moveTo: contourStart.
				numBeziers timesRepeat: [
					ttEndX _ contourData at: i. i _ i + 1.
					ttEndY _ contourData at: i. i _ i + 1.
					ttControlX _ contourData at: i. i _ i + 1.
					ttControlY _ contourData at: i. i _ i + 1.
					cx _ ttControlX * sx + x.
					cy _ ttControlY * sy + y.
					x _ ttEndX * sx + x.
					y _ ttEndY * sy + y.
					self quadraticBezierTo: x@y control: cx@cy
					].
				self lineTo: contourStart.
				].
			nextGlyphX _ nextGlyphX + (advanceWidth * sx).
			]
	].
	^ nextGlyphX@nextGlyphY! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 5/13/2021 14:31:31'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth boundsLeft boundsRight boundsTop boundsBottom numContours numBeziers baseIndex x y cx cy contourStart |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	self fillColor: aColor do: [
		nextGlyphX _ aPoint x.
		nextGlyphY _ aPoint y.
		baseIndex _ 0.
		byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
			byte _ aByteArray at: byteIndex.
			i _ contourDataIndexes at: baseIndex + byte + 1.
			aBoolean ifTrue: [
				byte = 95 ifTrue: [
					i _ contourDataIndexes at: 226 + 1.
					i _ contourDataIndexes at: 134 - i + 1.
					i _ contourDataIndexes at: 144 - i + 1 ].
				byte = 94 ifTrue: [ 
					i _ contourDataIndexes at: 226 + 1.
					i _ contourDataIndexes at: 134 - i + 1.
					i _ contourDataIndexes at: 145 - i + 1 ]].
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
						ttMoveToX _ contourData at: i. i _ i + 1.
						ttMoveToY _ contourData at: i. i _ i + 1.
						x _ ttMoveToX * sx + nextGlyphX.
						y _ ttMoveToY * sy + nextGlyphY.
						contourStart _ x@y.
						self moveTo: contourStart.
						numBeziers timesRepeat: [
							ttEndX _ contourData at: i. i _ i + 1.
							ttEndY _ contourData at: i. i _ i + 1.
							ttControlX _ contourData at: i. i _ i + 1.
							ttControlY _ contourData at: i. i _ i + 1.
							cx _ ttControlX * sx + x.
							cy _ ttControlY * sy + y.
							x _ ttEndX * sx + x.
							y _ ttEndY * sy + y.
							self quadraticBezierTo: x@y control: cx@cy
							].
						self lineTo: contourStart.
						].
					nextGlyphX _ nextGlyphX + (advanceWidth * sx).
					baseIndex _ 0.
				]
			]
	].
	^ nextGlyphX@nextGlyphY! !

!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 1/17/2022 13:08:20'!
pathSequence: aFloat32Array
	"
	(VectorEngine onForm: Display subPixelAntiAliasing: true)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color black do: [ :engine |
			engine
				pathSequence: `#[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 24.0 21.0 30.0
					3.0 45.0 15.0 40.0 29.0 45.0 24.0
					3.0 30.0 0.0 45.0 6.0 39.0 0.0
					3.0 15.0 15.0 21.0 0.0 15.0 6.0
					0.0 21.0 19.0
					2.0 30.0 24.0 23.0 23.0
					2.0 39.0 19.0 37.0 23.0
					0.0 25.0 11.0
					1.0 25.0 13.0
					0.0 35.0 11.0
					1.0 35.0 13.0
					] asFloat32Array`
				].
	Display forceToScreen
	"
	| size i |
	size _ aFloat32Array size.
	i _ 1.
	[ i < size ] whileTrue: [
		| commandType p1x p1y p2x p2y p3x p3y |
		commandType _ aFloat32Array at: i. i _ i + 1.
		commandType caseOf: {
			[0.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x _ aFloat32Array at: i. i _ i + 1.
				p1y _ aFloat32Array at: i. i _ i + 1.
				self moveTo: p1x@p1y].
			[1.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x _ aFloat32Array at: i. i _ i + 1.
				p1y _ aFloat32Array at: i. i _ i + 1.
				self lineTo: p1x@p1y].
			[2.0] -> [
				i+2 < size ifFalse: [ ^self "failure" ].
				p1x _ aFloat32Array at: i. i _ i + 1.
				p1y _ aFloat32Array at: i. i _ i + 1.
				p2x _ aFloat32Array at: i. i _ i + 1.
				p2y _ aFloat32Array at: i. i _ i + 1.
				self quadraticBezierTo: p1x@p1y control: p2x@p2y ].
			[3.0] -> [
				i+4 < size ifFalse: [ ^self "failure" ].
				p1x _ aFloat32Array at: i. i _ i + 1.
				p1y _ aFloat32Array at: i. i _ i + 1.
				p2x _ aFloat32Array at: i. i _ i + 1.
				p2y _ aFloat32Array at: i. i _ i + 1.
				p3x _ aFloat32Array at: i. i _ i + 1.
				p3y _ aFloat32Array at: i. i _ i + 1.
				self cubicBezierTo: p1x@p1y control1: p2x@p2y control2: p3x@p3y ]}
		otherwise: [ ^self "failure"]].! !

!VectorEngine methodsFor: 'morphic' stamp: 'jmv 3/16/2021 09:55:37'!
canvas: aMorphicCanvas
	"Only needed if we are being called from a Canvas"

	canvas _ aMorphicCanvas! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 3/25/2021 19:31:28'!
doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Evaluate a sequence of path commands. Path parameters already set."

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[self]).
	self finishPath: ensureClosePath! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 3/18/2021 15:10:50'!
initializePath

	firstX _ firstY _ Float nan.
	currentX _ currentY _ 0.0.
	self newTrajectoryFragment.! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 5/14/2020 16:03:34'!
newTrajectoryFragment! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 6/11/2021 11:57:00'!
pvtHopLength: aFloat
	hop _ aFloat! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 5/15/2020 09:27:20'!
pvtStrokeWidth: aNumber

	strokeWidth _ aNumber.! !

!VectorEngine methodsFor: 'private' stamp: 'jmv 10/23/2021 08:54:31'!
updateMorphBoundsLeft: l top: t right: r bottom: b
	"For internal framework use only"

	morphBoundsLeft _ morphBoundsLeft min: l.
	morphBoundsTop _ morphBoundsTop min: t.
	morphBoundsRight _ morphBoundsRight max: r.
	morphBoundsBottom _ morphBoundsBottom max: b.! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:38:22'!
circleApproximationExamples
"
VectorEngine circleApproximationExamples
https://spencermortensen.com/articles/bezier-circle/"
| ve c cx cy d e r |
cx _ 250.
cy _ 250.
r _ 200.
c _ 0.085 * r.
d _ 0.5 * r.
e _ 0.552 * r.
ve _ VectorEngine onForm: Display subPixelAntiAliasing: false.
ve
	strokeWidth: 5.0 color: Color red do: [ :engine |
		engine
			circleCenter: cx @ cy radius: r].
ve												"Quadratic, coarse and better approximations"
	strokeWidth: 1.0 color: Color white do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			quadraticBezierTo: cx - r @ cy control: (cx - r) @ (cy + r);
			moveTo: cx @ (cy + r);
			quadraticBezierTo: cx - r @ cy control: (cx - r + c) @ (cy + r - c) ].
ve												"Cubic, coarse approximation"
	strokeWidth: 1.0 color: Color green do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			cubicBezierTo: cx - r @ cy control1: (cx - d) @ (cy + r) control2: (cx - r) @ (cy + d) ].
ve												"Cubic, good approximation"
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			cubicBezierTo: cx - r @ cy control1: (cx - e) @ (cy + r) control2: (cx - r) @ (cy + e) ].
Display forceToScreen
! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:46:46'!
experLinesAndShapes
	"
Display fill: (0@0 corner: 120@1200) fillColor: Color white.
	VectorEngine experLinesAndShapes
	"
	| engine y1 yy2 dy y2 aaw |
	engine _ self onForm: Display subPixelAntiAliasing: true.
	aaw _ engine class defaultAntiAliasingWidth.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	Display forceToScreen.
	engine strokeWidth: aaw color: Color black fillColor: Color black do: [
		engine
			moveTo: 220@10;
			lineTo: 410@15; lineTo: 220@20;
			lineTo: 215@410; lineTo: 210@20;
			lineToFirstPoint ].
	Display forceToScreen.

	y1 _ 10.
	dy _ 3.
	y2 _ aaw * 2.
	15 timesRepeat: [
		"stroke with w > aaw"
		yy2 _ y2 >= 1 ifTrue: [ aaw * 0.5 ] ifFalse: [0].
		engine strokeWidth: aaw color: Color black fillColor: Color black do: [
			engine
				moveTo: 431@(y1+yy2); lineTo: 530@(y1+yy2+dy);
				lineTo: 530@(y1+y2-yy2+dy); lineTo: 431@(y1+y2-yy2);
				lineToFirstPoint.
			engine
				moveTo: y1+yy2@431; lineTo: y1+yy2+dy@530;
				lineTo: y1+y2-yy2+dy@530; lineTo: y1+y2-yy2@431;
				lineToFirstPoint ].
		"Stroke too narrow to be visible"
		engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
			engine
				moveTo: 531@(y1+dy); lineTo: 630@(y1+dy+dy);
				lineTo: 630@(y1+y2+dy+dy); lineTo: 531@(y1+y2+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy@531; lineTo: y1+dy+dy@630;
				lineTo: y1+y2+dy+dy@630; lineTo: y1+y2+dy@531;
				lineToFirstPoint ].
		"Only fill."
		engine fillColor: Color black do: [
			engine
				moveTo: 631@(y1+dy+dy); lineTo: 730@(y1+dy+dy+dy);
				lineTo: 730@(y1+y2+dy+dy+dy); lineTo: 631@(y1+y2+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy@631; lineTo: y1+dy+dy+dy@730;
				lineTo: y1+y2+dy+dy+dy@730; lineTo: y1+y2+dy+dy@631;
				lineToFirstPoint ].
		"Only stroke, no fill"
		engine strokeWidth: y2 color: Color black do: [
			engine
				moveTo: y2*0.5+731@(y2*0.5 + y1+dy+dy+dy); lineTo: 830@(y2 * 0.5 + y1 + dy + dy+dy+dy).
			engine
				moveTo: y2*0.5 + y1+dy+dy+dy@(y2*0.5+731); lineTo: y2 * 0.5 + y1 + dy + dy+dy+dy @ 830 ].
		y1 _ y1 + y2 + aaw + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:48:16'!
experLinesAndShapes2
	"
	VectorEngine experLinesAndShapes2
	"
	| engine y1 dy y2 aaw |
	engine _ self onForm: Display subPixelAntiAliasing: true.
	aaw _ engine class defaultAntiAliasingWidth.

	y1 _ 10.
	dy _ 3.
	y2 _ aaw*4*2.
	15 timesRepeat: [
		"stroke is narrow but visible"
		engine strokeWidth: 1.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 131@y1; lineTo: 230@(y1+dy);
				lineTo: 230@(y1+y2+dy); lineTo: 131@(y1+y2);
				lineToFirstPoint.
			engine
				moveTo: y1@131; lineTo: y1+dy@230;
				lineTo: y1+y2+dy@230; lineTo: y1+y2@131;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.8 color: Color green fillColor: Color red do: [
			engine
				moveTo: 231@(y1+dy); lineTo: 330@(y1+dy+dy);
				lineTo: 330@(y1+y2+dy+dy); lineTo: 231@(y1+y2+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy@231; lineTo: y1+dy+dy@330;
				lineTo: y1+y2+dy+dy@330; lineTo: y1+y2+dy@231;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.6 color: Color green fillColor: Color red do: [.
			engine
				moveTo: 331@(y1+dy+dy); lineTo: 430@(y1+dy+dy+dy);
				lineTo: 430@(y1+y2+dy+dy+dy); lineTo: 331@(y1+y2+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy@331; lineTo: y1+dy+dy+dy@430;
				lineTo: y1+y2+dy+dy+dy@430; lineTo: y1+y2+dy+dy@331;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.4 color: Color green fillColor: Color red do: [
			engine
				moveTo: 431@(y1+dy+dy+dy); lineTo: 530@(y1+dy+dy+dy+dy);
				lineTo: 530@(y1+y2+dy+dy+dy+dy); lineTo: 431@(y1+y2+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy@431; lineTo: y1+dy+dy+dy+dy@530;
				lineTo: y1+y2+dy+dy+dy+dy@530; lineTo: y1+y2+dy+dy+dy@431;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.2 color: Color green fillColor: Color red do: [
			engine
				moveTo: 531@(y1+dy+dy+dy+dy); lineTo: 630@(y1+dy+dy+dy+dy+dy);
				lineTo: 630@(y1+y2+dy+dy+dy+dy+dy); lineTo: 531@(y1+y2+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy@531; lineTo: y1+dy+dy+dy+dy+dy@630;
				lineTo: y1+y2+dy+dy+dy+dy+dy@630; lineTo: y1+y2+dy+dy+dy+dy@531;
				lineToFirstPoint ].
		"stroke zero means no stroke"
		engine strokeWidth: 0.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 631@(y1+dy+dy+dy+dy+dy); lineTo: 730@(y1+dy+dy+dy+dy+dy+dy);
				lineTo: 730@(y1+y2+dy+dy+dy+dy+dy+dy); lineTo: 631@(y1+y2+dy+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy+dy@631; lineTo: y1+dy+dy+dy+dy+dy+dy@730;
				lineTo: y1+y2+dy+dy+dy+dy+dy+dy@730; lineTo: y1+y2+dy+dy+dy+dy+dy@631;
				lineToFirstPoint ].
		y1 _ y1 + y2 + aaw + 3.
		Display forceToScreen.
		y2 print.
		y2 _ y2 * 0.8.
	].
	
	Display forceToScreen! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:48:55'!
experimentHopsPerPixel1
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 1: Very thin stroke, the same color as fill
	VectorEngine experimentHopsPerPixel1
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 3.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ self onForm: f1 subPixelAntiAliasing: true.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ self onForm: f2 subPixelAntiAliasing: true.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) *10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:49:25'!
experimentHopsPerPixel2
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 2: Stroke of various widths, no fill
	VectorEngine experimentHopsPerPixel2
	"
	| d engine f1 f2 hops1 hops2 strokeWidth |
	hops1 _ 200.
	hops1 _ 20.
	hops2 _ 3.
	strokeWidth _ 1.4.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ self onForm: f1 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50 @50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ self onForm: f2 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50@50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 1/17/2022 12:49:32'!
experimentHopsPerPixel3
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 3: No stroke, just fill
	VectorEngine experimentHopsPerPixel3
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 _ 200.
	hops2 _ 3.
	f1 _ Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine _ self onForm: f1 subPixelAntiAliasing: true.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 _ Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine _ self onForm: f2 subPixelAntiAliasing: true.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d _ (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 3/22/2021 14:59:34'!
defaultAntiAliasingWidth
	self subclassResponsibility! !

!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 1/17/2022 13:04:14'!
onForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing
	Use the VectorEnginePlugin if available."

	| doSubPixelAntiAliasing |
	doSubPixelAntiAliasing _ aBoolean ifNil: [
		"Assume high resolution implies high DPI"
		Display width > 1920 ].
	^(VectorEngineWithPlugin isPluginAvailable
		ifTrue: [ doSubPixelAntiAliasing
			ifTrue: [VectorEngineWithPluginSubPixel]
			ifFalse: [VectorEngineWithPluginWholePixel]]
		ifFalse: [ doSubPixelAntiAliasing
			ifTrue: [Smalltalk at: #VectorEngineSubPixel]
			ifFalse: [Smalltalk at: #VectorEngineWholePixel]])
				new initializeOnForm: aForm.! !

!VectorEngineBoundsFinder methodsFor: 'accessing' stamp: 'jmv 11/17/2020 10:04:59'!
currentMorph: aMorph! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 12:01:10'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	self pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth).
	self pvtHopLength: 0.5.
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:53:03'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtHopLength: 0.5.										"See #experimentHopsPerPixel3"
	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:54:46'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		do: pathCommandsBlock
		ensureClosePath: false! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 3/5/2021 15:20:36'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	clientStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	^ self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 8/8/2021 22:26:44'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	| dx dy trx try tcx tcy center angle xp yp x y d hops |
	dx _ geometryTransformation externalizeDelta: rx@0.
	dy _ geometryTransformation externalizeDelta: 0@ry.
	trx _ dx r.
	try _ dy r.
	hops _ ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d _ sweepAngle / hops.
	center _ geometryTransformation externalizePosition: cx@cy.
	tcx _ center x.
	tcy _ center y.
	(trx = try and: [
		d * hops > 6.28]) ifTrue: [ 						"If a circle, not an ellipse, and a complete circle, or pretty close"
			spanLeft _ spanLeft min: tcx - trx.
			spanRight _ spanRight max: tcx + trx.
			spanTop _ spanTop min: tcy - trx.
			spanBottom _ spanBottom max: tcy + trx ]

		ifFalse: [						"Else go the more expensive way"
			angle _ startAngle.
			hops _ hops+1.
			hops timesRepeat: [
				xp _ angle cos * trx.
				yp _ angle sin * try.
				x _ thetaCos * xp - (thetaSin * yp) + tcx.
				y _ thetaSin * xp + (thetaCos * yp) + tcy.
				spanLeft _ spanLeft min: x.
				spanRight _ spanRight max: x.
				spanTop _ spanTop min: y.
				spanBottom _ spanBottom max: y.
				angle _ angle + d. ]].! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 8/8/2021 22:26:44'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2
	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 xMinEnd xMaxEnd yMinEnd yMaxEnd |
	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.
	p _ geometryTransformation externalizePosition: xControl1@yControl1. txControl1 _ p x. tyControl1 _ p y.
	p _ geometryTransformation externalizePosition: xControl2@yControl2. txControl2 _ p x. tyControl2 _ p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ txFrom min: txTo.
	xMaxEnd _ txFrom max: txTo.
	yMinEnd _ tyFrom min: tyTo.
	yMaxEnd _ tyFrom max: tyTo.
	spanLeft _ spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight _ spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop _ spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom _ spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 8/8/2021 22:26:44'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| p txFrom tyFrom txTo tyTo |
	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.

	spanLeft _ spanLeft min: (txFrom min: txTo).
	spanRight _ spanRight max: (txFrom max: txTo).
	spanTop _ spanTop min: (tyFrom min: tyTo).
	spanBottom _ spanBottom max: (tyFrom max: tyTo).! !

!VectorEngineBoundsFinder methodsFor: 'primitive algorithms' stamp: 'jmv 8/8/2021 22:26:44'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl
	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."

	| p txFrom tyFrom txTo tyTo txControl tyControl xMinEnd xMaxEnd yMinEnd yMaxEnd |
	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.
	p _ geometryTransformation externalizePosition: xControl@yControl. txControl _ p x. tyControl _ p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ txFrom min: txTo.
	xMaxEnd _ txFrom max: txTo.
	yMinEnd _ tyFrom min: tyTo.
	yMaxEnd _ tyFrom max: tyTo.
	spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop _ spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 8/10/2021 12:03:57'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"

	spanLeft _ (spanLeft - swDilatedHalf - subPixelDelta + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop _ (spanTop - swDilatedHalf + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight _ (spanRight + swDilatedHalf + subPixelDelta) truncated + 1. 											"+1 not strictly needed in BoundsFinder, but just answer same bounds as other engines."
	spanBottom _ (spanBottom + swDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft _ morphBoundsLeft min: spanLeft.
	morphBoundsTop _ morphBoundsTop min: spanTop.
	morphBoundsRight _ morphBoundsRight max: spanRight.
	morphBoundsBottom _ morphBoundsBottom max: spanBottom.! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 8/30/2021 16:35:33'!
initializePath

	super initializePath.

	spanTop _ 16r3FFFFFFF.						"drawable bottom. Will later be refined."
	spanBottom _ 0.						"drawable top. Will later be refined."
	spanLeft _ 16r3FFFFFFF.						"drawable right. Will later be refined."
	spanRight _ 0.						"drawable left. Will later be refined."! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 8/10/2021 12:03:57'!
pvtStrokeWidth: aNumber

	super pvtStrokeWidth: aNumber.
	swDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 8/30/2021 16:35:36'!
resetMorphBounds

	morphBoundsTop _ 16r3FFFFFFF.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ 16r3FFFFFFF.
	morphBoundsRight _ 0! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 3/22/2021 11:19:05'!
subPixelDelta: aNumber
	subPixelDelta _ aNumber! !

!VectorEngineBoundsFinder methodsFor: 'initialization' stamp: 'jmv 8/11/2021 20:04:04'!
initialize
	self resetMorphBounds.! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:07:18'!
clipCurrentMorph: aBoolean
	clipCurrentMorph _ aBoolean.! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:07:01'!
clipRect: aRectangle 
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipLeft _ aRectangle left asInteger max: 0.
	clipTop _ aRectangle top asInteger max: 0.
	clipRight _ (aRectangle right asInteger min: targetWidth) - 1.
	clipBottom _ (aRectangle bottom asInteger min: targetHeight) - 1! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:07:18'!
currentMorph: aMorph
	aMorph
		ifNil: [
			currentMorphId _ 0.
			currentClipsSubmorphs _ false.
			clipCurrentMorph _ false. 							"Essentially for initialization"
			]
		ifNotNil: [
			currentMorphId _ aMorph morphId.
			currentClipsSubmorphs _ aMorph clipsSubmorphsReally ].! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/8/2021 23:24:12'!
disableSubPixelSampling
	"NOP here.
	To be redefined if needed"! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 2/25/2022 12:27:40'!
morphContourAfterDrawInto: contourArray into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or answer a new array if not.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment."

	| answer i requiredSize y0 y1 dilationHalfHeight strokeHalfSquared |
	y0 _ morphBoundsTop max: 0.
	y1 _ morphBoundsBottom min: targetHeight -1.
	requiredSize _ (y1 - y0 + 1) *  2 max: 0.
	answer _ contourArray.
	(answer isNil or: [ requiredSize > answer size ]) ifTrue: [
		answer _ IntegerArray new: requiredSize ].
	dilationHalfHeight _ (strokeWidth / 2.0) rounded.
	strokeHalfSquared _ (strokeWidth / 2.0) squared.
	i _ 1.
	y0 to: y1 do: [ :y | | x0 x1 |
		x0 _ targetWidth.
		x1 _ 0.
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y + dy between: y0 and: y1) ifTrue: [
				dx _ (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 _ x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 _ x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded.
		]].
		answer at: i put: x0.
		answer at: i+1 put: x1.
		i _ i + 2 ].
	i to: answer size by: 2 do: [ :ii |
		answer at: ii put: targetWidth.
		answer at: ii+1 put: 0 ].
	aBlock value: answer value: y0 value: y1.! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:06:26'!
morphContourAfterDrawUpdate: oldContour oldTop: oldTop oldBottom: oldBottom into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment."

	| answer i ii requiredSize oldContourDelta y0 y1 dilationHalfHeight strokeHalfSquared mbb mbt |
	dilationHalfHeight _ (strokeWidth / 2.0) rounded.
	strokeHalfSquared _ (strokeWidth / 2.0) squared.
	y0 _ (morphBoundsTop min: oldTop) max: 0.
	y1 _ (morphBoundsBottom max: oldBottom) min: targetHeight -1.
	requiredSize _ (y1 - y0 + 1) *  2.
	answer _ oldContour.
	oldContourDelta _ 0.
	(answer size < requiredSize or: [y0 < oldTop] ) ifTrue: [
		oldContourDelta _ (oldTop - y0) * 2.
		answer _ IntegerArray new: requiredSize ].
	i _ 1.
	ii _ 1-oldContourDelta.
	mbt _ morphBoundsTop max: 0.
	mbb _ morphBoundsBottom min: targetHeight -1.
	y0 to: y1 do: [ :y | | x0 x1 |
		(ii between: 1 and: oldContour size-1) ifTrue: [
			x0 _ oldContour at: ii.
			x1 _ oldContour at: ii+1 ]
		ifFalse: [
			x0 _ targetWidth.
			x1 _ 0 ].
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y+dy between: mbt and: mbb) ifTrue: [
				dx _ (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 _ x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 _ x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		answer at: i put: x0.
		answer at: i+1 put: x1.
		i _ i + 2.
		ii _ ii + 2 ].
	i to: answer size by: 2 do: [ :iii |
		answer at: iii put: targetWidth.
		answer at: iii+1 put: 0 ].
	aBlock value: answer value: y0 value: y1.
	^answer! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/16/2021 15:34:41'!
morphIds
	^morphIds! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 8/8/2021 23:24:28'!
subPixelDelta
	"To be redefined if needed"

	^0.0! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 9/3/2021 14:23:06'!
auxPixelDataClass
	^self subclassResponsibility! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 9/7/2021 09:53:47'!
clearArrays
	morphIds atAllPut: 0.
	edgeCounts atAllPut: 0.
	alphaMask atAllPut: 0.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 8/11/2021 20:04:10'!
initializeOnForm: aForm
	self currentMorph: nil.
	self pvtSetForm: aForm.! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 9/20/2021 12:06:54'!
morphIdAt: aPoint
	| index |
	index _ aPoint y * targetWidth + aPoint x + 1.
	(index between: 1 and: morphIds size) ifFalse: [ ^0 ].
	^(morphIds at: index) >> 8! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 9/21/2021 20:11:21'!
pvtSetForm: aForm

	| s |
	targetForm _ aForm.
	targetWidth _ targetForm width.
	targetHeight _ targetForm height.
	s _ targetWidth * targetHeight.

	self flag: #VEPapi5.		"Eventually remove the +1. Needed for VEPapi=5 and 6 (for plugins older than Sept-21-2021, and version less than 1.29)"
	morphIds _ WordArray new: s+1.

	edgeCounts _ self auxPixelDataClass new: s.
	alphaMask _ self auxPixelDataClass new: s.
	contour _ Float32Array new: targetHeight * 2.
	(aForm == Display and: [	Smalltalk isSpur]) ifTrue: [
		Smalltalk primitiveGarbageCollect.
		"Display bits is pinned by the VM. Pinning these large arrays should relieve some burden on the GC."
		morphIds pin. edgeCounts pin. alphaMask pin. contour pin ].
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation _ MorphicTranslation identity.! !

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 8/10/2021 12:05:47'!
resetMorphBounds

	morphBoundsTop _ targetHeight - 1.
	morphBoundsBottom _ 0.
	morphBoundsLeft _ targetWidth - 1.
	morphBoundsRight _ 0! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:58:30'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	| sharper apparentWeight |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"There are two possible strategies when requested stroke width is narrower than anti aliasing filter.
			To more effectively avoid aliasing, set sharper to false.
			To draw sharper thin lines, at the price of very little extra aliasing in them, set sharper to true.
			This could be turned into a Preference."
			sharper _ true.
			sharper ifTrue: [
				"If the requested stroke width is used, filter will be clipped, resulting in an incorrect weight for the stroke."
				apparentWeight _ ((aStrokeWidth squared / antiAliasingWidth) + (2.0 * aStrokeWidth) + antiAliasingWidth) / 4.0.
				"{'(sharper)desired weight'. aStrokeWidth. 'but would get'. apparentWeight. 'correction'. aStrokeWidth / apparentWeight} print."
				self pvtStrokeWidth: aStrokeWidth.
				self pvtStrokeColor: (aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / apparentWeight)]
			ifFalse: [
				"If the requested stroke width is too thin, make them wide enough to be properly sampled."
				"{'(wider)desired weight'. aStrokeWidth. 'but would get'. antiAliasingWidth. 'correction'. aStrokeWidth / antiAliasingWidth} print."
				self pvtStrokeWidth: antiAliasingWidth.
				self pvtStrokeColor: (aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / antiAliasingWidth)]
			]
		ifFalse: [
			self pvtStrokeWidth: aStrokeWidth.
			self pvtStrokeColor: aStrokeColor ].
	self pvtFillColor: nil.
	self pvtHopLength: 0.5.									"See #experimentHopsPerPixel2"

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:48:50'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: false ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		canvas ifNotNil: [ canvas initializePath ].
		self basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: true.
		^ self ].

	self pvtStrokeWidth: aStrokeWidth.
	self pvtStrokeColor: aStrokeColor.
	self pvtFillColor: aFillColor.
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"Strokes are really thin, but strokeColor is same as fillColor, so there's no problem sampling them."
			self pvtHopLength: 0.5 ]								"See #experimentHopsPerPixel1"
		ifFalse: [
			self pvtHopLength: 0.5 ].								"See #experimentHopsPerPixel2."

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:48:55'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.
	self pvtHopLength: 0.5.										"See #experimentHopsPerPixel3"
	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:52:14'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		do: pathCommandsBlock
		ensureClosePath: false! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 8/11/2021 11:52:20'!
strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock! !

!VectorEngineDrawer methodsFor: 'testing' stamp: 'jmv 8/27/2021 16:05:35'!
usesVectorEnginePlugin
	^false! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 8/10/2021 12:06:26'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	self
		primArcCenterX: cx asFloat centerY: cy asFloat
		radiusX: rx asFloat radiusY: ry asFloat
		start: startAngle asFloat
		sweep: sweepAngle asFloat
		rotationCos: thetaCos asFloat rotationSin: thetaSin asFloat
		edgeCounts: edgeCounts alphaMask: alphaMask contour: contour! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 8/10/2021 12:06:26'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	self
		primCubicBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		control1X: xControl1 asFloat y: yControl1 asFloat
		control2X: xControl2 asFloat y: yControl2 asFloat
		edgeCounts: edgeCounts alphaMask: alphaMask contour: contour! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 8/10/2021 12:06:26'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	self
		primLineFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		edgeCounts: edgeCounts alphaMask: alphaMask contour: contour! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 8/10/2021 12:06:26'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	self
		primQuadraticBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		controlX: xControl asFloat y: yControl asFloat
		edgeCounts: edgeCounts alphaMask: alphaMask contour: contour! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 8/10/2021 12:06:05'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale asFloat.
	sy _ scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForIso8859s15.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByIso8859s15.
	nextGlyphX _ aPoint x asFloat.
	nextGlyphY _ aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX _ self
			primDisplayString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
			useST80Glyphs: aBoolean
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 8/10/2021 12:06:05'!
displayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX _ aPoint x asFloat.
	nextGlyphY _ aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX _ self
			primDisplayUtf32: aWordArray from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
			useST80Glyphs: aBoolean
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 8/10/2021 12:06:05'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription useST80Glyphs: aBoolean color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale _ 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx _ scale * trueTypeScale.
	sy _ scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy _ sy negated ].
	contourData _ aTTFontDescription contourDataForUtf8.
	contourDataIndexes _ aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX _ aPoint x asFloat.
	nextGlyphY _ aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX _ self
			primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
			useST80Glyphs: aBoolean
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 8/10/2021 12:06:26'!
pathSequence: aFloat32Array

	self
		primPathSequence: aFloat32Array asFloat32Array
		size: aFloat32Array size
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 1/16/2022 12:52:34'!
doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Evaluate a sequence of path commands. Path parameters already set.
	VectorEnginePlugin is not reentrant.
	- Any other process must wait.
	- Nested calls in the same process are not supported, and ignored."

	self class protected: [
		super doPath: pathCommandsBlock ensureClosePath: ensureClosePath ].! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 8/22/2021 15:31:21'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| spanLeft spanTop spanRight spanBottom |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self primUpdateContourLastLine: contour.

	spanLeft _ self primSpanLeft.
	spanTop _ self primSpanTop.
	spanRight _ self primSpanRight.
	spanBottom _ self primSpanBottom.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft _ morphBoundsLeft min: spanLeft.
	morphBoundsTop _ morphBoundsTop min: spanTop.
	morphBoundsRight _ morphBoundsRight max: spanRight.
	morphBoundsBottom _ morphBoundsBottom max: spanBottom.

	spanLeft< clipLeft ifTrue: [ spanLeft _ clipLeft ].
	spanTop < clipTop ifTrue: [ spanTop _ clipTop ].
	spanRight > clipRight ifTrue: [ spanRight _ clipRight ].
	spanBottom > clipBottom ifTrue: [ spanBottom _ clipBottom ].

	(spanRight >= spanLeft and: [ spanBottom >= spanTop ]) ifTrue: [
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self primBlendStrokeAndFillLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds edgeCounts: edgeCounts alphaMask: alphaMask ]
				ifFalse: [ self primBlendFillOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds edgeCounts: edgeCounts alphaMask: alphaMask ]]
			ifFalse: [
				self primBlendStrokeOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds alphaMask: alphaMask ]].! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 8/10/2021 12:07:18'!
initializePath

	super initializePath.

	"Set all plugin state. There might be several instances of this class, all using the same plugin.
	Only one of them will use the plugin during a path draw operation. See class variable PluginAccess."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat
		hopLength: hop asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self primCurrentMorphId: currentMorphId currentClipsSubmorphs: currentClipsSubmorphs.
	self primClipCurrentMorph: clipCurrentMorph morphIds: morphIds.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.
	self primStrokeWidth: strokeWidth.
	self primStrokeColor: strokeColor.
	self primFillColor: fillColor.

	self primInitializePath.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 5/18/2021 12:25:12'!
newTrajectoryFragment

	self primNewTrajectoryFragment.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 8/10/2021 12:06:37'!
pvtFillColor: aFillColorOrNil
	fillColor _ aFillColorOrNil.
	self primFillColor: fillColor.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 8/10/2021 12:06:31'!
pvtStrokeColor: aStrokeColorOrNil
	strokeColor _ aStrokeColorOrNil.
	self primStrokeColor: strokeColor.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 5/18/2021 12:25:12'!
pvtStrokeWidth: aNumber

	super pvtStrokeWidth: aNumber asFloat.
	self primStrokeWidth: strokeWidth.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 8/22/2021 15:28:54'!
resetMorphBounds

	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		self
			primResetContourTop: (morphBoundsTop max: 0)
			bottom: (morphBoundsBottom min: targetHeight -1)
			contour: contour ].
	super resetMorphBounds.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 6/11/2021 12:05:33'!
primAntiAliasingWidth: aNumber subPixelDelta: otherFloat hopLength: anotherFloat
	<primitive: 'primAntiAliasingWidthsubPixelDeltaHopLength' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primClipCurrentMorph: aBoolean morphIds: aWordArray
	<primitive: 'primClipCurrentMorph' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primClipLeft: l clipTop: t clipRight: r clipBottom: b
	<primitive: 'primClipLeftclipTopclipRightclipBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primCurrentMorphId: aNumber currentClipsSubmorphs: aBoolean
	<primitive: 'primCurrentMorphIdcurrentClipsSubmorphs' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primFillColor: aFillColorOrNil
	aFillColorOrNil notNil
		ifTrue: [
			self
				primFillR: aFillColorOrNil red
				g: aFillColorOrNil green
				b: aFillColorOrNil blue
				a: aFillColorOrNil alpha]
		ifFalse: [self primFillR: 0.0 g: 0.0 b: 0.0 a: 0.0].! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primFillR: r g: g b: b a: a
	<primitive: 'primFillRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primGeometryTxA11: a11 a12: a12 a13: a13 a21: a21 a22: a22 a23: a23
	<primitive: 'primGeometryTxSet' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primInitializePath
	<primitive: 'primInitializePath' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primNewTrajectoryFragment
	<primitive: 'primNewTrajectoryFragment' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:25:58'!
primResetContourTop: t bottom: b
	"Only for compatibility with VMs including plugin API v5"
	<primitive: 'primResetContour' module: 'VectorEnginePlugin'>

	self primitiveFailed.
	self flag: #VEPapi5.		"Eventually delete this method"! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:27:01'!
primResetContourTop: t bottom: b contour: aFloat32Array
	<primitive: 'primReset2Contour' module: 'VectorEnginePlugin'>
	"
	self primitiveFailed.
	"
	self flag: #VEPapi5.		"Eventually delete this part, and just call primitiveFailed"
	"In case of primitive failure, assume it is because plugin is API version 5 and not 6."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primResetContourTop: t bottom: b! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanBottom
	<primitive: 'primSpanBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanLeft
	<primitive: 'primSpanLeft' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanRight
	<primitive: 'primSpanRight' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanTop
	<primitive: 'primSpanTop' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primStrokeColor: aStrokeColorOrNil
	aStrokeColorOrNil notNil
		ifTrue: [
			self
				primStrokeR: aStrokeColorOrNil red
				g: aStrokeColorOrNil green
				b: aStrokeColorOrNil blue
				a: aStrokeColorOrNil alpha]
		ifFalse: [self primStrokeR: 0.0 g: 0.0 b: 0.0 a: 0.0]! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primStrokeR: r g: g b: b a: a
	<primitive: 'primStrokeRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primStrokeWidth: aNumber
	<primitive: 'primStrokeWidth' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:25:51'!
primUpdateContourLastLine
	"Only for compatibility with VMs including plugin API v5"
	<primitive: 'primUpdateContourLastLine' module: 'VectorEnginePlugin'>

	self primitiveFailed.
	self flag: #VEPapi5.		"Eventually delete this method"! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:26:44'!
primUpdateContourLastLine: aFloat32Array
	<primitive: 'primUpdate2ContourLastLine' module: 'VectorEnginePlugin'>
	"
	self primitiveFailed.
	"
	self flag: #VEPapi5.		"Eventually delete this part, and just call primitiveFailed"
	"In case of primitive failure, assume it is because plugin is API version 5 and not 6."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primUpdateContourLastLine! !

!VectorEngineWithPlugin methodsFor: 'testing' stamp: 'jmv 8/27/2021 16:05:47'!
usesVectorEnginePlugin
	^true! !

!VectorEngineWithPlugin class methodsFor: 'instance creation' stamp: 'jmv 8/22/2021 16:27:30'!
isPluginAvailable
	"
	VectorEngineWithPlugin isPluginAvailable
	Check if services provided by VM plugin match this class requirements.
	"
	| pluginApiVersion |
	pluginApiVersion _ self pluginApiVersion.
	self flag: #VEPapi5.		"Eventually accept only v6"
	^ pluginApiVersion = 5 or: [ pluginApiVersion = 6 ]! !

!VectorEngineWithPlugin class methodsFor: 'private' stamp: 'jmv 5/18/2021 12:51:46'!
pluginApiVersion
	"
	VectorEngineWithPlugin isPluginAvailable
	Semantic version numbering in a single SmallInteger:
	Version numbers 1 to 99 are backwards compatible. When incompatibility is needed. jump straight to 101.
	Version numbers 101 to 199 are backwards compatible. When incompatibility is needed. jump straight to 201.
	And so.
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	<primitive: 'pluginApiVersion' module: 'VectorEnginePlugin'>

	^ 0! !

!VectorEngineWithPlugin class methodsFor: 'private' stamp: 'jmv 1/16/2022 12:52:46'!
protected: aBlock
	"VectorEnginePlugin is not reentrant.
	- Any other process must wait.
	- Nested calls in the same process are not supported, and ignored."

	^(AccessLock ifNil: [ AccessLock := Mutex new ])
		critical: aBlock
		onReentry: [
			'Nested call to VectorEnginePlugin. Inner call ignored.' print.
			'Please debug this or report to the Cuis-Dev mail list.' print.
			thisContext printStack: 10 ]! !

!VectorEngineWithPluginSubPixel methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:08:00'!
disableSubPixelSampling
	subPixelDelta _ 0.0! !

!VectorEngineWithPluginSubPixel methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:08:00'!
subPixelDelta

	^subPixelDelta! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:55:01'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primArc' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:31:01'!
primBlendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendFillOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:35:55'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendStrokeAndFill' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:47'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendStrokeOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:56:26'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primCubicBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:32'!
primDisplayString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayString' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:11'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayUtf32' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:53'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayUtf8' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:03:16'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primLine' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:34:07'!
primPathSequence: aFloat32Array size: s edgeCounts: otherWordArray alphaMask: anotherWordArray contour: otherFloat32Array
	<primitive: 'primPathSequence' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:01:04'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primQuadraticBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:05:29'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTarget' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:23:45'!
auxPixelDataClass
	"Because we hold data for subpixels"

	^WordArray! !

!VectorEngineWithPluginSubPixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:27:21'!
pvtSetForm: aForm

	super pvtSetForm: aForm.
	subPixelDelta _ self class defaultSubPixelDelta.! !

!VectorEngineWithPluginSubPixel class methodsFor: 'instance creation' stamp: 'jmv 5/18/2021 11:44:04'!
defaultAntiAliasingWidth
	^1.4! !

!VectorEngineWithPluginSubPixel class methodsFor: 'instance creation' stamp: 'jmv 5/18/2021 12:41:20'!
defaultSubPixelDelta

	^0.25  			"Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:55:07'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primArcWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:30:59'!
primBlendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendFillOnlyWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:07'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeAndFillWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:46'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeOnlyWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:56:31'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primCubicBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:37'!
primDisplayString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayStringWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:16'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayUtf32WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:57'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayUtf8WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:03:20'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primLineWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:34:11'!
primPathSequence: aFloat32Array size: s edgeCounts: otherByteArray alphaMask: anotherByteArray contour: otherFloat32Array
	<primitive: 'primPathSequenceWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:00:40'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primQuadraticBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:05:44'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTargetWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:24:08'!
auxPixelDataClass
	"Because we hold data for whole pixels"

	^ByteArray! !

!VectorEngineWithPluginWholePixel class methodsFor: 'instance creation' stamp: 'jmv 5/18/2021 11:44:58'!
defaultAntiAliasingWidth
	^1.6! !

!PathCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:15'!
buildFrom: commandChar stream: aStream

	coordinatesAreRelative _ commandChar isLowercase! !

!PathCommand methodsFor: 'building' stamp: 'jmv 5/4/2010 06:16'!
buildFromStream: aStream in: parentToBe

	coordinatesAreRelative _ aStream next isLowercase! !

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

!PathArcCommand methodsFor: 'building' stamp: 'jmv 6/18/2021 11:55:45'!
buildFrom: commandChar stream: aStream

	| rx ry xAxisRotation doLargerArc useIncreasingAngle x y |
	super buildFrom: commandChar stream: aStream.
	rs _ OrderedCollection new.
	xAxisRotations _ OrderedCollection new.
	toPoints _ OrderedCollection new.
	doLargerArcs _ OrderedCollection new.
	useIncreasingAngles _ OrderedCollection new.
	[
		rx _ aStream nextNumber.
		ry _ aStream nextNumber.
		xAxisRotation _ aStream nextNumber.
		doLargerArc _ (aStream nextNumber = 0) not.
		useIncreasingAngle _ (aStream nextNumber = 0) not.
		x _ aStream nextNumber.
		y _ aStream nextNumber.
		y notNil ] whileTrue: [
			rs add: rx@ry.
			xAxisRotations add: xAxisRotation degreesToRadians.
			doLargerArcs add: doLargerArc.
			useIncreasingAngles add: useIncreasingAngle.
			toPoints add: x@y ]! !

!PathArcCommand methodsFor: 'drawing' stamp: 'jmv 6/18/2021 11:57:04'!
runOn: aCanvas

	aCanvas multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngles: useIncreasingAngles! !

!PathArcCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:07'!
svgName

	^$A! !

!PathCloseCommand methodsFor: 'building' stamp: 'jmv 6/11/2010 14:10'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream! !

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

!PathMoveToCommand methodsFor: 'drawing' stamp: 'jmv 12/16/2021 12:04:19'!
runOn: aCanvas

	aCanvas closePathAndMoveBack.
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

!PathQuadraticCurveToSmoothCommand methodsFor: 'drawing' stamp: 'jmv 12/22/2017 13:02:58'!
runOn: aCanvas

	aCanvas multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints! !

!PathQuadraticCurveToSmoothCommand class methodsFor: 'instance creation' stamp: 'jmv 5/4/2010 06:09'!
svgName

	^$T! !

!FormGlyph methodsFor: 'copying' stamp: 'jmv 5/11/2019 21:32:14'!
postCopy
	form _ form copy! !

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

!TTContourConstruction methodsFor: 'printing'!
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: points size;
		"space;
		print: self type;"
		nextPut:$)! !

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

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/18/2021 10:11:13'!
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
		controlPoints add: cp - prevEndPoint.
		prevEndPoint _ ep ].
	^ PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 5/10/2022 13:17:16'!
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
		"If previousPoint and point are equal, we'd be adding a null Bezier with same start, end and control.
		This, besides being being useless, is dangerous.
		Rasterization code uses hop length for normalization, and this case would result in a division by zero,
		possibly crashing the system."
		point = previousPoint ifFalse: [
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
							aBlock value: point value: point ]]].
		previousPoint _ point.
		previousPointWasControlPoint _ pointIsControlPoint.
	].
	(previousPoint = firstEndPoint) ifFalse: [
		previousPointWasControlPoint
			ifTrue: [
				aBlock value: firstEndPoint value: previousPoint ]
			ifFalse: [
				aBlock value: firstEndPoint value: firstEndPoint ]]! !

!TTContourConstruction methodsFor: 'primitive format' stamp: 'jmv 5/10/2022 13:12:20'!
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
		"Make control point distict from start and end points for the benefit of VectorEnginePlugin"
		(cpr = `0.0@0.0` or: [ cpr = epr ]) ifTrue: [
			cpr _ epr * 0.5 ].
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

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 5/12/2022 09:31:45'!
appendGlyphDataTo: stream glyphsArray: glyphsArray indexes: indexes base: baseIndex	 bar: aProgressBarOrNil unknown: theUnknownGlyph

		glyphsArray withIndexDo: [ :glyphOrArrayOrNil :index |
			aProgressBarOrNil ifNotNil: [ :bar | bar value: index ].
			glyphOrArrayOrNil ifNotNil: [
				glyphOrArrayOrNil == theUnknownGlyph
					ifTrue: [ indexes at: baseIndex + index put: 1 ]
					ifFalse: [
						glyphOrArrayOrNil isArray
							ifTrue: [
								| nextBaseIndex |
								nextBaseIndex := indexes size - 128.
								indexes addAll: (IntegerArray new: 64).
								indexes at: baseIndex + index put: nextBaseIndex negated.
								self appendGlyphDataTo: stream glyphsArray: glyphOrArrayOrNil indexes: indexes
									base: nextBaseIndex bar: nil unknown: theUnknownGlyph ]
							ifFalse: [
								indexes at: baseIndex + index put: stream position+1.
								glyphOrArrayOrNil addGlyphDataTo: stream tx: nil ]]]].! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 5/12/2022 09:46:33'!
glyphsByIso8859s15: array8 glyphsByUtf8Bytes: arrayUtf8 unknownGlyph: theUnknownGlyph scaleArrowGlyphs: arrowGlyphs

	| mGlyph glyph tx |
	
	mGlyph _ array8 at: $M numericValue + 1.
	letterMTopSideBearing _ (mGlyph ifNil: [theUnknownGlyph]) topSideBearing.

	contourDataIndexesByIso8859s15 _ IntegerArray new: array8 size.
	contourDataForIso8859s15 _ Float32Array streamContents: [ :stream |
		theUnknownGlyph addGlyphDataTo: stream tx: nil. 	"First data is for unknownGlyph"
		array8 withIndexDo: [ :ttGlyphOrNil :index |
			(ttGlyphOrNil isNil or: [ttGlyphOrNil == theUnknownGlyph])
				ifTrue: [ contourDataIndexesByIso8859s15 at: index put: 1]
				ifFalse: [
					glyph _ ttGlyphOrNil.
					tx _ nil.
					ttGlyphOrNil isNumber ifTrue: [
						glyph _ arrowGlyphs at: ttGlyphOrNil.
						tx _ AffineTransformation withScale: letterMTopSideBearing asFloat / arrowGlyphs second topSideBearing].
					contourDataIndexesByIso8859s15 at: index put: stream position+1.
					glyph addGlyphDataTo: stream tx: tx ]]].
	self perform: (Preferences at: #assignmentGlyphSelector).

	"This takes a significant amout of space."
	(Preferences at: #loadOnlyLatinGlyphData) ifFalse: [
		contourDataIndexesByUtf8 _ (IntegerArray new: 256) as: OrderedCollection.
		contourDataForUtf8 _ Float32Array streamContents: [ :stream |
			theUnknownGlyph addGlyphDataTo: stream tx: nil.		"First data is for unknownGlyph"	
			self fullName, ' - Processing glyph data' 
				displayProgressAt: 100@100
				from: 1 
				to: arrayUtf8 size 
				during: [ :bar |
					self appendGlyphDataTo: stream glyphsArray: arrayUtf8 indexes: contourDataIndexesByUtf8
						base: 0 bar: bar unknown: theUnknownGlyph ]].
		contourDataIndexesByUtf8 _ contourDataIndexesByUtf8 as: IntegerArray].! !

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

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 3/26/2021 10:10:38'!
useAlwaysLeftArrow
	"Not only when Smalltalk glyphs requested."
	| arrowIndex |
	arrowIndex _ contourDataIndexesByIso8859s15 at: Character arrowLeft numericValue + 1.
	arrowIndex = 1 ifFalse: [	"If font doesn't include the arrow, don't do substitution."
		contourDataIndexesByIso8859s15 at: $_ numericValue + 1 put: arrowIndex ].! !

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 3/26/2021 10:10:06'!
useLeftArrow
	"When Smalltalk glyphs requested.
	Nothing to do, here."! !

!TTFontDescription methodsFor: 'testing' stamp: 'jmv 5/12/2022 10:34:30'!
includesAscii
	"Answer true if we include the basic ASCII characters.
	If not, we are not suitable, for instance, for displaying Smalltalk code."

	^(33 to: 127) allSatisfy: [ :i | (contourDataIndexesByUtf8  at: i) > 1 ].! !

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

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 4/21/2022 13:36:45'!
formGlyphFor: aCharacter useST80Glyphs: aBoolean pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i iso8859s15Code |

	iso8859s15Code _ aCharacter iso8859s15Code.
	(aBoolean and: [`{$_ . $^}` statePointsTo: aCharacter]) ifTrue: [
		aCharacter = $_ ifTrue: [ iso8859s15Code _ Character arrowLeft iso8859s15Code ].
		aCharacter = $^ ifTrue: [ iso8859s15Code _ Character arrowUp iso8859s15Code ]].
	i _ contourDataIndexesByIso8859s15 at: iso8859s15Code + 1.
	
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
	canvas _ VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			displayString: (String with: aCharacter)
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

!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 6/15/2022 11:01:49'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize useST80Glyphs: aBoolean
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i |

	byte1 _ aByte1.
	byte2OrNil _ aByte2OrNil.
	byte3OrNil _ aByte3OrNil.
	byte4OrNil _ aByte4OrNil.
	aBoolean ifTrue: [
		byte1 = `$_ numericValue` ifTrue: [ byte1 _ 226. byte2OrNil _ 134. byte3OrNil _ 144 ].
		byte1 = `$^numericValue` ifTrue: [ byte1 _ 226. byte2OrNil _ 134. byte3OrNil _ 145 ]].
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

	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i _ 1 ].

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
	canvas _ VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			displayUtf8: utf8ByteArray
			fromByte: 1
			toByte: utf8ByteArray size
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

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 5/4/2022 12:42:10'!
widthOf: aCharacter useST80Glyphs: aBoolean
	"Double dispatch"

	^aCharacter widthIn: self useST80Glyphs: aBoolean.! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 5/4/2022 12:35:25'!
widthOfCharacter: aCharacter useST80Glyphs: aBoolean

	| i iso8859s15Code advanceWidth |
	iso8859s15Code _ aCharacter iso8859s15Code.
	aBoolean ifTrue: [
		iso8859s15Code = `$_ numericValue` ifTrue: [ iso8859s15Code _ `Character arrowLeft iso8859s15Code`].
		iso8859s15Code = `$^ numericValue` ifTrue: [ iso8859s15Code _ `Character arrowUp iso8859s15Code` ]].
	i _ contourDataIndexesByIso8859s15 at: iso8859s15Code + 1.
	advanceWidth _ contourDataForIso8859s15 at: i.
	^ advanceWidth / letterMTopSideBearing! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 5/5/2022 12:07:41'!
widthOfCodePoint: aNumber useST80Glyphs: aBoolean

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil useST80Glyphs: aBoolean ]
		withUtf8BytesOfUnicodeCodePoint: aNumber.! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 5/9/2022 15:32:27'!
widthOfUtf8Byte1: aByte1 b2: aByte2OrNil b3: aByte3OrNil b4: aByte4OrNil useST80Glyphs: aBoolean

	| byte1 byte2 byte3 byte4 i advanceWidth |
	byte1 _ aByte1.
	byte2 _ aByte2OrNil.
	byte3 _ aByte3OrNil.
	byte4 _ aByte4OrNil.
	aBoolean ifTrue: [ "Assumes that $_ and $^are ASCII, and use just one byte"
		byte1 = `$_ codePoint` ifTrue: [
			"(String with: Character arrowLeft) asUtf8"
			byte1 _ 226. byte2 _ 134. byte3 _ 144 ].
		byte1 = `$^ codePoint` ifTrue: [
			"(String with: Character arrowUp) asUtf8"
			byte1 _ 226. byte2 _ 134. byte3 _ 145 ]].
	i _ contourDataIndexesByUtf8 at: byte1 + 1.
	byte2 notNil ifTrue: [
		i _ contourDataIndexesByUtf8 at: i negated + byte2 + 1.
		byte3 notNil ifTrue: [
			i _ contourDataIndexesByUtf8 at: i negated + byte3 + 1.
			byte4 notNil ifTrue: [
				i _ contourDataIndexesByUtf8 at: i negated + byte4 + 1 ]]].
	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i _ 1 ].
	advanceWidth _ contourDataForUtf8 at: i.
	^ advanceWidth / letterMTopSideBearing.! !

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

!TTFontDescription class methodsFor: 'class initialization' stamp: 'jmv 5/18/2021 10:11:16'!
initialize
	"
	TTFontDescription initialize
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/12/2022 14:47:22'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByIso5559s15 glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes carriageReturn |
	unknownGlyph _ glyphs first.
	nullGlyph _ glyphs second.
	mapSize _ characterMap size.
	glyphsByIso5559s15 _ Array new: 256.
	glyphsByUTF8 _ Array new: 256.

	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes _ #(8592 8593 8594 8595 8617) collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs _ arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	mapSize > 8617
		ifTrue: [
			carriageReturn _ glyphs at: (characterMap at: 8617+1)+1.
			carriageReturn == unknownGlyph ifTrue: [ carriageReturn _ FallbackArrowGlyphs at: 5 ]]
		ifFalse: [ carriageReturn _ FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint _ codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint _ 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint _ 16r2217 ]. "CENTERED ASTERISK"
		g _ glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g _ glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		(Character iso8859s15CodeForUnicodeCodePoint: codePoint) ifNotNil: [ :iso8859s15 |
			g == unknownGlyph
				ifFalse: [
					glyphsByIso5559s15 at: iso8859s15 + 1 put: g ]
				ifTrue: [				"Tell that we need an alternative arrow glyph"
					(FallbackArrowGlyphs notNil and: [ codePoint between: 8592 and: 8595 ]) ifTrue: [
						glyphsByIso5559s15 at: iso8859s15 + 1 put: codePoint - 8591 ]]].

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
	glyphsByIso5559s15 at: 13 +1 put: carriageReturn.
	glyphsByUTF8 at: 13 +1 put: carriageReturn.
	fontDescription
		glyphsByIso8859s15: glyphsByIso5559s15 glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: FallbackArrowGlyphs! !

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/9/2022 12:00:25'!
processCharacterMappingTable: entry
	"Read the font's character to glyph index mapping table.
	If an appropriate mapping can be found then return an association
	with the format identifier and the contents of the table"
	| initialOffset nSubTables platformID platformSpecificEncodingID offset offsets platformIDsAndEncodings orderOfPreference found |
	initialOffset := entry offset.
	entry skip: 2. "Skip table version"
	nSubTables := entry nextUShort.
	platformIDsAndEncodings _ Array new: nSubTables.
	offsets _ Array new: nSubTables.
	1 to: nSubTables do: [ :i |
		platformID := entry nextUShort.
		platformSpecificEncodingID := entry nextUShort.
		offset := entry nextULong.
		platformIDsAndEncodings at: i put: {platformID. platformSpecificEncodingID}.
		offsets  at: i put: offset  ].
	"see, for instance, https://docs.microsoft.com/en-us/typography/opentype/spec/cmap"
	orderOfPreference _ (Preferences at: #loadOnlyLatinGlyphData)
		ifTrue: [  				"Prefer restricted range, to save some memory and time"
			#(
				#(0 3)  			"platform=Unicode, encoding=3 (Unicode, BMP only)"
				#(3 1)  			"platform=Windows, encoding=1 (Unicode, BMP only)"
				#(0 4)  			"platform=Unicode, encoding=4 (Unicode, full range)"
				#(0 10) 			"platform=Unicode, encoding=10 (Undocumented but widely used. Appears equivalent to #(0 4) or #(3 10))"
				#(3 10)  			"platform=Windows, encoding=10(Unicode, full range), format=12(Segment coverage)"
				#(0 0)  			"platform=Unicode, encoding=3(Unicode 1.0, deprecated)"
			) ]
		ifFalse: [  				"Prefer full range"
			#(
				#(0 4)  			"platform=Unicode, encoding=4 (Unicode, full range)"
				#(0 10) 			"platform=Unicode, encoding=10 (Undocumented but widely used. Appears equivalent to #(0 4) or #(3 10))"
				#(3 10)  			"platform=Windows, encoding=10(Unicode, full range), format=12(Segment coverage)"
				#(0 3)  			"platform=Unicode, encoding=3 (Unicode, BMP only)"
				#(3 1)  			"platform=Windows, encoding=1 (Unicode, BMP only)"
				#(0 0)  			"platform=Unicode, encoding=3(Unicode 1.0, deprecated)"
			) ].
	orderOfPreference do: [ :preferredPlatformAndEncoding |
		found _ platformIDsAndEncodings indexOf: preferredPlatformAndEncoding.
		found = 0 ifFalse: [
			"Go to the beginning of the table"
			offset _ offsets at: found.
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/9/2022 12:02:09'!
processGlyphDataTable: entry offsets: offsetArray
	"Read the actual glyph data from the font.
	offsetArray contains the start offsets in the data for each glyph."
	| initialOffset |
	initialOffset _ entry offset.
	glyphs _ Array new: nGlyphs.
	fontDescription fullName, ' - Reading glyph data' 
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/12/2022 17:31:51'!
processHorizontalMetricsTable: entry length: numHMetrics
	| lastAW glyph |
	1
		to: numHMetrics
		do: [ :i |
			glyph _ glyphs at: i.
			glyph advanceWidth: entry nextUShort.
			glyph leftSideBearing: entry nextShort.
			glyph updateRightSideBearing ].
	numHMetrics = nGlyphs ifTrue: [ ^ true ].
	lastAW _ (glyphs at: numHMetrics) advanceWidth.
	numHMetrics + 1
		to: nGlyphs
		do: [ :i |
			glyph _ glyphs at: i.
			glyph advanceWidth: lastAW.
			glyph leftSideBearing: entry nextShort.
			glyph updateRightSideBearing ].! !

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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/18/2021 10:11:41'!
processKerningTable: entry
	"Extract the kerning information for pairs of glyphs."
	| version nTables covLow covHigh |
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

!TTFontReader methodsFor: 'processing' stamp: 'jmv 5/18/2021 10:11:59'!
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
	(glyphEntry := self getTableDirEntry: 'glyf' from: fontData offset: offset) == nil ifTrue: [
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

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/9/2022 11:35:25'!
decodeCmapFmtTable: entry

	| cmapFmt |
	cmapFmt := entry nextUShort.
	cmapFmt = 0 ifTrue: [ "byte encoded table"
		^self decodeCmapFmtTableFormat0: entry ].
	cmapFmt = 4 ifTrue: [ "segment mapping to deltavalues"
		^ self decodeCmapFmtTableFormat4: entry ].
	cmapFmt = 6 ifTrue: [ "trimmed table"
		^self decodeCmapFmtTableFormat6: entry ].
	cmapFmt = 10 ifTrue: [ "trimmed array"
		^self decodeCmapFmtTableFormat10: entry ].
	cmapFmt = 12 ifTrue: [ "segmented coverage"
		^ self decodeCmapFmtTableFormat12: entry ].
	^ nil! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/8/2022 21:45:09'!
decodeCmapFmtTableFormat0: entry
	"byte encoded table"

	| length cmap |
	length := entry nextUShort.
	entry skip: 2. "skip version"
	length := length - 6. 		"should be always 256"
	length <= 0 ifTrue: [ ^ nil ].	"but sometimes, this table is empty"
	cmap := WordArray new: length.
	entry nextBytes: length into: cmap startingAt: entry offset.
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/9/2022 11:38:25'!
decodeCmapFmtTableFormat10: entry
	"trimmed array"

	| length entryCount cmap firstCode |
	entry skip: 2. "skip reserved"
	length := entry nextULong.
	entry skip: 4. "skip language"
	firstCode := entry nextULong.
	entryCount := entry nextULong.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUShort ].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/9/2022 11:02:16'!
decodeCmapFmtTableFormat12: entry
	"segmented coverage"

	| length cmap numGroups |
	entry skip: 2. "reserved"
	length := entry nextULong.
	entry skip: 4. "language"
	numGroups := entry nextULong.
	cmap _ WordArray new.
	numGroups timesRepeat: [ | endCharCode prev startCharCode startGlyphId |
		startCharCode := entry nextULong.
		endCharCode := entry nextULong.
		startGlyphId := entry nextULong.
		"
{startCharCode. endCharCode. startGlyphId} print.
"
"de ahi para arriba es banana
http://www.alanwood.net/unicode/unicode_samples.html
"
endCharCode < 16rE0000 ifTrue: [
		prev _ cmap.
		cmap _ WordArray new: endCharCode+1.
		cmap replaceFrom: 1 to: prev size with: prev. 
		startCharCode+1 to: endCharCode+1 do: [ :i |
			cmap at: i put: startGlyphId.
			startGlyphId _ startGlyphId + 1 ]]
].
"
self halt.
"
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'DF 5/12/2022 17:18:37'!
decodeCmapFmtTableFormat4: entry
	"segment mapping to deltavalues"

	| length entryCount segCount segments offset cmap |
	length := entry nextUShort.
	entry skip: 2. "skip version"
	segCount := entry nextUShort // 2.
	entry skip: 6. "skip searchRange, entrySelector, rangeShift"
	segments := Array new: segCount.
	segments := (1 to: segCount) collect: [ :e | Array new: 4 ].
	1 to: segCount do: [ :i | (segments at: i) at: 2 put: entry nextUShort ]. "endCount"
	entry skip: 2. "skip reservedPad"
	1 to: segCount do: [ :i | (segments at: i) at: 1 put: entry nextUShort]. "startCount"
	1 to: segCount do: [ :i | (segments at: i) at: 3 put: entry nextShort ]. "idDelta"
	offset := entry offset.
	1 to: segCount do: [ :i | (segments at: i) at: 4 put: entry nextUShort ]. "idRangeOffset"
	"do not process the end-of-search segment"
	segments _ segments reject:[:each | (each at:1) = 0xFFFF and:[(each at:2) = 0xFFFF] ].
	entryCount := segments inject: 0 into: [ :max :seg | max max: seg second ].
	cmap := WordArray new: entryCount+1.
	segments withIndexDo: [ :seg :si | | code |
		seg first to: seg second do: [ :i |
			seg last > 0 ifTrue: ["offset to glypthIdArray - this is really C-magic!!"
				entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 
				code := entry nextUShort.
				code > 0 ifTrue: [code := code + seg third ]]
			ifFalse: ["simple offset"
				code := i + seg third ].
			cmap at: i + 1 put: code ]].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/8/2022 21:46:18'!
decodeCmapFmtTableFormat6: entry
	"trimmed table"

	| length entryCount cmap firstCode |
	length := entry nextUShort.
	entry skip: 2. "skip version"
	firstCode := entry nextUShort.
	entryCount := entry nextUShort.
	cmap := WordArray new: entryCount + firstCode.
	entryCount timesRepeat: [
		cmap at: (firstCode := firstCode + 1) put: entry nextUShort ].
	^ cmap! !

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/18/2021 10:11:26'!
getGlyphFlagsFrom: entry size: nPts
	"Read in the flags for this glyph. The outer loop gathers the flags that
	are actually contained in the table. If the repeat bit is set in a flag
	then the next byte is read from the table; this is the number of times
	to repeat the last flag. The inner loop does this, incrementing the
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

!TTFontReader methodsFor: 'private' stamp: 'jmv 5/18/2021 10:12:09'!
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
			"First we check to see if bit one is set. This would indicate that
			the corresponding coordinate data in the table is 1 byte long.
			If the bit is not set, then the coordinate data is 2 bytes long."
			xValue _ (flagBits allMask: 2)
				ifTrue: [ "one byte"
					(flagBits allMask: 16)
						ifTrue: [entry nextByte]
						ifFalse: [entry nextByte negated].
				] ifFalse: [ "two byte"
					"If bit four is set, then this coordinate is the same as the
					last one, so the relative offset (of zero) is stored. If bit
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

!TTGlyph methodsFor: 'printing' stamp: 'tk 9/13/1999 09:54'!
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: (contours ifNil: [0] ifNotNil: [contours size]);
		nextPut:$).! !

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

!TTGlyph methodsFor: 'drawing' stamp: 'jmv 5/10/2021 12:49:00'!
drawGlyphOn: aVectorCanvas geometryTransformation: txOrNil
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			aVectorCanvas moveTo: `0.0@0.0`.
			(contour initialMoveToCommand: txOrNil) runOn: aVectorCanvas .
			(contour pathCommand: txOrNil) runOn: aVectorCanvas ]].! !

!TTGlyph methodsFor: 'primitive format' stamp: 'jmv 3/3/2020 09:46:30'!
addContourDataTo: stream tx: aGeometryTransformationOrNil

	contours do: [ :c | c addContourDataTo: stream tx: aGeometryTransformationOrNil ]! !

!TTGlyph methodsFor: 'primitive format' stamp: 'jmv 4/28/2021 10:36:49'!
addGlyphDataTo: stream tx: aGeometryTransformationOrNil
	| aw rb tl |
	aw _ self advanceWidth.
	tl _ self boundsLeft@self boundsTop.
	rb _ self boundsRight@self boundsBottom.
	aGeometryTransformationOrNil ifNotNil: [
		aw _ aGeometryTransformationOrNil externalizeScalar: aw.
		tl _ aGeometryTransformationOrNil externalizePosition: tl.
		rb _ aGeometryTransformationOrNil externalizePosition: rb.
		].
	stream nextPut: aw.
	stream nextPut: tl x.
	stream nextPut: rb x.
	stream nextPut: tl y.
	stream nextPut: rb y.
	stream nextPut: self contourCount.
	self addContourDataTo: stream tx: aGeometryTransformationOrNil! !

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

!Character methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 5/4/2022 12:45:53'!
widthIn: aTTFontDescription useST80Glyphs: aBoolean
	"Double dispatch"

	^ aTTFontDescription widthOfCharacter: self useST80Glyphs: aBoolean! !

!String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/2/2022 17:00:10'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	^ engine
		displayString: self
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/2/2022 17:00:16'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"
	^ engine 
		displayString: self
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		useST80Glyphs: aTrueTypeFont isST80Glyphs
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true.! !

!UnicodeCodePoint methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 5/5/2022 12:04:16'!
widthIn: aTTFontDescription useST80Glyphs: aBoolean
	"Double dispatch"

	^ aTTFontDescription widthOfCodePoint: value useST80Glyphs: aBoolean! !

!Utf32String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/5/2022 10:49:46'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	^engine
		displayUtf32: codePoints
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!Utf32String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/5/2022 10:49:52'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	^engine
		displayUtf32: codePoints
		from: startIndex to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		useST80Glyphs: aTrueTypeFont isST80Glyphs
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true
! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/12/2022 10:21:19'!
demoAncientEgyptian
	"
	Utf32String demoAncientEgyptian.
	"
	| utf32 fontFamily |
	utf32 _ #(77824 78200 78400 78894 77840 77841 77842 32 78064 78065 78066 78067 78068 78069 78070 32 77952 32 78200 78201 78202 78203 32 78205 32 78206 78207 32 78432 78433 78434 78435 78436 32 78258 77943 78820 78751 77824 77904 78821 78016 78358 78641 78163 78747 78193 77844 78358 78641 78163 78747 78283 78283 78799 77904 78861 77963 78580 77981 78747 78569 78420 78506 78811 78580 78880 78143 77963 78799 78615 77825 78647 77981 78799 78829 78811 78334 78799 78163 78193 77824 77904 78826 78016 78626 78626 78358 78358 78811) asWordArray.

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 60))
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:30:14'!
demoArabic
	"
	Utf32String demoArabic
	"
	| utf32 |
	utf32 _ #(1603 1604 32 1571 1608 1587 1593 32 1575 1604 1578 1606 1575 1586 1604 1610 32 1601 1589 1604 44 32 1605 1593 32 1593 1605 1604 1610 1577 32 1575 1604 1588 1585 1602 1610 32 1575 1606 1578 1576 1575 1607 32 1610 1603 1606 44 32 1593 1604 32 1576 1581 1602 32 1579 1605 1617 1577 32 1608 1581 1604 1601 1575 1572 1607 1575 46 32 1605 1593 32 1590 1585 1576 32 1583 1601 1617 1577 32 1588 1585 1587 1577 32 1575 1604 1589 1601 1581 1575 1578 44 32 1605 1587 1572 1608 1604 1610 1577 32 1575 1604 1585 1576 1610 1593 1548 32 1604 1576 1608 1604 1606 1583 1575 1548 32 1608 1578 1605 32 1602 1583 46 32 1582 1591 1617 1577 32 1604 1605 1581 1575 1603 1605 32 1608 1589 1604 32 1601 1610 44 32 1608 1601 1606 1604 1606 1583 1575 32 1575 1604 1573 1606 1586 1575 1604 32 1575 1604 1605 1578 1575 1582 1605 1577 32 1593 1606 32 1581 1610 1579 46 32 1575 1604 1609 32 1607 1575 1605 1588 32 1575 1604 1610 1575 1576 1575 1606 32 1604 1605 46 32 1578 1591 1608 1610 1585 32 1601 1607 1585 1587 1578 32 1604 1605 32 1601 1593 1604 46 32 1583 1606 1608 32 1575 1604 1593 1587 1603 1585 1610 32 1608 1605 1591 1575 1604 1576 1577 32 1576 1604 44 32 1605 1604 1610 1608 1606 32 1608 1576 1575 1587 1578 1579 1606 1575 1569 32 1605 1593 32 1583 1608 1606 44 32 1605 1606 32 1605 1603 1606 32 1573 1576 1617 1575 1606 32 1575 1604 1580 1606 1608 1576 1610 46 32 1576 1600 32 1605 1575 1584 1575 32 1571 1608 1585 1575 1602 1607 1605 32 1581 1610 1579 44 32 1603 1575 1606 32 1578 1605 32 1587 1575 1576 1602 32 1605 1603 1617 1606 32 1575 1604 1587 1610 1569 44 32 1571 1587 1585 32 1601 1607 1585 1587 1578 32 1575 1604 1587 1575 1583 1587 32 1575 1604 1573 1581 1578 1601 1575 1592 32 1608 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Arabic'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:30:01'!
demoArmenian
	"
	Utf32String demoArmenian
	"
	| utf32 |
	utf32 _ #(1388 1400 1404 1381 1396 32 1387 1402 1405 1400 1410 1396 32 1380 1400 1388 1400 1404 32 1405 1387 1385 32 1377 1396 1381 1385 44 32 1412 1400 1410 1377 1381 1412 1400 1410 1381 32 1381 1400 1410 1387 1405 1396 1400 1380 32 1404 1381 1402 1400 1410 1380 1387 1377 1404 1381 32 1400 1410 1385 32 1405 1387 1385 46 32 1406 1381 1404 1400 32 1404 1381 1414 1400 1404 1396 1387 1380 1377 1398 1405 32 1390 1400 1410 32 1405 1387 1385 44 32 1380 1381 1378 1381 1385 32 1402 1381 1404 1414 1381 1390 1385 1400 32 1405 1377 1388 1400 1410 1385 1377 1385 1400 1410 1405 32 1381 1385 32 1390 1400 1410 1396 44 32 1392 1377 1405 32 1385 1404 1387 1385 1377 1398 1387 32 1398 1400 1398 1400 1410 1396 1381 1405 32 1398 1381 46 32 1396 1381 1387 32 1398 1400 32 1405 1400 1388 1400 1410 1385 1377 32 1388 1377 1378 1400 1404 1381 1405 32 1400 1396 1387 1385 1385 1377 1396 44 32 1381 1389 32 1379 1404 1377 1381 1390 1400 32 1380 1381 1414 1387 1398 1387 1385 1387 1400 1398 1381 1405 32 1392 1377 1405 44 32 1381 1377 32 1396 1381 1388 32 1412 1400 1410 1387 1405 32 1398 1387 1405 1388 32 1377 1405 1405 1400 1410 1396 46 32 1387 1398 32 1377 1396 1381 1385 32 1398 1381 1396 1400 1404 1381 32 1400 1410 1405 1400 1410 46 32 1381 1404 1377 1385 32 1387 1400 1410 1406 1377 1404 1381 1385 32 1381 1377 32 1405 1381 1380 44 32 1392 1377 1405 32 1377 1388 1387 1377 32 1412 1400 1410 1377 1381 1412 1400 1410 1381 32 1390 1400 1404 1402 1400 1404 1377 32 1377 1380 46 32 1381 1400 1405 32 1414 1377 1405 1385 1387 1380 1387 1387 32 1380 1381 1414 1387 1398 1387 1385 1387 1400 1398 1381 1396 32 1398 1381 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Armenian'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/12/2022 10:22:54'!
demoChinese
	"
	Utf32String demoChinese
	"
	| utf32 fontFamily |
	utf32 _ #(38899 21147 39208 20998 39423 22243 26131 21402 22909 25943 28988 38283 24247 20870 12290 25925 23567 22679 38754 25991 27211 36578 21839 38272 20250 29572 38283 21435 25152 27491 24819 35441 26469 12290 24180 20129 22971 23384 36523 25552 36895 20140 24235 26178 20028 27770 33021 12290 21496 23455 20869 23652 36074 21475 36002 27714 20844 38745 27841 21046 24341 26178 22343 39640 26032 12290 30906 26399 25968 38598 26408 35611 27490 27839 20633 36942 22269 36092 27178 21916 21047 24247 25104 23546 12290 35352 20998 21442 21628 23798 22269 35488 27177 22238 27491 24180 36557 27096 20170 20778 25991 31105 21542 12290 25972 24739 36259 31038 26126 26908 30058 26524 25512 31893 19990 26412 23567 28961 31216 30010 12290 25237 26684 21307 24460 20219 31995 36578 25919 26465 35441 25522 26376 29699 12290 24724 37240 35211 22825 20889 22793 21442 38520 22235 21517 21270 33495 21320 27835 38555 20303 35207 20024 25945 30028) asWordArray.

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'KiwiMaru'.
	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 24))
		editLabel: 'Chinese'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:28:33'!
demoCyrillic
	"
	Utf32String demoCyrillic
	"
	| utf32 |
	utf32 _ #(1051 1086 1088 1077 1084 32 1080 1087 1089 1091 1084 32 1076 1086 1083 1086 1088 32 1089 1080 1090 32 1072 1084 1077 1090 44 32 1076 1077 1073 1077 1090 32 1089 1072 1083 1091 1090 1072 1085 1076 1080 32 1084 1077 1080 32 1077 1080 46 32 1040 1085 32 1093 1080 1089 32 1075 1088 1072 1077 1094 1077 32 1091 1090 1072 1084 1091 1088 32 1076 1086 1083 1086 1088 1091 1084 44 32 1083 1086 1088 1077 1084 32 1087 1088 1080 1085 1094 1080 1087 1077 1089 32 1084 1086 1083 1077 1089 1090 1080 1072 1077 32 1076 1091 1086 32 1080 1085 46 32 1045 1080 32 1084 1086 1074 1077 1090 32 1085 1086 1089 1090 1088 1086 32 1086 1087 1090 1080 1086 1085 32 1085 1072 1084 44 32 1077 1086 1089 32 1077 1087 1080 1094 1091 1088 1080 32 1086 1084 1080 1090 1090 1072 1085 1090 1091 1088 32 1091 1090 46 32 1059 1090 32 1084 1091 1085 1077 1088 1077 32 1083 1072 1073 1086 1088 1077 32 1080 1085 1090 1077 1083 1083 1077 1075 1077 1073 1072 1090 32 1091 1089 1091 44 32 1077 1072 32 1089 1077 1076 32 1076 1077 1073 1077 1090 32 1083 1091 1087 1090 1072 1090 1091 1084 44 32 1074 1080 1076 1080 1090 32 1080 1083 1083 1091 1076 32 1080 1091 1076 1080 1094 1086 32 1103 1091 1086 32 1091 1090 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Cyrillic'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:28:18'!
demoGeorgian
	"
	Utf32String demoGeorgian
	"
	| utf32 |
	utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel:'Georgian'.! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:28:03'!
demoGreek
	"
	Utf32String demoGreek
	"
	| utf32 |
	utf32 _ #(923 959 961 949 956 32 953 960 963 952 956 32 948 959 955 959 961 32 963 953 964 32 945 956 949 964 44 32 956 959 948 959 32 118 949 957 953 945 956 32 118 953 948 949 961 949 961 32 953 957 32 99 952 956 44 32 949 958 32 945 952 964 949 956 32 956 945 955 953 963 32 959 966 966 949 957 948 953 964 32 949 963 964 46 32 921 948 32 964 959 955 955 953 964 32 963 952 963 99 953 960 953 964 32 963 945 955 952 964 945 964 952 963 32 113 952 953 44 32 99 952 32 113 952 945 957 948 959 32 960 961 959 960 961 953 945 949 32 953 956 960 949 961 948 953 949 964 32 118 949 955 44 32 956 949 945 32 949 964 32 949 961 961 949 956 32 947 955 959 961 953 945 964 952 961 46 32 917 958 949 961 99 953 32 948 949 966 953 957 953 964 953 959 957 949 956 32 956 949 955 32 949 945 46 32 917 958 32 963 949 945 32 949 955 953 964 32 948 953 963 963 949 957 964 953 949 964 44 32 956 945 955 953 963 32 964 961 953 964 945 957 953 32 118 953 963 32 953 948 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Greek'! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:27:53'!
demoHebrew
	"
	Utf32String demoHebrew
	"
	| utf32 |
	utf32 _ #(1506 1500 32 1502 1488 1502 1512 32 1494 1511 1493 1511 32 1513 1491 1512 1493 1514 32 1488 1512 1509 44 32 1492 1502 1500 1510 1514 32 1502 1491 1493 1497 1511 1497 1501 32 1513 1499 1500 32 1513 1500 44 32 1506 1497 1510 1493 1489 32 1504 1497 1493 1493 1496 32 1513 1497 1514 1493 1508 1497 1514 32 1506 1493 1491 32 1489 46 32 1505 1491 1512 32 1488 1493 32 1512 1508 1493 1488 1492 32 1513 1497 1502 1493 1513 1497 32 1502 1493 1506 1502 1491 1497 1501 46 32 1492 1497 1488 32 1508 1497 1505 1497 1511 1492 32 1492 1490 1512 1508 1497 1501 32 1513 1497 1502 1493 1513 1497 1497 1501 32 1513 1500 46 32 1513 1499 1500 32 1506 1500 32 1506 1494 1512 1492 32 1500 1492 1508 1493 1498 32 1488 1493 1493 1497 1512 1493 1504 1488 1493 1496 1497 1511 1492 44 32 1488 1504 1488 32 1488 1514 32 1506 1494 1512 1492 32 1500 1502 1504 1493 1506 32 1500 1502 1495 1497 1511 1492 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Hebrew'! !

!Utf32String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 5/11/2022 10:27:42'!
demoLatin
	"
	Utf32String demoLatin
	"
	| utf32 |
	utf32 _ #(76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46) asWordArray.

	(Text string: (Utf32String withWords: utf32) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Latin Alphabet'! !

!Utf8String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/5/2022 16:58:20'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex
		toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!Utf8String methodsFor: '*VectorGraphics-displaying' stamp: 'jmv 5/5/2022 16:58:23'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		useST80Glyphs: aTrueTypeFont isST80Glyphs
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true
! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:28:47'!
demoAncientEgyptian
	"
	Utf8String demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 _  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 60))
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:28:53'!
demoArabic
	"
	Utf8String demoArabic
	"
	| utf8 |
	utf8 _  #[217 131 217 132 32 216 163 217 136 216 179 216 185 32 216 167 217 132 216 170 217 134 216 167 216 178 217 132 217 138 32 217 129 216 181 217 132 44 32 217 133 216 185 32 216 185 217 133 217 132 217 138 216 169 32 216 167 217 132 216 180 216 177 217 130 217 138 32 216 167 217 134 216 170 216 168 216 167 217 135 32 217 138 217 131 217 134 44 32 216 185 217 132 32 216 168 216 173 217 130 32 216 171 217 133 217 145 216 169 32 217 136 216 173 217 132 217 129 216 167 216 164 217 135 216 167 46 32 217 133 216 185 32 216 182 216 177 216 168 32 216 175 217 129 217 145 216 169 32 216 180 216 177 216 179 216 169 32 216 167 217 132 216 181 217 129 216 173 216 167 216 170 44 32 217 133 216 179 216 164 217 136 217 132 217 138 216 169 32 216 167 217 132 216 177 216 168 217 138 216 185 216 140 32 217 132 216 168 217 136 217 132 217 134 216 175 216 167 216 140 32 217 136 216 170 217 133 32 217 130 216 175 46 32 216 174 216 183 217 145 216 169 32 217 132 217 133 216 173 216 167 217 131 217 133 32 217 136 216 181 217 132 32 217 129 217 138 44 32 217 136 217 129 217 134 217 132 217 134 216 175 216 167 32 216 167 217 132 216 165 217 134 216 178 216 167 217 132 32 216 167 217 132 217 133 216 170 216 167 216 174 217 133 216 169 32 216 185 217 134 32 216 173 217 138 216 171 46 32 216 167 217 132 217 137 32 217 135 216 167 217 133 216 180 32 216 167 217 132 217 138 216 167 216 168 216 167 217 134 32 217 132 217 133 46 32 216 170 216 183 217 136 217 138 216 177 32 217 129 217 135 216 177 216 179 216 170 32 217 132 217 133 32 217 129 216 185 217 132 46 32 216 175 217 134 217 136 32 216 167 217 132 216 185 216 179 217 131 216 177 217 138 32 217 136 217 133 216 183 216 167 217 132 216 168 216 169 32 216 168 217 132 44 32 217 133 217 132 217 138 217 136 217 134 32 217 136 216 168 216 167 216 179 216 170 216 171 217 134 216 167 216 161 32 217 133 216 185 32 216 175 217 136 217 134 44 32 217 133 217 134 32 217 133 217 131 217 134 32 216 165 216 168 217 145 216 167 217 134 32 216 167 217 132 216 172 217 134 217 136 216 168 217 138 46 32 216 168 217 128 32 217 133 216 167 216 176 216 167 32 216 163 217 136 216 177 216 167 217 130 217 135 217 133 32 216 173 217 138 216 171 44 32 217 131 216 167 217 134 32 216 170 217 133 32 216 179 216 167 216 168 217 130 32 217 133 217 131 217 145 217 134 32 216 167 217 132 216 179 217 138 216 161 44 32 216 163 216 179 216 177 32 217 129 217 135 216 177 216 179 216 170 32 216 167 217 132 216 179 216 167 216 175 216 179 32 216 167 217 132 216 165 216 173 216 170 217 129 216 167 216 184 32 217 136 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) editLabel: 'Arabic'! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:28:59'!
demoArmenian
	"
	Utf8String demoArmenian
	"
	| utf8 |
	utf8 _  #[213 172 213 184 213 188 213 165 213 180 32 213 171 213 186 213 189 213 184 214 130 213 180 32 213 164 213 184 213 172 213 184 213 188 32 213 189 213 171 213 169 32 213 161 213 180 213 165 213 169 44 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 165 213 184 214 130 213 171 213 189 213 180 213 184 213 164 32 213 188 213 165 213 186 213 184 214 130 213 164 213 171 213 161 213 188 213 165 32 213 184 214 130 213 169 32 213 189 213 171 213 169 46 32 213 190 213 165 213 188 213 184 32 213 188 213 165 214 134 213 184 213 188 213 180 213 171 213 164 213 161 213 182 213 189 32 213 174 213 184 214 130 32 213 189 213 171 213 169 44 32 213 164 213 165 213 162 213 165 213 169 32 213 186 213 165 213 188 214 134 213 165 213 174 213 169 213 184 32 213 189 213 161 213 172 213 184 214 130 213 169 213 161 213 169 213 184 214 130 213 189 32 213 165 213 169 32 213 174 213 184 214 130 213 180 44 32 213 176 213 161 213 189 32 213 169 213 188 213 171 213 169 213 161 213 182 213 171 32 213 182 213 184 213 182 213 184 214 130 213 180 213 165 213 189 32 213 182 213 165 46 32 213 180 213 165 213 171 32 213 182 213 184 32 213 189 213 184 213 172 213 184 214 130 213 169 213 161 32 213 172 213 161 213 162 213 184 213 188 213 165 213 189 32 213 184 213 180 213 171 213 169 213 169 213 161 213 180 44 32 213 165 213 173 32 213 163 213 188 213 161 213 165 213 174 213 184 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 189 32 213 176 213 161 213 189 44 32 213 165 213 161 32 213 180 213 165 213 172 32 214 132 213 184 214 130 213 171 213 189 32 213 182 213 171 213 189 213 172 32 213 161 213 189 213 189 213 184 214 130 213 180 46 32 213 171 213 182 32 213 161 213 180 213 165 213 169 32 213 182 213 165 213 180 213 184 213 188 213 165 32 213 184 214 130 213 189 213 184 214 130 46 32 213 165 213 188 213 161 213 169 32 213 171 213 184 214 130 213 190 213 161 213 188 213 165 213 169 32 213 165 213 161 32 213 189 213 165 213 164 44 32 213 176 213 161 213 189 32 213 161 213 172 213 171 213 161 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 174 213 184 213 188 213 186 213 184 213 188 213 161 32 213 161 213 164 46 32 213 165 213 184 213 189 32 214 134 213 161 213 189 213 169 213 171 213 164 213 171 213 171 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 180 32 213 182 213 165 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) 
		editLabel: 'Armenian'.! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:29:05'!
demoChinese
	"
	Utf8String demoChinese
	"
	| utf8 fontFamily |
	utf8 _  #[233 159 179 229 138 155 233 164 168 229 136 134 233 167 191 229 155 163 230 152 147 229 142 154 229 165 189 230 149 151 231 132 188 233 150 139 229 186 183 229 134 134 227 128 130 230 149 133 229 176 143 229 162 151 233 157 162 230 150 135 230 169 139 232 187 162 229 149 143 233 150 128 228 188 154 231 142 132 233 150 139 229 142 187 230 137 128 230 173 163 230 131 179 232 169 177 230 157 165 227 128 130 229 185 180 228 186 161 229 166 187 229 173 152 232 186 171 230 143 144 233 128 159 228 186 172 229 186 171 230 153 130 228 184 188 230 177 186 232 131 189 227 128 130 229 143 184 229 174 159 229 134 133 229 177 164 232 179 170 229 143 163 232 178 162 230 177 130 229 133 172 233 157 153 230 179 129 229 136 182 229 188 149 230 153 130 229 157 135 233 171 152 230 150 176 227 128 130 231 162 186 230 156 159 230 149 176 233 155 134 230 156 168 232 172 155 230 173 162 230 178 191 229 130 153 233 129 142 229 155 189 232 179 188 230 168 170 229 150 156 229 136 183 229 186 183 230 136 144 229 175 186 227 128 130 232 168 152 229 136 134 229 143 130 229 145 188 229 179 182 229 155 189 232 170 160 230 168 169 229 155 158 230 173 163 229 185 180 232 187 141 230 167 152 228 187 138 229 132 170 230 150 135 231 166 129 229 144 166 227 128 130 230 149 180 230 130 163 232 182 163 231 164 190 230 152 142 230 164 156 231 149 170 230 158 156 230 142 168 231 178 149 228 184 150 230 156 172 229 176 143 231 132 161 231 167 176 231 148 186 227 128 130 230 138 149 230 160 188 229 140 187 229 190 140 228 187 187 231 179 187 232 187 162 230 148 191 230 157 161 232 169 177 230 142 178 230 156 136 231 144 131 227 128 130 230 130 148 233 133 184 232 166 139 229 164 169 229 134 153 229 164 137 229 143 130 233 153 184 229 155 155 229 144 141 229 140 150 232 139 151 229 141 136 230 178 187 233 154 155 228 189 143 232 166 135 228 184 184 230 149 153 231 149 140].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'KiwiMaru'.
	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 24))
		editLabel: 'Chinese'.! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:29:51'!
demoCyrillic
	"
	Utf8String demoCyrillic
	"
	| utf8 |
	utf8 _  #[208 155 208 190 209 128 208 181 208 188 32 208 184 208 191 209 129 209 131 208 188 32 208 180 208 190 208 187 208 190 209 128 32 209 129 208 184 209 130 32 208 176 208 188 208 181 209 130 44 32 208 180 208 181 208 177 208 181 209 130 32 209 129 208 176 208 187 209 131 209 130 208 176 208 189 208 180 208 184 32 208 188 208 181 208 184 32 208 181 208 184 46 32 208 144 208 189 32 209 133 208 184 209 129 32 208 179 209 128 208 176 208 181 209 134 208 181 32 209 131 209 130 208 176 208 188 209 131 209 128 32 208 180 208 190 208 187 208 190 209 128 209 131 208 188 44 32 208 187 208 190 209 128 208 181 208 188 32 208 191 209 128 208 184 208 189 209 134 208 184 208 191 208 181 209 129 32 208 188 208 190 208 187 208 181 209 129 209 130 208 184 208 176 208 181 32 208 180 209 131 208 190 32 208 184 208 189 46 32 208 149 208 184 32 208 188 208 190 208 178 208 181 209 130 32 208 189 208 190 209 129 209 130 209 128 208 190 32 208 190 208 191 209 130 208 184 208 190 208 189 32 208 189 208 176 208 188 44 32 208 181 208 190 209 129 32 208 181 208 191 208 184 209 134 209 131 209 128 208 184 32 208 190 208 188 208 184 209 130 209 130 208 176 208 189 209 130 209 131 209 128 32 209 131 209 130 46 32 208 163 209 130 32 208 188 209 131 208 189 208 181 209 128 208 181 32 208 187 208 176 208 177 208 190 209 128 208 181 32 208 184 208 189 209 130 208 181 208 187 208 187 208 181 208 179 208 181 208 177 208 176 209 130 32 209 131 209 129 209 131 44 32 208 181 208 176 32 209 129 208 181 208 180 32 208 180 208 181 208 177 208 181 209 130 32 208 187 209 131 208 191 209 130 208 176 209 130 209 131 208 188 44 32 208 178 208 184 208 180 208 184 209 130 32 208 184 208 187 208 187 209 131 208 180 32 208 184 209 131 208 180 208 184 209 134 208 190 32 209 143 209 131 208 190 32 209 131 209 130 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Cyrillic'.! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:29:57'!
demoGeorgian
	"
	Utf8String demoGeorgian
	"
	| utf8 |
	utf8 _  #[225 131 154 225 131 157 225 131 160 225 131 148 225 131 155 32 225 131 152 225 131 164 225 131 161 225 131 163 225 131 155 32 225 131 147 225 131 157 225 131 154 225 131 157 225 131 160 32 225 131 161 225 131 152 225 131 151 32 225 131 144 225 131 155 225 131 148 225 131 151 44 32 225 131 167 225 131 163 225 131 157 32 225 131 163 225 131 151 32 225 131 147 225 131 152 225 131 170 225 131 144 225 131 151 32 225 131 144 225 131 154 225 131 152 225 131 167 225 131 163 225 131 152 225 131 147 44 32 225 131 144 225 131 147 32 225 131 154 225 131 152 225 131 145 225 131 160 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 160 225 131 148 225 131 164 225 131 160 225 131 148 225 131 176 225 131 148 225 131 156 225 131 147 225 131 163 225 131 156 225 131 151 32 225 131 148 225 131 161 225 131 151 46 32 225 131 154 225 131 148 225 131 146 225 131 148 225 131 160 225 131 148 32 225 131 170 225 131 157 225 131 160 225 131 164 225 131 157 225 131 160 225 131 144 32 225 131 148 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 144 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 144 225 131 151 32 225 131 148 225 131 144 225 131 155 44 32 225 131 156 225 131 157 32 225 131 167 225 131 163 225 131 144 225 131 161 32 225 131 155 225 131 148 225 131 156 225 131 144 225 131 156 225 131 147 225 131 160 225 131 152 32 225 131 160 225 131 148 225 131 164 225 131 148 225 131 160 225 131 160 225 131 148 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 167 225 131 163 225 131 152 46 32 225 131 151 225 131 148 32 225 131 164 225 131 163 225 131 151 225 131 148 225 131 156 225 131 151 32 225 131 155 225 131 157 225 131 154 225 131 154 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 149 225 131 152 225 131 161 46 32 225 131 149 225 131 152 225 131 155 32 225 131 147 225 131 148 225 131 156 225 131 152 225 131 167 225 131 163 225 131 148 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 151 225 131 176 225 131 148 225 131 157 225 131 164 225 131 176 225 131 160 225 131 144 225 131 161 225 131 151 225 131 163 225 131 161 32 225 131 148 225 131 174 44 32 225 131 148 225 131 144 32 225 131 176 225 131 144 225 131 161 32 225 131 149 225 131 157 225 131 154 225 131 163 225 131 155 225 131 163 225 131 161 32 225 131 164 225 131 144 225 131 161 225 131 151 225 131 152 225 131 147 225 131 152 225 131 152 32 225 131 147 225 131 148 225 131 161 225 131 148 225 131 160 225 131 163 225 131 156 225 131 151 46 32 225 131 148 225 131 174 32 225 131 157 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 152 225 131 161 32 225 131 151 225 131 157 225 131 160 225 131 167 225 131 163 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 155 225 131 148 225 131 152 44 32 225 131 147 225 131 157 225 131 170 225 131 151 225 131 163 225 131 161 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 152 225 131 156 32 225 131 167 225 131 163 225 131 157 46 32 225 131 161 225 131 163 225 131 155 225 131 155 225 131 157 32 225 131 151 225 131 160 225 131 144 225 131 170 225 131 151 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 148 225 131 152 32 225 131 152 225 131 163 225 131 161 44 32 225 131 163 225 131 151 32 225 131 157 225 131 155 225 131 156 225 131 148 225 131 161 32 225 131 149 225 131 152 225 131 160 225 131 151 225 131 163 225 131 151 225 131 148 32 225 131 147 225 131 148 225 131 151 225 131 160 225 131 144 225 131 174 225 131 152 225 131 151 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Georgian'.! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:30:04'!
demoGreek
	"
	Utf8String demoGreek
	"
	| utf8 |
	utf8 _  #[206 155 206 191 207 129 206 181 206 188 32 206 185 207 128 207 131 206 184 206 188 32 206 180 206 191 206 187 206 191 207 129 32 207 131 206 185 207 132 32 206 177 206 188 206 181 207 132 44 32 206 188 206 191 206 180 206 191 32 118 206 181 206 189 206 185 206 177 206 188 32 118 206 185 206 180 206 181 207 129 206 181 207 129 32 206 185 206 189 32 99 206 184 206 188 44 32 206 181 206 190 32 206 177 206 184 207 132 206 181 206 188 32 206 188 206 177 206 187 206 185 207 131 32 206 191 207 134 207 134 206 181 206 189 206 180 206 185 207 132 32 206 181 207 131 207 132 46 32 206 153 206 180 32 207 132 206 191 206 187 206 187 206 185 207 132 32 207 131 206 184 207 131 99 206 185 207 128 206 185 207 132 32 207 131 206 177 206 187 206 184 207 132 206 177 207 132 206 184 207 131 32 113 206 184 206 185 44 32 99 206 184 32 113 206 184 206 177 206 189 206 180 206 191 32 207 128 207 129 206 191 207 128 207 129 206 185 206 177 206 181 32 206 185 206 188 207 128 206 181 207 129 206 180 206 185 206 181 207 132 32 118 206 181 206 187 44 32 206 188 206 181 206 177 32 206 181 207 132 32 206 181 207 129 207 129 206 181 206 188 32 206 179 206 187 206 191 207 129 206 185 206 177 207 132 206 184 207 129 46 32 206 149 206 190 206 181 207 129 99 206 185 32 206 180 206 181 207 134 206 185 206 189 206 185 207 132 206 185 206 191 206 189 206 181 206 188 32 206 188 206 181 206 187 32 206 181 206 177 46 32 206 149 206 190 32 207 131 206 181 206 177 32 206 181 206 187 206 185 207 132 32 206 180 206 185 207 131 207 131 206 181 206 189 207 132 206 185 206 181 207 132 44 32 206 188 206 177 206 187 206 185 207 131 32 207 132 207 129 206 185 207 132 206 177 206 189 206 185 32 118 206 185 207 131 32 206 185 206 180 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Greek'! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:30:11'!
demoHebrew
	"
	Utf8String demoHebrew
	"
	| utf8 |
	utf8 _  #[215 162 215 156 32 215 158 215 144 215 158 215 168 32 215 150 215 167 215 149 215 167 32 215 169 215 147 215 168 215 149 215 170 32 215 144 215 168 215 165 44 32 215 148 215 158 215 156 215 166 215 170 32 215 158 215 147 215 149 215 153 215 167 215 153 215 157 32 215 169 215 155 215 156 32 215 169 215 156 44 32 215 162 215 153 215 166 215 149 215 145 32 215 160 215 153 215 149 215 149 215 152 32 215 169 215 153 215 170 215 149 215 164 215 153 215 170 32 215 162 215 149 215 147 32 215 145 46 32 215 161 215 147 215 168 32 215 144 215 149 32 215 168 215 164 215 149 215 144 215 148 32 215 169 215 153 215 158 215 149 215 169 215 153 32 215 158 215 149 215 162 215 158 215 147 215 153 215 157 46 32 215 148 215 153 215 144 32 215 164 215 153 215 161 215 153 215 167 215 148 32 215 148 215 146 215 168 215 164 215 153 215 157 32 215 169 215 153 215 158 215 149 215 169 215 153 215 153 215 157 32 215 169 215 156 46 32 215 169 215 155 215 156 32 215 162 215 156 32 215 162 215 150 215 168 215 148 32 215 156 215 148 215 164 215 149 215 154 32 215 144 215 149 215 149 215 153 215 168 215 149 215 160 215 144 215 149 215 152 215 153 215 167 215 148 44 32 215 144 215 160 215 144 32 215 144 215 170 32 215 162 215 150 215 168 215 148 32 215 156 215 158 215 160 215 149 215 162 32 215 156 215 158 215 151 215 153 215 167 215 148 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Hebrew'! !

!Utf8String class methodsFor: '*VectorGraphics-demo' stamp: 'jmv 6/9/2022 12:30:17'!
demoLatin
	"
	Utf8String demoLatin
	"
	| utf8 |
	utf8 _  #[76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46].

	(Text string: (Utf8String fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Latin Alphabet'! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 10/29/2021 10:52:48'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"
	| fdestX char glyph maxHeight ascent lineGap |

	ascent _ aTrueTypeFont ascent.		  "expensive"
	lineGap _ aTrueTypeFont lineGap."expensive"
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	startIndex to: stopIndex do: [ :charIndex |
		char _ aString at: charIndex.
		glyph _ aTrueTypeFont glyphAt: char.
		destX _ (fdestX + glyph leftOffset) roundedHAFZ.
		destY _ aPoint y + ascent + lineGap-1 - glyph topOffset. 
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (ascent*0.3)]].
		destY _ destY roundedHAFZ.
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height ].
	^ fdestX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 10/29/2021 10:52:53'!
basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"
	| fdestX glyph maxHeight ascent lineGap |

	ascent _ aTrueTypeFont ascent.		  "expensive"
	lineGap _ aTrueTypeFont lineGap."expensive"
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	startIndex to: stopIndex do: [ :i | | codePoint |
		codePoint _ aWordArray at: i.
		Character
			evaluateOnce: [ :byte1 :byte2 :byte3 :byte4 |
				glyph _ aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
		destX _ (fdestX + glyph leftOffset) roundedHAFZ.
		destY _ aPoint y + ascent + lineGap-1 - glyph topOffset.
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (ascent*0.3)]].
		destY _ destY roundedHAFZ.
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height ].
	^ fdestX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 5/5/2022 11:16:25'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap |
	ascent _ aTrueTypeFont ascent.		  "expensive"
	lineGap _ aTrueTypeFont lineGap."expensive"
	destY _ aPoint y.
	fdestX _ aPoint x.
	maxHeight _ 0.
	byteIndex _ byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph _ aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ] 
			into: nil.
		destX _ (fdestX + glyph leftOffset) roundedHAFZ.
		destY _ aPoint y + ascent + lineGap-1 - glyph topOffset.
		aTrueTypeFont isSuperscript
			ifTrue: [destY _ destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY _ destY + (ascent*0.3)]].
		destY _ destY roundedHAFZ.
		sourceForm _ glyph form.
		height _ sourceForm height.
		width _ sourceForm width.
		self copyBits.
		fdestX _ fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight _ maxHeight max: height.
		byteIndex _ byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight)! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/23/2021 13:03:04'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
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

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/25/2021 16:30:39'!
displayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
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
		^self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: '*VectorGraphics-TrueType' stamp: 'jmv 3/23/2021 15:09:47'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
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
		^self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 4/21/2022 14:07:32'!
addLabel: string font: font centeredAt: position rotationDegrees: angle
	"Add a label top-centered at position and rotated of angle degrees"
	| p label |
	p _ position - (position normal * (font widthOfString: string asPlainString) / 2).
	label _ LabelMorph
		contents: string
		font: font.
	label rotation: (90 - angle) degreesToRadians.
	self addMorph: label.
	label morphPosition: p.
	^ label.! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 2/24/2022 15:11:49'!
bitMask
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask1  when appropriate!!
	(i.e. when we change in such a way to make the bitMask invalid)"

	| prevNotVisibleFlag bounds answer auxCanvas |

	(self valueOfProperty: #bitMask) ifNotNil: [ :bitMask |
		^bitMask ].

	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag _ self privateFlagAt: 3.
	bounds _ self findFullBoundsInOwner.
	[
		answer _ Form extent: bounds extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas _ VectorCanvas onFormWithSubPixelAntiAliasing: answer translation: bounds origin negated.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^self setProperty: #bitMask toValue:
		"Reverse it so we get bit=1 for affected pixels."
		((answer asGrayForm: 1) asFormOfDepth: 1) reverse.! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 10/29/2021 12:29:16'!
imageFormVG: extentOrNil depth: depth
	"Draw using a VectorCanvas
	Scale as needed. Draw unrotated."

	| prevNotVisibleFlag bounds extent fauxLocation answer auxCanvas |
	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag _ self privateFlagAt: 3.
	[
		bounds _ self findFullBoundsInOwner.
		extentOrNil
			ifNotNil: [ | scale |
				extent _ extentOrNil.
				scale _ extent x asFloat / bounds width min: extent y asFloat / bounds height.
				fauxLocation _ AffineTransformation withScale: scale position: bounds origin negated +1 * scale ]
			ifNil: [
				extent _ bounds extent.
				fauxLocation _ MorphicTranslation withTranslation: bounds origin negated ].
		answer _ Form extent: extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas _ VectorCanvas onFormWithSubPixelAntiAliasing: answer.
		auxCanvas geometryTransformation: fauxLocation.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 2/24/2022 15:42:01'!
showBitMask
	"Just to play with #bitMask from an inspector. Not to be called from code."
	| r |
	self flag: #bitMask.						"So this method is shown in senders of #bitMask"
	r _ self displayFullBounds.
	r top to: r bottom-1 do: [ :y |
		r left to: r right-1 do: [ :x |
			(self coversPixel: x@y) ifTrue: [
				Display colorAt: x@y put: Color random ]]].
	Display forceToScreen! !

!Morph methodsFor: '*vectorGraphics' stamp: 'jmv 2/26/2022 11:54:39'!
showContour
	"Just to play with #contour from an inspector. Not to be called from code."
	
	(self valueOfProperty: #contour) ifNotNil: [ :contourArray | | contourTop contourBottom  x0 x1 |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		contourTop to: contourBottom do: [ :y |
			x0 _ contourArray at: (y - contourTop) * 2 + 1.
			x1 _ contourArray at: (y - contourTop) * 2 + 2.
			Display colorAt: x0+1@y put: `Color black`.
			Display colorAt: x0+2@y put: `Color black`.
			Display colorAt: x1-2@y put: `Color black`.
			Display colorAt: x1-1@y put: `Color black`.
			Display colorAt: x0@y put: `Color lightGreen`.
			Display colorAt: x1@y put: `Color white`.
		].
		Display forceToScreen.
	].! !

!PlacedMorph methodsFor: '*vectorGraphics' stamp: 'jmv 12/17/2021 10:11:34'!
fitInto: aRectangle
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxedMorph, and maybe others will not scale, but resize, changing the actual extent."

	| currentBounds scale slackForAntiAliasing |
	currentBounds _ self findFullBoundsInOwner.
	slackForAntiAliasing _ 0.
	scale _ (aRectangle width-slackForAntiAliasing / (currentBounds width-slackForAntiAliasing))
		min: (aRectangle height-slackForAntiAliasing / (currentBounds height-slackForAntiAliasing)).
	scale _ scale asFloat.
	self scaleBy: scale.

	currentBounds _ self findFullBoundsInOwner.
	self morphPosition: aRectangle origin - (currentBounds origin - self morphPosition) + (aRectangle extent - currentBounds extent // 2).! !

!LayoutMorph class methodsFor: '*VectorGraphics-examples' stamp: 'jmv 9/28/2021 16:24:36'!
example101VG
"
	self example101VG
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (Sample09Clock new name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 100); 
	addMorph: (Sample01Star new name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Sample03Smiley new name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BoxedMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (Sample03Smiley new name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color lightRed;
	addMorph: (Sample02Bezier new name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Sample02Bezier new  name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (Sample02Bezier new  name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BoxedMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BoxedMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BoxedMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !
AbstractVectorCanvas initialize!
VectorCanvas initialize!
TTFontDescription initialize!
