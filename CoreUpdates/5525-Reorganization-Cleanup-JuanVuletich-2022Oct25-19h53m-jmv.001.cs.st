'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 8:05:21 pm'!
!classDefinition: #Magnitude category: #'Kernel-Objects'!
Object subclass: #Magnitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Utf32String commentStamp: '<historical>' prior: 0!
String like objects handling Unicode code points, encoded as UTF-32 bytes.

Play with the class side demo methods.!


!Utf8String commentStamp: '<historical>' prior: 0!
String like objects handling Unicode code points, encoded as UTF-8 bytes.
Based on https://github.com/svenvc/UTF8String by Sven Van Caekenberghe. Good ideas and implementation. Thanks Sven!!

Play with the class side demo methods.!


!SmartRefStream commentStamp: 'jmv 9/29/2022 18:26:34' prior: 0!
Ordinary ReferenceStreams assume that the names and order of instance variables is exactly the same when an object file is written and read.  
	SmartRefStream allows object files to be read even after instance variables have changed or the entire class has been renamed.

When an object file is written, no one knows how the classes will change in the future.  Therefore, all conversion must be done when the file is read.  The key is to store enough information in the file about the names of the instance variables of all outgoing classes.  

SmartRefStream works best with only one tree of objects per file.  You can nextPut: more than once, but each object tree gets its own class structure description, which is big.  

Conversion of old objects is done by a method in each class called (convertToCurrentVersion: varDict refStream: smartRefStrm).  At fileOut time, ChangeSet>>checkForConversionMethods creates a prototype of this method (if Preference #conversionMethodsAtFileOut is true).  The programmer must edit this method to (1) test if the incoming object needs conversion, (2) put non-nil values into any new inst vars that need them, and (3) save the data of any inst vars that are being deleted. 

Determining which old version is represented by the incoming object can be done in several ways: noticing that a current inst var is nil when it should have data, noticing that there is an older inst var name in the variable dictionary (varDict), checking kinds of objects in one or more inst vars, or retrieving the classVersion of the incoming object from the ref stream.  

If a class is renamed, a method goes into SmartRefStream telling the new name.  The conversion method of the new class must be prepared to accept instances of the old class also.  If no inst var names have changed, the conversion method does nothing.

An example:  
	Suppose we change the representation of class Rectangle from ('origin' 'corner') to ('origin' 'extent').  Suppose lots of Rectangle instances are already out on files (in .pr project files, especially).  
	The programmer changes the class definition, modifies all the methods, and filesOut.  A series of dialogs appear, asking if instances Rectangle might be in an object file, if 'extent' needs to be non-nil (yes), and if the info in 'corner' needs to be preserved (yes).  This method appears:

Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm
	"These variables are automatically stored into the new instance: #('origin').
	Test for this particular conversion.  Get values using expressions like (varDict at: 'foo')."

	"New variables: #('extent').  If a non-nil value is needed, please assign it."
	"These are going away #('corner').  Possibly store their info in some other variable?"
	"Move your code above the ^ super...  Delete extra comments."
	^ super convertToCurrentVersion: varDict refStream: smartRefStrm

The programmer modifies it to be:

Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm

(varDict includesKey: 'extent') ifFalse: ["old version!!"
	"Create the new extent, and preserve the info from the old corner"
	extent _ (varDict at: 'corner') - origin.
	].
^ super convertToCurrentVersion: varDict refStream: smartRefStrm

	This conversion method stays in the system and is ready to convert the old format of Rectangle whenever one is encountered in an object file.  Note that the subclasses of Rectangle, (B3DViewport, CharacterBlock, and Quadrangle) do not need conversion methods.  Their instances will be converted by the code in Rectangle.  

	Files written by SmartRefStream are in standard fileout format.  You can mix raw objects with code to be filed in.  The file starts out in the normal fileOut format.  Definitions of new classes on the front.

structures 	Dictionary of (#Rectangle -> #(<classVersionInteger> 'origin' 'corner')).  Inst 
				var names are strings.
steady 		Set of Classes who have the same structure now as on the incoming file.
				Includes classes with same inst vars except for new ones added on the end.
reshaped 	Dictionary of Classes who have a different structure now from the incoming file.  
				Includes those with same inst vars but new version number.
				(old class name -> method selector to fill in data for version to version)
renamed	Dictionary of Classes who have a different name.  Make an instance of the new
			class, and send it the conversion call.
				(old class name symbol -> new class name).  
renamedConv	Dictionary of conversion selector for Classes who have a different name.
				(old class name symbol -> conversion selector).  
topCall		Tells if next or nextPut: are working on the top object in the tree.  
			nil if outside, the top object if deep inside.

See DataStream.typeIDFor: for where the tangle of objects is clipped, so the whole system will not be written on the file.

No object that is written on the file is ever a class.  All class definitions are filed in.  A class may be stored inside an ImageSegment that itself is stored in a SmartRefStream.

There is a separate subclass for doing veryDeepCopy (in memory).  Currently, any object for which objectToStoreOnDataStream return an object other than self, does this:  The new object (a DiskProxy) is traced.  When it comes time to go through the fields of the old object, they are not found as keys in references (DiskProxies are there instead).  So the old field value is left in the new object.  That is OK for Class, MetaClass, DisplayScreen.  But the DiskProxies are evaluated, which takes a lot of time.

Some metaclasses are put into the structures table.  This is for when a block has a receiver that is a class.  See checkFatalReshape:.

ImageSegments:
	A ReferenceStream is used to enumerate objects to put inside an ImageSegment.
	A SmartRefStream is used to store the ImageSegment.  Roots are nil, and the segment is a wordArray.  We are encoding the outPointers.  Structures contains all classes from both places.

--Ted Kaehler and Bob Arning.
!

!classDefinition: #PathCommandError category: 'Graphics-PathCommands'!
Error subclass: #PathCommandError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!

!DiskProxy commentStamp: 'jmv 9/29/2022 18:26:45' prior: 0!
A DiskProxy is an externalized form of an object to write on a
DataStream. It contains a "constructor" message to regenerate
the object, in context, when sent a comeFullyUpOnReload: message
(i.e. "internalize").

We are now using DiskProxy for shared system objects.

The idea is to define, for each kind of object that needs special
externalization, a class method that will internalize the object by
reconstructing it from its defining state. We call this a
"constructor" method. Then externalize such an object as a frozen
message that invokes this method--a DiskProxy.

(Here is the old comment:
Constructing a new object is good for any object that (1) can not be
externalized simply by snapshotting and reloading its instance
variables (like a CompiledMethod or a Picture), or (2) wants to be
free to evolve its internal representation without making stored
instances obsolete (and dangerous). Snapshotting and reloading an
object"s instance variables is a dangerous breach of encapsulation.

The internal structure of the class is then free to evolve. All
externalized instances will be useful as long as the
constructor methods are maintained with the same semantics.

There may be several constructor methods for a particular class. This
is useful for (1) instances with characteristically different
defining state, and (2) newer, evolved forms of an object and its
constructors, with the old constructor methods kept around so old
data can still be properly loaded.)

Create one like this example from class Picture

    DiskProxy global: #Picture
            selector: #fromByteArray:
                args: (Array with: self storage asByteArray)

* See also subclass DiskProxyQ that will construct an object in
the above manner and then send it a sequence of messages. This may save
creating a wide variety of constructor methods. It is also useful because
the newly read-in DiskProxyQ can catch messages like #objectContainedIn:
(via #doesNotUnderstand:) and add them to the queue of messages to
send to the new object.

* We may also want a subclass of DiskProxy that evaluates a string
expression to compute the receiver of the constructor message.

My instance variables:
* globalObjectName -- the Symbol name of a global object in the
    System dictionary (usually a class).
* constructorSelector -- the constructor message selector Symbol to
    send to the global object (perform:withArguments:), typically a
    variation on newFrom:.
* constructorArgs -- the Array of arguments to pass in the
    constructor message.

-- 11/9/92 Jerry Morrison
!

!classDefinition: #TrueTypeFont category: 'Graphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphForms glyphFormsByUtf8 family baseFont derivativeFonts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TrueTypeFontFamily category: 'Graphics-TrueType'!
FontFamily subclass: #TrueTypeFontFamily
	instanceVariableNames: 'baseTTFontDescription ttFontDescriptionsByEmphasis'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #Sample01Star category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample01Star
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample02Bezier category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample02Bezier
	instanceVariableNames: 'aStart aEnd aControlPoint bStart bEnd bControlPoint1 bControlPoint2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample03Smiley category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample03Smiley
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample04Pacman category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample04Pacman
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample06EasyToGrab category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample06EasyToGrab
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample07Clipping category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample07Clipping
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample08Form category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample08Form
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample09Clock category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample09Clock
	instanceVariableNames: 'hourHand minuteHand secondHand'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample09ClockHourHand category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample09ClockHourHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample09ClockMinuteHand category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample09ClockMinuteHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #Sample09ClockSecondHand category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample09ClockSecondHand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #AbstractVectorCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #AbstractVectorCanvas
	instanceVariableNames: 'firstPoint lastControlPoint currentPoint'
	classVariableNames: 'IconPathData'
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #BoundsFinderCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #BoundsFinderCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #HybridCanvas category: #'Morphic-Support'!
BitBltCanvas subclass: #HybridCanvas
	instanceVariableNames: 'vectorCanvas morphIDsForm drawingOnMorphIDs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngine category: #'Morphic-Support'!
Object subclass: #VectorEngine
	instanceVariableNames: 'canvas antiAliasingWidth geometryTransformation strokeWidth morphBoundsLeft morphBoundsTop morphBoundsRight morphBoundsBottom hop firstX firstY currentX currentY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineBoundsFinder category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineBoundsFinder
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom swDilatedHalf subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor fillColor clipLeft clipTop clipRight clipBottom currentMorphId currentClipsSubmorphs clipCurrentMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineWithPlugin category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineWithPlugin
	instanceVariableNames: ''
	classVariableNames: 'AccessLock'
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineWithPluginSubPixel category: #'Morphic-Support'!
VectorEngineWithPlugin subclass: #VectorEngineWithPluginSubPixel
	instanceVariableNames: 'subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineWithPluginWholePixel category: #'Morphic-Support'!
VectorEngineWithPlugin subclass: #VectorEngineWithPluginWholePixel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #PathCommand category: 'Graphics-PathCommands'!
Object subclass: #PathCommand
	instanceVariableNames: 'coordinatesAreRelative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathArcCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathArcCommand
	instanceVariableNames: 'rs xAxisRotations toPoints doLargerArcs useIncreasingAngles'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathCloseCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathCloseCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathCurveToCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathCurveToCommand
	instanceVariableNames: 'toPoints control1Points control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathCurveToSmoothCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathCurveToSmoothCommand
	instanceVariableNames: 'toPoints control2Points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathLineToCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathLineToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathLineToHCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathLineToHCommand
	instanceVariableNames: 'xs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathLineToVCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathLineToVCommand
	instanceVariableNames: 'ys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathMoveToCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathMoveToCommand
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathQuadraticCurveToCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathQuadraticCurveToCommand
	instanceVariableNames: 'toPoints controlPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #PathQuadraticCurveToSmoothCommand category: 'Graphics-PathCommands'!
PathCommand subclass: #PathQuadraticCurveToSmoothCommand
	instanceVariableNames: 'toPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-PathCommands'!
!classDefinition: #FormGlyph category: 'Graphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTContourConstruction category: 'Graphics-TrueType'!
Object subclass: #TTContourConstruction
	instanceVariableNames: 'points controlPointFlags'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTFontDescription category: 'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForIso8859s15 contourDataIndexesByIso8859s15 contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTFontReader category: 'Graphics-TrueType'!
Object subclass: #TTFontReader
	instanceVariableNames: 'glyphs nGlyphs kernPairs fontDescription'
	classVariableNames: 'FallbackArrowGlyphs'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTFontTableDirEntry category: 'Graphics-TrueType'!
Object subclass: #TTFontTableDirEntry
	instanceVariableNames: 'tag fontData offset length checkSum'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTGlyph category: 'Graphics-TrueType'!
Object subclass: #TTGlyph
	instanceVariableNames: 'boundsLeft boundsRight boundsTop boundsBottom contours advanceWidth leftSideBearing rightSideBearing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTCompositeGlyph category: 'Graphics-TrueType'!
TTGlyph subclass: #TTCompositeGlyph
	instanceVariableNames: 'glyphs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTKernPair category: 'Graphics-TrueType'!
Object subclass: #TTKernPair
	instanceVariableNames: 'left right value mask'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

!SystemOrganizer methodsFor: 'remove' stamp: 'jmv 9/30/2022 16:51:08'!
removeEmptySystemCategories
	"
	SystemOrganization removeEmptySystemCategories.
	SystemOrganization categories do: [ :c |
	(SystemOrganization listAtCategoryNamed: c) isEmpty ifTrue: [
		c print ]].
	"
	self categories do: [ :c |
		(self listAtCategoryNamed: c) isEmpty ifTrue: [
			self removeCategory: c ]]! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/25/2022 20:01:56'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance."

	| boundingRect |
	boundingRect _ (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
		encompassingIntegerRectangle.
	"Include an extra pixel to cover possible anti aliasing."
	boundingRect _ boundingRect outsetBy: 1.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('services' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('dispatch' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash sameAs:)
('converting' asUtf32String asUtf32Words asUtf8Bytes)
('testing' isAscii isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('initialization' initialize)
('dispatch' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!Utf32String class reorganize!
('instance creation' fromUtf32Words: fromUtf8Bytes: new:withAll: newFrom: with: with:with: withWords:)
('unicode demo' demoAncientEgyptian demoArabic demoArmenian demoChinese demoCyrillic demoGeorgian demoGreek demoHebrew demoLatin)
!


!Utf8String reorganize!
('accessing' at: bytes findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with: copyWith: shallowCopy)
('comparing' < <= = > >= basicHash caseInsensitiveLessOrEqual: computeHash hash is:substringAt: sameAs:)
('converting' asByteString asHex asUtf32Words asUtf8Bytes asUtf8String reversed substrings surroundedBySingleQuotes thatCanBeModified withLineEndings:)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('printing' isLiteral)
('private' boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: privateBytes:codePointCount: stride)
('services' numArgs)
('initialization' initialize)
('dispatch' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!Utf8String class reorganize!
('instance creation' crString crlfString empty fromUtf32Words: fromUtf8Bytes: fromUtf8Bytes:codePointCount: lfString new:withAll: newFrom: newLineString tab with: with:with: with:with:with: withAll:)
('services' compare:with: utf32FromUtf8: utf8FromUtf16: utf8FromUtf32:)
('system' symbolClassToUse)
('stream creation' streamContents: writeStream)
('unicode demo' demoAncientEgyptian demoArabic demoArmenian demoChinese demoCyrillic demoGeorgian demoGreek demoHebrew demoLatin)
!


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky boundsFinderCanvas canvas color layerNumber location location: lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' fullRedrawNotNeeded highlighted: isHighlighted isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo: valueOfProperty:ifPresentDo:ifAbsent:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:for: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate okayToDuplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError showBitMask showContour)
('drawing' drawOn: drawingFails drawingFailsNot fullyCovers: hide icon imageForm: imageForm:depth: imageFormVG:depth: isKnownFailing postDrawOn: refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: acceptDroppingMorph:event: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent: focusKeyboardFor: handleFocusEvent: invalidDrop: processDropFiles: processDropMorph: processKeyDown: processKeyUp: processKeystroke: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent: processWindowEvent: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange: keyboardFocused)
('geometry' adjustDisplayBoundsBy: basicDisplayBounds displayBounds displayBoundsForHalo displayFullBounds extentChanged: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: findFullBoundsInOwner fontPreferenceChanged fullBoundsInOwner internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent morphExtent morphExtentInOwner morphLocalBoundsForError morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: rotation rotation:scale: rotationDegrees rotationDegrees: scale topLeftInOwner)
('geometry services' bitMask collides: contourIntersects:top:bottom: coversAnyPixelCloserThan:to: coversPixel: fullIncludesPixel: includesPixel: isCloserThan:toPixel: on:ifCovered:uncoveredPartsDo:else: ownsPixel:)
('geometry testing' clipsSubmorphs clipsSubmorphsReally hasOwnLocation hasVariableExtent isOrAnyOwnerIsRotated knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('halos and balloon help' addHalo addHalo: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloShowsCoordinateSystem mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily okayToScaleEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: showBalloon:hand:doWordWrap: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo: wantsRecolorHandle)
('initialization' initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' layoutSubmorphs layoutSubmorphsIfNeeded minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets)
('naming' label)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stepTime stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root topmostWorld veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsBut:do: allMorphsDo: allSubmorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addLabel:font:centeredAt:rotationDegrees: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' canDiscardEdits disregardUnacceptedEdits drawsKeyboardFocusIndicator hasModel honorsLayerOrder is: isCollapsed isIncludedInTaskbar isOwnedByHand isOwnedByWorld isOwnedByWorldOrHand isProportionalHeight isProportionalWidth isReallyVisible)
('updating' invalidateBounds redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse embeddedInMorphicWindowLabeled: flash flashWith: showAndComeToFront toggleCollapseOrShow unlockOneSubpart whenUIinSafeState:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateDisplayBounds privateDisplayBounds: privateFixLayerOrder privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!


!PlacedMorph reorganize!
('accessing' location location:)
('geometry' adjustExtent allocHeightForFactor: allocWidthForFactor: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: fitInto: internalize: internalizeDistance: minimumLayoutExtent morphAlign:with: morphPosition morphPosition: morphPositionInWorld: orbitBy: referencePosition referencePositionInOwner rotateBy: rotation rotation: rotation:scale: rotationCenter scale scale: scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation isOrAnyOwnerIsRotated yAxisPointsUp)
('private' fixYAxisDirection privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isProportionalHeight isProportionalWidth)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemOrganization removeEmptySystemCategories.!

