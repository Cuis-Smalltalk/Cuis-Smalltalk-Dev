"
A MorphicCanvas offers 2D drawing services. It works on a 'form', usually the Display. These services are used, for example, in #drawOn: methods.

Subclasses are specific implementations. BitBltCanvas is based on BitBlt, the raster operation invented by Dan Ingalls for Smalltalk, and included in Smalltalk-80 and Squeak. VectorCanvas is based on its VectorEngine, using a novel technique for the rasterization (sampling) of vector graphics, invented by Juan Vuletich.
"
Class {
	#name : 'MorphicCanvas',
	#superclass : 'Object',
	#instVars : [
		'world',
		'form',
		'clipRect',
		'transformations',
		'currentTransformation',
		'cti',
		'currentMorph',
		'drawingMorphStack',
		'engine',
		'clippingMorphDisplayBounds'
	],
	#classVars : [
		'ActiveSubclass'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
MorphicCanvas class >> activeSubclass [
	ActiveSubclass ifNil: [
		ActiveSubclass := BitBltCanvas ].
	Display depth = 32 ifFalse: [
		"VectorCanvas can only work on 32 bit target"
		ActiveSubclass := BitBltCanvas ].
	^ActiveSubclass
]

{ #category : 'instance creation' }
MorphicCanvas class >> activeSubclass: aMorphicCanvasSubclass [
	"
	self runningWorld canvas class
	MorphicCanvas activeSubclass: BitBltCanvas
	MorphicCanvas activeSubclass: HybridCanvas
	MorphicCanvas activeSubclass: VectorCanvas
	"
	(#(BitBltCanvas HybridCanvas VectorCanvas)
		includes: aMorphicCanvasSubclass name) ifFalse: [
			^self error: 'Invalid Canvas class' ].
	ActiveSubclass := aMorphicCanvasSubclass.
	UISupervisor ui ifNotNil: [ :world |
		world whenUIinSafeState: [
			UISupervisor newUIProcess ]].
]

{ #category : 'instance creation' }
MorphicCanvas class >> depth: depth over: aRectangle [

	^self onForm: (Form extent: aRectangle extent depth: depth) translation: aRectangle topLeft negated.
]

{ #category : 'services' }
MorphicCanvas class >> guiSizePreferenceChanged [
	"Some preference related to size of gui elements may have changed."
]

{ #category : 'instance creation' }
MorphicCanvas class >> onDisplay [
	"Answer an instance of the appropriate subclass targetting Display,
	or an intermediate buffer, as appropriate.
	Create a new VectorEngineDrawer or BitBltCanvasEngine as appropriate.
	Use a#deferredUpdatingDisplay, so actual contents are flushed to visible pixels only when appropriate.
	See DisplayScreen 'deferred updating' class methods category."

	| canvasTarget |
	canvasTarget := DisplayScreen deferredUpdatingDisplay.
	^self onForm: canvasTarget.
]

{ #category : 'instance creation' }
MorphicCanvas class >> onDisplayToReplace: oldCanvasOrNil [
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	| vectorEngineToReuse |
	self subclassToUse = oldCanvasOrNil class ifFalse: [
		^self onDisplay ].
	vectorEngineToReuse := oldCanvasOrNil vectorEngine.
	vectorEngineToReuse isNil ifTrue: [
		^self onDisplay ].
	vectorEngineToReuse isConsistentWithTarget ifFalse: [
		^self onDisplay ].
	^self subclassToUse basicNew
		setVectorEngine: vectorEngineToReuse;
		initializeWithTranslation: `0@0`.
]

{ #category : 'instance creation' }
MorphicCanvas class >> onForm: aForm [

	^ self onForm: aForm translation: `0@0`.
]

{ #category : 'instance creation' }
MorphicCanvas class >> onForm: aForm translation: aPoint [

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false" not";
		initializeWithTranslation: aPoint.
]

{ #category : 'instance creation' }
MorphicCanvas class >> onFormWithSubPixelAntiAliasing: aForm [

	^ self onFormWithSubPixelAntiAliasing: aForm translation: `0@0`.
]

{ #category : 'instance creation' }
MorphicCanvas class >> onFormWithSubPixelAntiAliasing: aForm translation: aPoint [

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: true;
		initializeWithTranslation: aPoint.
]

{ #category : 'instance creation' }
MorphicCanvas class >> subclassToUse [
	"If asked to a specific subclass, use that."

	^ self == MorphicCanvas
		ifFalse: [ self ]
		ifTrue: [ self activeSubclass ]
]

{ #category : 'instance creation' }
MorphicCanvas class >> withExtent: extent depth: depth [

	^ self onForm: (Form extent: extent depth: depth)
]

{ #category : 'morphic world' }
MorphicCanvas >> beMainCanvasOfWorld [
	world mainCanvas: self.
	self doOwnEngine.
	world displayWorldOn: self.
]

{ #category : 'morphic world' }
MorphicCanvas >> boundingRectOfCurrentIfTrivial [
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		^ (currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle ].
	^nil.
]

{ #category : 'morphic world' }
MorphicCanvas >> boundingRectOfCurrentMorphAfterDraw [
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #localBounds is known.
	Subclasses may raise this restriction. See inherintance."

	| boundingRect |
	boundingRect := self boundingRectOfCurrentIfTrivial.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]
]

{ #category : 'accessing' }
MorphicCanvas >> boundsFinderCanvas [
	^self subclassResponsibility
]

{ #category : 'testing' }
MorphicCanvas >> canDoVectorGraphics [
	^false
]

{ #category : 'private' }
MorphicCanvas >> canvasOrigin [
	"Return the current origin for drawing operations"
	^ transformations first translation
]

{ #category : 'private' }
MorphicCanvas >> canvasToUse [

	^self.
]

{ #category : 'morphic' }
MorphicCanvas >> clipByCurrentMorphStart [

]

{ #category : 'morphic' }
MorphicCanvas >> clipByCurrentMorphStop [

]

{ #category : 'accessing' }
MorphicCanvas >> clipRect [
	"Return the currently active clipping rectangle"
	"In targetForm coordinates"
	^ clipRect
]

{ #category : 'private' }
MorphicCanvas >> clippingByCurrentMorphDo: aBlock [
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphs ifFalse: [ ^aBlock value ].

	prevClipRect := clipRect.
	prevClippingMorphRect := clippingMorphDisplayBounds.
	clippingMorphDisplayBounds := self boundingRectOfCurrentMorphAfterDraw insetBy: 1.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipByCurrentMorphStart.
	aBlock ensure: [
		self clipByCurrentMorphStop.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds := prevClippingMorphRect ]
]

{ #category : 'morphic' }
MorphicCanvas >> contourClearOnCurrentMorph [
	"Remove knowledge of any contour."

	currentMorph setProperty: #contour toValue: nil.
	currentMorph setProperty: #contourY0 toValue: nil.
	currentMorph setProperty: #contourY1 toValue: nil.
]

{ #category : 'accessing' }
MorphicCanvas >> currentMorph [
	^currentMorph
]

{ #category : 'accessing' }
MorphicCanvas >> currentTransformation [
	"Warning. Only valid inside a #drawOn: method"
	^currentTransformation
]

{ #category : 'morphic' }
MorphicCanvas >> displayBoundsSetOnCurrentMorph [
	"At time of last draw on world main canvas. nil if unknown.
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Clipped appropiately if some owner clips currentMorph.
	See also #displayBoundsUpdateOn:"

	currentMorph mainCanvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: self canvasToUse boundingRectOfCurrentMorphAfterDraw ]].
]

{ #category : 'morphic' }
MorphicCanvas >> displayBoundsUpdateOnCurrentMorph [
	"At time of last draw on world main canvas. nil if unknown.
	Only to be used for the world main canvas
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Rectangle is clipped appropiately if some owner clips currentMorph.
	To be called after #postDrawOn:. See also #displayBoundsSetOn:
	This more complicated update is needed because the contour information from #drawOn: was lost
	when drawing submorphs. So, the contour from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	currentMorph mainCanvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: 
				(currentMorph privateDisplayBounds quickMerge:
					self canvasToUse boundingRectOfCurrentMorphAfterDraw) ]].
]

{ #category : 'private' }
MorphicCanvas >> doOwnEngine [
	"VectorCanvas needs this."
]

{ #category : 'drawing-widgets' }
MorphicCanvas >> drawButtonIconFromCurrentMorph [
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	currentMorph iconDrawSelector ifNotNil: [ :selector |
		self perform: selector.
		^true ].

	currentMorph magnifiedIcon ifNotNil: [ :theIcon |
		self pvtDrawButtonFaceForm: theIcon.
		^true ].

	^false
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawCloseIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawCollapseIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'morphic' }
MorphicCanvas >> drawCurrentAndSubmorphs [

	self subclassResponsibility
]

{ #category : 'morphic' }
MorphicCanvas >> drawCurrentAsError [
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r := currentMorph localBoundsForError.
	w := r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft+1 to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight + (w negated@1) to: r bottomLeft + (1@ w negated) width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourClearOnCurrentMorph.
]

{ #category : 'morphic' }
MorphicCanvas >> drawCurrentAsOutline [

	currentMorph visible ifTrue: [
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.
		self privateFrameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].
	^true
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawDownIcon [
	"For Scrollbars"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawExpandIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.
]

{ #category : 'morphic' }
MorphicCanvas >> drawHand [
	"For the user Hand. Especially when carrying morphs around."

	self subclassResponsibility.
]

{ #category : 'morphic' }
MorphicCanvas >> drawHighlight: aMorph [

	aMorph displayBounds ifNotNil: [ :r |
		self
			privateFrameAndFillGlobalRect: (r intersect: clipRect)
			fillColor: `Color pink alpha: 0.1`
			borderWidth: 2
			color: `Color black`
			].
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawLeftIcon [
	"For Scrollbars"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawMenuIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawNotExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawPushPinIcon [
	"For Menu stay-up button"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawRightIcon [
	"For Scrollbars"

	self subclassResponsibility.
]

{ #category : 'morphic world' }
MorphicCanvas >> drawRoots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage [
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."
	"Iterate from back to front."

	| allDamage newDamageFromMorphsBelow |
	"Don't need a copy just because nobody will use it afterwards"
	newDamageFromMorphsBelow := backgroundDamage.
	allDamage := Rectangle merging: newDamageFromMorphsBelow.
	rootMorphs size to: 1 by: -1 do: [ :i | | morph morphDamage |
		morph := rootMorphs at: i.
		morphDamage := rootsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			newDamageFromMorphsBelow add: morphDamage.
			allDamage
				ifNil: [ allDamage := morphDamage copy ]
				ifNotNil: [ allDamage updateMerging: morphDamage ]]].
	^allDamage
]

{ #category : 'drawing-windows' }
MorphicCanvas >> drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight [

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawSquareCloseIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawSquareExpandIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawSquareMenuIcon [
	"For Window control buttons"

	self subclassResponsibility.
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s at: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s at: pt font: fontOrNil color: aColor borderWidth: sw borderColor: sc [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor borderWidth: sw borderColor: sc
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: (self fontToUse: fontOrNil) color: aColor ]
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atBaseline: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atBaseline: pt font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atBaseline: pt font: fontOrNil color: aColor borderWidth: sw borderColor: sc [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atBaseline: pt font: font color: aColor borderWidth: sw borderColor: sc
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atCenterX: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		at: pt - ((font widthOfString: s) / 2 @ 0)
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atCenterXBaselineY: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		atBaseline: pt - ((font widthOfString: s) / 2 @ 0)
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atWaist: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self drawString: s from: 1 to: s size atWaist: pt font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atWaistCenter: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font dy |
	font := self fontToUse: fontOrNil.
	dy := currentTransformation doesMirror
		ifFalse: [ font ascent * 0.25 ]
		ifTrue: [ font ascent * -0.25 ].
	^self
		drawString: s
		from: 1 to: s size
		atBaseline: pt + ((font widthOfString: s) negated / 2 @ dy)
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: s atWaistRight: pt font: fontOrNil color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font := self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		atWaist: pt - ((font widthOfString: s) @ 0)
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor [
	"Eventually, convert all senders to the 'Baseline' protocol"

	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0@(font ascent + font lineGap-1))
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor borderWidth: sw borderColor: sc [
	"Eventually, convert all senders to the 'Baseline' protocol"

	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0@(font ascent + font lineGap-1))
		font: font color: aColor
		borderWidth: sw borderColor: sc
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor [
	self subclassResponsibility 
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc [
	"Ignore border if not supported"
	^self drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: font color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| dy |
	dy := currentTransformation doesMirror
		ifFalse: [ font ascent * 0.4 ]
		ifTrue: [ font ascent * -0.4 ].
	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0 @ dy)
		font: font color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	aColor = `Color black` ifFalse: [ | topColor |
		topColor := aColor alphaMixed: 0.25 with: `Color black`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: font
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor := aColor alphaMixed: 0.22 with: `Color white`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + `0@2`
			font: font
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + `0@1`
		font: font
		color: aColor
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc [
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	self drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: false
]

{ #category : 'drawing-text' }
MorphicCanvas >> drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine [
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	| text lines displayScanner leftInRun line possiblyVisible firstLineIndex lastLineIndex |

	text := aTextComposition textComposed.
	lines := aTextComposition lines.
	possiblyVisible := currentTransformation boundsOfInverseTransformOf: clipRect.
	possiblyVisible := (possiblyVisible translatedBy: drawingPosition negated) intersect: (0@0 extent: anExtent).

	displayScanner := MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: text
		foreground: c.
	displayScanner canvas: self.

	leftInRun := 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	firstLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible topLeft max: `0@0`).
	"If this line includes Block Indent highlight, include previous lines until one that doesn't.
	This is needed for correct preparation of blockNestingStartX"
	[firstLineIndex > 1 and: [
		text hasShoutBackgroundColorAt: (lines at: firstLineIndex) first]] whileTrue:
			[
				firstLineIndex := firstLineIndex - 1 ].

	lastLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible bottomRight min: anExtent).
	avoidLastNotFullyVisibleLine ifTrue: [
		line := lines at: lastLineIndex.
		line top + line baseline > anExtent y ifTrue: [
			lastLineIndex := lastLineIndex - 1 ]].
	firstLineIndex to: lastLineIndex do: [ :i |
		line := lines at: i.
		aTextComposition
			displaySelectionInLine: line
			on: self
			textTopLeft: drawingPosition
			selectionColor: sc.
		leftInRun := displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: leftInRun ].
]

{ #category : 'drawing-icons' }
MorphicCanvas >> drawUpIcon [
	"For Scrollbars"

	self subclassResponsibility.
]

{ #category : 'morphic world' }
MorphicCanvas >> drawWorld: aWorldMorph repair: aDamageRecorder [
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage damage worldBackgroundRects loopCount |
	visibleRootMorphs := aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage := visibleRootMorphs collect: [ :m | aDamageRecorder damageReportedFor: m ].
	damage := aDamageRecorder damageReportedOther.
	aDamageRecorder reset.
	self gatherMorphDamage: visibleRootMorphs rootsReportedDamage: visibleRootsDamage addDamageTo: damage.
	worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage.
	"It is possible that some morph's damage gets enlarged due to morphs beneath it.
	If so, act as if 	the updated morph damage was requested damage from the start, and retry.
	Ocassionally this seemed to loop without bounds. Limit the number of retries."
	loopCount := 0.
	[ (self updateAllDamage: damage roots: visibleRootMorphs rootsDamage: visibleRootsDamage 
					backgroundDamage: worldBackgroundRects) and: [loopCount < 10] ] whileTrue: [
		loopCount := loopCount + 1.
		worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage ].

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	^ self drawRoots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects.

	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"
]

{ #category : 'morphic world' }
MorphicCanvas >> drawWorldBackground: aWorldMorph rects: worldBackgroundDamage [
	"Draw worldBackgroundDamage rects for aWorldMorph.
	Do not include submorphs."

	worldBackgroundDamage do: [ :r |
		aWorldMorph drawOn: (self newClipRect: r) ].
]

{ #category : 'drawing-ovals' }
MorphicCanvas >> ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor [
	self subclassResponsibility 
]

{ #category : 'accessing' }
MorphicCanvas >> extent [

	^ form extent
]

{ #category : 'drawing-rectangles' }
MorphicCanvas >> fillRectangle: aRectangle color: aColor [
	self subclassResponsibility 
]

{ #category : 'drawing-rectangles' }
MorphicCanvas >> fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder [
	"
	(BitBltCanvas onForm: Display)
		fillRectangle: (10@10 extent: 300@200) 
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised
		baseColorForBorder: Color red.
	Display forceToScreen.
	"

	borderWidth = 0 ifTrue: [
		^ self fillRectangle: aRectangle color: aColor ].
	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol.
]

{ #category : 'private' }
MorphicCanvas >> fontToUse: fontOrNil [
	"Answer a suitable font, aFont if possible."

	^fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].
]

{ #category : 'accessing' }
MorphicCanvas >> form [

	^ form
]

{ #category : 'drawing-rectangles' }
MorphicCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [
	self subclassResponsibility 
]

{ #category : 'drawing-rectangles' }
MorphicCanvas >> frameRectangle: r borderWidth: borderWidth color: borderColor [
	self subclassResponsibility 
]

{ #category : 'drawing-rectangles' }
MorphicCanvas >> frameRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol [
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| tlColor brColor |
	tlColor := aColor.
	brColor := aColor.
	aSymbol == #raised ifTrue: [
		tlColor := aColor lighter.
		brColor := aColor darker ].

	aSymbol == #inset ifTrue: [
		tlColor := aColor darker.
		brColor := aColor lighter ].

	self frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth
]

{ #category : 'morphic' }
MorphicCanvas >> fullDraw: aMorph [
	"Draw the full Morphic structure on us"
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	currentMorph layoutSubmorphsIfNeeded.

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self canvasToUse drawCurrentAsOutline ifTrue: [
			self outOfMorph.
			^ self]].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph.
]

{ #category : 'morphic' }
MorphicCanvas >> fullDrawHand: aHandMorph [
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	aHandMorph visible ifFalse: [^ self].
	self into: aHandMorph.
	aHandMorph fullDrawHandOn: self.
	self outOfMorph
]

{ #category : 'morphic' }
MorphicCanvas >> fullyCoveredRectangleOf: aMorph [

	"Assume a bit of Anti Aliasing"
	aMorph requiresVectorCanvas ifTrue: [
		^aMorph fullyCoveredRectangle ifNotNil: [ :r | r insetBy: 2 ]].

	^aMorph fullyCoveredRectangle
]

{ #category : 'morphic world' }
MorphicCanvas >> gatherMorphDamage: rootMorphs rootsReportedDamage: rootsReportedDamage addDamageTo: allDamage [
	"For each morph, remove any reported damage that is completely covered by another morph on top.
	Add any  areas that need to be redrawn to uncoveredDamage.
	uncoveredDamage will contain all the areas that need to be repaired. rootMorphsReportedDamage no longer relevant.
	Iterate from front to back"

	1 to: rootMorphs size do: [ :i | | thisMorphReportedDamage |
		thisMorphReportedDamage := rootsReportedDamage at: i.
		thisMorphReportedDamage ifNotNil: [
			"Iterate all morphs up to the one directly above us"
			1 to: i-1 do: [ :j |
				(self fullyCoveredRectangleOf: (rootMorphs at: j)) ifNotNil: [ :coveringR | | nonCoveredParts |
					nonCoveredParts := nil.
					1 to: thisMorphReportedDamage size do: [ :k |
						(thisMorphReportedDamage at: k) ifNotNil: [ :r |
							"If damage is completely covered, remove it. If it is partially covered, just keep uncovered parts."
							(coveringR intersects: r) ifTrue: [
								thisMorphReportedDamage at: k put: nil.
								(coveringR containsRect: r) ifFalse: [
									nonCoveredParts :=nonCoveredParts
										ifNil: [ r areasOutside: coveringR ]
										ifNotNil: [ nonCoveredParts, (r areasOutside: coveringR) ]]]]].
					nonCoveredParts ifNotNil: [
						thisMorphReportedDamage addAll: nonCoveredParts ]]].
			"Store"
			thisMorphReportedDamage do: [ :r |
				r ifNotNil: [
					"DebuggingAid"
					"Display fill: r fillColor: Color random. (Delay forSeconds: 0.01) wait."
					allDamage add: r ]]]].

	"Remove redundant rectangles"
	1 to: allDamage size do: [ :i1 |
		(allDamage at: i1) ifNotNil: [ :r1 |
			1 to: allDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(allDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ 
								allDamage at: i2 put: nil ]]]]]].
]

{ #category : 'accessing' }
MorphicCanvas >> geometryTransformation: aGeometryTransformation [
	"Only used for testing. Usually, transformations are handled as we traverse the Morphs tree."

	currentTransformation := aGeometryTransformation.
]

{ #category : 'drawing-images' }
MorphicCanvas >> image: aForm at: aPoint [
	self subclassResponsibility 
]

{ #category : 'initialization' }
MorphicCanvas >> initializeWithTranslation: aPoint [
	"Set up these only on initialization."

	currentTransformation := MorphicTranslation withTranslation: aPoint.
	cti := 1.
	transformations
		ifNil: [ transformations := OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack := OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].
	currentMorph := nil.
]

{ #category : 'morphic' }
MorphicCanvas >> into: aMorph [
	| locationOrNil previousLast |
	locationOrNil := aMorph location.
	currentMorph := aMorph.
	cti := cti + 1.
	transformations size < cti
		ifTrue: [
			drawingMorphStack add: aMorph.
			currentTransformation := locationOrNil
				ifNotNil: [ currentTransformation composedWith: locationOrNil]
				ifNil: [ currentTransformation copy ].
			transformations add: currentTransformation ]
		ifFalse: [
			drawingMorphStack at: cti put: aMorph.
			previousLast := currentTransformation.
			locationOrNil
				ifNil: [
					currentTransformation := previousLast copy.
					transformations at: cti put: currentTransformation ]
				ifNotNil: [ :location |
					currentTransformation := transformations at: cti.
					"reuse the instance if possible"
					(previousLast class == location class and: [ previousLast class == currentTransformation class ])
						ifTrue: [
							previousLast composedWith: location into: currentTransformation ]
						ifFalse: [
							currentTransformation := previousLast composedWith: location.
							transformations at: cti put: currentTransformation ]
				]
		]
]

{ #category : 'testing' }
MorphicCanvas >> isBoundsFinderCanvas [
	^false
]

{ #category : 'other' }
MorphicCanvas >> isConsistentWithTarget [
	^true
]

{ #category : 'drawing' }
MorphicCanvas >> line: pt1 to: pt2 width: wp color: c [
	self subclassResponsibility 
]

{ #category : 'accessing' }
MorphicCanvas >> newClipRect: aRectangleOrNil [
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ `0@0` corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ]).
	clippingMorphDisplayBounds := nil.
]

{ #category : 'private' }
MorphicCanvas >> nilState [
	"Nil possibly trouble making references. See senders."
	world := nil.
	form := nil.
]

{ #category : 'morphic' }
MorphicCanvas >> outOfMorph [

	drawingMorphStack at: cti put: nil.			"Don't hold any morphs that could be collected"
	cti := cti - 1.
	currentTransformation := transformations at: cti.
	currentMorph := drawingMorphStack at: cti
]

{ #category : 'other' }
MorphicCanvas >> pinArrays [

]

{ #category : 'printing' }
MorphicCanvas >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll:' on: '; print: form.
]

{ #category : 'private-framework' }
MorphicCanvas >> privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor [
	self subclassResponsibility.
]

{ #category : 'private-framework' }
MorphicCanvas >> privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor [
	self subclassResponsibility.
]

{ #category : 'private-framework' }
MorphicCanvas >> privateFrameReverseGlobalRect: r borderWidth: borderWidth [
	self subclassResponsibility.
]

{ #category : 'drawing-roundedRectangles' }
MorphicCanvas >> roundRect: aRectangle color: aColor radius: r [
	self subclassResponsibility 
]

{ #category : 'morphic world' }
MorphicCanvas >> scatterDamageTo: rootMorphs rootsDamageIn: rootsDamage allDamage: allDamage [
	"uncoveredDamage was originally built possible without checking which areas are actually covered by some morph.
	Remove from uncoveredDamage any area that can be assigned to a morph.
	Give it to the topmost morph that can cover it.
	Answer uncoveredDamage, the areas where World background needs to be redrawn.
	Iterate from front to back"

	| unassignedDamage |
	unassignedDamage := allDamage copy.
	1 to: rootMorphs size do: [ :i | | thisMorph thisMorphDamageRect |
		thisMorph := rootMorphs at: i.
		thisMorphDamageRect := nil.
		thisMorph displayFullBounds ifNotNil: [ :thisMorphFullBounds | | addThisMorphDamageToUncoveredDamage |
			addThisMorphDamageToUncoveredDamage := false.
			1 to: unassignedDamage size do: [ :j |
				(unassignedDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphFullBounds) ifTrue: [
						thisMorphDamageRect
							ifNil: [ thisMorphDamageRect := r intersect: thisMorphFullBounds ]
							ifNotNil: [ thisMorphDamageRect updateMerging: (r intersect: thisMorphFullBounds) ].
						(self fullyCoveredRectangleOf: thisMorph)
							ifNil: [ addThisMorphDamageToUncoveredDamage := true ]
							ifNotNil: [ :fullyCovered |
								r areasOutside: fullyCovered do: [ :notCovered | unassignedDamage add: notCovered ].
								unassignedDamage at: j put: nil ]]]].
			addThisMorphDamageToUncoveredDamage ifTrue: [
				unassignedDamage add: thisMorphDamageRect ].
		].
		rootsDamage at: i put: thisMorphDamageRect 
	].

	^unassignedDamage select: [ :r | r notNil ]
]

{ #category : 'private' }
MorphicCanvas >> setClipRect: aRectangle [
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipRect := aRectangle
]

{ #category : 'other' }
MorphicCanvas >> unpinAndFreeMemory [
	form ifNotNil: [
		form == Display ifFalse: [
			form bits unpin ]].
]

{ #category : 'morphic world' }
MorphicCanvas >> updateAllDamage: allDamage roots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage [
	"Detect any morph whose damage would grow because of morphs beneath it.
	If one found, add the new larger damage to allDamage and exit.
	See sender."
	"Iterate from back to front."

	| newDamageFromMorphsBelow |
	newDamageFromMorphsBelow := backgroundDamage copy.
	rootMorphs size to: 1 by: -1 do: [ :i |
		| morph morphPreviousDamage morphDamage |
		morph := rootMorphs at: i.
		morphPreviousDamage := rootsDamage at: i.
		morphDamage := morphPreviousDamage copy.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			newDamageFromMorphsBelow add: morphDamage ].
		morphDamage = morphPreviousDamage ifFalse: [
			allDamage add: morphDamage.
			^true 	]].
	^false
]

{ #category : 'testing' }
MorphicCanvas >> usesVectorEnginePlugin [
	^false
]

{ #category : 'accessing' }
MorphicCanvas >> vectorEngine [
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^nil
]

{ #category : 'initialization' }
MorphicCanvas >> world: aWorldMorph [
	world := aWorldMorph.
	self into: world
]
