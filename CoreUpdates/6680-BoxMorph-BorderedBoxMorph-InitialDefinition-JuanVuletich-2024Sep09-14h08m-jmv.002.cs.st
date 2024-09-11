'From Cuis7.1 [latest update: #6679] on 9 September 2024 at 2:11:02 pm'!
!classDefinition: #BoxMorph category: #'Morphic-Kernel'!
PlacedMorph subclass: #BoxMorph
	instanceVariableNames: 'extent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!BoxMorph commentStamp: 'jmv 9/9/2024 13:26:56' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape.
Subclasses can add things like 'roundedCornerRadius' or such.
Note: Answers false to #submorphsMightProtrude. If you need a box like morph that can hold submorphs that can protrude if (i.e. submorphs may extend beyond box bounds) DONT subclass from here.!

!classDefinition: #BorderedBoxMorph category: #'Morphic-Kernel'!
BoxMorph subclass: #BorderedBoxMorph
	instanceVariableNames: 'color borderWidth borderColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!BorderedBoxMorph commentStamp: 'jmv 9/9/2024 13:27:14' prior: 0!
BorderedRectMorph adds a customizable background color. Instances may have a border, see instanceVariables borderWidth and borderColor. Subclasses can use a variety of border styles: simple, inset, raised.!


!BoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 12:58:22'!
initialize
	super initialize.
	extent := `50@40`.! !

!BoxMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2024 13:08:29'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: `Color lightGreen`.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:01:25'!
ensureMinimumExtent

	| oldExtent |
	oldExtent := extent.
	(self privateExtent: extent) ifTrue: [
		self extentChanged: oldExtent ].! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:09:31'!
extentInOwner: newExtent

	self morphExtent: (self internalizeDistance: newExtent).! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:03:49'!
extentInWorld
	"World coordinates"

	^ (self externalizeDistanceToWorld: extent) ceiling! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:09:50'!
extentInWorld: newExtent
	"world coordinates"

	self morphExtent: (self internalizeDistanceFromWorld: newExtent).! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:14:02'!
fitInto: aRectangle
	"Change the position and extent of this morph. Arguments are owner's coordinates.
	See inheritance: For general, non resizeable morphs, adjust position and scale."

	self morphPosition: aRectangle origin.
	self extentInOwner: aRectangle extent.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:12:18'!
fullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	Note: Answers an integer rectangle"

	"Rethoric question. If it is ever true, call super."
	"(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifFalse: ["
	^ (self externalizeBoundingRectOf: self localBounds) encompassingIntegerRectangle! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:14:18'!
heightInOwner: newHeight

	self extentInOwner: self extentInOwner x @ newHeight! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:25'!
localBounds

	^ `0@0` extent: extent! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:14:26'!
localBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^self localBounds.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:21'!
morphBottomLeft
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ 0 @ extent y! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:03:37'!
morphBottomRight
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ extent! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:00:21'!
morphExtent
	"In our own coordinates!!
	nil if unknown."

	^ extent! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:00:28'!
morphExtent: newExtent
	"In our own coordinates!!"

	| oldExtent |
	extent = newExtent ifFalse: [
		oldExtent := extent.
		(self privateExtent: newExtent) ifTrue: [
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:02:41'!
morphHeight
	"In own's coordinates"

	^ extent y! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:03:04'!
morphHeight: aNumber
	"In our own coordinates!!"

	self morphExtent: extent x@aNumber! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:14:39'!
morphTopLeft
	"Local coordinates.
	Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"

	^`0@0`.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:12'!
morphTopRight
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ extent x @ 0! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:36'!
morphWidth
	"In own's coordinates"

	^ extent x! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:44'!
morphWidth: aNumber
	"In our own coordinates!!"

	self morphExtent: aNumber@extent y! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:10:25'!
position: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self extentInOwner: newExtent.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:04:08'!
privateExtent: aPoint
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent := aPoint max: self minimumExtent.
	^ extent = newExtent
		ifFalse: [ extent := newExtent ]; not! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:00:09'!
rotationCenter
	"In own coordinates."

	^ extent / 2.0.! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:10:46'!
topLeftInOwner

	^ self morphPosition! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 13:11:13'!
widthInOwner: newWidth

	self extentInOwner: newWidth @ self extentInOwner y! !

!BoxMorph methodsFor: 'geometry services' stamp: 'jmv 9/9/2024 13:15:16'!
coversLocalPoint: aLocalPoint
	"Answer true as long as aLocalPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it."

	"If not visible, won't cover any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self localBounds containsPoint: aLocalPoint! !

!BoxMorph methodsFor: 'geometry services' stamp: 'jmv 9/9/2024 13:15:29'!
coversPixel: worldPoint
	"Answer true as long as worldPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it.
	This implementation is cheap, we are a rectangular shape."

	^ self coversLocalPoint:
		(self internalizeFromWorld: worldPoint)! !

!BoxMorph methodsFor: 'geometry services' stamp: 'jmv 9/9/2024 13:15:37'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:
	(See comment and implementation at Morph).
	Note: This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !

!BoxMorph methodsFor: 'geometry testing' stamp: 'jmv 9/9/2024 13:17:58'!
hasVariableExtent
	^ true! !

!BoxMorph methodsFor: 'geometry testing' stamp: 'jmv 9/9/2024 13:18:01'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: on a BoundsFinderCanvas
	to explore affected pixels to deduce actual morph bounds.
	
	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ true! !

!BoxMorph methodsFor: 'geometry testing' stamp: 'jmv 9/9/2024 13:19:24'!
requiresVectorCanvas
	"Widgets can usually run with any kind of Canvas, but not if zoomed or rotated.
	Note: Subclasses that use VectorGraphics for their drawing should answer true.

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ location isPureTranslation not or: [ owner notNil and: [ owner requiresVectorCanvas ]].! !

!BoxMorph methodsFor: 'geometry testing' stamp: 'jmv 9/9/2024 13:13:26'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ false! !

!BoxMorph methodsFor: 'geometry testing' stamp: 'jmv 9/9/2024 13:19:43'!
wantsContour
	"Widget morphs don't need contour.
	See also #knowsOwnLocalBounds and senders."

	^ false! !

!BoxMorph methodsFor: 'layout' stamp: 'jmv 9/9/2024 13:05:19'!
minItemWidth

	^ extent x! !

!BoxMorph methodsFor: 'layout-properties' stamp: 'jmv 9/9/2024 13:20:19'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec! !

!BoxMorph methodsFor: 'testing' stamp: 'jmv 9/9/2024 13:21:25'!
is: aSymbol
	^ aSymbol == #BoxMorph or: [ super is: aSymbol ]! !

!BoxMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/9/2024 13:21:56'!
haloShowsCoordinateSystem
	"We are usually not concerned with this level of detail for Widgets, as they prefer using automatic Layout."

	^ self requiresVectorCanvas! !

!BoxMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/9/2024 13:22:06'!
okayToResizeEasily
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true! !

!BoxMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/9/2024 13:22:26'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.! !

!BoxMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/9/2024 13:22:34'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.! !

!BoxMorph methodsFor: 'window like behavior' stamp: 'jmv 9/9/2024 13:23:40'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self displayBounds.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self position: possibleBounds topLeft extent: possibleBounds extent.! !

!BoxMorph methodsFor: 'window like behavior' stamp: 'jmv 9/9/2024 13:23:34'!
resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self displayBounds].
	self position: boundingBox origin extent: boundingBox extent.! !


!BoxMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/9/2024 13:24:17'!
categoryInNewMorphMenu
	^ 'Basic'! !


!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
borderColor
	^ borderColor! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
borderColor: aColor
	borderColor = aColor ifFalse: [
		borderColor := aColor.
		self redrawNeeded]! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
borderWidth
	^ borderWidth! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth := anInteger max: 0.
		self  ensureMinimumExtent;
			redrawNeeded ].! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
color

	^ color! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/9/2024 12:38:06'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color := aColor.
		self redrawNeeded ]! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 12:38:06'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color gray`! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 13:35:35'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 5! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 12:38:06'!
defaultColor
	^ `Color orange`! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 12:58:32'!
initialize
	super initialize.
	color := self defaultColor.
	borderColor := self defaultBorderColor.
	borderWidth := self defaultBorderWidth.! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 12:38:06'!
noBorder
	self borderWidth: 0.! !

!BorderedBoxMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2024 12:38:06'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.! !

!BorderedBoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 12:38:06'!
extentBorder
	"This is the number of pixels to add to internal minimum to calculate
	my minimumExtent.  "
	
	^ 2 * borderWidth ! !

!BorderedBoxMorph methodsFor: 'testing' stamp: 'jmv 9/9/2024 13:21:36'!
is: aSymbol
	^ aSymbol == #BorderedBoxMorph or: [ super is: aSymbol ]! !

