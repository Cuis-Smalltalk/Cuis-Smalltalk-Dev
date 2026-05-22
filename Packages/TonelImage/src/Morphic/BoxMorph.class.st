"
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Shapes that are not strictly rectangles, but close enough, including rectangles with rounded corners are possible too. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape.
Subclasses can add things like 'roundedCornerRadius' or such.
Note: Answers false to #submorphsMightProtrude. If you need a box like morph that can hold submorphs that can protrude it (i.e. submorphs may extend beyond box bounds) DONT subclass from here.
"
Class {
	#name : 'BoxMorph',
	#superclass : 'PlacedMorph',
	#instVars : [
		'extent'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
BoxMorph class >> categoryInNewMorphMenu [
	^ 'Basic'
]

{ #category : 'private' }
BoxMorph >> adjustExtentFor: aBlock [
	"For morphs with variable extent, ensure extent is at least minimumExtent.
	This may mean do the same with owner.
	See implementors. See senders."

	(layoutSpec notNil and: [ owner notNil ]) ifTrue: [
		^ owner adjustExtentFor: [ self basicAdjustExtentFor: aBlock ]].
	self basicAdjustExtentFor: aBlock
]

{ #category : 'private' }
BoxMorph >> basicAdjustExtentFor: aBlock [
	"Ensure extent is at least minimumExtent.
	This may mean do the same with owner.
	See implementors. See senders."

	| forceMinimumExtent |
	forceMinimumExtent := extent = self minimumExtent.
	aBlock value.
	self morphExtent: (forceMinimumExtent ifTrue: [`0@0`] ifFalse: [extent]).
]

{ #category : 'geometry services' }
BoxMorph >> coversLocalPoint: aLocalPoint [
	"Answer true as long as aLocalPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it."

	"If not visible, won't cover any point at all."
	self visible ifFalse: [ ^false ].

	"We know our local bounds, and completely fill them."
	^ self localBounds containsPoint: aLocalPoint
]

{ #category : 'geometry services' }
BoxMorph >> coversPixel: worldPoint [
	"Answer true as long as worldPoint is inside our shape even if:
		- a submorph (above us) also covers it
		- a sibling that is above us or one of their submorphs also covers it.
	This implementation is cheap, we are a rectangular shape."

	^ self coversLocalPoint:
		(self internalizeFromWorld: worldPoint)
]

{ #category : 'initialization' }
BoxMorph >> defaultExtent [
	^`50@40`.
]

{ #category : 'layout-properties' }
BoxMorph >> defaultLayoutSizeSpec [
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec keepMorphExtent
]

{ #category : 'drawing' }
BoxMorph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: `Color lightGreen`.
]

{ #category : 'geometry' }
BoxMorph >> ensureMinimumExtent [
	"Make our extent no smaller than #minimumExtent"

	self morphExtent:
		(extent
			ifNotNil: [extent max: self minimumExtent]
			ifNil: [self minimumExtent]).
]

{ #category : 'geometry' }
BoxMorph >> extentChanged: oldExtent [
	"Our extent changed. oldExtent is provided in case it is useful."
]

{ #category : 'geometry' }
BoxMorph >> extentInOwner: newExtent [

	self morphExtent: (self internalizeDistance: newExtent).
]

{ #category : 'geometry' }
BoxMorph >> extentInWorld [
	"World coordinates"

	^ (self externalizeDistanceToWorld: extent) ceiling
]

{ #category : 'geometry' }
BoxMorph >> extentInWorld: newExtent [
	"world coordinates"

	self morphExtent: (self internalizeDistanceFromWorld: newExtent).
]

{ #category : 'private' }
BoxMorph >> fitInOwner [

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: (owner displayBounds insetBy: Theme current fullScreenDeskMargin) .
	self morphPosition: trialRect origin + delta.
]

{ #category : 'geometry' }
BoxMorph >> fitInto: aRectangle [
	"Change the position and extent of this morph. Arguments are owner's coordinates.
	See inheritance: For general, non resizeable morphs, adjust position and scale."

	self morphPosition: aRectangle origin.
	self extentInOwner: aRectangle extent.
]

{ #category : 'geometry' }
BoxMorph >> fullBoundsInOwner [
	"Find and answer full bounds in whatever owner.
	Note: Answers an integer rectangle"

	"Rethoric question. If it is ever true, call super."
	"(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifFalse: ["
	^ (self externalizeBoundingRectOf: self localBounds) encompassingIntegerRectangle
]

{ #category : 'geometry services' }
BoxMorph >> fullIncludesPixel: worldPoint [
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsDo: [ :m |
			(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.
]

{ #category : 'window like behavior' }
BoxMorph >> fullScreen [
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
	self position: possibleBounds topLeft extent: possibleBounds extent.
]

{ #category : 'halos and balloon help' }
BoxMorph >> haloShowsCoordinateSystem [
	"We are usually not concerned with this level of detail for Widgets, as they prefer using automatic Layout."

	^ self requiresVectorCanvas
]

{ #category : 'geometry testing' }
BoxMorph >> hasVariableExtent [
	^ true
]

{ #category : 'geometry' }
BoxMorph >> heightInOwner: newHeight [

	self extentInOwner: self extentInOwner x @ newHeight
]

{ #category : 'geometry services' }
BoxMorph >> includesPixel: worldPoint [
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:
	(See comment and implementation at Morph).
	Note: This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Also see #ownsPixel:, #coversPixel:
	Also see #fullIncludesPixel:, #topMorphAt:, #topUnlockedMorphAt:"

	^ self visible and: [self coversPixel: worldPoint].
]

{ #category : 'initialization' }
BoxMorph >> initialize [
	super initialize.
	extent := self defaultExtent.
]

{ #category : 'testing' }
BoxMorph >> is: aSymbol [
	^ aSymbol == #BoxMorph or: [ super is: aSymbol ]
]

{ #category : 'geometry testing' }
BoxMorph >> knowsOwnLocalBounds [
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: on a BoundsFinderCanvas
	to explore affected pixels to deduce actual morph bounds.
	
	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ true
]

{ #category : 'geometry' }
BoxMorph >> localBounds [

	^ `0@0` extent: extent
]

{ #category : 'geometry' }
BoxMorph >> localBoundsForError [
	"Only to be used for drawing in an error condition. See senders."

	^self localBounds.
]

{ #category : 'layout' }
BoxMorph >> minItemWidth [

	^ extent x
]

{ #category : 'geometry' }
BoxMorph >> minimumExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `1@1`
]

{ #category : 'layout' }
BoxMorph >> minimumLayoutExtent: cachedValues [
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self externalizeDistance: self minimumExtent.
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]
]

{ #category : 'geometry' }
BoxMorph >> morphBottomLeft [
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ 0 @ extent y
]

{ #category : 'geometry' }
BoxMorph >> morphBottomRight [
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ extent
]

{ #category : 'geometry' }
BoxMorph >> morphExtent [
	"In our own coordinates!
	nil if unknown."

	^ extent
]

{ #category : 'geometry' }
BoxMorph >> morphExtent: newExtent [
	"In our own coordinates!"

	| oldExtent |
	oldExtent := extent.
	(self privateExtent: newExtent) ifTrue: [
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
]

{ #category : 'geometry' }
BoxMorph >> morphHeight [
	"In own's coordinates"

	^ extent y
]

{ #category : 'geometry' }
BoxMorph >> morphHeight: aNumber [
	"In our own coordinates!"

	self morphExtent: extent x@aNumber
]

{ #category : 'geometry' }
BoxMorph >> morphTopLeft [
	"Local coordinates.
	Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"

	^`0@0`.
]

{ #category : 'geometry' }
BoxMorph >> morphTopRight [
	"Local coordinates
	Must be consistent with #morphTopLef and #morphExtent"

	^ extent x @ 0
]

{ #category : 'geometry' }
BoxMorph >> morphWidth [
	"In own's coordinates"

	^ extent x
]

{ #category : 'geometry' }
BoxMorph >> morphWidth: aNumber [
	"In our own coordinates!"

	self morphExtent: aNumber@extent y
]

{ #category : 'halos and balloon help' }
BoxMorph >> okayToResizeEasily [
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true
]

{ #category : 'halos and balloon help' }
BoxMorph >> okayToRotateEasily [
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.
]

{ #category : 'halos and balloon help' }
BoxMorph >> okayToScaleEasily [
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.
]

{ #category : 'geometry' }
BoxMorph >> position: newPos extent: newExtent [
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self extentInOwner: newExtent.
]

{ #category : 'geometry' }
BoxMorph >> privateExtent: aPoint [
	"May or may not honor argument: #minimumExtente will be taken into account,
	as well as other possible restrictions by subclasses. See implementors.
	Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent := aPoint max: self minimumExtent.
	^ extent = newExtent
		ifFalse: [ extent := newExtent ]; not
]

{ #category : 'geometry testing' }
BoxMorph >> requiresVectorCanvas [
	"Widgets can usually run with any kind of Canvas, but not if zoomed or rotated.
	Note: Subclasses that use VectorGraphics for their drawing should answer true.

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ location isPureTranslation not or: [ owner notNil and: [ owner requiresVectorCanvas ]].
]

{ #category : 'window like behavior' }
BoxMorph >> resize: boundingBox [
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self displayBounds].
	self position: boundingBox origin extent: boundingBox extent.
]

{ #category : 'geometry' }
BoxMorph >> rotationCenter [
	"In own coordinates."

	^ extent / 2.0.
]

{ #category : 'geometry testing' }
BoxMorph >> submorphsMightProtrude [
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ false
]

{ #category : 'geometry' }
BoxMorph >> topLeftInOwner [

	^ self morphPosition
]

{ #category : 'submorphs-accessing' }
BoxMorph >> topMorphAt: worldPoint satisfying: aCondition [
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint, 
	satisfies the condition block, and is not covered by any other morph. 
	Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	(self visible and: [ aCondition value: self ]) ifFalse: [ ^ nil ].

	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [ self clipsSubmorphs not ] ]) ifTrue: [
		submorphs do: [ :submorph |
			(submorph topMorphAt: worldPoint satisfying: aCondition)
				ifNotNil: [ :foundMorph | ^ foundMorph ] ] ].
	doWeIncludePixel ifTrue: [ ^ self ].

	^nil
]

{ #category : 'submorphs-accessing' }
BoxMorph >> topUnlockedMorphAt: worldPoint [
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	self isLocked ifTrue: [ ^nil ].
	
	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].
	
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil
]

{ #category : 'geometry testing' }
BoxMorph >> wantsContour [
	"Widget morphs don't need contour.
	See also #knowsOwnLocalBounds and senders."

	^ false
]

{ #category : 'geometry' }
BoxMorph >> widthInOwner: newWidth [

	self extentInOwner: newWidth @ self extentInOwner y
]
