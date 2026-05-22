"
PlacedMorphs have a location instance variable, and therefore a specific position (scale and rotation) inside the space defined by their owner. They can move to different places (by user action or by code), and they can be zoomed (i.e. scaled) and rotated.

The location instance variable holds a kind of GeometryTransformation. This defines the local coordinate system of their inner space, used to interpret positions.
Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.

"
Class {
	#name : 'PlacedMorph',
	#superclass : 'Morph',
	#instVars : [
		'location',
		'layoutSpec'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
PlacedMorph class >> categoryInNewMorphMenu [
	^ 'Vector Graphics'
]

{ #category : 'dropping/grabbing' }
PlacedMorph >> aboutToBeGrabbedBy: aHand [
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."
	"Added isSticky check"
	self isSticky ifTrue: [^nil].
	^self "Grab me"
]

{ #category : 'geometry' }
PlacedMorph >> adjustExtent [
	"Some morphs may need to adjust their extent after some geometry change
	either in itself, some submorpy, or an owner.
	These include morphs intended to be included in PluggableScrollPanes as scrolled content.
	See implementors. NOP by default."
]

{ #category : 'geometry' }
PlacedMorph >> adjustExtentFor: aBlock [
	"For morphs with variable extent, ensure extent is at least minimumExtent.
	See implementors. See senders."
]

{ #category : 'geometry' }
PlacedMorph >> allocHeightForFactor: aFactor minLayoutExtentCache: cachedValues [

	"Morphs with LayoutSizeSpecs (to be used in LinearLayoutMorphs) may specify
	proportional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := (self minimumLayoutExtent: cachedValues) y.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * layoutSpec proportionalLayoutHeight) ]
]

{ #category : 'geometry' }
PlacedMorph >> allocWidthForFactor: aFactor minLayoutExtentCache: cachedValues [

	"Morphs with LayoutSizeSpecs (to be used in LinearLayoutMorphs) may specify
	proportional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := (self minimumLayoutExtent: cachedValues) x.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * layoutSpec proportionalLayoutWidth) ]
]

{ #category : 'layout-properties' }
PlacedMorph >> assureLayoutSpecForOwner: toBeOwner [
	"If we haven't been set a layout spec yet, or we carry one that can no longe be used,
	set a default one for our new context."

	(toBeOwner isLayoutSpecOk: layoutSpec ) ifFalse: [
		layoutSpec := nil ].

	layoutSpec ifNil: [
		layoutSpec := toBeOwner defaultLayoutSpecFor: self.
		layoutSpec morph: self ].
]

{ #category : 'layout-properties' }
PlacedMorph >> defaultLayoutEdgesSpec [
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a FormLayout.
	Not really useful in practice, just _some_  default."

	^LayoutEdgesSpec
		tlEdgesWeight: 0.5@0.5 offset: -100 @ -10
		brEdgesWeight: 0.5@0.5 offset: 100 @ 10
]

{ #category : 'layout-properties' }
PlacedMorph >> defaultLayoutSizeSpec [
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec useAll
]

{ #category : 'class membership' }
PlacedMorph >> externalize: aPoint [
	"aPoint is in own coordinates. Answer is in owner's coordinates."

	^ location externalizePosition: aPoint.
]

{ #category : 'geometry' }
PlacedMorph >> externalizeBoundingRectOf: aRectangle [

	^location externalBoundingRectOf: aRectangle.
]

{ #category : 'geometry' }
PlacedMorph >> externalizeBoundsToWorld: aRectangle [
	"aRectangle is in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners := location externalBoundingRectOf: aRectangle.
	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: inOwners ]
		ifNil: [ inOwners ]
]

{ #category : 'geometry' }
PlacedMorph >> externalizeDistance: aPoint [
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ (location externalizeDelta: aPoint) abs
]

{ #category : 'geometry' }
PlacedMorph >> fitInto: aRectangle [
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxMorph, and maybe others will not scale, but resize, changing the actual extent."

	self scaleToFitInto: aRectangle.
]

{ #category : 'private' }
PlacedMorph >> fixYAxisDirection [
	"Ensure the direction of the Y axis used by our location for coordinate transformations matches our #yAxisPointsUp."

	| ownersYAxisPointsUp |
	ownersYAxisPointsUp := owner ifNil: [false] ifNotNil: [owner yAxisPointsUp].
	location := location invertingYAxis: (self yAxisPointsUp = ownersYAxisPointsUp) not
]

{ #category : 'geometry testing' }
PlacedMorph >> hasOwnLocation [
	^true
]

{ #category : 'initialization' }
PlacedMorph >> initialize [
	"initialize the state of the receiver"

	super initialize.
	location := MorphicTranslation new.
	self fixYAxisDirection.
]

{ #category : 'category-name' }
PlacedMorph >> internalize: aPoint [
	"aPoint is in owner's coordinates. Answer is in own coordinates."

	^ location internalizePosition: aPoint
]

{ #category : 'geometry' }
PlacedMorph >> internalizeDistance: aPoint [
	"aPoint is a delta in owner's coordinates. Answer is in own coordinates."

	^ location internalizeDelta: aPoint
]

{ #category : 'geometry testing' }
PlacedMorph >> isOrAnyOwnerIsRotated [
	"True if we or any owner is rotated.
	A zoomed widget will answer false, but a rotated one will answer true (even if only
		some owner is rotated)."

	^ location doesNotRotate not or: [ owner notNil and: [ owner isOrAnyOwnerIsRotated ]].
]

{ #category : 'testing' }
PlacedMorph >> isPlacedMorph [
	"Meaning that:
	- we can moved around by the user or by code
	- we can be scaled
	- maybe we can be resized
	This all means, for instance, that we can be added to LayoutMorphs."

	^true
]

{ #category : 'testing' }
PlacedMorph >> isProportionalHeight [
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ layoutSpec
		ifNil: [ false ]
		ifNotNil: [ :ls | ls isProportionalHeight ]
]

{ #category : 'testing' }
PlacedMorph >> isProportionalWidth [
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ layoutSpec
		ifNil: [ false ]
		ifNotNil: [ :ls | ls isProportionalWidth ]
]

{ #category : 'layout-properties' }
PlacedMorph >> layoutSpec [
	"Layout specific. Return the layout spec describing where the
	receiver should appear in an owning layout.
	If asked before setting it, and before adding us to some owner,
	just answer nil."

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	owner notNil ifTrue: [
		self assureLayoutSpecForOwner: owner ].
	^ layoutSpec.
]

{ #category : 'layout-properties' }
PlacedMorph >> layoutSpec: aLayoutSpec [
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
]

{ #category : 'accessing' }
PlacedMorph >> location [
	^location
]

{ #category : 'accessing' }
PlacedMorph >> location: aGeometryTransformation [
	location := aGeometryTransformation.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self fixYAxisDirection.
	self redrawNeeded.
]

{ #category : 'layout' }
PlacedMorph >> minimumLayoutExtent: cachedValues [
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self minimumExtent.
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]
]

{ #category : 'layout' }
PlacedMorph >> minimumShrinkExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent := self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumShrinkWidth @ layoutSpec minimumShrinkHeight ]
]

{ #category : 'layout' }
PlacedMorph >> minimumShrinkHeight [
	"This returns the minimum height that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkHeight |
	minShrinkHeight := self minimumExtent y.
	^ layoutSpec
		ifNil: [ minShrinkHeight ]
		ifNotNil: [ minShrinkHeight max: layoutSpec minimumShrinkHeight ]
]

{ #category : 'layout' }
PlacedMorph >> minimumShrinkWidth [
	"This returns the minimum width that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	| minShrinkWidth |
	minShrinkWidth := self minimumExtent x.
	^ layoutSpec
		ifNil: [ minShrinkWidth ]
		ifNotNil: [ minShrinkWidth max: layoutSpec minimumShrinkWidth ]
]

{ #category : 'geometry' }
PlacedMorph >> morphAlign: aPoint with: anotherPoint [
	^ self morphPosition: self morphPosition + anotherPoint - aPoint
]

{ #category : 'geometry' }
PlacedMorph >> morphPosition [
	"Answer our position inside our owner, in owner's coordinates."

	^ location translation
]

{ #category : 'geometry' }
PlacedMorph >> morphPosition: aPoint [
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation := location translation.
	location := location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
]

{ #category : 'initialization' }
PlacedMorph >> openInWorld: aWorld [
	"Add this morph to the requested World."

	aWorld addMorph: self.
	
	"We could have already set our location, or maybe #noteNewOwner: was redefined to set it.
	Position us near the hand only if we haven't been located somewhere already."
	(location isIdentity or: [ location isPureMirroring ])
		ifTrue: [ "No real position set yet"
			aWorld center: self near: aWorld activeHand morphPosition ].
]

{ #category : 'geometry' }
PlacedMorph >> orbitBy: radians [
	"Rotate the receiver around the origin (0@0) in owner coordinates.
	Argument is an angle (possibly negative), to be added to current rotation."

	location := (AffineTransformation withRadians: radians) composedWith: location.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
PlacedMorph >> positionInWorld: newPositionInWorld [
	"Change the position of this morph.
	Argument is in world coordinates."

	| newPositionInOwner |
	newPositionInOwner := owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	location := location withTranslation: newPositionInOwner.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'private' }
PlacedMorph >> privateOwner: aMorph [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner := owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition := self positionInWorld ].
	owner := aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location := location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent := owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!"
					]].
	self fixYAxisDirection.
]

{ #category : 'private' }
PlacedMorph >> privatePosition: aPoint [
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	location := location withTranslation: aPoint
]

{ #category : 'geometry' }
PlacedMorph >> referencePosition [
	^self externalizeToWorld: self rotationCenter
]

{ #category : 'geometry' }
PlacedMorph >> referencePositionInOwner [
	^self externalize: self rotationCenter
]

{ #category : 'geometry' }
PlacedMorph >> rotateBy: radians [
	"Change the rotation of this morph. Argument is an angle (possibly negative).
	Note: the rotation is done around our #rotationCenter. This mimics the behavior of the rotation halo handle."

	| r |
	r := self yAxisPointsUp ifTrue: [ radians negated ] ifFalse: [ radians ].
	location := (location composedWith: (
		AffineTransformation withRadians: r around: self rotationCenter)).
	self fixYAxisDirection.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
PlacedMorph >> rotation [
	^location radians
]

{ #category : 'geometry' }
PlacedMorph >> rotation: radians [
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

    self rotation: radians scale: location scale.
]

{ #category : 'geometry' }
PlacedMorph >> rotation: radians scale: scale [
	"Change the rotation and scale of this morph. Arguments are an angle and a scale."

	| r |
	r := self yAxisPointsUp ifTrue: [ radians negated ] ifFalse: [ radians ].
	location := location withRotation: r scale: scale.
	self fixYAxisDirection.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
PlacedMorph >> rotationCenter [
	"In own coordinates.
	Subclasses might redefine if desired."

	self valueOfProperty: #rotationCenter ifPresentDo: [ :p | ^p ].
	self displayFullBounds ifNotNil: [ :r |
		^self setProperty: #rotationCenter toValue: (self internalizeFromWorld: r center) ].
	^`0@0`.
]

{ #category : 'geometry' }
PlacedMorph >> scale [
	^location scale
]

{ #category : 'geometry' }
PlacedMorph >> scale: scale [
	"Change the scale of this morph. Arguments are an angle and a scale."
	location := location withScale: scale.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
PlacedMorph >> scaleBy: scaleFactor [
	"Change the scale of this morph. Argument is a factor."
	location := location scaledBy: scaleFactor.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.
]

{ #category : 'geometry' }
PlacedMorph >> scaleBy: factor around: center [
	| delta |
	delta := center - self morphPosition.
	self translateBy: delta; scaleBy: factor; translateBy: delta negated * factor
]

{ #category : 'geometry' }
PlacedMorph >> scaleToFitInto: aRectangle [
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxMorph, and maybe others will not scale, but resize, changing the actual extent."

	| currentBounds scale slackForAntiAliasing |
	currentBounds := self findFullBoundsInOwner.
	slackForAntiAliasing := 0.
	scale := (aRectangle width-slackForAntiAliasing / (currentBounds width-slackForAntiAliasing))
		min: (aRectangle height-slackForAntiAliasing / (currentBounds height-slackForAntiAliasing)).
	scale := scale asFloat.
	self scaleBy: scale.

	currentBounds := self findFullBoundsInOwner.
	self morphPosition: aRectangle origin - (currentBounds origin - self morphPosition) + (aRectangle extent - currentBounds extent // 2).
]

{ #category : 'geometry' }
PlacedMorph >> translateBy: aPoint [
	location := location translatedBy: aPoint.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded
]

{ #category : 'layout-properties' }
PlacedMorph >> useDefaultLayoutSizeSpec [
	"Set our default for LinearLayout. Answer it for convenience."

	self layoutSpec: self defaultLayoutSizeSpec.
	^layoutSpec
]

{ #category : 'geometry testing' }
PlacedMorph >> yAxisPointsUp [
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^false
]
