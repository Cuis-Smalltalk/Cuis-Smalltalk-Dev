'From Cuis 5.0 [latest update: #4447] on 10 November 2020 at 4:31:24 pm'!

!MovableMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 11/10/2020 16:14:40'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^self "Grab me"! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 11/10/2020 16:15:25'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^nil  		"MovableMorphs can be grabbed and moved around with the hand"! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/10/2020 16:09:55'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds intersect: clipRect.
	r hasPositiveExtent ifFalse: [r _ clipRect ].
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight -(w@0) to: r bottomLeft -(0@w)width: w color: `Color yellow`! !


!MovableMorph reorganize!
('accessing' location)
('geometry' allocHeightForFactor: allocWidthForFactor: externalize: externalizeDisplayBounds: externalizeDistance: internalize: internalizeDistance: minimumLayoutExtent morphAlign:with: morphPosition morphPosition: morphPositionInWorld: orbitBy: referencePosition referencePosition: rotateBy: rotation: rotation:scale: rotationDegrees: scale scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('private' privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isProportionalHeight isProportionalWidth)
!

