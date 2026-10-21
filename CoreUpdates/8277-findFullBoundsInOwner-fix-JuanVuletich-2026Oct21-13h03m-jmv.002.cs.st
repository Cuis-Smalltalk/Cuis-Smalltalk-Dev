'From Cuis7.9 [latest update: #8266] on 21 October 2026 at 3:37:17 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 21/Oct/2026, 6:37:06 pm (UTC)'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w := owner ]
	ifFalse: [
		w := UISupervisor ui ].
	c := w mainBoundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner := owner.
	owner := w.
	prevNotVisibleFlag := self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	[
		c fullUpdateProtrudingBounds: self.
		answer := self displayFullBounds ] on: Error do: [ :ex |
			answer := (self is: #BoxMorph)
				ifTrue: [ self fullBoundsInOwner ]
				ifFalse: [ (self externalize: 0@0) extent: 100@100 ].
		ex resume ].
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!!"
	prevOwner == owner ifFalse: [
		owner := prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer origin corner: answer corner -(1@0).! !


!PlacedMorph reorganize!
('accessing' location location:)
('geometry' adjustExtent adjustExtentFor: allocHeightForFactor:minLayoutExtentCache: allocWidthForFactor:minLayoutExtentCache: externalize: externalizeBoundingRectOf: externalizeBoundsToWorld: externalizeDistance: fitInto: internalizeDistance: morphAlign:with: morphPosition morphPosition: orbitBy: positionInWorld: referencePosition referencePositionInOwner rotateBy: rotation rotation: rotation:scale: rotationCenter rotationDegrees: scale scale: scaleBy: scaleBy:around: scaleToFitInto: translateBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutExtent: minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation isOrAnyOwnerIsRotated yAxisPointsUp)
('private' fixYAxisDirection privateOwner: privatePosition:)
('layout-properties' assureLayoutSpecForOwner: defaultLayoutEdgesSpec defaultLayoutSizeSpec layoutSpec layoutSpec: useDefaultLayoutSizeSpec)
('testing' isPlacedMorph isProportionalHeight isProportionalWidth)
('category-name' internalize:)
!

