'From Cuis 5.0 [latest update: #5001] on 17 December 2021 at 10:24:27 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 10:15:38'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates.
	Only subclasses of MovableMorph actually have a position."

	^ 0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/17/2021 10:16:18'!
rotation: radians scale: scale
	"Change the rotation and scale of this morph. Arguments are an angle and a scale.
	Ignored by morphs that are not movable."! !


!MovableMorph reorganize!
('accessing' location location:)
('geometry' adjustExtent allocHeightForFactor: allocWidthForFactor: externalize: externalizeDistance: internalize: internalizeDistance: minimumLayoutExtent morphAlign:with: morphPosition morphPosition: morphPositionInWorld: orbitBy: referencePosition referencePositionInOwner rotateBy: rotation rotation: rotation:scale: rotationCenter scale scale: scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy: externalizeBoundingRectOf: externalizeBoundsToWorld:)
('layout' minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation isOrAnyOwnerIsRotated yAxisPointsUp)
('private' fixYAxisDirection privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isProportionalHeight isProportionalWidth)
!

