'From Cuis 5.0 of 7 November 2016 [latest update: #3789] on 10 June 2019 at 10:59:02 am'!

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 6/10/2019 10:39:02'!
clippingRectForCurrentMorph
	"In form coordinates"

	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds! !

!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 6/10/2019 10:47:56'!
isCurrentMorphVisible

	| aRectangle |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle _ self clippingRectForCurrentMorph.
	"Optimization"
	aRectangle right < clipLeft			ifTrue: [^ false].
	aRectangle left > (clipRight+1)		ifTrue: [^ false].
	aRectangle bottom < clipTop		ifTrue: [^ false].
	aRectangle top > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !


!Morph methodsFor: 'geometry' stamp: 'jmv 6/10/2019 10:53:51'!
externalizeDisplayBounds: r

	| inOwners |
	"All senders of #displayBoundsOfTransformOf: should be rethought..."
	self flag: #jmvVer2.

	inOwners _ location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeDisplayBounds: inOwners ]
		ifNil: [ inOwners ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 6/10/2019 10:44:13'!
clippingRectForCurrentMorph
	"This only works reasonably with BitBltCanvas (where submorph clipping is only about Rectangles (aligned with target form pixel grille).
	For submorph cliping in VectorCanvas we use #currentOwnerIfClips:, and this 'clippingRect' is just an optimization of the area to be redrawn.
	So, we need a better name than #clippingRectForCurrentMorph"
	self revisar.
	self subclassResponsibility! !

!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 6/10/2019 10:34:59'!
isCurrentMorphVisible
	self subclassResponsibility! !

