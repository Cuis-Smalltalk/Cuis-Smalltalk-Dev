'From Cuis 4.2 of 25 July 2013 [latest update: #2462] on 17 August 2015 at 11:44:49.371366 am'!

!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 8/17/2015 10:41'!
icon
	^ (self imageForm: 32)
		ifNil: [ Theme current morphsIcon ]
		ifNotNil: [ :form | form icon ]! !


!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 8/17/2015 11:42'!
isCurrentMorphVisible

	| aRectangle |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	"Optimization"
	aRectangle right < clipLeft			ifTrue: [^ false].
	aRectangle left > (clipRight+1)		ifTrue: [^ false].
	aRectangle bottom < clipTop		ifTrue: [^ false].
	aRectangle top > (clipBottom+1)	ifTrue: [^ false].
	^ true
! !

