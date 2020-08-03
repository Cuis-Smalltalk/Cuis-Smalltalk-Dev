'From Cuis 5.0 [latest update: #4292] on 24 July 2020 at 10:57:32 am'!

!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 7/24/2020 10:56:35'!
isCurrentMorphVisible
	"Note: answer true if we are not sure."
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle := self boundingRectOfCurrentMorphAfterDraw.
	aRectangle ifNil: [ ^true ].
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true
! !

!methodRemoval: BitBltCanvas #isCurrentMorphVisible stamp: 'jmv 7/24/2020 10:54:44'!
BitBltCanvas removeSelector: #isCurrentMorphVisible!
