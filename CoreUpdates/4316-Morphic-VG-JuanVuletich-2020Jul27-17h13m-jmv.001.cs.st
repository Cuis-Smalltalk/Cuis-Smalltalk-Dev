'From Cuis 5.0 [latest update: #4312] on 27 July 2020 at 5:13:53 pm'!

!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 7/27/2020 17:13:44'!
isCurrentMorphVisible
	"Note: answer true if we are not sure."
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle := currentMorph displayBounds.
	aRectangle ifNil: [ ^true ].
	(currentMorph firstOwnerSuchThat: [ :m | m is: #HandMorph ]) notNil ifTrue: [ ^true ].

self flag: #jmvHacks.
	true ifTrue: [ ^true ].
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true
! !

