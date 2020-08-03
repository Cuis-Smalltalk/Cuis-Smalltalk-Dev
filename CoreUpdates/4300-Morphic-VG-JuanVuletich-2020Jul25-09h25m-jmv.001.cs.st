'From Cuis 5.0 [latest update: #4296] on 25 July 2020 at 9:31:11 am'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 7/25/2020 09:07:22'!
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas.
	If a total repaint has been requested, return the given rectangle.
	Forget about all damage."
	| answer |
	answer _ totalRepaint ifTrue: [ Array with: aRectangle ].
	self pvtAccessProtect critical: [
		answer ifNil: [
			answer _ OrderedCollection new.
			invalidRects do: [ :r |
				r ifNotNil: [
					(aRectangle containsRect: r)
						ifTrue: [ answer add: r ]
						ifFalse: [ answer add: (r intersect: aRectangle) ].
					]]].
		self pvtReset ].
	^ answer.! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 7/25/2020 08:58:56'!
isCurrentMorphVisible
	"Note: answer true if we are not sure."
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle := currentMorph displayBounds.
	aRectangle ifNil: [ ^true ].
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true
! !

