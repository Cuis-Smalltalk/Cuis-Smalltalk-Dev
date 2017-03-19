'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 15 March 2017 at 2:07:10 pm'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 3/15/2017 14:06:54'!
maximumUsableAreaInWorld: aWorldOrNil

	| allowedArea |
	allowedArea _ Display boundingBox.
	aWorldOrNil ifNotNil: [
		allowedArea _ allowedArea intersect: aWorldOrNil viewBox.
		aWorldOrNil taskbar ifNotNil: [ :tb |
			tb morphBoundsInWorld ifNotNil: [ :r |
				allowedArea _ (allowedArea areasOutside: r) first ]]].
	^allowedArea
! !

