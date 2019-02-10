'From Cuis 5.0 of 7 November 2016 [latest update: #3589] on 4 February 2019 at 3:31:26 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'AY 2/4/2019 03:26:10'!
atFront
	
	^owner firstSubmorph == self! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'AY 2/4/2019 03:26:20'!
comeToFront

	self atFront ifFalse: [owner addMorphFront: self]! !

!Morph methodsFor: 'user interface' stamp: 'AY 2/4/2019 03:30:37'!
toggleCollapseOrShow
	"If collapsed, show me.
	If visible, collapse me."

	(self visible and: [self atFront])
		ifTrue:  [ self collapse ]
		ifFalse: [ self showAndComeToFront ]! !

