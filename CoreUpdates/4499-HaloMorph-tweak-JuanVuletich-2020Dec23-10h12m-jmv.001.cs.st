'From Cuis 5.0 [latest update: #4494] on 23 December 2020 at 10:12:54 am'!

!HaloMorph methodsFor: 'initialization' stamp: 'jmv 12/22/2020 16:57:41'!
delete

	super delete.
	target ifNotNil: [ target redrawNeeded ].! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/22/2020 16:59:58'!
addHalo: evt
	| halo |
	self displayBoundsForHalo ifNotNil: [ :r |
		halo _ HaloMorph new.
		halo popUpFor: self event: evt.
		halo morphBounds: r ].
	self whenUIinSafeState: [self redrawNeeded].
	^halo! !

