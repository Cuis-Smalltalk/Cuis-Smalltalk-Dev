'From Cuis 4.2 of 25 July 2013 [latest update: #1972] on 6 April 2014 at 6:20:24.958135 pm'!

!Morph methodsFor: 'structure' stamp: 'jmv 3/30/2014 17:36'!
allOwnersReverseDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ]! !

!Morph methodsFor: 'structure' stamp: 'jmv 3/30/2014 17:33'!
withAllOwnersReverseDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ].
	aBlock value: self! !


!Morph methodsFor: 'structure' stamp: 'jmv 3/30/2014 17:36'!
allOwnersDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]! !

!Morph methodsFor: 'structure' stamp: 'jmv 3/30/2014 17:34'!
withAllOwnersDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock value: self.
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]! !

