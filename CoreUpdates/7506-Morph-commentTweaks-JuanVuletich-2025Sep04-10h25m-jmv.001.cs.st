'From Cuis7.5 [latest update: #7505] on 4 September 2025 at 10:25:42 am'!

!Morph methodsFor: 'structure' stamp: 'jmv 8/31/2025 18:12:28'!
allOwnersDo: aBlock
	"Evaluate aBlock with all owners of the receiver.
	Start from our owner, go up to top owner."
	
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]! !

!Morph methodsFor: 'structure' stamp: 'jmv 8/31/2025 18:11:35'!
allOwnersReverseDo: aBlock
	"Evaluate aBlock with all owners of the receiver.
	Start from top owner, go down to our owner."

	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ]! !

!Morph methodsFor: 'structure' stamp: 'jmv 8/31/2025 18:12:34'!
withAllOwnersDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners.
	Start from us, go up to top owner."

	aBlock value: self.
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]! !

!Morph methodsFor: 'structure' stamp: 'jmv 8/31/2025 18:11:48'!
withAllOwnersReverseDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners.
	Start from top owner, go down to us."

	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ].
	aBlock value: self! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 9/1/2025 10:23:07'!
visible
	"Answer if I am visible -- default is true.
	Store value of 'hidden' instead, because flags default to false."

	^ (self privateFlagAt: 3) not! !

