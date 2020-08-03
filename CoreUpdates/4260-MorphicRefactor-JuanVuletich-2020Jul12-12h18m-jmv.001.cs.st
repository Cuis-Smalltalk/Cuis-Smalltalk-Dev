'From Cuis 5.0 [latest update: #4260] on 12 July 2020 at 12:18:16 pm'!

!Morph methodsFor: 'private' stamp: 'jmv 7/11/2020 21:57:34'!
privateMoveBackMorph: aMorph

	| oldIndex myWorld index |
	myWorld _ self world.
	"aMorph's position changes within in the submorph chain"
	"moving aMorph to front"
	oldIndex _ submorphs indexOf: aMorph.
	"moving aMorph to back"
	index _ submorphs size.
	submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1.
	submorphs at: index put: aMorph.
	myWorld ifNotNil: [aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.! !

!Morph methodsFor: 'private' stamp: 'jmv 7/11/2020 21:58:44'!
privateMoveFrontMorph: aMorph

	| oldIndex myWorld |
	myWorld _ self world.
	"aMorph's position changes within in the submorph chain"
	"moving aMorph to front"
	oldIndex _ submorphs indexOf: aMorph.
	oldIndex-1 to: 1 by: -1 do: [ :i |
		submorphs at: i+1 put: (submorphs at: i)].
	submorphs at: 1 put: aMorph.
	myWorld ifNotNil: [aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/11/2020 21:58:57'!
comeToFront

	self atFront ifFalse: [owner privateMoveFrontMorph: self]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/11/2020 21:58:00'!
goBehind

	owner privateMoveFrontMorph: self.
! !

