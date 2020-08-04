'From Cuis 5.0 [latest update: #4330] on 3 August 2020 at 5:01:25 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/3/2020 16:59:07'!
canAdd: aMorph
	self world ifNotNil: [ :w |
		^w canHandle: aMorph ].
	^true! !


!PasteUpMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/3/2020 16:58:11'!
canHandle: aMorph
	^self canvas canDraw: aMorph! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/3/2020 16:57:53'!
canDraw: aMorph
	^true! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 8/3/2020 16:58:06'!
canDraw: aMorph
	^aMorph requiresVectorCanvas not! !


!Morph methodsFor: 'private' stamp: 'jmv 8/3/2020 17:01:17'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	(aCollection allSatisfy: [ :m | self canAdd: m]) ifFalse: [
		^self error: 'We can''t add requested morph. Maybe install and activate VectorGraphics canvas.' ].
	myWorld _ self world.
	otherSubmorphs _ submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs _ aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs _ otherSubmorphs, aCollection]
			ifFalse:[	submorphs _ otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner |
		itsOwner _ m owner.
		itsOwner ifNotNil: [
			itsWorld _ m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [m redrawNeeded]].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [m redrawNeeded].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 8/3/2020 17:00:36'!
privateAddMorph: aMorph atIndex: index

	| oldIndex myWorld itsWorld oldOwner |
	(self canAdd: aMorph) ifFalse: [
		^self error: 'We can''t add requested morph. Maybe install and activate VectorGraphics canvas.' ].
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue:[
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
	] ifFalse:[
		"adding a new morph"
		oldOwner ifNotNil:[
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil:[aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ].
! !

!Morph methodsFor: 'private' stamp: 'jmv 8/3/2020 17:00:40'!
privateAddMorph: aMorph atIndex: index position: aPoint

	| oldIndex myWorld itsWorld oldOwner |
	(self canAdd: aMorph) ifFalse: [
		^self error: 'We can''t add requested morph. Maybe install and activate VectorGraphics canvas.' ].
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue: [
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
		aMorph privatePosition: aPoint.
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		aMorph privatePosition: aPoint.
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

