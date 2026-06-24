'From Cuis7.9 [latest update: #8017] on 24 June 2026 at 3:44:02 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 24/Jun/2026, 5:34:51 pm (UTC)'!
outOfWorld: aWorld
	"We are about to be removed from the world. Note:
		* aWorld can be nil (due to optimizations in other places)
	Important: Keep this method fast - it is run whenever morphs are removed."

	aWorld ifNil: [ ^self ].
	aWorld stopSteppingMorph: self.
	aWorld taskbar ifNotNil: [ :tb |
		tb wasDeleted: self ].
	self submorphsDo: [ :m | m outOfWorld: aWorld ].! !


!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 24/Jun/2026, 6:38:15 pm (UTC)'!
removeMorph: aMorph

	"Remove the given morph from my submorphs"
	aMorph owner == self ifFalse: [^self].
	super removeMorph: aMorph.
	aMorph outOfWorld: self.! !


!Morph methodsFor: 'private' stamp: 'jmv 24/Jun/2026, 5:24:31 pm (UTC)'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	myWorld := self world.
	otherSubmorphs := submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs := aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs := otherSubmorphs, aCollection]
			ifFalse:[	submorphs := otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner |
		itsOwner := m owner.
		itsOwner ifNotNil: [
			itsWorld := m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [m redrawNeeded]].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [m redrawNeeded].
		(myWorld == itsWorld) ifFalse: [
			m outOfWorld: itsWorld.
			m intoWorld: myWorld ].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 24/Jun/2026, 5:24:41 pm (UTC)'!
privateAddMorph: aMorph atIndex: index position: aPointOrNil

	| oldIndex myWorld itsWorld oldOwner |
	aMorph == self ifTrue: [^ self error: 'can not be own submorph'].
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld := self world.
	oldOwner := aMorph owner.
	(oldOwner == self and: [(oldIndex := submorphs indexOf: aMorph) > 0]) ifTrue: [
		oldIndex = index ifTrue: [ ^self ].
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue: [
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1.
			submorphs at: index put: aMorph.
		] ifFalse: [
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do: [ :i |
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld := aMorph world.
			itsWorld ifNotNil: [aMorph invalidateBounds].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs := submorphs copyReplaceFrom: index to: index-1 with: {aMorph}.
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
		(itsWorld == myWorld) ifFalse: [
			aMorph outOfWorld: itsWorld.
			aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [
		index = 1
			ifTrue: [aMorph redrawNeeded ]
			ifFalse: [aMorph invalidateBounds]].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !


!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 24/Jun/2026, 6:21:15 pm (UTC)'!
possiblyChanged
	| embeddedMorphs w |
	w := self world.
	embeddedMorphs := model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil.
			each outOfWorld: w ]].
	embeddedMorphs do: [ :each|
		each owner == self ifFalse: [
			self addMorphFront: each.
			each hide "Show it only when properly located"]].
	(owner is: #ScrollPane) ifTrue: [
		owner possiblyChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 24/Jun/2026, 2:33:11 pm (UTC)'!
replaceSubmorph: oldMorph by: newMorph
	| index w |
	w := self world.
	index := submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	w ifNotNil: [
		oldMorph removedMorph: w ].
	self privateAddMorph: newMorph atIndex: index.! !

