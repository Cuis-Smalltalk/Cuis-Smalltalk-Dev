'From Cuis7.5 [latest update: #7624] on 30 September 2025 at 10:12:28 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 10:07:34'!
moveMorphBack: aMorph
	"Make aMorph our last submorph, the one below all its siblings.
	aMorph must be our submorph."

	aMorph owner == self ifFalse: [
		self error: 'Morph is not our submorph' ].
	^self privateAddMorph: aMorph atIndex: submorphs size! !


!Morph methodsFor: 'private' stamp: 'jmv 9/30/2025 09:47:36'!
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
		submorphs := submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
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


!SystemWindow methodsFor: 'menu' stamp: 'jmv 9/30/2025 10:07:46'!
sendToBack
	| thisWorld |
	thisWorld := self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld moveMorphBack: self ]! !

