'From Cuis 5.0 [latest update: #4905] on 8 October 2021 at 10:05:48 am'!

!Morph methodsFor: 'private' stamp: 'jmv 10/8/2021 10:05:28'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	(aCollection allSatisfy: [ :m | self canAdd: m]) ifFalse: [
		(self confirm: 'Some requested morph requires VectorGraphics support. Do you want to install and activate it?')
			ifTrue: [
				Feature require: 'VectorGraphics'.
				MorphicCanvas activeSubclass: (Smalltalk at: #VectorCanvas) ]
			ifFalse: [
				^self error: 'We can''t add requested morphs.' ]].
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

!Morph methodsFor: 'private' stamp: 'jmv 10/8/2021 10:05:31'!
privateAddMorph: aMorph atIndex: index position: aPointOrNil

	| oldIndex myWorld itsWorld oldOwner |
	(self canAdd: aMorph) ifFalse: [
		(self confirm: 'The requested morph requires VectorGraphics support. Do you want to install and activate it?')
			ifTrue: [
				Feature require: 'VectorGraphics'.
				MorphicCanvas activeSubclass: (Smalltalk at: #VectorCanvas) ]
			ifFalse: [
				^self error: 'We can''t add requested morph.' ]].
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
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph invalidateBounds].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [
		index = 1
			ifTrue: [aMorph redrawNeeded ]
			ifFalse: [aMorph invalidateBounds]].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

