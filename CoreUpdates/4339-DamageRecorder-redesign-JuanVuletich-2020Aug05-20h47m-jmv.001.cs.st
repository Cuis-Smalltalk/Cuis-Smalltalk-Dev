'From Cuis 5.0 [latest update: #4338] on 5 August 2020 at 8:48:09 pm'!

!Morph methodsFor: 'updating' stamp: 'jmv 8/5/2020 12:17:02'!
invalidateBounds
	"Report that the area occupied by this morph should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self invalidateDisplayRect: self displayBoundsOrBogus fromSubmorph: nil for: owner.! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2020 12:23:18'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean
	Store value of 'hidden', because flags default to false."

	self visible == aBoolean
		ifTrue: [ ^ self ].
	aBoolean
		ifTrue: [ self redrawNeeded ]
		ifFalse: [ self invalidateBounds ].
	self privateFlagAt: 3 put: aBoolean not.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/5/2020 12:04:47'!
removeMorph: aMorph
	"Remove the given morph from my submorphs"

	aMorph owner == self ifFalse: [^self].
	aMorph redrawNeeded.
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
	self someSubmorphPositionOrExtentChanged.! !

!Morph methodsFor: 'private' stamp: 'jmv 8/5/2020 14:53:22'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	(aCollection allSatisfy: [ :m | self canAdd: m]) ifFalse: [
		(self confirm: 'Some requested morph requires VectorGraphics support. Do you want to install and activate it?')
			ifTrue: [
				Feature require: 'VectorGraphics'.
				MorphicCanvas activeSubclass: (Smalltalk at: #HybridCanvas) ]
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

!Morph methodsFor: 'private' stamp: 'jmv 8/5/2020 14:53:05'!
privateAddMorph: aMorph atIndex: index

	| oldIndex myWorld itsWorld oldOwner |
	(self canAdd: aMorph) ifFalse: [
		(self confirm: 'The requested morph requires VectorGraphics support. Do you want to install and activate it?')
			ifTrue: [
				Feature require: 'VectorGraphics'.
				MorphicCanvas activeSubclass: (Smalltalk at: #HybridCanvas) ]
			ifFalse: [
				^self error: 'We can''t add requested morph.' ]].
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
			itsWorld ifNotNil: [aMorph invalidateBounds].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [aMorph invalidateBounds].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ].
! !

!Morph methodsFor: 'private' stamp: 'jmv 8/5/2020 14:53:11'!
privateAddMorph: aMorph atIndex: index position: aPoint

	| oldIndex myWorld itsWorld oldOwner |
	(self canAdd: aMorph) ifFalse: [
		(self confirm: 'The requested morph requires VectorGraphics support. Do you want to install and activate it?')
			ifTrue: [
				Feature require: 'VectorGraphics'.
				MorphicCanvas activeSubclass: (Smalltalk at: #HybridCanvas) ]
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

!Morph methodsFor: 'previewing' stamp: 'jmv 8/5/2020 12:21:15'!
endPreview

	self previewing ifTrue: [
		owner notNil ifTrue: [
			self visible: self visibleBeforePreview.
			owner addMorph: self inFrontOf: self morphBehindBeforePreview ].
		self previewing: false. ]! !


!PasteUpMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/5/2020 14:55:30'!
canHandle: aMorph
	self canvas ifNil: [^false].
	^self canvas canDraw: aMorph! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/5/2020 11:44:26'!
damageReportedNotVisibleAndCarriedByHand
	| answer |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m isNil or: [
				m visible not or: [m owner isNil or: [m owner is: #HandMorph]]]) ifTrue: [
					answer add: r]]].
	^answer! !

