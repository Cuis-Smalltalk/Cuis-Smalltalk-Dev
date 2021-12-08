'From Cuis 5.0 [latest update: #4945] on 25 October 2021 at 6:51:23 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 9/13/2021 17:26:23'!
imageForm: extentOrNil depth: depth

	self subclassResponsibility! !

!Morph methodsFor: 'updating' stamp: 'jmv 10/25/2021 18:50:03'!
invalidateBounds
	"Report that the area occupied by this morph and submorphs should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self displayFullBounds ifNotNil: [ :r |
		self invalidateDisplayRect: r for: nil ].
! !

!Morph methodsFor: 'updating' stamp: 'jmv 10/25/2021 18:50:09'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way.
	Notes:
		Area used before any change is reported here.
		Area used after any changes is reported when displayBounds are updated.
			See updateBoundsIn:addDamageTo: and senders.
	"

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self basicDisplayBounds ifNotNil: [ :b |
		self invalidateDisplayRect: b for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !

!Morph methodsFor: 'private' stamp: 'jmv 10/25/2021 18:49:29'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 10/25/2021 18:49:39'!
privateAddMorph: aMorph atIndex: index position: aPointOrNil

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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

