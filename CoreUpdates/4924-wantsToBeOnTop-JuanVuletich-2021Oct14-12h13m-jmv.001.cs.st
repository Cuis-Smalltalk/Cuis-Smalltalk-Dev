'From Cuis 5.0 [latest update: #4923] on 14 October 2021 at 12:15:40 pm'!

!Morph methodsFor: 'structure' stamp: 'jmv 10/14/2021 10:27:31'!
wantsToBeOnTop: aBoolean
	"If true, will be above all siblings who don't."

	self privateFlagAt: 6 put: aBoolean.! !

!Morph methodsFor: 'testing' stamp: 'jmv 10/14/2021 10:27:38'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ self privateFlagAt: 6.! !

!Morph methodsFor: 'private' stamp: 'jmv 10/14/2021 10:36:00'!
privateArrangeWantsToBeOnTop
	"Ensure that all morphs who #wantsToBeOnTop (i.e. toppers) are above all morphs who not #wantsToBeOnTop.
	Do it without reordering these two subsets.
	Do it in a single pass, and exit as soon as possible."

	| firstMisplacedTopperIndex firstNonTopperIndex topper nonTopper |
	submorphs size < 2 ifTrue: [ ^self ].
	firstMisplacedTopperIndex _ 0.
	firstNonTopperIndex _ 1.
	[
		"Look for next."
		[ (submorphs at: firstNonTopperIndex) wantsToBeOnTop ] whileTrue: [
			firstNonTopperIndex _ firstNonTopperIndex + 1.
			firstNonTopperIndex = submorphs size ifTrue: [
				"All toppers until the end (at most, one non topper as last). Nothing else to do."
				^self ]].
		firstMisplacedTopperIndex _ firstMisplacedTopperIndex max: firstNonTopperIndex+1.
		[ (submorphs at: firstMisplacedTopperIndex) wantsToBeOnTop not ] whileTrue: [
			firstMisplacedTopperIndex _ firstMisplacedTopperIndex + 1.
			firstMisplacedTopperIndex > submorphs size ifTrue: [
				"No more toppers until the end. Nothing else to do."
				^self ]].

		"We have actually found a misplaced topper. Fix it!!"
		nonTopper _ submorphs at: firstNonTopperIndex.
		topper _ submorphs at: firstMisplacedTopperIndex.
		submorphs at: firstNonTopperIndex put: topper invalidateBounds.
		submorphs at: firstMisplacedTopperIndex put: nonTopper invalidateBounds.
	] repeat.! !


!StringRequestMorph methodsFor: 'testing' stamp: 'jmv 10/14/2021 10:31:13'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ true! !


!FillInTheBlankMorph methodsFor: 'testing' stamp: 'jmv 10/14/2021 10:28:52'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ true! !


!HoverHelpMorph methodsFor: 'testing' stamp: 'jmv 10/14/2021 10:27:49'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ true! !


!MenuMorph methodsFor: 'testing' stamp: 'jmv 10/14/2021 12:15:06'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ true! !


!AutoCompleterMorph methodsFor: 'testing' stamp: 'jmv 10/14/2021 10:29:00'!
wantsToBeOnTop
	"If true, will be above all siblings who don't."

	^ true! !


!Morph methodsFor: 'private' stamp: 'jmv 10/14/2021 10:25:16'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 10/14/2021 10:25:22'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

!Morph methodsFor: 'private' stamp: 'jmv 10/14/2021 10:25:26'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged.! !

!Morph methodsFor: 'private' stamp: 'jmv 10/14/2021 10:25:30'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged.! !

