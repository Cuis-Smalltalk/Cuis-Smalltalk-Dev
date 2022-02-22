'From Cuis 6.0 [latest update: #5069] on 21 February 2022 at 4:42:24 pm'!

!Morph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:03:26'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	Please make them natural numbers.
	See implementors."

	^self valueOfProperty: #layerNumber ifAbsent: [100]! !

!Morph methodsFor: 'testing' stamp: 'jmv 2/21/2022 16:41:36'!
honorsLayerOrder
	"Most morphs don't go through the expense of the needed resorting."

	^false! !

!Morph methodsFor: 'private' stamp: 'jmv 2/21/2022 16:33:14'!
privateFixLayerOrder
	"Ensure that morphs with smaller #layerNumber are above morphs with larger #layerNumber."

	| layerNumbers sortedSubmorphs newIndex |
	self honorsLayerOrder ifFalse: [ ^self ].
	submorphs size < 2 ifTrue: [ ^self ].
	layerNumbers _ Set new.
	submorphs do: [ :m | layerNumbers add: m layerNumber ].
	layerNumbers _ layerNumbers asArray sort.
	sortedSubmorphs _ Array new: submorphs size.
	newIndex _ 1.
	layerNumbers do: [ :layerNumber |
		submorphs withIndexDo: [ :m :i |
			m layerNumber = layerNumber ifTrue: [
				sortedSubmorphs at: newIndex put: m.
				newIndex = i ifFalse: [ m invalidateBounds ].
				newIndex _ newIndex + 1 ]]].
	submorphs _ sortedSubmorphs.! !


!PasteUpMorph methodsFor: 'testing' stamp: 'jmv 2/21/2022 16:16:13'!
honorsLayerOrder

	^true! !


!StringRequestMorph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:39:20'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be above regular morphs."

	^30! !


!FillInTheBlankMorph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:39:13'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be above regular morphs."

	^30! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:40:23'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be on top."

	^10! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:38:31'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20! !


!AutoCompleterMorph methodsFor: 'accessing' stamp: 'jmv 2/21/2022 16:38:39'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20! !


!Morph methodsFor: 'private' stamp: 'jmv 2/21/2022 16:00:38'!
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
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
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 2/21/2022 16:00:42'!
privateAddMorph: aMorph atIndex: index position: aPointOrNil

	| oldIndex myWorld itsWorld oldOwner |
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
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

!Morph methodsFor: 'private' stamp: 'jmv 2/21/2022 16:00:45'!
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
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.! !

!Morph methodsFor: 'private' stamp: 'jmv 2/21/2022 16:00:48'!
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
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.! !

!methodRemoval: AutoCompleterMorph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:51'!
AutoCompleterMorph removeSelector: #wantsToBeOnTop!
!methodRemoval: MenuMorph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:51'!
MenuMorph removeSelector: #wantsToBeOnTop!
!methodRemoval: HoverHelpMorph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:51'!
HoverHelpMorph removeSelector: #wantsToBeOnTop!
!methodRemoval: FillInTheBlankMorph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:50'!
FillInTheBlankMorph removeSelector: #wantsToBeOnTop!
!methodRemoval: StringRequestMorph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:50'!
StringRequestMorph removeSelector: #wantsToBeOnTop!
!methodRemoval: Morph #wantsToBeOnTop: stamp: 'jmv 2/21/2022 16:36:51'!
Morph removeSelector: #wantsToBeOnTop:!
!methodRemoval: Morph #wantsToBeOnTop stamp: 'jmv 2/21/2022 16:40:49'!
Morph removeSelector: #wantsToBeOnTop!
!methodRemoval: Morph #privateArrangeWantsToBeOnTop stamp: 'jmv 2/21/2022 16:36:38'!
Morph removeSelector: #privateArrangeWantsToBeOnTop!

!PasteUpMorph reorganize!
('accessing' color:)
('caching' releaseCachedState)
('drawing' drawOn:)
('dropping/grabbing' allowsMorphDrop allowsSubmorphDrag)
('events' mouseButton1Down:localPosition:)
('event handling testing' handlesMouseDown:)
('event handling' wantsWindowEvent:)
('geometry' extentChanged:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('stepping and presenter' wantsSteps)
('testing' honorsLayerOrder isReallyVisible)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
!

