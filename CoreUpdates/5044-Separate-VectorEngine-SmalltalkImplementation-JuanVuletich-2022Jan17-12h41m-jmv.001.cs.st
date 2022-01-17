'From Cuis 6.0 [latest update: #5043] on 17 January 2022 at 1:01:55 pm'!

!DisplayScreen methodsFor: 'accessing' stamp: 'jmv 1/17/2022 13:00:17'!
getMainCanvas
	"Return a Canvas that can be used to draw onto the receiver.
	Being the sole 'main' canvas means a priviledge: Can use VectorEnginePlugin if available."

	| answer |
	answer _ [ MorphicCanvas onForm: self ]
				on: OutOfMemory
				do: [
					'Not enough memory to run VectorEngine. Using BitBltCanvas instead.' print.
					MorphicCanvas activeSubclass: BitBltCanvas.
					MorphicCanvas onForm: self ].
	^answer! !


!FontFamily class methodsFor: 'file read write' stamp: 'jmv 1/17/2022 12:01:11'!
readAdditionalTrueTypeFonts
	Feature require: 'VectorGraphics'.
	(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
		Feature require: 'VectorEngineInSmalltalk' ].
	UISupervisor whenUIinSafeState: [
		Smalltalk at: #TrueTypeFontFamily ifPresent: [ :cls | cls readAdditionalFonts ]]! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 1/17/2022 12:01:06'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| selectedNameOrDirectory fontFamily |
	selectedNameOrDirectory _ self promptUserFolders.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^Preferences setDefaultFont: it familyName ].
	Feature require: 'VectorGraphics'.
	(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
		Feature require: 'VectorEngineInSmalltalk' ].
	UISupervisor whenUIinSafeState: [
		fontFamily _ FontFamily availableFamilies values
			detect: [ :any | any folderName = selectedNameOrDirectory name ]
			ifNone: [
				FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ].
		Preferences setDefaultFont: fontFamily familyName ]! !


!Morph methodsFor: 'private' stamp: 'jmv 1/17/2022 12:02:09'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'private' stamp: 'jmv 1/17/2022 12:02:35'!
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
	self privateArrangeWantsToBeOnTop.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !

!methodRemoval: BitBltCanvas #canDraw: stamp: 'jmv 1/17/2022 12:41:55'!
BitBltCanvas removeSelector: #canDraw:!
!methodRemoval: BitBltCanvas #setPluginAndForm: stamp: 'jmv 1/17/2022 13:00:46'!
BitBltCanvas removeSelector: #setPluginAndForm:!
!methodRemoval: MorphicCanvas class #withVectorEnginePluginOnForm: stamp: 'jmv 1/17/2022 13:00:38'!
MorphicCanvas class removeSelector: #withVectorEnginePluginOnForm:!
!methodRemoval: MorphicCanvas #canDraw: stamp: 'jmv 1/17/2022 12:41:55'!
MorphicCanvas removeSelector: #canDraw:!
!methodRemoval: WorldMorph #canHandle: stamp: 'jmv 1/17/2022 12:41:55'!
WorldMorph removeSelector: #canHandle:!
!methodRemoval: Morph #canAdd: stamp: 'jmv 1/17/2022 12:41:55'!
Morph removeSelector: #canAdd:!
