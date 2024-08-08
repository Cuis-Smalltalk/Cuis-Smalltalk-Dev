'From Cuis7.1 [latest update: #6573] on 8 August 2024 at 3:16:03 pm'!

!LayoutMorph class methodsFor: 'instance creation' stamp: 'KenD 8/8/2024 06:52:13'!
initializedInstance

	^self newRow! !


!LayoutMorph class methodsFor: 'instance creation' stamp: 'KenD 8/8/2024 06:53:03'!
new
	"Answer a row by default so the 'new morph' menu doesn't fail..."
	^self error: 'Use LayoutMorph newRow or LayoutMorph newColumn'.! !

