Class {
	#name : 'AddMorphTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Morphic-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Morphic-Kernel'
}

{ #category : 'tests - addMorph:' }
AddMorphTest >> test001_CanAddAMorph [

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorph: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }
]

{ #category : 'tests - addMorph:' }
AddMorphTest >> test002_AddingANewSubmorphInFrontOfTheRest [

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.

	owner addMorph: newSubmorph.

	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }. 
]

{ #category : 'tests - addMorph:' }
AddMorphTest >> test003_MorphCannotBeItsOwnSubmorph [

	| owner |
	owner := Morph new.
	
	self
		should: [ owner addMorph: owner ] 
		raise: Error
		withMessageText: 'can not be own submorph'.
	
	self assert: owner submorphs isEmpty
]

{ #category : 'tests - addMorph:' }
AddMorphTest >> test004_AddingAnExistingSubmorph_RemainsASubmorph [

	| owner existingSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	owner addMorph: existingSubmorph.

	owner addMorph: existingSubmorph.
	
	self assert: owner submorphs equals: { existingSubmorph }.
]

{ #category : 'tests - addMorph:' }
AddMorphTest >> test005_AddingAnExistingSubmorphMoreThanOnce_PlacesItInFront [

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2.
	
	owner addMorph: submorph1.

	self assert: owner submorphs equals: { submorph1 . submorph2 }. 
]

{ #category : 'tests - addMorphBack:' }
AddMorphTest >> test006_CanAddAMorphBack_WhenNotHavingSubmorphs [

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorphBack: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }
]

{ #category : 'tests - addMorphBack:' }
AddMorphTest >> test007_AddingANewSubmorphBack_PlaceItAtTheEndOfSubmorphs [

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorphBack: existingSubmorph.
	
	owner addMorphBack: newSubmorph.

	self assert: owner submorphs equals: { existingSubmorph . newSubmorph }
]

{ #category : 'tests - addMorphBack:' }
AddMorphTest >> test008_AddAnExistingSubmorphToTheBack_PlaceTheSubmorphAtEndOfSubmorphs [

	| owner submorphToMove submorph |
	owner := Morph new.
	submorphToMove := Morph new.
	submorph := Morph new.
	owner addMorphBack: submorphToMove.
	owner addMorphBack: submorph.
	
	owner addMorphBack: submorphToMove.

	self assert: owner submorphs equals: { submorph . submorphToMove }
]

{ #category : 'tests - addMorphFront:' }
AddMorphTest >> test009_CanAddAMorphFront_WhenNotHavingSubmorphs [

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.
	
	owner addMorphFront: newSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph }
]

{ #category : 'tests - addMorphFront:' }
AddMorphTest >> test010_AddingANewSubmorphFront_PlaceItAtTheStartOfSubmorphs [

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorphFront: existingSubmorph.
	
	owner addMorphFront: newSubmorph.

	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }
]

{ #category : 'tests - addMorphFront:' }
AddMorphTest >> test011_AddSubmorphFront_PlaceSubmorphsAtBeginningOfSubmorphs [

	| owner submorphToMove anotherSubmorph |
	owner := Morph new.
	submorphToMove := Morph new.
	anotherSubmorph := Morph new.
	owner addMorphFront: submorphToMove.
	owner addMorphFront: anotherSubmorph.
	
	owner addMorphFront: submorphToMove.

	self assert: owner submorphs equals: { submorphToMove . anotherSubmorph }. 
]

{ #category : 'tests - addMorph:behind:' }
AddMorphTest >> test012_CanAddANewSubmorphBehindAnExistingSubmorph [

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.
	
	owner addMorph: newSubmorph behind: existingSubmorph.
	
	self assert: owner submorphs equals: { existingSubmorph . newSubmorph }
]

{ #category : 'tests - addMorph:behind:' }
AddMorphTest >> test013_WhenASubmorphIsBehindAnother_AddingItAgainKeepThemInSamePlaces [

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2 behind: submorph1.
	
	owner addMorph: submorph2 behind: submorph1.

	self assert: owner submorphs equals: { submorph1 . submorph2 }.
]

{ #category : 'tests - addMorph:behind:' }
AddMorphTest >> test014_CanAddANewSubmorphBetweenExistingsSubmorphs [

	| owner submorph1 submorph2 newSubmorph |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	newSubmorph := Morph new.
	owner addMorph: submorph2.
	owner addMorph: submorph1.

	owner addMorph: newSubmorph behind: submorph1.

	self assert: owner submorphs equals: { submorph1 . newSubmorph . submorph2 }.
]

{ #category : 'tests - addMorph:behind:' }
AddMorphTest >> test015_CannotAddMorphBehindNonSubmorph [

	| owner anotherMorph |
	owner := Morph new.
	anotherMorph := Morph new.

	self
		should: [ owner addMorph: anotherMorph behind: Morph new ]
		raise: Error.
		
	self assert: owner submorphs isEmpty
]

{ #category : 'tests - addMorph:behind:' }
AddMorphTest >> test016_CanMoveASubmorphBehindAnother [

	| owner submorph submorphToMove |
	owner := Morph new.
	submorph := Morph new.
	submorphToMove := Morph new.
	owner addMorph: submorph.
	owner addMorph: submorphToMove.

	owner addMorph: submorphToMove behind: submorph.

	self assert: owner submorphs equals: { submorph . submorphToMove }.
]

{ #category : 'tests - addMorph:inFrontOf:' }
AddMorphTest >> test017_CanAddANewSubmorphInFrontOfAnExistingSubmorph [

	| owner existingSubmorph newSubmorph |
	owner := Morph new.
	existingSubmorph := Morph new.
	newSubmorph := Morph new.
	owner addMorph: existingSubmorph.
	
	owner addMorph: newSubmorph inFrontOf: existingSubmorph.
	
	self assert: owner submorphs equals: { newSubmorph . existingSubmorph }.
]

{ #category : 'tests - addMorph:inFrontOf:' }
AddMorphTest >> test018_WhenASubmorphIsAfterAnother_AddingItAgainKeepThemInSamePlaces [

	| owner submorph1 submorph2 |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2 inFrontOf: submorph1.
	
	owner addMorph: submorph2 inFrontOf: submorph1.
	
	self assert: owner submorphs equals: { submorph2 . submorph1 }
]

{ #category : 'tests - addMorph:inFrontOf:' }
AddMorphTest >> test019_CanAddANewSubmorphBetweenExistingsSubmorphs [

	| owner submorph1 submorph2 newSubmorph |
	owner := Morph new.
	submorph1 := Morph new.
	submorph2 := Morph new.
	newSubmorph := Morph new.
	owner addMorph: submorph1.
	owner addMorph: submorph2.

	owner addMorph: newSubmorph inFrontOf: submorph1.

	self assert: owner submorphs equals: { submorph2 . newSubmorph . submorph1 }
]

{ #category : 'tests - addMorph:inFrontOf:' }
AddMorphTest >> test020_CannotAddMorphInFrontOfNonSubmorph [

	| owner newSubmorph |
	owner := Morph new.
	newSubmorph := Morph new.

	self
		should: [ owner addMorph: newSubmorph inFrontOf: Morph new ]
		raise: Error.
	
	self assert: owner submorphs isEmpty.
]

{ #category : 'tests - addMorph:inFrontOf:' }
AddMorphTest >> test021_CanMoveASubmorphInFrontOfAnother [

	| owner submorphToMove submorph |
	owner := Morph new.
	submorphToMove := Morph new.
	submorph := Morph new.
	owner addMorph: submorphToMove.
	owner addMorph: submorph.

	owner addMorph: submorphToMove inFrontOf: submorph.

	self assert: owner submorphs equals: { submorphToMove  . submorph }
]
