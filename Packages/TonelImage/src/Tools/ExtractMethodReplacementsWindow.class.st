Class {
	#name : 'ExtractMethodReplacementsWindow',
	#superclass : 'MethodSetWindow',
	#instVars : [
		'applier',
		'finder'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethodReplacementsWindow class >> openFrom: anExtractMethodApplier with: aFinder [

	| window messageSet |

	messageSet := ExtractMethodMessageSet finder: aFinder.
	"I have to set a autoSelectString even if I do not use it because if not the 
	autoSelect event is not triggered - Hernan"
	messageSet autoHighlightString: aFinder sourceCodeToExtract allOccurrences: false.
	
	window := self open: messageSet label: 'Select replacements'. 
	window initializeFrom: anExtractMethodApplier with: aFinder.
	
	^window 


]

{ #category : 'GUI building' }
ExtractMethodReplacementsWindow >> addButton: button to: row color: buttonColor [

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
]

{ #category : 'GUI building' }
ExtractMethodReplacementsWindow >> addButtonsTo: row color: buttonColor [

	self 
		addButton: self createRemoveButton to: row color: buttonColor;
		addButton: self createRefactorButton to: row color: buttonColor;
		addButton: self createExtractSelectionOnlyButton to: row color: buttonColor;
		addButton: self createExtractInMethodOnlyButton to: row color: buttonColor;
		addButton: self createExtractAllInClassButton to: row color: buttonColor;
		addButton: self createCancelButton to: row color: buttonColor.

]

{ #category : 'GUI building' }
ExtractMethodReplacementsWindow >> buildLowerPanes [

	| codeAndButtons  |
	
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons 
]

{ #category : 'GUI building' }
ExtractMethodReplacementsWindow >> buttonsRow [

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> closeAfter: aBlock [

	model doNotRefreshSelection.
	aBlock value.
	self whenUIinSafeState: [ self delete ].
	
]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createCancelButton [
	
	^RefactoringApplier createCancelButtonOn: self
]

{ #category : 'as yet unclassified' }
ExtractMethodReplacementsWindow >> createCodePaneMorph [

	^super createCodePaneMorph
		disableEditing;
		yourself
]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createExtractAllInClassButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractAllInClass
		label: 'In Class'
		balloonText: 'Applies the refactoring to all methods defined in the class'

]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createExtractInMethodOnlyButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractInMethodOnly
		label: 'In Method'
		balloonText: 'Applies the refactoring to all repetitions in the source method'

]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createExtractSelectionOnlyButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractSelectionOnly
		label: 'Selection Only'
		balloonText: 'Applies the refactoring to the selected code only'

]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createRefactorButton [

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'
		balloonText: 'Applies the refactoring to all places without showing the changes'
]

{ #category : 'button creation' }
ExtractMethodReplacementsWindow >> createRemoveButton [
	
	^RefactoringApplier createRemoveButtonOn: self
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> extractAllInClass [
	
	self closeAfter: [ applier valueWithMethodsInClass ].
	
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> extractInMethodOnly [

	self closeAfter: [ applier valueWithSourceMethod ].
	
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> extractSelectionOnly [
	
	self closeAfter: [ applier valueWithOriginalSelection ].
	
]

{ #category : 'initialization' }
ExtractMethodReplacementsWindow >> initializeFrom: anExtractMethodApplier with: aFinder [

	applier := anExtractMethodApplier.
	finder := aFinder 
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> refactor [
	
	self closeAfter: [ applier valueWithAllReplacements ].
	
]

{ #category : 'actions' }
ExtractMethodReplacementsWindow >> remove [
	
	model removeMessageFromBrowserKeepingLabel
]
