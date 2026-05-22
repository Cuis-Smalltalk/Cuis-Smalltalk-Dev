Class {
	#name : 'ChangeSelectorWizardStepWindow',
	#superclass : 'MethodSetWindow',
	#instVars : [
		'applier'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ChangeSelectorWizardStepWindow class >> methodReferencesOf: methods [

	^methods asOrderedCollection collect: [:aCompiledMethod | aCompiledMethod methodReference ].

]

{ #category : 'instance creation' }
ChangeSelectorWizardStepWindow class >> openFrom: aChangeSelectorApplier methods: methods label: aLabel selecting: somethingToSelect [

	| window |
	
	window := self openMessageList: methods label: aLabel autoHighlight: somethingToSelect allOccurrences: false.
	window initializeFrom: aChangeSelectorApplier.
	
	^window 


]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> add [

	self subclassResponsibility 
]

{ #category : 'GUI building' }
ChangeSelectorWizardStepWindow >> addButton: button to: row color: buttonColor [

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
]

{ #category : 'GUI building' }
ChangeSelectorWizardStepWindow >> addButtonsTo: row color: buttonColor [
	
	self subclassResponsibility 
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> addToList: aMethod [

	model addMethodReference: aMethod methodReference ifIncluded: [ self inform: 'Method already in list' ]
]

{ #category : 'GUI building' }
ChangeSelectorWizardStepWindow >> buildLowerPanes [

	| codeAndButtons  |
	
	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons 
]

{ #category : 'GUI building' }
ChangeSelectorWizardStepWindow >> buttonsRow [

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	
]

{ #category : 'compile methods' }
ChangeSelectorWizardStepWindow >> compiledMethodsFrom: methodReferences [

	"If the method is not implemented, I leave the not implemented reference because actual senders of it
	should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan"
	^ methodReferences collect: [:aMethodReference |
		aMethodReference compiledMethodIfAbsent: [ aMethodReference ]]
]

{ #category : 'button creation' }
ChangeSelectorWizardStepWindow >> createAddButton [
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #add
		label: 'Add'
		balloonText: 'Add a new method to refactor'
]

{ #category : 'button creation' }
ChangeSelectorWizardStepWindow >> createCancelButton [
	
	^RefactoringApplier createCancelButtonOn: self
]

{ #category : 'button creation' }
ChangeSelectorWizardStepWindow >> createJustRefactorButton [

	^RefactoringApplier createJustRefactorButtonOn: self 
]

{ #category : 'button creation' }
ChangeSelectorWizardStepWindow >> createRefactorButton [

	^RefactoringApplier createRefactorButtonOn: self
]

{ #category : 'button creation' }
ChangeSelectorWizardStepWindow >> createRemoveButton [
	
	^RefactoringApplier createRemoveButtonOn: self
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> do: aBlock withEnteredClassLabeled: aLabel [
	ClassNameRequestMorph
		request: aLabel
		initialAnswer: ''
		do: [:className| self withClassNamed: className do: aBlock]
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> inform: aClass doesNotImplement: aSelector [

 	self inform: aClass name, ' does not implement #', aSelector 
]

{ #category : 'initialization' }
ChangeSelectorWizardStepWindow >> initializeFrom: aChangeSelectorApplier [

	applier := aChangeSelectorApplier 
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> justRefactor [
	
	applier doNotShowChanges.
	self refactor.
]

{ #category : 'accessing' }
ChangeSelectorWizardStepWindow >> oldSelector [
	
	^applier oldSelector 
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> refactor [
	
	applier wizardStepWindow: self. 	
	applier wizardEnded.
	
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> remove [
	
	model removeMessageFromBrowserKeepingLabel
]

{ #category : 'actions' }
ChangeSelectorWizardStepWindow >> withClassNamed: aName do: aBlock [

	| trimmedNamed |
	
	trimmedNamed := aName withBlanksTrimmed.
	
	(Smalltalk classNamed: trimmedNamed asSymbol)
		ifNotNil: aBlock
		ifNil: [ self inform: 'Class ', trimmedNamed , ' does not exist' ].

]
