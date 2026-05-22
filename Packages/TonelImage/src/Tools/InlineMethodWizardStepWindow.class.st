Class {
	#name : 'InlineMethodWizardStepWindow',
	#superclass : 'MethodSetWindow',
	#instVars : [
		'applier'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InlineMethodWizardStepWindow class >> methodReferencesOf: methods [

	^methods asOrderedCollection collect: [:aCompiledMethod | aCompiledMethod methodReference ].

]

{ #category : 'instance creation' }
InlineMethodWizardStepWindow class >> openFrom: anInlineMethodApplier methods: methods label: aLabel selecting: somethingToSelect [

	| window |

	window := self openMessageList: methods label: aLabel autoHighlight: somethingToSelect allOccurrences: false.
	window initializeFrom: anInlineMethodApplier.

	^window


]

{ #category : 'GUI building' }
InlineMethodWizardStepWindow >> addButton: button to: row color: buttonColor [

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
]

{ #category : 'GUI building' }
InlineMethodWizardStepWindow >> addButtonsTo: row color: buttonColor [

	self subclassResponsibility 
]

{ #category : 'GUI building' }
InlineMethodWizardStepWindow >> buildLowerPanes [

	| codeAndButtons  |

	codeAndButtons := LinearLayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.

	^codeAndButtons 
]

{ #category : 'GUI building' }
InlineMethodWizardStepWindow >> buttonsRow [

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.

	^row

	
]

{ #category : 'compile methods' }
InlineMethodWizardStepWindow >> compiledMethodsFrom: methodReferences [

	"If the method is not implemented, I leave the not implemented reference because actual senders of it
	should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan"
	^ methodReferences collect: [:aMethodReference |
		aMethodReference compiledMethodIfAbsent: [ aMethodReference ]]
]

{ #category : 'button creation' }
InlineMethodWizardStepWindow >> createCancelButton [

	^RefactoringApplier createCancelButtonOn: self

]

{ #category : 'button creation' }
InlineMethodWizardStepWindow >> createJustRefactorButton [

	^RefactoringApplier createJustRefactorButtonOn: self 
]

{ #category : 'button creation' }
InlineMethodWizardStepWindow >> createRefactorButton [

	^RefactoringApplier createRefactorButtonOn: self
]

{ #category : 'button creation' }
InlineMethodWizardStepWindow >> createRemoveButton [

	^RefactoringApplier createRemoveButtonOn: self
]

{ #category : 'actions' }
InlineMethodWizardStepWindow >> do: aBlock withEnteredClassLabeled: aLabel [
	ClassNameRequestMorph
		request: aLabel
		initialAnswer: ''
		do: [:className| self withClassNamed: className do: aBlock]
]

{ #category : 'actions' }
InlineMethodWizardStepWindow >> inform: aClass doesNotImplement: aSelector [

 	self inform: aClass name, ' does not implement #', aSelector 
]

{ #category : 'initialization' }
InlineMethodWizardStepWindow >> initializeFrom: anInlineMethodApplier [

	applier := anInlineMethodApplier 
]

{ #category : 'testing' }
InlineMethodWizardStepWindow >> isMessageSelected [

	^model isNil ifTrue: [ false ] ifFalse: [ model selection notNil ]
]

{ #category : 'actions' }
InlineMethodWizardStepWindow >> justRefactor [

	applier doNotShowChanges.
	self refactor.
]

{ #category : 'actions' }
InlineMethodWizardStepWindow >> refactor [

	applier wizardStepWindow: self.
	applier wizardEnded.
	
]

{ #category : 'accessing' }
InlineMethodWizardStepWindow >> selectorToInline [

	^applier selectorToInline 
]

{ #category : 'actions' }
InlineMethodWizardStepWindow >> withClassNamed: aName do: aBlock [

	| trimmedNamed |

	trimmedNamed := aName withBlanksTrimmed.

	(Smalltalk classNamed: trimmedNamed asSymbol)
		ifNotNil: aBlock
		ifNil: [ self inform: 'Class ', trimmedNamed , ' does not exist' ].

]
