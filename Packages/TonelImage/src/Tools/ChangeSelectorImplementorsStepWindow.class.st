Class {
	#name : 'ChangeSelectorImplementorsStepWindow',
	#superclass : 'ChangeSelectorWizardStepWindow',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ChangeSelectorImplementorsStepWindow class >> openFrom: aChangeSelectorApplier [

	^self 
		openFrom: aChangeSelectorApplier 
		methods: (self methodReferencesOf: aChangeSelectorApplier implementors) 
		label: 'Implementors of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: nil  

]

{ #category : 'actions' }
ChangeSelectorImplementorsStepWindow >> add [
		
	self 
		do: [ :classOfImplementorToAdd | self addImplementorIn: classOfImplementorToAdd ]
		withEnteredClassLabeled:  'Class that implements ', self oldSelector 
]

{ #category : 'GUI building' }
ChangeSelectorImplementorsStepWindow >> addButtonsTo: row color: buttonColor [

	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeSendersButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

]

{ #category : 'actions' }
ChangeSelectorImplementorsStepWindow >> addImplementorIn: classOfImplementorToAdd  [

	| implementorToAdd |
	
	implementorToAdd := classOfImplementorToAdd 
		compiledMethodAt: self oldSelector 
		ifAbsent: [ ^self inform: classOfImplementorToAdd doesNotImplement: self oldSelector ].
		
	self addToList: implementorToAdd
]

{ #category : 'actions' }
ChangeSelectorImplementorsStepWindow >> changeImplementors [
	
	applier implementors: (self compiledMethodsFrom: model messageList).
	
]

{ #category : 'button creation' }
ChangeSelectorImplementorsStepWindow >> createSeeSendersButton [
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeSenders
		label: 'See Senders'
		balloonText: 'Shows the senders that will be affected by the refactoring'


]

{ #category : 'actions' }
ChangeSelectorImplementorsStepWindow >> refactor [

	self changeImplementors.
	super refactor 
]

{ #category : 'actions' }
ChangeSelectorImplementorsStepWindow >> seeSenders [

	self changeImplementors.
	self delete.	
	
	"Necesary indirection to support actual senders in LiveTyping - Hernan"
	applier openChangeSelectorSendersStepWindow
]
