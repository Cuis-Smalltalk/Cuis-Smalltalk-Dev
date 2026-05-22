Class {
	#name : 'InlineMethodImplementorsStepWindow',
	#superclass : 'InlineMethodWizardStepWindow',
	#instVars : [
		'selectedImplementor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InlineMethodImplementorsStepWindow class >> openFrom: anInlineMethodApplier [

	^self
		openFrom: anInlineMethodApplier
		methods: (self methodReferencesOf: anInlineMethodApplier implementors)
		label: 'Implementors of #', anInlineMethodApplier selectorToInline, ' to Refactor'
		selecting: nil

]

{ #category : 'GUI building' }
InlineMethodImplementorsStepWindow >> addButtonsTo: row color: buttonColor [

	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

]

{ #category : 'button creation' }
InlineMethodImplementorsStepWindow >> createSelectImplementorButton [

	^PluggableButtonMorph
		model: self
		stateGetter: #isMessageSelected
		action: #selectImplementor
		label: 'Select'
		balloonText: 'Applies the refactoring in the selected place'

]

{ #category : 'actions' }
InlineMethodImplementorsStepWindow >> justRefactor [

	self setImplementorInApplier.
	super justRefactor.
]

{ #category : 'actions' }
InlineMethodImplementorsStepWindow >> refactor [

	self setImplementorInApplier.
	super refactor.
]

{ #category : 'actions' }
InlineMethodImplementorsStepWindow >> selectImplementor [

	selectedImplementor := model selection.
	self refactor.
]

{ #category : 'actions' }
InlineMethodImplementorsStepWindow >> setImplementorInApplier [

	selectedImplementor := selectedImplementor ifNil: [model selection].
	applier implementors: (self compiledMethodsFrom: (Array with: selectedImplementor)).
	
]
