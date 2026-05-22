Class {
	#name : 'InlineMethodImplementorsWithShowUsagesStepWindow',
	#superclass : 'InlineMethodImplementorsStepWindow',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'button creation' }
InlineMethodImplementorsWithShowUsagesStepWindow >> addButtonsTo: row color: buttonColor [

	self addButton: self createSelectAndSeeUsagesButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.
]

{ #category : 'button creation' }
InlineMethodImplementorsWithShowUsagesStepWindow >> createSelectAndSeeUsagesButton [

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #selectAndSeeUsages
		label: 'Select and see usages'
		balloonText: 'Selects the replacement to refactor and shows the usages'



]

{ #category : 'actions' }
InlineMethodImplementorsWithShowUsagesStepWindow >> selectAndSeeUsages [

	self setImplementorInApplier.
	self delete.

	"Necesary indirection to support actual senders in LiveTyping - Hernan"
	applier openMessageSendsSelectionWindow.
]

{ #category : 'actions' }
InlineMethodImplementorsWithShowUsagesStepWindow >> selectImplementor [

	selectedImplementor := model selection.
	self selectAndSeeUsages.
]
