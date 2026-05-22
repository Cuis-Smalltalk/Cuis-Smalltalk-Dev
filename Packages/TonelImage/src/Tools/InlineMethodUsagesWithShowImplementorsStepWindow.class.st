Class {
	#name : 'InlineMethodUsagesWithShowImplementorsStepWindow',
	#superclass : 'InlineMethodUsagesStepWindow',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'button creation' }
InlineMethodUsagesWithShowImplementorsStepWindow >> addButtonsTo: row color: buttonColor [

	super addButtonsTo: row color: buttonColor.
	self addButton: self createSeeImplementorsButton to: row color: buttonColor.
]

{ #category : 'button creation' }
InlineMethodUsagesWithShowImplementorsStepWindow >> createSeeImplementorsButton [

	^RefactoringApplier createSeeImplementorsButtonOn: self
]

{ #category : 'actions' }
InlineMethodUsagesWithShowImplementorsStepWindow >> seeImplementors [

	self changeUsages.
	self delete.

	applier openImplementorSelectionWindow.
]
