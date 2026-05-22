Class {
	#name : 'ChangeSelectorSendersStepWindow',
	#superclass : 'ChangeSelectorWizardStepWindow',
	#instVars : [
		'changedMethods'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ChangeSelectorSendersStepWindow class >> openFrom: aChangeSelectorApplier [

	^self 
		openFrom: aChangeSelectorApplier 
		methods: aChangeSelectorApplier senders asOrderedCollection 
		label: 'Senders of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: aChangeSelectorApplier oldSelector 
]

{ #category : 'actions' }
ChangeSelectorSendersStepWindow >> add [
	
	self 
		do: [ :classOfSenderToAdd | self askAndAddSenderOf: classOfSenderToAdd ]
		withEnteredClassLabeled: 'Class that sends #', self oldSelector
		
]

{ #category : 'GUI building' }
ChangeSelectorSendersStepWindow >> addButtonsTo: row color: buttonColor [
	
	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeImplementorsButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

]

{ #category : 'actions' }
ChangeSelectorSendersStepWindow >> askAndAddSenderOf: classOfSenderToAdd  [
		
	| senderSelector senderToAdd |
	
	senderSelector := self request: 'Selector of sender of #', self oldSelector initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd 
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].
		
	(senderToAdd sendsSelector: self oldSelector) ifFalse: [
		^self inform: senderToAdd classAndSelector, ' does not refer to #', self oldSelector ].
	
	self addToList: senderToAdd
]

{ #category : 'actions' }
ChangeSelectorSendersStepWindow >> changeSenders [
	
	applier senders: model messageList
	
]

{ #category : 'button creation' }
ChangeSelectorSendersStepWindow >> createSeeImplementorsButton [

	^RefactoringApplier createSeeImplementorsButtonOn: self
]

{ #category : 'actions' }
ChangeSelectorSendersStepWindow >> refactor [

	self changeSenders.
	super refactor 
]

{ #category : 'actions' }
ChangeSelectorSendersStepWindow >> seeImplementors [
	
	self changeSenders.
	self delete.
	
	ChangeSelectorImplementorsStepWindow openFrom: applier
]
