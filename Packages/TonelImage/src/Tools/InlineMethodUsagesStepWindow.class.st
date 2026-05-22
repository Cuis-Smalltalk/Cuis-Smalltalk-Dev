Class {
	#name : 'InlineMethodUsagesStepWindow',
	#superclass : 'InlineMethodWizardStepWindow',
	#instVars : [
		'changedMethods'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InlineMethodUsagesStepWindow class >> openFrom: anInlineMethodApplier [

	| window messageSendsSet |

	messageSendsSet := InlineMethodMessageSendsSet applier: anInlineMethodApplier.

	messageSendsSet autoHighlightString: anInlineMethodApplier selectorToInline allOccurrences: false.

	window := self open: messageSendsSet label: 'Message sends of #', anInlineMethodApplier selectorToInline, ' to Refactor'.
	window initializeFrom: anInlineMethodApplier.

	^window.
]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> add [

		self
			do: [ :classOfSenderToAdd | self askAndAddAllUsagesInSenderOf: classOfSenderToAdd ]
			withEnteredClassLabeled: 'Sender of #', self selectorToInline
			
]

{ #category : 'GUI building' }
InlineMethodUsagesStepWindow >> addButtonsTo: row color: buttonColor [

	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> addToList: aMessageNodeReference [

	model addMessageNodeReference: aMessageNodeReference ifIncluded: [ self inform: 'Usage already in list' ]
]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> askAndAddAllUsagesInSenderOf: classOfSenderToAdd [

	"Adds all usages of the selected sender to the usages to refactor"
	| senderSelector senderToAdd |

	senderSelector := self request: 'Selector of sender of #', self selectorToInline initialAnswer: '' orCancel: [^self ].
	senderToAdd := classOfSenderToAdd
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].

	(senderToAdd sendsSelector: self selectorToInline) ifFalse: [
		^self inform: senderToAdd classAndSelector, ' does not refer to #', self selectorToInline].

	(applier refactoringClass findReferencesToSelector: self selectorToInline in: senderToAdd asMethodReference)
		do: [:aMessageNodeReference | self addToList: aMessageNodeReference]
]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> changeUsages [

	applier messageSends: model messageList
	
]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> refactor [

	self changeUsages.
	super refactor 
]

{ #category : 'actions' }
InlineMethodUsagesStepWindow >> remove [

	model removeMessageFromBrowserKeepingLabel.
]
