Class {
	#name : 'ChangeKeywordsSelectorOrder',
	#superclass : 'ChangeSelectorKeepingParameters',
	#instVars : [
		'changedOrder'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ChangeKeywordsSelectorOrder class >> assertIsKeywordWithMoreThanOneParameter: anOldSelector [

	(anOldSelector isKeyword and: [ anOldSelector numArgs > 1 ]) ifFalse: [ self signalSelectorToChangeIsNotKeywordWithMoreThanOneParameter ]
]

{ #category : 'changed order' }
ChangeKeywordsSelectorOrder class >> changedOrderFrom: anOldSelector to: aNewSelector [

	| changedOrder newSelectorKeywords |

	changedOrder := Dictionary new.
	newSelectorKeywords := aNewSelector keywords.
	anOldSelector keywords withIndexDo: [ :keyword :index |
		changedOrder
			at: (newSelectorKeywords indexOf: keyword ifAbsent: [ self signalNewSelectorDoesNotIncludeOldSelectorKeywords ])
			put: index ].

	^changedOrder 
]

{ #category : 'instance creation' }
ChangeKeywordsSelectorOrder class >> from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders [

	| changedOrder |

	self assertIsKeywordWithMoreThanOneParameter: anOldSelector.
	changedOrder := self changedOrderFrom: anOldSelector to: aNewSelector.

	^ (super from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders)
		initializeChangedOrder: changedOrder

]

{ #category : 'exceptions' }
ChangeKeywordsSelectorOrder class >> newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage [

	^'New selector keywords do not include old selector keywords'
]

{ #category : 'exceptions' }
ChangeKeywordsSelectorOrder class >> selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage [

	^'Can only change a selector order for keyword messages with more that one parameter'
]

{ #category : 'exceptions' }
ChangeKeywordsSelectorOrder class >> signalNewSelectorDoesNotIncludeOldSelectorKeywords [

	self refactoringError: self newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage
]

{ #category : 'exceptions' }
ChangeKeywordsSelectorOrder class >> signalSelectorToChangeIsNotKeywordWithMoreThanOneParameter [

	self refactoringError: self selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage
]

{ #category : 'create new implementors - private' }
ChangeKeywordsSelectorOrder >> addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords [

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index).
	rangesToNewKeywords add:
		(currentImplementorMethodNode parameterDefinitionPositionAt: index) ->
		(currentImplementorMethodNode argumentNames at: (changedOrder at: index))
]

{ #category : 'initialization' }
ChangeKeywordsSelectorOrder >> initializeChangedOrder: aChangeOrder [

	changedOrder := aChangeOrder 
]
