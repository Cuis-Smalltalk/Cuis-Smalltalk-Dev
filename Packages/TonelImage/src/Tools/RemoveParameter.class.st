Class {
	#name : 'RemoveParameter',
	#superclass : 'ParametersSizeChange',
	#instVars : [
		'parameterToRemove',
		'parameterIndex',
		'senderReplacementString',
		'isLastParameter'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RemoveParameter class >> assert: aParamerterToRemove isDefinedIn: methodNode [

	| parameterIndex |
	
	parameterIndex := methodNode arguments findFirst: [ :aParameterNode | aParameterNode name = aParamerterToRemove ].
	parameterIndex = 0 ifTrue: [ self signalParameterNotInMessage: aParamerterToRemove ].
	
	^parameterIndex 
		
]

{ #category : 'pre-conditions' }
RemoveParameter class >> assert: parameterIndex isValidFor: oldSelector [

	parameterIndex isInteger ifFalse: [ self signalInvalidParameterIndex ].
	(parameterIndex between: 1 and: oldSelector numArgs) ifFalse: [ self signalInvalidParameterIndex ].
]

{ #category : 'pre-conditions' }
RemoveParameter class >> assertCanRemoveParameterFrom: oldSelector [

	self assertIsNotUnary: oldSelector.
	self assertIsNotBinary: oldSelector.
]

{ #category : 'pre-conditions' }
RemoveParameter class >> assertIsNotBinary: aSelector [

	aSelector isInfix ifTrue: [ self signalCanNotRemoveParameterFromBinaryMessages]
]

{ #category : 'pre-conditions' }
RemoveParameter class >> assertIsNotUnary: aSelector [

	aSelector isUnary ifTrue: [ self signalCanNotRemoveParameterFromUnaryMessages]
]

{ #category : 'pre-conditions' }
RemoveParameter class >> assertNoImplementorFrom: implementorsCollection reference: aParamerterToRemove definedAt: parameterIndex [

	| implementorsReferencingParameter |
	
	implementorsReferencingParameter := implementorsCollection select: [:anImplementor | anImplementor referencesParameterAt: parameterIndex ].
	
	implementorsReferencingParameter isEmpty ifFalse: [ self signalCanNotRemove: aParamerterToRemove dueToReferencesIn: implementorsReferencingParameter ].
]

{ #category : 'instance creation' }
RemoveParameter class >> atIndex: parameterIndex named: aParameterToRemove from: oldSelector implementors: implementorsCollection senders: sendersCollection [

	| newSelector |

	self assertCanRemoveParameterFrom: oldSelector.
	self assert: parameterIndex isValidFor: oldSelector.

	self assertAllImplementors: implementorsCollection haveSame: oldSelector.
	self assertAllSenders: sendersCollection send: oldSelector.
	self assertNoImplementorFrom: implementorsCollection reference: aParameterToRemove definedAt: parameterIndex.

	newSelector := self newSelectorFrom: oldSelector removingParameterAt: parameterIndex.

	^self new
		initializeNamed: aParameterToRemove
		ofKeywordAtIndex: parameterIndex
		from: oldSelector
		creating: newSelector
		implementors: implementorsCollection
		senders: sendersCollection

]

{ #category : 'exceptions' }
RemoveParameter class >> canNotRemoveParameterFromBinaryMessagesErrorMessage [
	
	^'Can not remove parameter from binary messages.
Rename message to a keyword one first'
]

{ #category : 'exceptions' }
RemoveParameter class >> canNotRemoveParameterFromUnaryMessagesErrorMessage [
	
	^'There is no parameter to remove in unary messages'
]

{ #category : 'exceptions' }
RemoveParameter class >> errorMessageForParameterNotInMessage: aParameterToRemove [

	^aParameterToRemove, ' is not define as parameter'
]

{ #category : 'exceptions' }
RemoveParameter class >> errorMessageForParameterToRemoveIsReferenced: aParameterToRemove [

	^aParameterToRemove, ' is being referenced in implementors'
]

{ #category : 'exceptions' }
RemoveParameter class >> invalidParameterIndexErrorMessage [

	^'Invalid parameter index'
]

{ #category : 'instance creation' }
RemoveParameter class >> named: aParameterToRemove from: aMethod implementors: implementorsCollection senders: sendersCollection [

	| parameterIndex |

	parameterIndex := self assert: aParameterToRemove isDefinedIn: aMethod notOptimizedMethodNode.

	^self
		atIndex: parameterIndex
		named: aParameterToRemove
		from: aMethod selector
		implementors: implementorsCollection
		senders: sendersCollection 
]

{ #category : 'instance creation - private' }
RemoveParameter class >> newSelectorConcatenating: oldSelectorKeywords removingAt: anIndex [
	
	| keywords |
	
	keywords := oldSelectorKeywords asOrderedCollection.
	keywords removeIndex: anIndex.

	^(String streamContents: [ :strm | keywords do: [ :keyword | strm nextPutAll: keyword ]]) asSymbol.
]

{ #category : 'instance creation - private' }
RemoveParameter class >> newSelectorFrom: oldSelector removingParameterAt: parameterIndex [

	| oldSelectorKeywords newSelector |
	
	oldSelectorKeywords := oldSelector keywords.
	
	newSelector := oldSelectorKeywords size = 1 
		ifTrue: [ oldSelector allButLast asSymbol ]
		ifFalse: [ self newSelectorConcatenating: oldSelectorKeywords removingAt: parameterIndex ].
	
	^newSelector
]

{ #category : 'exceptions' }
RemoveParameter class >> signalCanNotRemove: aParameterToRemove dueToReferencesIn: implementorsReferencingParameterToRemove [

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForParameterToRemoveIsReferenced: aParameterToRemove)
		references: (implementorsReferencingParameterToRemove collect: [ :implementor | MethodReference method: implementor ])
		to: aParameterToRemove 
	 
]

{ #category : 'exceptions' }
RemoveParameter class >> signalCanNotRemoveParameterFromBinaryMessages [
	
	self refactoringError: self canNotRemoveParameterFromBinaryMessagesErrorMessage
]

{ #category : 'exceptions' }
RemoveParameter class >> signalCanNotRemoveParameterFromUnaryMessages [
	
	self refactoringError: self canNotRemoveParameterFromUnaryMessagesErrorMessage
]

{ #category : 'exceptions' }
RemoveParameter class >> signalInvalidParameterIndex [

	self refactoringError: self invalidParameterIndexErrorMessage
]

{ #category : 'exceptions' }
RemoveParameter class >> signalParameterNotInMessage: aParameterToRemove [

	self refactoringError: (self errorMessageForParameterNotInMessage: aParameterToRemove)
]

{ #category : 'rename senders - private' }
RemoveParameter >> addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords [
	
	| keywordAndParameterPositions senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := self keywordAndParameterPositionsIn: aMethodNode.
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition | | lastPosition |
		lastPosition := self lastSeparatorIndexIn: senderSourceCode startingFrom: aKeywordAndParameterPosition last.
		rangesToKeywords add: (aKeywordAndParameterPosition first to: lastPosition) -> senderReplacementString ]
	
]

{ #category : 'create new implementors - private' }
RemoveParameter >> implementorNewSourceCodeOf: anImplementor [

	| implementorMethodNode newSource originalSource parameterToRemovePosition selectorToRemovePosition |

	implementorMethodNode := anImplementor notOptimizedMethodNode.
	selectorToRemovePosition := implementorMethodNode selectorKeywordPositionAt: parameterIndex.
	parameterToRemovePosition := implementorMethodNode parameterDefinitionPositionAt: parameterIndex.

	originalSource := anImplementor sourceCode.
	newSource := String streamContents: [ :newSourceStream |
		self writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition.
		self writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition ].

	^newSource
]

{ #category : 'initialization' }
RemoveParameter >> initializeNamed: aParameterToRemove [
	ofKeywordAtIndex: aParameterIndex
	from: anOldSelector 
	creating: aNewSelector 
	implementors: implementorsCollection 
	senders: sendersCollection
	
	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.

	parameterToRemove := aParameterToRemove.
	parameterIndex := aParameterIndex.
	senderReplacementString := newSelector isUnary ifTrue: [ newSelector asPlainString ] ifFalse: [ '' ].
	isLastParameter := oldSelector numArgs = parameterIndex 
	
]

{ #category : 'rename senders - private' }
RemoveParameter >> keywordAndParameterPositionsIn: aMethodNode [
	
	^aMethodNode messageSendKeywordAndParameterPositionsAt: parameterIndex of: oldSelector ifAbsent: [ #() ].
]

{ #category : 'rename senders - private' }
RemoveParameter >> lastSeparatorIndexIn: senderSourceCode startingFrom: aPosition [

	| lastPosition senderSourceCodeSize |
	
	lastPosition := aPosition.
	senderSourceCodeSize := senderSourceCode size.
	[ lastPosition := lastPosition + 1.
	lastPosition <= senderSourceCodeSize and: [ (senderSourceCode at: lastPosition) isSeparator ]] whileTrue. 

	^lastPosition - 1
]

{ #category : 'create new implementors - private' }
RemoveParameter >> writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition [
	
	| afterParameterPosition |
	
	afterParameterPosition := parameterToRemovePosition last.
	isLastParameter ifFalse: [ afterParameterPosition := self lastSeparatorIndexIn: originalSource startingFrom: afterParameterPosition ].
	
	newSourceStream nextPutAll: (originalSource copyFrom: afterParameterPosition + 1 to: originalSource size) 	
]

{ #category : 'create new implementors - private' }
RemoveParameter >> writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition [

	newSelector isUnary 
		ifTrue: [ newSourceStream nextPutAll: newSelector ]
		ifFalse: [ newSourceStream nextPutAll: (originalSource copyFrom: 1 to: selectorToRemovePosition first - 1) ].

]
