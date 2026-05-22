"
I am a refactoring abstract class whose purpose is to change a given selector; either by renaming it or changing arguments (adding, removing, change order)
"
Class {
	#name : 'ChangeSelector',
	#superclass : 'ScopedRefactoring',
	#instVars : [
		'oldSelector',
		'newSelector',
		'implementors',
		'senders',
		'changes'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ChangeSelector class >> assertAllImplementors: implementors haveSame: aSelector [

	| invalidImplementors |
	
	invalidImplementors := implementors reject: [ :anImplementor | anImplementor selector = aSelector ].
	invalidImplementors notEmpty ifTrue: [ self signalInvalidImplementors: invalidImplementors ].
]

{ #category : 'pre-conditions' }
ChangeSelector class >> assertAllSenders: senders send: aSelector [

	| invalidSenders |
	
	invalidSenders := senders reject: [ :aSender | aSender hasLiteral: aSelector ].
	invalidSenders notEmpty ifTrue: [ self signalInvalidSenders: invalidSenders of: aSelector ].
]

{ #category : 'pre-conditions' }
ChangeSelector class >> assertIsNotEmpty: aCollection signalMessageText: errorMessageText [

	aCollection isEmpty ifTrue: [ self refactoringError: errorMessageText ].
]

{ #category : 'exceptions' }
ChangeSelector class >> errorMessageForInvalidImplementors: aCollectionOfInvalidImplementors [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: (aCollectionOfInvalidImplementors size = 1 ifTrue: [ 'This method' ] ifFalse: [ 'These methods']);
		 	nextPutAll: ' do not have same the same selector to rename: '.
		aCollectionOfInvalidImplementors asCommaSeparated: [:anImplementor | anImplementor printClassAndSelectorOn: stream ] on: stream ] 
]

{ #category : 'exceptions' }
ChangeSelector class >> errorMessageForInvalidSenders: aCollectionOfInvalidSenders of: anOldSelector [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'Message #';
			nextPutAll: anOldSelector;
			nextPutAll: ' is not send by: '.
		aCollectionOfInvalidSenders asCommaSeparated: [:aSender | aSender printClassAndSelectorOn: stream ] on: stream ] 
]

{ #category : 'exceptions' }
ChangeSelector class >> signalInvalidImplementors: invalidImplementors [

	self refactoringError: (self errorMessageForInvalidImplementors: invalidImplementors).
]

{ #category : 'exceptions' }
ChangeSelector class >> signalInvalidSenders: invalidSender of: aSelector [

	self refactoringError: (self errorMessageForInvalidSenders: invalidSender of: aSelector).
]

{ #category : 'rename senders - private' }
ChangeSelector >> add: oldSelectorLiteralRanges to: rangesToKeywords [

	oldSelectorLiteralRanges do: [ :oldSelectorLiteralRange | 
		rangesToKeywords add: (oldSelectorLiteralRange first + 1 to: oldSelectorLiteralRange last) -> newSelector ].
	

	
]

{ #category : 'rename senders - private' }
ChangeSelector >> addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords [

	self subclassResponsibility 
]

{ #category : 'rename senders - private' }
ChangeSelector >> addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords [

	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsInLiteralArrayOf: oldSelector.
	self add: oldSelectorLiteralRanges to: rangesToKeywords 
]

{ #category : 'rename senders - private' }
ChangeSelector >> addRangesForLiteralOf: methodNode to: rangesToKeywords [
	
	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsForLiteralNode: oldSelector ifAbsent: [ ^#() ].
	self add: oldSelectorLiteralRanges to: rangesToKeywords.
	
]

{ #category : 'create new implementors - private' }
ChangeSelector >> addToSendersIfOldSelectorIsSentIn: newImplementor [
	
	(newImplementor sendsSelector: oldSelector) ifTrue: [
		senders add: newImplementor ].
]

{ #category : 'applying' }
ChangeSelector >> apply [
	
	self 
		createNewImplementors;
		renameSenders;
		removeOldImplementors.
		
	^changes

]

{ #category : 'changes' }
ChangeSelector >> changes [
	
	^changes
]

{ #category : 'create new implementors - private' }
ChangeSelector >> compileNewImplementorOf: anImplementor [

	| implementorClassification newSourceCode |

	newSourceCode := self implementorNewSourceCodeOf: anImplementor.
	implementorClassification := anImplementor methodClass organization categoryOfElement: oldSelector.

	anImplementor methodClass
		compile: newSourceCode
		classified: implementorClassification.

]

{ #category : '*ExtractParameterObject-create new implementors - private' }
ChangeSelector >> createNewImplementorOf: anImplementor [

	| newImplementor |

	self compileNewImplementorOf: anImplementor.
	newImplementor := anImplementor methodClass compiledMethodAt: newSelector.
	self addToSendersIfOldSelectorIsSentIn: newImplementor.
	
	^ changes add: newImplementor methodReference.
	
	
]

{ #category : 'create new implementors - private' }
ChangeSelector >> createNewImplementors [

	implementors
		select: [ :anImplementor | anImplementor isValid ]
	 	thenDo: [:anImplementor | self createNewImplementorOf: anImplementor ]
	
]

{ #category : 'create new implementors - private' }
ChangeSelector >> implementorNewSourceCodeOf: anImplementor [

	self subclassResponsibility 
]

{ #category : 'implementors/senders' }
ChangeSelector >> implementorsSize [
	
	^implementors size
]

{ #category : 'initialization' }
ChangeSelector >> initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders [
	
	oldSelector := anOldSelector.
	newSelector := aNewSelector.
	implementors := aCollectionOfImplementors.
	"I have to make a copy of senders because it can change with recursive implementors - Hernan"
	senders := aCollectionOfSenders asOrderedCollection.
	
	changes := Set new
	
]

{ #category : 'selectors' }
ChangeSelector >> newSelector [
	
	^newSelector 
]

{ #category : 'rename senders - private' }
ChangeSelector >> rangesToKeywordsOf: aMethod [

	| methodNode rangesToKeywords |

	methodNode := aMethod notOptimizedMethodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right | left key first < right key first ].

	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.

	^rangesToKeywords	
]

{ #category : 'remove old implementors - private' }
ChangeSelector >> removeOldImplementor: anImplementor  [

	anImplementor methodClass removeSelector: anImplementor selector.
	changes add: anImplementor methodReference
]

{ #category : 'remove old implementors - private' }
ChangeSelector >> removeOldImplementors [
	
	implementors do: [:anImplementor | self removeOldImplementor: anImplementor ]
]

{ #category : 'rename senders - private' }
ChangeSelector >> renameSenders [
	
	senders do: [ :aSender | self renameSendersIn: aSender ].
	
]

{ #category : '*ExtractParameterObject-rename senders - private' }
ChangeSelector >> renameSendersIn: aMethod [

	| newSource rangesToNewStrings |

	rangesToNewStrings := self rangesToKeywordsOf: aMethod.
	newSource := aMethod sourceCode copyReplacing: rangesToNewStrings.
	aMethod methodClass compile: newSource.

	^changes add: (MethodReference method: aMethod)
]

{ #category : 'implementors/senders' }
ChangeSelector >> sendersSize [
	
	^senders size
]
