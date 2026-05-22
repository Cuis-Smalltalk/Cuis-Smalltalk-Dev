"
I am a refactoring that adds a new parameter to a given selector (that has to be a unary or keyword). The input is the following:

* the new parameter name
* the selector that is going to be modified
* the position of the new parameter in the selector
* the keyword for the new parameter
* the default value for senders of this message
* the collection of implementors affected by the change
* the collection of senders affected by the change
"
Class {
	#name : 'AddParameter',
	#superclass : 'ParametersSizeChange',
	#instVars : [
		'newParameter',
		'newParameterValue',
		'implementorTrailingString',
		'senderTrailingString',
		'index',
		'isAddingLast'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
AddParameter class >> addParenthesisTo: trimmedNewParameterValue ifNewParameterValueIsKeywordMessage: newParameterValueMethodNode [

	| newParameterNode |
	
	newParameterNode := newParameterValueMethodNode block statements first expr.
		
	^ (self hasToAddParenthesisBasedOn: newParameterNode)
		ifTrue: [ '(', trimmedNewParameterValue, ')' ]
		ifFalse: [ trimmedNewParameterValue ]
	
]

{ #category : 'pre-conditions' }
AddParameter class >> assert: anIndex isValidIndexFor: aNumberOfParameters [

	(anIndex isInteger and: [ (anIndex between: 1 and: aNumberOfParameters + 1)]) ifFalse: [ self signalInvalidParameterIndex: anIndex for: aNumberOfParameters ]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertCanAddParameterTo: aSelector [

	aSelector isInfix ifTrue: [ self signalSelectorCanNotBeBinary]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertIsKeyword: aSelector [

	aSelector isKeyword ifFalse: [ self signalSelectorMustBeKeyword]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertIsUnary: aSelector [

	aSelector isUnary ifFalse: [ self signalSelectorMustBeUnary]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertIsValidKeywordForNewParameter: aNewKeyword [

	(aNewKeyword isKeyword and: [aNewKeyword numArgs = 1 ]) ifFalse: [ self signalNotValidKeywordForNewParameter]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertIsValidParameterName: aName [

	| scannedNames |

	scannedNames := [ Scanner new scanFieldNames: aName ]
		on: Error
		do: [ :anError |  self signalInvalidParameterName: aName ].
	scannedNames size = 1 ifFalse: [ self signalInvalidParameterName: aName ].
	scannedNames first = aName ifFalse: [ self signalInvalidParameterName: aName ].

]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameter: aNewParameter isNotDefinedAsInstanceVariableInAny: implementors [

	| classesDefiningInsVars |

	classesDefiningInsVars := self classesDefiningInstanceVariable: aNewParameter inAny: implementors.
	classesDefiningInsVars ifNotEmpty: [
		self signalNewParameter: aNewParameter definedAsInstanceVariableIn: classesDefiningInsVars ]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameter: aNewParameter isNotDefinedAsLocalInAny: implementors [

	| implementorsDefiningNewParameterAsLocal |

	implementorsDefiningNewParameterAsLocal := implementors select: [ :implementor |
		implementor isValid and: [ implementor notOptimizedMethodNode hasLocalNamed: aNewParameter ]].
	implementorsDefiningNewParameterAsLocal ifNotEmpty: [
		self signalNewParameter: aNewParameter isDefinedAsLocalIn: implementorsDefiningNewParameterAsLocal ]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameterValueCompiles: trimmedNewParameterValue [

	^ [ Parser new parse: trimmedNewParameterValue readStream class: self noPattern: true notifying: nil ifFail: nil ]
		on: SyntaxErrorNotification 
		do: [ :error | self signalNewParameterValueDoesNotCompile ].
]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameterValueDoesNotHaveMoreThanOneStatement: newParameterValueMethodNode [

	newParameterValueMethodNode block statements size = 1 ifFalse: [ self signalNewParameterValueCanNotHaveMoreThanOneStatement ].
	
]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameterValueIsNotEmpty: aNewParameterValue [

	aNewParameterValue withBlanksTrimmed isEmpty ifTrue: [ self signalNewParameterValueCanNotBeEmpty]
]

{ #category : 'pre-conditions' }
AddParameter class >> assertNewParameterValueIsValid: aNewParameterValue [

	| newParameterValueMethodNode trimmedNewParameterValue |
	
	trimmedNewParameterValue := aNewParameterValue withBlanksTrimmed.
	self assertNewParameterValueIsNotEmpty: trimmedNewParameterValue.

	newParameterValueMethodNode := self assertNewParameterValueCompiles: trimmedNewParameterValue.
	self assertNewParameterValueDoesNotHaveMoreThanOneStatement: newParameterValueMethodNode.
	
	^self addParenthesisTo: trimmedNewParameterValue ifNewParameterValueIsKeywordMessage: newParameterValueMethodNode.
	
]

{ #category : 'exceptions' }
AddParameter class >> classesDefiningInstanceVariable: aName inAny: implementors [
 
	^ implementors 
		inject: Set new
		into: [ :classesDefiningInstVar :implementor | 
			(implementor methodClass whichClassDefinesInstanceVariable: aName ifNone: nil) ifNotNil: [ :classDefiningInstVar |
				classesDefiningInstVar add: classDefiningInstVar ].
			classesDefiningInstVar ]

]

{ #category : 'exceptions' }
AddParameter class >> errorMessageForInvalidParameterIndex: anIndex for: aNumberOfParameters [

	^anIndex printString, ' is an invalid insertion index. It has to be between 1 and ', (aNumberOfParameters + 1) printString
]

{ #category : 'exceptions' }
AddParameter class >> errorMessageForInvalidParameterName: aName [

	^ '''' , aName , ''' is not a valid parameter name'.
]

{ #category : 'exceptions' }
AddParameter class >> errorMessageForNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses [

	^ String streamContents: [ :messageStream |
		messageStream 
			nextPutAll: definingClasses asCommaStringAnd;
			space;
			nextPutAll: (definingClasses size = 1 ifTrue: [ 'defines' ] ifFalse: [ 'define' ]);
			space;
			nextPutAll: aNewParameter;
			nextPutAll: ' as instance variable' ]
]

{ #category : 'exceptions' }
AddParameter class >> errorMessageForNewParameterDefinedAsLocal: aNewParameter [

	^ aNewParameter, ' is already defined as parameter or temporary'
]

{ #category : 'pre-conditions' }
AddParameter class >> hasToAddParenthesisBasedOn: newParameterNode [
	
	^ newParameterNode isMessageNode and: [  newParameterNode selector key isKeyword ]
]

{ #category : 'instance creation - private' }
AddParameter class >> keywordImplementorTrailingFor: aNewKeyword and: aNewParameter addingLast: isAddingLast [

	^String streamContents: [ :stream |
		isAddingLast ifTrue: [ stream space ].
		stream
			nextPutAll: aNewKeyword;
			space;
			nextPutAll: aNewParameter;
			space ]
]

{ #category : 'instance creation - private' }
AddParameter class >> keywordSenderTrailingFor: aNewKeyword and: aNewParameterValue addingLast: isAddingLast [

	^String streamContents: [ :stream |
		isAddingLast ifTrue: [ stream space ].
		stream
			nextPutAll: aNewKeyword;
			space;
			nextPutAll: aNewParameterValue.
		isAddingLast ifFalse: [ stream space ] ]

	
]

{ #category : 'instance creation - private' }
AddParameter class >> named: aNewParameter  [
	at: anIndex
	addingLast: isAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector
	addingToImplementors: implementorTrailingString 
	addingToSenders: senderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	| trimmedNewParameter |
	
	trimmedNewParameter := aNewParameter withBlanksTrimmed.
	
	self assertCanAddParameterTo: anOldSelector.
	self assertIsValidParameterName: trimmedNewParameter.
	self assertNewParameter: trimmedNewParameter isNotDefinedAsInstanceVariableInAny: implementorsCollection.
	self assertNewParameter: trimmedNewParameter isNotDefinedAsLocalInAny: implementorsCollection.
	self assertAllImplementors: implementorsCollection haveSame: anOldSelector.
	self assertAllSenders: sendersCollection send: anOldSelector.
	
	^ self new 
		initializedNamed: trimmedNewParameter
		at: anIndex 
		addingLast: isAddingLast
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: aNewSelector
		addingToImplementors: implementorTrailingString 
		addingToSenders: senderTrailingString
		implementors: implementorsCollection 
		senders: sendersCollection

]

{ #category : 'instance creation' }
AddParameter class >> named: aNewParameter [
	at: anIndex 
	initializedWith: aNewParameterValue
	using: aNewKeyword 
	toKeywordSelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	| validNewParameterValue isAddingLast numberOfParameters |
	
	self assertIsKeyword: anOldSelector.
	self assertIsValidKeywordForNewParameter: aNewKeyword.
	validNewParameterValue := self assertNewParameterValueIsValid: aNewParameterValue.
	numberOfParameters := anOldSelector numArgs.
	self assert: anIndex isValidIndexFor: numberOfParameters.
	isAddingLast := anIndex > numberOfParameters.
	
	^self 
		named: aNewParameter 
		at: anIndex
		addingLast: isAddingLast 
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: (self newSelectorAdding: aNewKeyword at: anIndex to: anOldSelector) 
		addingToImplementors: (self keywordImplementorTrailingFor: aNewKeyword and: aNewParameter addingLast: isAddingLast) 
		addingToSenders: (self keywordSenderTrailingFor: aNewKeyword and: validNewParameterValue addingLast: isAddingLast)
		implementors: implementorsCollection 
		senders: sendersCollection

]

{ #category : 'instance creation' }
AddParameter class >> named: aNewParameter  [
	initializedWith: aNewParameterValue 
	toUnarySelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	| validNewParameterValue |
	
	self assertIsUnary: anOldSelector.
	validNewParameterValue := self assertNewParameterValueIsValid: aNewParameterValue.

	^self 
		named: aNewParameter 
		at: 1
		addingLast: true
		initializedWith: aNewParameterValue 
		to: anOldSelector 
		implementing: (self newSelectorFromUnary: anOldSelector)
		addingToImplementors: (self unaryImplementorTrailingFor: aNewParameter)
		addingToSenders: (self unarySenderTrailingFor: validNewParameterValue)
		implementors: implementorsCollection 
		senders: sendersCollection

]

{ #category : 'instance creation' }
AddParameter class >> named: aNewParameter  [
	initializedWith: aNewParameterValue
	using: aNewKeyword 
	toKeywordSelector: anOldSelector  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		at: anOldSelector numArgs + 1
		initializedWith: aNewParameterValue
		using: aNewKeyword 
		toKeywordSelector: anOldSelector  
		implementors: implementorsCollection 
		senders: sendersCollection

]

{ #category : 'exceptions' }
AddParameter class >> newParameterValueCanNotBeEmptyErrorMessage [
	
	^'New parameter value can not be empty'
]

{ #category : 'exceptions' }
AddParameter class >> newParameterValueCanNotHaveMoreThanOneStatementErrorMessage [
	
	^'New parameter value can not have more than one statement'
]

{ #category : 'exceptions' }
AddParameter class >> newParameterValueDoesNotCompileErrorMessage [
	
	^'New parameter value code does not compile'
]

{ #category : 'instance creation - private' }
AddParameter class >> newSelectorAdding: aNewKeyword at: anIndex to: anOldSelector [

	| keywords |
	
	keywords := anOldSelector keywords asOrderedCollection. 
	keywords add: aNewKeyword beforeIndex: anIndex.
	
	^(String streamContents: [ :strm | keywords do: [ :keyword | strm nextPutAll: keyword ]]) asSymbol.
	
	
]

{ #category : 'instance creation - private' }
AddParameter class >> newSelectorFromUnary: anOldSelector [

	^(anOldSelector, ':') asSymbol
]

{ #category : 'exceptions' }
AddParameter class >> notValidKeywordForNewParameterErrorMessage [
	
	^'New keyword can not be unary or binary. It has to be a keyword with one parameter'
]

{ #category : 'exceptions' }
AddParameter class >> selectorCanNotBeBinaryErrorMessage [
	
	^'Can not add parameter to a binary selector.
Rename it to a keyword message first.'
]

{ #category : 'exceptions' }
AddParameter class >> selectorMustBeKeywordErrorMessage [
	
	^'Selector must be of keyword type'
]

{ #category : 'exceptions' }
AddParameter class >> selectorMustBeUnaryErrorMessage [
	
	^'Selector must be unary'
]

{ #category : 'exceptions' }
AddParameter class >> signalInvalidParameterIndex: anIndex for: aNumberOfParameters [

	self refactoringError: (self errorMessageForInvalidParameterIndex: anIndex for: aNumberOfParameters)
]

{ #category : 'exceptions' }
AddParameter class >> signalInvalidParameterName: aName [

	self refactoringError: (self errorMessageForInvalidParameterName: aName)


]

{ #category : 'exceptions' }
AddParameter class >> signalNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses [

	self refactoringError: (self errorMessageForNewParameter: aNewParameter definedAsInstanceVariableIn: definingClasses)
]

{ #category : 'exceptions' }
AddParameter class >> signalNewParameter: aNewParameter isDefinedAsLocalIn: implementors [

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForNewParameterDefinedAsLocal: aNewParameter)
		references: (implementors collect: [ :implementor | MethodReference method: implementor ])
		to: aNewParameter 
	 
]

{ #category : 'exceptions' }
AddParameter class >> signalNewParameterValueCanNotBeEmpty [
	
	self refactoringError: self newParameterValueCanNotBeEmptyErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalNewParameterValueCanNotHaveMoreThanOneStatement [
	
	self refactoringError: self newParameterValueCanNotHaveMoreThanOneStatementErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalNewParameterValueDoesNotCompile [
	
	self refactoringError: self newParameterValueDoesNotCompileErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalNotValidKeywordForNewParameter [
	
	self refactoringError: self notValidKeywordForNewParameterErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalSelectorCanNotBeBinary [
	
	self refactoringError: self selectorCanNotBeBinaryErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalSelectorMustBeKeyword [
	
	self refactoringError: self selectorMustBeKeywordErrorMessage
]

{ #category : 'exceptions' }
AddParameter class >> signalSelectorMustBeUnary [
	
	self refactoringError: self selectorMustBeUnaryErrorMessage
]

{ #category : 'instance creation - private' }
AddParameter class >> unaryImplementorTrailingFor: aNewParameter [

	^ ': ', aNewParameter
]

{ #category : 'instance creation - private' }
AddParameter class >> unarySenderTrailingFor: aNewParameterValue [

	^ ': ', aNewParameterValue
]

{ #category : 'rename senders - private' }
AddParameter >> addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords  [
	
	| originalSourceCode |
	
	originalSourceCode := aMethodNode sourceText.
	insertionPoints do: [ :aPosition | | newPosition |
		newPosition := self firstNoSeparatorIndexIn: originalSourceCode startingFrom: aPosition.
		rangesToKeywords add: ((newPosition+1) to: newPosition) -> senderTrailingString ]
]

{ #category : 'rename senders - private' }
AddParameter >> addKeywordRangesUsing: insertionPoints to: rangesToKeywords [

	insertionPoints do: [ :aPosition | 
		rangesToKeywords add: (aPosition to: aPosition-1) -> senderTrailingString ]

]

{ #category : 'rename senders - private' }
AddParameter >> addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords [
	
	| insertionPoints |
	
	isAddingLast 
		ifTrue: [ 
			insertionPoints := self messageSendLastPositionIn: aMethodNode. 
			self addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords ]
		ifFalse: [ 
			insertionPoints := self messageSendKeywordPositionsIn: aMethodNode.
			self addKeywordRangesUsing: insertionPoints to: rangesToKeywords ]
]

{ #category : 'rename senders - private' }
AddParameter >> firstNoLineSeparatorIndexIn: sourceCode startingFrom: aStartingPosition [
	
	^self firstNot: [ :aChar | aChar isLineSeparator ] indexIn: sourceCode startingFrom: aStartingPosition 
]

{ #category : 'rename senders - private' }
AddParameter >> firstNoSeparatorIndexIn: sourceCode startingFrom: aStartingPosition [

	"Looks going back for the first no separator char. See #test24AddingParameterToSendersTakeCaresOfSeparators
	It assumes that there is always going to be a no separator wich holds due to how aStartPosition is obtained - Hernan"
	
	^self firstNot: [ :aChar | aChar isSeparator ] indexIn: sourceCode startingFrom: aStartingPosition 
]

{ #category : 'rename senders - private' }
AddParameter >> firstNot: aBlock indexIn: sourceCode startingFrom: aStartingPosition [
	
	| noSeparatorIndex |
	
	noSeparatorIndex := aStartingPosition.
	[ noSeparatorIndex > 0 and: [ aBlock value: (sourceCode at: noSeparatorIndex) ]] whileTrue: [ noSeparatorIndex := noSeparatorIndex - 1 ].
	
	^noSeparatorIndex 
		
]

{ #category : 'create new implementors - private' }
AddParameter >> implementorNewSourceCodeOf: anImplementor [

	| implementorMethodNode newSource originalSource insertionPoint |

	implementorMethodNode := anImplementor notOptimizedMethodNode.
	insertionPoint := isAddingLast
		ifTrue: [ implementorMethodNode selectorLastPosition ]
		ifFalse: [ (implementorMethodNode selectorKeywordPositionAt: index) first - 1].

	originalSource := anImplementor sourceCode.
	insertionPoint := self firstNoLineSeparatorIndexIn: originalSource startingFrom: insertionPoint.

	newSource := String streamContents: [ :newSourceStream |
		newSourceStream
			nextPutAll: (originalSource copyFrom: 1 to: insertionPoint);
			nextPutAll: implementorTrailingString;
			nextPutAll: (originalSource copyFrom: insertionPoint+1 to: originalSource size) ].

	^newSource
]

{ #category : 'initialization' }
AddParameter >> initializedNamed: aNewParameter  [
	at: anIndex
	addingLast: anIsAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector  
	addingToImplementors: anImplementorTrailingString 
	addingToSenders: aSenderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.
	
	newParameter := aNewParameter.
	newParameterValue := aNewParameterValue.
	implementorTrailingString := anImplementorTrailingString.
	senderTrailingString := aSenderTrailingString.
	
	index := anIndex.
	isAddingLast := anIsAddingLast 
]

{ #category : 'rename senders - private' }
AddParameter >> messageSendKeywordPositionsIn: aMethodNode [
	
	^aMethodNode messageSendKeywordPositionsAt: index of: oldSelector ifAbsent: [ #()].
]

{ #category : 'rename senders - private' }
AddParameter >> messageSendLastPositionIn: aMethodNode [
	
	^aMethodNode messageSendLastPositionsOf: oldSelector ifAbsent: [ #() ].
]

{ #category : 'parameter' }
AddParameter >> newParameter [
	
	^newParameter
]
