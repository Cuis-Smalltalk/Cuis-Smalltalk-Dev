'From Cuis 5.0 of 7 November 2016 [latest update: #3817] on 26 June 2019 at 2:47:57 pm'!

!BlockClosure methodsFor: 'testing' stamp: 'sqr 6/26/2019 12:09:30'!
isCleanClosure
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free', 'clean' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Compiler evaluate: source. ] on: UndeclaredVariableWarning do: [ :ex | ex return].
	recreated isNil ifTrue: [^false].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			anyClosureDoesMethodReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'sqr 6/26/2019 12:46:01'!
withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

	| methodNode nodeAtRange nodeUnderCursor |
	methodNode := self codeProvider
		methodNodeOf: model actualContents
		ifErrorsParsing: [ :anError | aParsingErrorBlock valueWithPossibleArgument: anError.  anError return ].
	methodNode isNil ifTrue: [^nil].

	self startIndex < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: self startIndex ifAbsent: [ ^ aNoSelectorBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].

	^ aNoSelectorBlock value ! !


!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:48'!
should: aBlock raise: anExceptonHandlingCondition 

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [ :anException | ]
			! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:34'!
should: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [:anException | ] description: aFailDescription! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:39'!
should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock description: self defaultFailDescription! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:56:51'!
should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription

	| passed |
	passed := [aBlock value.  false]
		on: anExceptionHandlingCondition
		do: [:ex | assertionsBlock value: ex.  ex return: true].
	passed ifFalse: [self failWith: aFailDescription]! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:37:05'!
shouldnt: aBlock raise: anExceptonHandlingCondition 

	self shouldnt: aBlock raise: anExceptonHandlingCondition description: anExceptonHandlingCondition printString, ' was not expected to be raised'! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:37:10'!
shouldnt: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	aBlock 
		on: anExceptonHandlingCondition 
		do: [ :anException | self failWith: aFailDescription ]
! !

