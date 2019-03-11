'From Cuis 5.0 of 7 November 2016 [latest update: #3652] on 11 March 2019 at 9:09:24 am'!

!Compiler methodsFor: 'public access' stamp: 'HAW 3/11/2019 09:04:53'!
compileDoIt: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: false
		doIt: true
		ifFail: failBlock! !

!Compiler methodsFor: 'private' stamp: 'HAW 3/11/2019 09:04:33'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self parser
		sourceStreamGetter: sourceStreamGetter;			"Cuis specific. Do not remove!!"
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 3/11/2019 09:00:46'!
doItInSelector

	^#DoItIn:! !

!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 3/11/2019 09:00:54'!
doItSelector

	^#DoIt! !


!Parser methodsFor: 'expression types' stamp: 'HAW 3/11/2019 09:01:34'!
method: noPattern doIt: doIt context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives temporaries messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	doIt ifFalse: [self pragmaSequence].
	temporaries := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doIt ifFalse: [self pragmaSequence].
	primitives := self pragmaPrimitives.
	
	self statements: #() innerBlock: doIt.
	block := parseNode.
	doIt 
		ifTrue: [block returnLast]
		ifFalse: [block returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temporaries
		block: block
		encoder: encoder
		primitive: primitives
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode! !

!Parser methodsFor: 'public access' stamp: 'HAW 3/11/2019 09:02:15'!
parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		startPosition _ myStream position.
		sourceCode _ myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ sourceStreamGetter notNil 	"Cuis specific. Do not remove!!"
					ifTrue: [ requestor perform: sourceStreamGetter ]
					ifFalse: [ ReadStream on: requestor text string ]].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!Scanner class reorganize!
('testing' findSelectorIn: findSelectorTests isLiteralSymbol: isValidInstanceVariableName: pseudoVariableNames)
('instance creation' new)
('class initialization' initialize initializeDoitCharacter)
('cached class state' doItCharacterValue initTypeTable releaseClassCachedState typeTable)
('doIt selectors' doItInSelector doItSelector)
!

