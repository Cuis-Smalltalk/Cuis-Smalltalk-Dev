'From Cuis6.3 [latest update: #6328] on 23 April 2024 at 12:25:19 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 4/23/2024 12:19:12'!
method: noPattern context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives tempsDeclarationNode messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	noPattern ifFalse: [self pragmaSequence].
	tempsDeclarationNode := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	noPattern ifFalse: [self pragmaSequence].
	primitives := self pragmaPrimitives.
	
	self statements: #() innerBlock: noPattern.
	block := parseNode.
	noPattern 
		ifTrue: [block returnLast]
		ifFalse: [block returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporariesDeclaration: tempsDeclarationNode
		block: block
		encoder: encoder
		primitive: primitives
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode! !

!Parser methodsFor: 'public access' stamp: 'jmv 4/23/2024 12:24:36'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."

	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category := aCategory.
	myStream := sourceStream.
	[
		repeatNeeded := false.
		startPosition := myStream position.
		sourceCode := myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		noPattern ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag := noPattern.
		failBlock := aBlock.
		[ methNode := self method: noPattern context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded := true.
				myStream := requestor text string readStream ].
		repeatNeeded
	] whileTrue: [ encoder := self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!Compiler methodsFor: 'private' stamp: 'jmv 4/23/2024 12:24:22'!
translate: aStream noPattern: noPattern ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends

	^self parser
		optimizeSpecialSends: doOptimizeSpecialSends;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Parser methodsFor: 'public access' stamp: 'jmv 4/23/2024 12:22:06'!
parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock 

	^self parse: sourceStream class: class category: nil noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock! !

!methodRemoval: Parser #parse:class:category:noPattern:doIt:context:notifying:ifFail: stamp: 'jmv 4/23/2024 12:22:17'!
Parser removeSelector: #parse:class:category:noPattern:doIt:context:notifying:ifFail:!
!methodRemoval: Parser #method:doIt:context: stamp: 'jmv 4/23/2024 12:19:47'!
Parser removeSelector: #method:doIt:context:!
!methodRemoval: Compiler #translate:noPattern:doIt:ifFail:optimizeSpecialSends: stamp: 'jmv 4/23/2024 12:23:29'!
Compiler removeSelector: #translate:noPattern:doIt:ifFail:optimizeSpecialSends:!
