'From Cuis 5.0 [latest update: #4158] on 21 May 2020 at 5:47:22 pm'!

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

!Compiler methodsFor: 'private' stamp: 'jmv 4/17/2019 15:15:58'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self parser
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Parser methodsFor: 'expression types' stamp: 'RNG 3/13/2020 00:27:50'!
method: noPattern doIt: doIt context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives tempsDeclarationNode messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	doIt ifFalse: [self pragmaSequence].
	tempsDeclarationNode := self temporaries.
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
		temporariesDeclaration: tempsDeclarationNode
		block: block
		encoder: encoder
		primitive: primitives
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode! !

!Parser methodsFor: 'public access' stamp: 'jmv 4/17/2019 15:17:07'!
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
				myStream _ ReadStream on: requestor text string ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!Compiler methodsFor: 'private' stamp: 'HAW 3/11/2019 07:07:06'!
translate: aStream noPattern: noPattern ifFail: failBlock

	^self translate: aStream noPattern: noPattern doIt: noPattern ifFail: failBlock ! !


!Parser methodsFor: 'public access' stamp: 'HAW 3/11/2019 08:31:06'!
parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock 

	^self parse: sourceStream class: class category: nil noPattern: noPattern doIt: noPattern context: aContext notifying: aRequestor ifFail: aBlock ! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/17/2019 15:15:33'!
compileSelectionFor: anObject in: evalContext ifFail: failBlock

	| methodNode method sourceCode compiler |

	sourceCode := self selectionDoItSourceCodeIn: evalContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler		
			compileDoIt: sourceCode
			in: anObject class
			context: evalContext
			notifying: self
			ifFail: [ ^ failBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method _ methodNode generate.
	method methodNode: methodNode.

	^{ #method -> method. #compiler -> compiler } asDictionary 
! !

!methodRemoval: Parser #parse:class:category:noPattern:context:notifying:ifFail: stamp: 'jmv 5/21/2020 17:46:58'!
Parser removeSelector: #parse:class:category:noPattern:context:notifying:ifFail:!
!methodRemoval: Parser #method:context: stamp: 'jmv 5/21/2020 17:46:59'!
Parser removeSelector: #method:context:!
