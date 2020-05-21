'From Cuis 5.0 [latest update: #4151] on 21 May 2020 at 2:29:11 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 5/21/2020 14:12:42'!
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

!Parser methodsFor: 'public access' stamp: 'jmv 5/21/2020 14:13:11'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock

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
		[ methNode _ self method: noPattern context: aContext ]
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


!Decompiler methodsFor: 'private' stamp: 'eem 9/6/2008 08:45'!
blockScopeRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1! !


!Compiler methodsFor: 'private' stamp: 'jmv 5/21/2020 14:14:22'!
translate: aStream noPattern: noPattern ifFail: failBlock

	^self parser
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Parser methodsFor: 'public access' stamp: 'jmv 5/21/2020 14:13:27'!
parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock 

	^self parse: sourceStream class: class category: nil noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock ! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/21/2020 14:16:37'!
compileSelectionFor: anObject in: evalContext ifFail: failBlock

	| methodNode method sourceCode compiler |

	sourceCode := self selectionDoItSourceCodeIn: evalContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler		
			compileNoPattern: sourceCode
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

!methodRemoval: Parser #parse:class:category:noPattern:doIt:context:notifying:ifFail: stamp: 'jmv 5/21/2020 14:13:36'!
Parser removeSelector: #parse:class:category:noPattern:doIt:context:notifying:ifFail:!
!methodRemoval: Parser #method:doIt:context: stamp: 'jmv 5/21/2020 14:12:54'!
Parser removeSelector: #method:doIt:context:!
!methodRemoval: Compiler #translate:noPattern:doIt:ifFail: stamp: 'jmv 5/21/2020 14:14:46'!
Compiler removeSelector: #translate:noPattern:doIt:ifFail:!
!methodRemoval: Compiler #compileDoIt:in:context:notifying:ifFail: stamp: 'jmv 5/21/2020 14:16:51'!
Compiler removeSelector: #compileDoIt:in:context:notifying:ifFail:!
