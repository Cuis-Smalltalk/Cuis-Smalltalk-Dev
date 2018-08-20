'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 8:12:52 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:12:10'!
argumentNameWithRangeDo: aBlock

	hereType == #word ifFalse: [^self expected: 'Argument name'].
	
	^self advanceWithRangeDo: aBlock! !


!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:07:48'!
method: doit context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap blk prim temps messageComment methodNode |
	sap := self pattern: doit inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	doit ifFalse: [self pragmaSequence].
	temps := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doit ifFalse: [self pragmaSequence].
	prim := self pragmaPrimitives.
	self statements: #() innerBlock: doit.
	blk := parseNode.
	doit ifTrue: [blk returnLast]
		ifFalse: [blk returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temps
		block: blk
		encoder: encoder
		primitive: prim
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
	self interactive ifTrue:
		[self declareUndeclaredTemps: methodNode.
		 self removeUnusedTemps].
	^methodNode! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:10:22'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	| args selector end start argumentName |
	
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1. nil }]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]].

	hereType == #word 
		ifTrue: [^ self advanceWithRangeDo: [ :sel :range | {sel asSymbol. {}. 1. {range}} ]].

	(hereType == #verticalBar or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [ | selectorRange |
		self advanceWithRangeDo: [ :sel :range |
			selector _ sel asSymbol.
			selectorRange _ range ].

		start _ self startOfNextToken + requestorOffset.
		argumentName _ self argumentName.
		end _ self endOfLastToken + requestorOffset.
	
		args _ Array with: (encoder bindArg: argumentName range: (start to: end)).
		^ {selector. args. 2. {selectorRange}}].

	hereType == #keyword ifTrue: [ | ranges |
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		ranges := OrderedCollection new.
		[hereType == #keyword] whileTrue:[ 
			self advanceWithRangeDo: [ :keywordAsString :range |
				selector nextPutAll: keywordAsString.
				ranges add: range ].
			
			start _ self startOfNextToken + requestorOffset.
			argumentName _ self argumentName.
			end _ self endOfLastToken + requestorOffset.
	
			args addLast: (encoder bindArg: argumentName range: (start to: end)).
		].
		^ {selector contents asSymbol. args. 3. ranges}].
	^self expected: 'Message pattern'
! !

