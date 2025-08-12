'From Cuis7.5 [latest update: #7403] on 12 August 2025 at 2:36:11 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:00:30'!
addArgumentNames: argNames argRanges: argRanges toArguments: arguments isDoIt: aBoolean

	| prevMarkRecover |
	prevMarkRecover := prevMark.
	argNames with: argRanges do: [ :argName :range |
		(argName = Encoder doItInContextName and: [ aBoolean ])
			ifTrue: [ arguments addLast: (encoder addMultiRange: range for: (encoder lookupVariable: argName ifAbsent: [ self shouldNotHappen ]))]
			ifFalse: [
				"This is done so #notify: is shown at the right position."
				prevMark := range first.
				arguments addLast: (encoder bindArg: argName range: range)]].
	prevMark := prevMarkRecover! !

!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:02:02'!
addKeywordPatternPartTo: selector keywordRanges: keywordRanges argumentNames: argNames argRanges: argRanges

	self advanceWithRangeDo: [ :keywordAsString :range |
		selector nextPutAll: keywordAsString.
		keywordRanges add: range ].

	self argumentNameWithRangeDo: [ :argName :range |
		argNames addLast: argName.
		argRanges addLast: range ].! !

!Parser methodsFor: 'as yet unclassified' stamp: 'jmv 8/12/2025 13:07:52'!
selector: aSymbol
	encoder == self ifFalse: [
		encoder selector: aSymbol ].
	self properties selector: aSymbol.! !


!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:08:14'!
binaryPattern

	^ self advanceWithRangeDo: [ :selectorAsString :selectorRange | | selectorsymbol arguments |
		selectorsymbol := selectorAsString asSymbol.
		self selector: selectorsymbol.
		self argumentNameWithRangeDo: [ :argumentName :argumentRange |
		arguments := Array with: (encoder bindArg: argumentName range: argumentRange).
		{selectorsymbol. arguments. 2. {selectorRange}}]]! !

!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:08:09'!
keywordPattern

	| keywordRanges selector selectorsymbol argNames argRanges arguments |
	
	selector := source collectionSpecies writeStream.
	keywordRanges := OrderedCollection new.
	argNames := OrderedCollection new.
	argRanges := OrderedCollection new.
	arguments := OrderedCollection new.
	
	"[hereType == #keyword] whileTrue:[ 
		self addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments ]."
	
	[hereType == #keyword] whileTrue:[ 
		self addKeywordPatternPartTo: selector keywordRanges: keywordRanges  argumentNames: argNames argRanges: argRanges ].
	
	selectorsymbol := selector contents asSymbol.
	self selector: selectorsymbol.
	
	self addArgumentNames: argNames argRanges: argRanges toArguments: arguments isDoIt: selector contents = self class doItInSelector.
		
	^ {selectorsymbol. arguments. 3. keywordRanges}
		! !

!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:09:39'!
method: noPattern context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives tempsDeclarationNode messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
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

!Parser methodsFor: 'expression types' stamp: 'jmv 8/12/2025 13:08:19'!
unaryPattern

	^ self advanceWithRangeDo: [ :sel :range | | selectorsymbol |
		selectorsymbol := sel asSymbol.
		self selector: selectorsymbol.
		{selectorsymbol. {}. 1. {range}} ]! !


!Encoder methodsFor: 'private' stamp: 'jmv 8/12/2025 13:12:59'!
warnAboutShadowed: aVariableNode evenIfDuringFileIn: aBoolean

	| line1 line2 line3 msg |
	line1 := 'During the definition of: ', class name.
	selector ifNotNil: [ line1 := line1, '>>', selector printString. ].
	line2 := ' a clashing ', aVariableNode descriptiveString, ' was found.'.
	line3 := ' Please use a different name.'.
	msg := line1, line2, line3.
	
	msg print.
	requestor addWarning: msg.

	aBoolean
		ifTrue: [self notify: line2]
		ifFalse: [
			InMidstOfFileinNotification signal ifFalse: [
					requestor interactive
						ifTrue: [
							self notify: line2 ]
						ifFalse: [
							msg := line1, String newLineString, line2, String newLineString, line3.
							(RecompilationFailure class: class messageText: msg) signal ]]].! !

!methodRemoval: Parser #addKeywordPatternPartTo:keywordRanges:arguments: stamp: 'jmv 8/12/2025 14:32:26'!
Parser removeSelector: #addKeywordPatternPartTo:keywordRanges:arguments:!
