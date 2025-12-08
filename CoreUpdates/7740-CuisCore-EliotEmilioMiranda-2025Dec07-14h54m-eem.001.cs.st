'From Cuis7.5 [latest update: #7659] on 7 December 2025 at 4:32:00 pm'!

!Parser methodsFor: 'primitives' stamp: 'eem 12/7/2025 15:33:22'!
externalErrorFunctionDeclaration
	"Parse the function declaration for the error reaping augmentation of a call to an
	 external library. Create an instance of ExternalLibraryFunction and add it to an
	 existing ExternalLibraryFunction created by externalFunctionDeclaration."
	| descriptorClass errReaper extFunc |
	(properties pragmas anySatisfy: [:p| p keyword keywords first = #primitive: and: [p arguments first = 120]]) ifFalse:
		[^false].
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	"Parse return type"
	self advance.
	errReaper := self
				parseAnExternalFunction: descriptorClass
				callType: (descriptorClass callingConventionFor: #cdecl:)
				checkReturnType: [:returnType| returnType isIntegerType ifFalse: [^self expected: 'integral type']]
				error: [:reasonString| ^self expected: reasonString].
	extFunc := encoder firstLiteral.
	extFunc setErrorReaper: errReaper.
	^true! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/7/2025 15:37:06'!
externalErrorVariableDeclaration
	"Parse the function declaration for the error reaping augmentation of a call to an
	 external library. Create an instance of ExternalLibraryFunction and add it to an
	 existing ExternalLibraryFunction created by externalFunctionDeclaration."
	| descriptorClass errReaper extFunc |
	(properties pragmas anySatisfy: [:p| p keyword keywords first = #primitive: and: [p arguments first = 120]]) ifFalse:
		[^false].
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	"Parse variable type"
	self advance.
	(self externalType: descriptorClass)
		ifNil: [^self expected: 'variable type']
		ifNotNil:
			[:argType | | variableName moduleName variableClass |
			argType isIntegerType ifFalse:
				[^self expected: 'integral type'].
			hereType = #word ifFalse:
				[^self expected: 'variable name'].
			variableName := here.
			self advance.
			(self matchToken: 'module:')
				ifTrue:
					[moduleName := here.
					(self match: #string) ifFalse:
						[^self expected: 'module name string'].
					moduleName := moduleName asSymbol]
				ifFalse: [self advance].
			variableClass := Smalltalk at: #ExternalVariable ifAbsent: [^ false].
			errReaper := variableClass name: variableName module: moduleName type: argType].
	extFunc := encoder firstLiteral.
	extFunc setErrorReaper: errReaper.
	^true! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/7/2025 15:14:08'!
parseAnExternalFunction: descriptorClass callType: callType checkReturnType: returnTypeBlockOrNil error: expectedBlock
	"Parse just a function declaration followed by an optional module: name to yield an ExternalLibraryFunction."
	| retType externalName args module |
	"Parse return type"
	retType := self externalType: descriptorClass.
	retType ifNil: [expectedBlock value: 'return type'].
	returnTypeBlockOrNil ifNotNil:
		[returnTypeBlockOrNil value: retType].
	"Parse function name or index"
	externalName := here.
	(self match: #string) 
		ifTrue:[externalName := externalName asSymbol]
		ifFalse:[(self match:#number) ifFalse:[expectedBlock value: 'function name or index']].
	(self match: #leftParenthesis) ifFalse:[expectedBlock value: 'argument list'].
	args := WriteStream on: {}.
	[self match: #rightParenthesis] whileFalse:
		[(self externalType: descriptorClass)
			ifNil: [expectedBlock value: 'argument']
			ifNotNil:
				[:argType| argType isVoid & argType isPointerType not ifFalse:[args nextPut: argType]]].
	(self matchToken: 'module:') ifTrue:
		[module := here.
		(self match: #string) ifFalse:[expectedBlock value: 'String'].
		module := module asSymbol].
	^Smalltalk
		at: #ExternalLibraryFunction
		ifPresent:
			[:xfn|
			xfn name: externalName 
				module: module 
				callType: callType
				returnType: retType
				argumentTypes: args contents]
		ifAbsent: []! !


!SHParserST80 methodsFor: 'token testing' stamp: 'eem 12/7/2025 16:09:18'!
isTokenExternalErrorReaper
	^#(osErrFunc: osErrVar:) includes: currentToken! !

!SHParserST80 methodsFor: 'parse' stamp: 'eem 12/7/2025 16:17:59'!
parseExternalErrorReaperFunction
	self scanNext.
	self failWhen: currentToken isNil.
	self scanPast: #externalCallType.
	self failUnless: currentTokenFirst == $'.
	self parseString.
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failWhen: currentToken isNil.
			self scanPast: #externalCallType].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'eem 12/7/2025 16:24:04'!
parseExternalErrorReaperVariable
	self scanNext.
	self failWhen: currentToken isNil.
	currentToken = 'const' ifTrue:
		[self scanPast: #comment].
	self scanPast: #externalCallType.
	
	currentTokenFirst == $'
		ifTrue: [self parseString]
		ifFalse:
			[self failUnless: currentToken notNil.
			 self scanPast: #string].

	currentToken = 'module:'  ifTrue:
		[self scanPast: #module.
		self failUnless: currentTokenFirst == $'.
		self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !


!Parser methodsFor: 'pragmas' stamp: 'eem 12/7/2025 14:59:39'!
pragmaStatement
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	"These are ugly hacks into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].
	here = #osErrFunc: ifTrue:
		[ ^ self externalErrorFunctionDeclaration ].
	 here = #osErrVar: ifTrue:
		[ ^ self externalErrorVariableDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		selector := encoder asciiStringLiteral: selector.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last == $: or: [ selector first isLetter not ]) ifTrue: [
			arguments add: (encoder asciiStringLiteral: (self pragmaLiteral: selector)) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/7/2025 15:26:35'!
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library.
	(1) Create an instance of ExternalLibraryFunction and install it as first literal.
	(2) Add a pragma to primitive call 120."
	| descriptorClass callType modifier fn |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	fn := self
			parseAnExternalFunction: descriptorClass
			callType: callType
			checkReturnType: nil
			error: [:reasonString| ^self expected: reasonString].
	fn ifNotNil:
		[self allocateLiteral: fn].
	(self matchToken: 'error:')
		ifTrue:
			[| errorCodeVariable |
			 errorCodeVariable := here.
			(hereType == #string
			 or: [hereType == #word]) ifFalse:[^self expected: 'error code (a variable or string)'].
			 self advance.
			 self addPragma: (Pragma keyword: #primitive:error: arguments: (Array with: 120 with: errorCodeVariable)).
			 fn ifNotNil: [fn setErrorCodeName: errorCodeVariable]]
		ifFalse:
			[self addPragma: (Pragma keyword: #primitive: arguments: #(120))].
	^true! !


!SHParserST80 methodsFor: 'parse' stamp: 'eem 12/7/2025 16:15:27'!
parsePragmaSequence
	[currentToken = '<' ]
		whileTrue:[
			self scanPast: #primitiveOrExternalCallStart.
			currentToken = 'primitive:' 
				ifTrue: [
					self rangeType: #primitive.
					self parsePrimitive]
				ifFalse:[
					self isTokenExternalFunctionCallingConvention 
						ifTrue: [
							self rangeType: #externalFunctionCallingConvention.
							self parseExternalCall]
						ifFalse:
							[(self isTokenExternalErrorReaper
							 and: [(ranges anySatisfy: [:range| range rangeType == #externalCallType])
							 and: [ranges noneSatisfy: [:range| range rangeType == #externalErrorReaper]]]) "can't have two"
								ifTrue: [
									currentToken = 'osErrFunc:'
										ifTrue: [self parseExternalErrorReaperFunction]
										ifFalse:
											[currentToken = 'osErrVar:' ifTrue:
												[self parseExternalErrorReaperVariable]]]
								ifFalse:[
									self isName
										ifTrue:[
											self scanPast: #pragmaUnary.
											self failUnless: currentToken = '>'.
											self scanPast: #primitiveOrExternalCallEnd]
										ifFalse:[
											self isKeyword
												ifTrue:[
													self parsePragmaKeyword]
												ifFalse:[
													self isBinary
														ifTrue:[self parsePragmaBinary]
														ifFalse:[	self error	": 'Invalid External Function Calling convention'" ]]]]]]]! !


!Parser reorganize!
('private' addComment encoder init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom: selector:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalErrorFunctionDeclaration externalErrorVariableDeclaration externalFunctionDeclaration externalType: parseAnExternalFunction:callType:checkReturnType:error: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addArgumentNames:argRanges:toArguments:isDoIt: addKeywordPatternPartTo:keywordRanges:argumentNames:argRanges: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: performInteractiveChecks: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset:)
('public access' encoderClass: optimizeSpecialSends: parse:class: parse:class:category:noPattern:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
!

