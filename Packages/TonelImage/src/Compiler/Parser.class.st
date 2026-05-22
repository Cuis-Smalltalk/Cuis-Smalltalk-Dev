"
I parse Smalltalk syntax and create a MethodNode that is the root of the parse tree. I look one token ahead.

See Documentation/HitchHikersCompiler.md
"
Class {
	#name : 'Parser',
	#superclass : 'Scanner',
	#instVars : [
		'here',
		'hereType',
		'hereMark',
		'hereEnd',
		'prevMark',
		'prevEnd',
		'encoder',
		'requestor',
		'parseNode',
		'failBlock',
		'requestorOffset',
		'tempsMark',
		'doitFlag',
		'properties',
		'category',
		'optimizeSpecialSends'
	],
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'accessing' }
Parser class >> chainExamplesDoubleColon [

	1 bitShift: 3 :: + 5.  "13"

	3 + 4 :: negated + 2 :: negated - 2 bitShift: 2 :: bitShift: -1.  "6"

	1 = 2 and: [3 = 4] :: ifTrue: [5] ifFalse: [6].  "6"

	1 = 1
		:: and: [2 = 2]
		:: and: [3 = 3]
		:: and: [4 = 4]
		:: ifTrue: [#yes] ifFalse: [#bummer].  "#yes"

	1 = 1 :: = true.  "true"

	1 = 1 :: ifTrue: [2] ifFalse: [3] :: + 1 ; + 10.  "12"

	1 bitShift: 3 :: + 2 bitShift: -1.  "5"

	1 to: 10 by: 3 :: size.  "4"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x].  "22"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x] :: bitShift: -1 :: - 1.  "10"

	CompiledMethod allInstances
		:: reject: [:x | x getSourceFromFile isNil]
		:: collect: [:x | x getSourceFromFile -> x]
		:: collect: [:x | x key occurrencesOf: $( :: + (x key occurrencesOf: $)) :: -> x key -> x value]
		:: asSortedCollection: [:x :y | x > y]
		:: inspect.
]

{ #category : 'services' }
Parser class >> methodHeaderLengthFrom: methodSource [

	| parser |
	parser := self new.
	parser parseSelector: methodSource.
	^ parser endOfLastToken min: methodSource size
]

{ #category : 'parsing' }
Parser class >> parse: sourceCode class: aClass [

	^self parse: sourceCode class: aClass noPattern: false
]

{ #category : 'parsing' }
Parser class >> parse: sourceCode class: aClass noPattern: aBoolean [

	^(self new
		parse: sourceCode class: aClass noPattern: aBoolean)
			sourceText: sourceCode;
			yourself
]

{ #category : 'accessing' }
Parser class >> primitivePragmaSelectors [
	"Answer the selectors of pragmas that specify VM primitives.
	 Needed for compile and decomple."
	^#(primitive:
		primitive:error:
		primitive:error:module:
		primitive:module:
		primitive:module:error:)
]

{ #category : 'services' }
Parser class >> selectorFrom: methodSource [

	^self new privateReadSelectorFrom: methodSource
]

{ #category : 'expression types' }
Parser >> addArgumentNames: argNames argRanges: argRanges toArguments: arguments isDoIt: aBoolean [

	| prevMarkRecover |
	prevMarkRecover := prevMark.
	argNames with: argRanges do: [ :argName :range |
		(argName = Encoder doItInContextName and: [ aBoolean ])
			ifTrue: [ arguments addLast: (encoder addMultiRange: range for: (encoder lookupVariable: argName ifAbsent: [ self shouldNotHappen ]))]
			ifFalse: [
				"This is done so #notify: is shown at the right position."
				prevMark := range first.
				arguments addLast: (encoder bindArg: argName range: range)]].
	prevMark := prevMarkRecover
]

{ #category : 'private' }
Parser >> addComment [
	(parseNode notNil and: [ currentComment notNil ])
		ifTrue: [
			parseNode addComment: currentComment.
			currentComment := nil ]
]

{ #category : 'expression types' }
Parser >> addKeywordPatternPartTo: selector keywordRanges: keywordRanges argumentNames: argNames argRanges: argRanges [

	self advanceWithRangeDo: [ :keywordAsString :range |
		selector nextPutAll: keywordAsString.
		keywordRanges add: range ].

	self argumentNameWithRangeDo: [ :argName :range |
		argNames addLast: argName.
		argRanges addLast: range ].
]

{ #category : 'pragmas' }
Parser >> addPragma: aPragma [
	properties := self properties copyWith: aPragma
]

{ #category : 'backtick' }
Parser >> addToSentInLiterals: aSymbolOrBinding [

	sentInLiterals add: aSymbolOrBinding 
]

{ #category : 'error handling' }
Parser >> addWarning: aString [
	"ignored by the default compiler."
]

{ #category : 'scanning' }
Parser >> advance [
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := self compensateTwoCharacterLookahead.
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this
]

{ #category : 'scanning' }
Parser >> advanceWithRangeDo: aBlock [

	| lexema start end |
	
	start := self startOfNextToken + requestorOffset.
	lexema := self advance.
	end := self endOfLastToken + requestorOffset.
	
	^ aBlock value: lexema value: (start to: end)
]

{ #category : 'primitives' }
Parser >> allocateLiteral: lit [
	encoder litIndex: lit
]

{ #category : 'expression types' }
Parser >> argumentName [
	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance
]

{ #category : 'expression types' }
Parser >> argumentNameWithRangeDo: aBlock [

	hereType == #word ifFalse: [^self expected: 'Argument name'].
	
	^self advanceWithRangeDo: aBlock
]

{ #category : 'expression types' }
Parser >> assignment: varNode [
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true
]

{ #category : 'backtick' }
Parser >> backtickExpression [

	| start range |

	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'expression'].
	(self match: #backtick) ifFalse: [^self expected: 'backtick'].
	range := start to: prevEnd.

	parseNode := BacktickNode
		expression: parseNode
		source: (source contents copyFrom: range first+1 to: range last-1)
		parser: self
		range: range.

	encoder noteSourceRange: range forNode:  parseNode.

	^true
]

{ #category : 'expression types' }
Parser >> binaryPattern [

	^ self advanceWithRangeDo: [ :selectorAsString :selectorRange | | selectorsymbol arguments |
		selectorsymbol := selectorAsString asSymbol.
		self selector: selectorsymbol.
		self argumentNameWithRangeDo: [ :argumentName :argumentRange |
		arguments := Array with: (encoder bindArg: argumentName range: argumentRange).
		{selectorsymbol. arguments. 2. {selectorRange}}]]
]

{ #category : 'temps' }
Parser >> bindArg: name [

	^ self bindTemp: name
]

{ #category : 'temps' }
Parser >> bindArg: aName range: aRange [

	^ self bindArg: aName
]

{ #category : 'temporaries' }
Parser >> bindTemp: name  [
	^name
]

{ #category : 'expression types' }
Parser >> blockExpression [
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode tempsDeclarationNode variableNodes temporaryBlockVariables start |

	self advance.
	
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[self argumentNameWithRangeDo: [ :argumentName :range |
			variableNodes addLast: (encoder bindBlockArg: argumentName within: blockNode range: range)]].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	tempsDeclarationNode := self temporaryBlockVariablesFor: blockNode.
	temporaryBlockVariables := tempsDeclarationNode allDeclaredVariableNodes.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporariesDeclaration: tempsDeclarationNode.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1].

	^true
]

{ #category : 'expression types' }
Parser >> braceExpression [
	" { elements } => BraceNode."

	| elements locations loc more sourceRangeStart sourceRangeEnd |

	sourceRangeStart := hereMark.
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression or right brace'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	sourceRangeEnd := hereEnd.

	(self match: #rightBrace) ifFalse: [^self expected: 'Period or right brace'].
	encoder noteSourceRange: (sourceRangeStart to: sourceRangeEnd) forNode: parseNode.

	^true
]

{ #category : 'error correction' }
Parser >> canDeclareClassVariable [
	^encoder classEncoding ~~ UndefinedObject
]

{ #category : 'error correction' }
Parser >> canDeclareInstanceVariable [
	^encoder classEncoding ~~ UndefinedObject
]

{ #category : 'expression types' }
Parser >> cascade [
	" {; message} => CascadeNode."

	| receiverNode messageNodes |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	receiverNode := parseNode cascadeReceiver.
	messageNodes := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := receiverNode.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			messageNodes addLast: parseNode].
	self createCascadeNodeWith: receiverNode and: messageNodes
]

{ #category : 'expression types' }
Parser >> chain [
	" {: message} => more messages"

	self advance.
	"The following three lines implement double colon chains"
	hereType == #colon ifFalse: [self expected: 'Chain'].
	hereMark-prevMark = 1 ifFalse: [self expected: 'Chain'].
	self advance.

	(self messagePart: 3 repeat: true) ifFalse: [self expected: 'Chain'].
	hereType == #semicolon ifTrue: [self cascade].
	hereType == #colon ifTrue: [self chain]
]

{ #category : 'scanning' }
Parser >> compensateTwoCharacterLookahead [

	^source position - (aheadChar = DoItCharacter ifTrue: [hereChar = DoItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2])
	
]

{ #category : 'error correction' }
Parser >> correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction [
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta adjustedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection := requestor selectionInterval.

	delta := self sourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first + delta to: interval last + delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector := UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]
]

{ #category : 'error correction' }
Parser >> correctVariable: proposedVariable interval: aSpot [
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action delta spot |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection := requestor selectionInterval.
	delta := self sourceDelta.
	spot := aSpot first + delta to: aSpot last + delta.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action := UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value ifNil: [self fail].
	
]

{ #category : 'expression types' }
Parser >> createCascadeNodeWith: receiverNode and: messageNodes [

	| sourceRangeOfFirstMessage |

	parseNode := CascadeNode new receiver: receiverNode messages: messageNodes.
	sourceRangeOfFirstMessage := encoder rawSourceRanges at: messageNodes first.
	sourceRangeOfFirstMessage ifNotNil: [
		| cascadeSourceRangeStart |
		cascadeSourceRangeStart := sourceRangeOfFirstMessage first.
		encoder noteSourceRange: (cascadeSourceRangeStart to: hereMark + 1) forNode: parseNode ]
]

{ #category : 'error correction' }
Parser >> createEmptyTempsDeclarationAfter: aDeclarationPosition [
	"Return the position of the end of the declaration."
	| offset |
	
	offset := self insertWord: ' | |' at: aDeclarationPosition + 1.
	^aDeclarationPosition + offset
]

{ #category : 'error correction' }
Parser >> createEmptyTempsDeclarationIfNecessaryIn: aBlockNode [
	"Return the new tempsMark for this BlockNode"
	| blockTempsMark |
	
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	(self hasNoTempDeclarationPipes: aBlockNode) ifTrue: [ 
		blockTempsMark := self createEmptyTempsDeclarationAfter: blockTempsMark ].
	^blockTempsMark
]

{ #category : 'error correction' }
Parser >> createTempDeclarationInMethodWith: aTempName [
	"No bars - insert some with CR, tab. Return the new tempsMark."
	
	| characterBeforeMark delta insertion theTextString offset |
	
	theTextString := requestor text string.
	characterBeforeMark := theTextString at: tempsMark - 1 ifAbsent: [$ ].
	insertion := '| ', aTempName, ' |', String newLineString.
	delta := 2.	"the bar and CR"
	characterBeforeMark = Character tab ifTrue: [
		insertion := insertion , String tab.
		delta := delta + 1.	"the tab"
	].
	
	offset := self insertWord: insertion at: tempsMark.

	^tempsMark + offset - delta.
]

{ #category : 'temps' }
Parser >> createTempDeclarationOf: variableNode sourceRange: sourceRange [

	| declarationNode |
	declarationNode := TemporaryDeclarationNode of: variableNode.
	encoder noteSourceRange: sourceRange forNode: declarationNode.
	^ declarationNode
]

{ #category : 'temps' }
Parser >> createTempsDeclarationWith: tempDeclarationNodes sourceRange: sourceRange [

	|tempsDeclarationNode|
	tempsDeclarationNode := TemporariesDeclarationNode withAll: tempDeclarationNodes.
	encoder noteSourceRange: sourceRange forNode: tempsDeclarationNode.
	^ tempsDeclarationNode
]

{ #category : 'error correction' }
Parser >> declarationRangesForTemps: someTempNodes [
	
	^someTempNodes collect: [ :temporaryNode | 
		(encoder rangeForNode: temporaryNode ifAbsent: nil) first ].
]

{ #category : 'error correction' }
Parser >> declareClassVar: name [
	| sym class |
	sym := name asSymbol.
	class := encoder classEncoding.
	class := class theNonMetaClass.		"not the metaclass"
	class addClassVarName: name.
	"Not needed in Cuis"
	"Smalltalk logChange: class definition."
	^ encoder global: (class classPool associationAt: sym)
			name: sym
]

{ #category : 'error correction' }
Parser >> declareGlobal: name [
	| sym |
	sym := name asSymbol.
	^encoder
		global: (Smalltalk declare: sym)
		name: sym
]

{ #category : 'error correction' }
Parser >> declareInstVar: name [
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarName: name.
	"Not needed in Cuis"
	"Smalltalk logChange: encoder classEncoding definition."
	^InstanceVariableNode new name: name index: encoder classEncoding instSize
		
]

{ #category : 'error correction' }
Parser >> declareTemp: name at: levelTag [
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^(encoder bindUndeclaredTemp: name)
		tag: levelTag;
		yourself
]

{ #category : 'error correction' }
Parser >> declareTempAndPaste: name [
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^encoder bindUndeclaredTemp: name
]

{ #category : 'error correction' }
Parser >> declareUndeclaredTemps: methodNode [
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."
	| undeclared userSelection blocksToVars |
	(undeclared := encoder undeclaredTemps) isEmpty ifTrue: [ ^ self ].
	userSelection := requestor selectionInterval.
	blocksToVars := IdentityDictionary new.
	undeclared do: [ :var |
		(blocksToVars
			at: (var tag == #method
				ifTrue: [ methodNode block ]
				ifFalse: [ methodNode accept: (VariableScopeFinder new ofVariable: var) ])
			ifAbsentPut: [ SortedCollection new ]) add: var name ].
	(blocksToVars removeKey: methodNode block ifAbsent: nil) ifNotNil: [ :rootVars |
		rootVars do: [ :varName |
			self pasteTempAtMethodLevel: varName ]].
	(blocksToVars keys sort: [ :a :b |
		a tempsMark < b tempsMark ]) do: [ :block | | blockUndeclaredVars |
		blockUndeclaredVars := blocksToVars at: block.
		self declareUndeclaredTemps: blockUndeclaredVars inBlock: block ].
	requestor
		selectInvisiblyFrom: userSelection first
		to: userSelection last + requestorOffset.
	ReparseAfterSourceEditing signal
]

{ #category : 'error correction' }
Parser >> declareUndeclaredTemps: undeclaredTempNodes inBlock: aDeclaringBlockNode [

	| blockTempsMark |
	
	blockTempsMark := self createEmptyTempsDeclarationIfNecessaryIn: aDeclaringBlockNode.
	undeclaredTempNodes do: [ :varName | blockTempsMark := self pasteTemp: varName before: blockTempsMark ]
]

{ #category : 'error correction' }
Parser >> defineClass: className [
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| classNameAsSymbol classCategory classDefinition userClassDefinition newClass |
	
	classNameAsSymbol := className asSymbol.
	classCategory := self 
		request: 'Enter class category:' 
		initialAnswer: self encoder classEncoding theNonMetaClass category 
		orCancel: [ ^nil ].
	classCategory ifEmpty: [classCategory := 'Unknown'].
	
	classDefinition := 'Object subclass: #' , classNameAsSymbol , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , classCategory , ''''.
	userClassDefinition := self 
		request: 'Edit class definition:' 
		initialAnswer: classDefinition 
		orCancel: [ ^nil ].
	userClassDefinition ifEmpty: [userClassDefinition := classDefinition].
	
	^[ newClass := Compiler evaluate: userClassDefinition.
	(newClass isKindOf: Behavior)
		ifTrue: [
			encoder
				global: (Smalltalk associationAt: classNameAsSymbol)
				name: classNameAsSymbol]
		ifFalse: [ 
			self inform: 
				('The provided class definition did not created a class but\the object: ', newClass printString) withNewLines.
			nil ]] 
		on: Error
		do: [ :anError | 
			self inform: ('There is an error in the provided class definition:\', anError description) withNewLines.
			anError return: nil ].
		
	
]

{ #category : 'expression types' }
Parser >> doitPatternInContext: context [

	self selector: self class doItSelector.
	^context
		ifNil: [{self class doItSelector. {}. 1. nil }]
		ifNotNil: [{self class doItInSelector. {encoder encodeVariable: Encoder doItInContextName}. 3. nil}]
]

{ #category : 'private' }
Parser >> encoder [
	^encoder ifNil:
		[encoder := CompiledMethod preferredBytecodeSetEncoderClass new]
]

{ #category : 'public access' }
Parser >> encoderClass: anEncoderClass [
	encoder ifNotNil: [
		self error: 'encoder already set'].
	encoder := anEncoderClass new
]

{ #category : 'scanning' }
Parser >> endOfLastToken [

	^ prevEnd ifNil: [mark]
]

{ #category : 'error handling' }
Parser >> expected: aString  [
	"Notify a problem at token 'here'."

	tokenType == #doIt ifTrue: [hereMark := hereMark + 1].
	hereType == #doIt ifTrue: [hereMark := hereMark + 1].
	^ self notify: aString , ' expected' at: hereMark + requestorOffset
]

{ #category : 'expression types' }
Parser >> expression [

	(hereType == #word and: [tokenType == #leftArrow])
		ifTrue: [^ self assignment: self variable].
	hereType == #leftBrace
		ifTrue: [self braceExpression]
		ifFalse: [self primaryExpression ifFalse: [^ false]].
	(self messagePart: 3 repeat: true) ifTrue:
		[
			hereType == #semicolon ifTrue: [self cascade].
			hereType == #colon ifTrue: [self chain]
		].
	^ true
]

{ #category : 'primitives' }
Parser >> externalErrorFunctionDeclaration [
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
	^true
]

{ #category : 'primitives' }
Parser >> externalErrorVariableDeclaration [
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
	^true
]

{ #category : 'primitives' }
Parser >> externalFunctionDeclaration [
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
	^true
]

{ #category : 'primitives' }
Parser >> externalType: descriptorClass [
	"Parse an return an external type"
	| xType |
	(here == #'.' and: [hereType == #period and: [tokenType == #period]]) ifTrue:
		[[here == #'.'] whileTrue: [self advance].
		 ^descriptorClass punctuatorNamed: #'...'].
	xType := descriptorClass atomicTypeNamed: here.
	xType ifNil: "Look up from class scope"
		[Symbol
			hasInterned: here
			ifTrue:[:sym| xType := descriptorClass structTypeNamed: sym]].
	xType ifNil:
		["Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	^(self matchToken: #*)
		ifTrue:[xType asPointerType]
		ifFalse:[xType]
]

{ #category : 'error handling' }
Parser >> fail [

	| exitBlock |
	(encoder == nil or: [encoder == self])
		ifFalse: [encoder release. encoder := nil]. "break cycle"
	exitBlock := failBlock.
	failBlock := nil.
	^exitBlock value
]

{ #category : 'error correction' }
Parser >> filterSeparatorsToTheLeftOn: currentSource startingAt: anInitialPosition [

	| start |
	
	start := anInitialPosition. 
	[ (currentSource at: start-1) isSeparator ] whileTrue: [ start := start - 1 ].
	
	^start 
	 	
]

{ #category : 'error correction' }
Parser >> filterSeparatorsToTheRightOn: currentSource startingAt: anInitialPosition [

	| end |
	
	end := anInitialPosition.	
	[ (currentSource at: end+1) isSeparator ] whileTrue: [ end := end + 1 ].
	
	^end

]

{ #category : 'error correction' }
Parser >> hasNoTempDeclarationPipes: aBlockNode [
	"Determine if a BlockNode already has the '| |' used to declare temps."
	| blockTempsMark sourceCode hasNoTemps |
	
	sourceCode := requestor text string.
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	hasNoTemps := aBlockNode temporaries isEmpty.
	^hasNoTemps and: [ (self isLastPipeOfEmptyTempsDeclaration: blockTempsMark) not ].
]

{ #category : 'private' }
Parser >> init: sourceStream notifying: req failBlock: aBlock [

	requestor := req.
	failBlock := aBlock.
	requestorOffset := 0.
	"Hey, don't call super with a different selector!"
	self scan: sourceStream.
	prevMark := hereMark := mark.
	self advance
]

{ #category : 'private' }
Parser >> initPattern: aString notifying: req return: aBlock [
	| result |
	self
		init: aString asPlainString readStream
		notifying: req
		failBlock: [^nil].
	encoder := self.
	result := aBlock value: (self pattern: false inContext: nil).
	encoder := failBlock := nil.  "break cycles"
	^result
]

{ #category : 'error correction' }
Parser >> insertWord: anInsertion at: aPosition [
	
	^self substituteWord: anInsertion
		wordInterval: (aPosition to: aPosition - 1)
		offset: 0.
]

{ #category : 'error handling' }
Parser >> interactive [
	"Answer true if compilation is interactive"

	^ requestor notNil
]

{ #category : 'error correction' }
Parser >> isLastPipeOfEmptyTempsDeclaration: pipeIndex [

	| indexOfPreviousPipe sourceCode |
	
	sourceCode := requestor text string.
	indexOfPreviousPipe := sourceCode lastIndexOf: $| startingAt: pipeIndex - 1 endingAt: 1 ifAbsent: [ ^false ].
	^(sourceCode at: pipeIndex) == $| and: [
		(sourceCode copyFrom: indexOfPreviousPipe + 1 to: pipeIndex - 1) allSatisfy: [ :char | char isSeparator ]]
]

{ #category : 'expression types' }
Parser >> isLiteralExpression [

	^ hereType == #string or: [hereType == #number or: [hereType == #literal]]
]

{ #category : 'expression types' }
Parser >> isLiteralNumberExpression [

	^ here == #- and: [tokenType == #number]
]

{ #category : 'expression types' }
Parser >> keywordPattern [

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
]

{ #category : 'expression types' }
Parser >> literalExpression [

	parseNode := self advanceWithRangeDo: [ :lexema :range | encoder encodeLiteral: lexema range: range ].

	^true
]

{ #category : 'expression types' }
Parser >> literalNumberExpression [

	self advanceWithRangeDo: [ :minusChar :minusRange |
		self advanceWithRangeDo: [ :number :numberRange |
			parseNode := encoder encodeLiteral: number negated range: (minusRange first to: numberRange last)]].

	^true
]

{ #category : 'comparing' }
Parser >> match: type  [
	"Answer with true if next tokens type matches"

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false
]

{ #category : 'scanning' }
Parser >> matchReturn [

	^ self match: #upArrow
]

{ #category : 'comparing' }
Parser >> matchToken: thing  [
	"matches the token, not its type"

	here = thing ifTrue: [self advance. ^true].
	^false
]

{ #category : 'expression types' }
Parser >> messagePart: level repeat: repeat [

	| start receiver selector args precedence words keywordStart |

	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: [
			start := self startOfNextToken.
			selector := source collectionSpecies writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue:
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [

			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue:
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue:
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
				optimizeSpecialSends: optimizeSpecialSends;
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true
]

{ #category : 'expression types' }
Parser >> method: noPattern context: ctxt [
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
	
	^methodNode
]

{ #category : 'expression types' }
Parser >> newMethodNode [
	^self encoder methodNodeClass new
]

{ #category : 'error correction' }
Parser >> newRangeRemovingTempsDeclarationFrom: currentSource  [
	startingAt: start
	to: originalEnd
	
	| end |
	
	end := self filterSeparatorsToTheRightOn: currentSource startingAt: originalEnd.
	
	(currentSource at: end+1) == $| ifTrue: [ | possibleNewStart |
		possibleNewStart := (self filterSeparatorsToTheLeftOn: currentSource startingAt: start) - 1.
		(currentSource at: possibleNewStart) == $| ifTrue: [ ^Array with: possibleNewStart with: end + 1 ]]. 
	
	^Array with: start with: end

]

{ #category : 'temps' }
Parser >> noTempsDeclaration [

	^ TemporariesDeclarationNode empty
]

{ #category : 'error handling' }
Parser >> notify: aString  [
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset
]

{ #category : 'error handling' }
Parser >> notify: string at: location [
	| adjustedLocation |
	adjustedLocation := location + self sourceDelta.
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [
				^ self fail ]. "failure setting up syntax error"
			SyntaxErrorNotification
				inClass: encoder classEncoding
				category: category
				withCode: 
					(source contents
						copyReplaceFrom: adjustedLocation
						to: adjustedLocation - 1
						with: string , ' ->')
				doitFlag: doitFlag
				errorMessage: string
				location: adjustedLocation]
		ifNotNil: [
			requestor
				notify: string , ' ->'
				at: adjustedLocation
				in: source].
	^self fail
]

{ #category : 'error handling' }
Parser >> offEnd: aString  [
	"Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!"

	requestorOffset == nil
		ifTrue: [^ self notify: aString at: mark]
		ifFalse: [^ self notify: aString at: mark + requestorOffset]

]

{ #category : 'public access' }
Parser >> optimizeSpecialSends: aBoolean [

	optimizeSpecialSends := aBoolean
]

{ #category : 'expression types' }
Parser >> parenthesisExpression [

	self advance.
	self expression ifFalse: [^self expected: 'expression'].
	(self match: #rightParenthesis) ifFalse: [^self expected: 'right parenthesis'].

	^true
]

{ #category : 'public access' }
Parser >> parse: sourceStreamOrString class: behavior [

	^ self parse: sourceStreamOrString readStream class: behavior
		noPattern: false context: nil notifying: nil ifFail: [^nil]
]

{ #category : 'public access' }
Parser >> parse: sourceStream class: class category: aCategory noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock [

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

	"See BackTickNode >> #addLiteralsOfExpressionToParser"
	sentInLiterals do: [ :each |
		each isSymbol ifTrue: [encoder noteOptimizedSelector: each].
		each isVariableBinding ifTrue: [encoder noteBactickLiteralBinding: each]].

	^ methNode.
]

{ #category : 'public access' }
Parser >> parse: sourceStreamOrString class: behavior noPattern: aBoolean [
	| stream |
	stream := (sourceStreamOrString is: #Stream)
		ifTrue: [sourceStreamOrString]
		ifFalse: [sourceStreamOrString asPlainString readStream].
	^ self
		parse: stream
		class: behavior
		noPattern: aBoolean
		context: nil
		notifying: nil
		ifFail: [^nil]
]

{ #category : 'public access' }
Parser >> parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock  [

	^self parse: sourceStream class: class category: nil noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock
]

{ #category : 'public access' }
Parser >> parse: sourceStream class: class noPattern: noPattern notifying: req ifFail: aBlock [

	^ self parse: sourceStream class: class noPattern: noPattern context: nil notifying: req ifFail: aBlock
]

{ #category : 'primitives' }
Parser >> parseAnExternalFunction: descriptorClass callType: callType checkReturnType: returnTypeBlockOrNil error: expectedBlock [
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
		ifAbsent: []
]

{ #category : 'public access' }
Parser >> parseSelector: aString  [
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self initScannerForTokenization.
	^self
		initPattern: aString
		notifying: nil
		return: [:pattern | pattern at: 1]
]

{ #category : 'error correction' }
Parser >> pasteTemp: name before: aTempsMark [
	"Return the new position of the tempsMark."
	
	| insertion theTextString  characterBeforeMark offset |
	
	theTextString := requestor text string.
	insertion := name, ' '.
	characterBeforeMark := theTextString at: aTempsMark - 1 ifAbsent: [$ ].
	characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
	offset := self insertWord: insertion at: aTempsMark.
	
	^aTempsMark + offset.
]

{ #category : 'error correction' }
Parser >> pasteTempAtMethodLevel: name [

	| theTextString |
	
	theTextString := requestor text string.
	tempsMark := 
		(theTextString at: tempsMark) == $| 
			ifTrue: [
  				"Paste it before the second vertical bar"
		 		self pasteTemp: name before: tempsMark ] 
			ifFalse: [ self createTempDeclarationInMethodWith: name ].
	
]

{ #category : 'expression types' }
Parser >> pattern: fromDoit inContext: ctxt [
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].
	hereType == #word ifTrue: [^self unaryPattern ].
	self transformVerticalBarAndUpArrowIntoABinarySelector.
	hereType == #binary ifTrue: [^self binaryPattern ].
	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: 'Message pattern'

]

{ #category : 'error correction' }
Parser >> performInteractiveChecks: aMethodNode [

	self
		declareUndeclaredTemps: aMethodNode;
		removeUnusedTempsOf: aMethodNode 
]

{ #category : 'error correction' }
Parser >> possibleVariablesFor: proposedVariable  [
	^encoder possibleVariablesFor: proposedVariable
]

{ #category : 'pragmas' }
Parser >> pragmaLiteral: selectorSoFar [
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (Scanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'
]

{ #category : 'pragmas' }
Parser >> pragmaPrimitives [
	| primitives |
	self properties isEmpty ifTrue:
		[^0].
	primitives := properties pragmas select:
					[:pragma|
					self class primitivePragmaSelectors includes: pragma keyword].
	primitives isEmpty ifTrue:
		[^0].
	primitives size > 1 ifTrue:
		[^self notify: 'Ambigous primitives'].
	^self perform: primitives first keyword withArguments: primitives first arguments
]

{ #category : 'pragmas' }
Parser >> pragmaSequence [
	"Parse a sequence of method pragmas."
	
	[ true ] whileTrue: [
		(self matchToken: #<)
			ifFalse: [ ^ self ].
		self pragmaStatement.
		(self matchToken: #>)
			ifFalse: [ ^ self expected: '>' ] ]
]

{ #category : 'pragmas' }
Parser >> pragmaStatement [
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
	^ true
]

{ #category : 'expression types' }
Parser >> primaryExpression [

	hereType == #word ifTrue: [ ^self variableExpression ].
	hereType == #leftBracket ifTrue: [ ^self blockExpression ].
	hereType == #leftBrace ifTrue: [ ^self braceExpression ].
	hereType == #leftParenthesis ifTrue: [ ^self parenthesisExpression ].
	(self isLiteralExpression) ifTrue: [ ^self literalExpression ].
	(self isLiteralNumberExpression) ifTrue: [ ^self literalNumberExpression ].
	hereType == #backtick ifTrue: [ ^self backtickExpression ].

	^false
]

{ #category : 'primitives' }
Parser >> primitive: anIntegerOrString [
	"Create indexed primitive."
	
	^self primitive: anIntegerOrString error: nil
]

{ #category : 'primitives' }
Parser >> primitive: anIntegerOrString error: errorCodeVariableOrNil [
	"Create indexed primitive with optional error code."
	
	^anIntegerOrString isInteger
		ifTrue:
			[errorCodeVariableOrNil ifNotNil:
				[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
			 anIntegerOrString]
		ifFalse:
			[anIntegerOrString isString
				ifTrue: [self primitive: anIntegerOrString module: nil error: errorCodeVariableOrNil]
				ifFalse: [self expected: 'Indexed primitive']]
]

{ #category : 'primitives' }
Parser >> primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil [
	"Create named primitive with optional error code."
	
	^self primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
]

{ #category : 'primitives' }
Parser >> primitive: aNameString module: aModuleStringOrNil [
	"Create named primitive."
	
	^self primitive: aNameString module: aModuleStringOrNil error: nil
]

{ #category : 'primitives' }
Parser >> primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil [
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117
]

{ #category : 'private' }
Parser >> privateReadSelector [
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := source collectionSpecies writeStream.
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: 'Message pattern'
]

{ #category : 'private' }
Parser >> privateReadSelectorFrom: aMethodSource [
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun 4824
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun  342
	"

	| result |
	self initScannerForTokenization.
	self
		init: aMethodSource asString asPlainString readStream
		notifying: nil
		failBlock: [ ^nil ].
	encoder := self.
	result := self privateReadSelector.
	encoder := failBlock := nil.  "break cycles"
	^result
]

{ #category : 'pragmas' }
Parser >> properties [
	^ properties ifNil: [ properties := AdditionalMethodState new ]
]

{ #category : 'error correction' }
Parser >> queryUndefined [
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	requestor selectFrom: varStart to: varStart + varName size - 1.
	(UndefinedVariable name: varName) ifFalse: [^ self fail]
]

{ #category : 'error correction' }
Parser >> rangesForRemovableUnusedTempsInBlockNode: aNodeWithTemporaries [
	
	| definedButUnusedTemps unusedTemps removableTemps |
	
	definedButUnusedTemps := aNodeWithTemporaries temporaries select: [ :temporaryNode |
		temporaryNode isUnusedTemp & temporaryNode isUndefTemp not ].
	definedButUnusedTemps notEmpty ifTrue: [
		definedButUnusedTemps := definedButUnusedTemps collect: [ :tempNode | tempNode name ].
		'There are defined but unused temporal variables.' print.
		('Consider removing them: ', definedButUnusedTemps asArray printString) print ].
	
	unusedTemps := aNodeWithTemporaries temporaries select: [ :temporaryNode |
		temporaryNode isUnusedTemp & temporaryNode isUndefTemp ].

	removableTemps := unusedTemps select: [ :temporaryNode |
		UnusedVariable name: temporaryNode name ].
	
	^self declarationRangesForTemps: removableTemps.
]

{ #category : 'error correction' }
Parser >> rangesForRemovableUnusedTempsOf: aMethodNode [
	
	| tempsToRemove |
	
	tempsToRemove := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: aMethodNode). 
	aMethodNode nodesDo: [ :node |
		node isBlockNode ifTrue: [
			tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: node) ]].
	
	^tempsToRemove
]

{ #category : 'error correction' }
Parser >> removeUnusedTempRange: aRangeToRemove with: delta [

	| currentSource start end newRangeToRemove | 

	currentSource := requestor text. 
	newRangeToRemove := self 
		newRangeRemovingTempsDeclarationFrom: currentSource 
		startingAt: aRangeToRemove first - delta 
		to: aRangeToRemove last - delta.
	start := newRangeToRemove first.
	end := newRangeToRemove last.
		
	requestor correctFrom: start to: end with: ''.
	
	^delta + end - start + 1

]

{ #category : 'error correction' }
Parser >> removeUnusedTempsOf: aMethodNode [
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	self removeUnusedTempsRanges: (self rangesForRemovableUnusedTempsOf: aMethodNode)
]

{ #category : 'error correction' }
Parser >> removeUnusedTempsRanges: tempsToRemove [
	
	tempsToRemove inject: 0 into: [ :delta :aRangeToRemove |
		self removeUnusedTempRange: aRangeToRemove with: delta ]
]

{ #category : 'private' }
Parser >> selector: aSymbol [
	encoder == self ifFalse: [
		encoder selector: aSymbol ].
	self properties selector: aSymbol.
]

{ #category : 'error correction' }
Parser >> sourceDelta [

	(encoder selector = Scanner doItSelector or: [ encoder selector = Scanner doItInSelector ]) ifTrue: [
		requestor notNil ifTrue: [
			^requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ]]].
	^ 0
]

{ #category : 'scanning' }
Parser >> startOfNextToken [
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark
]

{ #category : 'expression types' }
Parser >> statements: argNodes innerBlock: inner [

	^self statements: argNodes innerBlock: inner blockNode: BlockNode new
]

{ #category : 'expression types' }
Parser >> statements: argNodes innerBlock: inner blockNode: theBlockNode [

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	"Very important. Nil the comment, to avoid printing comments multiple times when pretty-printing"
	currentComment := nil.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						(stmts isEmpty and: [hereType ~= #period])
							ifTrue: [stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true
]

{ #category : 'error correction' }
Parser >> substituteSelector: selectorParts wordIntervals: spots [
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset := 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset := self substituteWord: word wordInterval: interval offset: offset ]

]

{ #category : 'error correction' }
Parser >> substituteVariable: each atInterval: anInterval  [
	self 
		substituteWord: each
		wordInterval: anInterval
		offset: 0.
	^encoder encodeVariable: each
]

{ #category : 'error correction' }
Parser >> substituteWord: correctWord wordInterval: spot offset: o [
	"Substitute the correctSelector into the (presumed interactive) receiver.
	 Update requestorOffset based on the delta size and answer the updated offset."

	requestor correctFrom: spot first + o to: spot last + o with: correctWord.
	requestorOffset := requestorOffset + correctWord size - spot size.
	^o + correctWord size - spot size
]

{ #category : 'expression types' }
Parser >> temporaries [
	" [ '|' (variable)* '|' ]"
	| tempDeclarationNodes theActualText declarationStartIndex |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [requestor selectionInterval first]
								ifFalse: [1].
			^ self noTempsDeclaration ].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ self noTempsDeclaration ].
	tempDeclarationNodes := OrderedCollection new.
	declarationStartIndex := prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode := encoder bindTemp: variableName range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifTrue: [
		tempsMark := prevMark.
		^ self
			createTempsDeclarationWith: tempDeclarationNodes
			sourceRange: (declarationStartIndex to: prevMark) ].
	^ self expected: 'Vertical bar'
]

{ #category : 'expression types' }
Parser >> temporaryBlockVariablesFor: aBlockNode [
	"Scan and answer temporary block variables."

	| tempDeclarationNodes declarationStartIndex |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[aBlockNode tempsMark: prevMark + requestorOffset.
		 ^ self noTempsDeclaration ].

	tempDeclarationNodes := OrderedCollection new.
	declarationStartIndex := prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode := encoder bindBlockTemp: variableName within: aBlockNode range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifFalse:
		[^self expected: 'Vertical bar'].
	aBlockNode tempsMark: prevMark + requestorOffset.
	^ self
		createTempsDeclarationWith: tempDeclarationNodes
		sourceRange: (declarationStartIndex to: prevMark)
]

{ #category : 'scanning' }
Parser >> transformVerticalBarAndUpArrowIntoABinarySelector [
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a series of consecutive vertical bars, up arrows, colons and regular binary selector characters.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain.
	Another special case is binary selectors ending in $-, like in
	1@-2
	This could be interpreted as `1 @ -2` or `1 @- 2`. Preference #atMinusDigitMeaning tells what to do."

	| toMakeBinary |
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	"Note: Also include #binary, to allow stuff like #+| where a special character comes after after a regular binary operator character. "
	toMakeBinary := #(binary verticalBar upArrow colon).
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asPlainString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			(token == #- and: [hereChar isDigit])
				ifTrue: [
					(Preferences at: #atMinusDigitMeaning) == #disabled
						"Disallow `1@-2`. Insists on some whitespace to disambiguate."
						ifTrue: [ ^self expected: 'A space character after selector' ].
					(Preferences at: #atMinusDigitMeaning) == #st80
						ifTrue: [ ^self ].				"interpret `1@-2` like `1 @ -2`"
					"Assume (Preferences at: #atMinusDigitMeaning) == #ansiSmalltalk
					interpret `1@-2` like `1 @- 2`, i.e. consider #@- a valid binary selector"
					].
			here := here , token asPlainString.
			hereEnd := hereEnd + 1.
			self scanToken ].
]

{ #category : 'expression types' }
Parser >> unaryPattern [

	^ self advanceWithRangeDo: [ :sel :range | | selectorsymbol |
		selectorsymbol := sel asSymbol.
		self selector: selectorsymbol.
		{selectorsymbol. {}. 1. {range}} ]
]

{ #category : 'expression types' }
Parser >> variable [
	
	^self advanceWithRangeDo: [ :variableName :range | | varName result rightRange |
		varName := variableName.
		
		"See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat
		There is a very difficult bug to fix. It happends when the source code ends with a return of a variable of 
		one char an no other char after that, for example: '^a' 
		In that case the range includes the ^ that is incorrect and makes the rename temporary fail.
		I do this fix becuase changing how the range is calculated is almost imposible due to the coupling 
		and complexity of the parser. This change applies only to variables and therefore it assures no
		unexpected behavior. I'm not cheching for size = 1 because it is redundant - Hernan"
		rightRange := varName size = range size
			ifTrue: [ range ]
			ifFalse: [ range last - varName size + 1 to: range last ].
		
		[result := encoder encodeVariable: varName sourceRange: rightRange ifUnknown: nil.
		result ifNil: [
			result := (UndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: rightRange first;
				varEnd: rightRange last;
				signal ].
		result isString ] whileTrue: [ varName := result].
		encoder addMultiRange: rightRange for: result ].
	
]

{ #category : 'expression types' }
Parser >> variableExpression [

	parseNode := self variable.

	"
	(parseNode isUndefTemp and: [self interactive] and: [
		((requestor isKindOf: Editor) or: [ requestor is: #Morph ])])
	"
	"If the requestor is of an exotic kind (like a telnet server) we might not be
	 allowed to open a PupUpMenu for querying the user"

	(parseNode isUndefTemp and: [ self interactive ])
		ifTrue: [self queryUndefined].
	parseNode nowHasRef.

	^true
]
