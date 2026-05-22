"
I am a Smalltalk method / expression parser.

Rather than creating an Abstract Syntax Tree, I create a sequence of SHRanges (in my 'ranges' instance variable), which represent the tokens within the String I am parsing.

I am used by a SHTextStylerST80 to parse method source strings.
I am able to parse incomplete / incorrect methods, and so can be used to parse methods that are being edited.

My 'source' instance variable should be set to the string to be parsed.

My 'classOrMetaClass' instance var must be set to the class or metaClass for the method source so that I can correctly resolve identifiers within the source. If this is nil , I parse the source as an expression (i.e. a doIt expression).

My 'workspace' instance variable can be set to a Workspace, so that I can resolve workspace variables.

Example 1.
	ranges := SHParserST80 new
		classOrMetaClass: Object;
		source: 'testMethod ^self';
		parse;
		ranges

"
Class {
	#name : 'SHParserST80',
	#superclass : 'Object',
	#instVars : [
		'classOrMetaClass',
		'source',
		'workspace',
		'arguments',
		'sourcePosition',
		'currentToken',
		'currentTokenFirst',
		'temporaries',
		'instanceVariables',
		'errorBlock',
		'currentTokenSourcePosition',
		'blockDepth',
		'bracketDepth',
		'ranges',
		'blockDepths',
		'blockDepthsStartIndexes',
		'braceDepth'
	],
	#category : 'Tools-Syntax Highlighting',
	#package : 'Tools',
	#tag : 'Syntax Highlighting'
}

{ #category : 'indentifier iteration' }
SHParserST80 >> blockArgNamesDo: aBlock [
	"Iterate over block argument names valid at current blockDepth"

	| title |
	title := '-- block arguments'.
	blockDepth to: 1 by: -1 do: [ :level |
		arguments at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]
]

{ #category : 'accessing' }
SHParserST80 >> blockDepths [
	^blockDepths
]

{ #category : 'accessing' }
SHParserST80 >> blockDepthsStartIndexes [
	^blockDepthsStartIndexes
]

{ #category : 'indentifier iteration' }
SHParserST80 >> blockTempNamesDo: aBlock [
	"Iterate over block temp names valid at current blockDepth"

	| title |
	title := '-- block variables'.
	blockDepth to: 1 by: -1 do: [ :level |
		temporaries at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]
]

{ #category : 'accessing' }
SHParserST80 >> classOrMetaClass [
	
	^classOrMetaClass
]

{ #category : 'accessing' }
SHParserST80 >> classOrMetaClass: aClass [
    classOrMetaClass := aClass
]

{ #category : 'indentifier iteration' }
SHParserST80 >> classVarNamesDo: aBlock [

	| title |
	title := '-- class variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c classPool keysDo: [ :name | aBlock value: name value: title ] ]]
]

{ #category : 'scan' }
SHParserST80 >> currentChar [
	^source at: sourcePosition ifAbsent: nil
]

{ #category : 'parse support' }
SHParserST80 >> enterBlock [
	blockDepth := blockDepth + 1.
	bracketDepth := bracketDepth + 1
]

{ #category : 'error handling' }
SHParserST80 >> error [
	self 
		rangeType: #excessCode
		start: (ranges isEmpty ifTrue: [1] ifFalse: [ranges last end + 1])
		end: source size.
	errorBlock value
]

{ #category : 'error handling' }
SHParserST80 >> failUnless: aBoolean [
	aBoolean ifFalse:[self error]

]

{ #category : 'error handling' }
SHParserST80 >> failWhen: aBoolean [
	aBoolean ifTrue:[self error]
]

{ #category : 'parse support' }
SHParserST80 >> initializeInstanceVariables [
	instanceVariables := classOrMetaClass 
		ifNotNil: [classOrMetaClass allInstVarNames asArray]
		ifNil: [Set new]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> instVarNamesDo: aBlock [

	| title |
	title := '-- instance variables'.
	instanceVariables do: [ :name | aBlock value: name value: title ]
]

{ #category : 'token testing' }
SHParserST80 >> isAnsiAssignment [
	^currentToken = ':='
]

{ #category : 'token testing' }
SHParserST80 >> isAssignment [

	^ currentToken = ':=' or: [ currentToken = '_' or: [ currentToken = Character smalltalkLeftArrow asString ]]
]

{ #category : 'character testing' }
SHParserST80 >> isBigDigit: aCharacter base: anInteger [
    "Answer true if aCharacter is a digit or a capital
    letter appropriate for base anInteger"
	| digitValue |
	
	digitValue := aCharacter digitValue.
	anInteger = 1 ifTrue: [
		^digitValue = 1 ].
	^digitValue >= 0 and:[digitValue < anInteger]
]

{ #category : 'token testing' }
SHParserST80 >> isBinary [
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	"Special case: '::' is not a binary selector but the Chain operator"
	(sourcePosition - currentTokenSourcePosition = 1 and: [
		(source at: currentTokenSourcePosition ifAbsent: nil) == $: and: [		
			(source at: sourcePosition ifAbsent: nil) == $: ]])
				ifTrue: [^ false ].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isBinarySelectorCharacter: c) or: [c == $:])
			ifFalse: [^false]].
	^true
]

{ #category : 'character testing' }
SHParserST80 >> isBinarySelectorCharacter: aCharacter [
	aCharacter == $: ifTrue: [^ false].
	^aCharacter isValidInBinarySelectors
]

{ #category : 'identifier testing' }
SHParserST80 >> isBlockArgName: aString  [
	"Answer true if aString is the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isBlockTempName: aString  [
	"Answer true if aString is the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isClassVarName: aSymbol  [
	"Answer true if aString is the name of a class variable, false otherwise"

	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c | 
				(c classPool bindingOf: aSymbol) ifNotNil: [^true]]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isGlobal: aSymbol [
	"Answer true if aString is the name of a global variable, false otherwise"

	(Smalltalk bindingOf: aSymbol) ifNotNil: [^true].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteBlockArgName: aString  [
	"Answer true if aString is the start of the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteBlockTempName: aString  [
	"Answer true if aString is the start of the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteClassVarName: aString  [
	"Answer true if aString is the start of the name of a class variable, false otherwise"

	self classVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteGlobal: aString [
	"Answer true if aString is the start of the name of a global variable, false otherwise"

	^Smalltalk hasBindingThatBeginsWith: aString
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteInstVarName: aString  [
	"Answer true if aString is the start of the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteMethodArgName: aString  [
	"Answer true if aString is the start of the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteMethodTempName: aString  [
	"Answer true if aString is the start of the name of a method temporary, false otherwise."

	self methodTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompletePoolConstantName: aString  [
	"Answer true if aString is the start of the name of a pool constant, false otherwise"

	self poolConstantNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteReservedName: aString  [
	"Answer true if aString is the start of a reserved name, false otherwise"

	self reservedNames do: [ :arg | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isIncompleteWorkspaceVarName: aString  [
	"Answer true if aString is the  start of the name of an workspace variable, false otherwise"

	self workspaceNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isInstVarName: aString  [
	"Answer true if aString is the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
]

{ #category : 'token testing' }
SHParserST80 >> isKeyword [
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last == $: ]
]

{ #category : 'identifier testing' }
SHParserST80 >> isMethodArgName: aString  [
	"Answer true if aString is the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
]

{ #category : 'identifier testing' }
SHParserST80 >> isMethodTempName: aString  [
	"Answer true if aString is the name of a method temporary, false otherwise.
    Does not check whether aString is also a block temporary
    or argument"

	self methodTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
]

{ #category : 'token testing' }
SHParserST80 >> isName [
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last isValidInIdentifiers ]
]

{ #category : 'identifier testing' }
SHParserST80 >> isPoolConstantName: aSymbol  [
	"Answer true if aString is the name of a pool constant, false otherwise"

	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [:p | (p bindingOf: aSymbol) ifNotNil: [ ^true ]]]].
	^false
]

{ #category : 'token testing' }
SHParserST80 >> isTokenExternalErrorReaper [
	^#(osErrFunc: osErrVar:) includes: currentToken
]

{ #category : 'token testing' }
SHParserST80 >> isTokenExternalFunctionCallingConvention [
	| descriptorClass |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: nil.
	descriptorClass ifNil: [^false].
	^(descriptorClass callingConventionFor: currentToken) notNil
]

{ #category : 'identifier testing' }
SHParserST80 >> isWorkspaceVarName: aString  [
	"Answer true if aString is the name of an workspace variable, false otherwise"
	workspace
		ifNotNil: [ ^(workspace hasBindingOf: aString) ].
	^false
]

{ #category : 'accessing' }
SHParserST80 >> last3Ranges [
	| r s lastI |
	s := ranges size.
	s = 0 ifTrue: [ ^ #(nil nil nil) ].
	r := ranges last.
	lastI := r rangeType = #excessCode
		ifTrue: [ s - 1 ]
		ifFalse: [ s].
	^{ 
		(lastI > 2 ifTrue: [ranges at: lastI-2]).
		(lastI > 1 ifTrue: [ranges at: lastI-1]).
		(lastI > 0 ifTrue: [ranges at: lastI])
	}
]

{ #category : 'accessing' }
SHParserST80 >> lastRange [
	| r i s |
	s := ranges size.
	s = 0 ifTrue: [ ^nil ].
	r := ranges last.
	^r rangeType = #excessCode
		ifFalse: [ r ]
		ifTrue: [
			i := s-1.
			i > 0 ifTrue: [ ranges at: i ]]
]

{ #category : 'parse support' }
SHParserST80 >> leaveBlock [
	arguments removeKey: blockDepth ifAbsent: nil.
	temporaries removeKey: blockDepth ifAbsent: nil.
	blockDepth := blockDepth - 1.
	bracketDepth := bracketDepth - 1
]

{ #category : 'indentifier iteration' }
SHParserST80 >> methodArgNamesDo: aBlock [
	"Iterate over method argument names"

	| title |
	title := '-- method arguments'.
	^arguments at: 0  ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> methodTempNamesDo: aBlock [
	"Iterate over method temporary names"

	| title |
	title := '-- method variables'.
	^temporaries at: 0 ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> namesBeginningWith: aString do: aBlock [
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name :kindOfIdentifierTitle |
		(name beginsWith: aString) ifTrue: [ aBlock value: name value: kindOfIdentifierTitle ]].
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- classes' ] in: Smalltalk classNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- globals' ] in: Smalltalk nonClassNames
]

{ #category : 'aux' }
SHParserST80 >> namesBeginningWith: aString do: aBlock in: aCollection [
	"aCollection is sorted"
	"
	self new namesBeginningWith: 'O' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorer' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorerWrapper' do: [ :each | each print ] in: Smalltalk classNames
	"
	| count |

	"Find the first element  starting with aString"
	count := aCollection size.
	aCollection
		findBinaryIndex: [ :element |
			element < aString
				ifFalse: [ -1 ]
				ifTrue: [ 1 ] ]
		do: [ :found | "Will never find any" ]
		ifNone: [ :a :b | | i n |
			i := b.
			[ i <= count and: [ 
				n := aCollection at: i.
				aString isEmpty or: [
					n beginsWith: aString ]]] whileTrue: [
				aBlock value: n.
				i := i + 1 ]]
]

{ #category : 'scan' }
SHParserST80 >> nextChar [
	sourcePosition := sourcePosition + 1.
	^source at: sourcePosition ifAbsent: [$ ]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> nonGlobalNamesDo: aBlock [
	"Evaluate aBlock over all available names, except for globals"

	self
		blockArgNamesDo: aBlock;
		blockTempNamesDo: aBlock;
		methodArgNamesDo: aBlock;
		methodTempNamesDo: aBlock;
		instVarNamesDo: aBlock;
		classVarNamesDo: aBlock;
		poolConstantNamesDo: aBlock;
		workspaceNamesDo: aBlock.
	self
		reservedNames do: [ :name |
			aBlock value: name value: '-- pseudovariables'  ]
]

{ #category : 'parse' }
SHParserST80 >> parse [
    "Parse the receiver's text as a Smalltalk method"

    self parse: (classOrMetaClass notNil)
]

{ #category : 'parse' }
SHParserST80 >> parse: isAMethod  [
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	| continue prevSourcePosition |
	self initializeInstanceVariables.
	sourcePosition := 1.
	arguments := Dictionary new.
	temporaries := Dictionary new.
	blockDepth := bracketDepth := braceDepth := 0.
	blockDepths := OrderedCollection with: blockDepth.
	blockDepthsStartIndexes := OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new] ifNotNil: [ ranges reset].
	errorBlock := [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		"Iterate once for methods, but pontentially several times for workspaces
		(to recover after errors, for possible good next lines or chunks)"
		continue := true.
		[ continue ] whileTrue: [
			prevSourcePosition := sourcePosition.
			self parseStatementList.
			continue := sourcePosition > prevSourcePosition.
			isAMethod
				ifTrue: [
					"Only if we are parsing a method, consider everything after this point as error."
					currentToken ifNotNil: [ self error ].
					continue := false]
				ifFalse: [
					sourcePosition > source size ifTrue: [continue := false]]].
	] ensure: [errorBlock := nil].
	^true
]

{ #category : 'parse' }
SHParserST80 >> parseArray [
	[currentTokenFirst == $)] whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd
]

{ #category : 'parse' }
SHParserST80 >> parseBacktick [
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock
]

{ #category : 'parse' }
SHParserST80 >> parseBinary  [
	| binary type |
	self parseUnary.
	[ self isBinary ]
		whileTrue: [
			binary := currentToken.
			type := #binary.
			(binary isEmpty or: [ Symbol hasInternedAndImplemented: binary ])
				ifFalse: [
					type := (Symbol thatStartsCaseSensitive: binary)
						ifNil: [ #undefinedBinary]
						ifNotNil: [ #incompleteBinary]].	
			self scanPast: type. 	
			self parseTerm.
            	self parseUnary ]
]

{ #category : 'parse' }
SHParserST80 >> parseBinaryMessagePattern    [

   	self scanPast:  #patternBinary. 
	self failUnless: self isName.
	self scanPast: #patternArg.


]

{ #category : 'parse' }
SHParserST80 >> parseBlock [

	"Just read $["
	blockDepths add: blockDepth+1.
	blockDepthsStartIndexes add: sourcePosition-1.

	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $: ifTrue: [self parseBlockArguments].
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].

	"Just read $]"
	blockDepths add: blockDepth-1.
	blockDepthsStartIndexes add: sourcePosition.

	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock
]

{ #category : 'parse' }
SHParserST80 >> parseBlockArguments [
	[ currentTokenFirst == $: ]
		whileTrue: [
			self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg ].
	currentTokenFirst == $| 
		ifTrue: [ self scanPast: #blockArgsBar ]
]

{ #category : 'parse' }
SHParserST80 >> parseBlockTemporaries [

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #blockTempBar ].

	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #blockTempBar.
			[self isName] 
				whileTrue: [self scanPast: #blockPatternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #blockTempBar]
]

{ #category : 'parse' }
SHParserST80 >> parseBraceArray [
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace level: braceDepth.
	braceDepth := braceDepth - 1
]

{ #category : 'parse' }
SHParserST80 >> parseByteArray [
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst == $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst == $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failWhen: currentTokenFirst == $. .
				self error]].
	self scanPast: #arrayEnd
]

{ #category : 'parse' }
SHParserST80 >> parseCascadeAndChain [

	| beforeKeywordPos |
	beforeKeywordPos := currentTokenSourcePosition.
	self parseKeyword.

	"At least one message send before cascade and chain"
	beforeKeywordPos =​ currentTokenSourcePosition ifTrue: [ ^self ].

	currentTokenFirst == $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst == $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst == $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		].
]

{ #category : 'parse' }
SHParserST80 >> parseCharSymbol [
	| s e |
	s := sourcePosition - 1.
	e := sourcePosition.
	self nextChar.
	self scanPast: #symbol start: s end: e
]

{ #category : 'parse' }
SHParserST80 >> parseExpression [
	| assignType |
	self isName 
		ifTrue: [
			self scanPast: (self resolve: currentToken).
			self isAssignment 
				ifTrue: [
					assignType := self isAnsiAssignment 
						ifTrue: [#ansiAssignment]
						ifFalse: [#assignment].
					self scanPast: assignType.
					self parseExpression]
				ifFalse: [self parseCascadeAndChain]]
		ifFalse: [
			self parseTerm.
			self parseCascadeAndChain]
]

{ #category : 'parse' }
SHParserST80 >> parseExternalCall [
	self scanNext.
	self failWhen: currentToken isNil.
	self scanPast: #externalCallType.
	currentToken = '*' 
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: 	[
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failWhen: currentToken isNil.
			self scanPast: #externalCallType.
			currentToken = '*' 
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parseExternalErrorReaperFunction [
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
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parseExternalErrorReaperVariable [
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
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parseKeyword  [
    | keyword rangeIndices |
	self parseBinary.
	keyword := ''.
	rangeIndices := #().
	[
    		[ self isKeyword ]
        		whileTrue: [
				keyword := keyword, currentToken. 
				self rangeType: #keyword.
				"remember where this keyword token is in ranges"
				rangeIndices := rangeIndices copyWith: ranges size.
				self scanNext.
				self parseTerm.
				self parseBinary ]
	] ensure: [ | type |
		"do this in an ensure so that it happens even if the errorBlock evaluates before getting here"
		"patch up the keyword tokens, so that incomplete and undefined ones look different"
		(keyword isEmpty or: [ Symbol hasInternedAndImplemented: keyword ])
			ifFalse: [
				type := (Symbol thatStartsCaseSensitive: keyword)
					ifNil: [ #undefinedKeyword]
					ifNotNil: [ #incompleteKeyword ].
				rangeIndices do: [ :i | (ranges at: i) type: type ]]]
]

{ #category : 'parse' }
SHParserST80 >> parseKeywordMessagePattern    [

	[self isKeyword]
		whileTrue: [ 
			self scanPast:  #patternKeyword. 
			self failUnless: self isName.
			self scanPast: #patternArg]


]

{ #category : 'parse' }
SHParserST80 >> parseLiteral: inArray  [
	currentTokenFirst == $$ 
		ifTrue: [
			| pos |
			self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			self scanPast: #character start: pos end: pos.
			^self ].
	currentTokenFirst isDigit 
		ifTrue: [
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentToken = '-' 
		ifTrue: [
			| c |
			c := self currentChar.
			(inArray and: [c isNil or: [c isDigit not]]) 
				ifTrue: [
					"single - can be a symbol in an Array"
					self scanPast: #symbol.
					^self ].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			"token isNil ifTrue: [self error: 'Unexpected End Of Input']."
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentTokenFirst == $' ifTrue: [
		self parseString.
		^self ].
	currentTokenFirst == $# ifTrue: [
		self parseSymbol.
		^self ].
	(inArray and: [currentToken notNil]) ifTrue: [
		self scanPast: #symbol.
		^self ].
	self failWhen: currentTokenFirst == $. .
	self error	": 'argument missing'"
]

{ #category : 'parse' }
SHParserST80 >> parseLiteralArrayElement [
	 currentTokenFirst isValidStartOfIdentifiers ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst == $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true
]

{ #category : 'parse' }
SHParserST80 >> parseMessagePattern    [

	self isName 
		ifTrue: [self parseUnaryMessagePattern]
		ifFalse: [
			self isBinary
				ifTrue:[self parseBinaryMessagePattern]
				ifFalse:[
					self failUnless: self isKeyword.
					self parseKeywordMessagePattern]]
]

{ #category : 'parse' }
SHParserST80 >> parseMethodTemporaries [

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #methodTempBar ].

	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #methodTempBar.
			[self isName] 
				whileTrue: [self scanPast: #patternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #methodTempBar]
]

{ #category : 'parse' }
SHParserST80 >> parsePragmaBinary [

	self scanPast: #pragmaBinary.
	self isName
		ifTrue:[self scanPast: (self resolvePragmaArgument: currentToken)] 
		ifFalse:[	self parseLiteral: false].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parsePragmaKeyword [

	[self isKeyword]
		whileTrue:[
			self scanPast: #pragmaKeyword.
			self isName
				ifTrue:[self scanPast: (self resolvePragmaArgument: currentToken)] 
				ifFalse:[	self parseLiteral: false]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parsePragmaSequence [
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
														ifFalse:[	self error	": 'Invalid External Function Calling convention'" ]]]]]]]
]

{ #category : 'parse' }
SHParserST80 >> parsePrimitive [
	self scanNext.
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: [
			self failUnless: ('''#' includes: currentTokenFirst).
			self parseStringOrSymbol.
			currentToken = 'module:' ifTrue: [
				self scanPast: #module.
				self failUnless: ('''#' includes: currentTokenFirst).
				self parseStringOrSymbol]].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : 'parse' }
SHParserST80 >> parseStatement [
	(currentToken isNil or: [	currentToken size = 1]) ifTrue: [
		currentTokenFirst == $^ ifTrue: [self scanPast: #return].
		currentTokenFirst = Character smalltalkUpArrow ifTrue: [self scanPast: #return]].
	self parseExpression.
]

{ #category : 'parse' }
SHParserST80 >> parseStatementList [
	
	[[currentTokenFirst == $.] whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil and: [currentTokenFirst ~~ $]]) 
		ifTrue: [self parseStatement].
	currentTokenFirst == $.] 
			whileTrue: [self scanPast: #statementSeparator]
]

{ #category : 'parse' }
SHParserST80 >> parseStatementListForBraceArray [
	"same as parseStatementList, but does not allow empty statements e.g {...$a...}.
	A single terminating . IS allowed e.g. {$a.} "

	
	[currentTokenFirst ~~ $} ifTrue: [self parseExpression].
	currentTokenFirst == $.] 
		whileTrue: [self scanPast: #statementSeparator]
]

{ #category : 'parse' }
SHParserST80 >> parseString [
	| first c last |
	first := sourcePosition.
	[
		(c := self currentChar) ifNil: [
			self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error": 'unfinished string'" ].
		c ~~ $' or: [
			self peekChar == $' and: [
				sourcePosition := sourcePosition + 1.
				true ]]] whileTrue: [ sourcePosition := sourcePosition + 1 ].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last
]

{ #category : 'parse' }
SHParserST80 >> parseStringOrSymbol [

	currentTokenFirst == $' ifTrue: [ ^self parseString ].
	currentTokenFirst == $# ifTrue: [ ^self parseSymbol ].
	self error
]

{ #category : 'parse' }
SHParserST80 >> parseSymbol [
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c := self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [
		self parseSymbolString.
		^self ].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol
]

{ #category : 'parse' }
SHParserST80 >> parseSymbolIdentifier [
	| c start end |
	c := self currentChar.
	self failUnless: ( c isValidStartOfIdentifiers or: [ c == $: ]).
	start := sourcePosition.
	[
		c := self nextChar.
		c isValidInIdentifiers or: [ c == $: ]
	] whileTrue: [].
	end := sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end
]

{ #category : 'parse' }
SHParserST80 >> parseSymbolSelector [
	| start end |
	start := sourcePosition - 1.
	end := sourcePosition.
	[self isBinarySelectorCharacter: self nextChar] 
		whileTrue: [end := sourcePosition].
	self scanPast: #symbol start: start end: end
]

{ #category : 'parse' }
SHParserST80 >> parseSymbolString [
	| first c last |
	first := sourcePosition.
	self nextChar.
	[
		(c := self currentChar) 
			ifNil: [
				self rangeType: #unfinishedString start: first end: source size.
				self error	": 'unfinished string'"].
		c ~~ $' or: [
			self peekChar == $' 
				ifTrue: [sourcePosition := sourcePosition + 1. true] 
				ifFalse: [false]]
	] whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self scanPast: #stringSymbol start: first - 1 end: last
]

{ #category : 'parse' }
SHParserST80 >> parseTerm [
	self failWhen: currentToken isNil.
	currentTokenFirst == $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst == $[ ifTrue: [^self parseBlock].
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			braceDepth := braceDepth + 1.
			self scanPast: #leftBrace level: braceDepth.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false
]

{ #category : 'parse' }
SHParserST80 >> parseUnary [
	| unary type |
	[ self isName ]
		whileTrue: [
			unary := currentToken.
			type := #unary.
			(unary isEmpty or: [ Symbol hasInternedAndImplemented: unary ])
				ifFalse:[
					type := (Symbol thatStartsCaseSensitive: unary)
						ifNil: [ #undefinedUnary]
						ifNotNil: [ #incompleteUnary ]].
			self scanPast: type ]
]

{ #category : 'parse' }
SHParserST80 >> parseUnaryMessagePattern [
	
	 self scanPast: #patternUnary

]

{ #category : 'scan' }
SHParserST80 >> peekChar [
	^source at: sourcePosition + 1 ifAbsent: [ $  ]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> poolConstantNamesDo: aBlock [

	| title |
	title := '-- pool variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [ :pool |
					pool bindingsDo: [ :assoc | aBlock value: assoc key value: title ]]]]
]

{ #category : 'parse support' }
SHParserST80 >> pushArgument: aString  [
	(arguments at: blockDepth ifAbsentPut: [OrderedCollection new]) 
		add: aString
]

{ #category : 'parse support' }
SHParserST80 >> pushTemporary: aString  [
	(temporaries at: blockDepth ifAbsentPut: [OrderedCollection new]) 
		add: aString
]

{ #category : 'recording ranges' }
SHParserST80 >> rangeType: aSymbol  [
	^self 
		rangeType: aSymbol
		start: currentTokenSourcePosition
		end: currentTokenSourcePosition + currentToken size - 1
]

{ #category : 'recording ranges' }
SHParserST80 >> rangeType: aSymbol start: s end: e  [
	^ranges add: (SHRange start: s end: e type: aSymbol)
]

{ #category : 'accessing' }
SHParserST80 >> ranges [
	^ ranges
]

{ #category : 'accessing' }
SHParserST80 >> rangesWithoutExcessCode [
	
	^ranges 
		ifEmpty: [ ranges ]
		ifNotEmpty: [ 
			ranges last rangeType = #excessCode
				ifTrue: [ ranges allButLast ]
				ifFalse: [ ranges ]]
]

{ #category : 'indentifier iteration' }
SHParserST80 >> reservedNames [

	^Theme current pseudoVariables
]

{ #category : 'identifier testing' }
SHParserST80 >> resolve: aString [

	self reservedNames do: [ :symbol | aString = symbol ifTrue: [^symbol]].
	(self isBlockTempName: aString) ifTrue: [^#blockTempVar].
	(self isBlockArgName: aString) ifTrue: [^#blockArg].
	(self isMethodTempName: aString) ifTrue: [^#tempVar].
	(self isMethodArgName: aString) ifTrue: [^#methodArg].
	(self isInstVarName: aString) ifTrue: [^#instVar].
	(self isWorkspaceVarName: aString) ifTrue: [^#workspaceVar].
	Symbol hasInterned: aString ifTrue: [ :symbol |
		(self isClassVarName: symbol) ifTrue: [ ^#classVar ].
		(self isPoolConstantName: symbol) ifTrue: [ ^#poolConstant].
		(self isGlobal: symbol) ifTrue: [^#globalVar]].
	^self resolvePartial: aString
]

{ #category : 'identifier testing' }
SHParserST80 >> resolvePartial: aString  [
	"check if any identifier begins with aString"
	
	(self isIncompleteReservedName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteBlockTempName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteBlockArgName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteMethodTempName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteMethodArgName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteInstVarName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteWorkspaceVarName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteClassVarName: aString) ifTrue: [ ^#incompleteIdentifier ].
	(self isIncompletePoolConstantName: aString) ifTrue: [ ^#incompleteIdentifier ].
	(self isIncompleteGlobal: aString) ifTrue: [^#incompleteIdentifier].
	^#undefinedIdentifier
]

{ #category : 'identifier testing' }
SHParserST80 >> resolvePartialPragmaArgument: aString  [
	"check if any valid pragma argument begins with aString"
	
	(#('true' 'false' 'nil') anySatisfy: [:each | each beginsWith: aString]) 
		ifTrue: [^#incompleteIdentifier].
	"should really check that a matching binding is for a Class?"
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [:c | 
				(Smalltalk hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]]]
		ifFalse: [(Smalltalk hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]].
	^#undefinedIdentifier
]

{ #category : 'identifier testing' }
SHParserST80 >> resolvePragmaArgument: aString  [
	(#('true' 'false' 'nil') includes: aString) ifTrue: [^aString asSymbol].
	"should really check that global is a class?"
	Symbol hasInterned: aString ifTrue: [:sym | 
		classOrMetaClass isBehavior 
			ifTrue: [
				classOrMetaClass theNonMetaClass withAllSuperclasses do: [:c | 
					(Smalltalk bindingOf: sym) ifNotNil: [^#globalVar]]]
			ifFalse: [(Smalltalk bindingOf: sym) ifNotNil: [^#globalVar]]].
	^self resolvePartialPragmaArgument: aString
]

{ #category : 'scan' }
SHParserST80 >> scanBinary [
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := source class with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (source class with: d).
			self nextChar.
			^currentToken].
	"|| empty temp declaration"
	"This makes || colorize as an invalid method definition.
	It is better to allow #|| as valid selector, and mark an empty temp declaration as invalid instead!"
	"(c == $| and: [d == $|])
		ifTrue: [^currentToken]."
	c := d.
	[
		d := self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken := currentToken copyWith: c.
			c := self nextChar ].
	^currentToken
]

{ #category : 'scan' }
SHParserST80 >> scanComment [
	| c s e |
	s := sourcePosition.
	
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar) 
		ifNil: [
			self rangeType: #unfinishedComment start: s end: source size.
			^self error	": 'unfinished comment'"].
	c == $"] 
		whileFalse: [].
	e := sourcePosition.
	s < e ifTrue: [self rangeType: #comment start: s end: e].
	self nextChar.
	self scanWhitespace
]

{ #category : 'scan' }
SHParserST80 >> scanIdentifier [
	| c start |
	start := sourcePosition.
	[ (c := self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c == $: and: [(self isBinarySelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition := start
]

{ #category : 'scan' }
SHParserST80 >> scanNext [
	self scanWhitespace.
	currentTokenFirst := self currentChar.
	currentTokenFirst 
		ifNil: [" end of input "
			currentTokenFirst := $ .
			currentTokenSourcePosition := nil.
			currentToken := nil.
			^nil].
	currentTokenFirst isDigit ifTrue: [^self scanNumber].
	currentTokenFirst isValidStartOfIdentifiers ifTrue: [^self scanIdentifier].
	^self scanBinary
]

{ #category : 'scan' }
SHParserST80 >> scanNumber [
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: ((source copyFrom: start to: sourcePosition - 1) readStream).
			self peekChar == $- ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc == $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c == $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $. 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [
					sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^currentTokenSourcePosition := start]
				ifTrue: [self skipDigits]].
	c := self currentChar.
	('deqp' includes: c)
		ifTrue: [
			((nc := self nextChar) isDigit or: [nc == $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start
]

{ #category : 'scan' }
SHParserST80 >> scanPast: rangeType  [
	"record rangeType for current token .
	record argument and temp declarations.
	scan and answer the next token"
	rangeType == #blockPatternArg ifTrue: [self pushArgument: currentToken].
	rangeType == #blockPatternTempVar ifTrue: [self pushTemporary: currentToken].
	rangeType == #patternArg ifTrue: [self pushArgument: currentToken].
	rangeType == #patternTempVar ifTrue: [self pushTemporary: currentToken].
	^self
		rangeType: rangeType;
		scanNext
]

{ #category : 'scan' }
SHParserST80 >> scanPast: rangeType level: level [
	"first level adds no suffix to the rangeType.
	Suffix from 1 to 7 added in cycles , ((level-2) mod(7) + 1)"
	| cycle typePlusCycle |
	
	cycle := level <= 1 
		ifTrue: [0]
		ifFalse:[ ((level - 2) \\ 7) + 1].
	typePlusCycle := cycle = 0 
		ifTrue:[rangeType]
		ifFalse:[(rangeType, cycle printString) asSymbol].
	^self scanPast: typePlusCycle

]

{ #category : 'scan' }
SHParserST80 >> scanPast: rangeType start: startInteger end: endInteger [
	"record rangeType for current token from startInteger to endInteger,
	 and scanNext token"

	^self 
		rangeType: rangeType start: startInteger end: endInteger;
		scanNext
	

]

{ #category : 'scan' }
SHParserST80 >> scanWhitespace [
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $" ifTrue: [self scanComment]
]

{ #category : 'scan' }
SHParserST80 >> skipBigDigits: baseInteger [
	[self isBigDigit: self nextChar base: baseInteger] 
		whileTrue: []

]

{ #category : 'scan' }
SHParserST80 >> skipDigits [
	[self nextChar isDigit] 
		whileTrue: []
]

{ #category : 'accessing' }
SHParserST80 >> source [
	^source
]

{ #category : 'accessing' }
SHParserST80 >> source: aString [
    source := aString
]

{ #category : 'accessing' }
SHParserST80 >> workspace: aWorkspace [
    workspace := aWorkspace
]

{ #category : 'indentifier iteration' }
SHParserST80 >> workspaceNamesDo: aBlock [

	| title |
	title := '-- workspace variables'.
	workspace ifNotNil: [
		workspace bindingNamesDo: [ :name | aBlock value: name value: title ] ]
]
