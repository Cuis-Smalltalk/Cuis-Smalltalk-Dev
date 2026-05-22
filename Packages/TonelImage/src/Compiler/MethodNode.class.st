"
I am the root of the parse tree..

Instance Variables
	arguments: <SequenceableCollection>
	block: <BlockNode>
	encoder: <BytecodeEncoder>
	localsPool: <IdentitySet>
	locationCounter: <Integer>
	precedence: <Integer>
	primitive: <Integer>
	properties: <AdditionalMethodState|nil>
	selectorOrFalse: <Object>
	sourceText: <String|Text>
	temporaries: <SequenceableCollection>
	temporariesDeclaration: <TemporariesDeclarationNode>

arguments
	- the collection of parsed or decompiled method arguments

block
	- the BlockNode holding the method's statements

encoder
	- the object that comprises the copiler's scope table, literal pool and back-end bytecode generator

localsPool
	- a set used to determine the set of copied values for each block in the method

locationCounter
	- an integer used to mark block scopes for the purposes of the closure transformation.  See BlockNode>>#analyseArguments:temporaries:rootNode:

precedence
	- the precedence of the method's selector (see Symbol>>precedence)

primitive
	- if non-zero this is the integer code of the method's primitive

properties
	- the object used to accumulate method properties (a.k.a. pragmas)

selectorOrFalse
	- the method's selector or false if this is a doit

sourceText
	- the source test from which the method was compiled

temporaries
	- the collection of parsed or decompiled method temporaries

temporariesDeclaration
	- an alternative way to represent the temporaries declaration, by using a parse node to represent that; it should eventually replace the need for the "temporaries" instance variable (because the temporaries can be obtained through this object); every read to "temporaries" can be replaced by sending #allDeclaredVariableNodes to this object; right now the Parser initializes both "temporaries" and "temporariesDeclaration" to ease the migration process
"
Class {
	#name : 'MethodNode',
	#superclass : 'CodeNode',
	#instVars : [
		'selectorOrFalse',
		'precedence',
		'arguments',
		'block',
		'primitive',
		'encoder',
		'temporaries',
		'properties',
		'sourceText',
		'locationCounter',
		'localsPool',
		'selectorKeywordsRanges',
		'temporariesDeclaration'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
MethodNode >> accept: aVisitor [
	^aVisitor visitMethodNode: self
]

{ #category : 'code generation (closures)' }
MethodNode >> addLocalsToPool: locals "<Set of: TempVariableNode>" [
	localsPool isNil ifTrue:
		[localsPool := IdentitySet new].
	localsPool addAll: locals
]

{ #category : 'source mapping' }
MethodNode >> addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition [

	| insidePosition |

	insidePosition := literalArrayPosition first.
	[ insidePosition < literalArrayPosition last ] whileTrue: [
		insidePosition := self nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition ].
	
]

{ #category : 'testing' }
MethodNode >> allParseNodesWithin: aSourceCodeInterval satisfy: aCondition [

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			(sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ])
				ifTrue: [ ^ false ] ] ].
	^ true
]

{ #category : 'testing' }
MethodNode >> anyParseNodeWithin: aSourceCodeInterval satisfy: aCondition [

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			(sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ])
				ifTrue: [ ^ true ] ] ].
	^ false
]

{ #category : 'accessing' }
MethodNode >> argumentNames [

	^arguments collect: [ :anArgumentNode | anArgumentNode name ]
]

{ #category : 'accessing' }
MethodNode >> arguments [
	"For transformations etc, not used in compilation"
	^arguments
]

{ #category : 'accessing' }
MethodNode >> arguments: aCollectionOfArguments [

	"For transformations etc, not used in compilation"
	arguments := aCollectionOfArguments
]

{ #category : 'initialization' }
MethodNode >> block [
	^ block
]

{ #category : 'debugger support' }
MethodNode >> blockExtentsToTempsMap [
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := encoder classEncoding parserClass new
						encoderClass: encoder class;
						parse: (sourceText
							ifNil: [self decompileString]
							ifNotNil: [sourceText asPlainString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]
]

{ #category : 'accessing' }
MethodNode >> body [
	^block
]

{ #category : 'source mapping' }
MethodNode >> calculateCompleteSourceRangesOf: nodes	 [
	
	| sourcesRanges firstNode lastNode interval |
	
	sourcesRanges := nodes collect: [:node | 	self completeSourceRanges at: node ifAbsent: [#()]].
	
	sourcesRanges size = 1 ifTrue: [ ^sourcesRanges first ].
	
	firstNode := sourcesRanges first.
	lastNode := sourcesRanges last.
	
	interval := SourceCodeInterval from: firstNode first first to: lastNode first last.
	
	^OrderedCollection with: interval


]

{ #category : 'printing' }
MethodNode >> classAndSelector [

	^self methodClass name, '>>', self selector storeString
]

{ #category : 'source mapping' }
MethodNode >> completeSourceRanges [
	"Returns the 'expanded' version of the source ranges, for instance in message sends it also includes the receiver, and if there are parentheses they are included in the source range as well. Right now used for refactorings."

	^ encoder completeSourceRangesBasedOn: self sourceText
]

{ #category : 'source mapping' }
MethodNode >> completeSourceRangesDo: aBinaryBlock [
	"block has to receive parse node and collection of source ranges"

	^ self completeSourceRanges keysAndValuesDo: aBinaryBlock
]

{ #category : 'source mapping' }
MethodNode >> completeSourceRangesOf: requestedParseNode ifAbsent: emptySourceRangesBlock [
	"Returns the source ranges associated with the requested parse node."

	| completeSourceRanges |

	completeSourceRanges := Set new.

	"There may be more than one equivalent key in the complete source ranges map
	if more than one block declare a temporary variable with the same name - Fernando"
	self completeSourceRangesDo: [:parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [completeSourceRanges addAll: sourceRanges]].

	^completeSourceRanges ifEmpty: emptySourceRangesBlock 
	
]

{ #category : 'converting' }
MethodNode >> decompileString  [
	"Answer a string description of the parse tree whose root is the receiver."

	^self fullPrintString

]

{ #category : 'source ranges' }
MethodNode >> definitionStartPosition [
	
	"It does not includes temp definition because the extract can include temps - Hernan"
	^self selectorLastPosition + 1
]

{ #category : 'code generation' }
MethodNode >> encoder [
	^ encoder
]

{ #category : 'code generation (closures)' }
MethodNode >> ensureClosureAnalysisDone [
	block blockExtent ifNil:
		[temporaries := block analyseArguments: arguments temporaries: temporaries rootNode: self]
]

{ #category : 'testing' }
MethodNode >> equivalentTo: aParseNode [

	^ aParseNode isMethodNode
		and: [ block equivalentTo: aParseNode block]
]

{ #category : 'code generation' }
MethodNode >> generate [
	"The receiver is the root of a parse tree. Answer a CompiledMethod."

	^self generate: #(0 0 0 0)
]

{ #category : 'code generation' }
MethodNode >> generate: trailer  [
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod
]

{ #category : 'code generation' }
MethodNode >> generate: trailer ifQuick: methodBlock [
	^self generate: trailer using: CompiledMethod ifQuick: methodBlock
]

{ #category : 'code generation' }
MethodNode >> generate: trailer using: aCompiledMethodClass [
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := aCompiledMethodClass
					createMethod: blkSize
					trailer: trailer
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method
]

{ #category : 'code generation' }
MethodNode >> generate: trailer using: aCompiledMethodClass ifQuick: methodBlock [
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]])
		ifFalse: [^ self].
	v := block code.
	v < 0
		ifTrue: [^ self].
	v = LdSelf
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3)
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType))
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]
]

{ #category : 'testing' }
MethodNode >> hasArgumentOrTemporaryNamed: aVariableName [

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 
	
	^self tempNames includes: aVariableName
]

{ #category : 'debugger support' }
MethodNode >> hasGeneratedMethod [
	^encoder hasGeneratedMethod
]

{ #category : 'testing' }
MethodNode >> hasLocalNamed: aName [

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 

	^ encoder hasLocalNamed: aName 
]

{ #category : 'printing' }
MethodNode >> ifPrimitivePrintOn: aStream [
	
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream newLineTab: 1.
			 self printPrimitiveOn: aStream]].

]

{ #category : 'testing' }
MethodNode >> isMethodNode [

	^true.
]

{ #category : 'testing' }
MethodNode >> isMultipleRanges: aRangeOrRanges [

	^aRangeOrRanges isKindOf: OrderedCollection 
]

{ #category : 'code generation (closures)' }
MethodNode >> locationCounter [
	^locationCounter
]

{ #category : 'source mapping' }
MethodNode >> messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure [

	^encoder messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure
]

{ #category : 'source mapping' }
MethodNode >> messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock [

	^encoder messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock
	
]

{ #category : 'source mapping' }
MethodNode >> messageSendLastPositionsOf: aSelector ifAbsent: aBlock [

	^encoder messageSendLastPositionsOf: aSelector ifAbsent: aBlock
]

{ #category : 'source mapping' }
MethodNode >> messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock [

	^encoder messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock
]

{ #category : 'printing' }
MethodNode >> methodClass [

	^ encoder classEncoding
]

{ #category : 'initialization' }
MethodNode >> movingTemporariesDeclarationToBlock [
	
	block temporariesDeclaration: temporariesDeclaration.
	self temporariesDeclaration: TemporariesDeclarationNode empty.
	
	^self 
]

{ #category : 'source mapping' }
MethodNode >> nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition [

	| symbolStartPosition nextPosition |

	symbolStartPosition := sourceText indexOfSubCollection: symbolString startingAt: insidePosition.

	symbolStartPosition = 0
		ifTrue: [ nextPosition := SmallInteger maxVal ]
		ifFalse: [
			nextPosition := symbolStartPosition + symbolString size.
			(sourceText at: nextPosition) tokenish ifFalse: [ symbolPositions add: (symbolStartPosition to: nextPosition - 1) ]].

	^nextPosition

	
]

{ #category : 'code generation (closures)' }
MethodNode >> noteBlockEntry: aBlock [
	"Evaluate aBlock with the numbering for the block entry."
	locationCounter isNil ifTrue:
		[locationCounter := -1].
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
]

{ #category : 'code generation (closures)' }
MethodNode >> noteBlockExit: aBlock [
	"Evaluate aBlock with the numbering for the block exit."
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
]

{ #category : 'accessing' }
MethodNode >> numberOfStatements [

	^ block statements size
]

{ #category : 'source mapping' }
MethodNode >> parameterDefinitionPositionAt: anIndex [

	^encoder parameterDefinitionPositionFor: (arguments at: anIndex)
]

{ #category : 'source mapping' }
MethodNode >> parseNodeIncluding: aPosition ifAbsent: aBlockClosure  [
	
	^encoder parseNodeIncluding: aPosition ifAbsent: aBlockClosure 

]

{ #category : 'source mapping' }
MethodNode >> parseNodesPathAt: aPosition ifAbsent: aBlockClosure [

	"This is to be sure that the enconder has the completeSourceRanges - Hernan"
	self completeSourceRanges.
	^ encoder
		parseNodesPathAt: aPosition
		ifAbsent: aBlockClosure
]

{ #category : 'code generation' }
MethodNode >> parserClass [
	"Which parser produces this class of parse node"

	^ Parser
]

{ #category : 'source mapping' }
MethodNode >> positionsForInstanceVariable: aName ifAbsent: aBlock [

	^encoder positionsForInstanceVariable: aName ifAbsent: aBlock

]

{ #category : 'source mapping' }
MethodNode >> positionsForLiteralNode: aName ifAbsent: aBlock [

	^encoder positionsForLiteralNode: aName ifAbsent: aBlock

]

{ #category : 'source mapping' }
MethodNode >> positionsForLiteralVariableNode: aName ifAbsent: aBlock [

	^encoder positionsForLiteralVariableNode: aName ifAbsent: aBlock
]

{ #category : 'source mapping' }
MethodNode >> positionsForTemporaryVariable: aName ifAbsent: aBlock [

	^encoder positionsForTemporaryVariable: aName ifAbsent: aBlock
]

{ #category : 'source mapping' }
MethodNode >> positionsInLiteralArrayOf: aSymbol [

	| literalArrayPositions |

	literalArrayPositions := encoder positionsOfLiteralArraysContaining: aSymbol.

	^self positionsOf: aSymbol printString containedIn: literalArrayPositions.


]

{ #category : 'source mapping' }
MethodNode >> positionsOf: symbolString containedIn: literalArrayPositions [

	| symbolPositions |

	symbolPositions := OrderedCollection new.

	literalArrayPositions do: [ :literalArrayPosition | self addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition ].

	^symbolPositions





]

{ #category : 'converting' }
MethodNode >> preen [
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.

	 Currently two cases:

		preenLocalIfNotNilArg: blockNode
		hiding the assignment to the arg of an inlined block arg to ifNotNil:,
			(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...].

		preenTempsConflictingWithBlockNode: temps
		hiding the declaration of a temp that is redeclared in some block"

	self preenableNodes ifNotNil:
		[:preenableNodes|
		 preenableNodes keysAndValuesDo:
			[:nodeOrArray :selector |
			 self perform: selector with: nodeOrArray with: preenableNodes]]
]

{ #category : 'converting-private' }
MethodNode >> preenIfNotNilNode: messageNode preenableNodes: preenableNodes [
	"Transform a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| variable relevantNodes |
	self assert: (messageNode isMessageNode
				and: [messageNode macroPrinter == #printIfNilNotNil:indent:
				and: [messageNode receiver receiver isAssignmentNode]]).
	variable := messageNode receiver receiver variable.
	self assert: (variable isTemp and: [variable isRemote not]).
	relevantNodes := preenableNodes keys select:
						[:node| 
						node isMessageNode
						and: [messageNode macroPrinter == #printIfNilNotNil:indent:]].
	(variable isLocalToBlocksInIfNilIfNotNilNodes: relevantNodes in: self) ifFalse:
		[messageNode arguments do:
			[:argBlock|
			(argBlock arguments size = 1
			and: [argBlock firstArgument = variable]) ifTrue:
				[argBlock arguments: #()]].
		 variable beTemp.
		 ^self].
	messageNode arguments last arguments isEmpty
		ifTrue: [messageNode arguments last arguments: { variable }]
		ifFalse:
			[self assert: messageNode arguments last arguments asArray = { variable }.
			 variable := nil].
	messageNode receiver receiver: messageNode receiver receiver value.
	(temporaries includes: variable) ifTrue: "can't use removeAtIndex: cuz temporaries could be an Array"
		[temporaries := temporaries copyWithout: variable].
	variable ifNil: [^self].
	self nodesDo:
		[:node|
		((node == self or: [node isBlockNode])
		 and: [node temporaries anySatisfy: [:temp| temp = variable]]) ifTrue:
			[node temporaries: (node temporaries reject: [:temp| temp = variable])]]
]

{ #category : 'converting-private' }
MethodNode >> preenNilNodeFollowingNilIfNode: aNilIfMessageNode preenableNodes: preenableNodes [
	self nodesDo:
		[:node| | statements indices |
		 (node isBlockNode
		  and: [(statements := node statements) includes: aNilIfMessageNode]) ifTrue:
			[indices := (2 to: statements size) reject:
							[:i|
							(statements at: i) == NodeNil
							and: [(statements at: i - 1) isNilIf]].
			 node statements: (({1}, indices) collect: [:i| statements at: i])]]
]

{ #category : 'converting-private' }
MethodNode >> preenTempsConflictingWithBlockNode: temps preenableNodes: preenableNodes [
	"Push temps that conflict with other blocks down into their narrowest enclosing block scope."
	temps do:
		[:tempVar|
		(self accept: (NarrowerVariableScopeFinder new ofVariable: tempVar)) ifNotNil:
			[:enclosingScope |
			 self assert: enclosingScope isBlockNode.
			 self nodesDo:
				[:node|
				 ((node == self or: [node isBlockNode])
				  and: [node temporaries includes: tempVar]) ifTrue:
					[node temporaries: (node temporaries copyWithout: tempVar)]].
			 enclosingScope temporaries: enclosingScope temporaries, { tempVar }]]
]

{ #category : 'converting-private' }
MethodNode >> preenableNodes [
	"Answer a Dictionary from node or sequence of nodes to preen method selector for nodes in
	 the tree that require post-processing after either a format or a decompile.  Such issues are
	 the variable for an ifNotNil: which is local to the ifNotNil: block but, due to the inlining of
	 ifNotNil: appears to be declared at the outer level, and, similarly, a temporary variable that
	 conflicts with one of the same name in a block when, were the variable declared local to
	 some inlined block it would no longer conflict.  The resulting dictionary is used to perform
	 the value with the key (node or array) and the dictionary as arguments to preen the tree."

	| preenableNodes priorBlocks priorVariables |
	preenableNodes := Dictionary new.
	priorBlocks := OrderedCollection new.
	priorVariables := Set new.
	self nodesWithPrecedingStatementsDo:
		[:node :precedingStatementOrNil| | variable temps |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not]]]]]]) ifTrue:
			[preenableNodes at: node put: #preenIfNotNilNode:preenableNodes:.
			 priorVariables add: variable].
		node isBlockNode ifTrue:
			[temps := OrderedCollection new.
			 node temporaries do:
				[:temp|
				 priorBlocks do:
					[:aBlock|
					 aBlock temporaries do:
						[:priorTemp|
						 (priorVariables includes: priorTemp) ifFalse:
							[priorTemp key = temp key ifTrue:
								[temps addLast: priorTemp]]]]].
			 temps isEmpty ifFalse:
				[preenableNodes at: temps put: #preenTempsConflictingWithBlockNode:preenableNodes:].
			 priorBlocks addLast: node].
		(node == NodeNil
		 and: [precedingStatementOrNil notNil
		 and: [precedingStatementOrNil isMessageNode
		 and: [precedingStatementOrNil isNilIf]]]) ifTrue:
			[preenableNodes at: precedingStatementOrNil put: #preenNilNodeFollowingNilIfNode:preenableNodes:]].
	^preenableNodes
]

{ #category : 'accessing' }
MethodNode >> primitiveErrorVariableName [
	"Answer the primitive error code temp name, or nil if none."
	(primitive isInteger and: [primitive > 0]) ifTrue:
		[properties pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil

	"(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: Parser) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' error: foo module: ''bar''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' module: ''bar'' error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate"
]

{ #category : 'printing' }
MethodNode >> printCommentOn: aStream [
	
	comment ifNotNil: [
		aStream newLineTab: 1.
		self printCommentOn: aStream indent: 1].
]

{ #category : 'printing' }
MethodNode >> printOn: aStream [

	self 
		printSelectorAndArgumentsOn: aStream;
		printCommentOn: aStream;
		printTemporariesOn: aStream;
		ifPrimitivePrintOn: aStream;
		printPropertiesOn: aStream;
		printPragmasOn: aStream.
		
	aStream newLineTab: 1.
	block printStatementsOn: aStream indent: 0
]

{ #category : 'printing' }
MethodNode >> printPragmasOn: aStream [
	properties ifNil: [^self].
	properties pragmas do: [ :pragma |
		"Primitives are printed in printPrimitiveOn:; skip these"
		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream newLineTab: 1.
			 pragma printOn: aStream]]
]

{ #category : 'printing' }
MethodNode >> printPrimitiveOn: aStream [
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asPlainString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asPlainString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', ((interpreterClass primitiveTable) at: primitive + 1), '" ']
]

{ #category : 'printing' }
MethodNode >> printPropertiesOn: aStream [
	properties ifNil: [^self].
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream newLine; tab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]
]

{ #category : 'printing' }
MethodNode >> printSelectorAndArgumentsOn: aStream [

	| selectorNode |
	
	selectorNode := self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords withIndexDo:
				[:kwd :i | | arg |
				arg := arguments at: i.
				i = 1 ifFalse: [ aStream space ].
				aStream nextPutAll: kwd; space; nextPutAll: arg key ]].

]

{ #category : 'printing' }
MethodNode >> printTemporariesOn: aStream [
	
	block printTemporaries: temporaries on: aStream doPrior: [aStream newLineTab: 1].

]

{ #category : 'printing' }
MethodNode >> printWithClosureAnalysisOn: aStream  [
	self ensureClosureAnalysisDone.
	precedence = 1
		ifTrue: 
			[(self selector includesSubString: '()/')
				ifTrue: [aStream nextPutAll: (self selector copyUpTo: $)).
						arguments
							do: [:arg| aStream nextPutAll: arg key]
							separatedBy: [aStream nextPutAll: ', '].
						aStream nextPut: $)]
				ifFalse: [aStream nextPutAll: self selector]]  "no node for method selector"
		ifFalse: 
			[self selector keywords with: arguments do: 
				[:kwd :arg | 
				aStream nextPutAll: kwd; space.
				arg printDefinitionForClosureAnalysisOn: aStream.
				aStream space]].
	comment == nil ifFalse: [
			aStream newLineTab: 1.
			 self printCommentOn: aStream indent: 1].
	temporaries size > 0 ifTrue: [
			aStream newLineTab: 1; nextPut: $|.
			temporaries do: [:temp | 
				aStream space.
				temp printDefinitionForClosureAnalysisOn: aStream].
			aStream space; nextPut: $|].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream newLineTab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream newLineTab: 1.
	block printWithClosureAnalysisStatementsOn: aStream indent: 0
]

{ #category : 'code generation' }
MethodNode >> properties [
	^properties
]

{ #category : 'source mapping' }
MethodNode >> rangeForNode: node ifAbsent: aBlock [

	^encoder rangeForNode: node ifAbsent: aBlock
]

{ #category : 'source mapping' }
MethodNode >> rawSourceRanges [

	^self rawSourceRangesAndMethodDo: [:rawSourceRanges :method| rawSourceRanges]
]

{ #category : 'source mapping' }
MethodNode >> rawSourceRangesAndMethodDo: aBinaryBlock [
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := encoder classEncoding parserClass new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText asPlainString])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method
]

{ #category : 'code generation (closures)' }
MethodNode >> referencedValuesWithinBlockExtent: anInterval  [
	^(localsPool select:
		[:temp|
		 temp isReferencedWithinBlockExtent: anInterval]) collect:
			[:temp|
			temp isRemote ifTrue: [temp remoteNode] ifFalse: [temp]]
]

{ #category : 'testing' }
MethodNode >> referencesSelf [

	^block referencesSelf
]

{ #category : 'testing' }
MethodNode >> referencesSuper [

	^block referencesSuper
]

{ #category : 'primitive error codes' }
MethodNode >> removeAndRenameLastTempIfErrorCode [
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 temporaries last
			name: primitiveErrorVariableName
			key: primitiveErrorVariableName
			code: temporaries last code.
		 temporaries removeLast].
]

{ #category : 'accessing' }
MethodNode >> removeProperty: aSymbol [
	properties := properties copyWithout: (Association
											key: aSymbol
											value: (properties propertyValueAt: aSymbol))
]

{ #category : 'code generation' }
MethodNode >> selector  [
	"Answer the message selector for the method represented by the receiver."

	(selectorOrFalse isSymbol)
		ifTrue: [^selectorOrFalse].
	^selectorOrFalse key.

]

{ #category : 'initialization' }
MethodNode >> selector: symbol [

	selectorOrFalse := symbol
]

{ #category : 'initialization' }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim  [
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	self 
		selector: selOrFalse
		arguments: args
		precedence: p
		temporaries: temps
		block: blk
		encoder: anEncoder 
		primitive: prim 
		properties: AdditionalMethodState new.
]

{ #category : 'initialization' }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict [
	"Initialize the receiver with respect to the arguments given."
	"RNG: its external use is deprecated, in favor of any of the methods that receives a temporariesDeclaration instead of the collection of temporaries"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.
]

{ #category : 'initialization' }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range [
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	selectorKeywordsRanges := range.
	
	^self selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
]

{ #category : 'initialization' }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict [
	"Initialize the receiver with respect to the arguments given."
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporariesDeclaration := tempsDeclaration.
	temporaries := tempsDeclaration allDeclaredVariableNodes.
	block := blk.
	primitive := prim.
	properties := propDict.
]

{ #category : 'initialization' }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range [
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	selectorKeywordsRanges := range.

	^ self selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict
]

{ #category : 'printing' }
MethodNode >> selectorAndArgumentsAsString [
	
	^String streamContents: [ :aStream | self printSelectorAndArgumentsOn: aStream ]
]

{ #category : 'source mapping' }
MethodNode >> selectorKeywordPositionAt: anIndex [

	^selectorKeywordsRanges at: anIndex
]

{ #category : 'source mapping' }
MethodNode >> selectorKeywordsPositions [

	^selectorKeywordsRanges
]

{ #category : 'source mapping' }
MethodNode >> selectorLastPosition [

	"If #DoIt selector, returns 0 - Hernan"

	^self selector isUnary
		ifTrue: [ selectorKeywordsRanges ifNil: [ 0 ] ifNotNil: [ selectorKeywordsRanges last last ]]
		ifFalse: [
			(encoder 
				rangeForNode: arguments last
				ifAbsent: [ self shouldNotHappenBecause: 'arguments are part of the encoder'  ]) first last ]
]

{ #category : 'code generation' }
MethodNode >> selectorNode [
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: SelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [SelectorNode new key: selectorOrFalse]
]

{ #category : 'testing' }
MethodNode >> sendsMessageToSelf: aSelector [

	^ self block sendsMessageToSelf: aSelector
]

{ #category : 'source mapping' }
MethodNode >> singleCompleteSourceRangeOf: requestedParseNode [
	"Returns the source range associated with the requested parse node.
	Fails if there is no source range, or if there are multiple source ranges."

	^self
		singleCompleteSourceRangeOf: requestedParseNode
		ifPresent: [ :sourceRange | sourceRange ]
		ifAbsent: [ self error: 'could not find source range for node: ' , requestedParseNode printString ]
]

{ #category : 'source mapping' }
MethodNode >> singleCompleteSourceRangeOf: requestedParseNode ifPresent: sourceRangePresentBlock ifAbsent: sourceRangeAbsentBlock [
	"Finds the source range associated with the requested parse node.
	If it is present, evaluates sourceRangePresentBlock with the result.
	Otherwise, it evaluates sourceRangeAbsentBlock.
	Raises an error if the requested parse node has multiple source ranges"
	| sourceRanges |

	sourceRanges := self
		completeSourceRangesOf: requestedParseNode
		ifAbsent: [^sourceRangeAbsentBlock value].

	^sourceRanges size > 1
		ifTrue: [self error: 'there are multiple source ranges for the parse node: ' , requestedParseNode printString ]
		ifFalse: [sourceRangePresentBlock value: sourceRanges anyOne].
]

{ #category : 'source mapping' }
MethodNode >> sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep [
	
	| finder |
	
	finder := EquivalentNodesFinder for: nodesToLookFor in: self keepingSelection: aSelectionToKeep.
	self accept: finder.

	^finder equivalentNodesRanges
]

{ #category : 'printing' }
MethodNode >> sourceText [

	^ sourceText ifNil: [self printString]
]

{ #category : 'initialization' }
MethodNode >> sourceText: stringOrText [

	sourceText := stringOrText
]

{ #category : 'accessing' }
MethodNode >> statements [

	^block statements.
]

{ #category : 'printing' }
MethodNode >> tempNames [
	^encoder tempNames
]

{ #category : 'accessing' }
MethodNode >> tempNodes [

	^encoder tempNodes
]

{ #category : 'accessing' }
MethodNode >> temporaries [
	"For transformations etc, not used in compilation"

	"RNG: implementation can be changed after adopting the use of temporariesDeclaration inst var.
	After that, the implementation for this message can be changed to:
	^ temporariesDeclaration allDeclaredVariableNodes
	
	Or we can analyze the senders and change the way we request the temporaries"
	
	^temporaries
]

{ #category : 'accessing' }
MethodNode >> temporaries: aCollectionOfTemporaries [
	"For transformations etc, not used in compilation"
	"RNG: deprecated in favor of #temporariesDeclaration:"

	temporaries := aCollectionOfTemporaries
]

{ #category : 'accessing' }
MethodNode >> temporariesDeclaration [

	^ temporariesDeclaration
]

{ #category : 'accessing' }
MethodNode >> temporariesDeclaration: aTemporariesDeclarationNode [
	"RNG: after removing all the usages of the temporaries inst var, the last line can be removed"

	temporariesDeclaration := aTemporariesDeclarationNode.
	self temporaries: aTemporariesDeclarationNode allDeclaredVariableNodes
]

{ #category : 'source mapping' }
MethodNode >> withParseNodeIncluding: aPosition do: aBlock ifAbsent: anAbsentBlock [

	| nodeAndPosition |

	nodeAndPosition :=self parseNodeIncluding: aPosition ifAbsent: [ ^ anAbsentBlock value ].
	^aBlock value: nodeAndPosition key.
]

{ #category : 'converting' }
MethodNode >> withoutImplicitSelfReturn [
	
	block removeImplicitSelfReturnWhenIn: self.
	^self
]
