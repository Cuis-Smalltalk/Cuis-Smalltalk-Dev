"
I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.
"
Class {
	#name : 'Encoder',
	#superclass : 'ParseNode',
	#instVars : [
		'scopeTable',
		'nTemps',
		'supered',
		'requestor',
		'class',
		'selector',
		'literalStream',
		'selectorSet',
		'litIndSet',
		'litSet',
		'sourceRanges',
		'globalSourceRanges',
		'addedSelectorAndMethodClassLiterals',
		'optimizedSelectors',
		'cue',
		'completeSourceRanges',
		'bindingsInBacktickLiterals'
	],
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'enconding' }
Encoder class >> doItInContextName [

	^'originalContext'
]

{ #category : 'visiting' }
Encoder >> accept: aVisitor [
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
]

{ #category : 'source mapping' }
Encoder >> addMultiRange: aRange for: aNode [

	| ranges |
	
	"I'm using an OrderedCollection because ranges are added in order, while parsing the source code.
	If this constrain is not hold, a SortedCollection should be used - Hernan"
	ranges := sourceRanges at: aNode ifAbsentPut: [ OrderedCollection new ].
	ranges add: aRange asSourceCodeInterval.
	
	^aNode 
]

{ #category : 'results' }
Encoder >> allLiterals [
	| newLiterals |
	addedSelectorAndMethodClassLiterals ifFalse: [
		addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors and backtick introduced bindings in literals so they are shown in senders and references."
		optimizedSelectors := optimizedSelectors reject: [ :e | literalStream originalContents hasLiteral: e ].
		bindingsInBacktickLiterals := bindingsInBacktickLiterals reject: [ :e | literalStream originalContents hasLiteral: e ].
		newLiterals := optimizedSelectors asArray sort, bindingsInBacktickLiterals asArray.
		newLiterals isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + newLiterals size + 2 < self maxNumLiterals
				ifTrue: [ newLiterals do: [ :e | self litIndex: e ]]
				ifFalse: [ self litIndex: newLiterals ]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass ].
	^literalStream contents
]

{ #category : 'encoding' }
Encoder >> asciiStringLiteral: literalObject [
	"Convert UnicodeStrings to Strings if ASCII.
	Recursive for Arrays"
	
	literalObject class == Array ifTrue: [
		^literalObject collect: [ :each | self asciiStringLiteral: each ]].

	"Prefer String over UnicodeString for literals, if ASCII only."
	(literalObject class == UnicodeString and: [ literalObject isAscii ]) ifTrue: [
		^ literalObject asByteString ].

	^literalObject
]

{ #category : 'results' }
Encoder >> associationForClass [
	| assoc |
	assoc := Smalltalk associationAt: class name ifAbsent: nil.
	^assoc value == class
		ifTrue: [ assoc ]
		ifFalse: [ Association new value: class ]
]

{ #category : 'temps' }
Encoder >> autoBind: name  [
	"Declare a block argument as a temp if not already declared."
	| node |
	node := scopeTable 
			at: name
			ifAbsent: 
				[(self lookupInPools: name ifFound: [:assoc | assoc])
					ifTrue: [self warnAboutShadowed: name evenIfDuringFileIn: false].
				^ (self reallyBind: name) nowHasDef nowHasRef scope: 1].
	node isTemp
		ifTrue: [node scope >= 0 ifTrue:
					[^self warnAboutShadowed: node evenIfDuringFileIn: true].
				node nowHasDef nowHasRef scope: 1]
		ifFalse: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
	^node
]

{ #category : 'temps' }
Encoder >> bindAndJuggle: name [

	| node nodes first thisCode |
	node := self reallyBind: name.

	"Declared temps must precede block temps for decompiler and debugger to work right"
	nodes := self tempNodes.
	(first := nodes findFirst: [:n | n scope > 0]) > 0 ifTrue:
		[node == nodes last ifFalse: [self error: 'logic error'].
		thisCode := (nodes at: first) code.
		first to: nodes size - 1 do:
			[:i | (nodes at: i) key: (nodes at: i) key
							code: (nodes at: i+1) code].
		nodes last key: nodes last key code: thisCode].
	
	^ node
]

{ #category : 'temps' }
Encoder >> bindArg: name  [
	"Declare an argument."
	| node |
	nTemps >= 15
		ifTrue: [^self notify: 'Too many arguments'].
	node := self bindTemp: name.
	^ node nowHasDef nowHasRef
]

{ #category : 'temps' }
Encoder >> bindArg: aName range: aRange [
 
	^ self addMultiRange: aRange for: (self bindArg: aName)

]

{ #category : 'temps' }
Encoder >> bindBlockArg: name within: aBlockNode [
	"With standard Smalltalk-80 (BlueBook) blocks it used to be legal to use a
	 method temp as a block argument.  This shouldn't be the case with the
	 current compiler, which checks for temp names already being used as
	 block arguments.  But it is easily fooled by local block temps in optimized
	 blocks, e.g.
		false
			ifTrue: [| temp |]
			ifFalse:[[:temp|]]
	Rather than fix this we keep the semantics and fix it in the closure compiler."
	^self autoBind: name
]

{ #category : 'temps' }
Encoder >> bindBlockArg: name within: aBlockNode range: range [
	
	^self addMultiRange: range for: (self bindBlockArg: name within: aBlockNode)
	

]

{ #category : 'temps' }
Encoder >> bindBlockTemp: name  [
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |
	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
			node scope: 0]
		ifFalse: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
	^node

]

{ #category : 'temps' }
Encoder >> bindBlockTemp: name within: aBlockNode [
	"The BlockContext compiler (the Smalltalk-80 BlueBook compiler)
	 does provide support for ANSI block syntax, but not for ANSI block
	 semantics.  Here all temps live at the same level, the method level.
	 The approach taken to two block-local temps in different blocks is to
	 merge them into a single temp.  e.g.
		expr
			ifTrue: [|temp| self statementOne]
			ifFalse: [|temp| self statementTwo]
	 is effectvely transformed into
		| temp |
		expr
			ifTrue: [self statementOne]
			ifFalse: [self statementTwo]
	 and
		expr do: [:each| | temp | ...].
		expr do: [:each| | temp | ...].
	 is also effectively transformed into
		| temp |
		expr do: [:each|  ...].
		expr do: [:each| ...].

	 The closure compiler treats the former similarly, but not the latter.
	 The indirection through #bindBlockTemp:within: allows the closure encoder to do this."
	^self bindBlockTemp: name
]

{ #category : 'temps' }
Encoder >> bindBlockTemp: name within: aBlockNode range: range [

	^self addMultiRange: range for: (self bindBlockTemp: name within: aBlockNode)
	

]

{ #category : 'temps' }
Encoder >> bindTemp: name  [
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		node isTemp
			ifTrue:[^self warnAboutShadowed: node evenIfDuringFileIn: true]
			ifFalse:[self warnAboutShadowed: name evenIfDuringFileIn: false]].
	^self reallyBind: name
]

{ #category : 'temps' }
Encoder >> bindTemp: aName range: aRange [

	^ self addMultiRange: aRange for: (self bindTemp: aName)
]

{ #category : 'private' }
Encoder >> bindUndeclaredTemp: name [
	^scopeTable at: name put: (self newUndeclaredTemp: name)
]

{ #category : 'encoding' }
Encoder >> cantStoreInto: varName [
	^ StdVariables includesKey: varName
]

{ #category : 'private' }
Encoder >> classEncoding [
	"This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view."
	^ class
]

{ #category : 'source mapping' }
Encoder >> completeSourceRangesBasedOn: sourceCode [

	completeSourceRanges ifNil: [ self initializeCompleteSourceRangesBasedOn: sourceCode ].
	^completeSourceRanges 
]

{ #category : 'source mapping' }
Encoder >> criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo [

	| sourceRangeOne sourceRangeTwo |
	sourceRangeOne := parseNodeWithSourceRangeOne value.
	sourceRangeTwo := parseNodeWithSourceRangeTwo value.
	^ sourceRangeOne first > sourceRangeTwo first
		or: [ sourceRangeOne first = sourceRangeTwo first
			and: [ sourceRangeOne last <= sourceRangeTwo last ] ]
]

{ #category : 'encoding' }
Encoder >> encodeLiteral: literalObject [

	| object |
	object := self asciiStringLiteral: literalObject.
	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet.
]

{ #category : 'encoding' }
Encoder >> encodeLiteral: object range: aRange [

	^ self addMultiRange: aRange for: (self encodeLiteral: object)
]

{ #category : 'encoding' }
Encoder >> encodeSelector: aSelector [
	"Don't call it 'selector'. It would be shadowed."
	^self
		name: aSelector
		key: aSelector
		class: SelectorNode
		type: SendType
		set: selectorSet
]

{ #category : 'encoding' }
Encoder >> encodeVariable: name [
	^ self encodeVariable: name sourceRange: nil ifUnknown: [ self undeclared: name ]
]

{ #category : 'encoding' }
Encoder >> encodeVariable: name ifUnknown: action [
	^self encodeVariable: name sourceRange: nil ifUnknown: action
]

{ #category : 'encoding' }
Encoder >> encodeVariable: name sourceRange: range ifUnknown: action [
	| varNode |
	varNode := scopeTable
					at: name
					ifAbsent: 
						[(self lookupInPools: name 
							ifFound: [:assoc | varNode := self global: assoc name: name])
							ifTrue: [varNode]
							ifFalse: [^action value]].
	range ifNotNil:
		[name first canBeGlobalVarInitial ifTrue:
			[globalSourceRanges addLast: { name. range. false }]].

	(varNode isTemp and: [varNode scope < 0]) ifTrue:
		[^OutOfScopeNotification signal
			ifTrue: [action value]
			ifFalse: [self notify: 'out of scope']].
	^varNode
]

{ #category : 'initialization' }
Encoder >> fillDict: dict with: nodeClass mapping: keys to: codeArray [
	| codeStream |
	codeStream := ReadStream on: codeArray.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]
]

{ #category : 'temps' }
Encoder >> fixTemp: name [
	| node |
	node := scopeTable at: name ifAbsent: nil.
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node
]

{ #category : 'temps' }
Encoder >> floatTemp: node [
	(node ~~ (scopeTable at: node name ifAbsent: nil)
	or: [node class ~~ TempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1
]

{ #category : 'private' }
Encoder >> global: ref name: name [

	^self
		name: name
		key: ref
		class: LiteralVariableNode
		type: LdLitIndType
		set: litIndSet
]

{ #category : 'source mapping' }
Encoder >> globalSourceRanges [

	^ globalSourceRanges
]

{ #category : 'initialization' }
Encoder >> init: aClass context: aContext notifying: req [
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: Encoder doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= Encoder doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new.
	globalSourceRanges := OrderedCollection new.
]

{ #category : 'initialization' }
Encoder >> initScopeAndLiteralTables [

	StdVariables ifNil: [VariableNode initClassCachedState].
	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new.
	bindingsInBacktickLiterals := Set new.
]

{ #category : 'source mapping' }
Encoder >> initializeCompleteSourceRangesBasedOn: sourceCode [

	completeSourceRanges := Dictionary new.
	sourceRanges keysAndValuesDo: [ :parseNode :nodeRanges |
		"leaf nodes excluded because they have the same complete source ranges than the block nodes they wrap - Nahuel"
		parseNode class = LeafNode ifFalse: [
			| expandedNodeSourceRanges |
			expandedNodeSourceRanges := parseNode expandRanges: nodeRanges basedOn: sourceRanges using: sourceCode.
			completeSourceRanges at: parseNode put: expandedNodeSourceRanges ] ].

]

{ #category : 'private' }
Encoder >> interactive [
	"Answer true if compilation is interactive"

	^requestor interactive
]

{ #category : 'encoding' }
Encoder >> litIndex: literal [
	| p |
	p := literalStream position.
	p = self maxNumLiterals ifTrue:
		[self notify: 'More than ', self maxNumLiterals printString, ' literals referenced. 
You must split or otherwise simplify this method.
The ', (self maxNumLiterals + 1) printString, 'th literal is: ', literal printString. ^nil].
	"Would like to show where it is in the source code, 
	 but that info is hard to get."
	literalStream nextPut: literal.
	^ p
]

{ #category : 'results' }
Encoder >> literals [
	"Should only be used for decompiling primitives"
	^ literalStream contents
]

{ #category : 'private' }
Encoder >> lookupInPools: varName ifFound: assocBlock [

	^Symbol
		hasInterned: varName
		ifTrue:
			[:sym|
			(class bindingOf: sym)
				ifNil: [^false]
				ifNotNil: [:assoc| assocBlock value: assoc]]
]

{ #category : 'encoding' }
Encoder >> lookupVariable: name ifAbsent: aBlock [
	"Answer the binding of name in the scope table or aBlock's vaue if none.
	 Do not bind and do not lookup in pools.  Used for correction, explanation etc"
	^scopeTable at: name ifAbsent: aBlock
]

{ #category : 'accessing' }
Encoder >> maxIndexableLiterals [
	"Answer the maximum number of literals supported by the receiver's
	 bytecode set. This is a nominal value based on the Blue Book bytecode
	 set; subclasses answer a more accurate value."
	^63
]

{ #category : 'accessing' }
Encoder >> maxNumLiterals [
	^CompiledMethod maxNumLiterals min: self maxIndexableLiterals
]

{ #category : 'temporaries' }
Encoder >> maxTemp [
	^nTemps
]

{ #category : 'source mapping' }
Encoder >> messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aBlock [

	| positions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordAndParameterPositionAt: anIndex encodedWith: self ifAbsent: aBlock].

	^ positions isEmpty
		ifTrue: aBlock
		ifFalse: [ positions ]
	
]

{ #category : 'source mapping' }
Encoder >> messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: absentBlock [

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (aMessageSendNode keywordPositionAt: anIndex) first ]
		ifAbsent: absentBlock
	
]

{ #category : 'source mapping' }
Encoder >> messageSendLastPositionsOf: aSelector ifAbsent: absentBlock [

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (sourceRanges at: aMessageSendNode) last ]
		ifAbsent: absentBlock
	
]

{ #category : 'source mapping' }
Encoder >> messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock [

	| ranges sortedRanges |

	ranges := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordRanges ].

	ranges isEmpty ifTrue: [ ^aBlock value ].
	sortedRanges := ranges asSortedCollection: [ :left :right | left first first < right first first ].

	^sortedRanges 
]

{ #category : 'accessing' }
Encoder >> methodNodeClass [
	^MethodNode
]

{ #category : 'private' }
Encoder >> name: name key: key class: leafNodeClass type: type set: dict  [
	^dict at: key
		ifAbsentPut: 
			[leafNodeClass new 
				name: name
				key: key
				index: nil
				type: type]
]

{ #category : 'temps' }
Encoder >> newTemp: name [

	nTemps := nTemps + 1.
	^ TempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0
]

{ #category : 'temps' }
Encoder >> newUndeclaredTemp: name [
	^UndeclaredVariableNode new name: name
]

{ #category : 'encoding' }
Encoder >> noteBactickLiteralBinding: aVariableBinding [
	"Register a aVariableBinding.
	These will later be registered into the literals so that tools can easily browse references."
	bindingsInBacktickLiterals add: aVariableBinding
]

{ #category : 'encoding' }
Encoder >> noteOptimizedSelector: aSymbol [
	"Register a selector as being optimized.
	These optimized selectors will later be registered into the literals so that tools can easily browse senders."
	optimizedSelectors add: aSymbol
]

{ #category : 'source mapping' }
Encoder >> noteSourceRange: range forNode: node [

	sourceRanges at: node put: range asSourceCodeInterval
]

{ #category : 'initialize-release' }
Encoder >> noteSuper [
	supered := true
]

{ #category : 'error handling' }
Encoder >> notify: string  [
	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false
]

{ #category : 'error handling' }
Encoder >> notify: string at: location [

	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string at: location].
	^false
]

{ #category : 'source mapping' }
Encoder >> parameterDefinitionPositionFor: aParameterNode [

	^ (self rangeForNode: aParameterNode ifAbsent: [ self error: 'invalid parameter node' ]) first
]

{ #category : 'source mapping' }
Encoder >> parseNodeIncluding: aPosition ifAbsent: aBlockClosure [

	| smallestRangeSize nodeWithRangeAtPosition |

	smallestRangeSize := SmallInteger maxVal.
	nodeWithRangeAtPosition := nil.

	sourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange | | currentNodeRangeSize |
				currentNodeRangeSize := aRange size.
				currentNodeRangeSize < smallestRangeSize ifTrue: [
					smallestRangeSize := currentNodeRangeSize.
					nodeWithRangeAtPosition := nodeAtRange key -> aRange ]]].

	^ nodeWithRangeAtPosition ifNil: aBlockClosure ifNotNil: [ nodeWithRangeAtPosition ]
			
]

{ #category : 'source mapping' }
Encoder >> parseNodesPathAt: aPosition ifAbsent: aBlockClosure [

	| nodesWithRangeAtPosition |
	
	nodesWithRangeAtPosition := SortedCollection sortBlock: [ :parseNodeWithSourceRangeOne :parseNodeWithSourceRangeTwo |
		self criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo ].

	completeSourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange |
			nodesWithRangeAtPosition add: (nodeAtRange key -> aRange) ] ].

	^ nodesWithRangeAtPosition
		ifEmpty: aBlockClosure
		ifNotEmpty: [ nodesWithRangeAtPosition ]
]

{ #category : 'source mapping' }
Encoder >> positionsForInstanceVariable: aName ifAbsent: aBlock [

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isInstanceVariableNode ]
		ifAbsent: aBlock 
]

{ #category : 'source mapping' }
Encoder >> positionsForLiteralNode: aName ifAbsent: aBlock [

	| variableNode |

	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock
]

{ #category : 'source mapping' }
Encoder >> positionsForLiteralVariableNode: aName ifAbsent: aBlock [

	| variableNode |

	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock
]

{ #category : 'source mapping' }
Encoder >> positionsForTemporaryVariable: aName ifAbsent: aBlock [

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isTemp ]
		ifAbsent: aBlock 
]

{ #category : 'source mapping' }
Encoder >> positionsOfLiteralArraysContaining: aSymbol [

	| positions |

	positions := OrderedCollection new.
	litSet keysAndValuesDo: [ :aLiteral :aLiteralNode |
		(aLiteral isArray and: [ aLiteral hasLiteral: aSymbol ]) ifTrue: [ positions addAll: (sourceRanges at: aLiteralNode) ]].

	^positions 
]

{ #category : 'private' }
Encoder >> possibleNamesFor: proposedName [
	| results |
	results := class possibleVariablesFor: proposedName continuedFrom: nil.
	^ proposedName correctAgainst: nil continuedFrom: results.

]

{ #category : 'private' }
Encoder >> possibleVariablesFor: proposedVariable [

	| results |
	results := proposedVariable correctAgainstDictionary: scopeTable
								continuedFrom: nil.
	proposedVariable first canBeGlobalVarInitial ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results.

]

{ #category : 'source mapping' }
Encoder >> rangeForNode: node ifAbsent: aBlock [

	^sourceRanges at: node ifAbsent: aBlock
]

{ #category : 'source mapping' }
Encoder >> rangesForLiteralNode: aName ifAbsent: aBlock [

	| variableNode |
	
	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock
]

{ #category : 'source mapping' }
Encoder >> rangesForLiteralVariableNode: aName ifAbsent: aBlock [

	| variableNode |
	
	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock
]

{ #category : 'source mapping' }
Encoder >> rangesForVariable: aName checkingType: nodeTypeCheckBlock ifAbsent: anAbsentBlock [

	| variableNode |
	
	variableNode := scopeTable at: aName ifAbsent: [ ^anAbsentBlock value ].
	(nodeTypeCheckBlock value: variableNode) ifFalse: [ ^anAbsentBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: anAbsentBlock 
]

{ #category : 'source mapping' }
Encoder >> rawSourceRanges [

	^ sourceRanges 
]

{ #category : 'private' }
Encoder >> reallyBind: name [

	| node |
	node := self newTemp: name.
	scopeTable at: name put: node.
	^node
]

{ #category : 'initialize-release' }
Encoder >> release [
	requestor := nil
]

{ #category : 'error handling' }
Encoder >> requestor: aRequestor [

	requestor := aRequestor
]

{ #category : 'accessing' }
Encoder >> selector [
	^selector
]

{ #category : 'accessing' }
Encoder >> selector: aSymbol [
	selector := aSymbol
]

{ #category : 'encoding' }
Encoder >> sharableLitIndex: literal [
	"Special access prevents multiple entries for post-allocated super send special selectors"
	1 to: literalStream position do:
		[:index|
		(literal literalEqual: (literalStream originalContents at: index)) ifTrue:
			[^index - 1]].
	^self litIndex: literal
]

{ #category : 'source mapping' }
Encoder >> sourceMap [
	"Answer with a sorted set of associations (pc range)."

	^ (sourceRanges keys collect: 
		[:key |  Association key: key pc value: (sourceRanges at: key)])
			asSortedCollection
]

{ #category : 'results' }
Encoder >> tempNames  [

	^ self tempNodes collect:
		[:node | (node isMemberOf: MessageAsTempNode)
					ifTrue: [scopeTable keyAtValue: node]
					ifFalse: [node key]]
]

{ #category : 'results' }
Encoder >> tempNodes  [
	| tempNodes |
	tempNodes := SortedCollection sortBlock: [:n1 :n2 | n1 code <= n2 code].
	scopeTable associationsDo:
		[:assn |
		(assn value is: #Array)
			ifTrue: [assn value do: [:temp| tempNodes add: temp]]
			ifFalse: [assn value isTemp ifTrue: [tempNodes add: assn value]]].
	^tempNodes
	"jmv: perhaps better not include remote temps...
	^ tempNodes reject:
		[ : a |  a isUnusedTemp ]"
]

{ #category : 'initialization' }
Encoder >> temps: tempVars literals: lits class: cl  [
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	literalStream := ReadStream on: lits.
	literalStream position: lits size.
	sourceRanges := Dictionary new.
	globalSourceRanges := OrderedCollection new.

]

{ #category : 'results' }
Encoder >> tempsAndBlockArgs [
	| tempNodes |
	tempNodes := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | var |
		var := assn value.
		(var isTemp
		 and: [var isMethodArg not
		 and: [var scope = 0 or: [var scope = -1]]]) ifTrue:
			[tempNodes add: var]].
	^tempNodes
]

{ #category : 'encoding' }
Encoder >> undeclared: name [
	| sym |
	requestor interactive ifTrue: [
		 ^self notify: 'Undeclared'].
	"Allow knowlegeable clients to squash the undeclared warning if they want (e.g.
	 Diffing pretty printers that are simply formatting text).  As this breaks
	 compilation it should only be used by clients that want to discard the result
	 of the compilation.  To squash the warning use e.g.
		[Compiler format: code in: class notifying: nil decorated: false]
			on: UndeclaredVariableWarning
			do: [:ex| ex resume: false]"
	sym := name asSymbol.
	^(UndeclaredVariableWarning new name: name selector: selector class: class) signal
		ifTrue:
			[Undeclared at: sym put: nil.
			self global: (Undeclared associationAt: sym) name: sym]
		ifFalse:
			[self global: (Association key: sym) name: sym]
]

{ #category : 'results' }
Encoder >> undeclaredTemps  [
	^(scopeTable select: [:var | var isVariableNode and: [var isUndeclared]]) values
]

{ #category : 'results' }
Encoder >> unusedTempNames [
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= Encoder doItInContextName ifTrue: [unused add: name]]].
	^ unused
]

{ #category : 'private' }
Encoder >> warnAboutShadowed: aVariableNode evenIfDuringFileIn: aBoolean [

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
							(RecompilationFailure class: class messageText: msg) signal ]]].
]

{ #category : 'source mapping' }
Encoder >> withMessageSendKeywordPositionsOf: aSelector do: aMessageSendNodeBlock ifAbsent: aBlock [

	| positions sortedPositions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: aMessageSendNodeBlock.

	positions isEmpty ifTrue: [ ^aBlock value ].
	sortedPositions := positions asSortedCollection.

	^sortedPositions 
]

{ #category : 'source mapping' }
Encoder >> withRangesOf: nodeAtRange including: aPosition do: aBlock [

	| currentNodeRange ranges |

	currentNodeRange := nodeAtRange value.
	ranges := currentNodeRange isInterval ifTrue: [ Array with: currentNodeRange ] ifFalse: [ currentNodeRange ].

	ranges do: [ :aRange | (aRange includes: aPosition) ifTrue: [ aBlock value: aRange ]].

	
]
