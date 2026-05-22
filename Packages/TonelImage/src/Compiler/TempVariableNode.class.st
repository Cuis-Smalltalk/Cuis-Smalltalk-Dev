"
I am a parse tree leaf representing a temporary variable
"
Class {
	#name : 'TempVariableNode',
	#superclass : 'VariableNode',
	#instVars : [
		'argType',
		'hasRefs',
		'hasDefs',
		'scope',
		'definingScope',
		'readingScopes',
		'writingScopes',
		'remoteNode'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
TempVariableNode >> accept: aVisitor [
	^aVisitor visitTempVariableNode: self
]

{ #category : 'code generation (closures)' }
TempVariableNode >> addReadWithin: scopeBlock "<BlockNode>" at: location "<Integer>" [
	readingScopes ifNil: [readingScopes := Dictionary new].
	(readingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
]

{ #category : 'code generation (closures)' }
TempVariableNode >> addWriteWithin: scopeBlock "<BlockNode>" at: location "<Integer>" [
	writingScopes ifNil: [writingScopes := Dictionary new].
	(writingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
]

{ #category : 'code generation (closures)' }
TempVariableNode >> analyseClosure: rootNode "<MethodNode>" [
	"Analyse whether the temporary needs to be made remote
	 or not, and answer whether it was made remote.
	 A temp cannot be local if it is written to remotely,
	 or if it is written to after it is closed-over.  An exception
	 is an inlined block argument that appears to be written
	 remotely but is actually local to a block."
	| latestWrite |
	self isBlockArg ifTrue: [^false].
	remoteNode ifNotNil: [^false]. "If already remote, don't remote a second time"
	latestWrite := 0.
	((writingScopes notNil
	 and: [writingScopes associations anySatisfy: [:assoc|
			[:blockScope :refs|
			refs do: [:write| latestWrite := write max: latestWrite].
			"A temp cannot be local if it is written to remotely."
			blockScope actualScope ~~ definingScope actualScope]
				value: assoc key value: assoc value]])
	or: [readingScopes notNil
		and: [readingScopes associations anySatisfy: [:assoc|
				[:blockScope :refs|
				 "A temp cannot be local if it is written to after it is closed-over."
				 blockScope actualScope ~~ definingScope actualScope
				 and: [refs anySatisfy: [:read| read < latestWrite]]]
					value: assoc key value: assoc value]]]) ifTrue:
		[remoteNode := definingScope addRemoteTemp: self rootNode: rootNode.
		 ^true].
	^false
]

{ #category : 'code generation (closures)' }
TempVariableNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>" [
	self addReadWithin: scopeBlock at: rootNode locationCounter
]

{ #category : 'testing' }
TempVariableNode >> assignmentCheck: encoder at: location [
	^((self isBlockArg and: [(Preferences at: #allowBlockArgumentAssignment) not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]
]

{ #category : 'testing' }
TempVariableNode >> beBlockArg [
	argType := #block
]

{ #category : 'testing' }
TempVariableNode >> beMethodArg [
	argType := #method
]

{ #category : 'testing' }
TempVariableNode >> beTemp [
	"This is for preening of ifNil:ifNotNil:"
	argType := nil
]

{ #category : 'code generation (closures)' }
TempVariableNode >> beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>" [
	self addWriteWithin: scopeBlock at: rootNode locationCounter.
	"For analysis of optimized blocks also record the set of temporaries written to
	 within optimized blocks so that additional writes can be added at locations that
	 represent subsequent iterations of the loop. e.g. testInlineBlockCollectionSD1"
	assignmentPools keysAndValuesDo:
		[:outerScopeBlock :set|
		"definingScope can be nil in expr in expr ifNil: [:arg|...] expressions because
		 arg gets its definingScope set when [:arg|...] is analysed."
		outerScopeBlock actualScope
			= (definingScope
				ifNil: [scopeBlock]
				ifNotNil: [definingScope actualScope]) ifTrue:
			[set add: self]]
]

{ #category : 'code generation (closures)' }
TempVariableNode >> definingScope [
	^definingScope
]

{ #category : 'code generation (closures)' }
TempVariableNode >> definingScope: scopeBlock "<BlockNode>" [
	definingScope = scopeBlock ifTrue: [^ self]. "No need to bail"
	definingScope ifNotNil:
		[self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock
]

{ #category : 'printing' }
TempVariableNode >> descriptiveString [
	self isMethodArg
		ifTrue: [ ^'Method Argument named: ', name printString ].
	self isBlockArg
		ifTrue: [ ^'Block Argument named: ', name printString ].
	^'Temporary Variable named: ', name printString
]

{ #category : 'code generation' }
TempVariableNode >> emitCodeForStore: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStoreInto: self stack: stack encoder: encoder].
	encoder genStoreTemp: index
]

{ #category : 'code generation' }
TempVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStorePopInto: self stack: stack encoder: encoder].
	encoder genStorePopTemp: index.
	stack pop: 1
]

{ #category : 'code generation' }
TempVariableNode >> emitCodeForValue: stack encoder: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForValueOf: self stack: stack encoder: encoder].
	encoder genPushTemp: index.
	stack push: 1
]

{ #category : 'code generation (closures)' }
TempVariableNode >> index: anInteger [
	"For renumbering temps in the closure compiler."
	index := anInteger.
	code := self code: index type: LdTempType
]

{ #category : 'testing' }
TempVariableNode >> isArg [
	^argType notNil
]

{ #category : 'testing' }
TempVariableNode >> isBlockArg [
	^#block == argType
]

{ #category : 'testing' }
TempVariableNode >> isDeclaredAtMethodLevel [
	"For the explainer."
	^scope = 0
]

{ #category : 'code generation (closures)' }
TempVariableNode >> isDefinedWithinBlockExtent: anInterval [
	^anInterval rangeIncludes: definingScope actualScope blockExtent first
]

{ #category : 'code generation (closures)' }
TempVariableNode >> isIndirectTempVector [
	^false
]

{ #category : 'testing' }
TempVariableNode >> isLocalToBlocksInIfNilIfNotNilNodes: ifNilIfNotNilMessages "<Collection of: MessageNode>" in: aMethodNode "<MethodNode>" [
	"Answer if the receiver only occurs within the ifNotNil: blocks of the message nodes,
	 or is the target of the assignment that is the receiver of each node.
	 Assumes that all ifNilIfNotNilMessages are in fact subnodes of aMethodNode.

	Strategy:
		First visit all nodes outside of the message nodes. If the variable is referenced there answer false.
		Next visit the expressions of all assignments that are the receivers of the ifNilIfNotNil: messages.
		If the variable is referenced there answer false.
		If not found outside, answer true. This does not require the variable to be referenced within the
		block, but that is not necessary to produce a nicely preened tree."
	| ifNotNilBlocks ifNotNilAssignments finder |
	ifNotNilBlocks := Set new.
	ifNotNilAssignments := Set new.
	ifNilIfNotNilMessages do:
		[:messageNode|
		(messageNode receiver isMessageNode
		 and: [messageNode receiver selector key == #==
		 and: [messageNode receiver receiver isAssignmentNode
		 and: [messageNode receiver receiver variable = self]]]) ifTrue:
			[ifNotNilAssignments add: messageNode receiver receiver].
		ifNotNilBlocks add: (messageNode arguments at: (messageNode selector key keywords indexOf: #ifNotNil:))].
	finder := ParseNodeEnumerator
				ofBlock: [:node| node == self ifTrue: [^false]]
				select: [:node| ((node isBlockNode and: [ifNotNilBlocks includes: node])
								or: [node isAssignmentNode and: [ifNotNilAssignments includes: node]]) not].
	aMethodNode accept: finder.
	ifNotNilAssignments do:
		[:assignmentNode|
		assignmentNode value accept: finder].
	^true
]

{ #category : 'testing' }
TempVariableNode >> isMethodArg [
	^#method == argType
]

{ #category : 'code generation (closures)' }
TempVariableNode >> isReferencedWithinBlockExtent: anInterval  [
	readingScopes ~~ nil ifTrue:
		[readingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	^false
]

{ #category : 'testing' }
TempVariableNode >> isRemote [
	^remoteNode notNil
]

{ #category : 'testing' }
TempVariableNode >> isTemp [
	^ true
]

{ #category : 'testing' }
TempVariableNode >> isTempOrArg [

	^self isTemp or: [ self isArg ]
]

{ #category : 'testing' }
TempVariableNode >> isUndefTemp [
	^ hasDefs not
]

{ #category : 'testing' }
TempVariableNode >> isUnusedTemp [
	^ hasRefs not
]

{ #category : 'initialization' }
TempVariableNode >> name: varName index: i type: type scope: level [
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type
]

{ #category : 'initialization' }
TempVariableNode >> nowHasDef [
	hasDefs := true
]

{ #category : 'initialization' }
TempVariableNode >> nowHasRef [
	hasRefs := true
]

{ #category : 'printing' }
TempVariableNode >> printDefinitionForClosureAnalysisOn: aStream  [
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes ifNotNil: [
		refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asArray sort do: [:read| aStream nextPutAll: ' r@'; print: read]].
	writingScopes ifNotNil: [
		refs := Set new.
		writingScopes do: [:elems| refs addAll: elems].
		refs asArray sort do: [:write| aStream nextPutAll: ' w@'; print: write]].
	aStream nextPut: $}
]

{ #category : 'printing' }
TempVariableNode >> printWithClosureAnalysisOn: aStream indent: level [
	aStream nextPutAll: name.
	readingScopes ifNotNil: [
		(readingScopes
			inject: Set new
			into: [ :them :reads |
				them addAll: reads.
				them ]) asArray sort do: [ :location |
			aStream
				 space;
				 nextPut: $r;
				 nextPut: $@;
				 print: location ]].
	writingScopes ifNotNil: [
		(writingScopes
			inject: Set new
			into: [ :them :writes |
				them addAll: writes.
				them ]) asArray sort do: [ :location |
			aStream
				 space;
				 nextPut: $w;
				 nextPut: $@;
				 print: location ]].
]

{ #category : 'code generation (closures)' }
TempVariableNode >> referenceScopesAndIndicesDo: aBinaryBlock [
	"Evaluate aBinaryBlock with all read or write scopes and locations.
	 This is used to copy the reference information into RemoteTempVectorNodes"
	readingScopes ~~ nil ifTrue:
		[readingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]]
]

{ #category : 'testing' }
TempVariableNode >> remoteNode [
	^remoteNode
]

{ #category : 'decompiler' }
TempVariableNode >> remoteNode: aRemoteTempVectorNode [
	remoteNode := aRemoteTempVectorNode
]

{ #category : 'testing' }
TempVariableNode >> scope [
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	^scope
]

{ #category : 'initialization' }
TempVariableNode >> scope: level [
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level
]

{ #category : 'code generation' }
TempVariableNode >> sizeCodeForStore: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStoreInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStoreTemp: index
]

{ #category : 'code generation' }
TempVariableNode >> sizeCodeForStorePop: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStorePopInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStorePopTemp: index
]

{ #category : 'code generation' }
TempVariableNode >> sizeCodeForValue: encoder [
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForValueOf: self encoder: encoder].
	self reserve: encoder.
	^encoder sizePushTemp: index
]
