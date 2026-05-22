"
I construct the node tree for a Decompiler.
"
Class {
	#name : 'DecompilerConstructor',
	#superclass : 'ParseNode',
	#instVars : [
		'method',
		'instVars',
		'nArgs',
		'literalValues',
		'tempVars'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'visiting' }
DecompilerConstructor >> accept: aVisitor [
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
]

{ #category : 'constructor' }
DecompilerConstructor >> codeAnyLitInd: association [

	^LiteralVariableNode new
		name: association key
		key: association
		index: nil
		type: LdLitIndType
]

{ #category : 'constructor' }
DecompilerConstructor >> codeAnyLiteral: value [
	^LiteralNode new
		key: value
		index: 0
		type: LdLitType
]

{ #category : 'constructor' }
DecompilerConstructor >> codeAnySelector: selector [

	^SelectorNode new
		key: selector
		index: nil
		type: SendType
]

{ #category : 'constructor' }
DecompilerConstructor >> codeArguments: args block: block [
	^block arguments: args
]

{ #category : 'constructor' }
DecompilerConstructor >> codeArguments: args temps: temps block: block  [
	block
		arguments: args;
		temporaries: temps.
	^block
]

{ #category : 'constructor' }
DecompilerConstructor >> codeAssignTo: variable value: expression [
	^AssignmentNode new variable: variable value: expression
]

{ #category : 'constructor' }
DecompilerConstructor >> codeBlock: statements returns: returns [
	^ BlockNode statements: statements returns: returns
]

{ #category : 'constructor' }
DecompilerConstructor >> codeBrace: elements [

	^BraceNode new elements: elements
]

{ #category : 'constructor' }
DecompilerConstructor >> codeCascade: receiver messages: messages [

	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [CascadeNode new receiver: receiver messages: messages]
]

{ #category : 'constructor' }
DecompilerConstructor >> codeCascadedMessage: selector arguments: arguments [
	^self
		codeMessage: nil
		selector: selector
		arguments: arguments
]

{ #category : 'constructor' }
DecompilerConstructor >> codeConstants [
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])
]

{ #category : 'constructor' }
DecompilerConstructor >> codeEmptyBlock [
	^ BlockNode withJust: NodeNil
]

{ #category : 'constructor' }
DecompilerConstructor >> codeInst: index [

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index printString])
		index: index + 1
]

{ #category : 'constructor' }
DecompilerConstructor >> codeMessage: receiver selector: selector arguments: arguments [
	| symbol |
	symbol := selector key.
	(self
		decodeLiteralVariableValueDereferenceWithReceiver: receiver
		selector: symbol
		arguments: arguments) ifNotNil: [:node| ^node].

	^MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence
]

{ #category : 'constructor' }
DecompilerConstructor >> codeMethod: selector block: block tempVars: vars primitive: primitive class: class [

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: nil) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode
]

{ #category : 'constructor' }
DecompilerConstructor >> codeRemoteTemp: index remoteTemps: tempVector [

	^(RemoteTempVectorNode new
		name: 'remoteVar', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself
]

{ #category : 'constructor' }
DecompilerConstructor >> codeSelector: sel code: code [
	^SelectorNode new key: sel code: code
]

{ #category : 'constructor' }
DecompilerConstructor >> codeSuper [
	^NodeSuper
]

{ #category : 'constructor' }
DecompilerConstructor >> codeTemp: index [

	^ TempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0
]

{ #category : 'constructor' }
DecompilerConstructor >> codeTemp: index named: tempName [

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0
]

{ #category : 'constructor' }
DecompilerConstructor >> codeThisContext [
	^NodeThisContext
]

{ #category : 'constructor' }
DecompilerConstructor >> decodeIfNilWithReceiver: receiver selector: selector arguments: arguments [
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
	^ (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3)
		noteSpecialSelector: #ifNil:ifNotNil:
]

{ #category : 'constructor' }
DecompilerConstructor >> decodeIfNilWithReceiver: receiver selector: selector arguments: arguments tempReadCounts: tempReadCounts [
	
	| node temp |
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
				
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
		
	"Like #to:(by:)do:, support only local temps."
	(((temp := receiver ifNilTemporary) isNil or: [tempReadCounts includesKey: temp]) or: [
		"What about 'object ifNotNil: [:o | ]', which as not read the blockArg? Just check that there is no remote vector pointing to it."
		tempReadCounts keys noneSatisfy:
			[:otherTemp |
				otherTemp isIndirectTempVector
					ifTrue: [otherTemp remoteTemps anySatisfy:
						[:remoteTemp |
						remoteTemp name = temp name]]
					ifFalse: [otherTemp name = temp name]]
			])
		ifFalse: [^ nil].
		
	node := (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3).

	"Reconfigure the message node to #ifNil:ifNotNil:. Note that original* instance variables keep their optimized format. See MessageNode >> #printIfNilNotNil:indent:."	
	node
		noteSpecialSelector: #ifNil:ifNotNil:;
		selector: (SelectorNode new key: #ifNil:ifNotNil:).
	
	temp ifNil: [^ node].
	temp isTemp ifFalse: [^ node].
	
	(arguments second isJust: NodeNil) not ifTrue:
		[temp beBlockArg.
		node arguments: {
			arguments first.
			arguments second copy arguments: { temp }; yourself }].
				
	^ node
]

{ #category : 'constructor' }
DecompilerConstructor >> decodeLiteralVariableValueDereferenceWithReceiver: receiver selector: selector arguments: arguments [
	| varNode |
	(receiver notNil "cascades"
	 and: [receiver isLiteralNode
	 and: [receiver key isVariableBinding]]) ifFalse:
		[^nil].
	varNode := self codeAnyLitInd: receiver key.
	selector = #value ifTrue:
		[^varNode].
	^selector = #value: ifTrue:
		[self codeAssignTo: varNode value: arguments first]
]

{ #category : 'initialize-release' }
DecompilerConstructor >> method: aMethod class: aClass literals: literals [
	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals
]
