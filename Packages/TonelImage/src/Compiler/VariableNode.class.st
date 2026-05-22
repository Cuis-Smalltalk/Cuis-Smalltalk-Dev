"
I am a parse tree leaf representing a variable. Note that my name and key are different for pool variables: the key is the Object Reference.
"
Class {
	#name : 'VariableNode',
	#superclass : 'LeafNode',
	#instVars : [
		'name'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'class initialization' }
VariableNode class >> initClassCachedState [
	"
	VariableNode initialize. Decompiler initialize
	"
	| encoder |
	encoder := Encoder new.
	StdVariables := Dictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary newWithRoomForMoreThan: 64.
	encoder
		fillDict: StdSelectors
		with: SpecialSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (1 to: Smalltalk specialSelectorSize) asArray.
	StdLiterals := LiteralDictionary newWithRoomForMoreThan: 16.
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'
]

{ #category : 'class initialization' }
VariableNode class >> initialize [
	"
	VariableNode initialize
	"
	self initClassCachedState
]

{ #category : 'instance creation' }
VariableNode class >> named: aString  [

	^ self new name: aString; yourself
]

{ #category : 'class initialization' }
VariableNode class >> releaseClassCachedState [

	StdVariables := nil.
	StdSelectors := nil.
	StdLiterals := nil.

	NodeNil := nil.
	NodeTrue := nil.
	NodeFalse := nil.
	NodeSelf := nil.
	NodeThisContext := nil.
	NodeSuper := nil 
]

{ #category : 'visiting' }
VariableNode >> accept: aVisitor [
	^aVisitor visitVariableNode: self
]

{ #category : 'initialization' }
VariableNode >> asStorableNode: encoder [
	^ self
]

{ #category : 'testing' }
VariableNode >> assignmentCheck: encoder at: location [
	^(encoder cantStoreInto: name) ifTrue: [location] ifFalse: [-1]
]

{ #category : 'code generation (closures)' }
VariableNode >> beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>" [
	"No-op overridden by TempVariableNode"
]

{ #category : 'tiles' }
VariableNode >> currentValueIn: aContext [

	aContext ifNil: [^nil].
	^((self variableGetterBlockIn: aContext) ifNil: [^nil]) value printString
	


]

{ #category : 'printing' }
VariableNode >> descriptiveString [

	^self class name, ' named: ', name printString
]

{ #category : 'code generation (new scheme)' }
VariableNode >> emitCodeForReturn: stack encoder: encoder [
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			"short returns"
			 encoder genReturnSpecialLiteral: specialLiteral.
			 stack push: 1 "doesnt seem right".
			 ^self].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue: 
		["short returns"
		 encoder genReturnReceiver.
		 stack push: 1 "doesnt seem right".
		 ^self].
	super emitCodeForReturn: stack encoder: encoder
]

{ #category : 'code generation (new scheme)' }
VariableNode >> emitCodeForStore: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : 'code generation (new scheme)' }
VariableNode >> emitCodeForStorePop: stack encoder: encoder [
	self varNodeType ~= 1 ifTrue:
		[self halt].
	encoder genStorePopInstVar: index.
	stack pop: 1
]

{ #category : 'code generation (new scheme)' }
VariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder genPushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder genPushReceiver].
	code = LdThisContext ifTrue:
		[^encoder genPushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder genPushInstVar: index
]

{ #category : 'testing' }
VariableNode >> equivalentTo: aParseNode [

	^ aParseNode isVariableNode and: [ super equivalentTo: aParseNode ]
]

{ #category : 'code generation' }
VariableNode >> fieldOffset [
	"Return temp or instVar offset for this variable"
	^index ifNil: [code < 256
					ifTrue: [code \\ 16]
					ifFalse: [code \\ 256]]
]

{ #category : 'testing' }
VariableNode >> index [
	"If index is nil, this code attempts to reconstruct the index from its encoding in code."
	index ifNotNil:
		[^index].
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[self assert: index = (code \\ 256).
		^code \\ 256].
	code >= (CodeBases at: self varNodeType) ifTrue:
		[self assert: index = (code - (CodeBases at: self varNodeType)).
		^code - (CodeBases at: self varNodeType)].
	self assert: index = (code - self varNodeType).
	^code - self varNodeType
]

{ #category : 'testing' }
VariableNode >> isFalsePseudoVariable [
	
	^key = 'false' or: [name = '{{false}}']
]

{ #category : 'testing' }
VariableNode >> isNamed: aName [

	^ self name = aName
]

{ #category : 'testing' }
VariableNode >> isNilPseudoVariable [
	
	^ key = 'nil' or: [name = '{{nil}}']
]

{ #category : 'testing' }
VariableNode >> isSelfPseudoVariable [
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [ name = 'self'  or: [name = '{{self}}']]
]

{ #category : 'testing' }
VariableNode >> isSuperPseudoVariable [
	"Answer if this ParseNode represents the 'super' pseudo-variable."

	^ key = 'super' or: [name = '{{super}}']
]

{ #category : 'testing' }
VariableNode >> isThisContextPseudoVariable [
	
	^key = 'thisContext' or: [name = '{{thisContext}}']
]

{ #category : 'testing' }
VariableNode >> isTruePseudoVariable [
	
	^ key = 'true' or: [name = '{{true}}']
]

{ #category : 'testing' }
VariableNode >> isUndeclared [
	^false
]

{ #category : 'testing' }
VariableNode >> isVariableNode [
	^true
]

{ #category : 'testing' }
VariableNode >> isVariableReference [
	^true
]

{ #category : 'accessing' }
VariableNode >> name [
	^ name
]

{ #category : 'initialization' }
VariableNode >> name: string [
	"Change name"

	name := string
]

{ #category : 'initialization' }
VariableNode >> name: varName index: i type: type [
	"Only used for initting instVar refs"
	^self name: varName key: varName index: i type: type
]

{ #category : 'initialization' }
VariableNode >> name: string key: object code: byte [
	"Only used for initting std variables, nil, true, false, self, etc."
	name := string.
	key := object.
	code := byte
]

{ #category : 'initialization' }
VariableNode >> name: varName key: objRef index: i type: type [
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)
]

{ #category : 'initialization' }
VariableNode >> nameAndKey: aName [

	name := key := aName 
]

{ #category : 'printing' }
VariableNode >> printOn: aStream indent: level  [
	aStream nextPutAll: name
]

{ #category : 'printing' }
VariableNode >> printWithClosureAnalysisOn: aStream indent: level  [

	aStream nextPutAll: name
]

{ #category : 'testing' }
VariableNode >> referencesSelf [

	^ self isSelfPseudoVariable 
]

{ #category : 'testing' }
VariableNode >> referencesSuper [

	^self isSuperPseudoVariable 
]

{ #category : 'code generation' }
VariableNode >> sizeCodeForReturn: encoder [
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			 ^encoder sizeReturnSpecialLiteral: specialLiteral].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue:
		[^encoder sizeReturnReceiver].
	^super sizeCodeForReturn: encoder
]

{ #category : 'code generation (new scheme)' }
VariableNode >> sizeCodeForStore: encoder [
	self shouldNotImplement
]

{ #category : 'code generation (new scheme)' }
VariableNode >> sizeCodeForStorePop: encoder [
	self shouldNotImplement
]

{ #category : 'code generation (new scheme)' }
VariableNode >> sizeCodeForValue: encoder [
	self reserve: encoder.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral| "i.e. the pseudo-variables nil true & false"
			 ^encoder sizePushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder sizePushReceiver].
	code = LdThisContext ifTrue:
		[^encoder sizePushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder sizePushInstVar: index
]

{ #category : 'testing' }
VariableNode >> varNodeType [
	"This code attempts to reconstruct the type from its encoding in code.
		This allows one to test, for instance, (aNode type = LdInstType)."
	| type |
	code < 0 ifTrue: [^code negated].
	code >= 256 ifTrue: [^code // 256].
	type := CodeBases findFirst: [:one | code < one].
	^type = 0 ifTrue: [5] ifFalse: [type - 1]
]

{ #category : 'tiles' }
VariableNode >> variableGetterBlockIn: aContext [

	| temps tempIndex ivars |
	(self varNodeType = 4 and: [self key isVariableBinding]) ifTrue: [
		^[self key value]
	].
	aContext ifNil: [^nil].
	self isSelfPseudoVariable ifTrue: [^[aContext receiver]].
	self varNodeType = 1 ifTrue: [
		ivars := aContext receiver class allInstVarNames.
		tempIndex := ivars indexOf: self name ifAbsent: [^nil].
		^[aContext receiver instVarAt: tempIndex]
	].
	self varNodeType = 2 ifTrue: [
		temps := aContext tempNames.
		tempIndex := temps indexOf: self name ifAbsent: [^nil].
		^[aContext tempAt: tempIndex]
	].
	^nil

]
