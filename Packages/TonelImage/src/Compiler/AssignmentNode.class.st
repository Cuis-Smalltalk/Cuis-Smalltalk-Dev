"
AssignmentNode comment: 'I represent a (var_expr) construct.'
"
Class {
	#name : 'AssignmentNode',
	#superclass : 'ParseNode',
	#instVars : [
		'variable',
		'value'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
AssignmentNode >> accept: aVisitor [
	^aVisitor visitAssignmentNode: self
]

{ #category : 'code generation (closures)' }
AssignmentNode >> analyseTempsWithin: scopeBlock "<BlockNode>"  rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>" [
	"N.B.  since assigment happens _after_ the value is evaluated the value is sent the message _first_."
	value analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools.
	variable beingAssignedToAnalyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools
]

{ #category : 'code generation' }
AssignmentNode >> emitCodeForEffect: stack encoder: encoder [

	variable emitCodeForLoad: stack forValue: false encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStorePop: stack encoder: encoder
]

{ #category : 'code generation' }
AssignmentNode >> emitCodeForValue: stack encoder: encoder [

	variable emitCodeForLoad: stack forValue: true encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStore: stack encoder: encoder
]

{ #category : 'testing' }
AssignmentNode >> equivalentTo: aParseNode [

	^ aParseNode isAssignmentNode
		and: [ self variable equivalentTo: aParseNode variable ]
		and: [ self value equivalentTo: aParseNode value ]
]

{ #category : 'source ranges' }
AssignmentNode >> expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode [

	^ self consolidateAsCollection: (self
		expandRange: (sourceRanges isInterval ifTrue: [ sourceRanges ] ifFalse: [ sourceRanges first ])
		basedOn: (allSourceRanges at: variable))
]

{ #category : 'private' }
AssignmentNode >> ifNilTemporary [
	"(temp := object) == nil ifTrue: [...] ifFalse: [...]"
	
	^ self variable
]

{ #category : 'testing' }
AssignmentNode >> isAssignmentNode [
	^true
]

{ #category : 'testing' }
AssignmentNode >> isAssignmentToTemporary [

	^ variable isTemp 
]

{ #category : 'testing' }
AssignmentNode >> isComplex [
	^ value isComplex
]

{ #category : 'printing' }
AssignmentNode >> printOn: aStream indent: level  [
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printOn: aStream indent: level
]

{ #category : 'printing' }
AssignmentNode >> printOn: aStream indent: level precedence: p [

	aStream nextPut: $(.
	self printOn: aStream indent: level.
	aStream nextPut: $)
]

{ #category : 'printing' }
AssignmentNode >> printWithClosureAnalysisOn: aStream indent: level  [
	variable printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPutAll: ' := '.
	value printWithClosureAnalysisOn: aStream indent: level
]

{ #category : 'printing' }
AssignmentNode >> printWithClosureAnalysisOn: aStream indent: level precedence: p [

	aStream nextPut: $(.
	self printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPut: $)
]

{ #category : 'testing' }
AssignmentNode >> referencesSelf [

	^variable referencesSelf or: [value referencesSelf]
]

{ #category : 'testing' }
AssignmentNode >> referencesSuper [

	^variable referencesSuper or: [value referencesSuper]
]

{ #category : 'code generation' }
AssignmentNode >> sizeCodeForEffect: encoder [

	^(variable sizeCodeForLoad: encoder forValue: false)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStorePop: encoder)
]

{ #category : 'code generation' }
AssignmentNode >> sizeCodeForValue: encoder [

	^(variable sizeCodeForLoad: encoder forValue: true)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStore: encoder)
]

{ #category : 'initialization' }
AssignmentNode >> toDoIncrement: var [
	var = variable ifFalse: [^ nil].
	(value isMemberOf: MessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]
]

{ #category : 'initialization' }
AssignmentNode >> value [
	^ value
]

{ #category : 'equation translation' }
AssignmentNode >> variable [
	^variable
]

{ #category : 'initialize-release' }
AssignmentNode >> variable: aVariable value: expression  [
	variable := aVariable.
	value := expression
]

{ #category : 'initialization' }
AssignmentNode >> variable: aVariable value: expression from: encoder [

	"Case of remote temp vars"
	(aVariable isMemberOf: MessageAsTempNode) ifTrue: [ ^aVariable store: expression from: encoder].
	
	variable := aVariable.
	value := expression.
	
	^self
	

]

{ #category : 'initialization' }
AssignmentNode >> variable: aVariable value: expression from: encoder sourceRange: range [

	| realNode |
	
	realNode := self variable: aVariable value: expression from: encoder.
	encoder noteSourceRange: range forNode: realNode.
	
	^realNode
]
