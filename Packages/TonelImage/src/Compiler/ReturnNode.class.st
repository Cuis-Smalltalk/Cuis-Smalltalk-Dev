"
I represent an expression of the form ^expr.
"
Class {
	#name : 'ReturnNode',
	#superclass : 'ParseNode',
	#instVars : [
		'expr'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
ReturnNode >> accept: aVisitor [
	^aVisitor visitReturnNode: self
]

{ #category : 'code generation (closures)' }
ReturnNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>" [
	"Note we could do this:
		scopeBlock ~~ rootNode block ifTrue:
			[scopeBlock noteNonLocalReturn].
	 and pass up the flag in <BlockNode>>>analyseTempsWithin:rootNode:
	 which may be fast but will also give less information the debugger.
	 For now we consider clean blocks a premature optimization."
	self flag: 'consider clean blocks'.
	expr analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools
]

{ #category : 'converting' }
ReturnNode >> asReturnNode [

]

{ #category : 'code generation' }
ReturnNode >> code [
	^expr code
]

{ #category : 'code generation (new scheme)' }
ReturnNode >> emitCodeForReturn: stack encoder: encoder [

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
]

{ #category : 'code generation (new scheme)' }
ReturnNode >> emitCodeForValue: stack encoder: encoder [

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
]

{ #category : 'testing' }
ReturnNode >> equivalentTo: aParseNode [

	^ aParseNode isReturn and: [ expr equivalentTo: aParseNode expr ]
]

{ #category : 'printing' }
ReturnNode >> expr [

	^ expr.

]

{ #category : 'initialize-release' }
ReturnNode >> expr: e  [
	expr := e
]

{ #category : 'initialize-release' }
ReturnNode >> expr: e encoder: encoder sourceRange: range  [
	expr := e.
	encoder noteSourceRange: range forNode: self
]

{ #category : 'testing' }
ReturnNode >> isImplicitSelfReturnIn: aMethodNode [

	self isReturnSelf ifFalse: [^false]. 
	aMethodNode encoder rangeForNode: self ifAbsent: [^true].
	
	^false
]

{ #category : 'testing' }
ReturnNode >> isReturn [

	^true
]

{ #category : 'testing' }
ReturnNode >> isReturnSelf [
	^expr == NodeSelf
]

{ #category : 'testing' }
ReturnNode >> isSpecialConstant [

	^expr isSpecialConstant
]

{ #category : 'testing' }
ReturnNode >> isVariableReference [
	^expr isVariableReference
]

{ #category : 'printing' }
ReturnNode >> printOn: aStream indent: level [

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printOn: aStream indent: level.
	expr printCommentOn: aStream indent: level
]

{ #category : 'printing' }
ReturnNode >> printWithClosureAnalysisOn: aStream indent: level [

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printWithClosureAnalysisOn: aStream indent: level.
	expr printCommentOn: aStream indent: level
]

{ #category : 'testing' }
ReturnNode >> referencesSelf [

	^expr referencesSelf
]

{ #category : 'testing' }
ReturnNode >> referencesSuper [

	^expr referencesSuper
]

{ #category : 'testing' }
ReturnNode >> sendsMessageToSelf: aSelector [

	^expr sendsMessageToSelf: aSelector
]

{ #category : 'code generation (new scheme)' }
ReturnNode >> sizeCodeForReturn: encoder [

	^expr sizeCodeForReturn: encoder
]

{ #category : 'code generation (new scheme)' }
ReturnNode >> sizeCodeForValue: encoder [

	^expr sizeCodeForReturn: encoder
]
