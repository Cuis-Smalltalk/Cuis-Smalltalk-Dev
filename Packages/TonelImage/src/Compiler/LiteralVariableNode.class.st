Class {
	#name : 'LiteralVariableNode',
	#superclass : 'VariableNode',
	#instVars : [
		'readNode',
		'writeNode'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
LiteralVariableNode >> accept: aVisitor [
	^aVisitor visitLiteralVariableNode: self
]

{ #category : 'testing' }
LiteralVariableNode >> assignmentCheck: encoder at: location [
	^(key isVariableBinding and: [key canAssign not])
		ifTrue: [location]
		ifFalse: [-1]
]

{ #category : 'code generation' }
LiteralVariableNode >> emitCodeForLoad: stack forValue: forValue encoder: encoder [
	"If a normal literal variable (not sending value:), do nothing.
	 If for value (e.g. v := Binding := expr) do nothing; the work will be done in emitCodeForStore:encoder:.
	 If not for value then indeed load.  The rest of the work will be done in  emitCodeForStorePop:encoder:."
	(writeNode isNil or: [forValue]) ifTrue: [^self].
	encoder genPushLiteral: index.
	stack push: 1
]

{ #category : 'code generation' }
LiteralVariableNode >> emitCodeForStore: stack encoder: encoder [
	| exprOffset |
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop.
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver.  If they
	 did we could generate much simpler code, e.g.
		encoder genPushLiteral: index.
		stack push: 1.
		writeNode emitCode: stack args: 1 encoder: encoder super: false"
	exprOffset := stack position - 1.
	encoder genPushLiteral: index.
	stack push: 1.
	encoder genPushTempLong: exprOffset.
	stack push: 1.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop
]

{ #category : 'code generation' }
LiteralVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop
]

{ #category : 'code generation' }
LiteralVariableNode >> emitCodeForValue: stack encoder: encoder [
	^readNode
		ifNil: [stack push: 1.
			encoder genPushLiteralVar: index]
		ifNotNil: [readNode emitCodeForValue: stack encoder: encoder]
]

{ #category : 'testing' }
LiteralVariableNode >> isLiteralVariableNode [

	^ true
]

{ #category : 'testing' }
LiteralVariableNode >> isReferencingBehavior [
	
	^key value isBehavior
]

{ #category : 'testing' }
LiteralVariableNode >> isReferencingClassVariable [
	
	"If it is not a global, I assume it is a class var"
	^self isReferencingGlobal not and: [ self isUndeclared not ]
]

{ #category : 'testing' }
LiteralVariableNode >> isReferencingGlobal [
	
	^(Smalltalk bindingOf: key name) notNil  
]

{ #category : 'testing' }
LiteralVariableNode >> isUndeclared [
	
	^Undeclared includesKey: name 
]

{ #category : 'code generation' }
LiteralVariableNode >> sizeCodeForLoad: encoder forValue: forValue [
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding and: [forValue not]])
		ifTrue: [encoder sizePushLiteral: index]
		ifFalse: [0]
]

{ #category : 'code generation' }
LiteralVariableNode >> sizeCodeForStore: encoder [
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialWriteBinding]) ifFalse:
		[^encoder sizeStoreLiteralVar: index].
	writeNode := encoder encodeSelector: #value:.
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop."
	^(encoder sizePushLiteral: index)
	  + (encoder sizePushTempLong: 0) "we don't know yet, hence long, sigh..."
	  + (writeNode sizeCode: encoder args: 1 super: false)
	  + encoder sizePop
]

{ #category : 'code generation' }
LiteralVariableNode >> sizeCodeForStorePop: encoder [
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding])
		ifTrue: [	writeNode := encoder encodeSelector: #value:.
				^ (writeNode sizeCode: encoder args: 1 super: false)
	  			+ encoder sizePop]
		ifFalse: [encoder sizeStorePopLiteralVar: index]
]

{ #category : 'code generation' }
LiteralVariableNode >> sizeCodeForValue: encoder [
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialReadBinding]) 
		ifFalse:
			[^encoder sizePushLiteralVar: index].
	readNode := MessageNode new 
		receiver: (encoder encodeLiteral: key)
		selector: (encoder encodeSelector: #value)
		arguments: #()
		precedence: #value precedence.
	^readNode sizeCodeForValue: encoder
]

{ #category : 'testing' }
LiteralVariableNode >> variableBinding [
	^key
]
