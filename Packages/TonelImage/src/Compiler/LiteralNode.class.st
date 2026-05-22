"
I am a parse tree leaf representing a literal string or number.
"
Class {
	#name : 'LiteralNode',
	#superclass : 'LeafNode',
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
LiteralNode >> accept: aVisitor [
	^aVisitor visitLiteralNode: self
]

{ #category : 'code generation' }
LiteralNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	(encoder isSpecialLiteralForPush: key)
		ifTrue: [encoder genPushSpecialLiteral: key]
		ifFalse: [encoder genPushLiteral: index]
]

{ #category : 'testing' }
LiteralNode >> equivalentTo: aParseNode [

	^ aParseNode isLiteralNode and: [ super equivalentTo: aParseNode ]
]

{ #category : 'evaluation' }
LiteralNode >> eval [
	"When everything in me is a constant, I can produce a value.  This is only used by the Scripting system (TilePadMorph tilesFrom:in:)"

	^ key
]

{ #category : 'testing' }
LiteralNode >> isLiteralCharacter [
	^ key isCharacter
]

{ #category : 'testing' }
LiteralNode >> isLiteralNode [

	^ true
]

{ #category : 'testing' }
LiteralNode >> isLiteralNumber [
	^ key isNumber
]

{ #category : 'testing' }
LiteralNode >> isLiteralSmallInteger [
	^ key isNumber and: [ key class == SmallInteger ]
]

{ #category : 'testing' }
LiteralNode >> isSpecialConstant [
	^ code between: LdTrue and: LdMinus1+3
]

{ #category : 'testing' }
LiteralNode >> literalValue [

	^key
]

{ #category : 'initialization' }
LiteralNode >> name: literal key: object index: i type: type [
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
]

{ #category : 'printing' }
LiteralNode >> printOn: aStream indent: level [
	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream
						nextPutAll: '###';
						nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream
						nextPutAll: '##';
						nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ | isComplex |
					isComplex := false.
					key isArray ifTrue: [
						isComplex := key anySatisfy: [ :ea |
							ea isArray ]].
					"Is it complex? (i.e. array of arrays)"
					isComplex
						ifTrue: [
							aStream
								nextPut: $#;
								nextPut: $(.
							key do: [ :ea |
								aStream newLineTab: (1 max: level + 1).
								ea printAsLiteralOn: aStream ].
							aStream newLineTab: (1 max: level).
							aStream nextPut: $) ]
						ifFalse: [ key printAsLiteralOn: aStream ]]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $` ]]
]

{ #category : 'printing' }
LiteralNode >> printWithClosureAnalysisOn: aStream indent: level [

	key isVariableBinding
		ifTrue:
			[key key isNil
				ifTrue:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifFalse:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]
]

{ #category : 'code generation' }
LiteralNode >> reserve: encoder  [
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue:
		[index := key isVariableBinding "true if sending value[:] to a special binding"
					ifTrue: [encoder sharableLitIndex: key]
					ifFalse: [encoder litIndex: key].
		 code := self code: index type: 0 - code]
]

{ #category : 'code generation (new scheme)' }
LiteralNode >> sizeCodeForEffect: encoder [

	"Include im method literals even if unused"
	self reserve: encoder.
	^0
]

{ #category : 'code generation' }
LiteralNode >> sizeCodeForValue: encoder [
	^(encoder isSpecialLiteralForPush: key)
		ifTrue: [encoder sizePushSpecialLiteral: key]
		ifFalse:
			[self reserve: encoder.
			encoder sizePushLiteral: index]
]
