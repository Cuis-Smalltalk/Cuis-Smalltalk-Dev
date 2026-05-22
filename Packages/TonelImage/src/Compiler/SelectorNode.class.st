"
I am a parse tree leaf representing a selector.
"
Class {
	#name : 'SelectorNode',
	#superclass : 'LeafNode',
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
SelectorNode >> accept: aVisitor [
	^aVisitor visitSelectorNode: self
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> emitCode: stack args: nArgs encoder: encoder [

	self emitCode: stack
		args: nArgs
		encoder: encoder
		super: false
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> emitCode: stack args: nArgs encoder: encoder super: supered [
	stack pop: nArgs.
	^supered
		ifTrue:
			[encoder genSendSuper: index numArgs: nArgs]
		ifFalse:
			[encoder
				genSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> emitCodeForEffect: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> emitCodeForValue: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : 'testing' }
SelectorNode >> equivalentTo: aParseNode [

	^ aParseNode isSelectorNode and: [ super equivalentTo: aParseNode ]
]

{ #category : 'testing' }
SelectorNode >> isForFFICall [
	^key includesSubString: '()/'
]

{ #category : 'testing' }
SelectorNode >> isInfix [

	^key isInfix 
]

{ #category : 'testing' }
SelectorNode >> isInitializePvtSelector [
	"Answer whether the receiver is a private instance initialization message selector"

	^ key isInitializePvtSelector
]

{ #category : 'testing' }
SelectorNode >> isNamed: aSelectorName [

	^ self key = aSelectorName
]

{ #category : 'testing' }
SelectorNode >> isPvtSelector [
	"Answer if this selector node is a private message selector."

	^key isPvtSelector
]

{ #category : 'testing' }
SelectorNode >> isSelectorNode [

	^ true
]

{ #category : 'printing' }
SelectorNode >> key: aSelector [
	"This is for printing of FFI selectors."
	key := aSelector
]

{ #category : 'initialization' }
SelectorNode >> name: literal key: object index: i type: type [
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
]

{ #category : 'printing' }
SelectorNode >> printAsFFICallWithArguments: aSequence on: aStream indent: level [
	aStream nextPutAll: (key copyUpTo: $)).
	aSequence
		do: [:arg| arg printOn: aStream indent: level]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)
]

{ #category : 'printing' }
SelectorNode >> printOn: aStream indent: level  [
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])
]

{ #category : 'printing' }
SelectorNode >> printWithClosureAnalysisOn: aStream indent: level  [
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])
]

{ #category : 'code generation' }
SelectorNode >> reserve: encoder  [
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder sharableLitIndex: key) type: 0 - code]
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> sizeCode: encoder args: nArgs super: supered [
	self reserve: encoder.
	^supered
		ifTrue:
			[code < Send "i.e. its a special selector" ifTrue:
				[code := self code: (index := encoder sharableLitIndex: key) type: 5].
			 encoder sizeSendSuper: index numArgs: nArgs]
		ifFalse:
			[self flag: #yuck. "special selector sends cause this problem"
			 encoder
				sizeSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> sizeCodeForEffect: encoder [

	self shouldNotImplement
]

{ #category : 'code generation (new scheme)' }
SelectorNode >> sizeCodeForValue: encoder [

	self shouldNotImplement
]
