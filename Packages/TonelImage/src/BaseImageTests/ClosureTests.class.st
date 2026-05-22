Class {
	#name : 'ClosureTests',
	#superclass : 'TestCase',
	#instVars : [
		'collection',
		'ivar'
	],
	#classVars : [
		'ClassVar'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'utilities' }
ClosureTests >> assertValues: anArray [
	| values |
	values := collection collect: [ :each | each value ].
	self 
		assert: anArray asArray = values asArray
		description: 'Expected: ' , anArray asArray printString , 
			', but got ' , values asArray printString
]

{ #category : 'utilities' }
ClosureTests >> evaluateCopyOf: aBlock [
	aBlock copy value
]

{ #category : 'testing' }
ClosureTests >> methodArgument: anObject [
	^ [ anObject ]
	
]

{ #category : 'utilities' }
ClosureTests >> methodWithNonLocalReturn [
	self evaluateCopyOf: [^ self].
	self signalFailure: 'Should never reach here'
]

{ #category : 'running' }
ClosureTests >> setUp [
	super setUp.
	collection := OrderedCollection new
]

{ #category : 'testing' }
ClosureTests >> testBlockArgument [
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2
]

{ #category : 'testing' }
ClosureTests >> testBlockTemp [
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2
]

{ #category : 'testing' }
ClosureTests >> testCopyNonLocalReturn [
	self
		shouldnt: [self methodWithNonLocalReturn]
		raise: Error
]

{ #category : 'testing' }
ClosureTests >> testIsCleanClosure [
	"
	ClosureTests new testIsCleanClosure
	"
	| tempVar |
	tempVar := 1.
	self assert: [ 3 + 4 ] isCleanClosure.
	self assert: [ :a | a * 2 ] isCleanClosure.
	self assert: [ Smalltalk size ] isCleanClosure.
	self assert: [ ClosureTests selectors size ] isCleanClosure.
	self assert: [ :blockArg | blockArg printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp := 7 ] isCleanClosure.
	self assert: [ | c | c := [ :a :b | a+b ]. c value: 3 value: 4 ] isCleanClosure.

	self assert: [[ 3 + 4 ] value] isCleanClosure.
	self assert: [[ :a | a * 2 ] value] isCleanClosure.
	self assert: [[ Smalltalk size ] value] isCleanClosure.
	self assert: [[ ClosureTests selectors size ] value] isCleanClosure.
	self assert: [[ :blockArg | blockArg printString ] value] isCleanClosure.
	self assert: [[ | blockTemp | blockTemp printString ] value] isCleanClosure.
	self assert: [[ | blockTemp | blockTemp := 7 ] value] isCleanClosure.
	self assert: [[ | c | c := [ :a :b | a+b ]. c value: 3 value: 4 ] value] isCleanClosure.

	self assert: [ | outerBlockTemp | [ outerBlockTemp printString ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [ outerBlockTemp := 7 ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp := 7 ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp := 7 ]] ] isCleanClosure.

	self deny: [ | outerBlockTemp | [ outerBlockTemp printString ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [ outerBlockTemp := 7 ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp printString ]] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp := 7 ]] isCleanClosure ] value.

	self deny: [ tempVar + 1 ] isCleanClosure.
	self deny: [ tempVar := 1 ] isCleanClosure.
	self deny: [ ivar + 1 ] isCleanClosure.
	self deny: [ ivar := 1 ] isCleanClosure.
	self deny: [ ^ true ] isCleanClosure.
	self deny: [ self printString ] isCleanClosure.
	self deny: [ ^ self ] isCleanClosure.
	self deny: [ super printString ] isCleanClosure.
	self deny: [ ClassVar + 1 ] isCleanClosure.
	self deny: [ ClassVar := 1 ] isCleanClosure.

	self deny: [[ 3 + 4 ]. [ tempVar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ tempVar := 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ivar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ivar := 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ^ true ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ self printString ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ^ self ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ super printString ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ClassVar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ClassVar := 1 ] value ] isCleanClosure.
]

{ #category : 'testing' }
ClosureTests >> testMethodArgument [
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	self assert: block value = 2
]

{ #category : 'testing' }
ClosureTests >> testMethodTemp [
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoArgument [
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoArgumentNotInlined [
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoInsideTemp [
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoInsideTempNotInlined [
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoOutsideTemp [
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-todo' }
ClosureTests >> testToDoOutsideTempNotInlined [
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileModificationAfter [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		collection add: [ index ].
		index := index + 1 ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileModificationAfterNotInlined [
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileModificationBefore [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileModificationBeforeNotInlined [
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileWithTemp [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'testing-while' }
ClosureTests >> testWhileWithTempNotInlined [
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(1 2 3 4 5)
]
