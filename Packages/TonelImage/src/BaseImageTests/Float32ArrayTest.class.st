Class {
	#name : 'Float32ArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Arrayed',
	#package : 'BaseImageTests',
	#tag : 'Collections-Arrayed'
}

{ #category : 'Aux' }
Float32ArrayTest >> divideByArrayIndeterminate [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: #[0.0 2.0 3.141592] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float32ArrayTest >> divideByArrayOne [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: #[1.0 1.0 1.0] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float32ArrayTest >> divideByArrayZero [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat32Array
		divideBy: #[0.0 2.0 3.141592] asFloat32Array
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float32ArrayTest >> divideByScalarIndeterminate [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float32ArrayTest >> divideByScalarOne [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] asFloat32Array
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float32ArrayTest >> divideByScalarZero [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] asFloat32Array
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Tests' }
Float32ArrayTest >> testIsType [
	self assert: (#[1.2 2.4 3.6 ] is: #Collection).
	self assert: (#[1.2 2.4 3.6] asFloat32Array is: #Float32Array).
]

{ #category : 'Tests' }
Float32ArrayTest >> testZeroArrayDividend [

	self assert: #[1.2 2.4 0.0 ] asFloat32Array / #[1.2 1.2 1.2 ] asFloat32Array = #[1.0 2.0 0.0 ] asFloat32Array 
]

{ #category : 'Tests' }
Float32ArrayTest >> testZeroArrayDivisor [

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] asFloat32Array / #[1.0 1.0 0.0 ] asFloat32Array] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: #[1.0 1.0 0.0 0.0 ] asFloat32Array ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ] asFloat32Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat32Array
			divideBy: #[1.0 1.0 0.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] asFloat32Array
			divideBy: #[0.0 1.0 1.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592] asFloat32Array.
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.
]

{ #category : 'Tests' }
Float32ArrayTest >> testZeroDividend [

	self assert: #[1.2 2.4 3.6 0.0 ] asFloat32Array / 1.2 = #[1.0 2.0 3.0 0.0 ] asFloat32Array 
]

{ #category : 'Tests' }
Float32ArrayTest >> testZeroDivisor [

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] asFloat32Array / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ] asFloat32Array.

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] asFloat32Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] asFloat32Array
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592] asFloat32Array.
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.
]
