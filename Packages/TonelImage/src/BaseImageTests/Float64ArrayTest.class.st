Class {
	#name : 'Float64ArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Arrayed',
	#package : 'BaseImageTests',
	#tag : 'Collections-Arrayed'
}

{ #category : 'Aux' }
Float64ArrayTest >> divideByArrayIndeterminate [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float64ArrayTest >> divideByArrayOne [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592] copy
		divideBy: #[1.0 1.0 1.0]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float64ArrayTest >> divideByArrayZero [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592] copy
		divideBy: #[0.0 2.0 3.141592]
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float64ArrayTest >> divideByScalarIndeterminate [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592]
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float64ArrayTest >> divideByScalarOne [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[0.0 2.0 3.141592]
		divideBy: 1.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Aux' }
Float64ArrayTest >> divideByScalarZero [
	"An example for testing. Uses a method to test behavior of non-local return."
	^#[1.0 2.0 3.141592]
		divideBy: 0.0
		ifDivisorZero: [^ #divisionByZero]
		ifBothZero: [^ #indeterminateResult]
]

{ #category : 'Tests' }
Float64ArrayTest >> testIsCollection [
	self assert: (#[1.0 2.0 3.141592 0.0] is: #Collection).
	self assert: (#[-100.0 -100.0 -100.0 -200.0 ] is: #Collection).
]

{ #category : 'as yet unclassified' }
Float64ArrayTest >> testSum [
	| a |
	self assert: ({0.0. 1.0. Float pi. Float e} asFloat64Array + 1) = ({0.0+1. 1.0+1. Float pi+1. Float e+1} asFloat64Array).
	a _ {0.0. 1.0. Float pi. Float e} asFloat64Array.
	self assert: (a + a) = (a * 2 )
]

{ #category : 'Tests' }
Float64ArrayTest >> testZeroArrayDividend [

	self assert: #[1.2 2.4 0.0 ] / #[1.2 1.2 1.2 ] = #[1.0 2.0 0.0 ] 
]

{ #category : 'Tests' }
Float64ArrayTest >> testZeroArrayDivisor [

	"Test implicit exception"
	self should: [ #[1.2 2.4 3.6 ] / #[1.0 1.0 0.0 ]] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: #[1.0 1.0 0.0 0.0 ] ifDivisorZero: -100 ifBothZero: -200) = 
		#[1.0 2.0 -100.0 -200.0 ].

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: #[1.0 1.0 0.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: #[0.0 1.0 1.0] 
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByArrayOne = #[0.0 2.0 3.141592].
	self assert: self divideByArrayZero = #divisionByZero.
	self assert: self divideByArrayIndeterminate = #indeterminateResult.
]

{ #category : 'Tests' }
Float64ArrayTest >> testZeroDividend [

	self assert: #[1.2 2.4 3.6 0.0 ] / 1.2 = #[1.0 2.0 3.0 0.0 ] 
]

{ #category : 'Tests' }
Float64ArrayTest >> testZeroDivisor [

	"Test implicit exception"
	| |
	self should: [ #[1.2 2.4 3.6 ] / 0.0] raise: ZeroDivide.

	"Test explicit alternative value"
	self assert: (#[1.0 2.0 3.141592 0.0] copy divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200) = 
		#[-100.0 -100.0 -100.0 -200.0 ].

	"Test explicit exceptions"
	self should: [
		#[1.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: ZeroDivide.
	self should: [
		#[0.0 2.0 3.141592] copy
			divideBy: 0.0
			ifDivisorZero: [ZeroDivide signal ]
			ifBothZero: [DomainError signal ]]
		raise: DomainError.

	"Test non local returns in blocks"
	self assert: self divideByScalarOne = #[0.0 2.0 3.141592].
	self assert: self divideByScalarZero = #divisionByZero.
	self assert: self divideByScalarIndeterminate = #indeterminateResult.
]
