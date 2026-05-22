Class {
	#name : 'ArrayLiteralTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'tests' }
ArrayLiteralTest >> testIsArray [
	self assert: ({1. 2. 'hello'. #Number} is: #Array).
]

{ #category : 'tests' }
ArrayLiteralTest >> testReservedIdentifiers [
	
	self assert: #(nil true false) equals: {nil. true. false}.
]

{ #category : 'tests' }
ArrayLiteralTest >> testSymbols [

	self assert: #(#nil #true #false #'nil' #'true' #'false') equals: {#nil. #true. #false. #nil. #true. #false}.
]
