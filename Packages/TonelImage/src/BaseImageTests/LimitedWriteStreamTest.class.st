Class {
	#name : 'LimitedWriteStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Streams',
	#package : 'BaseImageTests',
	#tag : 'Collections-Streams'
}

{ #category : 'tests-boundary' }
LimitedWriteStreamTest >> testNextPutAllTruncatesAtLimit [
	"nextPutAll: truncates when the string would exceed the limit"
	| result |
	result := String streamContents: [:s |
		s nextPutAll: '12345678901234567890'] limitedTo: 7.
	self assert: result = '1234567'
]

{ #category : 'tests-boundary' }
LimitedWriteStreamTest >> testNextPutStopsAtLimit [
	"nextPut: stops writing when the limit is reached, invoking the limit block"
	| result |
	result := String streamContents: [:s |
		'abcdefghij' do: [:c | s nextPut: c]] limitedTo: 5.
	self assert: result size = 5.
	self assert: result = 'abcde'
]

{ #category : 'tests-scenario' }
LimitedWriteStreamTest >> testPrintStringUsesLimitedStream [
	"streamContents:limitedTo: is the mechanism behind printString overflow protection"
	| result |
	result := String streamContents: [:s |
		100 timesRepeat: [s nextPutAll: 'overflow']] limitedTo: 20.
	self assert: result size = 20.
	self assert: (result copyFrom: 1 to: 8) = 'overflow'
]

{ #category : 'tests-zero' }
LimitedWriteStreamTest >> testWriteNothing [
	"Writing nothing to a limited stream yields empty contents"
	| result |
	result := String streamContents: [:s | "do nothing"] limitedTo: 10.
	self assert: result = ''
]

{ #category : 'tests-one' }
LimitedWriteStreamTest >> testWriteOneCharWithinLimit [
	"Writing one character within the limit writes it normally"
	| result |
	result := String streamContents: [:s | s nextPut: $A] limitedTo: 10.
	self assert: result = 'A'
]

{ #category : 'tests-many' }
LimitedWriteStreamTest >> testWriteWithinLimitCompletesNormally [
	"Writing a string shorter than the limit returns the full string"
	| result |
	result := String streamContents: [:s | s nextPutAll: 'hello'] limitedTo: 10.
	self assert: result = 'hello'
]
