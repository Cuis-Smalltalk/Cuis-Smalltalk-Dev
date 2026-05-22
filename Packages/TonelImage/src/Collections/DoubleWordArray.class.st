"
DoubleWordArrays store 64-bit unsigned Integer values, between 0 and 16rFFFFFFFFFFFFFFFF.

"
Class {
	#name : 'DoubleWordArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'accessing' }
DoubleWordArray >> atAllPut: value [
	"Fill the receiver with the given value."
	<primitive: 145>
	super atAllPut: value
]

{ #category : 'accessing' }
DoubleWordArray >> byteSize [
	^self size * 8
]

{ #category : 'accessing' }
DoubleWordArray >> defaultElement [
	^0
]

{ #category : 'private' }
DoubleWordArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]
