"
DoubleByteArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF, or signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.
"
Class {
	#name : 'DoubleByteArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'accessing' }
DoubleByteArray >> atAllPut: value [
	"Fill the receiver with the given value."
	<primitive: 145>
	super atAllPut: value
]

{ #category : 'accessing' }
DoubleByteArray >> byteSize [
	^self size * 2
]

{ #category : 'accessing' }
DoubleByteArray >> defaultElement [
	^0
]

{ #category : 'private' }
DoubleByteArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'accessing' }
DoubleByteArray >> signedAt: index [
	"Return the 16-bit signed integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.

]

{ #category : 'accessing' }
DoubleByteArray >> signedAt: index put: value [
	"Store the given 16-bit signed integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.

]
