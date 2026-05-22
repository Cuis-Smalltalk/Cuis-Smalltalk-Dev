"
A TTKernPair represents a TrueType kerning pair.

Instance variables:
	left	<Integer>	The glyph index for the left character.
	right <Integer>	The glyph index for the right character.
	value <Integer>	The amount of kerning.
	mask <Integer>	An efficient representation for the left and the right value.
"
Class {
	#name : 'TTKernPair',
	#superclass : 'Object',
	#instVars : [
		'left',
		'right',
		'value',
		'mask'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'accessing' }
TTKernPair class >> maskFor: left with: right [
	^(left bitShift: 12) + right
]

{ #category : 'accessing' }
TTKernPair >> left [
	^left
]

{ #category : 'accessing' }
TTKernPair >> left: aNumber [

	left := aNumber
]

{ #category : 'accessing' }
TTKernPair >> mask [
	^mask ifNil:[mask := self class maskFor: left with: right]
]

{ #category : 'accessing' }
TTKernPair >> right [
	^right
]

{ #category : 'accessing' }
TTKernPair >> right: aNumber [

	right := aNumber
]

{ #category : 'accessing' }
TTKernPair >> value [
	^value
]

{ #category : 'accessing' }
TTKernPair >> value: aNumber [

	value := aNumber
]
