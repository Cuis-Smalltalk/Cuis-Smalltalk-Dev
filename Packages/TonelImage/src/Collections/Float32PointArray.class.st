"
This class stores points with 32 bit Floating point elements.

{1@2. 3@4} as: Float32PointArray
"
Class {
	#name : 'Float32PointArray',
	#superclass : 'Float32Array',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'instance creation' }
Float32PointArray class >> new: n [
	^super new: n*2
]

{ #category : 'accessing' }
Float32PointArray >> at: index [
	"Return the element (e.g., point) at the given index"
	^(super at: index * 2 - 1) @ (super at: index * 2)
]

{ #category : 'accessing' }
Float32PointArray >> at: index put: aPoint [
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aPoint x.
	super at: index * 2 put: aPoint y.
	^aPoint
]

{ #category : 'accessing' }
Float32PointArray >> at: index putX: aNumber [
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aNumber
]

{ #category : 'accessing' }
Float32PointArray >> at: index putY: aNumber [
	"Store the argument aPoint at the given index"
	super at: index * 2 put: aNumber
]

{ #category : 'accessing' }
Float32PointArray >> defaultElement [
	"Return the default element of the receiver"
	^`0.0@0.0`
]

{ #category : 'printing' }
Float32PointArray >> printElementsOn: aStream [
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: ${.
	self do: [ :point |
		point x printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream nextPut: $@.
		point y printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream nextPut: $..
		aStream space].
	self isEmpty ifFalse: [aStream skip: -2].
	aStream nextPut: $}
]

{ #category : 'accessing' }
Float32PointArray >> size [
	"Return the number of elements in the receiver"
	^ super size // 2
]
