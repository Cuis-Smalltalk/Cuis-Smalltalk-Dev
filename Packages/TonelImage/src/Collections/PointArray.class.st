"
This class stores 32bit Signed Integer points in place. It is used to pass data efficiently to the primitive level during high-bandwidth 2D graphics operations.
"
Class {
	#name : 'PointArray',
	#superclass : 'IntegerArray',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'instance creation' }
PointArray class >> new: n [
	^super new: n*2
]

{ #category : 'accessing' }
PointArray >> at: index [
	"Return the element (e.g., point) at the given index"
	^(self integerAt: index * 2 - 1) @ (self integerAt: index * 2)
]

{ #category : 'accessing' }
PointArray >> at: index put: aPoint [
	"Store the argument aPoint at the given index"
	self integerAt: index * 2 - 1 put: aPoint x asInteger.
	self integerAt: index * 2 put: aPoint y asInteger.
	^aPoint
]

{ #category : 'accessing' }
PointArray >> at: index putX: aNumber [
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aNumber asInteger
]

{ #category : 'accessing' }
PointArray >> at: index putY: aNumber [
	"Store the argument aPoint at the given index"
	super at: index * 2 put: aNumber asInteger
]

{ #category : 'accessing' }
PointArray >> bounds [
	| min max |
	min := max := self at: 1.
	self do:[:pt|
		min := min min: pt.
		max := max max: pt].
	^min corner: max
		
]

{ #category : 'accessing' }
PointArray >> defaultElement [
	"Return the default element of the receiver"
	^0@0
]

{ #category : 'accessing' }
PointArray >> size [
	"Return the number of elements in the receiver"
	^super size // 2
]
