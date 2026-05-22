"
This class stores points that are in signed short integer range (e.g., -32767 <= value <= 32768).
"
Class {
	#name : 'Int16PointArray',
	#superclass : 'DoubleByteArray',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'instance creation' }
Int16PointArray class >> new: n [
	^super new: n * 2.

]

{ #category : 'accessing' }
Int16PointArray >> at: index [
	"Return the element (e.g., point) at the given index"
	^(self signedAt: index * 2 - 1) @ (self signedAt: index * 2).

]

{ #category : 'accessing' }
Int16PointArray >> at: index put: aPoint [
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aPoint x asInteger.
	self signedAt: index * 2 put: aPoint y asInteger.
	^aPoint.

]

{ #category : 'accessing' }
Int16PointArray >> at: index putX: aNumber [
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aNumber asInteger.

]

{ #category : 'accessing' }
Int16PointArray >> at: index putY: aNumber [
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 put: aNumber asInteger.

]

{ #category : 'accessing' }
Int16PointArray >> defaultElement [
	"Return the default element of the receiver"
	^0@0.

]

{ #category : 'accessing' }
Int16PointArray >> size [
	"Return the number of elements in the receiver"
	^ super size // 2.

]
