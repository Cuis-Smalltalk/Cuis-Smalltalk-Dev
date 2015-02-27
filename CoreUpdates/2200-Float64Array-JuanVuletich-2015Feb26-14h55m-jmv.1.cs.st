'From Cuis 4.2 of 25 July 2013 [latest update: #2198] on 26 February 2015 at 2:56:20.982714 pm'!
!classDefinition: #Float64Array category: #'Collections-Arrayed'!
ArrayedCollection variableWordSubclass: #Float64Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Float64Array commentStamp: '<historical>' prior: 0!
FloatArrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.
Some support is included for subclasses in the style of Balloon3D-Math.!


!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 12:54'!
at: index
	"Return the element (e.g., 64 bit Float) at the given index"
	^self floatAt: index! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 12:55'!
at: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index"
	^self floatAt: index put: aFloat! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 12:51'!
defaultElement
	"Return the default element of the receiver"
	^0.0! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 12:54'!
floatAt: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Allow subclasses to redefine #at:"
	| answer word1 word2 |
	word1 _ self basicAt: index * 2 - 1.
	word2 _ self basicAt: index * 2.
	answer _ Float new.
	answer at: 1 put: word1.
	answer at: 2 put: word2.
	^answer! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 13:37'!
floatAt: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index
	Allow subclasses to redefine #at:put:"
	self basicAt: index * 2 - 1 put: (aFloat asFloat at: 1).
	self basicAt: index * 2  put: (aFloat asFloat at: 2).
	^aFloat! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 12:37'!
size
	"Return the number of elements in the receiver"
	^super size // 2! !

!Float64Array methodsFor: 'initialization' stamp: 'jmv 2/26/2015 13:02'!
loadFrom: srcObject

	self == srcObject ifTrue: [ ^self ].
	self class == srcObject class
		ifTrue: [ self replaceFrom: 1 to: self size with: srcObject startingAt: 1 ]
		ifFalse: [ self privateLoadFrom: srcObject ]! !

!Float64Array methodsFor: 'private' stamp: 'jmv 2/26/2015 12:45'!
basicAt: index
	"Copied from Float, as we use the same internal representation in all cases.
	Refer to the comment there."

	<primitive: 38>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index].
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Float64Array methodsFor: 'private' stamp: 'jmv 2/26/2015 12:45'!
basicAt: index put: value
	"Copied from Float, as we use the same internal representation in all cases.
	Refer to the comment there."

	<primitive: 39>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index put: value].
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Float64Array methodsFor: 'private' stamp: 'jmv 2/26/2015 12:59'!
privateLoadFrom: srcObject
	"Load the receiver from the given source object."
	self error:'Cannot load a ', srcObject class name,' into a ', self class name! !

!Float64Array methodsFor: 'objects from disk' stamp: 'jmv 2/26/2015 12:31'!
bytesPerElement
	^8! !


!Float64Array class methodsFor: 'instance creation' stamp: 'jmv 2/26/2015 13:32'!
new
	^self new: self numElements! !

!Float64Array class methodsFor: 'instance creation' stamp: 'jmv 2/26/2015 12:37'!
new: n
	^super new: n*2! !

!Float64Array class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!
numElements
	^0! !

