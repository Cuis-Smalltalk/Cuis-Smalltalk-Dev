'From Cuis 4.2 of 25 July 2013 [latest update: #2647] on 27 December 2015 at 4:32:33.200627 pm'!
!classDefinition: #Float64Array category: #'Collections-Arrayed'!
ArrayedCollection variableWordSubclass: #Float64Array
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder '
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Object methodsFor: 'accessing' stamp: 'jmv 12/24/2015 13:41'!
rawBasicAt: index 
	"A verbatim copy of #basicAt:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: 'accessing' stamp: 'jmv 12/24/2015 13:41'!
rawBasicAt: index put: value 
	"A verbatim copy of #basicAt:put:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !


!Float64Array methodsFor: 'private' stamp: 'jmv 12/27/2015 16:28'!
replaceWordsFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	self primitiveFailed! !

!Float64Array methodsFor: 'private' stamp: 'jmv 12/27/2015 16:24'!
swapWords

	| tmp |
	1 to: self size do: [ :i |
		tmp _ self rawBasicAt: i*2.
		self rawBasicAt: i*2 put: (self rawBasicAt: i*2-1).
		self rawBasicAt: i*2-1 put: tmp ]! !

!Float64Array methodsFor: 'objects from disk' stamp: 'jmv 12/27/2015 16:12'!
writeOn: aStream 
	"Store self on the argument, aStream.
	Write bigEndian / PowerPC order."

	aStream nextInt32Put: self size.
	self do: [ :f |
		aStream nextNumber: 4 put: (f basicAt: 1).
		aStream nextNumber: 4 put: (f basicAt: 2) ]! !


!Float64Array class methodsFor: 'objects from disk' stamp: 'jmv 12/27/2015 16:11'!
newFromStream: s
	"Read bigEndian / PowerPC order."

	| len answer f |
	len _ s nextInt32.
	answer _ self new: len.
	f _ Float new.
	1 to: len do: [ :i |
		f basicAt: 1 put: (s nextNumber: 4).
		f basicAt: 2 put: (s nextNumber: 4).
		answer at: i put: f ].
	^ answer! !

!Float64Array class methodsFor: 'class initialization' stamp: 'jmv 12/27/2015 16:19'!
initClassCachedState
	"Check if the word order has changed from the last save"
	(LastSaveOrder at: 1) = 1.0
		ifTrue: [ ^ self ].
	self swapWords! !

!Float64Array class methodsFor: 'class initialization' stamp: 'jmv 12/27/2015 16:20'!
initialize
	"
	Float64Array initialize
	"
	LastSaveOrder _ self with: 1.0! !

!Float64Array class methodsFor: 'class initialization' stamp: 'jmv 12/27/2015 16:19'!
swapWords

	self allSubInstancesDo: [ :inst |
		inst swapWords ]! !


!Float methodsFor: 'as yet unclassified' stamp: 'jmv 12/27/2015 16:29'!
replaceWordsFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	self primitiveFailed! !


!Float64Array methodsFor: 'accessing' stamp: 'jmv 12/27/2015 16:28'!
floatAt: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:"
	| answer |

	answer _ Float new.
	answer replaceWordsFrom: 1 to: 2 with: self startingAt: index * 2 - 1.
	^answer! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 12/25/2015 17:41'!
floatAt: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:put:"

	self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat startingAt: 1.
	^aFloat! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 12/27/2015 16:11'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order."

	| new |
	new _ Float new.		"To get an instance"
	new basicAt: 1 put: (byteStream nextNumber: 4).
	new basicAt: 2 put: (byteStream nextNumber: 4).
	^ new! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 12/27/2015 16:11'!
writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here.
	Write in bigEndian / PowerPC order."

	byteStream nextNumber: 4 put: (aFloat basicAt: 1).
	byteStream nextNumber: 4 put: (aFloat basicAt: 2).
! !

Float64Array initialize!
!methodRemoval: Float64Array #basicAt:!
Float64Array removeSelector: #basicAt:!
!methodRemoval: Float64Array #basicAt:put:!
Float64Array removeSelector: #basicAt:put:!
!classDefinition: #Float64Array category: #'Collections-Arrayed'!
ArrayedCollection variableWordSubclass: #Float64Array
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
