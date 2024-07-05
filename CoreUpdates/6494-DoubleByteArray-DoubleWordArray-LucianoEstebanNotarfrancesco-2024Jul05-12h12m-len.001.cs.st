'From Cuis7.1 [latest update: #6493] on 5 July 2024 at 12:18:04 pm'!
!classDefinition: #DoubleByteArray category: #'Collections-Arrayed'!
ArrayedCollection variableDoubleByteSubclass: #DoubleByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!DoubleByteArray commentStamp: '<historical>' prior: 0!
DoubleByteArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF.
!

!classDefinition: #DoubleWordArray category: #'Collections-Arrayed'!
ArrayedCollection variableDoubleWordSubclass: #DoubleWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!DoubleWordArray commentStamp: '<historical>' prior: 0!
DoubleWordArrays store 64-bit unsigned Integer values, between 0 and 16rFFFFFFFFFFFFFFFF.
!


!DoubleByteArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:26:16'!
atAllPut: value
	"Fill the receiver with the given value."
	<primitive: 145>
	super atAllPut: value! !

!DoubleByteArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:16:29'!
byteSize
	^self size * 2! !

!DoubleByteArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:18:48'!
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 2! !

!DoubleByteArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:17:53'!
defaultElement
	^0! !

!DoubleByteArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 12:14:53'!
signedAt: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.
! !

!DoubleByteArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 12:14:59'!
signedAt: index put: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.
! !

!DoubleByteArray methodsFor: 'private' stamp: 'len 5/16/2019 06:21:22'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !


!DoubleWordArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:26:24'!
atAllPut: value
	"Fill the receiver with the given value."
	<primitive: 145>
	super atAllPut: value! !

!DoubleWordArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:16:45'!
byteSize
	^self size * 8! !

!DoubleWordArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:19:05'!
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 8! !

!DoubleWordArray methodsFor: 'accessing' stamp: 'len 5/16/2019 06:18:00'!
defaultElement
	^0! !

!DoubleWordArray methodsFor: 'private' stamp: 'len 5/16/2019 06:21:40'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

