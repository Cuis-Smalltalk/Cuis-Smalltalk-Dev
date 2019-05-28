'From Cuis 5.0 of 7 November 2016 [latest update: #3773] on 26 May 2019 at 5:34:23 pm'!
'Description Only for Spur images'!
!provides: 'Collections-TwoAndEightByteArrays' 1 1!
SystemOrganization addCategory: #'Collections-TwoAndEightByteArrays'!


!classDefinition: #DoubleByteArray category: #'Collections-TwoAndEightByteArrays'!
ArrayedCollection variableDoubleByteSubclass: #DoubleByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-TwoAndEightByteArrays'!
!classDefinition: 'DoubleByteArray class' category: #'Collections-TwoAndEightByteArrays'!
DoubleByteArray class
	instanceVariableNames: ''!

!classDefinition: #DoubleWordArray category: #'Collections-TwoAndEightByteArrays'!
ArrayedCollection variableDoubleWordSubclass: #DoubleWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-TwoAndEightByteArrays'!
!classDefinition: 'DoubleWordArray class' category: #'Collections-TwoAndEightByteArrays'!
DoubleWordArray class
	instanceVariableNames: ''!


!DoubleByteArray commentStamp: '<historical>' prior: 0!
DoubleByteArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF.

Note: Only for Spur images. V3 images might use ShortWordArray instead.!

!DoubleWordArray commentStamp: '<historical>' prior: 0!
DoubleWordArrays store 64-bit unsigned Integer values, between 0 and 16rFFFFFFFFFFFFFFFF.

Note: Only for Spur images.!

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
