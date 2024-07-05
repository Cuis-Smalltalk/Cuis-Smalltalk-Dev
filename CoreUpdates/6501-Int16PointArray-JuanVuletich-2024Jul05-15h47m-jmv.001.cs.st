'From Cuis7.1 [latest update: #6500] on 5 July 2024 at 3:48:33 pm'!

!DoubleByteArray commentStamp: '<historical>' prior: 0!
DoubleByteArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF, or signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.!

!classDefinition: #Int16PointArray category: #'Collections-Arrayed'!
DoubleByteArray variableDoubleByteSubclass: #Int16PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Int16PointArray commentStamp: '<historical>' prior: 0!
This class stores points that are in signed short integer range (e.g., -32767 <= value <= 32768).!


!PointArray commentStamp: '<historical>' prior: 0!
This class stores 32bit Signed Integer points in place. It is used to pass data efficiently to the primitive level during high-bandwidth 2D graphics operations.!


!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:00:17'!
at: index
	"Return the element (e.g., point) at the given index"
	^(self signedAt: index * 2 - 1) @ (self signedAt: index * 2).
! !

!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:00:28'!
at: index put: aPoint
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aPoint x asInteger.
	self signedAt: index * 2 put: aPoint y asInteger.
	^aPoint.
! !

!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:00:42'!
at: index putX: aNumber
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aNumber asInteger.
! !

!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:00:49'!
at: index putY: aNumber
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 put: aNumber asInteger.
! !

!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:01:31'!
defaultElement
	"Return the default element of the receiver"
	^0@0.
! !

!Int16PointArray methodsFor: 'accessing' stamp: 'jmv 7/5/2024 15:01:59'!
size
	"Return the number of elements in the receiver"
	^ super size // 2.
! !


!Int16PointArray class methodsFor: 'instance creation' stamp: 'jmv 7/5/2024 15:38:22'!
new: n
	^super new: n * 2.
! !

