'From Cuis 5.0 [latest update: #4638] on 18 June 2021 at 11:52:40 am'!

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 6/18/2021 11:51:28'!
nextNumber
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $. or: [element = $)]]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !

