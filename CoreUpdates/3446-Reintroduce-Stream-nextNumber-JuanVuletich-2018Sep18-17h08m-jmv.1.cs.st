'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 18 September 2018 at 5:08:42 pm'!

!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 9/18/2018 17:08:36'!
nextNumber
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $)]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !

