'From Cuis 4.2 of 25 July 2013 [latest update: #2706] on 24 April 2016 at 1:43:51.658948 am'!

!SmallInteger methodsFor: 'bit manipulation' stamp: 'jmv 4/24/2016 01:40'!
bitAnd: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	self >= 0 ifTrue: [^ arg bitAnd: self].
	^ arg < 0
		ifTrue: [(arg bitInvert bitOr: self bitInvert) bitInvert]
		ifFalse: [arg bitClear: self bitInvert]! !

