'From Cuis 6.0 [latest update: #5226] on 6 June 2022 at 11:36:52 am'!

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 6/5/2022 18:54:33'!
skip: n
	"Skips the next amount objects in the receiver's future sequence values."

	self position: (self position + n min: self size).! !

